use std::convert::Infallible;
use std::path::Path;

use cssparser::*;
use cssparser_color::{Color, RgbaLegacy, DefaultColorParser, parse_color_with};

use lightningcss::error::PrinterErrorKind;
use lightningcss::printer::{Printer, PrinterOptions};
use lightningcss::rules::CssRule;
use lightningcss::stylesheet::{ParserOptions, StyleSheet};
use lightningcss::traits::AtRuleParser;
use lightningcss::traits::ToCss;
use lightningcss::values::ident::Ident;
use lightningcss::visit_types;
use lightningcss::visitor::{Visit, VisitTypes, Visitor};
use lightningcss::bundler::{Bundler, FileProvider};

pub const PALETTE_CSS: &str = include_str!("css/palette.css");
pub const LIGHT_CSS: &str = include_str!("css/light.css");
pub const LIGHT_DERIVE_CSS: &str = include_str!("css/light-derive.css");
pub const DARK_CSS: &str = include_str!("css/dark.css");
pub const DARK_DERIVE_CSS: &str = include_str!("css/dark-derive.css");

#[derive(Debug)]
pub enum Error {
    IoError(std::io::Error),
    ParserError(String),
}

pub fn from_file(path: &Path) -> Result<Vec<DefineColor>, Error> {
    let mode = dark_light::detect().unwrap_or(dark_light::Mode::Unspecified);

    let fs = FileProvider::new();
    let mut bundler = Bundler::new(
        &fs,
        None,
        ParserOptions {
            error_recovery: true,
            ..Default::default()
        },
    );
    let result = match bundler.bundle(path) {
        Ok(stylesheet) => {
            let css = stylesheet.to_css(PrinterOptions::default()).unwrap().code + if mode == dark_light::Mode::Dark {
                DARK_DERIVE_CSS
            } else {
                LIGHT_DERIVE_CSS
            };
            from_str(&css)
        }
        Err(e) => return Err(Error::ParserError(e.to_string())),
    };

    result
}
pub fn from_str(css: &str) -> Result<Vec<DefineColor>, Error> {
    let mut defined_colors_with_refs = vec![];
    match StyleSheet::parse_with(
        &css,
        ParserOptions {
            error_recovery: true,
            ..Default::default()
        },
        &mut ColorParser,
    ) {
        Ok(mut stylesheet) => {
            stylesheet
                .visit(&mut DefineColorCollector {
                    colors: &mut defined_colors_with_refs,
                })
                .unwrap();
        }
        Err(e) => return Err(Error::ParserError(e.to_string())),
    };
    
    // Resolve color references
    let mut color_map = std::collections::HashMap::new();
    let mut resolved_colors = vec![];
    
    // First pass: collect all direct colors (not references or functions)
    for color_def in &defined_colors_with_refs {
        if let ColorOrReference::Color(color) = &color_def.color_or_ref {
            color_map.insert(color_def.ident.clone(), color.clone());
        }
    }
    
    // Second pass: resolve references (may need multiple iterations for chained references)
    let mut changed = true;
    while changed {
        changed = false;
        for color_def in &defined_colors_with_refs {
            if !color_map.contains_key(&color_def.ident) {
                match &color_def.color_or_ref {
                    ColorOrReference::Color(color) => {
                        color_map.insert(color_def.ident.clone(), color.clone());
                        changed = true;
                    }
                    ColorOrReference::Reference(ref_name) => {
                        if let Some(resolved_color) = color_map.get(ref_name) {
                            color_map.insert(color_def.ident.clone(), resolved_color.clone());
                            changed = true;
                        }
                    }
                    ColorOrReference::Function(func) => {
                        // Try to evaluate the function if all its dependencies are available
                        match evaluate_color_function(func, &color_map) {
                            Ok(resolved_color) => {
                                color_map.insert(color_def.ident.clone(), resolved_color);
                                changed = true;
                            }
                            Err(_) => {
                                // Dependencies not available yet, will try again in next iteration
                            }
                        }
                    }
                }
            }
        }
    }

    // Convert to final format - resolve references and evaluate functions
    for color_def in defined_colors_with_refs {
        let color = if let Some(resolved) = color_map.get(&color_def.ident) {
            // Already resolved in the resolution loop
            resolved.clone()
        } else {
            // Not yet resolved, try to resolve it now
            match &color_def.color_or_ref {
                ColorOrReference::Color(c) => c.clone(),
                ColorOrReference::Reference(ref_name) => {
                    color_map.get(ref_name)
                        .ok_or_else(|| Error::ParserError(format!("Color reference '{}' not found", ref_name)))?
                        .clone()
                }
                ColorOrReference::Function(func) => {
                    evaluate_color_function(func, &color_map)?
                }
            }
        };
        resolved_colors.push(DefineColor {
            ident: color_def.ident,
            color,
            loc: color_def.loc,
        });
    }
    
    Ok(resolved_colors)
}

struct ColorParser;
#[derive(Debug, Clone)]
enum Prelude<'i> {
    DefineColor(Ident<'i>, ColorOrReference),
}
#[derive(Debug, Clone)]
enum ColorOrReference {
    Color(Color),
    Reference(String),
    Function(ColorFunction),
}

#[derive(Debug, Clone)]
enum ColorFunction {
    Lighter(ColorArg),
    Darker(ColorArg),
    Shade(ColorArg, f32),
    Alpha(ColorArg, f32),
    Mix(ColorArg, ColorArg, f32),
}

#[derive(Debug, Clone)]
enum ColorArg {
    Color(Color),
    Reference(String),
}
#[derive(Debug, Clone)]
enum AtRule {
    DefineColor(DefineColorWithRef),
}
#[derive(Debug, Clone)]
struct DefineColorWithRef {
    ident: String,
    color_or_ref: ColorOrReference,
    loc: SourceLocation,
}

#[derive(Debug, Clone, PartialEq)]
pub struct DefineColor {
    pub ident: String,
    pub color: Color,
    pub loc: SourceLocation,
}

fn resolve_color_arg(arg: &ColorArg, color_map: &std::collections::HashMap<String, Color>) -> Result<Color, Error> {
    match arg {
        ColorArg::Color(c) => Ok(c.clone()),
        ColorArg::Reference(ref_name) => {
            color_map.get(ref_name)
                .ok_or_else(|| Error::ParserError(format!("Color reference '{}' not found", ref_name)))
                .map(|c| c.clone())
        }
    }
}

/// Convert a cssparser_color::Color to RgbaLegacy for manipulation.
/// Handles various color formats by converting them to RGBA.
fn color_to_rgba(color: &Color) -> Result<RgbaLegacy, Error> {
    match color {
        Color::Rgba(rgba) => Ok(*rgba),
        Color::CurrentColor => Err(Error::ParserError("Cannot convert currentColor to RGBA".to_string())),
        Color::Hsl(hsl) => {
            use cssparser_color::hsl_to_rgb;
            let h = hsl.hue.unwrap_or(0.0);
            let s = hsl.saturation.unwrap_or(0.0);
            let l = hsl.lightness.unwrap_or(0.0);
            let alpha = hsl.alpha.unwrap_or(1.0);
            let (r, g, b) = hsl_to_rgb(h, s, l);
            Ok(RgbaLegacy {
                red: (r * 255.0) as u8,
                green: (g * 255.0) as u8,
                blue: (b * 255.0) as u8,
                alpha: alpha,
            })
        }
        Color::Hwb(hwb) => {
            use cssparser_color::hwb_to_rgb;
            let h = hwb.hue.unwrap_or(0.0);
            let w = hwb.whiteness.unwrap_or(0.0);
            let b = hwb.blackness.unwrap_or(0.0);
            let alpha = hwb.alpha.unwrap_or(1.0);
            let (r, g, b_val) = hwb_to_rgb(h, w, b);
            Ok(RgbaLegacy {
                red: (r * 255.0) as u8,
                green: (g * 255.0) as u8,
                blue: (b_val * 255.0) as u8,
                alpha: alpha,
            })
        }
        // For other color formats (Lab, Lch, Oklab, Oklch, ColorFunction),
        // we'll need to convert them. For now, return an error or convert via a simpler path.
        // In practice, most CSS colors will be RGBA or HSL, so this should cover most cases.
        _ => Err(Error::ParserError(format!("Unsupported color format for conversion: {:?}", color))),
    }
}

fn evaluate_color_function(
    func: &ColorFunction,
    color_map: &std::collections::HashMap<String, Color>,
) -> Result<Color, Error> {
    match func {
        ColorFunction::Lighter(arg) => {
            let color = resolve_color_arg(arg, color_map)?;
            Ok(shade_color(&color, 1.3))
        }
        ColorFunction::Darker(arg) => {
            let color = resolve_color_arg(arg, color_map)?;
            Ok(shade_color(&color, 0.7))
        }
        ColorFunction::Shade(arg, factor) => {
            let color = resolve_color_arg(arg, color_map)?;
            Ok(shade_color(&color, *factor))
        }
        ColorFunction::Alpha(arg, factor) => {
            let color = resolve_color_arg(arg, color_map)?;
            Ok(alpha_color(&color, *factor))
        }
        ColorFunction::Mix(arg1, arg2, factor) => {
            let color1 = resolve_color_arg(arg1, color_map)?;
            let color2 = resolve_color_arg(arg2, color_map)?;
            Ok(mix_colors(&color1, &color2, *factor))
        }
    }
}

fn shade_color(color: &Color, factor: f32) -> Color {
    // Convert to RGBA, adjust lightness, convert back
    let rgba = match color_to_rgba(color) {
        Ok(rgba) => rgba,
        Err(_) => return color.clone(), // Can't shade, return original
    };
    let r = rgba.red as f32 / 255.0;
    let g = rgba.green as f32 / 255.0;
    let b = rgba.blue as f32 / 255.0;
    
    // Convert RGB to HSL
    let (h, s, l) = rgb_to_hsl(r, g, b);
    
    // Adjust lightness: factor 0 = black, 1 = original, 2 = white
    let new_l = (l * factor).min(1.0).max(0.0);
    
    // Convert back to RGB
    let (r_new, g_new, b_new) = hsl_to_rgb(h, s, new_l);
    
    Color::Rgba(RgbaLegacy {
        red: (r_new * 255.0) as u8,
        green: (g_new * 255.0) as u8,
        blue: (b_new * 255.0) as u8,
        alpha: rgba.alpha,
    })
}

fn alpha_color(color: &Color, factor: f32) -> Color {
    let rgba = match color_to_rgba(color) {
        Ok(rgba) => rgba,
        Err(_) => return color.clone(), // Can't alpha, return original
    };
    let new_alpha = (rgba.alpha * factor).min(1.0).max(0.0);
    Color::Rgba(RgbaLegacy {
        red: rgba.red,
        green: rgba.green,
        blue: rgba.blue,
        alpha: new_alpha,
    })
}

fn mix_colors(color1: &Color, color2: &Color, factor: f32) -> Color {
    let rgba1 = match color_to_rgba(color1) {
        Ok(rgba) => rgba,
        Err(_) => return color1.clone(), // Can't mix, return first color
    };
    let rgba2 = match color_to_rgba(color2) {
        Ok(rgba) => rgba,
        Err(_) => return color1.clone(), // Can't mix, return first color
    };
    
    // Interpolate: factor 0 = color1, 1 = color2
    let r = rgba1.red as f32 * (1.0 - factor) + rgba2.red as f32 * factor;
    let g = rgba1.green as f32 * (1.0 - factor) + rgba2.green as f32 * factor;
    let b = rgba1.blue as f32 * (1.0 - factor) + rgba2.blue as f32 * factor;
    let a = rgba1.alpha as f32 * (1.0 - factor) + rgba2.alpha as f32 * factor;
    
    Color::Rgba(RgbaLegacy {
        red: r as u8,
        green: g as u8,
        blue: b as u8,
        alpha: a,
    })
}

fn rgb_to_hsl(r: f32, g: f32, b: f32) -> (f32, f32, f32) {
    let max = r.max(g).max(b);
    let min = r.min(g).min(b);
    let delta = max - min;
    
    let l = (max + min) / 2.0;
    
    let s = if delta == 0.0 {
        0.0
    } else {
        delta / (1.0 - (2.0 * l - 1.0).abs())
    };
    
    let h = if delta == 0.0 {
        0.0
    } else if max == r {
        60.0 * (((g - b) / delta) % 6.0)
    } else if max == g {
        60.0 * (((b - r) / delta) + 2.0)
    } else {
        60.0 * (((r - g) / delta) + 4.0)
    };
    
    (h / 360.0, s, l)
}

fn hsl_to_rgb(h: f32, s: f32, l: f32) -> (f32, f32, f32) {
    let c = (1.0 - (2.0 * l - 1.0).abs()) * s;
    let x = c * (1.0 - ((h * 6.0) % 2.0 - 1.0).abs());
    let m = l - c / 2.0;
    
    let (r, g, b) = if h < 1.0/6.0 {
        (c, x, 0.0)
    } else if h < 2.0/6.0 {
        (x, c, 0.0)
    } else if h < 3.0/6.0 {
        (0.0, c, x)
    } else if h < 4.0/6.0 {
        (0.0, x, c)
    } else if h < 5.0/6.0 {
        (x, 0.0, c)
    } else {
        (c, 0.0, x)
    };
    
    (r + m, g + m, b + m)
}

fn parse_color_arg<'i, 't>(input: &mut Parser<'i, 't>) -> Result<ColorArg, ParseError<'i, Infallible>> {
    let state = input.state();
    match input.next() {
        Ok(&Token::AtKeyword(ref ref_name)) => {
            Ok(ColorArg::Reference(ref_name.to_string()))
        }
        Ok(_) => {
            input.reset(&state);
            let mut parser = DefaultColorParser;
            match parse_color_with(&mut parser, input) {
                Ok(color) => Ok(ColorArg::Color(color)),
                Err(_) => {
                    input.reset(&state);
                    Err(input.new_error(BasicParseErrorKind::UnexpectedToken(Token::Ident("color".into()))))
                }
            }
        }
        Err(e) => {
            input.reset(&state);
            Err(e.into())
        }
    }
}

/// Parse a number factor that can be either a percentage (e.g., 83%) or a decimal (e.g., 0.83)
/// Returns a value between 0.0 and 1.0
fn parse_percentage_or_number<'i, 't>(input: &mut Parser<'i, 't>) -> Result<f32, ParseError<'i, Infallible>> {
    let state = input.state();
    match input.next() {
        Ok(&Token::Percentage { unit_value, .. }) => {
            // Percentage is already a value between 0 and 1 in cssparser
            Ok(unit_value)
        }
        Ok(&Token::Number { value, .. }) => {
            Ok(value)
        }
        Ok(_) => {
            input.reset(&state);
            // Try parsing as a number (handles cases like 0.83)
            let num = input.expect_number()?;
            Ok(num)
        }
        Err(_) => {
            input.reset(&state);
            // Try parsing as a number
            let num = input.expect_number()?;
            Ok(num)
        }
    }
}

fn parse_color_function<'i, 't>(
    input_parser: &mut Parser<'i, 't>,
    func_name: &CowRcStr<'i>,
) -> Result<ColorOrReference, ParseError<'i, Infallible>> {
    // Function token already consumed, parse the nested block (parentheses and arguments)
    input_parser.parse_nested_block(|input| {
    
        let func_lower = func_name.to_ascii_lowercase();
        if func_lower == "lighter" {
            let arg = parse_color_arg(input)?;
            input.expect_exhausted()?;
            Ok(ColorOrReference::Function(ColorFunction::Lighter(arg)))
        } else if func_lower == "darker" {
            let arg = parse_color_arg(input)?;
            input.expect_exhausted()?;
            Ok(ColorOrReference::Function(ColorFunction::Darker(arg)))
        } else if func_lower == "shade" {
            let arg = parse_color_arg(input)?;
            input.expect_comma()?;
            let factor = parse_percentage_or_number(input)?;
            input.expect_exhausted()?;
            Ok(ColorOrReference::Function(ColorFunction::Shade(arg, factor)))
        } else if func_lower == "alpha" {
            let arg = parse_color_arg(input)?;
            input.expect_comma()?;
            let factor = parse_percentage_or_number(input)?;
            input.expect_exhausted()?;
            Ok(ColorOrReference::Function(ColorFunction::Alpha(arg, factor)))
        } else if func_lower == "mix" {
            let arg1 = parse_color_arg(input)?;
            input.expect_comma()?;
            let arg2 = parse_color_arg(input)?;
            input.expect_comma()?;
            let factor = parse_percentage_or_number(input)?;
            input.expect_exhausted()?;
            Ok(ColorOrReference::Function(ColorFunction::Mix(arg1, arg2, factor)))
        } else {
            Err(input.new_error(BasicParseErrorKind::UnexpectedToken(Token::Ident(func_name.to_string().into()))))
        }
    })
}

impl<'i> AtRuleParser<'i> for ColorParser {
    type Prelude = Prelude<'i>;

    type AtRule = AtRule;

    type Error = Infallible;

    fn parse_prelude<'t>(
        &mut self,
        name: CowRcStr<'i>,
        input_parser: &mut Parser<'i, 't>,
        _: &ParserOptions<'_, 'i>,
    ) -> Result<Self::Prelude, ParseError<'i, Self::Error>> {
        match_ignore_ascii_case! { &name,
                    "define-color" => {

        let ident = input_parser.expect_ident_cloned()?;
        
        // Check what the next token is: AtKeyword (reference), Function (color function), or Color
        let state = input_parser.state();
        let next_token = input_parser.next();
        let color_or_ref = match next_token {
            Ok(&Token::AtKeyword(ref ref_name)) => {
                // It's a color reference - token already consumed by next()
                ColorOrReference::Reference(ref_name.to_string())
            }
            Ok(&Token::Function(ref func_name)) => {
                // It's a color function - Function token consumed, now parse the nested block
                let func_name_clone = func_name.to_string();
                drop(next_token); // Release the borrow
                parse_color_function(input_parser, &func_name_clone.into())?
            }
            Ok(_) => {
                // Not a reference or function, reset and parse as a regular color
                input_parser.reset(&state);
                let mut parser = DefaultColorParser;
                match parse_color_with(&mut parser, input_parser) {
                    Ok(color) => ColorOrReference::Color(color),
                    Err(_) => return Err(input_parser.new_error(BasicParseErrorKind::UnexpectedToken(Token::Ident("color".into())))),
                }
            }
            Err(_) => {
                // Error getting next token, try parsing as color
                input_parser.reset(&state);
                let mut parser = DefaultColorParser;
                match parse_color_with(&mut parser, input_parser) {
                    Ok(color) => ColorOrReference::Color(color),
                    Err(_) => return Err(input_parser.new_error(BasicParseErrorKind::UnexpectedToken(Token::Ident("color".into())))),
                }
            }
        };

        Ok(Prelude::DefineColor(
            Ident(ident.into()),
            color_or_ref,
        ))
                    },
                    _ => Err(input_parser.new_error(BasicParseErrorKind::AtRuleInvalid(name)))
                }
    }

    fn rule_without_block(
        &mut self,
        prelude: Self::Prelude,
        start: &ParserState,
        _: &ParserOptions<'_, 'i>,
        _: bool,
    ) -> Result<Self::AtRule, ()> {
        match prelude {
            Prelude::DefineColor(ident, color_or_ref) => {
                Ok(AtRule::DefineColor(DefineColorWithRef {
                    ident: ident.to_string(),
                    color_or_ref,
                    loc: start.source_location(),
                }))
            }
        }
    }
}

struct DefineColorCollector<'i> {
    colors: &'i mut Vec<DefineColorWithRef>,
}

impl<'i, V: Visitor<'i, AtRule>> Visit<'i, AtRule, V> for AtRule {
    const CHILD_TYPES: VisitTypes = VisitTypes::empty();

    fn visit_children(&mut self, _: &mut V) -> Result<(), V::Error> {
        Ok(())
    }
}

impl<'a, 'i> Visitor<'i, AtRule> for DefineColorCollector<'i> {
    type Error = Infallible;

    fn visit_types(&self) -> VisitTypes {
        visit_types!(RULES)
    }

    fn visit_rule(&mut self, rule: &mut CssRule<'i, AtRule>) -> Result<(), Self::Error> {
        if let CssRule::Custom(AtRule::DefineColor(color)) = rule {
            self.colors.push(color.clone());
        }

        Ok(())
    }

    fn visit_color(
        &mut self,
        color: &mut lightningcss::values::color::CssColor,
    ) -> Result<(), Self::Error> {
        if let Ok(lab_color) = color.to_lab() {
            *color = lab_color;
        }
        Ok(())
    }
}

impl ToCss for AtRule {
    fn to_css<W: std::fmt::Write>(
        &self,
        _: &mut Printer<'_, '_, '_, W>,
    ) -> Result<(), lightningcss::error::Error<PrinterErrorKind>> {
        unimplemented!()
    }
}
#[cfg(test)]
pub mod test {
    use cssparser_color::{Color, RgbaLegacy};

    use super::from_str;
    #[test]
    pub fn test() {
        let css = from_str(include_str!("gtk.css")).unwrap();

        assert_eq!(
            css.first().unwrap().ident,
            "accent_color"
        );
        assert_eq!(
            css.first().unwrap().color,
            Color::Rgba(RgbaLegacy {
                red: 233,
                green: 70,
                blue: 134,
                alpha: 255,
            })
        );

        assert_eq!(
            css.last().unwrap().ident,
            "cute_fg"
        );
        assert_eq!(
            css.last().unwrap().color,
            Color::Rgba(RgbaLegacy {
                red: 191,
                green: 16,
                blue: 76,
                alpha: 255,
            })
        )
    }
    
    #[test]
    pub fn test_two_colors() {
        let css = r#"
            @define-color base_color #ff0000;
            @define-color other_color #00ff00;
        "#;
        
        let colors = from_str(css).unwrap();
        println!("Parsed {} colors: {:?}", colors.len(), colors.iter().map(|c| &c.ident).collect::<Vec<_>>());
        assert_eq!(colors.len(), 2);
    }
    
    #[test]
    pub fn test_color_reference() {
        let css = r#"
            @define-color base_color #ff0000;
            @define-color derived_color @base_color;
        "#;
        
        let colors = from_str(css).unwrap();
        
        println!("Parsed {} colors: {:?}", colors.len(), colors.iter().map(|c| &c.ident).collect::<Vec<_>>());
        
        // Find the derived color
        let derived = colors.iter().find(|c| c.ident == "derived_color").expect("derived_color not found");
        let base = colors.iter().find(|c| c.ident == "base_color").expect("base_color not found");
        
        // The derived color should have the same color value as the base color
        assert_eq!(derived.color, base.color);
        assert_eq!(derived.color, Color::Rgba(RgbaLegacy {
            red: 255,
            green: 0,
            blue: 0,
            alpha: 255,
        }));
    }
    
    #[test]
    pub fn test_color_functions() {
        let css = r#"
            @define-color base_color #808080;
            @define-color lighter_color lighter(@base_color);
            @define-color darker_color darker(@base_color);
            @define-color shaded_color shade(@base_color, 0.5);
            @define-color alpha_color alpha(@base_color, 0.5);
            @define-color mixed_color mix(@base_color, #000000, 0.5);
        "#;
        
        let colors = from_str(css).unwrap();
        
        // All colors should be parsed successfully
        assert_eq!(colors.len(), 6);
        
        // Verify lighter is lighter than base
        let base = colors.iter().find(|c| c.ident == "base_color").unwrap();
        let lighter = colors.iter().find(|c| c.ident == "lighter_color").unwrap();
        if let (Color::Rgba(base_rgba), Color::Rgba(lighter_rgba)) = (&base.color, &lighter.color) {
            assert!(lighter_rgba.red > base_rgba.red || lighter_rgba.green > base_rgba.green || lighter_rgba.blue > base_rgba.blue);
        }
        
        // Verify darker is darker than base
        let darker = colors.iter().find(|c| c.ident == "darker_color").unwrap();
        if let (Color::Rgba(base_rgba), Color::Rgba(darker_rgba)) = (&base.color, &darker.color) {
            assert!(darker_rgba.red < base_rgba.red || darker_rgba.green < base_rgba.green || darker_rgba.blue < base_rgba.blue);
        }
        
        // Verify alpha is applied
        let alpha = colors.iter().find(|c| c.ident == "alpha_color").unwrap();
        if let (Color::Rgba(base_rgba), Color::Rgba(alpha_rgba)) = (&base.color, &alpha.color) {
            assert_eq!(alpha_rgba.alpha, base_rgba.alpha / 2);
        }
    }
    
    #[test]
    pub fn test_mix_with_references() {
        // Test the exact scenario from the user's issue
        let css = r#"
            @define-color window_bg_color #141218;
            @define-color view_bg_color #141218;
            @define-color insensitive_bg_color mix(@window_bg_color,@view_bg_color,.4);
            @define-color unfocused_insensitive_color @insensitive_bg_color;
        "#;
        
        let colors = from_str(css).unwrap();
        
        // All colors should be parsed and resolved
        assert_eq!(colors.len(), 4);
        
        // Verify insensitive_bg_color exists and is resolved
        let insensitive = colors.iter().find(|c| c.ident == "insensitive_bg_color").expect("insensitive_bg_color not found");
        let unfocused = colors.iter().find(|c| c.ident == "unfocused_insensitive_color").expect("unfocused_insensitive_color not found");
        
        // unfocused_insensitive_color should reference insensitive_bg_color
        assert_eq!(unfocused.color, insensitive.color);
    }
    
    #[test]
    pub fn test_percentage_parsing() {
        // Test that percentages work in mix, shade, and alpha functions
        let css = r#"
            @define-color base_color #808080;
            @define-color mixed_percent mix(@base_color, #000000, 50%);
            @define-color mixed_decimal mix(@base_color, #000000, 0.5);
            @define-color shaded_percent shade(@base_color, 83%);
            @define-color shaded_decimal shade(@base_color, 0.83);
            @define-color alpha_percent alpha(@base_color, 50%);
            @define-color alpha_decimal alpha(@base_color, 0.5);
        "#;
        
        let colors = from_str(css).unwrap();
        
        // All colors should be parsed successfully
        assert_eq!(colors.len(), 7);
        
        // Verify that percentage and decimal versions produce the same result
        let mixed_percent = colors.iter().find(|c| c.ident == "mixed_percent").unwrap();
        let mixed_decimal = colors.iter().find(|c| c.ident == "mixed_decimal").unwrap();
        assert_eq!(mixed_percent.color, mixed_decimal.color, "mix() with 50% should equal mix() with 0.5");
        
        let shaded_percent = colors.iter().find(|c| c.ident == "shaded_percent").unwrap();
        let shaded_decimal = colors.iter().find(|c| c.ident == "shaded_decimal").unwrap();
        assert_eq!(shaded_percent.color, shaded_decimal.color, "shade() with 83% should equal shade() with 0.83");
        
        let alpha_percent = colors.iter().find(|c| c.ident == "alpha_percent").unwrap();
        let alpha_decimal = colors.iter().find(|c| c.ident == "alpha_decimal").unwrap();
        assert_eq!(alpha_percent.color, alpha_decimal.color, "alpha() with 50% should equal alpha() with 0.5");
    }
}
