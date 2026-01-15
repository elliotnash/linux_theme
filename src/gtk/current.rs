use std::collections::HashMap;
use std::path::PathBuf;

use cssparser::Color;

use super::parse::{from_file, from_str, DefineColor};

const PALETTE_CSS: &str = include_str!("css/palette.css");
const LIGHT_CSS: &str = include_str!("css/light.css");
const DARK_CSS: &str = include_str!("css/dark.css");

#[cfg(all(unix, not(target_os = "macos")))]
/**
 * Get the current color variables applied by GTK
 *
 * Currently only supports Freedesktop platforms.
 */
pub fn current() -> (HashMap<String, Color>, Vec<Error>) {
    use std::env;

    

    let (mut map, mut error) = if let Ok(name) = std::env::var("GTK_THEME") {
        get_theme(&name)
    } else {
        (HashMap::new(), vec![])
    };

    get_theme_on_folder(
        &PathBuf::from(env::var("HOME").unwrap()).join(".config/gtk-4.0"),
        &mut map,
        &mut error,
    );
    (map, error)
}

pub fn get_theme(name: &str) -> (HashMap<String, Color>, Vec<Error>) {
    let mut themes = HashMap::new();
    let mut errors = vec![];

    for path in ["/usr/share/themes/", "~/.local/share/themes/", "~/.themes/"] {
        let path = PathBuf::from(path).join(name).join("gtk-4.0");

        get_theme_on_folder(&path, &mut themes, &mut errors);
    }

    (themes, errors)
}

fn push(theme: Result<Vec<DefineColor>, crate::gtk::parse::Error>, themes: &mut HashMap<String, Color>, errors: &mut Vec<Error>) {
    match theme {
        Ok(theme) => {
            for DefineColor {
                ident,
                color,
                loc: _,
            } in theme
            {
                themes.insert(ident, color);
            }
        }
        Err(error) => errors.push(Error::ParsingError(error)),
    };
}

fn get_theme_on_folder(
    path: &PathBuf,
    themes: &mut HashMap<String, Color>,
    errors: &mut Vec<Error>,
) {
    let mode = dark_light::detect();

    let default_css = PALETTE_CSS.to_owned() + if mode == dark_light::Mode::Dark {
        DARK_CSS
    } else {
        LIGHT_CSS
    };

    push(from_str(&default_css), themes, errors);

    if path.exists() {
        let normal = path.join("gtk.css");

        if normal.exists() {
            push(from_file(&normal), themes, errors);
        }

        if mode == dark_light::Mode::Dark {
            let dark = PathBuf::from(path).join("gtk-dark.css");

            if dark.exists() {
                push(from_file(&dark), themes, errors);
            }
        }
    }
}

#[derive(Debug)]
pub enum Error {
    ParsingError(super::parse::Error),
}
