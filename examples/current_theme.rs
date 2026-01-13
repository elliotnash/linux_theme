fn main() {
    #[cfg(all(feature = "gtk", unix, not(target_os = "macos")))]
    {
        let theme = rusty_themy::gtk::current::current();
        println!("{:#?}", &theme)
    }
}
