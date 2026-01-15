fn main() {
    #[cfg(all(feature = "gtk", unix, not(target_os = "macos")))]
    {
        let theme = linux_theme::gtk::current::current();
        println!("{:#?}", &theme)
    }
}
