fn main() {
    inwelling::to("structx");
    if std::env::var("CARGO_FEATURE_LIBTK").is_ok() {
        inwelling::to("lens-rs");
    }
}
