fn main() {
    let base = zebra::db::Database::default();

    let path: Option<String> = std::env::args().skip(1).next();
}
