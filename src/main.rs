extern crate boobool;

use boobool::errors::Result;
use boobool::parser;
use boobool::scanner;
use boobool::typechecker;
use boobool::types::{Type, Symtable};

use std::io::{self, Write};

fn main() {
    if let Err(e) = run() {
        let _ = writeln!(io::stderr(), "boobool: {}", e);
        ::std::process::exit(1);
    }
}


/// Reads expressions from stdin (one per line), scans them,
/// parses them, and type checks them.  If an expression is
/// valid, it will be pretty print to stdout and the next expression
/// (if any) will be processed.  If an expression is invalid,
/// an error is returned and no more expressions are processed.
fn run() -> Result<()> {
    let stdin = io::stdin();
    let mut buf = String::new();
    let st = make_symtable();
    while stdin.read_line(&mut buf).unwrap_or(0) != 0 {
        let bytes: Vec<u8> = buf.bytes().collect();
        let toks = scanner::Scanner::scan(bytes)?;
        let expr = parser::Parser::parse(toks)?;
        let texpr = typechecker::typecheck(expr, &st)?;
        println!("{}", texpr);
        buf.clear();
    }
    return Ok(());
}


/// Creates a symbol table for type checking.
fn make_symtable() -> Symtable {
    let mut st = Symtable::new();

    // Variables
    st.add("bidder_ids", Type::List(Box::new(Type::Int)));
    st.add("exchange", Type::Int);
    st.add("member_id", Type::Int);
    st.add("country", Type::Str);
    st.add("region", Type::Str);
    st.add("city", Type::Str);
    st.add("pc", Type::Str);
    st.add("dma", Type::Int);
    st.add("lg", Type::Str);
    st.add("doubleclick_verticals", Type::List(Box::new(Type::Int)));
    st.add("appnexus_categories", Type::List(Box::new(Type::Int)));
    st.add("iab_categories", Type::List(Box::new(Type::Int)));
    st.add("device_type_id", Type::Int);
    st.add("device_id", Type::Int);
    st.add("operating_system_id", Type::Int);
    st.add("operating_system_variant_id", Type::Int);
    st.add("browser_id", Type::Int);
    st.add("browser_variant_id", Type::Int);
    st.add("matched_user", Type::Bool);
    st.add("latitude", Type::Float);
    st.add("longitude", Type::Float);
    st.add("device_located", Type::Bool);
    st.add("ag_rank", Type::Int);
    st.add("sitelist_ids", Type::List(Box::new(Type::Int)));
    st.add("adgear_segments", Type::List(Box::new(Type::Int)));
    //st.add("segments_with_timestamp", Type::Segments);
    st.add("ip", Type::Str);
    st.add("now", Type::Int);
    st.add("iplist_ids", Type::List(Box::new(Type::Int)));
    st.add("ias_segments", Type::List(Box::new(Type::Int)));
    st.add("exchange_seller_site_id", Type::Int);
    st.add("geo_radius_list_ids", Type::List(Box::new(Type::Int)));
    st.add("lotame_segments", Type::List(Box::new(Type::Int)));
    st.add("krux_segments", Type::List(Box::new(Type::Str)));
    st.add("adobe_aam_segments", Type::List(Box::new(Type::Int)));
    st.add("nielsen_segments", Type::List(Box::new(Type::Int)));
    st.add("applist_ids", Type::List(Box::new(Type::Int)));
    st.add("impression_type", Type::Str);
    st.add("publisher_id", Type::Int);
    st.add("ssl", Type::Bool);
    st.add("device_set_historical_ids", Type::List(Box::new(Type::Int)));
    //st.add("content_provider_instant_pairs", Type::Segments);
    st.add("content_provider_historical_ids", Type::List(Box::new(Type::Int)));
    st.add("width", Type::Int);
    st.add("height", Type::Int);
    st.add("types", Type::List(Box::new(Type::Int)));
    st.add("position", Type::Str);
    st.add("deal_ids", Type::List(Box::new(Type::Str)));
    st.add("exchange_seat_ids", Type::List(Box::new(Type::Int)));
    st.add("private", Type::Bool);
    st.add("video_start_delay", Type::Int);
    st.add("player_w", Type::Int);
    st.add("player_h", Type::Int);
    st.add("player_premium", Type::Bool);
    //st.add("frequency_caps".to_string, Type::Frequency_caps);

    // Functions
    st.add("starts_with",
           Type::Func(vec![Type::Str, Type::Str], Box::new(Type::Bool)));
    st.add("ends_with",
           Type::Func(vec![Type::Str, Type::Str], Box::new(Type::Bool)));
    st.add("contains",
           Type::Func(vec![Type::Str, Type::Str], Box::new(Type::Bool)));
    st.add("within_frequency_cap",
           Type::Func(vec![Type::Str, Type::Str, Type::Int, Type::Int], Box::new(Type::Bool)));
    st.add("segment_before",
           Type::Func(vec![Type::Int, Type::Int], Box::new(Type::Bool)));
    st.add("segment_within",
           Type::Func(vec![Type::Int, Type::Int], Box::new(Type::Bool)));

    return st;
}
