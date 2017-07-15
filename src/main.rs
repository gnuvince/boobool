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


fn make_symtable() -> Symtable {
    let mut st = Symtable::new();

    // Variables
    st.add("bidder_ids".to_string(), Type::List(Box::new(Type::Int)));
    st.add("exchange".to_string(), Type::Int);
    st.add("member_id".to_string(), Type::Int);
    st.add("country".to_string(), Type::Str);
    st.add("region".to_string(), Type::Str);
    st.add("city".to_string(), Type::Str);
    st.add("pc".to_string(), Type::Str);
    st.add("dma".to_string(), Type::Int);
    st.add("lg".to_string(), Type::Str);
    st.add("doubleclick_verticals".to_string(), Type::List(Box::new(Type::Int)));
    st.add("appnexus_categories".to_string(), Type::List(Box::new(Type::Int)));
    st.add("iab_categories".to_string(), Type::List(Box::new(Type::Int)));
    st.add("device_type_id".to_string(), Type::Int);
    st.add("device_id".to_string(), Type::Int);
    st.add("operating_system_id".to_string(), Type::Int);
    st.add("operating_system_variant_id".to_string(), Type::Int);
    st.add("browser_id".to_string(), Type::Int);
    st.add("browser_variant_id".to_string(), Type::Int);
    st.add("matched_user".to_string(), Type::Bool);
    st.add("latitude".to_string(), Type::Float);
    st.add("longitude".to_string(), Type::Float);
    st.add("device_located".to_string(), Type::Bool);
    st.add("ag_rank".to_string(), Type::Int);
    st.add("sitelist_ids".to_string(), Type::List(Box::new(Type::Int)));
    st.add("adgear_segments".to_string(), Type::List(Box::new(Type::Int)));
    //st.add("segments_with_timestamp".to_string(), Type::Segments);
    st.add("ip".to_string(), Type::Str);
    st.add("now".to_string(), Type::Int);
    st.add("iplist_ids".to_string(), Type::List(Box::new(Type::Int)));
    st.add("ias_segments".to_string(), Type::List(Box::new(Type::Int)));
    st.add("exchange_seller_site_id".to_string(), Type::Int);
    st.add("geo_radius_list_ids".to_string(), Type::List(Box::new(Type::Int)));
    st.add("lotame_segments".to_string(), Type::List(Box::new(Type::Int)));
    st.add("krux_segments".to_string(), Type::List(Box::new(Type::Str)));
    st.add("adobe_aam_segments".to_string(), Type::List(Box::new(Type::Int)));
    st.add("nielsen_segments".to_string(), Type::List(Box::new(Type::Int)));
    st.add("applist_ids".to_string(), Type::List(Box::new(Type::Int)));
    st.add("impression_type".to_string(), Type::Str);
    st.add("publisher_id".to_string(), Type::Int);
    st.add("ssl".to_string(), Type::Bool);
    st.add("device_set_historical_ids".to_string(), Type::List(Box::new(Type::Int)));
    //st.add("content_provider_instant_pairs".to_string(), Type::Segments);
    st.add("content_provider_historical_ids".to_string(), Type::List(Box::new(Type::Int)));
    st.add("width".to_string(), Type::Int);
    st.add("height".to_string(), Type::Int);
    st.add("types".to_string(), Type::List(Box::new(Type::Int)));
    st.add("position".to_string(), Type::Str);
    st.add("deal_ids".to_string(), Type::List(Box::new(Type::Str)));
    st.add("exchange_seat_ids".to_string(), Type::List(Box::new(Type::Int)));
    st.add("private".to_string(), Type::Bool);
    st.add("video_start_delay".to_string(), Type::Int);
    st.add("player_w".to_string(), Type::Int);
    st.add("player_h".to_string(), Type::Int);
    st.add("player_premium".to_string(), Type::Bool);
    //st.add("frequency_caps".to_string, Type::Frequency_caps);

    // Functions
    st.add("starts_with".to_string(),
           Type::Func(vec![Type::Str, Type::Str], Box::new(Type::Bool)));
    st.add("ends_with".to_string(),
           Type::Func(vec![Type::Str, Type::Str], Box::new(Type::Bool)));
    st.add("contains".to_string(),
           Type::Func(vec![Type::Str, Type::Str], Box::new(Type::Bool)));
    st.add("within_frequency_cap".to_string(),
           Type::Func(vec![Type::Str, Type::Str, Type::Int, Type::Int], Box::new(Type::Bool)));
    st.add("segment_before".to_string(),
           Type::Func(vec![Type::Int, Type::Int], Box::new(Type::Bool)));
    st.add("segment_within".to_string(),
           Type::Func(vec![Type::Int, Type::Int], Box::new(Type::Bool)));

    return st;
}
