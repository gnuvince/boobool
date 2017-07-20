extern crate boobool;

use boobool::errors::Result;
use boobool::parser;
use boobool::scanner;
use boobool::transformers;
use boobool::typechecker;
use boobool::types::{Type, Nullable, Symtable};

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
        let texpr = transformers::transform(texpr, &st);
        println!("{}", texpr);
        buf.clear();
    }
    return Ok(());
}


/// Creates a symbol table for type checking.
fn make_symtable() -> Symtable {
    let mut st = Symtable::new();

    // Variables
    st.add("bidder_ids", Type::List(Box::new(Type::Int)), Nullable::No);
    st.add("exchange", Type::Int, Nullable::No);
    st.add("member_id", Type::Int, Nullable::No);
    st.add("country", Type::Str, Nullable::Yes);
    st.add("region", Type::Str, Nullable::Yes);
    st.add("city", Type::Str, Nullable::Yes);
    st.add("pc", Type::Str, Nullable::Yes);
    st.add("dma", Type::Int, Nullable::Yes);
    st.add("lg", Type::Str, Nullable::Yes);
    st.add("doubleclick_verticals", Type::List(Box::new(Type::Int)), Nullable::Yes);
    st.add("appnexus_categories", Type::List(Box::new(Type::Int)), Nullable::Yes);
    st.add("iab_categories", Type::List(Box::new(Type::Int)), Nullable::Yes);
    st.add("device_type_id", Type::Int, Nullable::No);
    st.add("device_id", Type::Int, Nullable::No);
    st.add("operating_system_id", Type::Int, Nullable::No);
    st.add("operating_system_variant_id", Type::Int, Nullable::No);
    st.add("browser_id", Type::Int, Nullable::No);
    st.add("browser_variant_id", Type::Int, Nullable::No);
    st.add("matched_user", Type::Bool, Nullable::No);
    st.add("latitude", Type::Float, Nullable::Yes);
    st.add("longitude", Type::Float, Nullable::Yes);
    st.add("device_located", Type::Bool, Nullable::No);
    st.add("ag_rank", Type::Int, Nullable::Yes);
    st.add("sitelist_ids", Type::List(Box::new(Type::Int)), Nullable::No);
    st.add("adgear_segments", Type::List(Box::new(Type::Int)), Nullable::Yes);
    //st.add("segments_with_timestamp", Type::Segments);
    st.add("ip", Type::Str, Nullable::Yes);
    st.add("now", Type::Int, Nullable::No);
    st.add("iplist_ids", Type::List(Box::new(Type::Int)), Nullable::Yes);
    st.add("ias_segments", Type::List(Box::new(Type::Int)), Nullable::Yes);
    st.add("exchange_seller_site_id", Type::Int, Nullable::Yes);
    st.add("geo_radius_list_ids", Type::List(Box::new(Type::Int)), Nullable::No);
    st.add("lotame_segments", Type::List(Box::new(Type::Int)), Nullable::Yes);
    st.add("krux_segments", Type::List(Box::new(Type::Str)), Nullable::Yes);
    st.add("adobe_aam_segments", Type::List(Box::new(Type::Int)), Nullable::Yes);
    st.add("nielsen_segments", Type::List(Box::new(Type::Int)), Nullable::Yes);
    st.add("applist_ids", Type::List(Box::new(Type::Int)), Nullable::No);
    st.add("impression_type", Type::Str, Nullable::No);
    st.add("publisher_id", Type::Int, Nullable::Yes);
    st.add("ssl", Type::Bool, Nullable::No);
    st.add("device_set_historical_ids", Type::List(Box::new(Type::Int)), Nullable::Yes);
    //st.add("content_provider_instant_pairs", Type::Segments);
    st.add("content_provider_historical_ids", Type::List(Box::new(Type::Int)), Nullable::Yes);
    st.add("width", Type::Int, Nullable::No);
    st.add("height", Type::Int, Nullable::No);
    st.add("types", Type::List(Box::new(Type::Int)), Nullable::No);
    st.add("position", Type::Str, Nullable::Yes);
    st.add("deal_ids", Type::List(Box::new(Type::Str)), Nullable::Yes);
    st.add("exchange_seat_ids", Type::List(Box::new(Type::Int)), Nullable::Yes);
    st.add("private", Type::Bool, Nullable::No);
    st.add("video_start_delay", Type::Int, Nullable::Yes);
    st.add("player_w", Type::Int, Nullable::Yes);
    st.add("player_h", Type::Int, Nullable::Yes);
    st.add("player_premium", Type::Bool, Nullable::Yes);
    //st.add("frequency_caps".to_string, Type::Frequency_caps);

    // Functions
    st.add("starts_with",
           Type::Func(vec![Type::Str, Type::Str], Box::new(Type::Bool)),
           Nullable::No
    );
    st.add("ends_with",
           Type::Func(vec![Type::Str, Type::Str], Box::new(Type::Bool)),
           Nullable::No
    );
    st.add("contains",
           Type::Func(vec![Type::Str, Type::Str], Box::new(Type::Bool)),
           Nullable::No
    );
    st.add("within_frequency_cap",
           Type::Func(vec![Type::Str, Type::Str, Type::Int, Type::Int], Box::new(Type::Bool)),
           Nullable::No
    );
    st.add("segment_before",
           Type::Func(vec![Type::Int, Type::Int], Box::new(Type::Bool)),
           Nullable::No
    );
    st.add("segment_within",
           Type::Func(vec![Type::Int, Type::Int], Box::new(Type::Bool)),
           Nullable::No
    );

    return st;
}
