use std::{
    env,
    path::{Path, PathBuf},
};

fn scan_rs_files( current_dir: impl AsRef<Path>, rs_files: &mut Vec<PathBuf> ) {
    if let Ok( entries ) = current_dir.as_ref().read_dir() {
        for entry in entries {
            if let Ok( entry ) = entry {
                let path = entry.path();
                if path.is_dir() {
                    scan_rs_files( path, rs_files );
                } else if let Some( extention ) = path.extension() {
                    if extention == "rs" {
                        rs_files.push( path );
                    }
                }
            }
        }
    }
}

fn main() {
    let rs_files = inwelling::inwelling()
        .sections
        .into_iter()
        .fold( Vec::new(), |mut rs_files, section| {
            let manifest_path = section.manifest.parent().unwrap();
            scan_rs_files( &manifest_path.join( "src"      ), &mut rs_files );
            scan_rs_files( &manifest_path.join( "examples" ), &mut rs_files );
            scan_rs_files( &manifest_path.join( "tests"    ), &mut rs_files );
            rs_files
        });

    let mut output = String::from( "structx_derive::scan_structx_from_source_files!{\n" );
    for rs_file in rs_files {
        output.push_str( &format!( "    \"{}\",\n", rs_file.to_str().unwrap() ));
    }
    output.push( '}' );

    let out_path = PathBuf::from( env::var( "OUT_DIR" )
        .expect( "$OUT_DIR should exist." ));
    std::fs::write( out_path.join( "bindings.rs" ), output )
        .expect( "bindings.rs should be generated." );
}
