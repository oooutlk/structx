#[cfg( test )]
mod tests {
    use structx::*;

    #[test]
    fn anonymous_struct() {
        let a = structx!{ width :  800, height: 600 };
        let b = structx!{ height:  600, width : 800 };
        let c = structx!{ width : 1024, height: 768 };
        assert_eq!( a, b );
        assert_ne!( a, c );
    }

    #[test]
    fn returns_anonymous_struct() {
        fn returns_structx( x: i32, y: i32 ) -> Structx!{ x: i32, y: i32 } {
            structx!{ x, y }
        }

        assert_eq!( returns_structx( 3, 4 ), structx!{ x:3, y:4 });

        #[derive( Debug, PartialEq )]
        struct Bar<T>( T );

        fn returns_generic_structx<T>( bar: Bar<T> ) -> Structx!{ bar: Bar<T>, baz: bool } {
            structx!{ bar, baz: true }
        }

        assert_eq!( returns_generic_structx( Bar("bar") ), structx!{ bar: Bar("bar"), baz: true });
    }

    #[test]
    fn named_argsuments() {
        use structx::named_args::*;

        #[named_args]
        fn with_owned_args( x: i32, y: String ) -> String {
            format!( "{} {}", x, y )
        }

        #[named_args]
        fn with_borrowed_args<'a>( x: bool, y: &'a str ) -> String {
            format!( "{} {}", x, y )
        }

        assert_eq!( with_owned_args( args!{ x: 3, y: "4".to_owned() }), "3 4".to_owned() );
        assert_eq!( with_borrowed_args( args!{ x: true, y: "false" }), "true false".to_owned() );
    }
}
