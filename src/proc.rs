extern crate proc_macro;
use proc_macro::{TokenStream};

#[proc_macro_derive(TokenSubType)]
pub fn derive_answer_fn(_item: TokenStream) -> TokenStream {
    match _item.into_iter().skip(1).next() {
        Some(name) => {
            let x = format!("
                impl From<{0:} > for TokenType {{
                    fn from(original: {0:}) -> TokenType {{
                        TokenType::{0:}(original)
                    }}
                }}
            ", name);
            println!("{}", x);
            match x.parse() {
                Ok(y) => {
                    println!("ok");
                    y
                },
                Err(err) => {
                    println!("{:?}", err);
                    TokenStream::new()
                }
            }

        },
        _ => TokenStream::new(),
    }
}
