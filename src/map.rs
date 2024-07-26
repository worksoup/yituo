use proc_macro2::{Span, TokenStream, TokenTree};
use std::ffi::CString;
use syn::Lit;

#[derive(Debug)]
pub struct MyLit(pub MyLitEnum, pub Span);
#[derive(Default, Debug)]
pub enum MyLitEnum {
    /// A UTF-8 string literal: `"foo"`.
    Str(String),

    /// A byte string literal: `b"foo"`.
    ByteStr(Vec<u8>),

    /// A nul-terminated C-string literal: `c"foo"`.
    CStr(CString),

    /// A byte literal: `b'f'`.
    Byte(u8),

    /// A character literal: `'a'`.
    Char(char),

    /// An integer literal: `1` or `1u16`.
    Int(i128),

    /// A floating point literal: `1f64` or `1.0e10f64`.
    ///
    /// Must be finite. May not be infinite or NaN.
    Float(f64),

    /// A boolean literal: `true` or `false`.
    Bool(bool),

    /// A raw token literal not interpreted by Syn.
    #[default]
    None,
}
fn lit_2_my_lit(lit: Lit, span: Span, negative_value: bool) -> MyLit {
    match &lit {
        Lit::Int(_) => {}
        _ => {
            if negative_value {
                panic!("意外的负号！")
            }
        }
    }
    let lit = match lit {
        Lit::Str(s) => MyLitEnum::Str(s.value()),
        Lit::ByteStr(s) => MyLitEnum::ByteStr(s.value()),
        Lit::CStr(s) => MyLitEnum::CStr(s.value()),
        Lit::Byte(b) => MyLitEnum::Byte(b.value()),
        Lit::Char(c) => MyLitEnum::Char(c.value()),
        Lit::Int(v) => {
            let mut v = v.base10_parse::<u64>().unwrap() as i128;
            if negative_value {
                v = -v;
            }
            MyLitEnum::Int(v)
        }
        Lit::Float(v) => {
            if negative_value {
                MyLitEnum::Float(-v.base10_parse::<f64>().unwrap())
            } else {
                MyLitEnum::Float(v.base10_parse().unwrap())
            }
        }
        Lit::Bool(v) => MyLitEnum::Bool(v.value),
        _ => MyLitEnum::None,
    };
    MyLit(lit, span)
}
pub fn map_single_lit<T, F, I>(input: TokenStream, f: F) -> impl Iterator<Item = String>
where
    T: ToString,
    F: Fn(MyLit) -> I,
    I: IntoIterator<Item = T>,
{
    let mut negative_value = false;
    let mut value = None;
    let mut mapper = Vec::new();
    let mut count = 0;
    for expr in input {
        match count {
            0 => {
                if let TokenTree::Literal(lit) = expr {
                    let span = lit.span();
                    let lit = lit_2_my_lit(Lit::new(lit), span, negative_value);
                    value = Some(lit);
                } else if let TokenTree::Punct(punct) = expr
                    && punct.as_char() == '-'
                {
                    if !negative_value {
                        negative_value = true;
                        continue;
                    } else {
                        panic!("不支持多个负号！")
                    }
                } else {
                    panic!("第一个参数应为字面值！")
                }
                negative_value = false;
            }
            1 => {
                if let TokenTree::Punct(punct) = expr
                    && punct.as_char() == ','
                {
                } else {
                    panic!("格式错误！应为半角逗号。")
                }
            }
            _ => mapper.push(expr),
        }
        count += 1;
    }
    if count < 2 {
        panic!("格式错误！格式为：`macro_name!(Literal, M)`.")
    }
    replace(f(value.unwrap()), mapper)
}

pub fn map_single_expr<T, F, I>(input: TokenStream, f: F) -> impl Iterator<Item = String>
where
    T: ToString,
    F: Fn(TokenTree) -> I,
    I: IntoIterator<Item = T>,
{
    let mut value = None;
    let mut mapper = Vec::new();
    let mut count = 0;
    for expr in input {
        match count {
            0 => {
                value = Some(expr);
            }
            1 => {
                if let TokenTree::Punct(punct) = expr
                    && punct.as_char() == ','
                {
                } else {
                    panic!("格式错误！应为半角逗号。")
                }
            }
            _ => mapper.push(expr),
        }
        count += 1;
    }
    if count < 2 {
        panic!("格式错误！格式为：`macro_name!(Expr, M)`.")
    }
    replace(f(value.unwrap()), mapper)
}
pub fn map_double_lit<T, F, I>(input: TokenStream, f: F) -> impl Iterator<Item = String>
where
    T: ToString,
    F: Fn(MyLit, MyLit) -> I,
    I: IntoIterator<Item = T>,
{
    let mut values = Vec::new();
    let mut mapper = Vec::new();
    let mut count = 0;
    let mut negative_value = false;
    for expr in input {
        match count {
            0 | 2 => {
                if let TokenTree::Literal(lit) = expr {
                    let span = lit.span();
                    let lit = lit_2_my_lit(Lit::new(lit), span, negative_value);
                    values.push(lit);
                } else if let TokenTree::Punct(punct) = expr
                    && punct.as_char() == '-'
                {
                    if !negative_value {
                        negative_value = true;
                        continue;
                    } else {
                        panic!("不支持多个负号！")
                    }
                } else {
                    panic!("该参数应为字面值！")
                }
                negative_value = false;
            }
            1 | 3 => {
                if let TokenTree::Punct(punct) = expr
                    && punct.as_char() == ','
                {
                } else {
                    panic!("格式错误！应为半角逗号。")
                }
            }
            _ => mapper.push(expr),
        }
        count += 1;
    }
    if count < 4 {
        panic!("格式错误！格式为：`macro_name!(Literal1, Literal2, M)`.")
    }
    let first = values.pop().unwrap();
    let second = values.pop().unwrap();
    replace(f(first, second), mapper)
}

fn replace<T: ToString, I: IntoIterator<Item = T>>(
    iter: I,
    mapper: Vec<TokenTree>,
) -> impl Iterator<Item = String> {
    let token_stream_str = mapper.into_iter().collect::<TokenStream>().to_string();
    let mut modified_stream = Vec::new();
    let mut tmp_str = String::new();
    let mut find_dollar_sym = false;
    for c in token_stream_str.chars() {
        if c == '?' {
            if find_dollar_sym {
                tmp_str.push(c);
            }
            find_dollar_sym = !find_dollar_sym;
        } else {
            if find_dollar_sym {
                modified_stream.push(tmp_str.clone());
                tmp_str.clear();
                find_dollar_sym = false;
            }
            tmp_str.push(c);
        }
    }
    modified_stream.push(tmp_str);
    if find_dollar_sym {
        modified_stream.push(String::new());
    }
    iter.into_iter().map(move |n| {
        let s = n.to_string();
        modified_stream.join(&s)
    })
}
#[cfg(test)]
mod tests {
    #[test]
    fn test() {
        let x = 'c' > 'b';
        println!("{x}")
    }
}
