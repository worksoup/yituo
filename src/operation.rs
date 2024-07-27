use crate::map::{map_double_lit, map_single_lit, MyLit, MyLitEnum};
use proc_macro2::TokenStream;
use syn::{LitByte, LitStr};

/// 输入字面值 `m`, `n`，生成字面值 `s` = `m` + `n`.
/// 最终生成表达式 `M(s)`, 见 [`map`](macro@map).
pub fn add(input: TokenStream) -> TokenStream {
    map_double_lit(input, |lit1, lit2| {
        let MyLit(lit1, span) = lit1;
        let lit2 = lit2.0;
        let r = match (lit1, lit2) {
            (MyLitEnum::Byte(lit1), MyLitEnum::Byte(lit2)) => {
                LitByte::new(lit1 + lit2, span).token().to_string()
            }
            (MyLitEnum::Int(lit1), MyLitEnum::Int(lit2)) => (lit1 + lit2).to_string(),
            (MyLitEnum::Float(lit1), MyLitEnum::Float(lit2)) => (lit1 + lit2).to_string(),
            (MyLitEnum::Str(lit1), MyLitEnum::Str(lit2)) => {
                LitStr::new(&(lit1 + lit2.as_str()), span)
                    .token()
                    .to_string()
            }
            _ => {
                panic!("字面值无法相加！")
            }
        };
        Some(r)
    })
    .next()
    .unwrap()
    .parse()
    .unwrap()
}
/// 输入字面值 `m`, `n`，生成字面值 `s` = `m` - `n`.
/// 最终生成表达式 `M(s)`, 见 [`map`](macro@map).
pub fn sub(input: TokenStream) -> TokenStream {
    map_double_lit(input, |lit1, lit2| {
        let MyLit(lit1, span) = lit1;
        let lit2 = lit2.0;
        let r = match (lit1, lit2) {
            (MyLitEnum::Byte(lit1), MyLitEnum::Byte(lit2)) => {
                LitByte::new(lit1 - lit2, span).token().to_string()
            }
            (MyLitEnum::Int(lit1), MyLitEnum::Int(lit2)) => (lit1 - lit2).to_string(),
            (MyLitEnum::Float(lit1), MyLitEnum::Float(lit2)) => (lit1 - lit2).to_string(),
            _ => {
                panic!("字面值无法相减！")
            }
        };
        Some(r)
    })
    .next()
    .unwrap()
    .parse()
    .unwrap()
}
/// 输入字面值 `m`, `n`，生成字面值 `r` = `m` * `n`.
/// 最终生成表达式 `M(r)`, 见 [`map`](macro@map).
pub fn mul(input: TokenStream) -> TokenStream {
    map_double_lit(input, |lit1, lit2| {
        let MyLit(lit1, span) = lit1;
        let lit2 = lit2.0;
        let r = match (lit1, lit2) {
            (MyLitEnum::Byte(lit1), MyLitEnum::Byte(lit2)) => {
                LitByte::new(lit1 * lit2, span).token().to_string()
            }
            (MyLitEnum::Int(lit1), MyLitEnum::Int(lit2)) => (lit1 * lit2).to_string(),
            (MyLitEnum::Float(lit1), MyLitEnum::Float(lit2)) => (lit1 * lit2).to_string(),
            _ => {
                panic!("字面值无法相乘！")
            }
        };
        Some(r)
    })
    .next()
    .unwrap()
    .parse()
    .unwrap()
}
/// 输入字面值 `m`, `n`，生成字面值 `r` = `m` / `n`.
/// 最终生成表达式 `M(r)`, 见 [`map`](macro@map).
pub fn div(input: TokenStream) -> TokenStream {
    map_double_lit(input, |lit1, lit2| {
        let MyLit(lit1, span) = lit1;
        let lit2 = lit2.0;
        let r = match (lit1, lit2) {
            (MyLitEnum::Byte(lit1), MyLitEnum::Byte(lit2)) => {
                LitByte::new(lit1 / lit2, span).token().to_string()
            }
            (MyLitEnum::Int(lit1), MyLitEnum::Int(lit2)) => (lit1 / lit2).to_string(),
            (MyLitEnum::Float(lit1), MyLitEnum::Float(lit2)) => (lit1 / lit2).to_string(),
            _ => {
                panic!("字面值无法相除！")
            }
        };
        Some(r)
    })
    .next()
    .unwrap()
    .parse()
    .unwrap()
}
/// 输入字面值 `a`, `b`，生成字面值 `m` = `a` % `b`.
/// 最终生成表达式 `M(m)`, 见 [`map`](macro@map).
pub fn modular(input: TokenStream) -> TokenStream {
    map_double_lit(input, |lit1, lit2| {
        let MyLit(lit1, span) = lit1;
        let lit2 = lit2.0;
        let r = match (lit1, lit2) {
            (MyLitEnum::Byte(lit1), MyLitEnum::Byte(lit2)) => {
                LitByte::new(lit1 % lit2, span).token().to_string()
            }
            (MyLitEnum::Int(lit1), MyLitEnum::Int(lit2)) => (lit1 % lit2).to_string(),
            (MyLitEnum::Float(lit1), MyLitEnum::Float(lit2)) => (lit1 % lit2).to_string(),
            _ => {
                panic!("字面值无法相除！")
            }
        };
        Some(r)
    })
    .next()
    .unwrap()
    .parse()
    .unwrap()
}
/// 输入字面值 `a`, `b`，生成字面值 `r` = `a` << `b`.
/// 最终生成表达式 `M(r)`, 见 [`map`](macro@map).
pub fn lsh(input: TokenStream) -> TokenStream {
    map_double_lit(input, |lit1, lit2| {
        let MyLit(lit1, span) = lit1;
        let lit2 = lit2.0;
        let r = match (lit1, lit2) {
            (MyLitEnum::Byte(lit1), MyLitEnum::Byte(lit2)) => {
                LitByte::new(lit1 << lit2, span).token().to_string()
            }
            (MyLitEnum::Int(lit1), MyLitEnum::Int(lit2)) => (lit1 << lit2).to_string(),
            _ => {
                panic!("字面值无法相除！")
            }
        };
        Some(r)
    })
    .next()
    .unwrap()
    .parse()
    .unwrap()
}
/// 输入字面值 `a`, `b`，生成字面值 `r` = `a` >> `b`.
/// 最终生成表达式 `M(r)`, 见 [`map`](macro@map).
pub fn rsh(input: TokenStream) -> TokenStream {
    map_double_lit(input, |lit1, lit2| {
        let MyLit(lit1, span) = lit1;
        let lit2 = lit2.0;
        let r = match (lit1, lit2) {
            (MyLitEnum::Byte(lit1), MyLitEnum::Byte(lit2)) => {
                LitByte::new(lit1 >> lit2, span).token().to_string()
            }
            (MyLitEnum::Int(lit1), MyLitEnum::Int(lit2)) => (lit1 >> lit2).to_string(),
            _ => {
                panic!("字面值无法相除！")
            }
        };
        Some(r)
    })
    .next()
    .unwrap()
    .parse()
    .unwrap()
}
/// 输入一个字面值 `n`，生成字面值 `r` = `!n`.
/// 最终生成表达式 `M(r)`, 见 [`map`](macro@map).
pub fn not(input: TokenStream) -> TokenStream {
    map_single_lit(input, |lit| {
        let MyLit(e, span) = lit;
        let e = match e {
            MyLitEnum::Byte(b) => LitByte::new(!b, span).token().to_string(),
            MyLitEnum::Int(v) => (!v).to_string(),
            MyLitEnum::Bool(v) => (!v).to_string(),
            _ => panic!("该类型不支持取反！"),
        };
        Some(e)
    })
    .next()
    .unwrap()
    .parse()
    .unwrap()
}
/// 输入字面值 `a`, `b`，生成字面值 `r` = `a` & `b` 或 `a` && `b`.
/// 最终生成表达式 `M(r)`, 见 [`map`](macro@map).
pub fn and(input: TokenStream) -> TokenStream {
    map_double_lit(input, |lit1, lit2| {
        let MyLit(lit1, span) = lit1;
        let lit2 = lit2.0;
        let r = match (lit1, lit2) {
            (MyLitEnum::Byte(lit1), MyLitEnum::Byte(lit2)) => {
                LitByte::new(lit1 & lit2, span).token().to_string()
            }
            (MyLitEnum::Int(lit1), MyLitEnum::Int(lit2)) => (lit1 & lit2).to_string(),
            (MyLitEnum::Bool(lit1), MyLitEnum::Bool(lit2)) => (lit1 && lit2).to_string(),
            _ => {
                panic!("字面值无法进行与运算！")
            }
        };
        Some(r)
    })
    .next()
    .unwrap()
    .parse()
    .unwrap()
}
/// 输入字面值 `a`, `b`，生成字面值 `r` = `a` | `b` 或 `a` || `b`.
/// 最终生成表达式 `M(r)`, 见 [`map`](macro@map).
pub fn or(input: TokenStream) -> TokenStream {
    map_double_lit(input, |lit1, lit2| {
        let MyLit(lit1, span) = lit1;
        let lit2 = lit2.0;
        let r = match (lit1, lit2) {
            (MyLitEnum::Byte(lit1), MyLitEnum::Byte(lit2)) => {
                LitByte::new(lit1 | lit2, span).token().to_string()
            }
            (MyLitEnum::Int(lit1), MyLitEnum::Int(lit2)) => (lit1 | lit2).to_string(),
            (MyLitEnum::Bool(lit1), MyLitEnum::Bool(lit2)) => (lit1 || lit2).to_string(),
            _ => {
                panic!("字面值无法进行或运算！")
            }
        };
        Some(r)
    })
    .next()
    .unwrap()
    .parse()
    .unwrap()
}
/// 输入字面值 `a`, `b`，生成字面值 `r` = `a` ^ `b` 或 `a` != `b`.
/// 最终生成表达式 `M(r)`, 见 [`map`](macro@map).
pub fn xor(input: TokenStream) -> TokenStream {
    map_double_lit(input, |lit1, lit2| {
        let MyLit(lit1, span) = lit1;
        let lit2 = lit2.0;
        let r = match (lit1, lit2) {
            (MyLitEnum::Byte(lit1), MyLitEnum::Byte(lit2)) => {
                LitByte::new(lit1 ^ lit2, span).token().to_string()
            }
            (MyLitEnum::Int(lit1), MyLitEnum::Int(lit2)) => (lit1 ^ lit2).to_string(),
            (MyLitEnum::Bool(lit1), MyLitEnum::Bool(lit2)) => (lit1 != lit2).to_string(),
            _ => {
                panic!("字面值无法进行异或运算！")
            }
        };
        Some(r)
    })
    .next()
    .unwrap()
    .parse()
    .unwrap()
}
macro def_compare($n:ident, $p:tt) {
    pub fn $n(input: TokenStream) -> TokenStream {
        map_double_lit(input, |lit1, lit2| {
            let lit1 = lit1.0;
            let lit2 = lit2.0;
            let r = match (lit1, lit2) {
                (MyLitEnum::Byte(lit1), MyLitEnum::Byte(lit2)) => (lit1 $p lit2).to_string(),
                (MyLitEnum::Int(lit1), MyLitEnum::Int(lit2)) => (lit1 $p lit2).to_string(),
                (MyLitEnum::Float(lit1), MyLitEnum::Float(lit2)) => (lit1 $p lit2).to_string(),
                (MyLitEnum::Char(lit1), MyLitEnum::Char(lit2)) => (lit1 $p lit2).to_string(),
                (MyLitEnum::Str(lit1), MyLitEnum::Str(lit2)) => (lit1 $p lit2).to_string(),
                (MyLitEnum::CStr(lit1), MyLitEnum::CStr(lit2)) => (lit1 $p lit2).to_string(),
                (MyLitEnum::ByteStr(lit1), MyLitEnum::ByteStr(lit2)) => (lit1 $p lit2).to_string(),
                _ => {
                    panic!("字面值无法比较！")
                }
            };
            Some(r)
        })
        .next()
        .unwrap()
        .parse()
        .unwrap()
    }
}
def_compare!(gt,>);
def_compare!(ge,>=);
def_compare!(lt,<);
def_compare!(le,<=);
def_compare!(eq,==);
def_compare!(ne,!=);
