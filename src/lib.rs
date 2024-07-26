#![feature(let_chains)]
mod map;

use crate::map::{map_double_lit, map_single_expr, map_single_lit, MyLit, MyLitEnum};
use proc_macro::TokenStream;
use syn::{LitByte, LitStr};

/// 输入一个表达式 `expr`，生成 `M(expr)`,
/// `M` 为任意表达式序列，其中的 `$` 将被替换为 `expr`, `$$` 转义为 `$`.
#[proc_macro]
pub fn map(input: TokenStream) -> TokenStream {
    map_single_expr(input.into(), |expr| quote::ToTokens::to_token_stream(&expr))
        .collect::<String>()
        .parse()
        .unwrap()
}
/// 输入一个字面值 `n`，生成元组序列 `0..n`.
/// 最终生成表达式元组序列 `(M(0)..M(n))`, 见 [`map`](macro@map).
#[proc_macro]
pub fn map_each(input: TokenStream) -> TokenStream {
    map_single_lit(input.into(), |lit| {
        let e = lit.0;
        let e = match e {
            MyLitEnum::Byte(lit) => 0..lit as i128,
            MyLitEnum::Int(lit) => 0..lit,
            _ => {
                panic!("第一个参数应为整数的字面值！")
            }
        };
        let r = quote::quote! {
           (#(#e),*)
        };
        Some(r.to_string())
    })
    .next()
    .unwrap()
    .parse::<proc_macro2::TokenStream>()
    .unwrap()
    .into()
}
/// 输入一个字面值 `n`，生成元组序列 `0..n`.
/// 最终生成表达式 `M(0..n)`, 见 [`map`](macro@map).
#[proc_macro]
pub fn map_seq(input: TokenStream) -> TokenStream {
    let r = map_single_lit(input.into(), |lit| {
        let e = lit.0;
        match e {
            MyLitEnum::Byte(lit) => 0..lit as i128,
            MyLitEnum::Int(lit) => 0..lit,
            _ => {
                panic!("第一个参数应为整数的字面值！")
            }
        }
    })
    .map(|s| s.parse::<proc_macro2::TokenStream>().unwrap());
    let r = quote::quote! {
       (#(#r),*)
    };
    r.into()
}
/// 输入字面值 `m`, `n`，生成字面值 `s` = `m` + `n`.
/// 最终生成表达式 `M(s)`, 见 [`map`](macro@map).
#[proc_macro]
pub fn map_add(input: TokenStream) -> TokenStream {
    map_double_lit(input.into(), |lit1, lit2| {
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
#[proc_macro]
pub fn map_sub(input: TokenStream) -> TokenStream {
    map_double_lit(input.into(), |lit1, lit2| {
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
#[proc_macro]
pub fn map_mul(input: TokenStream) -> TokenStream {
    map_double_lit(input.into(), |lit1, lit2| {
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
#[proc_macro]
pub fn map_div(input: TokenStream) -> TokenStream {
    map_double_lit(input.into(), |lit1, lit2| {
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
#[proc_macro]
pub fn map_mod(input: TokenStream) -> TokenStream {
    map_double_lit(input.into(), |lit1, lit2| {
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
#[proc_macro]
pub fn map_lsh(input: TokenStream) -> TokenStream {
    map_double_lit(input.into(), |lit1, lit2| {
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
#[proc_macro]
pub fn map_rsh(input: TokenStream) -> TokenStream {
    map_double_lit(input.into(), |lit1, lit2| {
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
#[proc_macro]
pub fn map_not(input: TokenStream) -> TokenStream {
    map_single_lit(input.into(), |lit| {
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
#[proc_macro]
pub fn map_and(input: TokenStream) -> TokenStream {
    map_double_lit(input.into(), |lit1, lit2| {
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
#[proc_macro]
pub fn map_or(input: TokenStream) -> TokenStream {
    map_double_lit(input.into(), |lit1, lit2| {
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
#[proc_macro]
pub fn map_xor(input: TokenStream) -> TokenStream {
    map_double_lit(input.into(), |lit1, lit2| {
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
/// 输入字面值 `a`, `b`，生成 `bool` 字面值 `r` = `a` > `b`.
/// 最终生成表达式 `M(r)`, 见 [`map`](macro@map).
#[proc_macro]
pub fn map_gt(input: TokenStream) -> TokenStream {
    map_double_lit(input.into(), |lit1, lit2| {
        let lit1 = lit1.0;
        let lit2 = lit2.0;
        let r = match (lit1, lit2) {
            (MyLitEnum::Byte(lit1), MyLitEnum::Byte(lit2)) => (lit1 > lit2).to_string(),
            (MyLitEnum::Int(lit1), MyLitEnum::Int(lit2)) => (lit1 > lit2).to_string(),
            (MyLitEnum::Float(lit1), MyLitEnum::Float(lit2)) => (lit1 > lit2).to_string(),
            (MyLitEnum::Char(lit1), MyLitEnum::Char(lit2)) => (lit1 > lit2).to_string(),
            (MyLitEnum::Str(lit1), MyLitEnum::Str(lit2)) => (lit1 > lit2).to_string(),
            (MyLitEnum::CStr(lit1), MyLitEnum::CStr(lit2)) => (lit1 > lit2).to_string(),
            (MyLitEnum::ByteStr(lit1), MyLitEnum::ByteStr(lit2)) => (lit1 > lit2).to_string(),
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
/// 输入字面值 `a`, `b`，生成 `bool` 字面值 `r` = `a` >= `b`.
/// 最终生成表达式 `M(r)`, 见 [`map`](macro@map).
#[proc_macro]
pub fn map_ge(input: TokenStream) -> TokenStream {
    map_double_lit(input.into(), |lit1, lit2| {
        let lit1 = lit1.0;
        let lit2 = lit2.0;
        let r = match (lit1, lit2) {
            (MyLitEnum::Byte(lit1), MyLitEnum::Byte(lit2)) => (lit1 >= lit2).to_string(),
            (MyLitEnum::Int(lit1), MyLitEnum::Int(lit2)) => (lit1 >= lit2).to_string(),
            (MyLitEnum::Float(lit1), MyLitEnum::Float(lit2)) => (lit1 >= lit2).to_string(),
            (MyLitEnum::Char(lit1), MyLitEnum::Char(lit2)) => (lit1 >= lit2).to_string(),
            (MyLitEnum::Str(lit1), MyLitEnum::Str(lit2)) => (lit1 >= lit2).to_string(),
            (MyLitEnum::CStr(lit1), MyLitEnum::CStr(lit2)) => (lit1 >= lit2).to_string(),
            (MyLitEnum::ByteStr(lit1), MyLitEnum::ByteStr(lit2)) => (lit1 >= lit2).to_string(),
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
/// 输入字面值 `a`, `b`，生成 `bool` 字面值 `r` = `a` < `b`.
/// 最终生成表达式 `M(r)`, 见 [`map`](macro@map).
#[proc_macro]
pub fn map_lt(input: TokenStream) -> TokenStream {
    map_double_lit(input.into(), |lit1, lit2| {
        let lit1 = lit1.0;
        let lit2 = lit2.0;
        let r = match (lit1, lit2) {
            (MyLitEnum::Byte(lit1), MyLitEnum::Byte(lit2)) => (lit1 < lit2).to_string(),
            (MyLitEnum::Int(lit1), MyLitEnum::Int(lit2)) => (lit1 < lit2).to_string(),
            (MyLitEnum::Float(lit1), MyLitEnum::Float(lit2)) => (lit1 < lit2).to_string(),
            (MyLitEnum::Char(lit1), MyLitEnum::Char(lit2)) => (lit1 < lit2).to_string(),
            (MyLitEnum::Str(lit1), MyLitEnum::Str(lit2)) => (lit1 < lit2).to_string(),
            (MyLitEnum::CStr(lit1), MyLitEnum::CStr(lit2)) => (lit1 < lit2).to_string(),
            (MyLitEnum::ByteStr(lit1), MyLitEnum::ByteStr(lit2)) => (lit1 < lit2).to_string(),
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
/// 输入字面值 `a`, `b`，生成 `bool` 字面值 `r` = `a` <= `b`.
/// 最终生成表达式 `M(r)`, 见 [`map`](macro@map).
#[proc_macro]
pub fn map_le(input: TokenStream) -> TokenStream {
    map_double_lit(input.into(), |lit1, lit2| {
        let lit1 = lit1.0;
        let lit2 = lit2.0;
        let r = match (lit1, lit2) {
            (MyLitEnum::Byte(lit1), MyLitEnum::Byte(lit2)) => (lit1 <= lit2).to_string(),
            (MyLitEnum::Int(lit1), MyLitEnum::Int(lit2)) => (lit1 <= lit2).to_string(),
            (MyLitEnum::Float(lit1), MyLitEnum::Float(lit2)) => (lit1 <= lit2).to_string(),
            (MyLitEnum::Char(lit1), MyLitEnum::Char(lit2)) => (lit1 <= lit2).to_string(),
            (MyLitEnum::Str(lit1), MyLitEnum::Str(lit2)) => (lit1 <= lit2).to_string(),
            (MyLitEnum::CStr(lit1), MyLitEnum::CStr(lit2)) => (lit1 <= lit2).to_string(),
            (MyLitEnum::ByteStr(lit1), MyLitEnum::ByteStr(lit2)) => (lit1 <= lit2).to_string(),
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
/// 输入字面值 `a`, `b`，生成 `bool` 字面值 `r` = `a` == `b`.
/// 最终生成表达式 `M(r)`, 见 [`map`](macro@map).
#[proc_macro]
pub fn map_eq(input: TokenStream) -> TokenStream {
    map_double_lit(input.into(), |lit1, lit2| {
        let lit1 = lit1.0;
        let lit2 = lit2.0;
        let r = match (lit1, lit2) {
            (MyLitEnum::Byte(lit1), MyLitEnum::Byte(lit2)) => (lit1 == lit2).to_string(),
            (MyLitEnum::Int(lit1), MyLitEnum::Int(lit2)) => (lit1 == lit2).to_string(),
            (MyLitEnum::Float(lit1), MyLitEnum::Float(lit2)) => (lit1 == lit2).to_string(),
            (MyLitEnum::Char(lit1), MyLitEnum::Char(lit2)) => (lit1 == lit2).to_string(),
            (MyLitEnum::Str(lit1), MyLitEnum::Str(lit2)) => (lit1 == lit2).to_string(),
            (MyLitEnum::CStr(lit1), MyLitEnum::CStr(lit2)) => (lit1 == lit2).to_string(),
            (MyLitEnum::ByteStr(lit1), MyLitEnum::ByteStr(lit2)) => (lit1 == lit2).to_string(),
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
/// 输入字面值 `a`, `b`，生成 `bool` 字面值 `r` = `a` != `b`.
/// 最终生成表达式 `M(r)`, 见 [`map`](macro@map).
#[proc_macro]
pub fn map_ne(input: TokenStream) -> TokenStream {
    map_double_lit(input.into(), |lit1, lit2| {
        let lit1 = lit1.0;
        let lit2 = lit2.0;
        let r = match (lit1, lit2) {
            (MyLitEnum::Byte(lit1), MyLitEnum::Byte(lit2)) => (lit1 != lit2).to_string(),
            (MyLitEnum::Int(lit1), MyLitEnum::Int(lit2)) => (lit1 != lit2).to_string(),
            (MyLitEnum::Float(lit1), MyLitEnum::Float(lit2)) => (lit1 != lit2).to_string(),
            (MyLitEnum::Char(lit1), MyLitEnum::Char(lit2)) => (lit1 != lit2).to_string(),
            (MyLitEnum::Str(lit1), MyLitEnum::Str(lit2)) => (lit1 != lit2).to_string(),
            (MyLitEnum::CStr(lit1), MyLitEnum::CStr(lit2)) => (lit1 != lit2).to_string(),
            (MyLitEnum::ByteStr(lit1), MyLitEnum::ByteStr(lit2)) => (lit1 != lit2).to_string(),
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
/// 将字符串字面值转为实际的表达式（[`proc_macro::TokenStream`]）。
#[proc_macro]
pub fn suppressor(input: TokenStream) -> TokenStream {
    syn::parse_macro_input!(input as LitStr)
        .value()
        .parse()
        .unwrap()
}
