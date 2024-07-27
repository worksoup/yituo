#![feature(let_chains)]
#![feature(decl_macro)]

mod common;
mod map;
mod operation;
mod string;

use proc_macro::TokenStream;
use crate::map::convert;

/// 输入一个表达式 `expr`，生成 `M(expr)`,
/// `M` 为任意表达式序列，其中的 `$` 将被替换为 `expr`, `$$` 转义为 `$`.
#[proc_macro]
pub fn map(input: TokenStream) -> TokenStream {
    convert(common::map)(input)
}
/// 输入一个字面值 `n`，生成元组序列 `0..n`.
/// 最终生成表达式元组序列 `(M(0)..M(n))`, 见 [`map`](macro@map).
#[proc_macro]
pub fn map_each(input: TokenStream) -> TokenStream {
    convert(common::for_each)(input)
}
/// 输入一个字面值 `n`，生成元组序列 `0..n`.
/// 最终生成表达式 `M(0..n)`, 见 [`map`](macro@map).
#[proc_macro]
pub fn map_seq(input: TokenStream) -> TokenStream {
    convert(common::gen_seq)(input)
}
/// 输入字面值 `m`, `n`，生成字面值 `s` = `m` + `n`.
/// 最终生成表达式 `M(s)`, 见 [`map`](macro@map).
#[proc_macro]
pub fn map_add(input: TokenStream) -> TokenStream {
    convert(operation::add)(input)
}
/// 输入字面值 `m`, `n`，生成字面值 `s` = `m` - `n`.
/// 最终生成表达式 `M(s)`, 见 [`map`](macro@map).
#[proc_macro]
pub fn map_sub(input: TokenStream) -> TokenStream {
    convert(operation::sub)(input)
}
/// 输入字面值 `m`, `n`，生成字面值 `r` = `m` * `n`.
/// 最终生成表达式 `M(r)`, 见 [`map`](macro@map).
#[proc_macro]
pub fn map_mul(input: TokenStream) -> TokenStream {
    convert(operation::mul)(input)
}
/// 输入字面值 `m`, `n`，生成字面值 `r` = `m` / `n`.
/// 最终生成表达式 `M(r)`, 见 [`map`](macro@map).
#[proc_macro]
pub fn map_div(input: TokenStream) -> TokenStream {
    convert(operation::div)(input)
}
/// 输入字面值 `a`, `b`，生成字面值 `m` = `a` % `b`.
/// 最终生成表达式 `M(m)`, 见 [`map`](macro@map).
#[proc_macro]
pub fn map_mod(input: TokenStream) -> TokenStream {
    convert(operation::modular)(input)
}
/// 输入字面值 `a`, `b`，生成字面值 `r` = `a` << `b`.
/// 最终生成表达式 `M(r)`, 见 [`map`](macro@map).
#[proc_macro]
pub fn map_lsh(input: TokenStream) -> TokenStream {
    convert(operation::lsh)(input)
}
/// 输入字面值 `a`, `b`，生成字面值 `r` = `a` >> `b`.
/// 最终生成表达式 `M(r)`, 见 [`map`](macro@map).
#[proc_macro]
pub fn map_rsh(input: TokenStream) -> TokenStream {
    convert(operation::rsh)(input)
}
/// 输入一个字面值 `n`，生成字面值 `r` = `!n`.
/// 最终生成表达式 `M(r)`, 见 [`map`](macro@map).
#[proc_macro]
pub fn map_not(input: TokenStream) -> TokenStream {
    convert(operation::not)(input)
}
/// 输入字面值 `a`, `b`，生成字面值 `r` = `a` & `b` 或 `a` && `b`.
/// 最终生成表达式 `M(r)`, 见 [`map`](macro@map).
#[proc_macro]
pub fn map_and(input: TokenStream) -> TokenStream {
    convert(operation::and)(input)
}
/// 输入字面值 `a`, `b`，生成字面值 `r` = `a` | `b` 或 `a` || `b`.
/// 最终生成表达式 `M(r)`, 见 [`map`](macro@map).
#[proc_macro]
pub fn map_or(input: TokenStream) -> TokenStream {
    convert(operation::or)(input)
}
/// 输入字面值 `a`, `b`，生成字面值 `r` = `a` ^ `b` 或 `a` != `b`.
/// 最终生成表达式 `M(r)`, 见 [`map`](macro@map).
#[proc_macro]
pub fn map_xor(input: TokenStream) -> TokenStream {
    convert(operation::xor)(input)
}
/// 输入字面值 `a`, `b`，生成 `bool` 字面值 `r` = `a` > `b`.
/// 最终生成表达式 `M(r)`, 见 [`map`](macro@map).
#[proc_macro]
pub fn map_gt(input: TokenStream) -> TokenStream {
    convert(operation::gt)(input)
}
/// 输入字面值 `a`, `b`，生成 `bool` 字面值 `r` = `a` >= `b`.
/// 最终生成表达式 `M(r)`, 见 [`map`](macro@map).
#[proc_macro]
pub fn map_ge(input: TokenStream) -> TokenStream {
    convert(operation::ge)(input)
}
/// 输入字面值 `a`, `b`，生成 `bool` 字面值 `r` = `a` < `b`.
/// 最终生成表达式 `M(r)`, 见 [`map`](macro@map).
#[proc_macro]
pub fn map_lt(input: TokenStream) -> TokenStream {
    convert(operation::lt)(input)
}
/// 输入字面值 `a`, `b`，生成 `bool` 字面值 `r` = `a` <= `b`.
/// 最终生成表达式 `M(r)`, 见 [`map`](macro@map).
#[proc_macro]
pub fn map_le(input: TokenStream) -> TokenStream {
    convert(operation::le)(input)
}
/// 输入字面值 `a`, `b`，生成 `bool` 字面值 `r` = `a` == `b`.
/// 最终生成表达式 `M(r)`, 见 [`map`](macro@map).
#[proc_macro]
pub fn map_eq(input: TokenStream) -> TokenStream {
    convert(operation::eq)(input)
}
/// 输入字面值 `a`, `b`，生成 `bool` 字面值 `r` = `a` != `b`.
/// 最终生成表达式 `M(r)`, 见 [`map`](macro@map).
#[proc_macro]
pub fn map_ne(input: TokenStream) -> TokenStream {
    convert(operation::ne)(input)
}
/// 将字符串字面值转为实际的表达式（[`TokenStream`]）。
#[proc_macro]
pub fn suppressor(input: TokenStream) -> TokenStream {
    common::suppressor(input)
}
