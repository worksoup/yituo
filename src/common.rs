use proc_macro2::TokenStream;
use syn::LitStr;
use crate::map::{map_multi_lit, map_single_expr, map_single_lit, MyLit, MyLitEnum, replace};

/// 输入一个表达式 `expr`，生成 `M(expr)`,
/// `M` 为任意表达式序列，其中的 `$` 将被替换为 `expr`, `$$` 转义为 `$`.
pub fn map(input: TokenStream) -> TokenStream {
    map_single_expr(input, |expr| quote::ToTokens::to_token_stream(&expr))
        .collect::<String>()
        .parse()
        .unwrap()
}
/// 输入一个字面值 `n`，生成元组序列 `0..n`.
/// 最终生成表达式元组序列 `(M(0)..M(n))`, 见 [`map`](macro@map).
pub fn for_each(input: TokenStream) -> TokenStream {
    let r = map_multi_lit(input, |mut lit, mapper| {
        let str = lit.pop();
        if let Some(MyLit(MyLitEnum::Str(s), span)) = str {
            let lit = replace(lit, s)
                .map(|s| MyLit(MyLitEnum::Str(s), span))
                .collect::<Vec<_>>();
            (lit, mapper)
        } else {
            str.map(|s| lit.push(s));
            (lit, mapper)
        }
    })
        .map(|s| s.parse::<proc_macro2::TokenStream>().unwrap());
    quote::quote! {
       (#(#r),*)
    }
}
/// 输入一个字面值 `n`，生成元组序列 `0..n`.
/// 最终生成表达式 `M(0..n)`, 见 [`map`](macro@map).
pub fn gen_seq(input: TokenStream) -> TokenStream {
    map_single_lit(input, |lit| {
        let e = lit.0;
        let e = match e {
            MyLitEnum::Byte(lit) => 0..lit as i128,
            MyLitEnum::Int(lit) => 0..lit,
            _ => {
                panic!("第一个参数应为整数的字面值！")
            }
        }
            .map(|s| s.to_string().parse::<proc_macro2::TokenStream>().unwrap());
        let r = quote::quote! {
           (#(#e),*)
        };
        Some(r.to_string())
    })
        .next()
        .unwrap()
        .parse()
        .unwrap()
}
/// 将字符串字面值转为实际的表达式（[`proc_macro::TokenStream`]）。
pub fn suppressor(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    syn::parse_macro_input!(input as LitStr)
        .value()
        .parse()
        .unwrap()
}
