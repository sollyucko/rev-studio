#![allow(incomplete_features)]
#![feature(array_value_iter)]
#![feature(const_generics)]
#![feature(const_generic_impls_guard)]
#![feature(iter_map_while)]

pub mod ast;
pub use self::ast::*;

use arrayvec::{Array, ArrayVec};
use rev_studio_arch_pvm::Instruction;
use std::array;
use std::cmp::Ordering;
use std::sync::Arc;

// Based on:
// - <https://github.com/python/cpython/blob/master/Python/compile.c>

mod names {
    use super::ast::Name;
    use lazy_static::lazy_static;
    use std::sync::Arc;

    macro_rules! names {
		($($ident:ident,)*) => {
			lazy_static! {
				$(
					pub static ref $ident: Name = Name(Arc::new(String::from(concat!("$", stringify!($ident)))));
				)*
			}
		}
	}

    names! {
        GET_ITER,
        GET_YIELD_FROM_ITER,
    }
}

pub enum Element {
    Instruction(Instruction),
    Expression(Expression),
}

fn replace_none<T>(option: &mut Option<T>, value: T) {
    assert!(option.is_none());
    *option = Some(value);
}

macro_rules! replace_none {
	($($lhs:expr => $rhs:expr;)*) => {
		$(replace_none(&mut $lhs, $rhs);)*
	}
}

fn try_pop_expr(stack: &mut Vec<Option<Element>>, i: usize) -> Option<(usize, Expression)> {
    for (i, tos) in stack[0..i].iter_mut().enumerate().rev() {
        match tos.take() {
            Some(Element::Expression(expr)) => return Some((i, expr)),
            x @ Some(Element::Instruction(_)) => {
                *tos = x;
                return None;
            }
            None => {}
        }
    }
    unimplemented!("Cannot call try_pop_expr on an empty stack");
}

fn try_pop_exprs<const N: usize>(
    stack: &mut Vec<Option<Element>>,
    mut i: usize,
) -> Option<[(usize, Expression); N]>
where
    [(usize, Expression); N]: Array<Item = (usize, Expression)>,
{
    (0..N)
        .map_while(|_| {
            let (j, expr) = try_pop_expr(stack, i)?;
            i = j;
            Some((j, expr))
        })
        .collect::<ArrayVec<[_; N]>>()
        .into_inner()
        .map_or_else(
            |x| {
                for (j, expr) in x {
                    replace_none! { stack[j] => Element::Expression(expr); }
                }
                None
            },
            Some,
        )
}

fn modify_tos(
    i: usize,
    stack: &mut Vec<Option<Element>>,
    side_effects: &mut Vec<Option<Statement>>,
    func: impl FnOnce(Expression) -> Expression,
) -> Option<()> {
    let (_, tos) = try_pop_expr(stack, i)?;
    let expr = Expression::Arc(Arc::new(func(tos)));
    replace_none! {
        stack[i] => Element::Expression(expr.clone());
        side_effects[i] => Statement::Expression(Box::new(expr));
    }
    Some(())
}

pub fn modify_toses<const N: usize>(
    i: usize,
    stack: &mut Vec<Option<Element>>,
    side_effects: &mut Vec<Option<Statement>>,
    func: impl FnOnce([Expression; N]) -> Expression,
) -> Option<()>
where
    [(usize, Expression); N]: Array<Item = (usize, Expression)> + array::LengthAtMost32,
    [Expression; N]: Array<Item = Expression>,
{
    let arr = try_pop_exprs::<N>(stack, i)?;
    let arr_tos = array::IntoIter::new(arr)
        .map(|(_, tos)| tos)
        .collect::<ArrayVec<_>>()
        .into_inner()
        .unwrap();
    let expr = Expression::Arc(Arc::new(func(arr_tos)));
    replace_none! {
        stack[i] => Element::Expression(expr.clone());
        side_effects[i] => Statement::Expression(Box::new(expr));
    }
    Some(())
}

fn process_instruction(
    inst: &Instruction,
    i: usize,
    stack: &mut Vec<Option<Element>>,
    side_effects: &mut Vec<Option<Statement>>,
    consts: &[Expression],
) -> Option<()> {
    match inst {
        Instruction::POP_TOP => {
            try_pop_expr(stack, i)?;
        }
        Instruction::ROT_TWO => {
            let [(j, tos), (j1, tos1)] = try_pop_exprs::<2>(stack, i)?;
            replace_none! {
                stack[j] => Element::Expression(tos1);
                stack[j1] => Element::Expression(tos);
            }
        }
        Instruction::ROT_THREE => {
            let [(j, tos), (j1, tos1), (j2, tos2)] = try_pop_exprs::<3>(stack, i)?;
            replace_none! {
                stack[j] => Element::Expression(tos1);
                stack[j1] => Element::Expression(tos2);
                stack[j2] => Element::Expression(tos);
            }
        }
        Instruction::ROT_FOUR => {
            let [(j, tos), (j1, tos1), (j2, tos2), (j3, tos3)] = try_pop_exprs::<4>(stack, i)?;
            replace_none! {
                stack[j] => Element::Expression(tos1);
                stack[j1] => Element::Expression(tos2);
                stack[j2] => Element::Expression(tos3);
                stack[j3] => Element::Expression(tos);
            }
        }
        Instruction::DUP_TOP => {
            let (j, tos) = try_pop_expr(stack, i)?;
            replace_none! {
                stack[j] => Element::Expression(tos.clone());
                stack[i] => Element::Expression(tos);
            }
        }
        Instruction::DUP_TOP_TWO => {
            let [(j, tos), (j1, tos1)] = try_pop_exprs::<2>(stack, i)?;
            replace_none! {
                stack[j] => Element::Expression(tos.clone());
                stack[j1] => Element::Expression(tos1.clone());
                stack[i] => Element::Expression(tos);
            }
            if i + 1 < stack.len() && stack[i + 1].is_none() {
                replace_none! { stack[i+1] => Element::Expression(tos1); }
            } else {
                stack.insert(i + 1, Some(Element::Expression(tos1)));
                side_effects.insert(i + 1, None);
            }
        }
        Instruction::UNARY_POSITIVE => modify_tos(i, stack, side_effects, |tos| {
            Expression::UnaryOp(UnaryOp::Plus, Box::new(tos))
        })?,
        Instruction::UNARY_NEGATIVE => modify_tos(i, stack, side_effects, |tos| {
            Expression::UnaryOp(UnaryOp::Minus, Box::new(tos))
        })?,
        Instruction::UNARY_NOT => modify_tos(i, stack, side_effects, |tos| {
            Expression::UnaryOp(UnaryOp::LogicalNot, Box::new(tos))
        })?,
        Instruction::UNARY_INVERT => modify_tos(i, stack, side_effects, |tos| {
            Expression::UnaryOp(UnaryOp::BitwiseNot, Box::new(tos))
        })?,
        Instruction::GET_ITER => modify_tos(i, stack, side_effects, |tos| {
            Expression::Call(
                Box::new(Expression::Name(names::GET_ITER.clone())),
                Arguments {
                    positional: Box::new([PositionalArgument::Expression(tos)]),
                    keyword: Box::new([]),
                },
            )
        })?,
        Instruction::GET_YIELD_FROM_ITER => modify_tos(i, stack, side_effects, |tos| {
            Expression::Call(
                Box::new(Expression::Name(names::GET_YIELD_FROM_ITER.clone())),
                Arguments {
                    positional: Box::new([PositionalArgument::Expression(tos)]),
                    keyword: Box::new([]),
                },
            )
        })?,
        Instruction::BINARY_POWER => modify_toses(i, stack, side_effects, |[tos, tos1]| {
            Expression::BinaryOp(Box::new(tos1), BinaryOp::Pow, Box::new(tos))
        })?,
        Instruction::LOAD_CONST(idx) => {
            replace_none! { stack[i] => Element::Expression(consts[*idx as usize].clone()); }
        }
        _ => unimplemented!("{:?} has not yet been implemented", inst),
    }
    Some(())
}

#[must_use]
pub fn convert_instructions_to_ast(instructions: &[Instruction], consts: &[Expression]) -> Suite {
    // We need:
    // 1. Mapping `Instruction`s to slots for `Statement`s, preserving ordering
    // 2. Keeping ordering of `Expression`s and `Instruction`s
    //
    // The best approach I came up with is two parallel, sparse `Vec`s.
    let mut stack: Vec<Option<Element>> = instructions
        .iter()
        .map(|inst| Some(Element::Instruction(inst.clone())))
        .collect();
    let mut side_effects: Vec<Option<Statement>> = instructions.iter().map(|_| None).collect();
    let mut prev_num_instructions = instructions.len();

    loop {
        for i in 0..stack.len() {
            match stack[i].take() {
                Some(Element::Instruction(inst)) => {
                    match process_instruction(&inst, i, &mut stack, &mut side_effects, consts) {
                        Some(()) => {}
                        None => stack[i] = Some(Element::Instruction(inst)),
                    }
                }
                x => stack[i] = x,
            }
        }

        let num_instructions = stack
            .iter()
            .filter(|x| matches!(x, Some(Element::Instruction(_))))
            .count();
        match num_instructions {
            0 => {
                return Suite(side_effects.into_iter().filter_map(|y| y).collect());
            }
            x => match Ord::cmp(&x, &prev_num_instructions) {
                Ordering::Less => {}
                Ordering::Equal => panic!("Unable to fully simplify all instructions"),
                Ordering::Greater => panic!("How TF did more instructions get added?"),
            },
        }
        prev_num_instructions = num_instructions;
    }
}

pub fn simplify(suite: &mut Suite) {
    let vec = std::mem::take(&mut suite.0).into_vec();
    let suite_ref = &suite; // prevent accidental mutation, pt. 1

    let _suite_ref_2 = suite_ref; // prevent accidental mutation, pt. 2
    suite.0 = vec.into_boxed_slice();
}

// TODO: simplify `Arc`s

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn smoke_test() {
        assert_eq!(
            convert_instructions_to_ast(
                &[
                    Instruction::LOAD_CONST(0),
                    Instruction::LOAD_CONST(1),
                    Instruction::ROT_TWO,
                    Instruction::BINARY_POWER,
                ],
                &[
                    Expression::String(String::from("a").into_boxed_str()),
                    Expression::String(String::from("b").into_boxed_str()),
                ]
            ),
            Suite(Box::new([Statement::Expression(Box::new(
                Expression::Arc(Arc::new(Expression::BinaryOp(
                    Box::new(Expression::String(String::from("b").into_boxed_str())),
                    BinaryOp::Pow,
                    Box::new(Expression::String(String::from("a").into_boxed_str())),
                )))
            ))]))
        );
    }
}
