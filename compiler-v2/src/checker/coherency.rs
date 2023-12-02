
use crate::{parser::grammar::{TypeItem, Test}, shared::logger::{LoggerRef, Message, Level}};

use super::{ast::{Node, Child}, ty::Ty};

impl<'n, 'g> TypeItem<'g> {
    fn eval(&self, node: &Node<'n, 'g>) -> Ty<'n, 'g> {
        match self {
            TypeItem::Type(ty) => todo!(),
            TypeItem::Member(member) => {
                match node.child(member).unwrap() {
                    Child::Node(n) => n.resolved_ty.clone().unwrap(),
                    Child::Maybe(maybe) => maybe.as_ref()
                        .and_then(|n| n.resolved_ty.clone())
                        .unwrap_or(Ty::Invalid),
                    Child::List(_) => panic!("can't eval list child"),
                }
            }
        }
    }
}

impl<'n, 'g> Node<'n, 'g> {
    fn for_all_children<F: FnMut(&mut Node<'n, 'g>)>(&mut self, mut fun: F) {
        self.children.iter_mut().for_each(|child|
            match &mut child.1 {
                Child::Node(n) => fun(n),
                Child::Maybe(n) => if let Some(n) = n { fun(n) },
                Child::List(l) => l.iter_mut().for_each(&mut fun),
            }
        );
    }

    fn check_coherency(&mut self, logger: LoggerRef) {
        // If this AST node has already been resolved, that means that all of 
        // its children have also been resolved and no need to check them again
        if self.resolved_ty.is_some() {
            return;
        }

        // Check all children
        let mut some_unresolved = false;
        self.for_all_children(|n| {
            n.check_coherency(logger.clone());
            if n.resolved_ty.is_some() {
                some_unresolved = true;
            }
        });
        if some_unresolved {
            return;
        }

        // Run check tests, otherwise this is resolved and results in Invalid
        let Some(check) = self.check else {
            self.resolved_ty = Some(Ty::Invalid);
            return;
        };
        for test in &check.tests {
            match test {
                Test::Equal { equal } => {
                    let (a, b) = (equal.0.eval(self), equal.1.eval(self));
                    if !a.convertible(&b) {
                        logger.lock().unwrap().log(Message::new(
                            Level::Error,
                            format!("Type {a} is not convertible to type {b}"),
                            a.decl().or(b.decl()).expect(
                                "neither type in \"test\".\"equal\" has a declaration"
                            ).span().as_ref()
                        ));
                    }
                }
            }
        }
        self.resolved_ty = Some(check.result.eval(self));
    }
}
