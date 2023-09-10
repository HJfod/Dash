use std::{hash::Hash, ops::Deref};


#[derive(Debug)]
pub struct RefWrapper<'t, T> {
    value: &'t T,
}

impl<'t, T> Copy for RefWrapper<'t, T> {}

impl<'t, T> Clone for RefWrapper<'t, T> {
    fn clone(&self) -> Self {
        Self { value: self.value }
    }
}

impl<'t, T> PartialEq for RefWrapper<'t, T> {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self, other)
    }
}

impl<'t, T> Eq for RefWrapper<'t, T> {}

impl<'t, T> Hash for RefWrapper<'t, T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        (self.value as *const T).hash(state)
    }
}

impl<'t, T> From<&'t T> for RefWrapper<'t, T> {
    fn from(value: &'t T) -> Self {
        Self { value }
    }
}

impl<'t, T> Deref for RefWrapper<'t, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        self.value
    }
}
