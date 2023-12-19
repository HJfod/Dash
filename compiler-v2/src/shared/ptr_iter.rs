
use std::{marker::PhantomData, ptr::NonNull};

pub struct PtrChainIter<'t, T, F: FnMut(&T) -> Option<NonNull<T>>> {
    current: *mut T,
    produce_next: F,
    _marker: PhantomData<&'t T>,
}

impl<'t, T, F: FnMut(&T) -> Option<NonNull<T>>> PtrChainIter<'t, T, F> {
    pub fn new(first: NonNull<T>, produce_next: F) -> Self {
        Self { current: first.as_ptr(), produce_next, _marker: PhantomData }
    }
}

impl<'t, T, F: FnMut(&T) -> Option<NonNull<T>>> Iterator for PtrChainIter<'t, T, F> {
    type Item = &'t mut T;
    fn next(&mut self) -> Option<Self::Item> {
        let ret = unsafe { self.current.as_mut()? };
        self.current = (self.produce_next)(ret).map(|r| r.as_ptr()).unwrap_or(std::ptr::null_mut());
        Some(ret)
    }
}
