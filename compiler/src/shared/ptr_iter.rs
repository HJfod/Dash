
use std::marker::PhantomData;

pub struct PtrChainIter<'t, T, F: FnMut(&'t T) -> *mut T> {
    current: *mut T,
    produce_next: F,
    _marker: PhantomData<&'t T>,
}

impl<'t, T, F: FnMut(&'t T) -> *mut T> PtrChainIter<'t, T, F> {
    pub fn new(first: *mut T, produce_next: F) -> Self {
        Self { current: first, produce_next, _marker: PhantomData }
    }
}

impl<'t, T, F: FnMut(&'t T) -> *mut T> Iterator for PtrChainIter<'t, T, F> {
    type Item = &'t mut T;
    fn next(&mut self) -> Option<Self::Item> {
        let ret = unsafe { self.current.as_mut()? };
        self.current = self.produce_next(ret);
        Some(ret)
    }
}
