
pub struct PtrChainIter<'t, T, F: FnMut(&'t T) -> *mut T> {
    current: *mut T,
    produce_next: F,
}

impl<'t, T, F: FnMut(&'t T) -> *mut T> PtrChainIter<'t, T, F> {
    pub fn new(first: *mut T, produce_next: F) -> Self {
        Self { current: first, produce_next }
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
