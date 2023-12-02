
// thanks https://stackoverflow.com/questions/74841526/why-does-stditerpeekablepeek-mutably-borrow-the-self-argument

use std::str::CharIndices;

struct CachedLookahead<I: Iterator, const SIZE: usize> {
    iter: I,
    next_items: [Option<I::Item>; SIZE],
}

impl<I: Iterator, const SIZE: usize> CachedLookahead<I, SIZE> {
    pub fn new(mut iter: I) -> Self {
        Self { next_items: core::array::from_fn(|_| iter.next()), iter }
    }
    pub fn peek(&self) -> Option<&I::Item> {
        self.next_items[0].as_ref()
    }
    pub fn peek_n(&self, n: usize) -> Option<&I::Item> {
        self.next_items[n].as_ref()
    }
}

impl<I: Iterator, const SIZE: usize> Iterator for CachedLookahead<I, SIZE> {
    type Item = I::Item;
    fn next(&mut self) -> Option<Self::Item> {
        self.next_items.rotate_left(1);
        std::mem::replace(&mut self.next_items[SIZE - 1], self.iter.next())
    }
}

struct CharIndicesWithOffset<'s> {
    src: &'s str,
    iter: CharIndices<'s>,
    offset: usize,
}

impl<'s> CharIndicesWithOffset<'s> {
    pub fn new(src: &'s str) -> Self {
        Self {
            src,
            iter: src.char_indices(),
            offset: 0,
        }
    }
}

impl<'s> Iterator for CharIndicesWithOffset<'s> {
    type Item = char;
    fn next(&mut self) -> Option<Self::Item> {
        let Some((i, n)) = self.iter.next() else {
            self.offset = self.src.len();
            return None;
        };
        self.offset = i;
        Some(n)
    }
}

pub struct CharIter<'s>(CachedLookahead<CharIndicesWithOffset<'s>, 2>);

impl<'s> CharIter<'s> {
    pub fn new(src: &'s str) -> Self {
        Self(CachedLookahead::new(CharIndicesWithOffset::new(src)))
    }
    pub fn offset(&self) -> usize {
        self.0.iter.offset
    }
    pub fn src_str(&self) -> &'s str {
        self.0.iter.src
    }
    pub fn peek(&self) -> Option<char> {
        self.0.peek().copied()
    }
    pub fn peek1(&self) -> Option<char> {
        self.0.peek_n(1).copied()
    }
}

impl<'s> Iterator for CharIter<'s> {
    type Item = char;
    fn next(&mut self) -> Option<Self::Item> {
        self.0.next()
    }
}
