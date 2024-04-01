use core::hash::Hash;
use std::{fmt::Debug, marker::PhantomData};

pub trait Idx: Copy + 'static + Eq + PartialEq + Debug + Hash {
    fn new(idx: usize) -> Self;
    fn index(self) -> usize;
    fn next(&self) -> Self;
}

#[derive(Debug)]
pub struct IndexVec<I: Idx, T> {
    pub raw: Vec<T>,
    _marker: PhantomData<fn(_: &I)>,
}

impl<I: Idx, T> IndexVec<I, T> {
    pub fn new() -> Self {
        IndexVec {
            raw: Vec::new(),
            _marker: PhantomData,
        }
    }

    pub fn from_raw(raw: Vec<T>) -> Self {
        IndexVec {
            raw,
            _marker: PhantomData,
        }
    }

    pub fn push(&mut self, elem: T) -> I {
        let id = I::new(self.raw.len());
        self.raw.push(elem);
        id
    }

    pub fn get(&self, idx: I) -> Option<&T> {
        self.raw.get(idx.index())
    }

    pub fn get_mut(&mut self, idx: I) -> Option<&mut T> {
        self.raw.get_mut(idx.index())
    }

    pub fn remove(&mut self, idx: I) -> T {
        self.raw.remove(idx.index())
    }

    pub fn len(&self) -> usize {
        self.raw.len()
    }
}
