use rayon::iter::plumbing::*;
use rayon::prelude::*;
use std::iter::{empty, once};

pub struct CartesianProduct<'a, T> {
    vals: Vec<&'a [T]>,
    size: usize,
    count: usize,
    lengths: Vec<usize>,
}

pub struct CartesianProductBuilder<'a, T> {
    vals: Box<dyn Iterator<Item = &'a [T]> + 'a>,
    lengths: Box<dyn Iterator<Item = usize>>,
    size: usize,
    count: usize,
}

impl<'a, T> CartesianProductBuilder<'a, T> {
    pub fn add(mut self, list: &'a [T]) -> CartesianProductBuilder<'a, T> {
        let l = list.len();
        assert!(l > 0);
        self.count += 1;
        self.size *= l;
        self.lengths = Box::new(self.lengths.chain(once(l)));
        self.vals = Box::new(self.vals.chain(once(list)));
        self
    }

    pub fn build(self) -> CartesianProduct<'a, T> {
        CartesianProduct {
            vals: self.vals.collect(),
            count: self.count,
            size: self.size,
            lengths: self.lengths.collect(),
        }
    }
}

impl<'a, T> CartesianProduct<'a, T> {
    pub fn builder() -> CartesianProductBuilder<'a, T> {
        CartesianProductBuilder {
            vals: Box::new(empty()),
            count: 0,
            size: 1,
            lengths: Box::new(empty()),
        }
    }

    pub fn nth_unchecked(&self, mut idx: usize) -> Vec<&'a T> {
        let mut res = vec![];
        let mut weight = self.size;
        for i in 0..self.count {
            weight /= self.lengths[i];
            let j = idx / weight;
            idx -= j * weight;
            res.push(&self.vals[i][j]);
        }
        res
    }

    pub fn nth(&self, idx: usize) -> Option<Vec<&'a T>> {
        if idx < self.size {
            Some(self.nth_unchecked(idx))
        } else {
            None
        }
    }
}

impl<'a, T: Sync> ParallelIterator for CartesianProduct<'a, T> {
    type Item = Vec<&'a T>;
    fn drive_unindexed<C>(self, consumer: C) -> C::Result
    where
        C: UnindexedConsumer<Self::Item>,
    {
        (0..self.size)
            .into_par_iter()
            .map(|n| self.nth_unchecked(n))
            .drive_unindexed(consumer)
    }
}

#[cfg(test)]
mod tests {
    use crate::*;
    #[test]
    fn it_works() {
        let a: Vec<u8> = (0..4).collect();

        let result = CartesianProduct::builder()
            .add(&a)
            .add(&a)
            .add(&a)
            .add(&a)
            .add(&a)
            .add(&a)
            .build();

        result.for_each(|i| drop(i));
    }
}
