use rayon::iter::plumbing::*;
use rayon::prelude::*;
use std::sync::Arc;

pub struct CartesianProduct<T: Sized + Send + Clone> {
    vals: Vec<Arc<Vec<T>>>,
    size: usize,
    count: usize,
    lengths: Vec<usize>,
}

impl<T: Sized + Send + Clone> CartesianProduct<T> {
    pub fn new(list: &[Arc<Vec<T>>]) -> Self {
        let lengths = list.iter().map(|l| l.len()).collect::<Vec<usize>>();
        let size = lengths.iter().product();
        CartesianProduct {
            vals: list.to_vec(),
            count: lengths.len(),
            size,
            lengths,
        }
    }

    pub fn nth_unchecked(&self, mut idx: usize) -> Vec<T> {
        let mut res = vec![];
        let mut weight = self.size;
        for i in 0..self.count {
            weight /= self.lengths[i];
            let j = idx / weight;
            idx -= j * weight;
            res.push(self.vals[i][j].clone());
        }
        res
    }

    pub fn nth(&self, idx: usize) -> Option<Vec<T>> {
        if idx < self.size {
            Some(self.nth_unchecked(idx))
        } else {
            None
        }
    }
}

impl<T: Sized + Send + Sync + Clone> ParallelIterator for CartesianProduct<T> {
    type Item = Vec<T>;
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
        let a = vec![0, 1, 2, 3, 4u8];
        let b = vec![5, 6, 7u8];
        let c = vec![8, 9u8];

        let result = CartesianProduct::new(&[Arc::new(a), Arc::new(b), Arc::new(c)]);
        result.for_each(|i| println!("{:?}", i));
    }
}
