use std::cell::RefCell;
use std::ops::Index;

#[derive(Debug, Clone)]
pub struct TraceableVector<T: Clone> {
    value: Vec<T>,
    trace: RefCell<Vec<usize>>,
}

unsafe impl<T: Clone + ?Sized> Sync for TraceableVector<T> {}

impl<T: Clone> TraceableVector<T> {
    pub fn new(val: &[T]) -> Self {
        TraceableVector {
            value: val.to_vec(),
            trace: RefCell::new(vec![]),
        }
    }

    pub fn trace(&self) -> Vec<usize> {
        self.trace.borrow().to_vec()
    }

    pub fn clear_trace(&self) {
        self.trace.borrow_mut().clear();
    }
}

impl<T: Clone> Index<usize> for TraceableVector<T> {
    type Output = T;
    fn index(&self, idx: usize) -> &Self::Output {
        &self.trace.borrow_mut().push(idx);
        &self.value[idx]
    }
}
