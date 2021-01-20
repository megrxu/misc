use rayon::prelude::*;
use std::ops::{Add, Mul, Sub};

#[allow(non_camel_case_types)]
type num = i128;

#[derive(Clone, Debug, PartialEq)]
struct Basis {
    basis: Vec<num>,
    pre: Vec<num>,
}

#[derive(Clone, Debug)]
struct RNS {
    basis: Box<Basis>,
    nums: Vec<num>,
}

trait IntoRNS {
    fn into_rns(&self, basis: &Basis) -> RNS;
}

impl IntoRNS for num {
    fn into_rns(&self, basis: &Basis) -> RNS {
        RNS::new(*self, basis)
    }
}

impl RNS {
    fn new(val: num, basis: &Basis) -> Self {
        let nums = basis.basis.par_iter().map(|&i| val % i).collect();
        let basis = Box::new(basis.clone());
        RNS { basis, nums }
    }

    fn val(&self) -> num {
        let mut res = 0;
        let mut mult = 1;
        self.basis
            .basis
            .iter()
            .zip(self.basis.pre.iter())
            .zip(self.nums.iter())
            .for_each(|((&base, pre), num)| {
                let t = pre * (num - res) % base;
                res += t * mult;
                mult *= base;
            });
        res
    }
}

macro_rules! impl_rns_op {
    ($op :tt, $Op : ident, $type: ty) => {
        impl $Op for $type {
            type Output = RNS;
            fn $op(self, rhs: $type) -> Self::Output {
                assert_eq!(rhs.basis, self.basis);
                assert_eq!(rhs.basis.basis.len(), self.nums.len());
                assert_eq!(rhs.basis.basis.len(), rhs.nums.len());
                RNS {
                    nums: self
                        .nums
                        .par_iter()
                        .zip(rhs.nums.par_iter())
                        .zip(self.basis.basis.par_iter())
                        .map(|((&x, &y), &b)| (x.$op(y)) % b)
                        .collect(),
                    basis: self.basis.clone(),
                }
            }
        }
    };
}

impl_rns_op!(add, Add, &RNS);
impl_rns_op!(sub, Sub, &RNS);
impl_rns_op!(mul, Mul, &RNS);
impl_rns_op!(add, Add, RNS);
impl_rns_op!(sub, Sub, RNS);
impl_rns_op!(mul, Mul, RNS);

impl Basis {
    fn new(b: &[num]) -> Self {
        let basis = b.into();
        let mut res = Basis { basis, pre: vec![] };
        res.precompute();
        res
    }

    // To precompute the inverse of the multipliers.
    fn precompute(&mut self) {
        let mut cur = 1;
        for &base in self.basis.iter() {
            self.pre.push(inverse(cur, base));
            cur *= base;
        }
    }
}

// Adopted from https://en.wikipedia.org/wiki/Extended_Euclidean_algorithm
fn inverse(a: num, n: num) -> num {
    let mut t = (0, 1);
    let mut r = (n, a);
    while r.1 != 0 {
        let quotient = r.0 / r.1;
        t = (t.1, t.0 - quotient * t.1);
        r = (r.1, r.0 - quotient * r.1);
    }
    t.0 % n
}

fn main() {
    // The basis must be coprime.
    let basis = Basis::new(&[
        113, 127, 131, 137, 139, 149, 151, 157, 163, 167, 173, 179, 181, 191, 193, 197, 199,
    ]);

    let a = 24123122812731;
    let b = 22314135123321;

    let a_rns = &a.into_rns(&basis);
    let b_rns = &b.into_rns(&basis);

    assert_eq!(a * b, (a_rns * b_rns).val());
    assert_eq!(a + b, (a_rns + b_rns).val());
    assert_eq!(a - b, (a_rns - b_rns).val());
    assert_eq!(b - a, (b_rns - a_rns).val());

    println!("{:?}", basis);
    println!("a in RNS: {:02x?}", a_rns.nums);
    println!("b in RNS: {:02x?}", b_rns.nums);
}
