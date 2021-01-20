use rand::prelude::*;
use std::collections::{HashMap, HashSet};
use std::convert::TryInto;

const N: usize = 3;
const M: usize = 2;

type record = (usize, usize);
type set = HashSet<[usize; N]>;
type map = HashMap<[usize; N], usize>;

fn simulate(count: usize) -> map {
    let mut m: map = HashMap::default();
    let mut rng = rand::thread_rng();
    for _ in 0..count {
        let res: [usize; N] = (0..N)
            .map(|_| rng.gen::<usize>() % M)
            .collect::<Vec<usize>>()[0..N]
            .try_into()
            .unwrap();
        match m.get(&res) {
            None => {
                m.insert(res, 1);
            }
            Some(v) => {
                m.insert(res, v + 1);
            }
        }
    }
    m
}

fn main() {
    let s = simulate(100000);
    std::thread::spawn(move || {
        println!("{:?}", s);
        std::mem::drop(s)
    });
}
