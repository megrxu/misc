use rand::Rng;
use std::fs::File;
use std::io::prelude::*;
use utils::aes::AES;
use utils::rand;

fn main() {
    let key = [0u8; 16];
    let cipher = AES::new(&key);
    let mut cfile = File::create("data/cs.bin").unwrap();
    let mut pfile = File::create("data/ps.bin").unwrap();
    let count = 100;
    for _ in 0..count {
        let p = rand!(16);
        pfile.write(&p).unwrap();
        cfile.write(&cipher.encrypt(&p)).unwrap();
    }
}
