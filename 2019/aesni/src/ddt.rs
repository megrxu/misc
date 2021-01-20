use super::aes::SBOX;
use lazy_static::lazy_static;

lazy_static! {
    #[derive(Debug)]
    pub static ref DDT: Vec<bool> = {
        let mut ddt = vec![false; 256];
        for byte in 0..0xffu8 {
            ddt[(byte ^ SBOX[byte as usize]) as usize] = true;
        }
        ddt
    };
}

#[test]
fn ddt_test() {
    for i in DDT.chunks(16) {
        println!("{:?}", i);
    }
}
