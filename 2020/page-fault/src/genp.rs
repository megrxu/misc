use page_fault::aes_openssl::page_trace::{pf_access, pf_trace};
use page_fault::aes_openssl::AES;
use page_fault::utils::*;
use std::io::prelude::*;

fn main() {
    let key: [u8; 16] = [
        0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0a, 0x0b, 0x0c, 0x0d, 0x0e,
        0x0f,
    ];
    let mut c = [0u8; 16];
    let mut p = [0u8; 16];

    let cipher = AES::new(&key[0..16]);

    let mut fp = file_to_append(&key);

    let mut count = 0;
    let mut i = 0;

    // Generate 1000 plaintexts, each of them will leads to 80 page faults for current key
    while count <= 1000 {
        random_u8_slice(&mut p);
        cipher.encrypt(&p, &mut c);
        let access = pf_access(i, 128);
        let len = pf_trace(&access).len();
        if len == 80 {
            fp.write(&p).unwrap();
            count += 1;
        }
        i += 1;
    }
}
