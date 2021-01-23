use page_fault::aes_openssl::page_trace::{pf_access, pf_trace};
use page_fault::aes_openssl::AES;
use page_fault::utils::random_u8_slice;

fn main() {
    let key: [u8; 16] = [
        0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0a, 0x0b, 0x0c, 0x0d, 0x0e,
        0x0f,
    ];
    let mut c = [0u8; 16];
    let mut p = [0u8; 16];

    let cipher = AES::new(&key[0..16]);

    let mut cnt = vec![0; 256];

    let count = 10000;
    for i in 0..count {
        random_u8_slice(&mut p);
        cipher.encrypt(&p, &mut c);
        let access = pf_access(i, 128);
        let len = pf_trace(&access).len();
        cnt[len] += 1;
    }
    for (i, &c) in cnt.iter().enumerate() {
        if c != 0 {
            println!("{}: {:?}", i, c);
        }
    }
    println!("80 percentage: {:?}", cnt[80] as f32 / count as f32);
}
