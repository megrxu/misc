use itertools::iproduct;
use std::fs::File;
use std::io::prelude::*;
use utils::aes::*;
use utils::ddt::DDT;
use utils::generic::create_u8x4x4;

fn main() {
    let mut ps: Vec<Vec<u8>> = vec![];
    let mut cs: Vec<Vec<u8>> = vec![];

    load_data(&mut ps, "data/ps.bin");
    load_data(&mut cs, "data/cs.bin");

    let mut rk: Vec<AESstate> = vec![[[0u8; 4]; 4]; 16];
    key_expansion(&[0u8; 16], &mut rk);

    let mut rks: Vec<AESstate> = vec![[[0; 4]; 4]; 10];
    for (_, k) in rk.iter().enumerate() {
        rks.push([[k[0][0]; 4]; 4]);
    }

    let mut key: Vec<u8> = vec![];
    for (rk1, rk2, rk3, rk4) in iproduct!(0..0xff, 0..0xff, 0..0xff, 0..0xff) {
        for p in ps.iter() {
            let si = forward(&p, &rks);
        }
    }

    for (rk5, rk6, rk7, rk8, rk9) in iproduct!(0..0xff, 0..0xff, 0..0xff, 0..0xff, 0..0xff) {
        for c in cs.iter() {
            let so = backward(&c, &rks);
        }
    }
}

fn forward(plaintext: &[u8], rk: &[AESstate]) -> AESstate {
    let mut state = create_u8x4x4(plaintext);
    state = add_round_key(&state, &rk[0]);
    for i in 1..5 {
        state = sub_bytes(&state, &SBOX);
        state = shift_rows(&state);
        // state = mix_columns(&state);
        state = add_round_key(&state, &rk[i]);
    }
    // state = sub_bytes(&state, &SBOX);
    state
}

fn backward(ciphertext: &[u8], rk: &[AESstate]) -> AESstate {
    let mut state = create_u8x4x4(ciphertext);
    state = add_round_key(&state, &rk[10]);
    for i in (5..10).rev() {
        state = inv_shift_rows(&state);
        state = inv_sub_bytes(&state, &RSBOX);
        state = add_round_key(&state, &rk[i]);
        // state = inv_mix_columns(&state);
    }
    state = inv_shift_rows(&state);
    state
}

fn load_data(data: &mut Vec<Vec<u8>>, filename: &str) {
    let mut file = File::open(filename).unwrap();
    let mut buffer = [0; 16];
    while let Ok(_) = file.read_exact(&mut buffer) {
        data.push(buffer.to_vec())
    }
}
