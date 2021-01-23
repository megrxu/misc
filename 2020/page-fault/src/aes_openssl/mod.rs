pub mod contants;
pub mod page_trace;
pub mod utils;
use contants::*;
use utils::*;

pub struct AES {
    pub key: Vec<u8>,
    pub key_size: usize,
    pub eround_keys: Vec<u32>,
    pub dround_keys: Vec<u32>,
}

impl AES {
    pub fn new(key: &[u8]) -> Self {
        match key.len() {
            16 | 24 | 32 => (),
            _ => panic!("Invalid key size for AES! It must be 128, 192 or 256 bits."),
        };
        let mut cipher = AES {
            key: key.to_vec(),
            key_size: key.len(),
            eround_keys: vec![],
            dround_keys: vec![],
        };
        cipher.set_encrypt_key(key);
        cipher.set_decrypt_key();
        cipher
    }

    fn set_encrypt_key(&mut self, key: &[u8]) {
        let mut rks: Vec<u32> = key.chunks(4).map(|r| get_u32_unchecked(r)).collect();
        match self.key_size {
            16 => {
                for i in 0..10 {
                    let temp = rks[3 + i * 4];
                    rks.push(
                        rks[0 + i * 4]
                            ^ (te2[((temp >> 16) & 0xff) as usize] & 0xff000000)
                            ^ (te3[((temp >> 8) & 0xff) as usize] & 0x00ff0000)
                            ^ (te0[((temp) & 0xff) as usize] & 0x0000ff00)
                            ^ (te1[((temp >> 24) as usize)] & 0x000000ff)
                            ^ rcon[i],
                    );
                    rks.push(rks[1 + i * 4] ^ rks[4 + i * 4]);
                    rks.push(rks[2 + i * 4] ^ rks[5 + i * 4]);
                    rks.push(rks[3 + i * 4] ^ rks[6 + i * 4]);
                }
            }
            24 => {
                for i in 0..8 {
                    let temp = rks[5 + i * 6];
                    rks.push(
                        rks[0 + i * 6]
                            ^ (te2[((temp >> 16) & 0xff) as usize] & 0xff000000)
                            ^ (te3[((temp >> 8) & 0xff) as usize] & 0x00ff0000)
                            ^ (te0[((temp) & 0xff) as usize] & 0x0000ff00)
                            ^ (te1[((temp >> 24) as usize)] & 0x000000ff)
                            ^ rcon[i],
                    );
                    rks.push(rks[1 + i * 6] ^ rks[6 + i * 6]);
                    rks.push(rks[2 + i * 6] ^ rks[7 + i * 6]);
                    rks.push(rks[3 + i * 6] ^ rks[8 + i * 6]);

                    if i + 1 == 8 {
                        break;
                    }

                    rks.push(rks[4 + i * 6] ^ rks[9 + i * 6]);
                    rks.push(rks[5 + i * 6] ^ rks[10 + i * 6]);
                }
            }
            32 => {
                for i in 0..7 {
                    let mut temp = rks[7 + i * 8];
                    rks.push(
                        rks[0 + i * 8]
                            ^ (te2[((temp >> 16) & 0xff) as usize] & 0xff000000)
                            ^ (te3[((temp >> 8) & 0xff) as usize] & 0x00ff0000)
                            ^ (te0[((temp) & 0xff) as usize] & 0x0000ff00)
                            ^ (te1[((temp >> 24) as usize)] & 0x000000ff)
                            ^ rcon[i],
                    );
                    rks.push(rks[1 + i * 8] ^ rks[8 + i * 8]);
                    rks.push(rks[2 + i * 8] ^ rks[9 + i * 8]);
                    rks.push(rks[3 + i * 8] ^ rks[10 + i * 8]);

                    if i + 1 == 7 {
                        break;
                    }

                    temp = rks[11 + i * 8];
                    rks.push(
                        rks[4 + i * 8]
                            ^ (te2[(temp >> 24) as usize] & 0xff000000)
                            ^ (te3[((temp >> 16) & 0xff) as usize] & 0x00ff0000)
                            ^ (te0[((temp >> 8) & 0xff) as usize] & 0x0000ff00)
                            ^ (te1[((temp & 0xff) as usize)] & 0x000000ff),
                    );
                    rks.push(rks[5 + i * 8] ^ rks[12 + i * 8]);
                    rks.push(rks[6 + i * 8] ^ rks[13 + i * 8]);
                    rks.push(rks[7 + i * 8] ^ rks[14 + i * 8]);
                }
            }
            _ => unreachable!(),
        };

        self.eround_keys = rks;
    }

    fn set_decrypt_key(&mut self) {
        let mut rks: Vec<u32> = self
            .eround_keys
            .chunks(4)
            .rev()
            .collect::<Vec<_>>()
            .concat();
        let rounds = rks.len() / 4;
        for i in 1..(rounds - 1) {
            rks[0 + i * 4] = td0[(te1[(rks[0 + i * 4] >> 24) as usize] & 0xff) as usize]
                ^ td1[(te1[((rks[0 + i * 4] >> 16) & 0xff) as usize] & 0xff) as usize]
                ^ td2[(te1[((rks[0 + i * 4] >> 8) & 0xff) as usize] & 0xff) as usize]
                ^ td3[(te1[((rks[0 + i * 4]) & 0xff) as usize] & 0xff) as usize];
            rks[1 + i * 4] = td0[(te1[(rks[1 + i * 4] >> 24) as usize] & 0xff) as usize]
                ^ td1[(te1[((rks[1 + i * 4] >> 16) & 0xff) as usize] & 0xff) as usize]
                ^ td2[(te1[((rks[1 + i * 4] >> 8) & 0xff) as usize] & 0xff) as usize]
                ^ td3[(te1[((rks[1 + i * 4]) & 0xff) as usize] & 0xff) as usize];
            rks[2 + i * 4] = td0[(te1[(rks[2 + i * 4] >> 24) as usize] & 0xff) as usize]
                ^ td1[(te1[((rks[2 + i * 4] >> 16) & 0xff) as usize] & 0xff) as usize]
                ^ td2[(te1[((rks[2 + i * 4] >> 8) & 0xff) as usize] & 0xff) as usize]
                ^ td3[(te1[((rks[2 + i * 4]) & 0xff) as usize] & 0xff) as usize];
            rks[3 + i * 4] = td0[(te1[(rks[3 + i * 4] >> 24) as usize] & 0xff) as usize]
                ^ td1[(te1[((rks[3 + i * 4] >> 16) & 0xff) as usize] & 0xff) as usize]
                ^ td2[(te1[((rks[3 + i * 4] >> 8) & 0xff) as usize] & 0xff) as usize]
                ^ td3[(te1[((rks[3 + i * 4]) & 0xff) as usize] & 0xff) as usize];
        }
        self.dround_keys = rks;
    }

    pub fn encrypt(&self, plaintext: &[u8], ciphertext: &mut [u8]) {
        let mut state: Vec<u32> = plaintext
            .chunks(4)
            .collect::<Vec<_>>()
            .iter()
            .enumerate()
            .map(|(i, r)| self.eround_keys[i] ^ get_u32_unchecked(r))
            .collect();
        let rounds = self.eround_keys.len() / 4 - 1;
        for i in 0..(rounds - 1) {
            let tmp: [u32; 4] = [state[0], state[1], state[2], state[3]];
            state[0] = Te0[(tmp[0] >> 24) as usize]
                ^ Te1[((tmp[1] >> 16) & 0xff) as usize]
                ^ Te2[((tmp[2] >> 8) & 0xff) as usize]
                ^ Te3[(tmp[3] & 0xff) as usize]
                ^ self.eround_keys[4 + 4 * i];
            state[1] = Te0[(tmp[1] >> 24) as usize]
                ^ Te1[((tmp[2] >> 16) & 0xff) as usize]
                ^ Te2[((tmp[3] >> 8) & 0xff) as usize]
                ^ Te3[(tmp[0] & 0xff) as usize]
                ^ self.eround_keys[5 + 4 * i];
            state[2] = Te0[(tmp[2] >> 24) as usize]
                ^ Te1[((tmp[3] >> 16) & 0xff) as usize]
                ^ Te2[((tmp[0] >> 8) & 0xff) as usize]
                ^ Te3[(tmp[1] & 0xff) as usize]
                ^ self.eround_keys[6 + 4 * i];
            state[3] = Te0[(tmp[3] >> 24) as usize]
                ^ Te1[((tmp[0] >> 16) & 0xff) as usize]
                ^ Te2[((tmp[1] >> 8) & 0xff) as usize]
                ^ Te3[(tmp[2] & 0xff) as usize]
                ^ self.eround_keys[7 + 4 * i];
        }
        let tmp: [u32; 4] = [state[0], state[1], state[2], state[3]];
        state[0] = (Te2[(tmp[0] >> 24) as usize] & 0xff000000)
            ^ (Te3[((tmp[1] >> 16) & 0xff) as usize] & 0x00ff0000)
            ^ (Te0[((tmp[2] >> 8) & 0xff) as usize] & 0x0000ff00)
            ^ (Te1[((tmp[3]) & 0xff) as usize] & 0x000000ff)
            ^ self.eround_keys[rounds * 4];
        state[1] = (Te2[(tmp[1] >> 24) as usize] & 0xff000000)
            ^ (Te3[((tmp[2] >> 16) & 0xff) as usize] & 0x00ff0000)
            ^ (Te0[((tmp[3] >> 8) & 0xff) as usize] & 0x0000ff00)
            ^ (Te1[((tmp[0]) & 0xff) as usize] & 0x000000ff)
            ^ self.eround_keys[rounds * 4 + 1];
        state[2] = (Te2[(tmp[2] >> 24) as usize] & 0xff000000)
            ^ (Te3[((tmp[3] >> 16) & 0xff) as usize] & 0x00ff0000)
            ^ (Te0[((tmp[0] >> 8) & 0xff) as usize] & 0x0000ff00)
            ^ (Te1[((tmp[1]) & 0xff) as usize] & 0x000000ff)
            ^ self.eround_keys[rounds * 4 + 2];
        state[3] = (Te2[(tmp[3] >> 24) as usize] & 0xff000000)
            ^ (Te3[((tmp[0] >> 16) & 0xff) as usize] & 0x00ff0000)
            ^ (Te0[((tmp[1] >> 8) & 0xff) as usize] & 0x0000ff00)
            ^ (Te1[((tmp[2]) & 0xff) as usize] & 0x000000ff)
            ^ self.eround_keys[rounds * 4 + 3];
        put_several_u32_unchecked(4, &state, ciphertext);
    }
}
