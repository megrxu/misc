use std::fs::File;
use std::fs::OpenOptions;

pub fn random_u8_slice(p: &mut [u8]) {
    for e in p.iter_mut() {
        *e = rand::random::<u8>();
    }
}

fn filename(key: &[u8]) -> String {
    format!(
        "data/{}.bin",
        key.iter()
            .fold("".to_string(), |res, b| res + &format!("{:02x}", b))
    )
}

pub fn file_to_append(key: &[u8]) -> File {
    let filename = filename(key);
    OpenOptions::new()
        .append(true)
        .read(true)
        .open(&filename)
        .unwrap_or(
            OpenOptions::new()
                .append(true)
                .read(true)
                .create(true)
                .open(&filename)
                .unwrap(),
        )
}

pub fn file_to_read(key: &[u8]) -> File {
    let filename = filename(key);
    OpenOptions::new().read(true).open(&filename).unwrap()
}
