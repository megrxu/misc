use page_fault::aes_openssl::*;
use page_fault::trace::*;

#[test]
fn test_trace() {
    let c: TraceableVector<u8> = TraceableVector::new(&[0; 100]);

    for &i in [0, 2, 3, 9, 97, 2, 56, 9].iter() {
        c[i];
    }

    assert_eq!(c.trace(), vec![0, 2, 3, 9, 97, 2, 56, 9]);
}

#[test]
fn aes_trace() {
    let key: [u8; 16] = [
        0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0a, 0x0b, 0x0c, 0x0d, 0x0e,
        0x0f,
    ];
    let p = [
        0x00, 0x11, 0x22, 0x33, 0x44, 0x55, 0x66, 0x77, 0x88, 0x99, 0xaa, 0xbb, 0xcc, 0xdd, 0xee,
        0xff,
    ];
    let mut c = [0u8; 16];

    let cipher = AES::new(&key[0..16]);

    cipher.encrypt(&p, &mut c);
    println!("Te0: {:?}", contants::Te0.trace().len());
    println!("Te1: {:?}", contants::Te1.trace().len());
    println!("Te2: {:?}", contants::Te2.trace().len());
    println!("Te3: {:?}", contants::Te3.trace().len());

    cipher.encrypt(&p, &mut c);
    assert_eq!(contants::Te0.trace()[0..40], contants::Te0.trace()[40..]);

    cipher.encrypt(&[0; 16], &mut c);
    assert_ne!(contants::Te0.trace()[0..40], contants::Te0.trace()[80..]);

    contants::Te0.clear_trace();
    assert_eq!(contants::Te0.trace(), vec![]);
}
