use super::contants::{Te0, Te1, Te2, Te3};

pub fn pf_access(enc_idx: usize, border: usize) -> Vec<bool> {
    let (t0, t1, t2, t3) = (
        &Te0.trace()[enc_idx * 40..(enc_idx + 1) * 40],
        &Te1.trace()[enc_idx * 40..(enc_idx + 1) * 40],
        &Te2.trace()[enc_idx * 40..(enc_idx + 1) * 40],
        &Te3.trace()[enc_idx * 40..(enc_idx + 1) * 40],
    );
    let mut res = vec![];
    (0..39).for_each(|i| {
        res.push(t0[i] < border);
        res.push(t1[i] < border);
        res.push(t2[i] < border);
        res.push(t3[i] < border);
    });
    res.push(t2[39] < border);
    res.push(t3[39] < border);
    res.push(t0[39] < border);
    res.push(t1[39] < border);
    res
}

pub fn pf_trace(access: &[bool]) -> Vec<bool> {
    let mut res = access[0..1].to_vec();
    access.windows(2).for_each(|r| {
        if r[0] != r[1] {
            res.push(r[1]);
        }
    });
    res
}
