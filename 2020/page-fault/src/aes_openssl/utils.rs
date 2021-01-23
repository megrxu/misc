fn get_u32_be(data: &[u8]) -> u32 {
    (0..4).fold(0, |res, i| res + ((data[3 - i] as u32) << (i * 8)))
}

fn put_several_u32_be(length: usize, nums: &[u32], data: &mut [u8]) {
    (0..length).for_each(|i| {
        (0..4).for_each(|j| data[i * 4 + j] = (nums[i] >> ((3 - j) * 8) & 0xff) as u8)
    });
}

pub fn get_u32_unchecked(data: &[u8]) -> u32 {
    get_u32_be(data)
}

pub fn put_several_u32_unchecked(length: usize, nums: &[u32], data: &mut [u8]) {
    put_several_u32_be(length, nums, data);
}
