#include "aes_ni.h"
#include "stdio.h"

#define CNT 2097152

inline void update_counter(uint8_t* counter, uint32_t idx)
{
    (*((uint32_t*)(counter))) += idx;
}

inline void set_counter(uint8_t* counter, uint32_t idx)
{
    (*((uint32_t*)(counter))) = idx;
}

void print_bytes(uint8_t* p_src, uint32_t cnt)
{
    for (int i = 0; i < cnt; i++) {
        for (int j = 0; j < 16; j++) {
            printf("0x%02hhx ", p_src[i * 16 + j]);
        }
        printf("\n");
    }
}

/* AES-CTR 128-bit
 * Parameters:
 *   Inputs:
 *     uint8_t *p_key - Pointer to the key used in encryption/decryption operation
 *     uint8_t *p_src - Pointer to the input stream to be encrypted/decrypted. Size of buffer 
 *       should be >= cnt * 16 where cnt = src_len % 16 ? src_len / 16 + 1 : src_len / 16.
 *     uint32_t src_len - Length of the input stream to be encrypted/decrypted
 *     uint8_t *p_nonce - Pointer to the nounce block. It should be 128-bit but with first 32/16
 *       bits zero, reserved for the 32/16-bit counter.
 *   Output:
 *     uint8_t *p_dst - Pointer to the cipher text. Size of buffer should be >= cnt * 16 where 
 *       cnt = src_len % 16 ? src_len / 16 + 1 : src_len / 16.
 */

void aes_ctr_cipher(const uint8_t* p_key, const uint8_t* p_src, const uint32_t src_len, uint8_t* p_nonce, uint8_t* p_dst)
{
    __m128i key_schedule[11];
    aes128_load_key(p_key, key_schedule);
    uint32_t cnt = src_len / 16;
    for (int i = 0; i < cnt; i++) {
        update_counter(p_nonce, 1);
        _mm_store_si128((__m128i*)p_dst + i,
            _mm_xor_si128(_mm_load_si128((__m128i*)p_src + i),
                aes128_enc_block(key_schedule,
                    _mm_load_si128((__m128i*)p_nonce))));
    }
}

void aes_ctr_cipher_block(__m128i* p_skey, uint8_t* p_src, uint8_t* p_nonce, uint8_t* p_dst)
{
    __m128i p, c, k;
    k = aes128_enc_block(p_skey, _mm_load_si128((__m128i*)p_nonce));
    p = _mm_load_si128((__m128i*)p_src);
    c = _mm_xor_si128(p, k);
    _mm_store_si128((__m128i*)p_dst, c);
}

int block_enc_test()
{
    uint8_t p_[] = { 0x00, 0x11, 0x22, 0x33, 0x44, 0x55, 0x66, 0x77, 0x88, 0x99, 0xaa, 0xbb, 0xcc, 0xdd, 0xee,
        0xff };
    uint8_t k_[] = { 0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0a, 0x0b, 0x0c, 0x0d, 0x0e,
        0x0f };
    uint8_t c_[] = { 0x69, 0xc4, 0xe0, 0xd8, 0x6a, 0x7b, 0x04, 0x30, 0xd8, 0xcd, 0xb7, 0x80, 0x70, 0xb4, 0xc5,
        0x5a };

    // init subkey arrays
    __m128i sk[11];
    aes128_load_key(k_, sk);

    // load one block p
    __m128i p = _mm_load_si128((__m128i*)p_);
    __m128i c = aes128_enc_block(sk, p);

    // compare with the reference ciphertext
    __m128i c_ref = _mm_load_si128((__m128i*)c_);
    __m128i cmp = _mm_cmpeq_epi8(c, c_ref);

    int res = 0;
    for (int i = 0; i < 16; i++) {
        printf("0x%0x ", ((uint8_t*)(&cmp))[i]);
    }

    return 0;
}

int main(void)
{
    uint8_t p_src[CNT] = { 0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0A, 0x0B, 0x0C, 0x0D, 0x0E, 0x0F, 0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17, 0x18, 0x19, 0x1A, 0x1B, 0x1C, 0x1D, 0x1E, 0x1F, 0x20, 0x21, 0x22, 0x23 };
    uint8_t p_key[] = { 0x76, 0x91, 0xBE, 0x03, 0x5E, 0x50, 0x20, 0xA8, 0xAC, 0x6E, 0x61, 0x85, 0x29, 0xF9, 0xA0, 0xDC };
    uint8_t p_dst[CNT] = { 0 };
    uint8_t p_buf[CNT] = { 0 };
    uint8_t p_nonce[16] = {
        0x00, 0x00, 0x00, 0x00, 0x00, 0xE0, 0x01, 0x7B, 0x27, 0x77, 0x7F, 0x3F, 0x4A, 0x17, 0x86, 0xF0
    };

    // printf("Counter Block: \n");
    // int i = 0;
    // do {
    //     i++;
    //     update_counter(p_nonce, i);
    //     print_bytes(p_nonce, 1);
    // } while (i < 3);

    // printf("\nPlaintext: \n");
    // print_bytes(p_src, 3);

    // printf("\nEncrypted data: \n");
    set_counter(p_nonce, 0);
    for (int i = 0; i < 1024 * 4; i++)
        aes_ctr_cipher(p_key, p_src, CNT, p_nonce, p_dst);
    print_bytes(p_dst, 0);

    // reset counter
    // set_counter(p_nonce, 0);
    // printf("\nDecrypted data: \n");
    // aes_ctr_cipher(p_key, p_dst, CNT, p_nonce, p_buf);
    // print_bytes(p_buf, 3);

    // to perform blockwise en/decryption, we need to init the subkeys and
    // compute the counter value first
    // __m128i p_skey[11];
    // aes128_load_key(p_key, p_skey);
    // set_counter(p_nonce, 0);
    // update_counter(p_nonce, 2);

    // printf("\nDecrypted 128 bit data (the 2nd row): \n");
    // aes_ctr_cipher_block(p_skey, p_dst + 16, p_nonce, p_buf);
    // print_bytes(p_buf, 1);
}
