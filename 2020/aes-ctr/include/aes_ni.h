/*
    This file is modified from https://github.com/sebastien-riou/aes-brute-force.
*/

#ifndef __AES_NI_H__
#define __AES_NI_H__

#include <stdint.h> //for int8_t
#include <string.h> //for memcmp
#include <wmmintrin.h> //for intrinsics for AES-NI
//compile using gcc and following arguments: -g;-O0;-Wall;-msse2;-msse;-march=native;-maes

// macros
#define DO_ENC_BLOCK(m, k)                  \
    do {                                    \
        m = _mm_xor_si128(m, k[0]);         \
        m = _mm_aesenc_si128(m, k[1]);      \
        m = _mm_aesenc_si128(m, k[2]);      \
        m = _mm_aesenc_si128(m, k[3]);      \
        m = _mm_aesenc_si128(m, k[4]);      \
        m = _mm_aesenc_si128(m, k[5]);      \
        m = _mm_aesenc_si128(m, k[6]);      \
        m = _mm_aesenc_si128(m, k[7]);      \
        m = _mm_aesenc_si128(m, k[8]);      \
        m = _mm_aesenc_si128(m, k[9]);      \
        m = _mm_aesenclast_si128(m, k[10]); \
    } while (0)

#define DO_DEC_BLOCK(m, k)                  \
    do {                                    \
        m = _mm_xor_si128(m, k[10 + 0]);    \
        m = _mm_aesdec_si128(m, k[10 + 1]); \
        m = _mm_aesdec_si128(m, k[10 + 2]); \
        m = _mm_aesdec_si128(m, k[10 + 3]); \
        m = _mm_aesdec_si128(m, k[10 + 4]); \
        m = _mm_aesdec_si128(m, k[10 + 5]); \
        m = _mm_aesdec_si128(m, k[10 + 6]); \
        m = _mm_aesdec_si128(m, k[10 + 7]); \
        m = _mm_aesdec_si128(m, k[10 + 8]); \
        m = _mm_aesdec_si128(m, k[10 + 9]); \
        m = _mm_aesdeclast_si128(m, k[0]);  \
    } while (0)

#define AES_128_KEY_EXP(k, rcon) aes_128_key_expansion(k, _mm_aeskeygenassist_si128(k, rcon))

static __m128i aes_128_key_expansion(__m128i key, __m128i keygen)
{
    keygen = _mm_shuffle_epi32(keygen, _MM_SHUFFLE(3, 3, 3, 3));
    key = _mm_xor_si128(key, _mm_slli_si128(key, 4));
    key = _mm_xor_si128(key, _mm_slli_si128(key, 4));
    key = _mm_xor_si128(key, _mm_slli_si128(key, 4));
    return _mm_xor_si128(key, keygen);
}

// public API
static void aes128_load_key(const uint8_t* enc_key, __m128i* key_schedule)
{
    key_schedule[0] = _mm_loadu_si128((const __m128i*)enc_key);
    key_schedule[1] = AES_128_KEY_EXP(key_schedule[0], 0x01);
    key_schedule[2] = AES_128_KEY_EXP(key_schedule[1], 0x02);
    key_schedule[3] = AES_128_KEY_EXP(key_schedule[2], 0x04);
    key_schedule[4] = AES_128_KEY_EXP(key_schedule[3], 0x08);
    key_schedule[5] = AES_128_KEY_EXP(key_schedule[4], 0x10);
    key_schedule[6] = AES_128_KEY_EXP(key_schedule[5], 0x20);
    key_schedule[7] = AES_128_KEY_EXP(key_schedule[6], 0x40);
    key_schedule[8] = AES_128_KEY_EXP(key_schedule[7], 0x80);
    key_schedule[9] = AES_128_KEY_EXP(key_schedule[8], 0x1B);
    key_schedule[10] = AES_128_KEY_EXP(key_schedule[9], 0x36);
}

static __m128i aes128_enc_block(__m128i* key_schedule, __m128i m)
{
    DO_ENC_BLOCK(m, key_schedule);
    return m;
}

#endif
