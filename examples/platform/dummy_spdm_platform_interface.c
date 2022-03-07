#include <string.h>
#ifdef __linux__
#include <stdio.h>
#include <err.h>
#else
#define errx(eval, format) return 0;
#define printf(...)
#endif
#include <../include/spdm_platform_interface.h>

unsigned char spdm_platform_config_ct_exponent(void) {
    return 10;
}

unsigned char spdm_platform_config_cap_mac(void) {
    return 1;
}

unsigned char spdm_platform_config_cap_encrypt(void) {
    return 1;
}

unsigned char spdm_platform_config_cap_meas_fresh(void) {
    return 0;
}

unsigned char spdm_platform_config_cap_meas(void) {
    //  The Responder supports MEASUREMENTS response but cannot perform signature generation.
    return 1;
}

unsigned char spdm_platform_config_cap_chal(void) {
    return 0;
}

unsigned char spdm_platform_config_cap_cert(void) {
    //  Supported in WP 2.3.7
    return 1;
}

unsigned char spdm_platform_config_cap_cache(void) {
    return 0;
}

unsigned char spdm_platform_config_cap_handshake_in_the_clear(void) {
    return 1;
}

unsigned char spdm_platform_config_cap_key_upd(void) {
    //  Supported in WP 2.3.8
    return 0;
}

unsigned char spdm_platform_config_cap_hbeat(void) {
    return 0;
}

unsigned char spdm_platform_config_cap_encap(void) {
    return 0;
}

unsigned char spdm_platform_config_cap_psk(void) {
    return 0;
}

unsigned char spdm_platform_config_cap_key_ex(void) {
    return 1;
}

unsigned char spdm_platform_config_cap_mut_auth(void) {
    return 0;
}

unsigned char spdm_platform_config_cap_pub_key_id(void) {
    return 0;
}

static int measurement_hash_size = 0;

unsigned char spdm_platform_select_measurement_hash_algo(unsigned char tpm_alg_sha_256,
                                                         unsigned char tpm_alg_sha_384,
                                                         unsigned char tpm_alg_sha_512,
                                                         unsigned char tpm_alg_sha3_256,
                                                         unsigned char tpm_alg_sha3_384,
                                                         unsigned char tpm_alg_sha3_512,
                                                         unsigned char raw_bit_streams_only)
{
    if (tpm_alg_sha3_512) {
        measurement_hash_size = 64;
        return 64;
    }

    if (tpm_alg_sha3_384) {
        measurement_hash_size = 48;
        return 32;
    }

    if (tpm_alg_sha3_256) {
        measurement_hash_size = 32;
        return 16;
    }

    if (tpm_alg_sha_512) {
        measurement_hash_size = 64;
        return 8;
    }

    if (tpm_alg_sha_384) {
        measurement_hash_size = 48;
        return 4;
    }

    if (tpm_alg_sha_256) {
        measurement_hash_size = 32;
        return 2;
    }

    if (raw_bit_streams_only) {
        measurement_hash_size = 0;
        return 1;
    }

    // No mode set, unsupported
    return 0;
}

long spdm_platform_select_base_asym_algo(unsigned char tpm_alg_ecdsa_ecc_nist_p384,
                                         unsigned char tpm_alg_rsapss_4096,
                                         unsigned char tpm_alg_rsassa_4096,
                                         unsigned char tpm_alg_ecdsa_ecc_nist_p256,
                                         unsigned char tpm_alg_rsapss_3072,
                                         unsigned char tpm_alg_rsassa_3072,
                                         unsigned char tpm_alg_rsapss_2048,
                                         unsigned char tpm_alg_rsassa_2048,
                                         unsigned char tpm_alg_ecdsa_ecc_nist_p521)
{
    // FIXME: When anything but BA_Unsupported is announced, spdm-emu bails out with:
    // ASSERT: contrib/dmtf/spdm-emu/libspdm/os_stub/spdm_device_secret_lib_sample/cert.c(76): ((boolean)(0 == 1))
    // Return 0 (i.e. BA_Unsupported) until this problem is resolved in spdm-emu.
    return 0;

    if (tpm_alg_ecdsa_ecc_nist_p521) return 256;
    if (tpm_alg_ecdsa_ecc_nist_p384) return 128;
    if (tpm_alg_ecdsa_ecc_nist_p256) return 16;
    if (tpm_alg_rsapss_4096) return 64;
    if (tpm_alg_rsassa_4096) return 32;
    if (tpm_alg_rsapss_3072) return 8;
    if (tpm_alg_rsassa_3072) return 4;
    if (tpm_alg_rsapss_2048) return 2;
    if (tpm_alg_rsassa_2048) return 1;
    return 0;
}

unsigned char spdm_platform_select_base_hash_algo(unsigned char tpm_alg_sha_256,
                                                  unsigned char tpm_alg_sha_384,
                                                  unsigned char tpm_alg_sha_512,
                                                  unsigned char tpm_alg_sha3_256,
                                                  unsigned char tpm_alg_sha3_384,
                                                  unsigned char tpm_alg_sha3_512)
{
    if (tpm_alg_sha3_512) return 32;
    if (tpm_alg_sha3_384) return 16;
    if (tpm_alg_sha3_256) return 8;
    if (tpm_alg_sha_512) return 4;
    if (tpm_alg_sha_384) return 2;
    if (tpm_alg_sha_256) return 1;
    return 0;
}

unsigned char spdm_platform_select_dhe(unsigned char secp521r1,
                                       unsigned char secp384r1,
                                       unsigned char secp256r1,
                                       unsigned char ffdhe4096,
                                       unsigned char ffdhe3072,
                                       unsigned char ffdhe2048)
{
    if (secp521r1) return 32;
    if (secp384r1) return 16;
    if (secp256r1) return 8;
    if (ffdhe4096) return 4;
    if (ffdhe3072) return 2;
    if (ffdhe2048) return 1;
    errx(4, "No DHE selected");
}

unsigned char spdm_platform_select_aead(unsigned char chacha20_poly1305,
                                        unsigned char aes_256_gcm,
                                        unsigned char aes_128_gcm)
{
    if (chacha20_poly1305) return 4;
    if (aes_256_gcm) return 2;
    if (aes_128_gcm) return 1;
    errx(5, "No AEAD selected");
}


long spdm_platform_select_rbba(unsigned char ra_tpm_alg_ecdsa_ecc_nist_p384,
                               unsigned char ra_tpm_alg_rsapss_4096,
                               unsigned char ra_tpm_alg_rsassa_4096,
                               unsigned char ra_tpm_alg_ecdsa_ecc_nist_p256,
                               unsigned char ra_tpm_alg_rsapss_3072,
                               unsigned char ra_tpm_alg_rsassa_3072,
                               unsigned char ra_tpm_alg_rsapss_2048,
                               unsigned char ra_tpm_alg_rsassa_2048,
                               unsigned char ra_tpm_alg_ecdsa_ecc_nist_p521)
{
    if (ra_tpm_alg_ecdsa_ecc_nist_p521) return 256;
    if (ra_tpm_alg_ecdsa_ecc_nist_p384) return 128;
    if (ra_tpm_alg_rsapss_4096) return 64;
    if (ra_tpm_alg_rsassa_4096) return 32;
    if (ra_tpm_alg_ecdsa_ecc_nist_p256) return 16;
    if (ra_tpm_alg_rsapss_3072) return 8;
    if (ra_tpm_alg_rsassa_3072) return 4;
    if (ra_tpm_alg_rsapss_2048) return 2;
    if (ra_tpm_alg_rsassa_2048) return 1;
    errx(6, "Invalid RBBA selected");
}

void spdm_platform_get_digests_data(char *data, long *length, unsigned char *slot_mask)
{
    for (long i = 0; i < *length; i++)
    {
        data[i] = 0x00;
    }

    *slot_mask = 7;

    // measurment_hash_size is a static variable set to the size of the currently
    // selected hash in spdm_platform_select_measurement_hash_algo (in bytes)
    *length = 3 * measurement_hash_size;

    for (long i = 0; i < *length; i++)
    {
        data[i] = 0x42;
    }
}

static unsigned char initialized = 0;
static unsigned char cert_0[1024];
static unsigned char cert_1[1024];
static unsigned char cert_2[1024];

unsigned char spdm_platform_validate_certificate_request(unsigned char slot,
                                                         unsigned short offset,
                                                         unsigned short length)
{
    if (slot > 3) return 0;

    if (!initialized) {
        bzero((void *)cert_0, sizeof(cert_0));
        bzero((void *)cert_1, sizeof(cert_1));
        bzero((void *)cert_2, sizeof(cert_2));
    }

    printf("slot=%d, offset=%d, length=%d\n", slot, offset, length);

    switch (slot) {
        case 0: return offset + length <= sizeof(cert_0);
        case 1: return offset + length <= sizeof(cert_1);
        case 2: return offset + length <= sizeof(cert_2);
    }
    return 0;
};

void spdm_platform_get_certificate_data (char *data,
                                         unsigned char slot,
                                         unsigned short offset,
                                         unsigned short *length)
{
    switch (slot) {
        case 0:
            memcpy((void *)data, (void *)(cert_0 + offset), *length);
            break;
        case 1:
            memcpy((void *)data, (void *)(cert_1 + offset), *length);
            break;
        case 2:
            memcpy((void *)data, (void *)(cert_2 + offset), *length);
            break;
    }
};
