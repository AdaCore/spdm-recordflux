#include <stdio.h>
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

unsigned char spdm_platform_select_measurement_hash_algo(unsigned char tpm_alg_sha_256,
                                                         unsigned char tpm_alg_sha_384,
                                                         unsigned char tpm_alg_sha_512,
                                                         unsigned char tpm_alg_sha3_256,
                                                         unsigned char tpm_alg_sha3_384,
                                                         unsigned char tpm_alg_sha3_512)
{
    printf("measurement_hash_algo: SHA_256: %d, SHA_384: %d, SHA_512: %d, SHA3_256: %d, SHA3_384: %d, SHA3_512: %d\n",
        tpm_alg_sha_256,
        tpm_alg_sha_384,
        tpm_alg_sha_512,
        tpm_alg_sha3_256,
        tpm_alg_sha3_384,
        tpm_alg_sha3_512);
    if (tpm_alg_sha3_256) return 8;
    if (tpm_alg_sha_256) return 1;
    if (tpm_alg_sha_384) return 2;
    if (tpm_alg_sha_512) return 4;
    if (tpm_alg_sha3_384) return 16;
    if (tpm_alg_sha3_512) return 32;
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
    return 0;
    if (tpm_alg_ecdsa_ecc_nist_p384) return 128;
    if (tpm_alg_rsapss_4096) return 64;
    if (tpm_alg_rsassa_4096) return 32;
    if (tpm_alg_ecdsa_ecc_nist_p256) return 16;
    if (tpm_alg_rsapss_3072) return 8;
    if (tpm_alg_rsassa_3072) return 4;
    if (tpm_alg_rsapss_2048) return 2;
    if (tpm_alg_rsassa_2048) return 1;
    if (tpm_alg_ecdsa_ecc_nist_p521) return 256;
    return 0;
}

unsigned char spdm_platform_select_base_hash_algo(unsigned char tpm_alg_sha_256,
                                                  unsigned char tpm_alg_sha_384,
                                                  unsigned char tpm_alg_sha_512,
                                                  unsigned char tpm_alg_sha3_256,
                                                  unsigned char tpm_alg_sha3_384,
                                                  unsigned char tpm_alg_sha3_512)
{
    return 1;
    if (tpm_alg_sha3_512) return 1;
    if (tpm_alg_sha_256) return 32;
    if (tpm_alg_sha_384) return 16;
    if (tpm_alg_sha_512) return 8;
    if (tpm_alg_sha3_256) return 4;
    if (tpm_alg_sha3_384) return 1;
    return 0;
}

unsigned char spdm_platform_select_dhe(unsigned char secp521r1,
                                       unsigned char secp384r1,
                                       unsigned char secp256r1,
                                       unsigned char ffdhe4096,
                                       unsigned char ffdhe3072,
                                       unsigned char ffdhe2048)
{
    return 16;
    if (secp384r1) return 16;
    if (secp521r1) return 32;
    if (secp256r1) return 8;
    if (ffdhe4096) return 4;
    if (ffdhe3072) return 2;
    if (ffdhe2048) return 1;
    return 0;
}

unsigned char spdm_platform_select_aead(unsigned char chacha20_poly1305,
                                        unsigned char aes_256_gcm,
                                        unsigned char aes_128_gcm)
{
    return 2;
    if (aes_256_gcm) return 2;
    if (chacha20_poly1305) return 4;
    if (aes_128_gcm) return 1;
    return 0;
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
    return 16;
    if (ra_tpm_alg_rsapss_3072) return 16;
    if (ra_tpm_alg_ecdsa_ecc_nist_p384) return 256;
    if (ra_tpm_alg_rsapss_4096) return 128;
    if (ra_tpm_alg_rsassa_4096) return 64;
    if (ra_tpm_alg_ecdsa_ecc_nist_p256) return 32;
    if (ra_tpm_alg_rsassa_3072) return 8;
    if (ra_tpm_alg_rsapss_2048) return 4;
    if (ra_tpm_alg_rsassa_2048) return 2;
    if (ra_tpm_alg_ecdsa_ecc_nist_p521) return 1;
    return 0;
}

unsigned char spdm_platform_config_slot_0_present()
{
    return 1;
}

unsigned char spdm_platform_config_slot_1_present()
{
    return 1;
}

unsigned char spdm_platform_config_slot_2_present()
{
    return 1;
}

unsigned char spdm_platform_config_slot_3_present()
{
    return 0;
}

unsigned char spdm_platform_config_slot_4_present()
{
    return 0;
}

unsigned char spdm_platform_config_slot_5_present()
{
    return 1;
}

unsigned char spdm_platform_config_slot_6_present()
{
    return 0;
}

unsigned char spdm_platform_config_slot_7_present()
{
    return 0;
}

void spdm_platform_get_digests_data(char *data, long length)
{
    for (long i = 0; i < length; i++)
    {
        data[i] = 0x42;
    }
}
