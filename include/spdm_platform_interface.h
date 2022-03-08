#ifndef __SPDM_PLATFORM_INTERFACE__
#define __SPDM_PLATFORM_INTERFACE__

unsigned char spdm_platform_config_ct_exponent(void);
unsigned char spdm_platform_config_cap_mac(void);
unsigned char spdm_platform_config_cap_encrypt(void);
unsigned char spdm_platform_config_cap_meas_fresh(void);
unsigned char spdm_platform_config_cap_meas(void);
unsigned char spdm_platform_config_cap_chal(void);
unsigned char spdm_platform_config_cap_cert(void);
unsigned char spdm_platform_config_cap_cache(void);
unsigned char spdm_platform_config_cap_handshake_in_the_clear(void);
unsigned char spdm_platform_config_cap_key_upd(void);
unsigned char spdm_platform_config_cap_hbeat(void);
unsigned char spdm_platform_config_cap_encap(void);
unsigned char spdm_platform_config_cap_psk(void);
unsigned char spdm_platform_config_cap_key_ex(void);
unsigned char spdm_platform_config_cap_mut_auth(void);
unsigned char spdm_platform_config_cap_pub_key_id(void);

unsigned char spdm_platform_select_measurement_hash_algo(unsigned char tpm_alg_sha_256,
                                                         unsigned char tpm_alg_sha_384,
                                                         unsigned char tpm_alg_sha_512,
                                                         unsigned char tpm_alg_sha3_256,
                                                         unsigned char tpm_alg_sha3_384,
                                                         unsigned char tpm_alg_sha3_512,
                                                         unsigned char raw_bit_streams_only);

long spdm_platform_select_base_asym_algo(unsigned char tpm_alg_ecdsa_ecc_nist_p384,
                                         unsigned char tpm_alg_rsapss_4096,
                                         unsigned char tpm_alg_rsassa_4096,
                                         unsigned char tpm_alg_ecdsa_ecc_nist_p256,
                                         unsigned char tpm_alg_rsapss_3072,
                                         unsigned char tpm_alg_rsassa_3072,
                                         unsigned char tpm_alg_rsapss_2048,
                                         unsigned char tpm_alg_rsassa_2048,
                                         unsigned char tpm_alg_ecdsa_ecc_nist_p521);

unsigned char spdm_platform_select_base_hash_algo(unsigned char tpm_alg_sha_256,
                                                  unsigned char tpm_alg_sha_384,
                                                  unsigned char tpm_alg_sha_512,
                                                  unsigned char tpm_alg_sha3_256,
                                                  unsigned char tpm_alg_sha3_384,
                                                  unsigned char tpm_alg_sha3_512);

unsigned char spdm_platform_select_dhe(unsigned char secp521r1,
                                       unsigned char secp384r1,
                                       unsigned char secp256r1,
                                       unsigned char ffdhe4096,
                                       unsigned char ffdhe3072,
                                       unsigned char ffdhe2048);

unsigned char spdm_platform_select_aead(unsigned char chacha20_poly1305,
                                        unsigned char aes_256_gcm,
                                        unsigned char aes_128_gcm);

long spdm_platform_select_rbba(unsigned char ra_tpm_alg_ecdsa_ecc_nist_p384,
                               unsigned char ra_tpm_alg_rsapss_4096,
                               unsigned char ra_tpm_alg_rsassa_4096,
                               unsigned char ra_tpm_alg_ecdsa_ecc_nist_p256,
                               unsigned char ra_tpm_alg_rsapss_3072,
                               unsigned char ra_tpm_alg_rsassa_3072,
                               unsigned char ra_tpm_alg_rsapss_2048,
                               unsigned char ra_tpm_alg_rsassa_2048,
                               unsigned char ra_tpm_alg_ecdsa_ecc_nist_p521);

void spdm_platform_get_digests_data(char *data, long *length, unsigned char *slot_mask);

unsigned char spdm_platform_validate_certificate_request(unsigned char slot,
                                                         unsigned short offset,
                                                         unsigned short length);

void spdm_platform_get_certificate_data (char *data,
                                         unsigned char slot,
                                         unsigned short offset,
                                         unsigned short *length,
                                         unsigned short *total_length);

#endif // __SPDM_PLATFORM_INTERFACE__
