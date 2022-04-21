#ifndef __SPDM_PLATFORM_INTERFACE__
#define __SPDM_PLATFORM_INTERFACE__

struct instance;
typedef struct instance instance_t;

void spdm_platform_initialize(instance_t **instance);

unsigned char spdm_platform_config_ct_exponent(instance_t *instance);
unsigned char spdm_platform_config_cap_mac(instance_t *instance);
unsigned char spdm_platform_config_cap_encrypt(instance_t *instance);
unsigned char spdm_platform_config_cap_meas_fresh(instance_t *instance);
unsigned char spdm_platform_config_cap_meas(instance_t *instance);
unsigned char spdm_platform_config_cap_chal(instance_t *instance);
unsigned char spdm_platform_config_cap_cert(instance_t *instance);
unsigned char spdm_platform_config_cap_cache(instance_t *instance);
unsigned char spdm_platform_config_cap_handshake_in_the_clear(instance_t *instance);
unsigned char spdm_platform_config_cap_key_upd(instance_t *instance);
unsigned char spdm_platform_config_cap_hbeat(instance_t *instance);
unsigned char spdm_platform_config_cap_encap(instance_t *instance);
unsigned char spdm_platform_config_cap_psk(instance_t *instance);
unsigned char spdm_platform_config_cap_key_ex(instance_t *instance);
unsigned char spdm_platform_config_cap_mut_auth(instance_t *instance);
unsigned char spdm_platform_config_cap_pub_key_id(instance_t *instance);

unsigned char spdm_platform_select_measurement_hash_algo(instance_t *instance,
                                                         unsigned char tpm_alg_sha_256,
                                                         unsigned char tpm_alg_sha_384,
                                                         unsigned char tpm_alg_sha_512,
                                                         unsigned char tpm_alg_sha3_256,
                                                         unsigned char tpm_alg_sha3_384,
                                                         unsigned char tpm_alg_sha3_512,
                                                         unsigned char raw_bit_streams_only);

long spdm_platform_select_base_asym_algo(instance_t *instance,
                                         unsigned char tpm_alg_ecdsa_ecc_nist_p384,
                                         unsigned char tpm_alg_rsapss_4096,
                                         unsigned char tpm_alg_rsassa_4096,
                                         unsigned char tpm_alg_ecdsa_ecc_nist_p256,
                                         unsigned char tpm_alg_rsapss_3072,
                                         unsigned char tpm_alg_rsassa_3072,
                                         unsigned char tpm_alg_rsapss_2048,
                                         unsigned char tpm_alg_rsassa_2048,
                                         unsigned char tpm_alg_ecdsa_ecc_nist_p521);

unsigned char spdm_platform_select_base_hash_algo(instance_t *instance,
                                                  unsigned char tpm_alg_sha_256,
                                                  unsigned char tpm_alg_sha_384,
                                                  unsigned char tpm_alg_sha_512,
                                                  unsigned char tpm_alg_sha3_256,
                                                  unsigned char tpm_alg_sha3_384,
                                                  unsigned char tpm_alg_sha3_512);

unsigned char spdm_platform_select_dhe(instance_t *instance,
                                       unsigned char secp521r1,
                                       unsigned char secp384r1,
                                       unsigned char secp256r1,
                                       unsigned char ffdhe4096,
                                       unsigned char ffdhe3072,
                                       unsigned char ffdhe2048);

unsigned char spdm_platform_select_aead(instance_t *instance,
                                        unsigned char chacha20_poly1305,
                                        unsigned char aes_256_gcm,
                                        unsigned char aes_128_gcm);

long spdm_platform_select_rbba(instance_t *instance,
                               unsigned char ra_tpm_alg_ecdsa_ecc_nist_p384,
                               unsigned char ra_tpm_alg_rsapss_4096,
                               unsigned char ra_tpm_alg_rsassa_4096,
                               unsigned char ra_tpm_alg_ecdsa_ecc_nist_p256,
                               unsigned char ra_tpm_alg_rsapss_3072,
                               unsigned char ra_tpm_alg_rsassa_3072,
                               unsigned char ra_tpm_alg_rsapss_2048,
                               unsigned char ra_tpm_alg_rsassa_2048,
                               unsigned char ra_tpm_alg_ecdsa_ecc_nist_p521);

void spdm_platform_get_digests_data(instance_t *instance, char *data, long *length, unsigned char *slot_mask);

unsigned char spdm_platform_validate_certificate_request(instance_t *instance,
                                                         unsigned char slot,
                                                         unsigned short offset,
                                                         unsigned short length);

void spdm_platform_get_certificate_data (instance_t *instance,
                                         char *data,
                                         unsigned char slot,
                                         unsigned short offset,
                                         unsigned short *length,
                                         unsigned short *total_length);

unsigned char spdm_platform_get_number_of_indices (instance_t *instance);

void spdm_platform_get_nonce(instance_t *instance, void *nonce);

void spdm_platform_get_dmtf_measurement_field (instance_t *instance,
                                               unsigned index,
                                               unsigned *representation,
                                               unsigned *type,
                                               unsigned *size,
                                               void *buffer);

unsigned spdm_platform_get_meas_signature_length (instance_t *instance);

void spdm_platform_get_meas_signature (instance_t *instance,
                                       void *message,
                                       unsigned message_length,
                                       unsigned nonce_offset,
                                       void *signature,
                                       unsigned *signature_length);

int spdm_platform_update_meas_signature (instance_t *instance,
                                         void *message,
                                         unsigned size,
                                         int reset);

#endif // __SPDM_PLATFORM_INTERFACE__
