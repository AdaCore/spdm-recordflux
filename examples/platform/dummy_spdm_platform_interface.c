#include <string.h>
#include <dummy_spdm_platform_interface.h>
#include <../include/spdm_platform_interface.h>

struct instance {
    unsigned char base_hash_algo;
    long base_asym_algo;
    unsigned char measurement_hash_algo;
    int valid_nonce;
    unsigned char nonce[32];
    void *measurement_hash_ctx;
};

void spdm_platform_initialize(instance_t **instance)
{
    *instance = malloc(sizeof(instance_t));
    if(!*instance){
        errx(1, "failed to create instance");
    }
    memset(*instance, 0, sizeof(instance_t));
}

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
    //  The Responder supports MEASUREMENTS response and can perform signature generation.
    return 2;
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

unsigned char spdm_platform_select_measurement_hash_algo(instance_t *instance,
                                                         unsigned char tpm_alg_sha_256,
                                                         unsigned char tpm_alg_sha_384,
                                                         unsigned char tpm_alg_sha_512,
                                                         unsigned char tpm_alg_sha3_256,
                                                         unsigned char tpm_alg_sha3_384,
                                                         unsigned char tpm_alg_sha3_512,
                                                         unsigned char raw_bit_streams_only)
{
    if (tpm_alg_sha3_512) {
        instance->measurement_hash_algo = 64;
    } else if (tpm_alg_sha3_384) {
        instance->measurement_hash_algo = 32;
    } else if (tpm_alg_sha3_256) {
        instance->measurement_hash_algo = 16;
    } else if (tpm_alg_sha_512) {
        instance->measurement_hash_algo = 8;
    } else if (tpm_alg_sha_384) {
        instance->measurement_hash_algo = 4;
    } else if (tpm_alg_sha_256) {
        instance->measurement_hash_algo = 2;
    } else if (raw_bit_streams_only) {
        instance->measurement_hash_algo = 1;
    } else {
        // No mode set, unsupported
        instance->measurement_hash_algo = 0;
    }
    printf("instance->measurement_hash_algo=%u\n", instance->measurement_hash_algo);
    return instance->measurement_hash_algo;
}

long spdm_platform_select_base_asym_algo(instance_t *instance,
                                         unsigned char tpm_alg_ecdsa_ecc_nist_p384,
                                         unsigned char tpm_alg_rsapss_4096,
                                         unsigned char tpm_alg_rsassa_4096,
                                         unsigned char tpm_alg_ecdsa_ecc_nist_p256,
                                         unsigned char tpm_alg_rsapss_3072,
                                         unsigned char tpm_alg_rsassa_3072,
                                         unsigned char tpm_alg_rsapss_2048,
                                         unsigned char tpm_alg_rsassa_2048,
                                         unsigned char tpm_alg_ecdsa_ecc_nist_p521)
{
    if (tpm_alg_ecdsa_ecc_nist_p521) instance->base_asym_algo = 256;
    else if (tpm_alg_ecdsa_ecc_nist_p384) instance->base_asym_algo = 128;
    else if (tpm_alg_ecdsa_ecc_nist_p256) instance->base_asym_algo = 16;
    else if (tpm_alg_rsapss_4096) instance->base_asym_algo = 64;
    else if (tpm_alg_rsassa_4096) instance->base_asym_algo = 32;
    else if (tpm_alg_rsapss_3072) instance->base_asym_algo = 8;
    else if (tpm_alg_rsassa_3072) instance->base_asym_algo = 4;
    else if (tpm_alg_rsapss_2048) instance->base_asym_algo = 2;
    else if (tpm_alg_rsassa_2048) instance->base_asym_algo = 1;
    else errx(3, "No Base Asym Algo selected");
    printf("instance->base_asym_algo=%ld\n", instance->base_asym_algo);
    return instance->base_asym_algo;
}

unsigned char spdm_platform_select_base_hash_algo(instance_t *instance,
                                                  unsigned char tpm_alg_sha_256,
                                                  unsigned char tpm_alg_sha_384,
                                                  unsigned char tpm_alg_sha_512,
                                                  unsigned char tpm_alg_sha3_256,
                                                  unsigned char tpm_alg_sha3_384,
                                                  unsigned char tpm_alg_sha3_512)
{
    if (tpm_alg_sha3_512) instance->base_hash_algo = 32;
    else if (tpm_alg_sha3_384) instance->base_hash_algo = 16;
    else if (tpm_alg_sha3_256) instance->base_hash_algo = 8;
    else if (tpm_alg_sha_512) instance->base_hash_algo = 4;
    else if (tpm_alg_sha_384) instance->base_hash_algo = 2;
    else if (tpm_alg_sha_256) instance->base_hash_algo = 1;
    else errx(3, "No Base Hash Algo selected");
    printf("instance->base_hash_algo=%u\n", instance->base_hash_algo);
    return instance->base_hash_algo;
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
    return 0;
}

unsigned char spdm_platform_select_aead(unsigned char chacha20_poly1305,
                                        unsigned char aes_256_gcm,
                                        unsigned char aes_128_gcm)
{
    if (chacha20_poly1305) return 4;
    if (aes_256_gcm) return 2;
    if (aes_128_gcm) return 1;
    errx(5, "No AEAD selected");
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
    return 0;
}

void spdm_platform_get_digests_data(__unused_cross__ instance_t *instance, char *data, long *length, unsigned char *slot_mask)
{
    for (long i = 0; i < *length; i++)
    {
        data[i] = 0x00;
    }

    *slot_mask = 7;

    // measurment_hash_size is a static variable set to the size of the currently
    // selected hash in spdm_platform_select_measurement_hash_algo (in bytes)
    *length = 3 * spdm_get_measurement_hash_size(instance->measurement_hash_algo);

    for (long i = 0; i < *length; i++)
    {
        data[i] = 0x42;
    }
}

unsigned char spdm_platform_validate_certificate_request(unsigned char slot,
                                                         __unused_cross__ unsigned short offset,
                                                         __unused_cross__ unsigned short length)
{
    if (slot > 3) return 0;
    printf("slot=%d, offset=%d, length=%d\n", slot, offset, length);
    return 1;
}

void spdm_platform_get_certificate_data (__unused_cross__ instance_t *instance,
                                         __unused_cross__ char *data,
                                         __attribute__((unused)) unsigned char slot,
                                         __unused_cross__ unsigned short offset,
                                         unsigned short *length,
                                         unsigned short *total_length)
{
    __unused_cross__ void *raw_data;
    uintn size = 0;
    boolean res = read_responder_public_certificate_chain(instance->base_hash_algo,
                                                          instance->base_asym_algo,
                                                          &raw_data, &size,
                                                          NULL, NULL);
    if(!res){
        errx(0, "failed to get certificate");
    }
    if(offset > size){
        *length = 0;
        return;
    }
    *total_length = size;
    if (size - offset < *length) {
        *length = size - offset;
    }
    memcpy(data, &((unsigned char *)raw_data)[offset], *length);
}

const char *measurements[] = {"[6:0]=00h immutable rom",
                              "[6:0]=01h mutable firmware",
                              "[6:0]=02h hardware configuration",
                              "[6:0]=03h firmware configuration",
                              "[6:0]=04h measurement manifest"};


unsigned char spdm_platform_get_number_of_indices (__attribute__((unused)) instance_t *instance)
{
    return sizeof(measurements) / sizeof(const char *);
}

void spdm_platform_get_nonce(instance_t *instance,
                             __unused_cross__ void *nonce)
{
    if(getrandom(instance->nonce, 32, 0) < 0){
        errx(2, "failed to get nonce");
    }
    instance->valid_nonce = 1;
    memcpy(nonce, instance->nonce, 32);
}


void spdm_platform_get_dmtf_measurement_field(instance_t *instance,
                                              unsigned index,
                                              unsigned *representation,
                                              unsigned *type,
                                              unsigned *size,
                                              __unused_cross__ void *buffer)
{
    if(index < 1 || index > spdm_platform_get_number_of_indices(instance)){
        errx(2, "invalid measurement index");
    }
    __unused_cross__ const char *measurement = measurements[index - 1];
    const unsigned length = strlen(measurement);
    *representation = 1;
    *type = index - 1;
    if(*size < length){
        memcpy(buffer, measurement, *size);
    }else{
        memcpy(buffer, measurement, length);
        *size = length;
    }
}

unsigned spdm_platform_get_meas_signature_length (__unused_cross__ instance_t *instance)
{
    return spdm_get_measurement_hash_size(instance->measurement_hash_algo);
}

void spdm_platform_get_meas_signature (instance_t *instance,
                                       void *message,
                                       __attribute__((unused)) unsigned message_length,
                                       __unused_cross__ unsigned nonce_offset,
                                       __unused_cross__ void *signature,
                                       unsigned *signature_length)
{
    __unused_cross__ const spdm_version_number_t version = {0, 0, 1, 1};
    __unused_cross__ const unsigned hash_size = spdm_get_hash_size(instance->measurement_hash_algo);
    __unused_cross__ unsigned char hash[hash_size];
    __attribute__((unused)) uintn sig_size = *signature_length;
    if(!instance->valid_nonce){
        *signature_length = 0;
        return;
    }
    instance->valid_nonce = 0;
    memcpy(message + nonce_offset, instance->nonce, 32);
    spdm_platform_update_meas_signature(instance, message, message_length, 1);
    if(!spdm_hash_final(instance->measurement_hash_algo, instance->measurement_hash_ctx, hash)){
        return;
    }
    spdm_hash_free(instance->measurement_hash_algo, instance->measurement_hash_ctx);
    //if(!spdm_responder_data_sign(version,
    //                             SPDM_MEASUREMENTS,
    //                             instance->base_asym_algo,
    //                             instance->measurement_hash_algo,
    //                             1,
    //                             (const uint8 *)&hash,
    //                             hash_size,
    //                             signature,
    //                             &sig_size)){
    //    sig_size = 0;
    //    printf("failed to sign\n");
    //}
    //*signature_length = sig_size;
    *signature_length = spdm_get_asym_signature_size(instance->base_asym_algo);
    memset(signature, 0x42, *signature_length);
    memcpy(signature, &version, sizeof(version));
    printf("signature_length=%u\n", *signature_length);
}

int spdm_platform_update_meas_signature (instance_t *instance,
                                         __unused_cross__ void *message,
                                         __unused_cross__ unsigned size,
                                         int reset)
{
    if(!instance->measurement_hash_ctx){
        instance->measurement_hash_ctx = spdm_hash_new(instance->measurement_hash_algo);
        if(!instance->measurement_hash_ctx){
            return 1;
        }
    }
    boolean result = spdm_hash_update(instance->measurement_hash_algo,
                                      instance->measurement_hash_ctx,
                                      message,
                                      size);
    if(reset){
        spdm_hash_free(instance->measurement_hash_algo, instance->measurement_hash_ctx);
    }
    return result;
}
