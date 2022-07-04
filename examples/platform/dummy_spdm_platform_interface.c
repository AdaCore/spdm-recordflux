#include <string.h>
#include <dummy_spdm_platform_interface.h>
#include <../include/spdm_platform_interface.h>

struct instance {
    unsigned char base_hash_algo;
    long base_asym_algo;
    unsigned char measurement_hash_algo;
    unsigned char dhe_named_group;
    int valid_nonce;
    unsigned char nonce[32];
    void *hashes[8];
    int hash_sizes[8];
    unsigned char dhe_key[512];
    unsigned dhe_key_size;
    unsigned char secure_session;
    unsigned transcript_stage;
    uint16 transcript_headers[16];
};

void spdm_platform_initialize(instance_t **instance)
{
    *instance = malloc(sizeof(instance_t));
    if(!*instance){
        errx(1, "failed to create instance");
    }
    memset(*instance, 0, sizeof(instance_t));
    memset((*instance)->hash_sizes, -1, sizeof(int) * 8);
    (*instance)->transcript_headers[0]  = 0x8410; //[GET_VERSION].*
    (*instance)->transcript_headers[1]  = 0x0410; //[VERSION].*
    (*instance)->transcript_headers[2]  = 0xe111; //[GET_CAPABILITIES].*
    (*instance)->transcript_headers[3]  = 0x6111; //[CAPABILITIES].*
    (*instance)->transcript_headers[4]  = 0xe311; //[NEGOTIATE_ALGORITHMS].*
    (*instance)->transcript_headers[5]  = 0x6311; //[ALGORITHMS].*
    (*instance)->transcript_headers[6]  = 0xffff; //Hash of the specified certificate chain in DER format (i.e., KEY_EXCHANGE Param2)
    (*instance)->transcript_headers[7]  = 0xe411; //[KEY_EXCHANGE].*
    (*instance)->transcript_headers[8]  = 0x6411; //[KEY_EXCHANGE_RSP].* except the `Signature` and `ResponderVerifyData` fields.
    (*instance)->transcript_headers[9]  = 0xffff; //[KEY_EXCHANGE_RSP].Signature ([KEY_EXCHANGE_RSP].* except the `ResponderVerifyData` field.)
    (*instance)->transcript_headers[10] = 0xffff; //[KEY_EXCHANGE_RSP].ResponderVerifyData ([KEY_EXCHANGE].*)
    (*instance)->transcript_headers[11] = 0xffff; //Hash of the specified certificate chain in DER format (i.e., FINISH Param2)
    (*instance)->transcript_headers[12] = 0xe511; //[FINISH].SPDM Header Fields
    (*instance)->transcript_headers[13] = 0xffff; //[FINISH].Signature
    (*instance)->transcript_headers[14] = 0x6511; //[FINISH_RSP].SPDM Header fields
    (*instance)->transcript_headers[15] = 0xffff; //[FINISH_RSP].*
}
#ifdef FEATURE_KEY_EXCHANGE
unsigned char spdm_platform_is_secure_session(instance_t *instance)
{
    return instance->secure_session;
}
#endif
unsigned char spdm_platform_config_ct_exponent(__attribute__((unused)) instance_t *instance) {
    return 10;
}

unsigned char spdm_platform_config_cap_meas_fresh(__attribute__((unused)) instance_t *instance) {
    return 0;
}

unsigned char spdm_platform_config_cap_meas(__attribute__((unused)) instance_t *instance) {
    //  The Responder supports MEASUREMENTS response and can perform signature generation.
    return 2;
}

unsigned char spdm_platform_config_cap_chal(__attribute__((unused)) instance_t *instance) {
    return 0;
}

unsigned char spdm_platform_config_cap_cert(__attribute__((unused)) instance_t *instance) {
    //  Supported in WP 2.3.7
    return 1;
}

unsigned char spdm_platform_config_cap_cache(__attribute__((unused)) instance_t *instance) {
    return 0;
}

unsigned char spdm_platform_config_cap_key_upd(__attribute__((unused)) instance_t *instance) {
    return 1;
}

unsigned char spdm_platform_config_cap_hbeat(__attribute__((unused)) instance_t *instance) {
    return 0;
}

unsigned char spdm_platform_config_cap_encap(__attribute__((unused)) instance_t *instance) {
    return 0;
}

unsigned char spdm_platform_config_cap_mut_auth(__attribute__((unused)) instance_t *instance) {
    return 0;
}

unsigned char spdm_platform_config_cap_pub_key_id(__attribute__((unused)) instance_t *instance) {
    return 0;
}
#ifdef FEATURE_KEY_EXCHANGE
unsigned char spdm_platform_config_cap_mac(__attribute__((unused)) instance_t *instance) {
    return 1;
}

unsigned char spdm_platform_config_cap_encrypt(__attribute__((unused)) instance_t *instance) {
    return 0;
}

unsigned char spdm_platform_config_cap_psk(__attribute__((unused)) instance_t *instance) {
    return 0;
}

unsigned char spdm_platform_config_cap_key_ex(__attribute__((unused)) instance_t *instance) {
    return 1;
}

unsigned char spdm_platform_config_cap_handshake_in_the_clear(__attribute__((unused)) instance_t *instance) {
    return 0;
}
#endif
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
#ifdef FEATURE_KEY_EXCHANGE
unsigned char spdm_platform_select_dhe(instance_t *instance,
                                       unsigned char secp521r1,
                                       unsigned char secp384r1,
                                       unsigned char secp256r1,
                                       unsigned char ffdhe4096,
                                       unsigned char ffdhe3072,
                                       unsigned char ffdhe2048)
{
    if (secp521r1) instance->dhe_named_group = 32;
    else if (secp384r1) instance->dhe_named_group = 16;
    else if (secp256r1) instance->dhe_named_group = 8;
    else if (ffdhe4096) instance->dhe_named_group = 4;
    else if (ffdhe3072) instance->dhe_named_group = 2;
    else if (ffdhe2048) instance->dhe_named_group = 1;
    else errx(4, "No DHE selected");
    printf("instance->dhe_named_group=%u\n", instance->dhe_named_group);
    return instance->dhe_named_group;
}

unsigned char spdm_platform_select_aead(__attribute__((unused)) instance_t *instance,
                                        unsigned char chacha20_poly1305,
                                        unsigned char aes_256_gcm,
                                        unsigned char aes_128_gcm)
{
    if (chacha20_poly1305) return 4;
    if (aes_256_gcm) return 2;
    if (aes_128_gcm) return 1;
    errx(5, "No AEAD selected");
    return 0;
}
#endif
long spdm_platform_select_rbaa(__attribute__((unused)) instance_t *instance,
                               unsigned char ra_tpm_alg_ecdsa_ecc_nist_p384,
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

void spdm_platform_get_digests_data(__unused_cross__ instance_t *instance,
                                    __unused_cross__ char *data,
                                    long *length,
                                    unsigned char *slot_mask)
{
    __unused_cross__ void *raw_data;
    __unused_cross__ uintn size = 0;
    const long hash_size = spdm_get_hash_size(instance->base_hash_algo);
    boolean res = read_responder_public_certificate_chain(instance->base_hash_algo,
                                                          instance->base_asym_algo,
                                                          &raw_data, &size,
                                                          NULL, NULL);
    if(!res){
        errx(0, "failed to get certificate");
    }
    if(*length < hash_size){
        *length = 0;
        *slot_mask = 0;
        return;
    }
    res = spdm_hash_all(instance->base_hash_algo, raw_data, size, (void *)data);
    if(!res){
        errx(1, "failed to hash certificate");
    }
    if(*length < 2 * hash_size){
        *length = hash_size;
        *slot_mask = 1;
        return;
    }
    memcpy(data + hash_size, data, hash_size);
    if(*length < 3 * hash_size){
        *length = 2 * hash_size;
        *slot_mask = 3;
        return;
    }
    memcpy(data + 2 * hash_size, data, hash_size);
    *slot_mask = 7;
    *length = 3 * hash_size;
}

unsigned char spdm_platform_validate_certificate_request(__attribute__((unused)) instance_t *instance,
                                                         unsigned char slot,
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
#ifdef FEATURE_KEY_EXCHANGE
unsigned char spdm_platform_get_number_of_indices_tcb (__attribute__((unused)) instance_t *instance)
{
    return sizeof(measurements) / sizeof(const char *);
}
#endif
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

void spdm_platform_get_meas_opaque_data(__attribute__((unused)) instance_t *instance,
                                        __attribute__((unused)) void *data,
                                        unsigned *size)
{
    *size = 0;
}

unsigned spdm_platform_get_new_hash(instance_t *instance)
{
    for(int i = 0; i < 8; i++){
        if(instance->hash_sizes[i] == -1){
            instance->hashes[i] = 0;
            instance->hash_sizes[i] = 0;
            return i;
        }
    }
    return 0xff;
}

unsigned char spdm_platform_valid_hash_id(instance_t *instance,
                                          unsigned hash)
{
    return hash <= 8 && instance->hash_sizes[hash] != -1;
}

unsigned spdm_platform_reset_hash(instance_t *instance,
                                  unsigned hash)
{
    if(!spdm_platform_valid_hash_id(instance, hash)){
        return hash;
    }
    if(instance->hash_sizes[hash] != -1){
        free(instance->hashes[hash]);
        instance->hash_sizes[hash] = -1;
        instance->hashes[hash] = 0;
    }
#ifdef FEATURE_KEY_EXCHANGE
    if(hash == 1){
        instance->transcript_stage = 0;
    }
#endif
    return spdm_platform_get_new_hash(instance);
}

unsigned char spdm_platform_update_hash(instance_t *instance,
                                        unsigned hash,
                                        __unused_cross__ void *data,
                                        __unused_cross__ unsigned offset,
                                        unsigned size)
{
#ifdef FEATURE_KEY_EXCHANGE
    if(hash == 1){
        printf("transcript stage %u: %04hx\n",
               instance->transcript_stage,
               instance->transcript_headers[instance->transcript_stage]);
        if(instance->transcript_headers[instance->transcript_stage] != 0xffff
                && instance->transcript_headers[instance->transcript_stage] != *(uint16 *)data){
            errx(1, "unexpected transcript stage, expected %04hx, got %04hx",
                    instance->transcript_headers[instance->transcript_stage],
                    *(uint16 *)data);
        }
        instance->transcript_stage = instance->transcript_stage + 1;
    }
#endif
    if(!spdm_platform_valid_hash_id(instance, hash)){
        errx(1, "invalid hash slot");
        return 0;
    }
    instance->hashes[hash] = realloc(instance->hashes[hash], instance->hash_sizes[hash] + size);
    if(!instance->hashes[hash]){
        errx(1, "realloc failed");
        return 0;
    }
    memcpy(instance->hashes[hash] + instance->hash_sizes[hash], data + offset, size);
    instance->hash_sizes[hash] = instance->hash_sizes[hash] + size;
    return 1;
}

unsigned char spdm_platform_update_hash_nonce(instance_t *instance,
                                              unsigned hash)
{
    if(!instance->valid_nonce){
        return 0;
    }
    instance->valid_nonce = 0;
    return spdm_platform_update_hash(instance, hash, instance->nonce, 0, 32);
}

void spdm_platform_get_signature(instance_t *instance,
                                 unsigned hash,
                                 __attribute__((unused)) unsigned char slot,
                                 __unused_cross__ void *signature,
                                 unsigned *size)
{
    __unused_cross__ const spdm_version_number_t version = {0, 0, 1, 1};
    __unused_cross__ const unsigned hash_size = spdm_get_hash_size(instance->base_hash_algo);
    __unused_cross__ unsigned char hash_data[*size];
    if(slot >= 3){
        *size = 0;
        return;
    }
    uintn sig_size = *size;
    if(!spdm_platform_valid_hash_id(instance, hash)){
        errx(1, "invalid hash slot");
    }
    boolean res = spdm_hash_all(instance->base_hash_algo,
                                instance->hashes[hash],
                                instance->hash_sizes[hash],
                                hash_data);
    if(!res){
        errx(1, "failed to hash summary");
    }
    __unused_cross__ unsigned code = 0;
    if(hash == 0){
        code = SPDM_MEASUREMENTS;
#ifdef FEATURE_KEY_EXCHANGE
    }
    if(hash == 1){
        code = SPDM_KEY_EXCHANGE_RSP;
#endif
    }
    if(!spdm_responder_data_sign(version,
                                 code,
                                 instance->base_asym_algo,
                                 instance->base_hash_algo,
                                 1,
                                 (const uint8 *)&hash_data,
                                 hash_size,
                                 signature,
                                 &sig_size)){
        sig_size = 0;
        printf("failed to sign\n");
    }
    *size = sig_size;
    printf("signature_length=%u\n", *size);
}
#ifdef FEATURE_KEY_EXCHANGE
void spdm_platform_get_exchange_data (__unused_cross__ instance_t *instance,
                                      __unused_cross__ void *data,
                                      unsigned size)
{
    uintn dhe_key_size = spdm_get_dhe_pub_key_size(instance->dhe_named_group);
    __unused_cross__ void *dhe_context = spdm_secured_message_dhe_new(instance->dhe_named_group);
    __unused_cross__ uint8 dhe_key[dhe_key_size];
    spdm_secured_message_dhe_generate_key(instance->dhe_named_group, dhe_context, dhe_key, &dhe_key_size);
    uintn dhe_priv_key_size = sizeof(instance->dhe_key);
    if(!spdm_dhe_compute_key(
            instance->dhe_named_group,
            dhe_context,
            data, size,
            instance->dhe_key, &dhe_priv_key_size)){
        errx(1, "failed to compute dhe key");
    }
    instance->dhe_key_size = dhe_priv_key_size;
    spdm_secured_message_dhe_free(instance->dhe_named_group, dhe_context);
    if(size < dhe_key_size){
        errx(1, "insufficient size for dhe key");
    }
    memcpy(data, dhe_key, dhe_key_size);
}

unsigned char spdm_platform_get_heartbeat_period (__attribute__((unused)) instance_t *instance)
{
    return 0;
}

unsigned char spdm_platform_valid_session_id (__attribute__((unused)) instance_t *instance,
                                              __attribute__((unused)) unsigned short session_id)
{
    return 1;
}

unsigned short spdm_platform_get_session_id (__attribute__((unused)) instance_t *instance,
                                             unsigned short session_id)
{
    return ~session_id;
}

unsigned char spdm_platform_use_mutual_auth (__attribute__((unused)) instance_t *instance)
{
    return 0;
}

void spdm_platform_get_summary_hash(__unused_cross__ instance_t *instance,
                                    __unused_cross__ void *summary,
                                    __unused_cross__ unsigned summary_size,
                                    __unused_cross__ void *hash,
                                    unsigned *hash_length)
{
    boolean res = spdm_hash_all(instance->base_hash_algo, summary, summary_size, hash);
    if(!res){
        errx(1, "failed to hash summary");
    }
    *hash_length = spdm_get_hash_size(instance->base_hash_algo);
}

unsigned char spdm_platform_update_hash_cert(instance_t *instance,
                                             unsigned hash,
                                             unsigned char slot)
{
    if(slot != 0){
        return 0;
    }
    __unused_cross__ void *raw_data = 0;
    uintn size = 0;
    boolean res = read_responder_public_certificate_chain(instance->base_hash_algo,
                                                          instance->base_asym_algo,
                                                          &raw_data, &size,
                                                          NULL, NULL);
    if(!res){
        errx(0, "failed to get certificate");
    }
    return spdm_platform_update_hash(instance, hash, raw_data, 0, size);
}

void spdm_platform_get_key_ex_opaque_data(__attribute__((unused)) instance_t *instance,
                                          __attribute__((unused)) void *data,
                                          __attribute__((unused)) unsigned *size)
{
}

void spdm_platform_get_key_ex_verify_data(__unused_cross__ instance_t *instance,
                                          __attribute__((unused)) void *data,
                                          unsigned *size)
{
    *size = spdm_get_hash_size(instance->base_hash_algo);
}

unsigned char spdm_platform_validate_finish_signature(__attribute__((unused)) instance_t *instance,
                                                      __attribute__((unused)) unsigned hash,
                                                      __attribute__((unused)) void *signature,
                                                      __attribute__((unused)) unsigned size,
                                                      __attribute__((unused)) unsigned char slot)
{
    return 1;
}

unsigned char spdm_platform_validate_finish_hmac(__attribute__((unused)) instance_t *instance,
                                                 __attribute__((unused)) unsigned hash,
                                                 __attribute__((unused)) void *hmac,
                                                 __attribute__((unused)) unsigned size,
                                                 __attribute__((unused)) unsigned char slot)
{
    return 1;
}

void spdm_platform_get_finish_verify_data(__unused_cross__ instance_t *instance,
                                          __attribute__((unused)) unsigned hash,
                                          __attribute__((unused)) unsigned char slot,
                                          __attribute__((unused)) void *data,
                                          unsigned *size)
{
    *size = spdm_get_hash_size(instance->base_hash_algo);
}

unsigned char spdm_platform_set_secure_session(instance_t *instance,
                                               unsigned char enable)
{
    printf("secure session: %u\n", enable);
    instance->secure_session = enable;
    return enable;
}

unsigned char spdm_platform_key_update(__attribute__((unused)) instance_t *instance,
                                       __attribute__((unused)) unsigned operation)
{
    return 1;
}

unsigned char spdm_platform_end_session(instance_t *instance)
{
    instance->secure_session = 0;
    memset(instance->dhe_key, 0, 512);
    instance->dhe_key_size = 0;
    return 1;
}
#endif
