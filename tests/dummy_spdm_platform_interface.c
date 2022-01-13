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
