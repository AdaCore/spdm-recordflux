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

#endif // __SPDM_PLATFORM_INTERFACE__
