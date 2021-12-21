#ifndef __SPDM_PLATFORM_INTERFACE__
#define __SPDM_PLATFORM_INTERFACE__

unsigned char spdm_platform_config_ct_exponent(void);
unsigned char spdm_platform_config_cap_mac(void);
unsigned char spdm_platform_config_cap_encrypt(void);
unsigned char spdm_platform_config_cap_meas_fresh(void);
unsigned char spdm_platform_config_cap_meas(void);

#endif // __SPDM_PLATFORM_INTERFACE__
