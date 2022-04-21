#ifndef __DUMMY_SPDM_PLATFORM_INTERFACE_H__
#define __DUMMY_SPDM_PLATFORM_INTERFACE_H__

#ifdef __linux__

#include <stdio.h>
#include <stdlib.h>
#include <err.h>
#include <sys/random.h>
#include <spdm_device_secret_lib_internal.h>
#include <spdm_crypt_lib.h>
#include <industry_standard/spdm.h>
#define __unused_cross__

#else

#define errx(...) {}
#define printf(...)
#define malloc(...) 0
#define memset(...)
#define read_responder_public_certificate_chain(...) 1
#define memcpy(...)
#define getrandom(...) 0
#define strlen(...) 0
#define spdm_get_hash_size(...) 0
#define spdm_get_measurement_hash_size(...) 0
#define spdm_hash_new(...) 0
#define spdm_hash_free(...)
#define spdm_hash_update(...) 1
#define spdm_hash_final(...) 1
#define spdm_hash_all(...) 1
#define spdm_responder_data_sign(...) 1
#define spdm_get_asym_signature_size(...) 0
typedef struct {
    unsigned char alpha;
    unsigned char update_version_number;
    unsigned char minor_version;
    unsigned char major_version;
} spdm_version_number_t;
#define __unused_cross__ __attribute__((unused))

typedef unsigned long uintn;
typedef unsigned char boolean;

#endif

#endif
