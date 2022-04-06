#ifndef __DUMMY_SPDM_PLATFORM_INTERFACE_H__
#define __DUMMY_SPDM_PLATFORM_INTERFACE_H__

#ifdef __linux__

#include <stdio.h>
#include <stdlib.h>
#include <err.h>
#include <sys/random.h>
#include <spdm_device_secret_lib_internal.h>
#define __unused_cross__

#else

#define errx(...) {}
#define printf(...)
#define malloc(...) 0
#define read_responder_public_certificate_chain(...) 1
#define memcpy(...)
#define getrandom(...) 0
#define strlen(...) 0
#define __unused_cross__ __attribute__((unused))

typedef unsigned long uintn;
typedef unsigned char boolean;

#endif

#endif
