# SPDM

RecordFlux/SPARK implementation of SPDM 1.1

WORK IN PROGRESS.

## Prerequisites

To build and test the SPDM library you need the following prerequisites:

- Debian 11 or Ubuntu 20.04 LTS
- GNAT Pro >= 21.x native compiler for linux-x86_64
- GNAT Pro >= 21.x cross compiler for your target platform
- Packages
	- libgmp-dev
	- GNU make
	- cmake
	- Python >= 3.7
	- virtualenv
	- pip

## Running the tests

```
$ PATH=/path/to/HOST/compiler:$PATH make
[this will take a while]
[...]
pdm_receive_spdm_response[0] (0x4):
0000: 11 7f 07 e3
spdm_init_connection - 0x7
Platform port Transmit command: 00 00 ff fe
Platform port Transmit transport_type: 00 00 00 00
Platform port Transmit size: 00 00 00 00
Platform port Transmit buffer:

State: Proxy_Send_Request
Error: invalid conversion "SPDM::Request (Request.SPDM_Payload)"
receive_platform_data Error - 0
Client stopped
```

The last error is expected, as the SPDM protocol is not yet fully implemented
and the session stops once spdm_emu sends an unsupported message. The "Client
stopped" message indicates that the test finished successfully.

## Building the library

Building the target library requires a two-step approach using a native and a
cross toolchain, respectively.

### RecordFlux installation and code generation

For the following step a Python virtualenv is recommended, but a user
installation will also work:

```
$ PATH=/path/to/your/NATIVE/gnat:$PATH pip3 install contrib/RecordFlux
[this will take a while]
```

### Cross-build the library

If a virtualenv had been used in the previous step, it must be active now.

```
$ LOCAL_RFLX=1 PATH=/path/to/your/CROSS/gnat:$PATH make lib
[...]

Build Libraries
   [gprlib]       spdm.lexch
   [archive]      libspdm.a
   [index]        libspdm.a
```

The resulting static library is build/lib/libspdm.a.
