# SPDM

RecordFlux/SPARK implementation of SPDM 1.1

## Prerequisites

To build and test the SPDM library you need the following prerequisites:

- Debian 11 or Ubuntu 20.04 LTS
- RecordFlux == 0.8.0
- GNAT Pro >= 21.x native compiler for linux-x86_64
- GNAT Pro >= 21.x cross compiler for your target platform
- Packages
	- libgmp-dev
	- GNU make
	- cmake
	- Python >= 3.7
	- pip

## Running the tests

To run the tests sucessfully, the repository must be checked out recursively to
make the required submodules available. Either `git checkout --recursive ...`
can be used on initial checkout or the respective submodules can be updated
recursively using `git submodule update --recursive` after checkout.

Running all tests requires a *native* GNAT compiler, as well as a cross compiler for ARM and RISCV64:

```
$ PATH=/path/to/HOST/compiler:/path/to/ARM_CROSS/compiler:/path/to/RISCV64_CROSS/compiler:$PATH make test
[this will take a while]
[...]
Loading request from ../../tests/data/spdm/Request/valid/V616-034_SPDM_GET_MEASUREMENTS_ALL_VALID_SLOT.raw
State: Send_Request
State: Receive_Response
State: Return_Response
Response: 11 60 6 0 0 0 0 0 E3 75 8C BD 6A C7 6A 9E 57 C5 FF 7C DC 2B 1B 1 AE 41 82 94 22 57 A4 B9 92 EC 14 18 82 8B 44 6E 0 0 D5 88 2C BF C2 C7 70 B9 48 7B CD D4 21 B7 54 9D A5 6D 96 64 D0 8E 53 72 35 7D 9C 20 A9 EB 6E 59 52 AE 12 5D 81 8 FC 1F 6A C2 E6 7A B9 22 DF 3F 27 46 52 40 B8 6F D2 C1 EE 2E 43 88 BE 8B 9E B 49 1 2C DE 97 24 22 B7 20 4E EA C5 46 24 88 40 AF C7 C7 20 FF 3D 56 B7 D7 E9 F6 51 99 18 41 DD
State: Prepare_Shutdown
State: Send_Shutdown
$
```

## Building the library

Building the target library requires a two-step approach using a native and a
cross toolchain, respectively.

### RecordFlux installation and code generation

For the following step a Python virtual environment is recommended, but a user
installation will also work:

```
$ python3 -m venv venv
$ . venv/bin/activate
$ PATH=/path/to/your/NATIVE/gnat:$PATH pip install RecordFlux==0.8.0
[this will take a while]
```

### Cross-build the library

If a virtual environment had been used in the previous step, it must be active now.

```
$ PATH=/path/to/your/gnat:$PATH make lib
[...]

Build Libraries
   [gprlib]       spdm.lexch
   [archive]      libspdm.a
   [index]        libspdm.a
```

The resulting static library is build/lib/libspdm.a.

The process to compile the library for ARM and RiscV64 is analog:
```
$ PATH=/path/to/your/cross/gnat:$PATH make libarm
```
```
$ PATH=/path/to/your/cross/gnat:$PATH make libriscv64
```

The resulting libraries will be build/arm/lib/libspdm.a and build/riscv64/lib/libspdm.a.

## Features

Some features of the SPDM responder can be explicitly disabled. For that purpose the specification files contain preprocessor commands. The specification files in `specs` cannot be used without preprocessing. By default, all features are enabled. The following feature flags exist:

- `FEATURE_CHALLENGE_AUTH`
- `FEATURE_RESPOND_IF_READY`
- `FEATURE_KEY_EXCHANGE`

To disable a feature, the feature flag has to be set to `False`. For example, `make check_spec CHALLENGE_AUTH=False KEY_EXCHANGE=False RESPOND_IF_READY=False` will create specification files with all optional features disabled in `build/specs/CHALLENGE_AUTH=False,KEY_EXCHANGE=False,RESPOND_IF_READY=False/`.
