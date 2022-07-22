# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.3.0] - 2022-07-22

### Changed

- Use single field setting to improve binary size (V621-019)
- Remove unnecessary checks to improve binary size (V621-019)
- Pass length of opaque object into platform functions
- Rework transcript hash and signature generation (V622-001)
- Distribute RecordFlux as source package
- Improve compilation switches to reduce binary size (V621-019)

### Fixed

- Improper deletion of code for disabled features (V616-037)
- Incomplete initialization of Slot ID for measurement signature (V616-034)
- Improperly preprocessed spec if only `KEY_EXCHANGE` is enabled (V627-027)
- Insufficient integration example for larger measurements (V713-064)
- Ensure sufficient buffer size for result of `Plat_Get_Certificate_Response`

## [0.2.0] - 2022-06-16

### Added

- Support for `Key_Exchange` request and response
- Support for `Finish` request and response
- Support for `Key_Update` request and response
- Support for `End_Session` request and response

### Changed

- Improved handling of signature length
- Enable assertions and compile-time checks in tests

### Fixed

- Provability of examples
- Incorrect sending of number of measurement indices when requesting all measurements (V525-053)
- Missing handling of opaque data in `Get_Measurements`
- Invalid capabilities if all features are disabled
- Handling of Slot IDs (V511-043)
- Handling of `Get_Version` request in state machine (V422-041)
- Missing code in examples (V316-067)

## [0.1.1] - 2022-05-25

### Added

- Changelog

### Changed

- Simplify measurement number of indices response
- Increase proof steps
- Use externally defined debug output function

### Fixed

- `Alg_Struct_Count` in `Negotiate_Algorithms` response (V503-050)
- Hash reset in `spdm_platform_update_meas_signature`
- Improper missing initialization of `Resp_Alg_Structs` (V511-039)
- File handling in requester
- Insufficient space in sequence due to missing reset (V523-003)

## [0.1.0] - 2022-05-11
