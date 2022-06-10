# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Fixed

- Provability of examples
- Incorrect sending of number of measurement indices when requesting all measurements (V525-053)
- Missing handling of opaque data in `Get_Measurements`
- Fix invalid capabilities if all features are disabled

### Changed

- Improved handling of signature length

### Added

- Support for `Key_Exchange` request and response
- Support for `Finish` request and response
- Support for `Key_Update` request and response

### Changed

- Enable assertions and compile-time checks in tests

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
