# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

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
