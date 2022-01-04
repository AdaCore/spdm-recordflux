#!/bin/bash

set -e

build/tests/responder &
sleep 1 && build/tests/proxy &
sleep 2 && (cd build/certificates && exec ../spdm_emu/bin/spdm_requester_emu --trans NONE)
