#!/bin/bash

set -e

build/tests/responder &
sleep 1 && build/tests/proxy &
sleep 2 && build/spdm_emu/bin/spdm_requester_emu --trans NONE
