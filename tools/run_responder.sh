#!/bin/bash

set -e

build/tests/responder &
RESPONDER_PID=$!
sleep 1 && build/tests/proxy &
PROXY_PID=$!
sleep 2 && (cd build/certificates && exec ../spdm_emu/bin/spdm_requester_emu --trans NONE)

set +e
kill $RESPONDER_PID > /dev/null 2>&1
kill $PROXY_PID > /dev/null 2>&1
exit 0
