#!/bin/bash

set -e

build/tests/responder &
RESPONDER_PID=$!
trap "kill $RESPONDER_PID > /dev/null 2>&1" INT SIGINT SIGTERM EXIT
sleep 1 && build/tests/proxy &
PROXY_PID=$!
trap "kill $RESPONDER_PID $PROXY_PID > /dev/null 2>&1" INT SIGINT SIGTERM EXIT
sleep 2 && (cd build/certificates && exec ../spdm_emu/bin/spdm_requester_emu --trans NONE)
