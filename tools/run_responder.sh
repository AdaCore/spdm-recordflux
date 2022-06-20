#!/bin/bash

set -e

cd build/certificates
../tests/responder &
RESPONDER_PID=$!
trap "kill $RESPONDER_PID > /dev/null 2>&1" INT SIGINT SIGTERM EXIT
sleep 1 && ../tests/proxy &
PROXY_PID=$!
trap "kill $RESPONDER_PID $PROXY_PID > /dev/null 2>&1" INT SIGINT SIGTERM EXIT
sleep 2 && ../spdm_emu/bin/spdm_requester_emu --trans NONE --exe_conn DIGEST,CERT,MEAS --exe_session KEY_EX,KEY_UPDATE,MEAS
