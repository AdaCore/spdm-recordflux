#!/bin/bash

set -e
set -m

build/responder/responder &
sleep 1 && build/responder/proxy &
sleep 2 && build/spdm_emu/bin/spdm_requester_emu --trans NONE &

fg %$(jobs | grep build/responder/responder | cut -d " " -f1 | tr -d "[]")

jobs -p | xargs -r kill
