#!/bin/bash

set -e

build/responder/responder &
sleep 1 && build/responder/proxy &
sleep 2 && build/spdm_emu/bin/spdm_requester_emu --trans NONE
