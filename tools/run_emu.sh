#!/bin/bash

set -e

PATH=$PWD/build/spdm_emu/bin:$PATH

cd contrib/dmtf/spdm-emu/libspdm/unit_test/sample_key
spdm_responder_emu --pcap $1 & sleep 1 && spdm_requester_emu
cd -
