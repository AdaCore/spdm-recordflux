#!/bin/bash

set -e

PATH=$PWD/build/spdm_emu/bin:$PATH

cd build/certificates
spdm_responder_emu --pcap $1 > $1.log & sleep 1 && spdm_requester_emu
cd -
