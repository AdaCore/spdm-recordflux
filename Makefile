.PHONY: all clean

all: build build/spdm_dump/bin/spdm_dump build/spdm_emu/bin/spdm_requester_emu build/spdm_emu/bin/spdm_responder_emu

build/spdm_dump:
	mkdir -p build/spdm_dump

build/spdm_emu:
	mkdir -p build/spdm_emu

build/spdm_dump/bin/spdm_dump: build/spdm_dump
	cmake -DARCH=x64 -DTOOLCHAIN=GCC -DTARGET=Release -DCRYPTO=mbedtls -S contrib/dmtf/spdm-dump -B build/spdm_dump
	make -C build/spdm_dump -j$(shell nproc)

build/spdm_emu/bin/spdm_%_emu: build/spdm_emu
	cmake -DARCH=x64 -DTOOLCHAIN=GCC -DTARGET=Release -DCRYPTO=mbedtls -S contrib/dmtf/spdm-emu -B build/spdm_emu
	make -C build/spdm_emu -j$(shell nproc)

test_validate: TMPDIR := $(shell mktemp -d)
test_validate: build/spdm_emu/bin/spdm_requester_emu build/spdm_emu/bin/spdm_responder_emu build/spdm_dump/bin/spdm_dump
	mkdir -p $(TMPDIR)/spdm
	build/spdm_emu/bin/spdm_responder_emu --pcap $(TMPDIR)/test_validate.pcap & sleep 1 && build/spdm_emu/bin/spdm_requester_emu
	PATH=build/spdm_dump/bin:$(PATH) tools/dump_validate.py -f $(TMPDIR)/test_validate.pcap -o $(TMPDIR)/spdm
	contrib/RecordFlux-specifications/tools/validate_spec.py -s contrib/RecordFlux-specifications/spdm.rflx -m SPDM::Request -v $(TMPDIR)/spdm/Request/valid --no-verification
	contrib/RecordFlux-specifications/tools/validate_spec.py -s contrib/RecordFlux-specifications/spdm.rflx -m SPDM::Response -v $(TMPDIR)/spdm/Response/valid --no-verification

clean:
	rm -rf build
