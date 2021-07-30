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

.PHONY: clean

clean:
	rm -rf build
