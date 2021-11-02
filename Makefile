.PHONY: all test check package test_package clean

TMPDIR := $(shell mktemp -d)
FILE_LIST := $(shell mktemp)
GITREV := $(shell git rev-parse --short HEAD 2>/dev/null || echo local)
RFLX = $(TMPDIR)/venv/bin/python $(TMPDIR)/venv/bin/rflx

all: check test test_package

test: test_validate test_responder

check: | $(RFLX)
	$(RFLX) check specs/spdm.rflx

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

build/generated/rflx.ads: specs/spdm.rflx specs/spdm_emu.rflx specs/spdm_proxy.rflx | $(RFLX)
	mkdir -p build/generated
	$(RFLX) --no-verification generate $^ --debug -d build/generated

build/responder/responder: build/generated/rflx.ads responder.gpr src/*.ads src/*.adb
	gprbuild -p responder.gpr -s

test_validate: test_validate_libspdm test_validate_static

test_validate_libspdm: $(RFLX)
test_validate_libspdm: build/spdm_emu/bin/spdm_requester_emu build/spdm_emu/bin/spdm_responder_emu build/spdm_dump/bin/spdm_dump
	mkdir -p $(TMPDIR)/spdm
	tools/run_emu.sh $(TMPDIR)/test_validate.pcap
	PATH=build/spdm_dump/bin:$(PATH) tools/dump_validate.py -f $(TMPDIR)/test_validate.pcap -l $(TMPDIR)/test_validate.pcap.log -o $(TMPDIR)/spdm
	$(RFLX) --no-verification --max-errors=1 validate -v $(TMPDIR)/spdm/Request/valid specs/spdm.rflx SPDM::Request
	$(RFLX) --no-verification --max-errors=1 validate -v $(TMPDIR)/spdm/Response/valid specs/spdm.rflx SPDM::Response

test_validate_static: $(RFLX)
	$(RFLX) --no-verification --max-errors=1 validate -v tests/data/spdm/Request/valid specs/spdm.rflx SPDM::Request
	$(RFLX) --no-verification --max-errors=1 validate -v tests/data/spdm/Response/valid specs/spdm.rflx SPDM::Response

test_responder: build/responder/responder build/spdm_emu/bin/spdm_requester_emu
	tools/run_responder.sh

$(RFLX):
	virtualenv -p python3 $(TMPDIR)/venv
	$(TMPDIR)/venv/bin/pip3 install contrib/RecordFlux[devel]

package: build/spdm_$(GITREV).tar.gz

build/spdm_$(GITREV).tar: .git/logs/HEAD
	git diff --summary --exit-code
	git diff --summary --exit-code --cached
	mkdir -p build
	git ls-files --recurse-submodules | grep -v -e "^.git\|/\.git" > $(FILE_LIST)
	tar cvf build/spdm_$(GITREV).tar -T $(FILE_LIST)
	git rev-parse HEAD > $(TMPDIR)/commit
	tar rvf build/spdm_$(GITREV).tar --directory $(TMPDIR) commit

build/spdm_$(GITREV).tar.xz: build/spdm_$(GITREV).tar
	xz -z -e -9 -T0 $^

build/spdm_$(GITREV).tar.gz: build/spdm_$(GITREV).tar
	gzip -f $^

test_package: build/spdm_$(GITREV).tar
	mkdir -p $(TMPDIR)/package_test
	tar -xvf $^ --directory $(TMPDIR)/package_test
	make -C $(TMPDIR)/package_test check test

clean:
	rm -rf build
