.PHONY: all test check package test_package clean

TMPDIR := $(shell mktemp -d)
FILE_LIST := $(shell mktemp)
GITREV := $(shell git rev-parse --short HEAD 2>/dev/null || echo local)

ifdef LOCAL_RFLX
RFLX = $(shell command -v rflx)
else
RFLX = $(TMPDIR)/venv/bin/python $(TMPDIR)/venv/bin/rflx
endif

all: check test

lib: build/lib/libspdm.a

build/lib/libspdm.a: build/generated/rflx.ads
	gprbuild -j0 -P spdm

test: test_validate test_responder

check: | $(RFLX)
	$(RFLX) check specs/spdm_responder.rflx

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

build/generated/rflx.ads: specs/spdm.rflx specs/spdm_responder.rflx specs/spdm_emu.rflx specs/spdm_proxy.rflx | $(RFLX)
	mkdir -p build/generated
	$(RFLX) --no-verification generate $^ --debug -d build/generated

build/tests/proxy build/tests/responder: build/generated/rflx.ads tests/tests.gpr tests/*.ad?
	gprbuild -p tests/tests.gpr -s

build/certificates:
	mkdir -p build/certificates
	cp contrib/dmtf/spdm-emu/libspdm/unit_test/sample_key/openssl.cnf build/certificates
	tools/generate_certificates.sh build/certificates

test_validate: test_validate_libspdm test_validate_static

test_validate_libspdm: build/spdm_emu/bin/spdm_requester_emu build/spdm_emu/bin/spdm_responder_emu build/spdm_dump/bin/spdm_dump build/certificates | $(RFLX)
	mkdir -p $(TMPDIR)/spdm build
	rm -f build/validate_libspdm_request.log build/validate_libspdm_response.log
	tools/run_emu.sh $(TMPDIR)/test_validate.pcap
	PATH="build/spdm_dump/bin:$(PATH)" tools/dump_validate.py -f $(TMPDIR)/test_validate.pcap -l $(TMPDIR)/test_validate.pcap.log -o $(TMPDIR)/spdm
	$(RFLX) --no-verification --max-errors=1 validate -o build/validate_libspdm_request.log -v $(TMPDIR)/spdm/Request/valid specs/spdm.rflx SPDM::Request
	$(RFLX) --no-verification --max-errors=1 validate -o build/validate_libspdm_response.log -v $(TMPDIR)/spdm/Response/valid specs/spdm.rflx SPDM::Response

test_validate_static: | $(RFLX)
	mkdir -p build
	rm -f build/validate_static_request.log build/validate_static_response.log
	$(RFLX) --no-verification --max-errors=1 validate -o build/validate_static_request.log -v tests/data/spdm/Request/valid specs/spdm.rflx SPDM::Request
	$(RFLX) --no-verification --max-errors=1 validate -o build/validate_static_response.log -v tests/data/spdm/Response/valid specs/spdm.rflx SPDM::Response

test_responder: build/tests/responder build/tests/proxy build/spdm_emu/bin/spdm_requester_emu
	tools/run_responder.sh

$(TMPDIR)/venv/bin/python $(TMPDIR)/venv/bin/rflx:
	python3 -m venv $(TMPDIR)/venv
	$(TMPDIR)/venv/bin/pip3 install contrib/RecordFlux[devel]

package: build/spdm_$(GITREV).tar.gz

build/spdm_$(GITREV).tar: .git/logs/HEAD
	# check for local changes, abort if not committed
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
	# NATIVE_GNAT_PATH must be set
	test -n "$(NATIVE_GNAT_PATH)"
	# CROSS_GNAT_PATH must be set
	test -n "$(CROSS_GNAT_PATH)"
	mkdir -p $(TMPDIR)/package_test
	tar -xvf $^ --directory $(TMPDIR)/package_test
	PATH="$(NATIVE_GNAT_PATH):$(PATH)" make -C $(TMPDIR)/package_test
	python3 -m venv $(TMPDIR)/package_test_venv
	$(TMPDIR)/package_test_venv/bin/pip3 install contrib/RecordFlux[devel]
	PATH="$(TMPDIR)/package_test_venv/bin:$(CROSS_GNAT_PATH):$(PATH)" make -C $(TMPDIR)/package_test lib
	# static library must exist
	test -f $(TMPDIR)/package_test/build/lib/libspdm.a

clean:
	rm -rf build
