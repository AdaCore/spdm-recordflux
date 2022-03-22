.PHONY: all test check check_spec check_stack check_stack_riscv64 check_stack_arm package test_package test_cross test_integration prove clean

TMPDIR := $(shell mktemp -d)
FILE_LIST := $(shell mktemp)
GITREV := $(shell git rev-parse --short HEAD 2>/dev/null || echo local)
GENERATED := rflx.ads \
    rflx-rflx_arithmetic.adb \
    rflx-rflx_arithmetic.ads \
    rflx-rflx_builtin_types.ads \
    rflx-rflx_builtin_types-conversions.ads \
    rflx-rflx_generic_types.adb \
    rflx-rflx_generic_types.ads \
    rflx-rflx_message_sequence.adb \
    rflx-rflx_message_sequence.ads \
    rflx-rflx_scalar_sequence.adb \
    rflx-rflx_scalar_sequence.ads \
    rflx-rflx_types.ads \
    rflx-spdm.ads \
    rflx-spdm-algorithms_response.adb \
    rflx-spdm-algorithms_response.ads \
    rflx-spdm-alg_struct.adb \
    rflx-spdm-alg_struct.ads \
    rflx-spdm-capabilities_response.adb \
    rflx-spdm-capabilities_response.ads \
    rflx-spdm-certificate_response.adb \
    rflx-spdm-certificate_response.ads \
    rflx-spdm-challenge_auth_response.adb \
    rflx-spdm-challenge_auth_response.ads \
    rflx-spdm-challenge_request.adb \
    rflx-spdm-challenge_request.ads \
    rflx-spdm-digests_response.adb \
    rflx-spdm-digests_response.ads \
    rflx-spdm-end_session_request.adb \
    rflx-spdm-end_session_request.ads \
    rflx-spdm-end_session_response.adb \
    rflx-spdm-end_session_response.ads \
    rflx-spdm-error_response.adb \
    rflx-spdm-error_response.ads \
    rflx-spdm-ext_alg.adb \
    rflx-spdm-ext_alg.ads \
    rflx-spdm-ext_algs.ads \
    rflx-spdm-finish_request.adb \
    rflx-spdm-finish_request.ads \
    rflx-spdm-finish_response.adb \
    rflx-spdm-finish_response.ads \
    rflx-spdm-get_capabilities_request.adb \
    rflx-spdm-get_capabilities_request.ads \
    rflx-spdm-get_certificate_request.adb \
    rflx-spdm-get_certificate_request.ads \
    rflx-spdm-get_digests_request.adb \
    rflx-spdm-get_digests_request.ads \
    rflx-spdm-get_measurements_request.adb \
    rflx-spdm-get_measurements_request.ads \
    rflx-spdm-get_version_request.adb \
    rflx-spdm-get_version_request.ads \
    rflx-spdm-key_exchange_request.adb \
    rflx-spdm-key_exchange_request.ads \
    rflx-spdm-key_exchange_response.adb \
    rflx-spdm-key_exchange_response.ads \
    rflx-spdm-key_update_ack_response.adb \
    rflx-spdm-key_update_ack_response.ads \
    rflx-spdm-key_update_request.adb \
    rflx-spdm-key_update_request.ads \
    rflx-spdm-measurements_response.adb \
    rflx-spdm-measurements_response.ads \
    rflx-spdm-measurement_block.adb \
    rflx-spdm-measurement_block.ads \
    rflx-spdm-measurement_record.ads \
    rflx-spdm-negotiate_algorithms_request.adb \
    rflx-spdm-negotiate_algorithms_request.ads \
    rflx-spdm-request.adb \
    rflx-spdm-request.ads \
    rflx-spdm-req_alg_structs.ads \
    rflx-spdm-respond_if_ready_request.adb \
    rflx-spdm-respond_if_ready_request.ads \
    rflx-spdm-response.adb \
    rflx-spdm-response.ads \
    rflx-spdm-response_not_ready_data.adb \
    rflx-spdm-response_not_ready_data.ads \
    rflx-spdm-resp_alg_structs.ads \
    rflx-spdm-slot_mask.adb \
    rflx-spdm-slot_mask.ads \
    rflx-spdm-version_number_entries.ads \
    rflx-spdm-version_number_entry.adb \
    rflx-spdm-version_number_entry.ads \
    rflx-spdm-version_response.adb \
    rflx-spdm-version_response.ads \
    rflx-spdm_emu.ads \
    rflx-spdm_emu-platform_port.adb \
    rflx-spdm_emu-platform_port.ads \
    rflx-spdm_proxy.ads \
    rflx-spdm_proxy-packet.adb \
    rflx-spdm_proxy-packet.ads \
    rflx-spdm_proxy-proxy.adb \
    rflx-spdm_proxy-proxy.ads \
    rflx-spdm_proxy-proxy_allocator.adb \
    rflx-spdm_proxy-proxy_allocator.ads \
    rflx-spdm_responder.ads \
    rflx-spdm_responder-digests_data.adb \
    rflx-spdm_responder-digests_data.ads \
    rflx-spdm_responder-session.adb \
    rflx-spdm_responder-session.ads \
    rflx-spdm_responder-session_allocator.adb \
    rflx-spdm_responder-session_allocator.ads \
    rflx-spdm_requester.ads \
    rflx-spdm_requester-requester_allocator.adb \
    rflx-spdm_requester-requester_allocator.ads \
    rflx-spdm_requester-requester_allocator.adb \
    rflx-spdm_requester-requester_allocator.ads \
    rflx-spdm_requester-request.adb \
    rflx-spdm_requester-request.ads \
    rflx-spdm-nonce.adb \
    rflx-spdm-nonce.ads \
    rflx-spdm-dmtf_measurement_field.adb \
    rflx-spdm-dmtf_measurement_field.ads

ifdef LOCAL_RFLX
RFLX = $(shell command -v python3) $(shell command -v rflx)
else
RFLX = $(TMPDIR)/venv/bin/python $(TMPDIR)/venv/bin/rflx
endif

all: check test prove

lib: build/lib/libspdm.a

libarm: build/arm/lib/libspdm.a

libriscv64: build/riscv64/lib/libspdm.a

test: test_validate test_responder test_cross lib test_integration

build/lib/libspdm.a: $(addprefix build/generated/,$(GENERATED)) build/generated/spdm_platform_interface.adb
	gprbuild -j0 -P spdm

build/%/lib/libspdm.a: $(addprefix build/generated/,$(GENERATED)) build/generated/spdm_platform_interface.adb
	gprbuild -j0 -P spdm -XTARGET=$*

build/%/example/main: $(addprefix build/generated/,$(GENERATED)) build/generated/spdm_platform_interface.adb
	gprbuild -j0 -P examples/build.gpr -XTARGET=$*
	test -f $@

test_cross: build/arm/example/main build/riscv64/example/main libarm libriscv64

check: check_spec check_stack

check_spec: $(wildcard specs/*.rflx) | $(RFLX)
	$(RFLX) check $^

check_stack: check_stack_riscv64 check_stack_arm

check_stack_riscv64: build/riscv64/gnatstack/example/gnatstack.log

check_stack_arm: build/arm/gnatstack/example/gnatstack.log

build/%/gnatstack/example/gnatstack.log: build/%/gnatstack/example/main
	gnatstack -Wa -l10 -v -f $@ -P examples/build.gpr -XCHECK_STACK=True -XTARGET=$* | tee $@.tmp
	mv $@.tmp $@

build/%/gnatstack/example/main: $(addprefix build/generated/,$(GENERATED)) build/generated/spdm_platform_interface.adb
	gprbuild -j0 -P examples/build.gpr -XCHECK_STACK=True -XTARGET=$*

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

build/debug/generated/%: specs/spdm.rflx specs/spdm_responder.rflx specs/spdm_emu.rflx specs/spdm_proxy.rflx specs/spdm_requester.rflx | $(RFLX)
	mkdir -p build/debug/generated
	$(RFLX) generate $^ --debug -d build/debug/generated

build/generated:
	mkdir -p build/generated

build/generated/%: build/debug/generated/% build/generated
	grep -v "Ada.Text_IO" $< > $@

build/tests/proxy build/tests/responder build/tests/requester: $(addprefix build/debug/generated/,$(GENERATED)) build/spdm_emu/bin/spdm_responder_emu tests/tests.gpr tests/*.ad?
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

test_responder: build/tests/responder build/tests/proxy build/spdm_emu/bin/spdm_requester_emu build/certificates
	tools/run_responder.expect

test_integration: build/tests/responder build/tests/proxy build/tests/requester build/certificates
	tests/integration/V407-039.expect

$(TMPDIR)/venv/bin/python $(TMPDIR)/venv/bin/rflx:
	python3 -m venv $(TMPDIR)/venv
	$(TMPDIR)/venv/bin/pip3 install wheel
	$(TMPDIR)/venv/bin/pip3 install contrib/RecordFlux[devel]

package: build/spdm_$(GITREV).tar.xz

build/spdm_$(GITREV).tar: .git/logs/HEAD
	# check for local changes, abort if not committed
	git diff --summary --exit-code
	git diff --summary --exit-code --cached
	mkdir -p build
	git ls-files --recurse-submodules | grep -v -e "^.git\|/\.git" | grep -v -e "^contrib/RecordFlux/examples/specs/tests/data" > $(FILE_LIST)
	tar cvf build/spdm_$(GITREV).tar -T $(FILE_LIST)
	git rev-parse HEAD > $(TMPDIR)/commit
	tar rvf build/spdm_$(GITREV).tar --directory $(TMPDIR) commit

build/spdm_$(GITREV).tar.xz: build/spdm_$(GITREV).tar
	xz -z -e -9 -T0 $^

test_package: build/spdm_$(GITREV).tar
	# Check for file with charcaters incompatible with Perforce
	# https://www.perforce.com/manuals/p4guide/Content/P4Guide/syntax.syntax.restrictions.html
	test -z "$$(tar -tf $^ | grep -e '\([@#]\|\.\.\.\|%[0-9]\)')"
	mkdir -p $(TMPDIR)/package_test
	tar -xvf $^ --directory $(TMPDIR)/package_test
	make -C $(TMPDIR)/package_test
	python3 -m venv $(TMPDIR)/package_test_venv
	$(TMPDIR)/package_test_venv/bin/pip3 install wheel
	$(TMPDIR)/package_test_venv/bin/pip3 install contrib/RecordFlux[devel]
	PATH="$(TMPDIR)/package_test_venv/bin:$(PATH)" make -C $(TMPDIR)/package_test lib
	# static library must exist
	test -f $(TMPDIR)/package_test/build/lib/libspdm.a

build/generated/spdm_platform_interface.adb: include/spdm_platform_interface.ads
	gnatstub --output-dir=$(dir $@) --no-exception --force $<

prove: $(addprefix build/generated/,$(GENERATED))
	gnatprove -P examples/build_lib.gpr -j0 -XTARGET=riscv64 -u responder -u responder_multiple_responders -u responder_select
	gnatprove -P examples/build.gpr -j0 -XTARGET=riscv64 -u main -u main_multiple_responders -u main_select

clean:
	rm -rf build
