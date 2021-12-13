#!/usr/bin/env -S python3 -O

import argparse
import os
import re
from subprocess import check_output
import sys
from hashlib import sha1
from pathlib import Path
from typing import Dict, List, Optional, Tuple

re_packet_line = re.compile(r"^\d")
re_packet_name = re.compile(r" [A-Z_]+ ")
re_packet_data = re.compile("^    [0-9a-f]{4}:")
re_packet_is_message = re.compile("^  SPDM Message")
re_dhe_secret = re.compile(r"\[DHE Secret\]")
re_psk = re.compile(r"\[PSK\]")

unsupported_messages = {
    "SPDM_GET_ENCAPSULATED_REQUEST",
    "SPDM_PSK_EXCHANGE",
    "SPDM_DELIVER_ENCAPSULATED_RESPONSE",
    "SPDM_HEARTBEAT",
    "SPDM_ENCAPSULATED_REQUEST",
    "SPDM_PSK_FINISH",
    "SPDM_ENCAPSULATED_RESPONSE_ACK",
    "SPDM_PSK_EXCHANGE_RSP",
    "SPDM_HEARTBEAT_ACK",
    "SPDM_PSK_FINISH_RSP",
}


def extract_secrets(log_file: Path) -> Tuple[Optional[str], Optional[str]]:
    dhe_secret: str = None
    psk: str = None
    with log_file.open("r") as log:
        for line in log.readlines():
            if dhe_secret and psk:
                return dhe_secret, psk
            if re_psk.match(line):
                psk = line.split(":")[1].strip()
            elif re_dhe_secret.match(line):
                dhe_secret = line.split(":")[1].strip()
    return dhe_secret, psk


def dump(pcap_file: Path, out_dir: Path, dhe_secret: str, psk: str) -> int:
    packets: Dict[str, List[Tuple[str, bytes]]] = {}
    spdm_dump = ["spdm_dump", "-r", str(pcap_file), "-x"]
    if dhe_secret and psk:
        spdm_dump.extend(["--dhe_secret", dhe_secret, "--psk", psk])
    print(" ".join(spdm_dump))
    output = check_output(spdm_dump).decode("utf-8")
    current_name: str = None
    current_type: str = None
    current_data: List[str] = []
    is_message = False

    def update_packets() -> None:
        nonlocal current_name
        nonlocal current_data
        nonlocal current_type
        if current_name and current_data:
            data = bytes.fromhex("".join(current_data))
            current_data = []
            print(f"Message: {current_name}")
            if current_type in packets:
                packets[current_type].append((current_name, data))
            else:
                packets[current_type] = [(current_name, data)]
            current_name = None

    for line in output.split("\n"):
        try:
            if "(?)->(?)" in line or "SecuredSPDM message" in line:
                update_packets()
                current_name = None
                current_type = None
                is_message = False
            elif re_packet_line.match(line):
                update_packets()
                try:
                    current_name = re_packet_name.findall(line)[0].strip()
                    current_type = "Request" if "REQ->RSP" in line else "Response"
                except IndexError:  # MTCP message
                    current_name = None
                    current_type = None
                is_message = False
            elif re_packet_is_message.match(line) and current_name:
                is_message = True
            elif re_packet_data.match(line) and is_message:
                current_data.extend(line.split(":")[1].strip().split(" "))
        except Exception as e:
            print(line)
            raise e
    update_packets()
    for m_type, messages in packets.items():
        valid = out_dir / m_type / "valid"
        os.makedirs(str(valid), exist_ok=True)
        for m in messages:
            if m[0] in unsupported_messages:
                continue
            basename = valid / f"{sha1(m[1]).hexdigest()}_{m[0]}"
            with (basename.with_suffix(".raw")).open("wb") as message:
                message.write(m[1])
            with (basename.with_suffix(".yaml")).open("w") as config:
                if m_type == "Request":
                    config.write(
                        "Exchange_Data_Size: 96\n"
                        "Signature_Size: 384\n"
                        "Hash_Size: 48\n"
                    )
                elif m_type == "Response":
                    config.write(
                        "Meas_Cap: 2\n"
                        "Hash_Type: 1\n"
                        "Hash_Size: 48\n"
                        "Signature_Size: 96\n"
                        "Handshake_In_The_Clear: True\n"
                        "Exchange_Data_Size: 96\n"
                        f"Message_Size: {8 * len(m[1])}\n"
                    )

if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("-f", "--file", type=Path, help="pcap file", required=True)
    parser.add_argument("-o", "--out", type=Path, help="output dir", required=True)
    parser.add_argument("-l", "--log", type=Path, help="spdm_emu log file")
    args = parser.parse_args(sys.argv[1:])
    dhe_secret: str = None
    psk: str = None
    if args.log:
        dhe_secret, psk = extract_secrets(args.log)
    exit(dump(args.file, args.out, dhe_secret, psk))
