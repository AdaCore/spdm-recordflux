#!/usr/bin/env -S python3 -O

import argparse
import os
import re
from subprocess import check_output
import sys
from pathlib import Path
from typing import Dict, List

re_packet_line = re.compile(r"^\d")
re_packet_name = re.compile(r" [A-Z_]+ ")
re_packet_data = re.compile("^    \d{4}:")


def convert_to_spec(name: str) -> str:
    name = name[5:].lower()
    if name.startswith("get"):
        return f"{name}_request"
    return f"{name}_response"


def dump(pcap_file: Path, out_dir: Path) -> int:
    packets: Dict[str, List[bytes]] = {}
    output = check_output(["spdm_dump", "-r", str(pcap_file), "-x"]).decode("utf-8")
    current_name: str = None
    current_data: List[str] = []

    def update_packets() -> None:
        nonlocal current_name
        nonlocal current_data
        if current_name and current_data:
            data = bytes.fromhex("".join(current_data))
            current_data = []
            print(f"Message: {current_name}")
            if current_name in packets:
                packets[current_name].append(data)
            else:
                packets[current_name] = [data]
            current_name = None

    for line in output.split("\n"):
        if re_packet_line.match(line):
            update_packets()
            current_name = re_packet_name.findall(line)[0].strip()
        if re_packet_data.match(line) and current_name:
            current_data.extend(line.split(":")[1].strip().split(" "))
    update_packets()
    for name, messages in packets.items():
        valid = out_dir / convert_to_spec(name) / "valid"
        os.makedirs(str(valid), exist_ok=True)
        for i in range(len(messages)):
            with (valid / f"message_{i}.bin").open("wb") as message:
                message.write(messages[i])


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("-f", "--file", type=Path, help="pcap file", required=True)
    parser.add_argument("-o", "--out", type=Path, help="output dir", required=True)
    args = parser.parse_args(sys.argv[1:])
    exit(dump(args.file, args.out))
