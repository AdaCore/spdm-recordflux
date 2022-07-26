#!/usr/bin/env python3

import sys
import re

changelog = "CHANGELOG.md"

if __name__ == "__main__":

    if len(sys.argv) != 2:
        print(f'Invalid arguments. Expected "{sys.argv[0]} <version>"')
        sys.exit(1)

    current_version = sys.argv[1]

    with open(changelog, "r") as f:
        for line in f.readlines():
            match = re.match(
                r"^\#\# \[(?P<version>\d+\.\d+\.\d+)\] - \d+-\d+-\d+$", line
            )
            if match:
                latest_version = match.group("version")
                if latest_version != current_version:
                    print(
                        f"Invalid version: latest is {latest_version}, expected {current_version}"
                    )
                    sys.exit(1)
                sys.exit(0)
    print(f"No version information found in {changelog}")
    sys.exit(1)
