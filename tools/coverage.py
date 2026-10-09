#!/usr/bin/env python3
# SPDX-License-Identifier: UPL-1.0 AND BSD-3-Clause
"""Convert this run's HPC data; retain only repository library source coverage.

hpc-codecov owns the HPC format. This wrapper scopes its result to Hide modules,
normalizes Windows paths, and records the exact toolchain alongside JUnit results.
It never turns a failed or interrupted test process into a successful run.
"""

import argparse
import json
import os
from pathlib import Path
import platform
import subprocess
import xml.etree.ElementTree as ET


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--builddir", default="build/coverage")
    parser.add_argument("--output", default="build/coverage-results")
    parser.add_argument("--converter", default="hpc-codecov")
    args = parser.parse_args()
    output = Path(args.output)
    output.mkdir(parents=True, exist_ok=True)
    converter = args.converter
    if os.name == "nt" and not converter.endswith(".exe"):
        converter += ".exe"
    raw = output / "hpc-all.info"
    build = Path(args.builddir).resolve()
    # Discovery matches a directory basename; restrict it to this exact build.
    subprocess.run([converter, "--root", str(build), "--build", build.name, "--src", str(Path.cwd()),
                    "--exclude", "Main,Paths_hide",
                    "--format", "lcov", "--out", str(raw), "cabal:editor-tests"], check=True)
    records = []
    source_files = set()
    executed = False
    for record in raw.read_text(encoding="utf-8").split("end_of_record"):
        lines = record.strip().splitlines()
        sources = [line[3:] for line in lines if line.startswith("SF:")]
        if not sources:
            continue
        if len(sources) != 1:
            raise ValueError("Expected one source file per LCOV record")
        path = Path(sources[0].replace("\\", "/"))
        if path.is_absolute():
            path = path.relative_to(Path.cwd())
        name = path.as_posix()
        if not name.startswith("src/Hide/"):
            continue
        if not path.is_file():
            raise ValueError(f"Coverage source does not exist: {path}")
        source_files.add(name)
        executed |= any(int(line.split(",")[1]) > 0 for line in lines if line.startswith("DA:"))
        records.append("\n".join("SF:" + name if line.startswith("SF:") else line for line in lines)
                       + "\nend_of_record\n")
    if not source_files or not executed:
        raise ValueError("HPC produced no executed Hide source coverage")
    (output / "coverage.info").write_text("".join(records), encoding="utf-8")

    # Missing/malformed JUnit is an error, never a synthetic passing report.
    cases = ET.parse(output / "tests.xml").findall(".//testcase")
    if not cases:
        raise ValueError("JUnit report contains no test cases")
    failed = sum(case.find("failure") is not None or case.find("error") is not None for case in cases)
    metadata = {
        "commit": subprocess.check_output(["git", "rev-parse", "HEAD"], text=True).strip(),
        "platform": platform.platform(),
        "architecture": platform.machine(),
        "ghc": subprocess.check_output(["ghc", "--numeric-version"], text=True).strip(),
        "cabal": subprocess.check_output(["cabal", "--numeric-version"], text=True).strip(),
        "configuration": "-f-window -f-terminal --enable-coverage -O1 (web enabled)",
        "scope": "Haskell Hide modules; no C, shaders, native-window or embedded-terminal backend",
        "source_files": len(source_files), "test_cases": len(cases), "failed_cases": failed,
    }
    (output / "host.json").write_text(json.dumps(metadata, indent=2) + "\n", encoding="utf-8")
    print(f"{len(source_files)} source files; {len(cases)} test cases, {failed} failed")


if __name__ == "__main__":
    main()
