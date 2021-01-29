#!/usr/bin/python3
import re
import shutil
import pathlib
import argparse
import subprocess


RE_LDD_LINE = re.compile(
    r"^\t(?:(?P<name>[\w/.-]+) => )?(?P<path>[\w/.-]+) \((?P<addr>\w+)\)$"
)


def parse_libs(lines):
    for line in lines:
        match = RE_LDD_LINE.match(line)
        if match is not None:
            lib = match.groupdict()
            if lib["path"] == "linux-vdso.so.1":
                # ignore virtual vdso lib
                continue
            yield lib


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument(
        'binary',
        type=pathlib.Path,
        help="path to binary"
    )
    parser.add_argument(
        'destination',
        type=pathlib.Path,
        help="path to output dir"
    )
    args = parser.parse_args()

    proc = subprocess.run(
        [
            "/usr/bin/ldd",
            args.binary
        ],
        capture_output=True,
        check=True,
        encoding="utf-8"
    )

    args.destination.mkdir(parents=True, exist_ok=True)
    shutil.copy2(args.binary, args.destination)
    for lib in parse_libs(proc.stdout.splitlines()):
        lib_path = pathlib.Path(lib["path"])
        lib_path_relative = lib_path.relative_to(lib_path.root)
        dest = args.destination.joinpath(lib_path_relative)
        dest.parent.mkdir(parents=True, exist_ok=True)
        shutil.copy2(lib["path"], dest)


if __name__ == "__main__":
    main()
