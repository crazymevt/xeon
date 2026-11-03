#!/usr/bin/env python3
"""Stage a portable Xeon distribution and verify the staged native libraries."""
import argparse
import os
from pathlib import Path
import platform
import plistlib
import shutil
import subprocess


ROOT = Path(__file__).resolve().parents[1]
PLATFORM = platform.system()
WINDOWS = PLATFORM == "Windows"
MAC = PLATFORM == "Darwin"


def copy_tree(source, destination):
    shutil.copytree(source, destination, ignore=shutil.ignore_patterns(".git", "*.fasl"))


def linux_deb(stage, output, machine):
    deb = output / "deb"
    control = deb / "DEBIAN"
    control.mkdir(parents=True)
    (control / "control").write_text(
        "Package: xeon-ide\nVersion: 0.1.0\nArchitecture: amd64\n"
        "Maintainer: Jessie Hughart\n"
        "Depends: libc6, libstdc++6, libgtk-4-1 (>= 4.14), libsqlite3-0\n"
        "Description: Xeon Lisp IDE\n", encoding="utf-8")
    application = deb / "opt/xeon"
    application.parent.mkdir()
    shutil.copytree(stage, application)
    executables = deb / "usr/bin"
    executables.mkdir(parents=True)
    # Resolve the launcher relative to its actual package directory.
    entry = executables / "xeon"
    entry.write_text('#!/bin/sh\nexec /opt/xeon/xeon "$@"\n', encoding="utf-8")
    entry.chmod(0o755)
    share = deb / "usr/share"
    desktop = share / "applications"
    desktop.mkdir(parents=True)
    (desktop / "xeon.desktop").write_text(
        "[Desktop Entry]\nType=Application\nName=Xeon\nExec=xeon\n"
        "Icon=xeon\nCategories=Development;IDE;\n", encoding="utf-8")
    icons = share / "icons/hicolor/scalable/apps"
    icons.mkdir(parents=True)
    shutil.copy2(stage / "assets/branding/xeon-icon.svg", icons / "xeon.svg")
    subprocess.run(["dpkg-deb", "--root-owner-group", "--build", str(deb),
                    str(output / f"xeon-linux-{machine}.deb")], check=True)


def package(args):
    output = args.output.resolve()
    bundle = output / ("Xeon.app" if MAC else "Xeon")
    stage = bundle / "Contents/Resources" if MAC else bundle
    stage.mkdir(parents=True)
    for directory in ("assets", "data", "src", "vendor"):
        copy_tree(ROOT / directory, stage / directory)
    for filename in ("xeon.asd", "README.md", "PROVENANCE.md"):
        shutil.copy2(ROOT / filename, stage)
    (stage / "packaging").mkdir()
    shutil.copy2(ROOT / "packaging/launch.lisp", stage / "packaging")
    launcher = "Xeon.cmd" if WINDOWS else "xeon"
    shutil.copy2(ROOT / "packaging" / launcher, stage)
    (stage / launcher).chmod(0o755)
    copy_tree(args.quicklisp, stage / "quicklisp")

    runtime = stage / "runtime"
    shutil.copytree(args.sbcl_home, runtime / "lib")
    (runtime / "bin").mkdir()
    sbcl = runtime / "bin" / ("sbcl.exe" if WINDOWS else "sbcl")
    shutil.copy2(args.sbcl.resolve(), sbcl)
    shutil.copy2(args.sbcl_notice, runtime / "SBCL-COPYING")
    if MAC:
        dependencies = subprocess.check_output(["otool", "-L", str(sbcl)], text=True)
        for line in dependencies.splitlines()[1:]:
            library = line.strip().split(" (", 1)[0]
            if library.startswith("/usr/lib/") or library.startswith("/System/"):
                continue
            source = Path(library)
            if source.name != "libzstd.1.dylib":
                raise RuntimeError(f"Unexpected non-system SBCL dependency: {library}")
            shutil.copy2(source, runtime / "lib")
            shutil.copy2(source.parent.parent / "LICENSE", runtime / "Zstandard-LICENSE")
            subprocess.run(["install_name_tool", "-change", library,
                            "@executable_path/../lib/" + source.name, str(sbcl)], check=True)
        subprocess.run(["codesign", "--force", "--sign", "-", str(sbcl)], check=True)
    if WINDOWS:
        shutil.copy2(args.native_bundle / "sqlite3.dll", runtime / "bin" / "sqlite3.dll")

    terminal = "krypton_terminal.dll" if WINDOWS else (
        "libkrypton_terminal.dylib" if MAC else "libkrypton_terminal.so")
    bridge = next((ROOT / "build/terminal/lib").rglob(terminal))
    (stage / "native/terminal").mkdir(parents=True)
    shutil.copy2(bridge, stage / "native/terminal")
    copy_tree(args.native_bundle, stage / "native/http")
    env = dict(os.environ, SBCL_HOME=str(runtime / "lib"),
               PATH=str(runtime / "bin") + os.pathsep + os.environ["PATH"],
               XEON_HTTP_BUNDLE=str(stage / "native/http"),
               XEON_TERMINAL_LIBRARY=str(stage / "native/terminal" / terminal))
    subprocess.run([str(sbcl), "--script", str(stage / "packaging/launch.lisp"), "--check"],
                   cwd=stage, env=env, check=True)
    if MAC:
        contents = stage.parent
        (contents / "MacOS").mkdir()
        executable = contents / "MacOS/Xeon"
        shutil.copy2(ROOT / "packaging/macos-launcher", executable)
        executable.chmod(0o755)
        with (contents / "Info.plist").open("wb") as stream:
            plistlib.dump({"CFBundleExecutable": "Xeon", "CFBundleName": "Xeon",
                           "CFBundleIdentifier": "io.github.crazymevt.xeon",
                           "CFBundlePackageType": "APPL", "CFBundleIconFile": "Xeon.icns"}, stream)
        iconset = output / "Xeon.iconset"
        iconset.mkdir()
        icon = stage / "assets/branding/xeon-icon.svg"
        for size in (16, 32, 128, 256, 512):
            for scale in (1, 2):
                pixels = str(size * scale)
                suffix = "@2x" if scale == 2 else ""
                target = iconset / f"icon_{size}x{size}{suffix}.png"
                subprocess.run(["rsvg-convert", "-w", pixels, "-h", pixels,
                                "-o", str(target), str(icon)], check=True)
        subprocess.run(["iconutil", "-c", "icns", str(iconset), "-o", str(stage / "Xeon.icns")],
                       check=True)
        subprocess.run(["codesign", "--force", "--deep", "--sign", "-", str(bundle)], check=True)
    archive = "zip" if WINDOWS else "gztar"
    machine = {"AMD64": "x86_64", "aarch64": "arm64"}.get(platform.machine(), platform.machine())
    if PLATFORM == "Linux":
        linux_deb(stage, output, machine)
    shutil.make_archive(str(output / f"xeon-{PLATFORM.lower()}-{machine}"),
                        archive, output, bundle.name)


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    for option in ("output", "quicklisp", "sbcl", "sbcl-home", "sbcl-notice"):
        parser.add_argument("--" + option, required=True, type=Path)
    parser.add_argument("--native-bundle", type=Path, default=ROOT / "native/http")
    package(parser.parse_args())
