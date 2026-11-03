"""Build an app-private shared libcurl with static TLS/zlib dependencies.

Build tools are maintainer/CI dependencies, never end-user requirements.
Invoke through quiet-run (or tools/ci-run.py in CI) to retain the build log.
"""
import argparse
import ctypes
import hashlib
import json
import os
from pathlib import Path
import platform
import shutil
import subprocess
import tempfile
import urllib.request

ROOT = Path(__file__).resolve().parent.parent
LOCK = json.loads((ROOT / "packaging/http/lock.json").read_text())


def run(*args, cwd=None):
    subprocess.run([str(arg) for arg in args], cwd=cwd, check=True)


def checkout(url, revision, path):
    if not (path / ".git").exists():
        path.mkdir(parents=True, exist_ok=True)
        run("git", "init", path)
        run("git", "-C", path, "remote", "add", "origin", url)
    head = subprocess.run(["git", "-C", str(path), "rev-parse", "HEAD"],
                          capture_output=True, text=True)
    if head.returncode or head.stdout.strip() != revision:
        run("git", "-C", path, "fetch", "--depth", "1", url, revision)
        run("git", "-C", path, "checkout", "--detach", revision)
    dirty = subprocess.check_output(["git", "-C", str(path), "diff", "HEAD", "--name-only"], text=True)
    if dirty.strip():
        raise RuntimeError(f"Refusing modified dependency checkout: {path}")


def download(url, target):
    with urllib.request.urlopen(url, timeout=90) as source:
        target.write_bytes(source.read())


def build(output, include_sqlite=False):
    system = platform.system()
    arch = {"aarch64": "arm64", "arm64": "arm64", "AMD64": "x64", "x86_64": "x64"}.get(platform.machine())
    if not arch or system not in ("Darwin", "Linux", "Windows"):
        raise RuntimeError("Supported targets: macOS, Windows, Linux; x64 or arm64")
    work = ROOT / "build/http-native"
    vcpkg = work / "vcpkg"
    checkout("https://github.com/microsoft/vcpkg.git", LOCK["vcpkg"], vcpkg)
    executable = vcpkg / ("vcpkg.exe" if system == "Windows" else "vcpkg")
    if not executable.exists():
        if system == "Windows":
            run("cmd", "/c", vcpkg / "bootstrap-vcpkg.bat", "-disableMetrics")
        else:
            run("sh", vcpkg / "bootstrap-vcpkg.sh", "-disableMetrics")
    triplets = work / "triplets"
    triplets.mkdir(parents=True, exist_ok=True)
    triplet = f"krypton-{arch}-{system.lower()}"
    crt = "static" if system == "Windows" else "dynamic"
    cmake = f"set(VCPKG_TARGET_ARCHITECTURE {arch})\nset(VCPKG_CRT_LINKAGE {crt})\n"
    cmake += 'if(PORT STREQUAL "curl" OR PORT STREQUAL "sqlite3")\n  set(VCPKG_LIBRARY_LINKAGE dynamic)\nelse()\n  set(VCPKG_LIBRARY_LINKAGE static)\nendif()\n'
    if system != "Windows":
        cmake += f"set(VCPKG_CMAKE_SYSTEM_NAME {'Darwin' if system == 'Darwin' else 'Linux'})\n"
    (triplets / (triplet + ".cmake")).write_text(cmake)
    installed = work / "installed"
    run(executable, "install", f"curl[core,openssl]:{triplet}", "--classic",
        f"--overlay-triplets={triplets}", f"--x-install-root={installed}", "--disable-metrics", cwd=work)
    if system == "Windows" and include_sqlite:
        run(executable, "install", f"sqlite3:{triplet}", "--classic",
            f"--overlay-triplets={triplets}", f"--x-install-root={installed}", "--disable-metrics", cwd=work)
    prefix = installed / triplet
    filename = {"Darwin": "libcurl.4.dylib", "Linux": "libcurl.so.4", "Windows": "libcurl.dll"}[system]
    library = prefix / ("bin" if system == "Windows" else "lib") / filename
    if not library.exists():
        raise RuntimeError(f"Missing native library: {library}")
    destination = output
    destination.parent.mkdir(parents=True, exist_ok=True)
    output = Path(tempfile.mkdtemp(prefix=".http-bundle-", dir=destination.parent))
    shutil.copy2(library.resolve(), output / filename)
    # Preserve the exact notices of every built dependency, including static ones.
    if system == "Windows" and include_sqlite:
        shutil.copy2(prefix / "bin/sqlite3.dll", output / "sqlite3.dll")
    licenses = output / "licenses"
    licenses.mkdir(exist_ok=True)
    for notice in (prefix / "share").glob("*/copyright"):
        shutil.copy2(notice, licenses / (notice.parent.name + ".txt"))
    for required in ("curl", "openssl", "zlib"):
        if not (licenses / (required + ".txt")).exists():
            raise RuntimeError(f"Missing redistribution notice: {required}")
    certifi = f"https://raw.githubusercontent.com/certifi/python-certifi/{LOCK['certifi']}"
    download(certifi + "/certifi/cacert.pem", output / "cacert.pem")
    download(certifi + "/LICENSE", licenses / "certifi.txt")
    # Include the MPL license text, not just its link, with the CA source.
    download("https://www.mozilla.org/media/MPL/2.0/index.txt", licenses / "MPL-2.0.txt")
    shutil.copy2(installed / "vcpkg/status", output / "dependency-versions.txt")
    shutil.copy2(ROOT / "packaging/http/lock.json", output / "source-lock.json")
    (output / "BUILD.txt").write_text(f"OS: {system}\nArchitecture: {arch}\nTriplet: {triplet}\n"
                                       "libcurl shared; OpenSSL and zlib static; no curl executable.\n")
    # Catch missing dynamic dependencies before declaring an artifact complete.
    native = ctypes.CDLL(str(output / filename))
    native.curl_version.restype = ctypes.c_char_p
    version = native.curl_version().decode()
    (output / "VERSION.txt").write_text(version + "\n")
    hashes = {str(p.relative_to(output)): hashlib.sha256(p.read_bytes()).hexdigest()
              for p in output.rglob("*") if p.is_file() and p.name != "SHA256SUMS.json"}
    (output / "SHA256SUMS.json").write_text(json.dumps(hashes, indent=2) + "\n")
    # Publish only a complete, loadable bundle; retain the old one on failure.
    backup = destination.with_name(destination.name + ".previous")
    if backup.exists():
        raise RuntimeError(f"Resolve previous interrupted bundle publish before retrying: {backup}")
    if destination.exists():
        destination.rename(backup)
    try:
        output.rename(destination)
    except Exception:
        if backup.exists():
            backup.rename(destination)
        raise
    if backup.exists():
        shutil.rmtree(backup)
    print(f"Bundled {version} in {destination}")


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output", type=Path, default=ROOT / "native/http")
    parser.add_argument("--include-sqlite", action="store_true", help="Stage SQLite for Windows releases")
    args = parser.parse_args()
    build(args.output.resolve(), args.include_sqlite)
