"""Loopback-only libcurl integration fixture; works without the curl executable."""
import http.server
import argparse
import shutil
import tempfile
import os
from pathlib import Path
import subprocess
import ssl
import threading
import time


class Handler(http.server.BaseHTTPRequestHandler):
    def handle(self):
        try:
            super().handle()
        except (BrokenPipeError, ConnectionResetError):
            pass  # Cooperative cancellation may close during headers as well.

    def log_message(self, *args):
        pass

    def do_HEAD(self):
        self.respond(200, b"")

    def do_GET(self):
        if self.path == "/slow":
            time.sleep(0.2)
        if self.path == "/redirect":
            return self.respond(302, b"", {"Location": "/must-not-follow"})
        if self.path == "/must-not-follow":
            return self.respond(500, b"redirect unexpectedly followed")
        body = b"x" * 10000 if self.path == "/large" else "hello λ".encode()
        self.respond(404 if self.path == "/missing" else 200, body)

    def do_POST(self):
        body = self.rfile.read(int(self.headers.get("Content-Length", 0)))
        self.respond(200, body)

    do_PUT = do_PATCH = do_DELETE = do_POST

    def respond(self, status, body, headers=None):
        self.send_response(status)
        self.send_header("Content-Length", str(len(body)))
        for key, value in (headers or {}).items():
            self.send_header(key, value)
        self.end_headers()
        try:
            self.wfile.write(body)
        except (BrokenPipeError, ConnectionResetError):
            pass  # Intentional timeout/size-limit tests close the connection.


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--relocate", action="store_true", help="Test a relocated native bundle in a path with spaces")
    parser.add_argument("--no-path", action="store_true", help="Run SBCL with an empty PATH (no external curl available)")
    args = parser.parse_args()
    root = Path(__file__).resolve().parent.parent
    sbcl = shutil.which(os.environ.get("SBCL", "sbcl"))
    if not sbcl:
        raise RuntimeError("SBCL not found")
    relocated = tempfile.TemporaryDirectory(prefix="krypton http bundle ")
    server = http.server.ThreadingHTTPServer(("127.0.0.1", 0), Handler)
    thread = threading.Thread(target=server.serve_forever, daemon=True)
    thread.start()
    secure = http.server.ThreadingHTTPServer(("127.0.0.1", 0), Handler)
    context = ssl.SSLContext(ssl.PROTOCOL_TLS_SERVER)
    context.load_cert_chain(root / "tests/fixtures/http/localhost-cert.pem",
                            root / "tests/fixtures/http/localhost-key.pem")
    secure.socket = context.wrap_socket(secure.socket, server_side=True)
    secure_thread = threading.Thread(target=secure.serve_forever, daemon=True)
    secure_thread.start()
    try:
        env = dict(os.environ, XEON_HTTP_TEST_URL=f"http://127.0.0.1:{server.server_port}")
        if args.relocate:
            source = Path(env.get("XEON_HTTP_BUNDLE", root / "native/http"))
            target = Path(relocated.name) / "native library"
            shutil.copytree(source, target)
            env["XEON_HTTP_BUNDLE"] = str(target)
        if args.no_path:
            env["PATH"] = ""
        env["XEON_HTTPS_TEST_URL"] = f"https://localhost:{secure.server_port}"
        env["XEON_HTTPS_MISMATCH_URL"] = f"https://127.0.0.1:{secure.server_port}"
        env["XEON_HTTP_TEST_CA"] = str(root / "tests/fixtures/http/localhost-cert.pem")
        # Ignore proxy environment settings for this loopback-only fixture.
        env["NO_PROXY"] = env["no_proxy"] = "127.0.0.1,localhost"
        result = subprocess.run([sbcl, "--noinform", "--no-sysinit",
                                 "--no-userinit", "--non-interactive", "--load",
                                 "tests/run-http.lisp"], cwd=root, env=env)
    finally:
        server.shutdown()
        server.server_close()
        thread.join()
        secure.shutdown()
        secure.server_close()
        secure_thread.join()
        relocated.cleanup()
    raise SystemExit(result.returncode)
