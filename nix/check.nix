# SPDX-FileCopyrightText: 2026 Edward Kmett
# SPDX-License-Identifier: UPL-1.0 AND BSD-3-Clause
{ pkgs, hide }:

pkgs.testers.runNixOSTest {
  name = "hide";
  nodes.machine = {
    environment.systemPackages = [ hide pkgs.python3 pkgs.curl ];
    environment.etc."hide-remote-check.py".source = ../test/remote-session.py;
    virtualisation.memorySize = 2048;
  };

  testScript = ''
    from pathlib import PurePosixPath
    import shlex

    start_all()
    machine.wait_for_unit("multi-user.target")
    machine.succeed("grep -q '^ID=nixos$' /etc/os-release")
    for command in ("hide", "th", "thc-edit"):
        machine.succeed(f"{command} --snapshot --demo > /tmp/{command}.txt")
        machine.succeed(f"test -s /tmp/{command}.txt")

    # Exercise the installed assets, replay, Unicode edits and clean shutdown.
    html = machine.succeed("find ${hide.data} -path '*/assets/web/index.html'").strip()
    assert html and "\n" not in html
    data = str(PurePosixPath(html).parents[2])
    machine.succeed(
        "env hide_datadir=" + shlex.quote(data)
        + " python3 /etc/hide-remote-check.py ${hide}/bin/hide"
    )

    # Leave hide_datadir unset here so the executable must find its own data.
    machine.succeed(
        "THC_EDIT_WEB_OPEN=0 hide --web --demo > /tmp/web.out 2> /tmp/web.log & echo $! > /tmp/web.pid"
    )
    machine.wait_until_succeeds("grep -q 'Haskell browser: http://' /tmp/web.log")
    url = machine.succeed("sed -n 's/^Haskell browser: //p' /tmp/web.log").strip()
    for asset in ("", "editor.js", "cell-shader.js", "canvas-shader.js", "canvas-images.js"):
        machine.succeed("curl --fail --silent " + shlex.quote(url + asset) + " > /tmp/asset")
        machine.succeed("test -s /tmp/asset")
    machine.succeed("kill -INT $(cat /tmp/web.pid)")
    machine.wait_until_fails("kill -0 $(cat /tmp/web.pid)")
  '';
}
