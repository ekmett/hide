# SPDX-FileCopyrightText: 2026 Edward Kmett
# SPDX-License-Identifier: BSD-2-Clause OR Apache-2.0
{ pkgs, hide, accesskitC }:
let
  menuCheck = import ./native-menu-check.nix { inherit pkgs accesskitC; };
  atspiPython = pkgs.python3.withPackages (ps: [ ps.pygobject3 ]);
in

pkgs.testers.runNixOSTest {
  name = "hide";
  nodes.machine = {
    environment.systemPackages = [ hide atspiPython pkgs.curl pkgs.dbus pkgs.glib pkgs.xorg.xorgserver pkgs.at-spi2-core pkgs.xdotool ];
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

    # Actual SDL frontend glue and independent AT-SPI client on a disposable
    # display/bus, including retained references and no-bus startup.
    machine.succeed(
        "env GI_TYPELIB_PATH=${pkgs.lib.makeSearchPath "lib/girepository-1.0" [ (pkgs.lib.getLib pkgs.at-spi2-core) (pkgs.lib.getLib pkgs.glib) ]}"
        + " LD_LIBRARY_PATH=${pkgs.lib.makeLibraryPath [ pkgs.at-spi2-core pkgs.glib pkgs.vulkan-loader ]}"
        + " XDG_DATA_DIRS=${pkgs.at-spi2-core}/share"
        + " ${menuCheck}/libexec/hide-atspi-menu/atspi-menu.sh ${menuCheck}/bin/hide-atspi-menu-host"
    )

    # The packaged Haskell frontend must publish its real menu projection too;
    # the C fixture above does not stand in for this owner/remote path.
    machine.succeed(
        "env GI_TYPELIB_PATH=${pkgs.lib.makeSearchPath "lib/girepository-1.0" [ (pkgs.lib.getLib pkgs.at-spi2-core) (pkgs.lib.getLib pkgs.glib) ]}"
        + " LD_LIBRARY_PATH=${pkgs.lib.makeLibraryPath [ pkgs.at-spi2-core pkgs.glib pkgs.vulkan-loader ]}"
        + " XDG_DATA_DIRS=${pkgs.at-spi2-core}/share HIDE_ATSPI_EDITOR=1"
        + " HIDE_ATSPI_VULKAN_ICD_DIR=${pkgs.mesa.drivers}/share/vulkan/icd.d"
        + " ${menuCheck}/libexec/hide-atspi-menu/atspi-menu.sh ${hide}/bin/hide"
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
