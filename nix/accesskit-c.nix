# SPDX-FileCopyrightText: 2026 Edward Kmett
# SPDX-License-Identifier: BSD-2-Clause OR Apache-2.0
{ lib, rustPlatform, fetchurl, unzip }:

rustPlatform.buildRustPackage rec {
  pname = "accesskit-c";
  version = "0.23.2";
  src = fetchurl {
    url = "https://github.com/AccessKit/accesskit-c/releases/download/${version}/accesskit-c-${version}.zip";
    hash = "sha256-x/qE+PNPypCfZiN1c3pU+YsXz56uUf/9lKs9n2YuNoE=";
  };
  nativeBuildInputs = [ unzip ];
  cargoLock.lockFile = ./accesskit-c-Cargo.lock;
  outputs = [ "out" "dev" ];
  # The editor's installed native check owns its adapter acceptance.
  doCheck = false;
  installPhase = ''
    runHook preInstall
    install -Dm644 "$(find target -name libaccesskit.a -print -quit)" "$out/lib/libaccesskit.a"
    install -Dm644 include/accesskit.h "$dev/include/accesskit.h"
    mkdir -p "$dev/lib/pkgconfig"
    cat > "$dev/lib/pkgconfig/accesskit-c.pc" <<PC
    prefix=$out
    libdir=$out/lib
    includedir=$dev/include
    Name: accesskit-c
    Description: AccessKit C bindings
    Version: ${version}
    Libs: -L$out/lib -laccesskit -ldl -lpthread -lm
    Cflags: -I$dev/include
    PC
    runHook postInstall
  '';
  meta = {
    description = "C bindings for the AccessKit accessibility toolkit";
    homepage = "https://github.com/AccessKit/accesskit-c";
    license = with lib.licenses; [ mit asl20 ];
    platforms = lib.platforms.linux;
  };
}
