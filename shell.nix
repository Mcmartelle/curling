let
  pkgs = import (fetchTarball("https://github.com/NixOS/nixpkgs/archive/nixos-23.11.tar.gz")) {};
in
pkgs.mkShell {
  packages = [
    pkgs.caddy
    pkgs.elmPackages.elm
    pkgs.elmPackages.elm-language-server
    pkgs.elmPackages.elm-format
    pkgs.elmPackages.elm-optimize-level-2
    pkgs.elmPackages.elm-review
    pkgs.elmPackages.elm-test
    pkgs.elmPackages.elm-json
    pkgs.nodePackages.uglify-js
  ];
}

