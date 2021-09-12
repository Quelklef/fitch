# modified version of the default.nix produced by elm2nix

{ pkgs ? import <nixpkgs> {} }:

pkgs.stdenv.mkDerivation {
  name = "fitch";

  src = ./.;

  buildInputs = [ pkgs.elmPackages.elm ];

  buildPhase = pkgs.elmPackages.fetchElmDeps {
    elmPackages = import ./elm2nix/elm-srcs.nix;
    elmVersion = "0.19.1";
    registryDat = ./elm2nix/registry.dat;
  };

  installPhase = ''
    mkdir $out
    elm make $src/src/Main.elm --output=$out/main.js
    cp $src/*.{html,js,css} $out/
  '';
}
