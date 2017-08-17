{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, servant, servant-client, stdenv, free, freer-effects, kan-extensions }:
      mkDerivation {
        pname = "listenbrainz-client";
        version = "0.1.0.0";
        src = ./.;
        libraryHaskellDepends = [ base servant servant-client free freer-effects kan-extensions ];
        homepage = "https://github.com/ocharles/listenbrainz-client";
        description = "A client library to the ListenBrainz project";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
