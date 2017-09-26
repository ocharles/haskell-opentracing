{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, bytestring, clock, hashable, QuickCheck
      , stdenv, text, thrift, unordered-containers, vector
      }:
      mkDerivation {
        pname = "jaeger-client";
        version = "0.1.0.0";
        src = ./.;
        libraryHaskellDepends = [
          base bytestring clock hashable QuickCheck text thrift
          unordered-containers vector
        ];
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = (haskellPackages.override {
    overrides = self: super: {
      thrift = with pkgs.haskell.lib; doJailbreak (dontCheck super.thrift);
    };
  }).callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
