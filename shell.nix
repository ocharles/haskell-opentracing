with import <nixpkgs> {};

let
  ghcWithPackages = 
    haskell.packages.ghc821.ghcWithPackages
      (hs: with hs;
      [ text
        vector
        (with pkgs.haskell.lib; dontCheck (doJailbreak thrift))
        clock
        unliftio
        wai
        scotty
        serialise
      ]);

in
  runCommand "make-env"
    { buildInputs = [ ghcWithPackages ]; }
    ""
