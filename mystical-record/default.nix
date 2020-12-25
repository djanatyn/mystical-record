{ zlib, haskellPackages }:

haskellPackages.mkDerivation rec {
  pname = "mystical-record";
  version = "0.1.0.0";

  isLibrary = false;
  isExecutable = true;

  executableHaskellDepends = with haskellPackages; [
    cabal-install
    haskell-language-server

    base
    text
    megaparsec
    parser-combinators
    selda
    servant
    servant-client
    servant-client-core

    zlib

    hxt
    hxt-css
  ];

  src = builtins.path {
    path = ./.;
    name = pname;
  };

  license = "Unlicense";
}
