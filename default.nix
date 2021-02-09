{ mkDerivation, base, bytestring, cereal, containers, lib
, monad-loops, pretty-hex, unix
}:
mkDerivation {
  pname = "netlink";
  version = "1.1.2.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring cereal containers monad-loops pretty-hex unix
  ];
  executableHaskellDepends = [ base ];
  homepage = "https://github.com/Ongy/netlink-hs";
  description = "Netlink communication for Haskell";
  license = lib.licenses.bsd3;
}
