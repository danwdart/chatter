{ mkDerivation, aeson, async, base, bytestring, discord-haskell
, mtl, process, random, req, safe, stdenv, text, transformers, unix
, vector
}:
mkDerivation {
  pname = "chatter";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson async base bytestring discord-haskell mtl process random req
    safe text transformers unix vector
  ];
  description = "Chat tools";
  license = stdenv.lib.licenses.agpl3;
}
