{
  nixpkgs ? import <nixpkgs> {},
  haskell-tools ? import (builtins.fetchTarball "https://github.com/danwdart/haskell-tools/archive/master.tar.gz") {
    nixpkgs = nixpkgs;
    compiler = compiler;
  },
  compiler ? "ghc910"
}:
let
  gitignore = nixpkgs.nix-gitignore.gitignoreSourcePure [ ./.gitignore ];
  tools = haskell-tools compiler;
  lib = nixpkgs.pkgs.haskell.lib;
  myHaskellPackages = nixpkgs.pkgs.haskell.packages.${compiler}.override {
    overrides = self: super: rec {
      chatter = lib.dontHaddock (self.callCabal2nix "chatter" (gitignore ./.) {});
      websockets = lib.doJailbreak super.websockets;
      discord-haskell = lib.doJailbreak super.discord-haskell;
    };
  };
  shell = myHaskellPackages.shellFor {
    packages = p: [
      p.chatter
    ];
    shellHook = ''
      gen-hie > hie.yaml
      for i in $(find -type f | grep -v dist-newstyle); do krank $i; done
    '';
    buildInputs = tools.defaultBuildTools ++ (with nixpkgs; [
      expect
      rlwrap
    ]);
    # withHoogle = false;
  };
  in
{
  inherit shell;
  chatter = lib.justStaticExecutables (myHaskellPackages.chatter);
}
