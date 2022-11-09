{
  nixpkgs ? import <nixpkgs> {},
  haskell-tools ? import (builtins.fetchTarball "https://github.com/danwdart/haskell-tools/archive/master.tar.gz") {
    nixpkgs = nixpkgs;
    compiler = compiler;
  },
  compiler ? "ghc94"
}:
let
  gitignore = nixpkgs.nix-gitignore.gitignoreSourcePure [ ./.gitignore ];
  tools = haskell-tools compiler;
  lib = nixpkgs.pkgs.haskell.lib;
  myHaskellPackages = nixpkgs.pkgs.haskell.packages.${compiler}.override {
    overrides = self: super: rec {
      discord-haskell = lib.doJailbreak (self.callHackage "discord-haskell" "1.15.3" {});
      # not released yet
      req = self.callHackage "req" "3.13.0" {};
      chatter = lib.dontHaddock (self.callCabal2nix "chatter" (gitignore ./.) {});
    };
  };
  shell = myHaskellPackages.shellFor {
    packages = p: [
      p.chatter
    ];
    buildInputs = tools.defaultBuildTools ++ (with nixpkgs; [
      expect
      rlwrap
    ]);
    # withHoogle = false;
  };
  exe = lib.justStaticExecutables (myHaskellPackages.chatter);
in
{
  inherit shell;
  chatter = lib.justStaticExecutables (myHaskellPackages.chatter);
}
