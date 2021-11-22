{ nixpkgs ? import (builtins.fetchTarball "https://github.com/NixOS/nixpkgs/archive/master.tar.gz") {},
  compiler ? "ghc901" }:
let
  gitignore = nixpkgs.nix-gitignore.gitignoreSourcePure [ ./.gitignore ];
  lib = nixpkgs.pkgs.haskell.lib;
  myHaskellPackages = nixpkgs.pkgs.haskell.packages.${compiler}.override {
    overrides = self: super: rec {
      # 1.8.4-1.8.7 are broken
      discord-haskell = self.callHackage "discord-haskell" "1.8.3" {};
      # Depends on cabal-un-published http-client versions.
      req = lib.doJailbreak (self.callHackage "req" "3.9.1" {});
      chatter = self.callCabal2nix "chatter" (gitignore ./.) {};
    };
  };
  shell = myHaskellPackages.shellFor {
    packages = p: [
      p.chatter
    ];
    buildInputs = with myHaskellPackages; with nixpkgs; with haskellPackages; [
      apply-refact
      cabal-install
      ghcid
      ghcide
      haskell-language-server
      hlint
      implicit-hie
      krank
      haskellPackages.stan # issue with 9.0.1
      stylish-haskell
      weeder
    ];
    # withHoogle = false;
  };
  exe = lib.justStaticExecutables (myHaskellPackages.chatter);
in
{
  inherit shell;
  inherit exe;
  inherit myHaskellPackages;
  chatter = myHaskellPackages.chatter;
}
