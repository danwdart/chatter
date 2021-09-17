{ nixpkgs ? import (builtins.fetchTarball "https://github.com/NixOS/nixpkgs/archive/master.tar.gz") {},
  compiler ? "ghc901" }:
let
  gitignore = nixpkgs.nix-gitignore.gitignoreSourcePure [ ./.gitignore ];
  myHaskellPackages = nixpkgs.pkgs.haskell.packages.${compiler}.override {
    overrides = self: super: rec {
      # 1.8.4-1.8.7 are broken
      discord-haskell = self.callHackage "discord-haskell" "1.8.3" {};
      # Depends on cabal-un-published http-client versions.
      req = nixpkgs.pkgs.haskell.lib.doJailbreak (self.callHackage "req" "3.9.1" {});
      chatter = self.callCabal2nix "chatter" (gitignore ./.) {};
    };
  };
  shell = myHaskellPackages.shellFor {
    packages = p: [
      p.chatter
    ];
    buildInputs = [
      nixpkgs.haskellPackages.cabal-install
      nixpkgs.wget
      nixpkgs.haskellPackages.ghcid
      nixpkgs.haskellPackages.stylish-haskell
      nixpkgs.haskellPackages.hlint
    ];
    # withHoogle = false;
  };
  exe = nixpkgs.haskell.lib.justStaticExecutables (myHaskellPackages.chatter);
in
{
  inherit shell;
  inherit exe;
  inherit myHaskellPackages;
  chatter = myHaskellPackages.chatter;
}
