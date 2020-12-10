{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc8102" }:
let
  myHaskellPackages = nixpkgs.pkgs.haskell.packages.${compiler}.override {
    overrides = self: super: rec {
      discord-haskell = self.callCabal2nix "discord-haskell" (builtins.fetchGit {
        url = "https://github.com/aquarial/discord-haskell.git";
        rev = "8e1988edaf9b39cc27f44c966e16a33d1ead7a35";
      }) {};
    };
  };
in
myHaskellPackages.callPackage ./chatter.nix {}
