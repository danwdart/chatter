{ nixpkgs ? import <nixpkgs> {},
  compiler ? "ghc8103" }:
let
  gitignore = nixpkgs.nix-gitignore.gitignoreSourcePure [ ./.gitignore ];
  myHaskellPackages = nixpkgs.pkgs.haskell.packages.${compiler}.override {
    overrides = self: super: rec {
      discord-haskell = self.callCabal2nix "discord-haskell" (builtins.fetchGit {
        url = "https://github.com/aquarial/discord-haskell.git";
        rev = "8e1988edaf9b39cc27f44c966e16a33d1ead7a35";
      }) {};
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
      nixpkgs.haskellPackages.stack
      nixpkgs.haskellPackages.ghcid
      nixpkgs.haskellPackages.stylish-haskell
      nixpkgs.haskellPackages.hlint
    ];
    # withHoogle = true;
  };
  exe = nixpkgs.haskell.lib.justStaticExecutables (myHaskellPackages.chatter);
in
{
  inherit shell;
  inherit exe;
  inherit myHaskellPackages;
  chatter = myHaskellPackages.chatter;
}