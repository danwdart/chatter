{
  nixpkgs ? import <nixpkgs> {},
  haskell-tools ? import (builtins.fetchTarball "https://github.com/danwdart/haskell-tools/archive/master.tar.gz") {
    nixpkgs = nixpkgs;
    compiler = compiler;
  },
  compiler ? "ghc92"
}:
let
  gitignore = nixpkgs.nix-gitignore.gitignoreSourcePure [ ./.gitignore ];
  tools = haskell-tools compiler;
  lib = nixpkgs.pkgs.haskell.lib;
  myHaskellPackages = nixpkgs.pkgs.haskell.packages.${compiler}.override {
    overrides = self: super: rec {
      # 0.1.3 not available in nix's hackage cache yet
      # emojis = self.callHackage "emojis" "0.1.3" {};
      emojis = self.callCabal2nix "emojis" (nixpkgs.fetchFromGitHub {
        owner = "jgm";
        repo = "emojis";
        rev = "6b9e79a3dc4e220cae1914c8849a61208b387e81";
        sha256 = "xu0UGkF4ClKWEiOelhb4iWbeZ7jLPnAM8tbiVvbXFbk=";
      }) {};
      # 1.15.6 not available in nix's hackage cache yet
      # discord-haskell = lib.doJailbreak (self.callHackage "discord-haskell" "1.15.6" {});
      discord-haskell = self.callCabal2nix "discord-haskell" (nixpkgs.fetchFromGitHub {
        owner = "discord-haskell";
        repo = "discord-haskell";
        rev = "617fa38cdba981ff3c7ffb8df429da429045eda3";
        sha256 = "ppBklj8DIWU4fiH0mOwPwPXv05UNkCJew0Zm6hHnA/k=";
      }) {};
      # not released yet
      # req = self.callHackage "req" "3.13.0" {};
      # http-api-data = lib.doJailbreak super.http-api-data;
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
