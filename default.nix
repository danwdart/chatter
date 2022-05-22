{
  nixpkgs ? import <unstable> {},
  haskell-tools ? import (builtins.fetchTarball "https://github.com/danwdart/haskell-tools/archive/master.tar.gz") {},
  compiler ? "ghc922"
}:
let
  gitignore = nixpkgs.nix-gitignore.gitignoreSourcePure [ ./.gitignore ];
  tools = haskell-tools compiler;
  lib = nixpkgs.pkgs.haskell.lib;
  myHaskellPackages = nixpkgs.pkgs.haskell.packages.${compiler}.override {
    overrides = self: super: rec {
      # 1.8.4-1.8.7 are broken
      discord-haskell = self.callHackage "discord-haskell" "1.12.4" {};
      # Depends on cabal-un-published http-client versions.
      req = lib.doJailbreak (self.callHackage "req" "3.10.0" {});
      wuss = lib.doJailbreak super.wuss;
      modern-uri = lib.doJailbreak super.modern-uri;
      chatter = lib.dontHaddock (self.callCabal2nix "chatter" (gitignore ./.) {});
    };
  };
  shell = myHaskellPackages.shellFor {
    packages = p: [
      p.chatter
    ];
    buildInputs = tools.defaultBuildTools;
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
