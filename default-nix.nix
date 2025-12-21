{
  nixpkgs ? import <nixpkgs> {},
  haskell-tools ? import (builtins.fetchTarball "https://github.com/danwdart/haskell-tools/archive/master.tar.gz") {
    inherit nixpkgs;
    inherit compiler;
  },
  compiler ? "ghc914"
}:
let
  gitignore = nixpkgs.nix-gitignore.gitignoreSourcePure [ ./.gitignore ];
  tools = haskell-tools compiler;
  inherit (nixpkgs.pkgs.haskell) lib;
  myHaskellPackages = nixpkgs.pkgs.haskell.packages.${compiler}.override {
    overrides = self: super: rec {
      chatio = lib.dontHaddock (self.callCabal2nix "chatio" (gitignore ./.) {});
      websockets = lib.doJailbreak super.websockets;
      discord-haskell = lib.doJailbreak super.discord-haskell;
    };
  };
  shell = myHaskellPackages.shellFor {
    packages = p: [
      p.chatio
    ];
    shellHook = ''
      gen-hie > hie.yaml
      for i in $(find . -type f | grep -v "dist-*"); do krank $i; done
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
  chatio = lib.justStaticExecutables myHaskellPackages.chatio;
}
