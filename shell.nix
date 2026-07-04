with import (builtins.fetchTarball "https://github.com/NixOS/nixpkgs/archive/master.zip") {};
# needs mkShell in order to use headers/etc. from deps! how do we do that from nix-shell 
let cabalPkgs = [
    # { name = "apply-refact"; exe = "refactor"; ver = "0.15.0.0"; }
    { name = "cabal-add"; ver = "0.2"; }
    { name = "cabal-fmt"; ver = "0.1.12"; }
    { name = "doctest"; exe = "cabal-doctest"; ver = "0.24.3"; flags = "-fcabal-doctest"; }
    { name = "ghcid"; ver = "0.8.9"; }
    # { name = "ghci-dap"; ver = "0.0.26.0"; }
    { name = "krank"; ver = "0.3.1"; flags = "--allow-newer=template-haskell"; }
    { name = "haskell-debugger"; ver = "0.13.1.0"; exe = "hdb"; }
    { name = "haskell-debug-adapter"; ver = "0.0.42.0"; flags = "--allow-newer=aeson,bytestring,containers,deepseq,template-haskell,time"; }
    { name = "haskell-language-server"; ver = "2.13.0.0"; flags = "--allow-newer=containers,deepseq,ghc,hiedb,template-haskell,time,websockets"; }
    { name = "hasktags"; ver = "0.73.0"; }
    { name = "hlint"; ver = "3.10"; flags = "--allow-newer=template-haskell"; }
    { name = "hoogle"; ver = "5.0.19.0"; flags = "--allow-newer=template-haskell"; }
    { name = "hpack"; ver = "0.39.3"; flags = "--allow-newer=template-haskell"; }
    { name = "implicit-hie"; exe = "gen-hie"; ver = "0.1.4.0"; flags = "--allow-newer=bytestring,template-haskell"; }
    # { name = "stan"; ver = "0.2.1.0"; }
    { name = "stylish-haskell"; ver = "0.15.1.0"; flags = "--allow-newer=template-haskell"; }
]; in
mkShell rec {
    packages = [
        haskell.compiler.ghc914
        pcre.dev
        cabal-install
        # krank
        pkg-config
        zlib.dev
    ];
    shellHook = ''
        cabal update >/dev/null
        set -euo pipefail
        ${lib.concatStringsSep "\n" (builtins.map ({name, exe ? name, ver, flags ? ""}:
            "[[ -f ~/.local/bin/${exe} ]] || echo cabal install ${name}-${ver} ${flags} >&2"
        ) cabalPkgs)}        
        export PATH="~/.local/bin:$PATH"
        # gen-hie > hie.yaml
        # for i in $(find . -type f | grep -v "dist-*"); do krank $i || : >&2; done
    '';
}