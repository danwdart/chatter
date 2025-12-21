with import (builtins.fetchTarball "https://github.com/guibou/nixpkgs/archive/ghc-914.zip") {};
# needs mkShell in order to use headers/etc. from deps! how do we do that from nix-shell 
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
        # no krank yet
        [[ -f ~/.local/bin/ghcid ]] || cabal install cabal-add cabal-fmt ghcid haskell-debug-adapter hasktags hoogle hpack implicit-hie krank --allow-newer
        export PATH=~/.local/bin:$PATH
        gen-hie > hie.yaml
        for i in $(find . -type f | grep -v "dist-*"); do krank $i; done
    '';
}