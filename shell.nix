with import <nixpkgs> {};
# needs mkShell in order to use headers/etc. from deps! how do we do that from nix-shell 
mkShell rec {
    packages = [
        haskell.compiler.ghc912
        pcre.dev
        cabal-install
        # krank
        pkg-config
        zlib.dev
    ];
    shellHook = ''
        # no krank yet
        [[ -f ~/.local/bin/refactor ]] || cabal install apply-refact cabal-add cabal-fmt doctest ghci-dap ghcid ghcide haskell-debug-adapter haskell-language-server hasktags hlint hoogle hpack implicit-hie stan stylish-haskell weeder --allow-newer
        export PATH=~/.local/bin:$PATH
        gen-hie > hie.yaml
        for i in $(find . -type f | grep -v "dist-*"); do krank $i; done
    '';
}