{
  description = "playground";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
    polysemy.url = github:polysemy-research/polysemy/16999eece5b10bc7c9cb8610be785f946e0140be;
  };

  outputs = inputs@{ self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        github = owner: repo: rev: sha256:
          builtins.fetchTarball { inherit sha256; url = "https://github.com/${owner}/${repo}/archive/${rev}.tar.gz"; };

        sources = { };

        jailbreakUnbreak = pkg:
          pkgs.haskell.lib.doJailbreak (pkgs.haskell.lib.dontCheck (pkgs.haskell.lib.unmarkBroken pkg));

        haskellPackages = pkgs.haskell.packages.ghc925.override {
          overrides = hself: hsuper: {
            type-errors = jailbreakUnbreak hsuper.type-errors;
            ListLike = jailbreakUnbreak hsuper.ListLike;
            polysemy = hsuper.callCabal2nix "polysemy" inputs.polysemy { };
            polysemy-plugin = hsuper.callCabal2nix "polysemy-plugin" "${inputs.polysemy}/polysemy-plugin" { };
          };
        };
      in
      rec
      {
        packages.playground = # (ref:haskell-package-def)
          haskellPackages.callCabal2nix "playground" ./. rec {
            # Dependency overrides go here
          };

        defaultPackage = packages.playground;

        devShell =
          let
            scripts = pkgs.symlinkJoin {
              name = "scripts";
              paths = pkgs.lib.mapAttrsToList pkgs.writeShellScriptBin {
                ormolu = ''
                  ${pkgs.ormolu}/bin/ormolu -o -XDataKinds -o -XDefaultSignatures -o -XDeriveAnyClass -o -XDeriveGeneric -o -XDerivingStrategies -o -XDerivingVia -o -XDuplicateRecordFields -o -XFlexibleContexts -o -XGADTs -o -XGeneralizedNewtypeDeriving -o -XKindSignatures -o -XLambdaCase -o -XNoImplicitPrelude -o -XOverloadedLists -o -XOverloadedStrings -o -XRankNTypes -o -XRecordWildCards -o -XScopedTypeVariables -o -XTypeApplications -o -XTypeFamilies -o -XTypeOperators -o -XNoImportQualifiedPost -o -XOverloadedRecordDot $@
                '';
              };
            };
          in
          pkgs.mkShell {
            buildInputs = with haskellPackages; [
              haskell-language-server
              ghcid
              cabal-install
              scripts
            ];
            inputsFrom = [
              self.defaultPackage.${system}.env
            ];
          };
      });
}
