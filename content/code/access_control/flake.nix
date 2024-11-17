{
  description = "accesscontrol";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, ... }@inputs:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        jailbreakUnbreak = pkg:
          pkgs.haskell.lib.doJailbreak
          (pkgs.haskell.lib.dontCheck (pkgs.haskell.lib.unmarkBroken pkg));

        haskellPackages = pkgs.haskellPackages;
      in rec {
        packages.accesscontrol = # (ref:haskell-package-def)
          haskellPackages.callCabal2nix "accesscontrol" ./. {
            # Dependency overrides go here
          };

        defaultPackage = packages.accesscontrol;

        devShell = let
          scripts = pkgs.symlinkJoin {
            name = "scripts";
            paths = pkgs.lib.mapAttrsToList pkgs.writeShellScriptBin { };
          };
        in pkgs.mkShell {
          buildInputs = with haskellPackages; [
            haskell-language-server
            ghcid
            cabal-install
            scripts
          ];
          inputsFrom = [ self.defaultPackage.${system}.env ];
        };
      });
}
