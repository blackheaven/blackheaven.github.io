{
  description = "cyberuniversity-soc-course";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = inputs@{ self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        # pkgs = nixpkgs.legacyPackages.${system};
        pkgs = import nixpkgs {
          inherit system;
          # for `vscode-with-extensions`
          config.allowUnfree = true;
        };

        github = owner: repo: rev: sha256:
          builtins.fetchTarball { inherit sha256; url = "https://github.com/${owner}/${repo}/archive/${rev}.tar.gz"; };

        sources = { };
      in
      rec
      {
        devShell =
          let
            scripts = pkgs.symlinkJoin {
              name = "scripts";
              paths = pkgs.lib.mapAttrsToList pkgs.writeShellScriptBin { };
            };
          in
          pkgs.mkShell {
            buildInputs = with pkgs; [
              zola
              scripts
            ];
            inputsFrom = [
            ];
          };
      });
}
