+++
title = "Haskell contributions kick-start"
date = 2023-10-04
draft = false
path = "2023-10/haskell-contributions-kickstart"

[taxonomies]
categories = ["FOSS"]
tags = ["foss", "contributions", "haskell"]
+++

Few days ago, one of my new LinkedIn connection, introduced himself/herself,
her/his background and public (Github) contributions, and asked me if I can
provide some links/advices to start contributing to the Haskell ecosystem.
I'd like to develop the answer I gave.

Note: no matter who you are, where you are at this point of your life,
what your background is, what your believes are, you should only be judged
on your contributions and your behavior (and most of the time, you are).

So, if you are looking for a smooth start, I would suggest to either participate
to Discourse discussions or [Github](https://discourse.haskell.org/) issues
(especially [CLC](https://github.com/haskell/core-libraries-committee) / [GHC Proposals](https://github.com/ghc-proposals/ghc-proposals)).
Not only giving your point of view, may highlight blind spots, but you'll also
get an understanding of Haskell design process.

You can (or should) contribute to the documentation effort: fixing typos, rewriting
ambiguous sentences, dropping dead links.
It cannot be understated that you can waste or save hours due/thanks to the documentation.

If you are looking to do code contributions checkout [Haskell on Github](https://github.com/haskell)
they are a lot of open issues (especially with Hacktoberfest).

If I might suggest having a look at [security-advisories repository](https://github.com/haskell/security-advisories).

When it comes to actually contributing, I try to pick a repository/project with
some activities in the last 6 months, then:

1. I browse issues (starting with `good first issue` tagged issues, then technical ones, such as dependencies upgrades or newer GHC supports)
2. Then, I fork, clone, setup my remotes and branches
3. Then I try to have compilations (and tests if possible) working locally (see below)
4. Finally, I manage to have a minimal version to show (which might be reworked later, but which is a basis for discussions/iterations)

And hopefully one of the maintainer will find some time (and you should be grateful for that).

One last thing, you should be clear with your intent when you contribute.

Usually I have one of these goals:

* Learn something
* Fix a bug I have encountered, implement a feature I need
* Fix the document to earn some time the next time I will read it
* Payback with my time

I try to not forget that I'm not the maintainer of the project and, consequently:

* I won't be responsible for maintaining my contributions
* I don't know all the decisions (and motivations behind them) taken to achieve to the project as it is
* I don't know all the stakeholders, so I cannot give an educated guess of my contribution impact

Note: I'm using NixOS, so, for Haskell projects, when there are no nix support, I start with a `flake.nix` like this:

```nix
{
  description = "password";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = inputs@{ self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };

        sources = { };

        jailbreakUnbreak = pkg:
          pkgs.haskell.lib.doJailbreak (pkgs.haskell.lib.dontCheck (pkgs.haskell.lib.unmarkBroken pkg));

        haskellPackages = pkgs.haskellPackages.override {
          overrides = hself: hsuper: {
            # Dependency overrides go here
          };
        };
      in
      rec
      {
        packages.password-types =
          # activateBenchmark
          (haskellPackages.callCabal2nix "password-types" ./password-types rec {
            # Dependency overrides go here
          });

        packages.password =
          # activateBenchmark
          (haskellPackages.callCabal2nix "password" ./password rec {
            # Dependency overrides go here
            password-types = packages.password-types;
          });

        packages.password-cli =
          # activateBenchmark
          (haskellPackages.callCabal2nix "password-cli" ./password-cli rec {
            # Dependency overrides go here
            password-types = packages.password-types;
            password = packages.password;
          });

        defaultPackage = packages.password-cli;

        devShell =
          pkgs.mkShell {
            buildInputs = with haskellPackages; [
              haskell-language-server
              ghcid
              cabal-install
            ];
            inputsFrom = [
              self.defaultPackage.${system}.env
            ];
          };
      });
}
```
