+++
title = "Nix: optimizing Haskell build size"
date = 2023-10-08
draft = false
path = "2023-10/nix-build-optimization"

[taxonomies]
categories = ["dev"]
tags = ["haskell", "nix", "build", "CI", "optimization"]
+++

Nearly three months ago I wrote a log about [reducing Docker size](@/blog/2023-07-16_nix-docker-optimization.md),
mainly by relying on static builds.

While perfectly relevant in this context, when I introduced Continuous Integration (CI)
at my work (which was building with a vanilla `callCabal2nix`), the CI server disk
filled-up in few hours.

For each build:

* Duration: 5 minutes
* Output size:
  * Total: 313 MiB
  * `bin` (9 executables): 101 MiB
  * `lib`: 212 MiB

After a quick look at the size per files extension, we got:

* `hi` (interface file): 15 MiB
* `dyn_hi` (interface file for dynamic linking): 15 MiB
* `so` (dynamic library): 17 MiB (1 file)
* `p_hi` (interface file for profiling): 18 MiB
* `a` (static library): 149 MiB (2 files)

Let's go for the most obvious:

* We only run dynamically linked executables
* We do not run profiled executables

We can wrap our final executable with:

```nix
exeOnly = b:
  with pkgs.haskell.lib;
  enableSharedExecutables (enableSharedLibraries
    (disableStaticLibraries
      (disableExecutableProfiling (disableLibraryProfiling b))));
```

Let's see the results:

* Duration: 2 minutes
* Output size:
  * Total: 91 MiB
  * `bin` (9 executables): 1.5 MiB
  * `lib`: 89 MiB

Note: the build time drop comes mainly from the profiling support drop

Let's have look at the size per files extension, we got:

* `hi` (interface file): 15 MiB
* `dyn_hi` (interface file for dynamic linking): 15 MiB
* `so` (dynamic library): 17 MiB (1 file)
* `a` (static library): 44 MiB (1 file)

Note: the remaining `a` file seems to come from a previous derivation.

That's a good first step, but actually, I only intend to run it, not to use it
as the input of another build, let's remove files manually:

```nix
exeOnly = b:
  with pkgs.haskell.lib;
  overrideCabal (enableSharedExecutables (enableSharedLibraries
    (disableStaticLibraries
      (disableExecutableProfiling (disableLibraryProfiling b)))))
  (drv: {
    postFixup = drv.postFixup or "" + ''
      ${pkgs.findutils}/bin/find $out -name '*.a' -exec rm \{\} \+
      ${pkgs.findutils}/bin/find $out -name '*.hi' -exec rm \{\} \+
    '';
  });
```

And the final results are:

* Duration: 2 minutes 10 seconds
* Output size:
  * Total: 34 MiB
  * `bin` (9 executables): 1.5 MiB
  * `lib`: 32 MiB

Note: I guess the 10 seconds increase is due to `find`

Finally we only have the necessary files:

* `dyn_hi` (interface file for dynamic linking): 15 MiB
* `so` (dynamic library): 17 MiB (1 file)

To sum-up:

* Time: 5 minutes -> 2 minutes 10 seconds (57% drop)
* Size: 313 MiB -> 34 MiB (89% drop)
