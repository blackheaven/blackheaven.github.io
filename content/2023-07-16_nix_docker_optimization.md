+++
title = "Nix: optimizing Haskell-based Docker-size"
date = 2023-07-16
draft = false
path = "2023-07/nix-docker-optimization"

[taxonomies]
categories = ["dev"]
tags = ["haskell", "nix", "docker", "optimization"]
+++

Few months ago, I joined the [Haskell Security Response Team](https://discourse.haskell.org/t/haskell-security-response-team-announcement-and-q2-2023-report/6931).

I've been involved in [hsec-tools](https://github.com/haskell/security-advisories/tree/main/code/hsec-tools), which aims, in the medium terms, to support the whole security advisories infrastructure.

One of our distributions is [nix](https://github.com/NixOS/nix), so, when I had to create a docker image, I started with:

```nix
pkgs.dockerTools.buildImage {
  name = "haskell/hsec-tools";
  tag = "latest";

  copyToRoot = pkgs.buildEnv {
    name = "image-root";
    paths = [
      self.packages.${system}.hsec-tools # is a haskellPackages.callCabal2nix
      pkgs.git.out
    ];
    pathsToLink = [ "/bin" "/" ];
  };
  config = {
    Cmd = [ "/bin/hsec-tools" ];
    Env = [
      "LOCALE_ARCHIVE=${pkgs.glibcLocales}/lib/locale/locale-archive"
      "LC_TIME=en_US.UTF-8"
      "LANG=en_US.UTF-8"
      "LANGUAGE=en"
      "LC_ALL=en_US.UTF-8"
      "GIT_DISCOVERY_ACROSS_FILESYSTEM=1"
    ];
    Volumes = {
      "/advisories" = { };
    };
    WorkDir = "/";
  };
};
```

Which gives a 671MB (compressed) / 4.17GB (loaded) docker image.

Terr... i... ble...

Thanks to [`dive`](https://github.com/wagoodman/dive), I had a look at the image, and it contains useless stuff such as GHC or the documentation of many packages.

```
...
 28 MB  │       ├── 0cddajm56ssb2y2k9his4sqa7z0v0n8h-pandoc-types-1.22.2.1  
 28 MB  │       │   ├── lib                                                 
 28 MB  │       │   │   └── ghc-9.2.5                                       
3.2 kB  │       │   │       ├── package.conf.d                              
3.2 kB  │       │   │       │   └── pandoc-types-1.22.2.1-1PXzD7fZz22Kk4Zvr 
 28 MB  │       │   │       └── x86_64-linux-ghc-9.2.5                      
2.9 MB  │       │   │           ├── libHSpandoc-types-1.22.2.1-1PXzD7fZz22K 
 25 MB  │       │   │           └── pandoc-types-1.22.2.1-1PXzD7fZz22Kk4Zvr 
 13 kB  │       │   │               ├── Paths_pandoc_types.dyn_hi           
 13 kB  │       │   │               ├── Paths_pandoc_types.hi               
 18 kB  │       │   │               ├── Paths_pandoc_types.p_hi             
4.2 MB  │       │   │               ├── Text                                
4.2 MB  │       │   │               │   └── Pandoc                          
 72 kB  │       │   │               │       ├── Arbitrary.dyn_hi            
 72 kB  │       │   │               │       ├── Arbitrary.hi                
 70 kB  │       │   │               │       ├── Arbitrary.p_hi              
140 kB  │       │   │               │       ├── Builder.dyn_hi              
140 kB  │       │   │               │       ├── Builder.hi                  
 92 kB  │       │   │               │       ├── Builder.p_hi                
1.0 MB  │       │   │               │       ├── Definition.dyn_hi           
1.0 MB  │       │   │               │       ├── Definition.hi               
1.0 MB  │       │   │               │       ├── Definition.p_hi             
 12 kB  │       │   │               │       ├── Generic.dyn_hi              
 12 kB  │       │   │               │       ├── Generic.hi                  
 11 kB  │       │   │               │       ├── Generic.p_hi                
 23 kB  │       │   │               │       ├── JSON.dyn_hi                 
 23 kB  │       │   │               │       ├── JSON.hi                     
 23 kB  │       │   │               │       ├── JSON.p_hi                   
148 kB  │       │   │               │       ├── Walk.dyn_hi                 
148 kB  │       │   │               │       ├── Walk.hi                     
150 kB  │       │   │               │       └── Walk.p_hi                   
7.4 MB  │       │   │               ├── libHSpandoc-types-1.22.2.1-1PXzD7fZ 
 13 MB  │       │   │               └── libHSpandoc-types-1.22.2.1-1PXzD7fZ 
 176 B  │       │   └── nix-support                                         
 176 B  │       │       └── propagated-build-inputs
...
679 MB  │       ├─⊕ 0jdbwn0ixmyk2irpiiygphjyrnzxyxnk-ghc-9.2.5-doc
...
1.7 GB  │       ├─⊕ 3abmvcz8b064a4l1k9vgbbqwaw3qxp7y-ghc-9.2.5
...
```

Hopefully, `haskell.lib` provides `justStaticExecutables`:

```nix
(pkgs.haskell.lib.justStaticExecutables self.packages.${system}.hsec-tools)
```

We got down to 174 MB / 574 MB.

We can do better, especially with:

```
224 MB  │       ├─⊕ zbaycxgvv3iaa18p889dagcfhinasvcx-glibc-locales-2.37-8
```

We need it to be able to read UTF-8 content, let's try another package:

```nix
"LOCALE_ARCHIVE=${pkgs.glibcLocalesUtf8}/lib/locale/locale-archive"
```

125 MB / 368 MB, great!

Let's keep going, which strikes me is `git` size: 285 MB (it pulls, among other things, `python3`), when [alpine](https://hub.docker.com/r/alpine/git) version is 21 MB.

It it used to inject creation/edition date during advisory parsing.

We can use an alternative package:

```nix
pkgs.gitMinimal.out
```
Now we're at 63 MB / 175MB.

One last thing useless is the `/share` directory:

```
 13 MB  └── share
   0 B      ├── bash-completion
   0 B      │   └── completions
   0 B      │       ├── git → ../../git/contrib/completion/git-completion.b
   0 B      │       └── git-prompt.sh → ../../git/contrib/completion/git-pr
567 kB      ├─⊕ git
 69 kB      ├─⊕ git-core
1.3 MB      ├─⊕ git-gui
429 kB      ├─⊕ gitk
 11 MB      └── locale
896 kB          ├── bg
896 kB          │   └── LC_MESSAGES
896 kB          │       └── git.mo
662 kB          ├── ca
662 kB          │   └── LC_MESSAGES
662 kB          │       └── git.mo
```

which is not particularly useful when you don't use it interactively:

```nix
runAsRoot = "rm -Rf /share";
```

Final image 59 MB / 159 MB.

Compared to our initial 671MB / 4.17GB image, it is reduced down to 8.8% / 3.7%.
