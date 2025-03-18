+++
title = "NAS Migration"
date = 2025-03-18
draft = false
path = "2025-03/nas-migration"

[taxonomies]
categories = ["ops"]
tags = ["ops", "nix", "nixos"]
+++

A long time, I have described my [home network](@/blog/2023-08-16_homelab-init.md),
I have worked a lot on my homelab (_Barracuda_), and, in late December, I have
decided to replace it, and my NAS.

I have replaced my 10 years old Netgear ReadyNAS 104 NAS _Hannibal_, and my
2 years old MINIS FORUM GK41 homelab _Barracuda_, by a Terramaster F4-424 Pro
NAS/homelab _Hannibal_ (yes, I have reused the name, yes, I had issue doing so).

It was motivated by many elements:

* _Hannibal_ was really noisy during snapshots (every night)
* _Barracuda_ was overload: the CPU was at 25% usage as a baseline (going to 100% during peaks, and was not responding), I had a 40% RAM usage and 25% swap usage
* The performances were bad: all my services were on _Barracuda_ and the storage on _Hannibal_, which made their use really inefficient too
* There were too many moving parts, from time to time, I had network/power-supply issues, which forced a restart, many of the services were not able to start because the network were down, or _Hannibal_ unreachable

I have picked the Terramaster F4-424 Pro for the following reasons:

* CPU (x5 compared to _Barracuda_), RAM (x4 compared to _Barracuda_), and 4 Bays (the same as _Hannibal_)
* Easiness to change the OS and plug storage (HDD & SSD)
* A solid case

So far, there's only one issue: officially, there are two USB 3.2 port, BUT,
one is used by a dongle for the Terramaster OS (TOS), in order to put your own,
you should unplug it, but, it is positioned such as, you can unplug it, but
it stays in the port, consequently, you have only one port left.

Doing so, prevents you to have your USB dongle with the GNU/Linux Live
distribution, and a keyboard to do installation instructions.

So, I have crafted a NixOS ISO image which sets up the network connection and
an OpenSSH server, so I can make the actions from _Looping_ (my desktop PC):

```nix
installer-iso = inputs.nixpkgs.lib.nixosSystem {
  specialArgs = { inherit inputs; };
  modules = [
    ({ pkgs, lib, modulesPath, ... }: {
      imports = [ "${modulesPath}/installer/cd-dvd/installation-cd-minimal.nix" ];
      nixpkgs.hostPlatform = "x86_64-linux";

      services.openssh = {
        enable = true;
        openFirewall = true;
      };


      environment.interactiveShellInit = " sleep 15 && ip addr";

      # networking.interfaces = {
      #   enp1s0.useDHCP = true;
      #   enp2s0.useDHCP = true;
      # };

      networking.firewall.enable = false;

      users.users = {
        root.initialPassword = "root";
        root.initialHashedPassword = lib.mkForce null;
        root.openssh.authorizedKeys.keys = [ "..." ];
        nixos.initialPassword = "nixos";
        nixos.initialHashedPassword = lib.mkForce null;
        nixos.openssh.authorizedKeys.keys = [ "..." ];
    };
    })
  ];
};
````

It worked so far, but I have encountered another issue: this NixOS was built
upon `nixos-unstable` (`nixos-25.05` preview), while _Barracuda_ was on an
older version `nixos-unstable` (`nixos-24.05` preview), I have mechanically
updated my `flake.lock`, which led to two issues:

* Some options were removed (I have to search extensively to find a migration process)
* Some of my services used (silently) invalid version

Let me elaborate on the last one: I was applying my _Barracuda_ NixOS module
by NixOS module.
Each time I was adding one in my import, making the options' migration,
deploying, if it was a successful deployment, I started again.

At some point, I checked my monitoring (one of the first module I have
integrated), and it was saying "CPU used at 60%, RAM used at 40%".
It was not what I was expecting, given that it was my _Barracuda_
statistics, and _Hannibal_ being 4-5 times more performant.

Actually, few of my services were misconfigured (the configuration changed
in the new version), or using the wrong package (so I had services constantly
restarting).

Then, I have added few more services previously handled by my NAS (especially
the storage, which now is handled by ZFS).

I have checked my monitoring again: "CPU used at 6%, RAM used at 25%".

Great so far.

I have learned few things doing this migration:

* Put everything possible in a declarative form: along the way, I have done few manual operations on _Barracuda_, documented nowhere, which made me waste a lot of time
* Don't reuse existing name, reusing _Hannibal_ without transition period was quite confusing, on my _Barracuda_ and _Looping_ configuration
* Always have a free storage big enough for all your data: I have choosen to reuse my _Hannibal_ HDDs directly, sadly, I didn't have and free storage big enough (I'll write on my backup strategy in few weeks)
* Globally, things went well, it took me 3 long days and two nights for the whole process, while I've put months building my homelab
