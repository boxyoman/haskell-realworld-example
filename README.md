# ![RealWorld Example App](logo.png)

> ### [Haskell/Servant/Beam] codebase containing real world examples (CRUD, auth, advanced patterns, etc) that adheres to the [RealWorld](https://github.com/gothinkster/realworld) spec and API.


### [Demo](https://github.com/gothinkster/realworld)&nbsp;&nbsp;&nbsp;&nbsp;[RealWorld](https://github.com/gothinkster/realworld)


This codebase was created to demonstrate a fully fledged fullstack application built with **[Haskell/Servant/Beam]** including CRUD operations, authentication, routing, pagination, and more.

We've gone to great lengths to adhere to the **[Haskell/Servant/Beam]** community styleguides & best practices.

For more information on how to this works with other frontends/backends, head over to the [RealWorld](https://github.com/gothinkster/realworld) repo.


# How it works

> TODO

# Getting started

## Installation

Install [Nix](https://nixos.org/nix/). If you're on a Mac follow
[these instructions](https://medium.com/@zw3rk/provisioning-a-nixos-server-from-macos-d36055afc4ad)
to setup NixOps, otherwise

    nix-env -i nixops

Clone the repository and move into it

    git clone https://github.com/boxyoman/haskell-realworld-example.git

    cd haskell-realworld-example

Create and deploy the NixOps config

    nixops create ./ops.nix -d realworld

    nixops deploy -d realworld --force-reboot

Find out what the IP Address of the virtual machine is

    nixops info -d
