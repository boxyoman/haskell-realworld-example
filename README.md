# ![RealWorld Example App](logo.png)

> ### [Haskell/Servant/Beam] codebase containing real world examples (CRUD, auth, advanced patterns, etc) that adheres to the [RealWorld](https://github.com/gothinkster/realworld) spec and API.


### [Demo](https://github.com/gothinkster/realworld)&nbsp;&nbsp;&nbsp;&nbsp;[RealWorld](https://github.com/gothinkster/realworld)


This codebase was created to demonstrate a fully fledged fullstack application built with **[Haskell/Servant/Beam]** including CRUD operations, authentication, routing, pagination, and more.

We've gone to great lengths to adhere to the **[Haskell/Servant/Beam]** community styleguides & best practices.

For more information on how to this works with other frontends/backends, head over to the [RealWorld](https://github.com/gothinkster/realworld) repo.


# Getting started

## Installation

Install [Nix](https://nixos.org/nix/). If you're on a Mac follow
[these instructions](https://medium.com/@zw3rk/provisioning-a-nixos-server-from-macos-d36055afc4ad)
to setup NixOps, otherwise

    nix-env -i nixops

You'll also need [VirtualBox](https://www.virtualbox.org). You'll need to do
one weird config change in VirtualBox

    VBoxManage hostonlyif create

Clone the repository and move into it

    git clone https://github.com/boxyoman/haskell-realworld-example.git

    cd haskell-realworld-example

Create and deploy the NixOps config

    nixops create ./ops.nix -d realworld

    nixops deploy -d realworld --force-reboot

Find out what the IP Address of the virtual machine is

    nixops info -d

View logs of the server

    nixops ssh -d realworld backend

    journalctl -u api.service -f

## Making changes

Make your change and get into a Nix Shell to compile

    nix-shell shell.nix

    cabal configure

    cabal build

Deploying changes to the VM

    nixops deploy -d realworld

# How it works

[Servant](https://haskell-servant.readthedocs.io/en/stable/) is used for the
routing. The source code for all the routing and handling can be found in
`src/Api.hs`.

[Beam](http://hackage.haskell.org/package/beam-core) is used for generating
queries to the database. The code for handling database requests is found in
`src/Database.hs`.

Types that are common to both modules are found in `src/Types.hs`. There are
lots of `newtypes` to make it harder to mix up arguments when passing them
around to different functions.

Authentication is done in `src/Lib.hs`. It's not very pretty, but it gets the
job done.

The `main` function is in `app/Main.hs`, it setups up the webserver on port
8080, CORS to accept data from any host, database migrations, and simple logging
for the webserver.
