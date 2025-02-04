# ![RealWorld Example App](logo.png)

> ### [Haskell/Servant/Beam] codebase containing real world examples (CRUD, auth, advanced patterns, etc) that adheres to the [RealWorld](https://github.com/gothinkster/realworld) spec and API.


### [Demo](https://github.com/gothinkster/realworld)&nbsp;&nbsp;&nbsp;&nbsp;[RealWorld](https://github.com/gothinkster/realworld)


This codebase was created to demonstrate a fully fledged fullstack application
built with **[Haskell/Servant/Beam]** including CRUD operations, authentication,
routing, pagination, and more.

We've gone to great lengths to adhere to the **[Haskell/Servant/Beam]**
community styleguides & best practices.

For more information on how to this works with other frontends/backends, head
over to the [RealWorld](https://github.com/gothinkster/realworld) repo.


# Getting started

## Running

1. Install [Nix](https://nixos.org/nix/).

2. Run `nix-shell`

3. Run `devenv up` inside the nix-shell.

The server should now be running on port 8080.

For haskell development [hoogle](https://wiki.haskell.org/index.php?title=Hoogle)
should be running on port 8081, just open a browser to
[http://localhost:8081/](http://localhost:8081/) to use it.


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
