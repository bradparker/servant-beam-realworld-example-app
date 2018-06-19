# ![RealWorld Example App](logo.png)

> ### A Servant and Beam codebase containing real world examples (CRUD, auth, advanced patterns, etc) that adheres to the [RealWorld](https://github.com/gothinkster/realworld) spec and API.

### [Demo](https://github.com/gothinkster/realworld) [RealWorld](https://github.com/gothinkster/realworld)

This codebase was created to demonstrate a fully fledged fullstack application built with **Servant and Beam** including CRUD operations, authentication, routing, pagination, and more.

We've gone to great lengths to adhere to the **Haskell** community styleguides & best practices.

For more information on how to this works with other frontends/backends, head over to the [RealWorld](https://github.com/gothinkster/realworld) repo.

# Getting started

1. Use Nix to get all the Haskell package and application dependencies you need.

  ```
  $ nix-shell
  ```

2. Create .envrc from example and allow contents

  ```
  $ cp .envrc.example .envrc
  $ direnv allow
  ```

3. Setup the database.

  ```
  $ database/scripts/setup
  ```

4. Run the test suite.

  ```
  $ cabal test
  ```

  You can run these in a watch mode using ghcid:

  ```
  $ ghcid --command 'cabal repl test:spec' --test=Main.main
  ```

5. Run the app.

  ```
  $ cabal run
  ```
