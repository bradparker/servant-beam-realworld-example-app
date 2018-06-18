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

  Along with GHC 8.2.2 and Cabal 2.0.0.1 you'll also get HLint, HIndent and GHCId.

2. Run the test suite.

  ```
  $ cabal test
  ```

3. Run the app.

  ```
  $ cabal run
  ```
