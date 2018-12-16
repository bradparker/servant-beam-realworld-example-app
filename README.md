# ![RealWorld Example App](logo.png)

[![Build Status](https://travis-ci.org/bradparker/servant-beam-realworld-example-app.svg?branch=master)](https://travis-ci.org/bradparker/servant-beam-realworld-example-app)

> ### A Servant and Beam codebase containing real world examples (CRUD, auth, advanced patterns, etc) that adheres to the [RealWorld](https://github.com/gothinkster/realworld) spec and API.

### [Demo](https://servant-beam-realworld.herokuapp.com/swagger/) [RealWorld](https://github.com/gothinkster/realworld)

This codebase was created to demonstrate a fully fledged fullstack application built with **Servant and Beam** including CRUD operations, authentication, routing, pagination, and more.

We've gone to great lengths to adhere to the **Haskell** community styleguides & best practices.

For more information on how to this works with other frontends/backends, head over to the [RealWorld](https://github.com/gothinkster/realworld) repo.

# Status

With the caveat that the Authorization header format is _slightly_ different, this is done:

```
┌─────────────────────────┬──────────┬──────────┐
│                         │ executed │   failed │
├─────────────────────────┼──────────┼──────────┤
│              iterations │        1 │        0 │
├─────────────────────────┼──────────┼──────────┤
│                requests │       31 │        0 │
├─────────────────────────┼──────────┼──────────┤
│            test-scripts │       46 │        0 │
├─────────────────────────┼──────────┼──────────┤
│      prerequest-scripts │       17 │        0 │
├─────────────────────────┼──────────┼──────────┤
│              assertions │      280 │        0 │
├─────────────────────────┴──────────┴──────────┤
│ total run duration: 18s                       │
├───────────────────────────────────────────────┤
│ total data received: 5.77KB (approx)          │
├───────────────────────────────────────────────┤
│ average response time: 25ms                   │
└───────────────────────────────────────────────┘
```

## TODO

* I'm not thrilled by that average response time, this is over localhost ... working with _quite_ empty tables.

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
  $ dev/watch-tests
  ```

  Which might look something like this:

  [![asciicast](https://asciinema.org/a/YhNioSPZ4SKkHTpLxO7lzTogx.png)](https://asciinema.org/a/YhNioSPZ4SKkHTpLxO7lzTogx)
