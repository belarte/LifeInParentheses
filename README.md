# LifeInParentheses

[![Build status](https://github.com/belarte/LifeInParentheses/actions/workflows/build.yml/badge.svg?branch=main)](https://github.com/belarte/LifeInParentheses/actions/workflows/build.yml)
[![System tests](https://github.com/belarte/LifeInParentheses/actions/workflows/tests.yml/badge.svg?branch=main)](https://github.com/belarte/LifeInParentheses/actions/workflows/tests.yml)

Another go at my life calculator

## Useful commands

The backend has been developed with NeoVim and the [Conjure](https://github.com/Olical/conjure) plugin.
To start an nREPL that Conjure will recognise and automatically connect to, use:

```shell
clojure -M:repl
```

The unit tests use [Kaocha](https://github.com/lambdaisland/kaocha) test runner. To focus on a given test package, use `--focus`.
To start the test executable in watch mode, use `--watch`.
To run the unit tests, from the `backend` folder:

```shell
clojure -M:test [--focus <test package>] [--watch]
```

To build the backend as an executable JAR, from the `backend` folder:

```shell
clojure -T:build uber
```

End-to-end tests have been written with [Babashka](https://github.com/babashka/babashka).
To run the tests, from the `tests` folder:

```shell
bb run-tests [path-to-backend-binary] [options]
```

Options notably include `--port` and `--no-startup`. Look at the help for more information.
