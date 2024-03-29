# LifeInParentheses

[![Build status](https://github.com/belarte/LifeInParentheses/actions/workflows/build.yml/badge.svg?branch=main)](https://github.com/belarte/LifeInParentheses/actions/workflows/build.yml)
[![Build frontend](https://github.com/belarte/LifeInParentheses/actions/workflows/build-frontend.yml/badge.svg?branch=main)](https://github.com/belarte/LifeInParentheses/actions/workflows/build-frontend.yml)
[![System tests](https://github.com/belarte/LifeInParentheses/actions/workflows/tests.yml/badge.svg?branch=main)](https://github.com/belarte/LifeInParentheses/actions/workflows/tests.yml)

Another go at my life calculator

## Useful commands

### Backend

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

### Frontend

The frontend has been developed with NeoVim and the [Conjure](https://github.com/Olical/conjure) plugin.
It leverages [shadow-cljs](https://github.com/thheller/shadow-cljs) for hot reloading of the code and nREPL connection.

To start the front end in developer mode, run:

```shell
npx shadow-cljs watch app
```

For Conjure to hook into the nREPL created by shadow-cljs, run the following in NeoVim:

`:ConjureShadowSelect app`

### nginx

The app uses [nginx](https://www.nginx.com/) as a proxy. To start the server locally, run:

```shell
nginx -g "daemon off;" -c <path to nginx.conf> -p <path to public folder>
```

It needs the configuration file for the app (committed to the project)
and the path to the `public` folder (in the `frontend` folder).
Also, nginx expects the backend to run on port `5000` and the frontend on port `8080`.

From there, you can access both the frontend and the backend from port `80`.

### End to end tests

End-to-end tests have been written with [Babashka](https://github.com/babashka/babashka).
To run the tests, from the `tests` folder:

```shell
bb run-tests [path-to-backend-binary] [options]
```

Options notably include `--port` and `--no-startup`. Look at the help for more information.
