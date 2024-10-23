# Othrottle

* Project Home: [https://sr.ht/~gary-kim/othrottle](https://sr.ht/~gary-kim/othrottle)
* Source Code: [https://git.sr.ht/~gary-kim/othrottle](https://git.sr.ht/~gary-kim/othrottle)
* Development Mailing List: [https://lists.sr.ht/~gary-kim/public-inbox](https://lists.sr.ht/~gary-kim/public-inbox) ([~gary-kim/public-inbox@lists.sr.ht](mailto:~gary-kim/public-inbox@lists.sr.ht))
* TODO: [https://todo.sr.ht/~gary-kim/othrottle](https://todo.sr.ht/~gary-kim/othrottle)

Inspired by [~ferdinandyb/throttle](https://sr.ht/~ferdinandyb/throttle/)

### Building

Dependencies: `opam`, `gcc-c++` (for re2)

``` bash
opam switch create . ocaml-base-compiler.5.2.0 --deps-only -y
eval $(opam env)
make othrottle
```

### License

Licensed under [AGPL-3.0-or-later](./LICENSE).

