# Othrottle

[![builds.sr.ht status](https://builds.sr.ht/~gary-kim/othrottle/commits/master/build.yaml.svg)](https://builds.sr.ht/~gary-kim/othrottle/commits/master/build.yaml?)

* Project Home: [https://sr.ht/~gary-kim/othrottle](https://sr.ht/~gary-kim/othrottle)
* Source Code: [https://git.sr.ht/~gary-kim/othrottle](https://git.sr.ht/~gary-kim/othrottle)
* Development Mailing List: [https://lists.sr.ht/~gary-kim/public-inbox](https://lists.sr.ht/~gary-kim/public-inbox) ([~gary-kim/public-inbox@lists.sr.ht](mailto:~gary-kim/public-inbox@lists.sr.ht))
* TODO: [https://todo.sr.ht/~gary-kim/othrottle](https://todo.sr.ht/~gary-kim/othrottle)

Inspired by [~ferdinandyb/throttle](https://sr.ht/~ferdinandyb/throttle/)

### Building

Dependencies: `opam`, `gcc-c++` (for re2)

``` bash
opam switch create . --deps-only -y
eval $(opam env)
make othrottle
```

### Config

A config file can be placed at `$XDG_CONFIG_DIR/othrottle/config.toml` (usually
`~/.config/othrottle/config.toml`) or specified with the `--config` flag when
running `othrottle server`.

**Example Config:**

```toml
job_timeout = 600
task_timeout = 300
shell = "bash"
notification_cmd = "notify-send 'othrottle error' \"$JOB\""
notify_on_counter = 2
retry_sequence = [5, 15, 30, 60, 120, 300, 900]

[[filters]]
pattern = "^mbsync 'gary.kim@cooper.edu:Sent'$"
substitute = "mbsync 'gary.kim@cooper.edu:Sent Items'"
```

### License

Licensed under [AGPL-3.0-or-later](./LICENSE).

