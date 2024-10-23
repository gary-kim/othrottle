Check help:
  $ othrottle help
  A job runner for asynchronous sequential commands
  
    othrottle SUBCOMMAND
  
  === subcommands ===
  
    config                     . Get server configuration
    kill                       . Kill a currently running job
    run                        . Add a job
    server                     . Run server daemon
    status                     . Get current job status
    version                    . print version information
    help                       . explain a given subcommand (perhaps recursively)
  


Check default config:
  $ bash -c 'set -m && touch ./empty.toml && othrottle server -c ./empty.toml -s ./othrottle.sock 2> /dev/null & sleep 1 && othrottle config -s ./othrottle.sock; kill %1'
  ((shell bash) (job_timeout 600) (task_timeout 30)
   (retry_sequence (5 15 30 60 120 300 900)) (filters ()))

