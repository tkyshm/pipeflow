pipeflow
=====

The Pipeflow pull message from cloud pubsub and export to gcs storage.

Build
-----

    $ rebar3 compile


## Environment variables

| name | --- |
| ---- | --- |
| PIPEFLOW_SUBSCRIPTION | cloud pubsub subscription                             |
| GCP_PROJECT_ID        | gcp project id                                        |
| PIPEFLOW_SUBSCRIPTION | exporter module name. default: pipeflow_gcs_exporter. |

## Config (sys.config)

- pipeflow_gcs_exporter

```
[
  {pipeflow, [{exporter_conf, #{bucket => <<"pipeflow-test">>, log_prefix => <<"logs/pipeflow">>}}]}
].
```
