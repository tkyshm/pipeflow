FROM erlang:23.1.1 AS build

ADD . /pipeflow
WORKDIR /pipeflow

RUN rebar3 as prod release

FROM erlang:23.1.1

COPY --from=build /pipeflow/_build/prod/rel/pipeflow/ /pipeflow

ENTRYPOINT ["/pipeflow/bin/pipeflow"]
