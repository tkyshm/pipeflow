.PHONY: elvis test dialyzer build/image

TAG ?= $(shell git log --format=%h -n 1)
APP_NAME := pipeflow

export GCP_PROJECT_ID ?= default
export PIPEFLOW_SUBSCRIPTION ?= test

start:
	rebar3 auto --apps pipeflow --config ./config/sys.config

elvis:
	elvis --config elvis.config

dialyzer:
	rebar3 dialyzer

test: elvis dialyzer
	PUBSUB_ENDPOINT=http://localhost:8001/v1/ GCP_PROJECT_ID=test PIPEFLOW_SUBSCRIPTION=test PIPEFLOW_EXPORTER=test_exporter rebar3 as test ct -v --config config/test.config

build/image:
	docker build -f Dockerfile -t $(APP_NAME):$(TAG) .
