.PHONY: run
run:
	swipl start.pl $(MOD)

.PHONY: test
test:
	swipl test.pl $(MOD)

.PHONY: docker-run
docker-run:
	docker run --rm -it -v ${PWD}:${PWD} -w ${PWD} swipl swipl start.pl $(MOD)

.PHONY: docker-test
docker-test:
	docker run --rm -it -v ${PWD}:${PWD} -w ${PWD} swipl swipl test.pl $(MOD)