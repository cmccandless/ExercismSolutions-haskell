.PHONY: lint test
lint:
	@hlint .

test:
	@ $(foreach FILE,$(FILES), \
		$(call dotest,$(FILE)) \
	)

test-all:
	@ $(foreach FILE,$(shell ls -d */), \
		$(call dotest, $(FILE)) \
	)

define dotest
	cd $(1); \
	stack test $(OPTS) || exit 1; \
	cd ..;
endef
