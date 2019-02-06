SLOW = alphametics prime-factors
EXTENSION := hs
SOURCE_FILES := $(shell find * -type f -name "*.$(EXTENSION)" | grep -v '.stack-work')
EXERCISES := $(shell echo $(SOURCE_FILES) | tr ' ' '\n' | cut -d'/' -f1 | uniq)
EXERCISES_SLOW := $(filter $(SLOW), $(EXERCISES))
EXERCISES_FAST := $(filter-out $(SLOW), $(EXERCISES))

.PHONY: all clean lint test-all $(EXERCISES)

all: lint test-all

lint:
	@hlint .

test-all: test-fast test-slow

test-fast: $(EXERCISES_FAST)

test-slow: $(EXERCISES_SLOW)

$(EXERCISES):
	@cd $@ && (stack test $(OPTS) || exit 1)
