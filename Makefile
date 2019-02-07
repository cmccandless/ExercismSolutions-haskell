SLOW = alphametics prime-factors
EXTENSION := hs
SOURCE_FILES := $(shell find * -type f -name "*.$(EXTENSION)" | grep -v '.stack-work')
EXERCISES := $(shell echo $(SOURCE_FILES) | tr ' ' '\n' | cut -d'/' -f1 | uniq)
EXERCISES_SLOW := $(filter $(SLOW), $(EXERCISES))
EXERCISES_FAST := $(filter-out $(SLOW), $(EXERCISES))
OUT_DIR=.build
OBJECTS=$(addprefix $(OUT_DIR)/,$(EXERCISES))

.PHONY: all clean lint test-all

all: lint test-all

lint:
	@hlint .

test-all: test-fast test-slow

test-fast: $(EXERCISES_FAST)

test-slow: $(EXERCISES_SLOW)

$(EXERCISES): %: $(OUT_DIR)/% 

$(OUT_DIR):
	@ mkdir -p $@

clean:
	rm -rf $(OUT_DIR)
	rm -rf ./*/.stack-work/

.SECONDEXPANSION:

GET_DEP = $(filter $(patsubst $(OUT_DIR)/%,%,$@)%,$(SOURCE_FILES))
$(OBJECTS): $$(GET_DEP) | $(OUT_DIR)
	$(eval EXERCISE := $(patsubst $(OUT_DIR)/%,%,$@))
	@ echo "Testing $(EXERCISE)..."
	@ echo "$@, $^"
	@ cd $(EXERCISE) && stack test $(OPTS)
	@ touch $@
