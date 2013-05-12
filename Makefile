DC = gdc
DFLAGS = -Isrc -O2
TEST_FLAGS = -funittest -O0

all: bin/templatizer

check: test/main
	time test/main

clean:
	rm -f bin/templatizer bin/*.o
	rm -f test/main

bin/templatizer: src/main.d
	$(DC) $(DFLAGS) $^ -o $@

test/main: src/main.d dx/*.d
	time $(DC) $(DFLAGS) $(TEST_FLAGS) $^ -o $@