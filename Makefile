DC = gdc -Wall
DFLAGS = -Isrc -O2
TEST_FLAGS = -funittest -O0

all: bin/templatizer

check: test/main
	test/main

clean:
	rm -f bin/templatizer bin/*.o
	rm -f test/main

bin/templatizer: src/main.d
	$(DC) $(DFLAGS) $^ -o $@

test/main: src/main.d dlangutil/testing.d
	$(DC) $(DFLAGS) $(TEST_FLAGS) $^ -o $@