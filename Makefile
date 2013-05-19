DC = gdc
DFLAGS = -Isrc -Llib
FINAL_FLAGS = -frelease -O2
TEST_FLAGS = -funittest -O0

all: bin/templatizer

check: test/main
	time test/main

clean:
	rm -f bin/templatizer bin/*.o
	rm -f test/main test/*.o
	rm -f lib/libdx.a lib/*.o

FINAL_OBJS = bin/main.o bin/binding.o bin/token.o bin/parser.o bin/render.o
bin/templatizer: $(FINAL_OBJS) lib/libdx.a
	$(DC) $(DFLAGS) $(FINAL_FLAGS) $(FINAL_OBJS) -ldx -o $@

TEST_OBJS = test/main.o test/binding.o test/token.o test/parser.o test/render.o
test/main: $(TEST_OBJS) lib/libdx.a
	$(DC) $(DFLAGS) $(TEST_FLAGS) $(TEST_OBJS) -ldx -o $@

# Libraries #
lib/%.o: dx/%.d
	$(DC) -c $(DFLAGS) $(FINAL_FLAGS) $^ -o $@
lib/libdx.a: lib/string.o
	rm -f $@
	ar -qc $@ $^

test/main.o: dx/testing.d
	$(DC) -c $(DFLAGS) $(TEST_FLAGS) $< -o $@

# Release Object Files #
bin/%.o: src/%.d lib/libdx.a
	$(DC) -c $(DFLAGS) $(FINAL_FLAGS) $< -o $@

BINDING_SOURCES = src/binding.d src/persistance.d
bin/binding.o: $(BINDING_SOURCES) lib/libdx.a
	$(DC) -c $(DFLAGS) $(FINAL_FLAGS) $(BINDING_SOURCES) -o $@

# Test Object Files #
test/%.o: src/%.d lib/libdx.a
	$(DC) -c $(DFLAGS) $(TEST_FLAGS) $< -o $@

test/binding.o: $(BINDING_SOURCES) lib/libdx.a
	$(DC) -c $(DFLAGS) $(TEST_FLAGS) $(BINDING_SOURCES) -o $@

# Dependencies #
src/main.d: src/binding.d src/persistance.d src/parser.d
	@touch $@
src/parser.d: src/binding.d src/token.d src/render.d
	@touch $@
src/render.d: src/binding.d src/token.d
	@touch $@
src/token.d: src/binding.d
	@touch $@