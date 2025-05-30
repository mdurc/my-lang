all:
	cmake -S . -B build
	$(MAKE) -C build

clean:
	rm -rf build

test: clean all
	./build/test_runner

build:
	$(MAKE) -C build

quick_test: build
	./build/test_runner

.PHONY: all clean test
