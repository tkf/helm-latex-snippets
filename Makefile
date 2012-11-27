.PHONY: build clean

build:
	python generate.py

clean:
	rm -rf build
