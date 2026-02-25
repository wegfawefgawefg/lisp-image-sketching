SBCL ?= sbcl
IMAGE ?= all
DOWNSAMPLE ?= 2

.PHONY: setup run build clean

setup:
	mkdir -p outputs build

run: setup
	$(SBCL) --script scripts/generate.lisp $(IMAGE) $(DOWNSAMPLE)

build: setup
	$(SBCL) --script scripts/build.lisp

clean:
	rm -f outputs/*.png outputs/*.ppm build/*.fasl
