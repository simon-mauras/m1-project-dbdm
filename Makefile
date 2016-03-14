CC := ocamlbuild
SRC := src
CFLAGS := -I $(SRC)

.PHONY: all mproper

all: main

main:
	$(CC) $(CFLAGS) $(SRC)/$@.byte
	mv $@.byte tinySQL

clean:
	$(CC) -clean
