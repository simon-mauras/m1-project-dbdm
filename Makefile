CC := ocamlbuild
SRC := src
CFLAGS := -I $(SRC)

.PHONY: all mproper

all: main

database:
	sqlite3 test/example-database.db < test/example-schema.sql

main:
	$(CC) $(CFLAGS) $(SRC)/$@.byte
	mv $@.byte tinySQL

clean:
	$(CC) -clean
