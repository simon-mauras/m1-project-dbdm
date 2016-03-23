CC := ocamlbuild
SRC := src
CFLAGS := -I $(SRC) -use-ocamlfind -pkg sqlite3

.PHONY: all mproper

all: main database

database:
	sqlite3 test/example-database.db < test/example-schema.sql
	sqlite3 test/test-database.db < test/test-schema.sql

main:
	$(CC) $(CFLAGS) $(SRC)/$@.byte
	mv $@.byte tinySQL

clean:
	$(CC) -clean
