CC = clang
CFLAGS = -Wall -Werror -pedantic -O3 -std=gnu99 -g

.PHONY: clean

finit: file_init.c ; $(CC) $(CFLAGS) file_init.c -o finit

clean: ; rm -rf finit test.*
