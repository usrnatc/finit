/**
 * file_init.c
 * Nathan Corcoran, 2022
 *
 * boilerplate code is the bane of my sad existence
 */

#define _GNU_SOURCE
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

//TODO: find the optimal way to store these (hashmap ?)
#define PYTHON "def main():\n\tprint(\"Hello, World!\")\n\nif __name__ == \"__main__\":\n\tmain()"
#define HASKELL "main :: IO ()\nmain =\n  do\n    print(\"Hello, World!\")"
#define C "#include <stdio.h>\n\nint main(void) {\n\n\treturn 0;\n}"
#define GO "package main\nimport \"fmt\"\n\nfunc main() {\n\tfmt.Println(\"Hello, World!\"\n}"
#define JAVA "class Code {\n\n\tpublic static void main(String args[]) {\n\n\tSystem.out.println(\"Hello, World!\");\n}"
#define HARE "use fmt;\n\nexport fn main() void = {\n\n\tfmt::println(\"Hello, World!\")!;\n};"
#define HTML "<!DOCTYPE html>\n<html>\n\t<head>\n\t</head>\n\t<body>\n\tHello, World!\n\t</body>\n</html>"
#define MAKE "CC=clang\nCFLAGS=-Wall -Werror -pedantic -O3 -g -std=gnu99\n.PHONY:clean\n#: #.c ; $(CC) $(CFLAGS) #.c -o #\n\nclean: ; rm -rf #"

#define ARG_NO 3
#define MAX_SZ 8

FILE *open_file(char *filepath);
void file_type_mutex(char *filetype, char *filepath);
void init_file(char *filepath, char *boilerplate);
void usage(void);
void check_args(int argc, char **argv);

FILE *open_file(char *filepath) {

    if (!filepath || !strlen(filepath)) return NULL;

    FILE *file = fopen(filepath, "w");

    return file;
}

// TODO: strncmp probably isn't safe 
void file_type_mutex(char *filetype, char *filepath) {

    char *boilerplate = NULL;

    if (!strncmp(filetype, "py", MAX_SZ)) {

        asprintf(&boilerplate, "%s", PYTHON);
    } else if (!strncmp(filetype, "haskell", MAX_SZ)) {

        asprintf(&boilerplate, "%s", HASKELL);
    } else if (!strncmp(filetype, "c", MAX_SZ)) {
     
        asprintf(&boilerplate, "%s", C);
    } else if (!strncmp(filetype, "go", MAX_SZ)) {

        asprintf(&boilerplate, "%s", GO);
    } else if (!strncmp(filetype, "java", MAX_SZ)) {

        asprintf(&boilerplate, "%s", JAVA);
    } else if (!strncmp(filetype, "hare", MAX_SZ)) {

        asprintf(&boilerplate, "%s", HARE);
    } else if (!strncmp(filetype, "html", MAX_SZ)) {

        asprintf(&boilerplate, HTML);
    } else if (!strncmp(filetype, "make", MAX_SZ)) {

        asprintf(&boilerplate, "%s", MAKE);
    } else {

        fprintf(stderr, "ERROR :: unknown filetype \"%s\"\n", filetype);
        if (boilerplate) free(boilerplate); // unreachable

        exit(-4);
    }

    init_file(filepath, boilerplate);
}

void init_file(char *filepath, char *boilerplate) {

    FILE *file = open_file(filepath);

    if (!file) {

        fprintf(stderr, "ERROR :: unable to open file \"%s\"\n", filepath);
        fclose(file);

        exit(-2);
    }

    if (!boilerplate) {

        fprintf(stderr, "ERROR :: boilerplate bug\n");
        fclose(file);

        exit(-3);
    }

    fprintf(file, "%s", boilerplate);
    fclose(file);
    free(boilerplate);
}

void usage(void) {

    fprintf(stderr, "Usage: ./finit filepath filetype\n");

    exit(-1);
}

void check_args(int argc, char **argv) {

    if (argc != ARG_NO) {

        usage();
    }
}

int main(int argc, char **argv) {

    check_args(argc, argv);

    file_type_mutex(argv[2], argv[1]);

    return 0;
}
