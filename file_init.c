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
#include <unistd.h>
#include <stdint.h>
#include <time.h>

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

/* global commandline flags */
uint8_t verbose,
        project;

/* current system time for logging */
time_t systemTime;

FILE *open_file(char *filepath);
void file_type_multiplexer(char *filetype, char *filepath);
void init_file(char *filepath, char *boilerplate);
void usage(void);
void check_args(int argc, char **argv);

/*
 * Logs the given message and current system time to stderr
 */
void errlog(const char *message) {

    time(&systemTime);
    char *charTime = ctime(&systemTime);
    charTime[strlen(charTime) - 1] = '\0';

    fprintf(stderr, "[%s] :: %s\n", charTime, message);
}

/*
 * Open a file at the given filepath and return its stream.
 *
 * Returns the address of the newly opened file
 *
 * Note: returns NULL on error
 */
FILE *open_file(char *filepath) {

    if (verbose) errlog("Creating file");

    if (!filepath || !strlen(filepath)) return NULL;

    FILE *file = fopen(filepath, "w");

    return file;
}

/*
 * Boilerplate code language multiplexer
 */
void file_type_multiplexer(char *filetype, char *filepath) {

    if (verbose) errlog("Determining filetype");

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

/*
 * Opens a file stream at the given filepath and writes the given 
 * boilerplate code to it.
 *
 * Note: on error, error message is errlogged to stderr and the
 * process is terminated
 */
void init_file(char *filepath, char *boilerplate) {

    if (verbose) errlog("Writing boilerplate to file");

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

/*
 * Usage error handling
 */
void usage(void) {

    fprintf(stderr, "Usage: ./finit [-h | [-v] [-p] filepath filetype]\n");

    exit(-1);
}

void help(void) {

    fprintf(stderr,
            "finit\t-\tAutomatically initialise files and small projects\n\n"
            "./finit [-h | [-v] [-p] filepath filetype]\n\n"
            "-h:\n"
            "\tdisplays this help message and terminates the program.\n\n"
            "-v:\n"
            "\tdisplays verbose log messages during program runtime.\n\n"
            "-p:\n"
            "\tinitialise a small project instead of a single file. When this\n"
            "\tflag is set, filepath is used as the main file of the\n"
            "\tcreated project, \"src\" and \"lib\" directories are made (empty).\n\n"
            "\tIf any of the following languages are set when initialising the\n"
            "\tsmall project, a makefile is also created and will automatically\n"
            "\tcompile and link files in the \"src\" and \"lib\" directories.\n"
    );

    exit(0);    // exit normally
}

/*
 * Checks given commandline arguments
 */
void check_args(int argc, char **argv) {
 
    int stop;
    while (stop = getopt(argc, argv, "pvh"), stop != -1) {

        switch (stop) {

            case 'h':

                help();
                break;
            case 'v':

                verbose = 1;
                break;
            case 'p':

                project = 1;
                break;
            default:

                usage();
        }
    }

    if (argc - optind < 2) usage();
}

int main(int argc, char **argv) {

    check_args(argc, argv);

    file_type_multiplexer(argv[optind + 1], argv[optind]);

    return 0;
}
