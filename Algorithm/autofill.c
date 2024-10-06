#include "stdio.h"
#include "stdlib.h"
#include "unistd.h"
#include "autofill.h"

/*
 * Autofill a crosswords grid
 */
int main(int argc, char **argv) {

    /* Parse arguments */
    int opt = -1;;
    char *dictionary = "dictionary.txt";
    while((opt = getopt(argc, argv, "hd:")) != -1) {
        switch (opt) {
            case 'd':
                dictionary = optarg;
                break;
            case 'h':
            default:
                fprintf(stderr, "Usage: %s [-h] [-d dictionary] [grid]\n",
                    argv[0]);
                return 1;
        }
    }

    /* Grid is the last argument or stdin */
    char *input = "-";
    if(optind < argc) {
        input = argv[argc-1];
    }

    printf("Hello crosswords, using dictionary = [%s] and input = [%s]\n",
        dictionary, input);
    autofill(dictionary, input);
    return 0;
}

/*
 * Parse a dictionary file, use it to autofill the grid file
 */
void autofill(const char *dictfile, const char *gridfile) {
    printf("DBG1\n");
    read_dictionary_file(dictfile);
    printf("DBG3\n");
}

void read_dictionary_file(const char *dictfile) {
    FILE *file = fopen(dictfile,"rt");
    if(NULL == file) {
        fprintf(stderr, "Dictionary file = [%s] could not be opened", dictfile);
        exit(1);
    }
    char *line = NULL;
    size_t length = 0;
    ssize_t read = 0;
    while((read = getline(&line, &length, file)) != -1) {

    }
    fclose(file);
}

