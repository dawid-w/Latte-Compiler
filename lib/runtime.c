// @Author - bartoszwjn
// https://github.com/bartoszwjn/lattec/blob/master/lib/runtime.c

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

void printInt(int n)
{
    printf("%d\n", n);
}

void printString(const char *str)
{
    printf("%s\n", str);
}

int readInt()
{
    int ret;
    char *input = NULL;
    size_t len = 0;
    getline(&input, &len, stdin);
    sscanf(input, "%d", &ret);
    return ret;
}

char *readString()
{
    char *ret = NULL;
    size_t len = 0;
    int read = getline(&ret, &len, stdin);
    if (read > 0 && ret[read - 1] == '\n')
    {
        ret[read - 1] = '\0';
    }
    return ret;
}

char *concat(const char *str1, const char *str2)
{
    size_t len1 = strlen(str1);
    size_t len2 = strlen(str2);
    char *ret = malloc(len1 + len2 + 1);
    strncpy(ret, str1, len1 + 1);
    strncpy(ret + len1, str2, len2 + 1);
    return ret;
}