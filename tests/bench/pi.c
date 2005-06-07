#include <stdlib.h>
#include <stdio.h>

void ComputePi(int numdigits, char* pi)
{
    int alength = 10 * numdigits / 3;
    int* a = (int*) malloc(alength * sizeof(int));
    int piLength = 0;
    int nines = 0;
    int predigit = 0;
    int i, j;
    for(i = 0; i < alength; ++i)
        a[i] = 2;

    for (j = 0; j < numdigits; ++j)
    {
        int q = 0;
        int p = 2 * alength - 1;
        for (i = alength; --i >= 0; )
        {
            int x = 10*a[i] + q*(i+1);
            a[i] = x % p;
            q = x / p;
            p -= 2;
        }

        a[0] = q % 10;
        q /= 10;
        if (q == 9)
            ++nines;
        else if (q == 10)
        {
            int k;
            pi[piLength] = (char) (predigit + 1 + '0');
            for (k = 1; k <= nines; ++k)
                pi[piLength+k] = '0';
            piLength += nines + 1;
            predigit = 0;
            nines = 0;
        }
        else
        {
            int k;
            pi[piLength] = (char)(predigit + '0');
            predigit = q;
            for (k = 1; k <= nines; ++k)
                pi[piLength + k] = '9';
            piLength += nines + 1;
            nines = 0;
        }
    }
    pi[piLength] = (char)(predigit + '0');
    pi[piLength+1] = '\0';

    free(a);
}

int main(int argc, char** argv)
{
    int numdigits;
    char* pi;

    if (argc <= 1)
    {
        fprintf(stderr, "usage: pi #DIGITS [FILE]");
        return 1;
    }

    numdigits = atoi(argv[1]);
    pi = (char*) malloc(numdigits+1);
    ComputePi(numdigits, pi);

    if (argc > 2)
    {
        FILE* fp = fopen(argv[2], "w");
        if (fp == NULL)
        {
           fprintf(stderr, "Cannot open %s\n", argv[2]);
           return 2;
        }
        fputs(pi, fp);
        fputc('\n', fp);
        fclose(fp);
    }
    else
        puts(pi);

    free(pi);

    return 0;
}
