/*
   Test header file to test conversion program.
*/

typedef struct {
  int x;
  int y;
  } a;

typedef union fpk {
  int X;
  int y;
  int z;
} b;

typedef _test test;

struct _test
{
  int x;
  int y;
};

void proc(int *,int);
void proc(int *p,int i);

typedef enum { First, second, third } C;

typedef enum { DFirst = 1, DSecond = 2, DThird = 3 } D;

typedef enum { EFirst = 100, ESecond, EThird } D;

void someproc(char *Firstarg,...);

mytype* somefunc (char *firstarg);

#define test 0x012345UL

extern long long i641;
extern unsigned long long q641;
extern long long int i642;
extern unsigned long long int q642;
