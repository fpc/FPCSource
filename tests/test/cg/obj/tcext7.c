struct CTest
{
  unsigned long a;
  unsigned long b;
  unsigned long c;
  unsigned long d;
} __attribute__((aligned(16)));

int TestFunc(unsigned long arg1, unsigned long arg2, unsigned long arg3, unsigned long arg4,
             unsigned long arg5, unsigned long arg6, unsigned long arg7, struct CTest arg8,
             unsigned long arg9, struct CTest arg10)
{
  if (arg1 != 1)
    return 1;
  if (arg2 != 2)
    return 2;
  if (arg3 != 3)
    return 3;
  if (arg4 != 4)
    return 4;
  if (arg5 != 5)
    return 5;
  if (arg6 != 6)
    return 6;
  if (arg7 != 7)
    return 7;
  if (arg8.a != 801)
    return 801;
  if (arg8.b != 802)
    return 802;
  if (arg8.c != 803)
    return 803;
  if (arg8.d != 804)
    return 804;
  if (arg9 != 9)
    return 9;
  if (arg10.a != 1001)
    return 1001;
  if (arg10.b != 1002)
    return 1002;
  if (arg10.c != 1003)
    return 1003;
  if (arg10.d != 1004)
    return 1004;
  return 0;
}
