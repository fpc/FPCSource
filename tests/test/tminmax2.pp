{ %opt=-Oonofastmath -O3 }  { with fast math, the operands of min/max might be swapped and this breaks the tests using NaN,
                              but test constant propagation and thus simplification }
{$I minmax.inc }