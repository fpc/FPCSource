{%TARGET=linux,darwin,go32v2}
{ Test the C++ name mangling for different parameter combinations }
program tcppclass1;

{$mode objfpc}
{$L cpptcl1.o}

type
  TestClass = cppclass external
    class procedure Test1;
    { boolean }
    class procedure Test2(aArg1: Boolean);
    { unsigned ordinals }
    class procedure Test3(aArg1: Byte);
    class procedure Test4(aArg1: Word);
    class procedure Test5(aArg1: LongWord);
    class procedure Test6(aArg1: QWord);
    { signed ordinals }
    class procedure Test7(aArg1: ShortInt);
    class procedure Test8(aArg1: SmallInt);
    class procedure Test9(aArg1: LongInt);
    class procedure Test10(aArg1: Int64);
    { floating point }
    class procedure Test11(aArg1: Single);
    class procedure Test12(aArg1: Double);
    { chars }
    class procedure Test13(aArg1: Char);
    class procedure Test14(aArg1: WideChar);
    { pointers }
    class procedure Test15(aArg1: Pointer);
    class procedure Test16(aArg1: PChar);
    class procedure Test17(aArg1: PWideChar);
    class procedure Test18(aArg1: PLongWord);
    class procedure Test19(aArg1: PSingle);
    { by reference }
    class procedure Test20(var aArg1: LongInt);
    class procedure Test21(var aArg1: LongWord);
    class procedure Test22(var aArg1: Pointer);
    class procedure Test23(var aArg1: Char);
    class procedure Test24(var aArg1: Single);
    { combinations }
    class procedure Test25(aArg1: Byte; aArg2: Word; aArg3: LongWord; aArg4: QWord);
    class procedure Test26(aArg1: Pointer; var aArg2: Char; aArg3: Single);
  end;

const
  HelloWorld = 'Hello World';
var
  li: LongInt = -42;
  lw: LongWord = 42;
  p: Pointer = Nil;
  c: Char = 'a';
  s: Single = 3.1416;
begin
  try
    TestClass.Test1;
    { boolean}
    TestClass.Test2(True);
    { unsigned ordinals }
    TestClass.Test3(42);
    TestClass.Test4(42);
    TestClass.Test5(42);
    TestClass.Test6(42);
    { signed ordinals }
    TestClass.Test7(-42);
    TestClass.Test8(-42);
    TestClass.Test9(-42);
    TestClass.Test10(-42);
    { floating point }
    TestClass.Test11(3.1416);
    TestClass.Test12(3.1416);
    { chars }
    TestClass.Test13('a');
    TestClass.Test14('a');
    { pointers }
    TestClass.Test15(Nil);
    TestClass.Test16(Nil);
    TestClass.Test17(Nil);
    TestClass.Test18(Nil);
    TestClass.Test19(Nil);
    { by reference }
    TestClass.Test20(li);
    TestClass.Test21(lw);
    TestClass.Test22(p);
    TestClass.Test23(c);
    TestClass.Test24(s);
    { combinations }
    TestClass.Test25(42, 42, 42, 42);
    TestClass.Test26(Nil, c, 3.1416);
  except
    ExitCode := 1;
    writeln('error');
  end;
end.

