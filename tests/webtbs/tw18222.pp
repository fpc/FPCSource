{ %norun }
program tw18222;
{$mode objfpc}{$H-}

uses sysutils;

type
  TFoo = class
  public
    FooValue: Integer;
  end;

  PFoo = ^TFoo;
  PFooTyped = type PFoo;
  PPFoo = ^PFoo;

  TFooClass = Class of TFoo;
  PFooClass = ^TFooClass;
  PPFooClass = ^PFooClass;

  TMyAnsiString = AnsiString;
  TMyOwnAnsiString = type AnsiString;


procedure FooFunc(
  ArgAnsiString1: AnsiString; var ArgAnsiString2: AnsiString; const ArgAnsiString3: AnsiString;
  ArgPAnsiString1: PAnsiString; var ArgPAnsiString2: PAnsiString; const ArgPAnsiString3: PAnsiString;

  ArgChar1: Char; var ArgChar2: Char; const ArgChar3: Char;
  ArgPChar1: PChar; var ArgPChar2: PChar; const ArgPChar3: PChar;

  ArgQW1, ArgQW2: QWord;

  Foo1, Foo1n: TFoo; var Foo2, Foo2n: TFoo;
  pFoo1, pFoo1n: PFoo; var pFoo2, pFoo2n: PFoo;

  FooClass1, FooClass1n: TFooClass; var FooClass2, FooClass2n: TFooClass;
  pFooClass1, pFooClass1n: PFooClass; var pFooClass2, pFooClass2n: PFooClass
);
var
  TestInt: Integer;
  TesTShortString: String[10];
  TestAnsiString: AnsiString;
  TestMyAnsiString: TMyAnsiString;
  TestMyOwnAnsiString: TMyOwnAnsiString;
  TestPChar: PChar;
  TestQW1: QWord;
  TestQW2: QWord;

  TestPPFoo: PPFoo;
  //TestPFooTyped: PFooTyped;
  TestPPFooClass: PPFooClass;

  function SubFoo(var AVal1: Integer; AVal2: Integer) : Integer;
  begin
    AVal1 := 2 * AVal2;
    Result := AVal2;
    inc(AVal2);   // First BreakBoint
  end;

begin
  TestInt := 3;
  TesTShortString := IntToStr(TestInt) + ':';
  TestAnsiString := TesTShortString + ' Foo';
  TestMyAnsiString := TesTShortString + ' FooMy';
  TestMyOwnAnsiString := TesTShortString + ' FooMyOwn';
  TestPChar := @TestAnsiString[2];
  TestQw1 := ArgQw1 + 1; dec(TestQW1);
  TestQw2 := ArgQw2 + 1; dec(TestQW2);
  TestPPFoo := @pFoo1;
  TestPPFooClass := @pFooClass1;
  SubFoo(TestInt, 5);
  // access all values, so the will not be optimized away
  writeln(TestPChar);
  // params
  writeln(ArgAnsiString1, ArgAnsiString2, ArgAnsiString3, ArgChar1, ArgChar2, ArgChar3); // breakpoint 2
  writeln(ArgQw1, ArgQw1, TestQW1, TestQW2);
  if (Foo1 is FooClass1) and (Foo2 is FooClass2) and (Foo1n = Foo2n) and (FooClass1 = FooClass2) then
    writeln(Foo1.FooValue + foo2.FooValue);
end;

var
  a1, a2, a3: ansistring;
  a2p: PAnsiString;
  c1, c2, c3: Char;
  c2p: PChar;
  f1, f2, fn: TFoo;
  f1p, fnp: PFoo;
  fc, fcn: TFooclass;
  fcp, fcnp: PFooClass;
begin
  a1 := 'abc';  a2 := 'def';  a3 := 'ghi';
  a2p := @a2;
  c1 := 'X';  c2 := 'Y';  c2 := 'Z';
  c2p := @c2;

  f1 := TFoo.Create;
  f2 := TFoo.Create;
  fn := nil;
  f1p := @f1;
  fnp := @fn;

  fc := TFoo;
  fcn := nil;
  fcp := @fc;
  fcnp := @fcp;

  FooFunc(
      a1, a2, a3,
      @a1, a2p, @a3,

      c1, c2, c3,
      @c1, c2p, @c3,

      //ArgQW1, ArgQW2: QWord;
      139784704, 139784871,

      //Foo1, Foo1n: TFoo; var Foo2, Foo2n: TFoo
      f1, nil, f2, fn,
      f1p, nil, f1p, fnp,

      //FooClass1, FooClass1n: TFooclass; var FooClass2, FooClass2n: TFoo
      TFoo, nil, fc, fcn,
      fcp, nil, fcp, fcnp
     );
  FreeAndNil(f1);
  FreeAndNil(f2);
end.
