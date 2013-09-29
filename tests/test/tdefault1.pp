program tdefault1;

{$APPTYPE CONSOLE}
{$mode objfpc}
{$modeswitch nestedprocvars}

uses
  variants;

type
  PLongInt = ^LongInt;

  TTestRecord = record
    first: LongInt;
    second: AnsiString;
    third: TObject;
  end;

  TTestObject = object
    first: LongInt;
    second: AnsiString;
    third: TObject;
  end;

  TTestEnum1 = (
    te1_1,
    te1_2,
    te1_3
  );

  TTestEnum2 = (
    te2_1 = 4,
    te2_2 = 8,
    te2_3 = 12
  );

  TTestProcedure = procedure;
  TTestMethod = procedure of object;
  TTestNested = procedure is nested;

  TTestSet1 = set of TTestEnum1;

  TRange1 = -5..5;
  TRange2 = -10..-5;
  TRange3 = 5..10;

  TTestArrayDyn = array of LongInt;
  TTestArrayStatic = array[0..5] of LongInt;
  TTestArrayStatic2 = array[0..5] of TTestRecord;

var
  trec, irec: TTestRecord;
  tobj: TTestObject;
  tstatic: TTestArrayStatic;
  tstatic2: TTestArrayStatic2;
  i: LongInt;
begin
  (* ordinal types *)
  if Default(ShortInt) <> 0 then
    Halt(1);
  if Default(SmallInt) <> 0 then
    Halt(2);
  if Default(LongInt) <> 0 then
    Halt(3);
  if Default(Int64) <> 0 then
    Halt(4);
  if Default(Byte) <> 0 then
    Halt(5);
  if Default(Word) <> 0 then
    Halt(6);
  if Default(LongWord) <> 0 then
    Halt(7);
{$ifdef fpc}
  if Default(QWord) <> 0 then
    Halt(8);
{$endif}
  (* boolean types *)
  if Default(Boolean) then
    Halt(9);
{$ifdef fpc}
  if Default(Boolean16) then
    Halt(10);
  if Default(Boolean32) then
    Halt(11);
  if Default(Boolean64) then
    Halt(12);
{$endif}
  if Default(ByteBool) then
    Halt(13);
  if Default(WordBool) then
    Halt(14);
  if Default(LongBool) then
    Halt(15);
{$ifdef fpc}
  if Default(QWordBool) then
    Halt(16);
{$endif}
  (* comma types *)
  if Default(Single) <> 0.0 then
    Halt(17);
  if Default(Double) <> 0.0 then
    Halt(18);
  if Default(Extended) <> 0.0 then
    Halt(19);
  if Default(Currency) <> 0.0 then
    Halt(20);
  if Default(Real) <> 0.0 then
    Halt(21);
  (* string types *)
  if Default(ShortString) <> '' then
    Halt(22);
  if Default(AnsiString) <> '' then
    Halt(23);
  if Default(WideString) <> '' then
    Halt(24);
  if Default(UnicodeString) <> '' then
    Halt(25);
  if Default(String) <> '' then
    Halt(26);
  (* char types *)
  if Default(AnsiChar) <> #0 then
    Halt(27);
  if Default(WideChar) <> #0 then
    Halt(28);
{$ifdef fpc}
  if Default(UnicodeChar) <> #0 then
    Halt(29);
{$endif}
  (* pointer types *)
  if Default(Pointer) <> Nil then
    Halt(30);
  if Default(PLongInt) <> Nil then
    Halt(31);
  (* structured types *)
  if Default(TObject) <> Nil then
    Halt(32);
  trec := Default(TTestRecord);
  if trec.first <> 0 then
    Halt(33);
  if trec.second <> '' then
    Halt(34);
  if trec.third <> Nil then
    Halt(35);
  tobj := Default(TTestObject);
  if tobj.first <> 0 then
    Halt(36);
  if tobj.second <> '' then
    Halt(37);
  if tobj.third <> Nil then
    Halt(38);
  if Default(IInterface) <> Nil then
    Halt(39);
  (* enumerations *)
  if Default(TTestEnum1) <> te1_1 then
    Halt(40);
  if Ord(Default(TTestEnum2)) <> 0 then
    Halt(41);
  (* sets *)
  if Default(TTestSet1) <> [] then
    Halt(42);
  (* range types *)
  if Default(TRange1) <> 0 then
    Halt(43);
  if Default(TRange2) <> 0 then
    Halt(44);
  if Default(TRange3) <> 0 then
    Halt(45);
  (* procedural types *)
  if Assigned(Default(TTestProcedure)) then
    Halt(46);
  if Assigned(Default(TTestMethod)) then
    Halt(47);
  (* Variant *)
  if not VarIsEmpty(Default(Variant)) then
    Halt(48);
  (* Arrays *)
  if Assigned(Default(TTestArrayDyn)) then
    Halt(49);
  tstatic := Default(TTestArrayStatic);
  for i in tstatic do
    if i <> 0 then
      Halt(50);
  tstatic2 := Default(TTestArrayStatic2);
  for irec in tstatic2 do
    if (irec.first <> 0) or (irec.second <> '') or assigned(irec.third) then
      Halt(51);
  (* other FPC specific types *)
  if Assigned(Default(TTestNested)) then
    Halt(52);
  Writeln('ok');
end.
