unit utcinterlocked;

{$mode Objfpc}

interface

uses punit;

Procedure RegisterTests;

implementation

uses
  SysUtils, SyncObjs, Classes;

function testlongint : TTestString;

var
  i32: Longint;
  New32, Old32: Longint;
  changed : Boolean;

begin
  Result:='';

  {* test all kinds of Longint usage *}
  i32 := 12;
  New32 := TInterlocked.Increment(i32);
  if New32 <> 13 then Exit('Error 1');
  if i32 <> 13 then Exit('Error 2');

  New32 := TInterlocked.Decrement(i32);
  if New32 <> 12 then Exit('Error 3');
  if i32 <> 12 then Exit('Error 4');

  New32 := TInterlocked.Add(i32, 12);
  if New32 <> 24 then Exit('Error 5');
  if i32 <> 24 then Exit('Error 6');

  Old32 := TInterlocked.CompareExchange(i32, 36, 24);
  if Old32 <> 24 then Exit('Error 7');
  if i32 <> 36 then Exit('Error 8');

  Old32 := TInterlocked.CompareExchange(i32, 48, 36, Changed);
  if Old32 <> 36 then Exit('Error 9');
  if Changed <> True then Exit('Error 10');
  if i32 <> 48 then Exit('Error 11');

  Old32 := TInterlocked.CompareExchange(i32, 123, 96, Changed);
  if Old32 <> 48 then Exit('Error 12');
  if Changed <> False then Exit('Error 13');
  if i32 <> 48 then Exit('Error 14');

  Old32 := TInterlocked.Exchange(i32, 96);
  if Old32 <> 48 then Exit('Error 15');
  if i32 <> 96 then Exit('Error 15');
end;

Function TestSingle : TTestString;

var
  s1, s2, sOld: Single;
begin
  Result:='';
  {* test all kinds of Single usage *}
  s1 := Single(3.14);
  s2 := Single(6.28);
  sOld := TInterlocked.CompareExchange(s1, s2, s1);
  if sOld <> Single(3.14) then Exit('Error 53');
  if s1 = Single(3.14) then Exit('Error 54');
  if s1 <> s2 then Exit('Error 55');

  sOld := TInterlocked.CompareExchange(s1, sOld, s2);
  if sOld <> Single(6.28) then Exit('Error 56');
  if s1 <> Single(3.14) then Exit('Error 57');
  if s1 = s2 then Exit('Error 58');

  sOld := TInterlocked.Exchange(s2, s1);
  if sOld <> Single(6.28) then Exit('Error 59');
  if s1 <> Single(3.14) then Exit('Error 60');
  if s1 <> s2 then Exit('Error 61');
end;

{$ifdef cpu64}
function testint64 : TTestString;
var
  i64: Int64;
  New64, Old64: Int64;
begin

  {* test all kinds of Int64 usage *}
  i64 := 12;
  New64 := TInterlocked.Increment(i64);
  if New64 <> 13 then Exit('Error 20');
  if i64 <> 13 then Exit('Error 21');

  New64 := TInterlocked.Decrement(i64);
  if New64 <> 12 then Exit('Error 22');
  if i64 <> 12 then Exit('Error 23');

  New64 := TInterlocked.Add(i64, 12);
  if New64 <> 24 then Exit('Error 24');
  if i64 <> 24 then Exit('Error 25');

  Old64 := TInterlocked.CompareExchange(i64, 36, 24);
  if Old64 <> 24 then Exit('Error 26');
  if i64 <> 36 then Exit('Error 27');

  Old64 := TInterlocked.Exchange(i64, 48);
  if Old64 <> 36 then Exit('Error 28');
  if i64 <> 48 then Exit('Error 29');

  Old64 := TInterlocked.Read(i64);
  if Old64 <> 48 then Exit('Error 30');
  if i64 <> 48 then Exit('Error 31');
end;

Function TestDouble : TTestString;

var
  d1, d2, dOld: Double;
begin
  Result:='';
  {* test all kinds of Double usage *}
  d1 := Double(3.14);
  d2 := Double(6.28);
  dOld := TInterlocked.CompareExchange(d1, d2, d1);
  if dOld <> Double(3.14) then Exit('Error 44');
  if d1 = Double(3.14) then Exit('Error 45');
  if d1 <> d2 then Exit('Error 46');

  d1 := dOld;
  dOld := TInterlocked.Exchange(d1, d2);
  if dOld <> Double(3.14) then Exit('Error 47');
  if d1 <> Double(6.28) then Exit('Error 48');
  if d1 <> d2 then Exit('Error 49');

  dOld := TInterlocked.CompareExchange(d1, dOld, d2);
  if dOld <> Double(6.28) then Exit('Error 50');
  if d1 <> Double(3.14) then Exit('Error 51');
  if d1 = d2 then Exit('Error 52');
end;

{$endif}

function TestObject : TTeststring;
var
  list1, list2, oldlist: TStringList;
begin
  Result:='';
  {* test all kinds of TObject and generic class usage *}
  List2:=nil;
  list1 := TStringList.Create;
  try
    list2 := TStringList.Create;
    list1.Add('A');
    list2.Add('B');
    list2.Add('C');

    { TObject }
    oldlist := TStringList(TInterlocked.CompareExchange(TObject(list1), TObject(list2), TObject(list1)));
    if list1 <> list2 then Exit('Error 32');
    if oldlist.Count = list1.Count then Exit('Error 33');
    if oldlist.Count = list2.Count then Exit('Error 34');

    oldlist := TStringList(TInterlocked.Exchange(TObject(list1), TObject(oldlist)));
    if oldlist <> list2 then Exit('Error 35');
    if list1.Count <> 1 then Exit('Error 36');
    if list2.Count <> 2 then Exit('Error 37');
  finally
    list1.Free;
    list2.Free;
  end;
end;

function TestGeneric : TTeststring;

var
  list1, list2, oldlist: TStringList;
begin
  Result:='';
  List2:=nil;
  list1 := TStringList.Create;
  try
    list2 := TStringList.Create;
    list1.Add('A');
    list2.Add('B');
    list2.Add('C');
    { generic class }
    oldlist := TInterlocked.specialize CompareExchange<TStringList>(list1, list2, list1);
    if list1 <> list2 then Exit('Error 38');
    if oldlist.Count = list1.Count then Exit('Error 39');
    if oldlist.Count = list2.Count then Exit('Error 40');

    oldlist := TInterlocked.specialize Exchange<TStringList>(list1, oldlist);
    if oldlist <> list2 then Exit('Error 41');
    if list1.Count <> 1 then Exit('Error 42');
    if list2.Count <> 2 then Exit('Error 43');
  finally
    list1.Free;
    list2.Free;
  end;
end;

Function TestBitTestAndClear : TTestString;
var
    i32: Longint;
    New32, Old32: Longint;
    i64: Int64;
    New64, Old64: Int64;
    Changed, OldBitValue: Boolean;
    list1, list2, oldlist: TStringList;
    d1, d2, dOld: Double;
    s1, s2, sOld: Single;
begin
  {* test BitTestAndClear usage *}
  i32 := 96;
  OldBitValue := TInterlocked.BitTestAndClear(i32, 6);
  if OldBitValue <> True then Exit('Error 62');
  if i32 <> 32 then Exit('Error 63');
  OldBitValue := TInterlocked.BitTestAndClear(i32, 6);
  if OldBitValue <> False then Exit('Error 64');
  if i32 <> 32 then Exit('Error 65');

  {* test BitTestAndSet usage *}
  OldBitValue := TInterlocked.BitTestAndSet(i32, 6);
  if OldBitValue <> False then Exit('Error 66');
  if i32 <> 96 then Exit('Error 67');
  OldBitValue := TInterlocked.BitTestAndSet(i32, 6);
  if OldBitValue <> True then Exit('Error 68');
  if i32 <> 96 then Exit('Error 69');
end;

Procedure RegisterTests;
var
  lSuite : PSuite;
begin
  lSuite:=AddSuite('TInterlocked');
  AddTest('Longint',@TestLongint,lSuite);
  {$IFDEF CPU64}
  AddTest('Int64',@TestInt64,lSuite);
  AddTest('Double',@TestDouble,lSuite);
  {$ENDIF}
  AddTest('Single',@TestSingle,lSuite);
  AddTest('Object',@TestObject,lSuite);
  AddTest('Generic',@TestGeneric,lSuite);
  AddTest('BitTestAndClear',@TestBitTestAndClear,lSuite);
end;

end.
