program TInterlocked_tests;

{$mode Delphi}

uses
  SysUtils, SyncObjs, Classes;

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
  writeln('start testing of TInterlocked methods');

  {* test all kinds of Longint usage *}
  i32 := 12;
  New32 := TInterlocked.Increment(i32);
  if New32 <> 13 then halt(1);
  if i32 <> 13 then halt(2);

  New32 := TInterlocked.Decrement(i32);
  if New32 <> 12 then halt(3);
  if i32 <> 12 then halt(4);

  New32 := TInterlocked.Add(i32, 12);
  if New32 <> 24 then halt(5);
  if i32 <> 24 then halt(6);

  Old32 := TInterlocked.CompareExchange(i32, 36, 24);
  if Old32 <> 24 then halt(7);
  if i32 <> 36 then halt(8);

  Old32 := TInterlocked.CompareExchange(i32, 48, 36, Changed);
  if Old32 <> 36 then halt(9);
  if Changed <> True then halt(10);
  if i32 <> 48 then halt(11);

  Old32 := TInterlocked.CompareExchange(i32, 123, 96, Changed);
  if Old32 <> 48 then halt(12);
  if Changed <> False then halt(13);
  if i32 <> 48 then halt(14);

  Old32 := TInterlocked.Exchange(i32, 96);
  if Old32 <> 48 then halt(15);
  if i32 <> 96 then halt(15);

{$ifdef cpu64}
  {* test all kinds of Int64 usage *}
  i64 := 12;
  New64 := TInterlocked.Increment(i64);
  if New64 <> 13 then halt(20);
  if i64 <> 13 then halt(21);

  New64 := TInterlocked.Decrement(i64);
  if New64 <> 12 then halt(22);
  if i64 <> 12 then halt(23);

  New64 := TInterlocked.Add(i64, 12);
  if New64 <> 24 then halt(24);
  if i64 <> 24 then halt(25);

  Old64 := TInterlocked.CompareExchange(i64, 36, 24);
  if Old64 <> 24 then halt(26);
  if i64 <> 36 then halt(27);

  Old64 := TInterlocked.Exchange(i64, 48);
  if Old64 <> 36 then halt(28);
  if i64 <> 48 then halt(29);

  Old64 := TInterlocked.Read(i64);
  if Old64 <> 48 then halt(30);
  if i64 <> 48 then halt(31);
{$endif}

  {* test all kinds of TObject and generic class usage *}
  list1 := TStringList.Create;
  list2 := TStringList.Create;
  try
    list1.Add('A');
    list2.Add('B');
    list2.Add('C');

    { TObject }
    oldlist := TStringList(TInterlocked.CompareExchange(TObject(list1), TObject(list2), TObject(list1)));
    if list1 <> list2 then halt(32);
    if oldlist.Count = list1.Count then halt(33);
    if oldlist.Count = list2.Count then halt(34);

    oldlist := TStringList(TInterlocked.Exchange(TObject(list1), TObject(oldlist)));
    if oldlist <> list2 then halt(35);
    if list1.Count <> 1 then halt(36);
    if list2.Count <> 2 then halt(37);

    { generic class }
    oldlist := TInterlocked.CompareExchange<TStringList>(list1, list2, list1);
    if list1 <> list2 then halt(38);
    if oldlist.Count = list1.Count then halt(39);
    if oldlist.Count = list2.Count then halt(40);

    oldlist := TInterlocked.Exchange<TStringList>(list1, oldlist);
    if oldlist <> list2 then halt(41);
    if list1.Count <> 1 then halt(42);
    if list2.Count <> 2 then halt(43);
  finally
    list1.Free;
    list2.Free;
  end;

  writeln('tests passed so far');

{$ifdef cpu64}
  {* test all kinds of Double usage *}
  d1 := Double(3.14);
  d2 := Double(6.28);
  dOld := TInterlocked.CompareExchange(d1, d2, d1);
  if dOld <> Double(3.14) then halt(44);
  if d1 = Double(3.14) then halt(45);
  if d1 <> d2 then halt(46);

  d1 := dOld;
  dOld := TInterlocked.Exchange(d1, d2);
  if dOld <> Double(3.14) then halt(47);
  if d1 <> Double(6.28) then halt(48);
  if d1 <> d2 then halt(49);

  dOld := TInterlocked.CompareExchange(d1, dOld, d2);
  if dOld <> Double(6.28) then halt(50);
  if d1 <> Double(3.14) then halt(51);
  if d1 = d2 then halt(52);
{$endif}

  {* test all kinds of Single usage *}
  s1 := Single(3.14);
  s2 := Single(6.28);
  sOld := TInterlocked.CompareExchange(s1, s2, s1);
  if sOld <> Single(3.14) then halt(53);
  if s1 = Single(3.14) then halt(54);
  if s1 <> s2 then halt(55);

  sOld := TInterlocked.CompareExchange(s1, sOld, s2);
  if sOld <> Single(6.28) then halt(56);
  if s1 <> Single(3.14) then halt(57);
  if s1 = s2 then halt(58);

  sOld := TInterlocked.Exchange(s2, s1);
  if sOld <> Single(6.28) then halt(59);
  if s1 <> Single(3.14) then halt(60);
  if s1 <> s2 then halt(61);

  {* test BitTestAndClear usage *}
  i32 := 96;
  OldBitValue := TInterlocked.BitTestAndClear(i32, 6);
  if OldBitValue <> True then halt(62);
  if i32 <> 32 then halt(63);
  OldBitValue := TInterlocked.BitTestAndClear(i32, 6);
  if OldBitValue <> False then halt(64);
  if i32 <> 32 then halt(65);

  {* test BitTestAndSet usage *}
  OldBitValue := TInterlocked.BitTestAndSet(i32, 6);
  if OldBitValue <> False then halt(66);
  if i32 <> 96 then halt(67);
  OldBitValue := TInterlocked.BitTestAndSet(i32, 6);
  if OldBitValue <> True then halt(68);
  if i32 <> 96 then halt(69);

  writeln('testing of TInterlocked methods ended');
end.