program tarray15;

{$define target_supports_rodata}
{$if defined(msdos)}
{$undef target_supports_rodata}
{$endif}

{$mode  objfpc}

{ needed for "except" to work }
uses
  SysUtils;

{$ifdef InLazIDE}
function CheckArray(aArr, aExpected: array of LongInt): Boolean;
{$else}
generic function CheckArray<T>(aArr, aExpected: array of T): Boolean;
{$endif}
var
  i: LongInt;
begin
  if Length(aArr) <> Length(aExpected) then
    Exit(False);
  for i := Low(aArr) to High(aArr) do
    if aArr[i] <> aExpected[i] then
      Exit(False);
  Result := True;
end;

var
  v1: array of LongInt = Nil;
  v2: array of LongInt = ();
  v3: array of LongInt = (1, 2, 3);
  v4: array of String = ('Alpha', 'Beta', 'Gamma', 'Delta');
  v5: array[0..2] of array of LongInt = (Nil, (), (1, 2, 3));
  v6: array of array[0..2] of LongInt = ((1, 2, 3), (4, 5, 6));
  v7: array[0..2] of array of array[0..2] of LongInt = (((1, 2, 3), (4, 5, 6)), (), ((7, 8, 9)));

{$push}
{$J-}
const
  rc1: array of LongInt = (1, 2, 3);
{$J+}
const
  wc1: array of LongInt = (1, 2, 3);
{$pop}

begin
  if Length(v1) <> 0 then
    Halt(1);
  if Length(v2) <> 0 then
    Halt(2);
{$ifndef InLazIDE}
  if not specialize CheckArray<LongInt>(v3, [1, 2, 3]) then
    Halt(3);
  if not specialize CheckArray<String>(v4, ['Alpha', 'Beta', 'Gamma', 'Delta']) then
    Halt(4);
  if Length(v5[0]) <> 0 then
    Halt(5);
  if Length(v5[1]) <> 0 then
    Halt(6);
  if not specialize CheckArray<LongInt>(v5[2], [1, 2, 3]) then
    Halt(7);
  if Length(v6) <> 2 then
    Halt(8);
  if not specialize CheckArray<LongInt>(v6[0], [1, 2, 3]) then
    Halt(9);
  if not specialize CheckArray<LongInt>(v6[1], [4, 5, 6]) then
    Halt(10);
  if Length(v7[0]) <> 2 then
    Halt(11);
  if Length(v7[1]) <> 0 then
    Halt(12);
  if Length(v7[2]) <> 1 then
    Halt(13);
  if not specialize CheckArray<LongInt>(v7[0, 0], [1, 2, 3]) then
    Halt(14);
  if not specialize CheckArray<LongInt>(v7[0, 1], [4, 5, 6]) then
    Halt(15);
  if not specialize CheckArray<LongInt>(v7[2, 0], [7, 8, 9]) then
    Halt(16);
  v3[1] := 42;
  if not specialize CheckArray<LongInt>(v3, [1, 42, 3]) then
    Halt(17);
  if not specialize CheckArray<LongInt>(rc1, [1, 2, 3]) then
    Halt(18);
{$ifdef target_supports_rodata}
  try
    rc1[1] := 42;
    Halt(19);
  except
  end;
{$endif}
  if not specialize CheckArray<LongInt>(wc1, [1, 2, 3]) then
    Halt(20);
  wc1[1] := 42;
  if not specialize CheckArray<LongInt>(wc1, [1, 42, 3]) then
    Halt(21);
{$endif}
  Writeln('ok');
end.
