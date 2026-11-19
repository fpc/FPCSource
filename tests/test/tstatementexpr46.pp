{ statement expressions with bare array constructor branches [..] }
{$Mode Delphi}

type
  TMyEnum = (en1,en2,en3);
  TMySet = set of TMyEnum;
  TByteSet = set of byte;
  TIntArr = array of integer;

procedure CheckArr(const Actual: TIntArr; const Expected: array of integer; Code: integer);
var
  i: integer;
begin
  if length(Actual)<>length(Expected) then
  begin
    writeln('Error ',Code,': expected length ',length(Expected),', got ',length(Actual));
    Halt(Code);
  end;
  for i:=0 to high(Expected) do
    if Actual[i]<>Expected[i] then
    begin
      writeln('Error ',Code,': index ',i,' expected ',Expected[i],', got ',Actual[i]);
      Halt(Code);
    end;
end;

var
  b, c: Boolean;
  i: integer;
  s: TMySet;
  bs: TByteSet;
  a, a2: TIntArr;
begin
  for b:=False to True do
  begin
    { sets of enums }
    s:=if b then [en1] else [];
    if b then begin if s<>[en1] then Halt(1); end
    else if s<>[] then Halt(2);

    s:=if b then [] else [en2];
    if b then begin if s<>[] then Halt(3); end
    else if s<>[en2] then Halt(4);

    s:=if b then [en1] else [en2,en3];
    if b then begin if s<>[en1] then Halt(5); end
    else if s<>[en2,en3] then Halt(6);

    s:=if b then [] else [];
    if s<>[] then Halt(7);

    s:=[en3];
    s:=if b then s else [];
    if b then begin if s<>[en3] then Halt(8); end
    else if s<>[] then Halt(9);

    { sets of byte }
    bs:=if b then [1,2] else [3];
    if b then begin if bs<>[1,2] then Halt(10); end
    else if bs<>[3] then Halt(11);

    { dynamic arrays }
    { [1] and [2,3] would be sets -> a typecast gives the type }
    a:=if b then TIntArr([1]) else [2,3];
    if b then CheckArr(a,[1],12)
    else CheckArr(a,[2,3],13);

    a:=if b then [1000] else [];
    if b then CheckArr(a,[1000],14)
    else CheckArr(a,[],15);

    a:=if b then [1] else [1000,-5];
    if b then CheckArr(a,[1],16)
    else CheckArr(a,[1000,-5],17);

    a2:=[7,8];
    a:=if b then a2 else [1];
    if b then CheckArr(a,[7,8],18)
    else CheckArr(a,[1],19);

    { case expression }
    for c:=False to True do
    begin
      i:=ord(b)*2+ord(c);
      s:=case i of
          0: [en1];
          1: [];
          2: [en2,en3];
        else [en1,en3];
        end;
      case i of
      0: if s<>[en1] then Halt(20);
      1: if s<>[] then Halt(21);
      2: if s<>[en2,en3] then Halt(22);
      else if s<>[en1,en3] then Halt(23);
      end;
    end;
  end;
  writeln('ok');
end.
