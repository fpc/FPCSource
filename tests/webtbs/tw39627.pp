{ %cpu=x86_64 }
{ %opt=-O- -O1 }
program cccc;

{$mode objfpc}{$H+}
uses classes;
procedure stackfill;
var a: array[1..10000] of byte;
  i: Integer;
begin
  for i := low(a) to high(a) do a[i] := i;
end;

//equal comparison, case sensitive, stopping at #0-bytes in p1, ignoring #0-bytes in l2
function strlnsequal(p1,p2:pansichar;l2: SizeInt):boolean;
var i:SizeInt;
begin
  for i:=0 to l2-1 do begin
    if p1[i]<>p2[i] then
      begin result := false; exit; end;
    if p1[i]=#0 then
      begin result := i = l2-1; exit; end
  end;
  result:=true;
end;

//Tests if the @code(p) starts with @code(expectedStart) (p is null-terminated)
function strbeginswith(const p: pansichar; const expectedStart: string): boolean; inline;
var
  q: PAnsiChar;
begin
  q := pansichar(pointer(expectedStart));
  result:=(expectedStart='') or (strlnsequal(p, q, length(expectedStart)));
end;



var list: TStringList;

function findOperator(const pos: pchar): TObject;
var
  i: SizeInt;
  j: SizeInt;
  sl, outerList: TStringList;
  k: SizeInt;
begin
  result := nil;
  outerList := list;
  for i := 0 to outerList.count - 1 do begin
    j := tstringlist(outerList.Objects[i]).IndexOf(pos^);
    if j >= 0 then begin
      sl := TStringList(TStringList(outerList.Objects[i]).Objects[j]);
      for k := 0 to sl.Count - 1 do
        if strBeginsWith(pos, sl[k]) then
          exit(sl.Objects[k]);
    end;
  end;
end;


begin
  list := TStringList.Create;
  list.AddObject('/', list);
  stackfill;
  if findOperator('//title')<>nil then
    writeln('ok');
end.

