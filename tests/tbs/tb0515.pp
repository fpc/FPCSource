{$mode objfpc}{$h+}

uses
  classes, sysutils;

const
  count = 64;
  factor = 15;

function ptrtopaddedstr(x: pointer): string;
begin
  result := format('%02p', [x]);
end;

procedure fatalerror(str: string; index: integer);
begin
  writeln(str, index);
  halt(1);
end;

var
  i, j: ptrint;
  strlist: tstringlist;
begin
  strlist := tstringlist.create;
  for i := 0 to count-1 do
  begin
    j := factor*i mod count;
    strlist.addobject(ptrtopaddedstr(pointer(j)), tobject(j));
  end;
  for i := 0 to count-1 do
  begin
    j := factor*i mod count;
    if strlist.strings[i] <> ptrtopaddedstr(pointer(j)) then
      fatalerror('string error at ', i);
    if strlist.objects[i] <> tobject(j) then
      fatalerror('object error at ', i);
  end;
  strlist.sort;
  for i := 0 to count-1 do
  begin
    if strlist.strings[i] <> ptrtopaddedstr(pointer(i)) then
      fatalerror('sorted string error at ', i);
    if strlist.objects[i] <> tobject(i) then
      fatalerror('sorted object error at ', i);
  end;
  strlist.delete(10);
  strlist.delete(25);
  if strlist.count <> count-2 then
    fatalerror('strlist.count is not ', count-2);
  j := 0;
  for i := 0 to 61 do
  begin
    if strlist.strings[i] <> ptrtopaddedstr(pointer(j)) then
      fatalerror('delete string error at ', i);
    if strlist.objects[i] <> tobject(j) then
      fatalerror('delete object error at ', i);
    inc(j);
    if (j = 10) or (j = 26) then
      inc(j);
  end;
  strlist.free;
  writeln('ok');
end.
