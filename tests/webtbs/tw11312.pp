{ %opt=-O- }
{$mode objfpc}
type
  ii = interface
    procedure p1;
    procedure p2;
  end;

  to1 = class(tinterfacedobject,ii)
    procedure p1;virtual;
    procedure p2;virtual;
  end;

var
  i : longint;

procedure to1.p1;
  begin
    inc(i);
  end;

procedure to1.p2;
  begin
    inc(i);
  end;

var
  a,b,c,d,e,f,g,h : longint;
  i1 : ii;
begin
  i:=0;
  i1:=to1.create;
  for a:=1 to 1 do
    for b:=1 to a do
      for c:=1 to b do
       for d:=1 to c do
         for e:=1 to d do
           for f:=1 to e do
             for g:=1 to f do
               i1.p1;
  if i<>1 then
    halt(1);
  for a:=1 to 1 do
    for b:=1 to a do
      for c:=1 to b do
       for d:=1 to c do
         for e:=1 to d do
           for f:=1 to e do
             for g:=1 to f do
               i1.p2;
  if i<>2 then
    halt(1);
  writeln('ok');
end.
