{ %cpu=i386,x86_64 }
{ %opt=-Cfsse3 -O3 }
{$mode objfpc}
uses
  cpu;

function test_double : longint;
  var
    f,f1,f2 : double;
    i : longint;
  begin
    result:=0;
    f1:=1;
    f2:=2;
    f:=f1*f1+f2*f2;
    if f<>5 then
      result:=1;
    f:=f1*f1-f2*f2;
    if f<>-3 then
      result:=1;
    { fool ssa }
    for i:=1 to 3 do
      begin
        f:=f1*f1+f2*f2;
        if f<>5 then
          result:=1;
        f:=f1*f1-f2*f2;
        if f<>-3 then
          result:=1;
      end;
  end;


function test_single : longint;
  var
    f,f1,f2 : single;
    i : longint;
  begin
    result:=0;
    f1:=1;
    f2:=2;
    f:=f1*f1+f2*f2;
    if f<>5 then
      result:=1;
    f:=f1*f1-f2*f2;
    if f<>-3 then
      result:=1;
    { fool ssa }
    for i:=1 to 3 do
      begin
        f:=f1*f1+f2*f2;
        if f<>5 then
          result:=1;
        f:=f1*f1-f2*f2;
        if f<>-3 then
          result:=1;
      end;
  end;

var
  f,f1,f2 : double;
  i : longint;
begin
  if not(is_sse3_cpu) then
    halt(0);
  f1:=1;
  f2:=2;
  f:=f1*f1+f2*f2;
  if f<>5 then
    halt(1);
  f:=f1*f1-f2*f2;
  if f<>-3 then
    halt(1);
  { fool ssa }
  for i:=1 to 3 do
    begin
      f:=f1*f1+f2*f2;
      if f<>5 then
        halt(1);
      f:=f1*f1-f2*f2;
      if f<>-3 then
        halt(1);
    end;
  if test_double<>0 then
    halt(1);
  if test_single<>0 then
    halt(1);
  writeln('ok');
end.
