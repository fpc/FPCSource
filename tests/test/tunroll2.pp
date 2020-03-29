{ %OPT=-Ooloopunroll -Sg }
{$mode objfpc}

uses
  sysutils;

var
  c,i : Integer;


function f1 : Integer;
  begin
    for Result:=1 to 2 do
      if (i=1234) and (Result=2) then
        Exit;
  end;


function f2 : Integer;
  begin
    for Result:=1 to 2 do
      if (i=1234) and (Result=2) then
        Break;
  end;

function f3 : Integer;
  label
    Stop;
  begin
    for Result:=1 to 2 do
      if (i=1234) and (Result=2) then
        Goto Stop;
  Stop:
  end;

function f4 : Integer;
  begin
    Result:=-1;
    for c:=1 to 2 do
      if (i=1234) and (Result=2) then
        Raise Exception.Create('Test');
  end;

begin
  i:=1234;
  if f1<>2 then
    halt(1);
  if f2<>2 then
    halt(1);
  if f3<>2 then
    halt(1);
  try
    f4;
  except
    if c<>2 then
      halt(1);
  end;
  writeln('ok');
end.
