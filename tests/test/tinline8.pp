{$ifdef fpc}
{$mode objfpc}
{$inline on}
{$endif}

uses
  sysutils;

var a: longint;

function f(l: longint): longint; inline;
var
  l1,l2,l3: longint;
begin
  result:=123456;
  if (l > 10) then
    exit;
  result:=30;
  for l1 := 1 to 10 do
    for l2 := 1 to 100 do
  ;
  result := 40;
  for l3 := 1 to 10 do;
end;


procedure test;
var
  l: longint;
begin
  l:= f(a);
  if (l<>123456) then
    halt(1);
end;


procedure test2;
var
  l: longint;
begin
  try
  finally
    l:= f(a);
    if (l<>123456) then
      halt(1);
  end;
end;


procedure inl2; inline;
begin
  try
  except on exception do ;
  end
end;


begin
  a:=20;
  test;
  test2;
  inl2
end.
