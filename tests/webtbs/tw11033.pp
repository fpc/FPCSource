{$ifdef fpc}
{$mode objfpc}
{$endif fpc}
program t;

uses SysUtils,variants;

var a, b: Variant;

procedure test_or(i : Integer);
var
  v : variant;
begin
  try
    v:=a or b;
    if v<>i then
      halt(1);
    writeln(v);
  except
    on E: exception do writeln(e.message);
  end;
end;


procedure test_and(i : Integer);
var
  v : variant;
begin
  try
    v:=a and b;
    if v<>i then
      halt(1);
    writeln(v);
  except
    on E: exception do writeln(e.message);
  end;
end;


procedure test_xor(i : Integer);
var
  v : variant;
begin
  try
    v:=a xor b;
    if v<>i then
      halt(1);
    writeln(v);
  except
    on E: exception do writeln(e.message);
  end;
end;


procedure test_div(i : Integer);
var
  v : variant;
begin
  try
    v:=a div b;
    if v<>i then
      halt(1);
    writeln(v);
  except
    on E: exception do writeln(e.message);
  end;
end;


procedure test_mod(i : Integer);
var
  v : variant;
begin
  try
    v:=a mod b;
    if v<>i then
      halt(1);
    writeln(v);
  except
    on E: exception do writeln(e.message);
  end;
end;


procedure test_shl(i : Integer);
var
  v : variant;
begin
  try
    v:=a shl b;
    if v<>i then
      halt(1);
    writeln(v);
  except
    on E: exception do writeln(e.message);
  end;
end;

procedure test_shr(i : Integer);
var
  v : variant;
begin
  try
    v:=a shr b;
    if v<>i then
      halt(1);
    writeln(v);
  except
    on E: exception do writeln(e.message);
  end;
end;


begin
  a := Integer(1);
  b := 2.0;
  write('or: '); test_or(3);
  write('and: '); test_and(0);
  write('xor: '); test_xor(3);
  write('div: '); test_div(0);
  write('mod: '); test_mod(1);
  write('shl: '); test_shl(4);
  write('shr: '); test_shr(0);
  writeln('ok');
end.
