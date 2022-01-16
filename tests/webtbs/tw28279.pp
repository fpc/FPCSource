{$mode objfpc}

program Project1;

var
  value_para_must_be_empty: boolean;

procedure Foo1(a: AnsiString; out b: AnsiString);
begin
  WriteLn(length(a));  WriteLn(length(b));
  if value_para_must_be_empty and
     (a<>'') then
    halt(2);
  if b<>'' then
    halt(3);
  b := 'a';
end;

procedure Foo2(out a: AnsiString; b: AnsiString);
begin
  WriteLn(length(a));  WriteLn(length(b));
  if a<>'' then
    halt(4);
  if value_para_must_be_empty and
     (b<>'') then
    halt(5);
  b := 'a';
end;

var s1: AnsiString;

function f: ansistring;
begin
  { the s1 parameter must be finalised first to prevent accidental use of
    the finalised value }
  if s1<>'' then
    halt(1);
  f:='a';
  f:=f+'b';
end;

const x: AnsiString = 'abcde';
begin
  value_para_must_be_empty:=true;

  s1 := copy(x,2,3)+'x';
  Foo1(s1,s1);

  s1 := copy(x,2,3)+'x';
  Foo2(s1,s1);

  value_para_must_be_empty:=false;
  s1 := copy(x,2,3)+'x';
  Foo1(f,s1);

  s1 := copy(x,2,3)+'x';
  Foo2(s1,f);
end. 
