{ %OPT=-Oodfa -Sew -vw}
program tdfa2;
{$mode objfpc}{$h+}

procedure test0;
const
  c: array[0..3] of integer = (0,1,2,3);
begin
  writeln(c[1]);
end;

procedure test1;
var
  i: integer;
begin
  for i:=0 to 10 do
    if i=5 then
    ;
end;

function test2(S1: PWideChar; Len: Integer): Integer;
var
  counter: Integer;
  c1: Word;
begin
  counter:=0;  
  repeat
    c1 := ord(s1[counter]);
    counter:=counter+1;
  until counter>=len;
  result := c1;
end;

function test3(S1: PWideChar; Len: Integer): Integer;
var
  counter: Integer;
  c1: Word;
begin
  counter:=0;  
  while counter<len do
  begin
    c1 := ord(s1[counter]);
    counter:=counter+1;
  end;
  result := c1;
end;


begin
end.