{ Source provided for Free Pascal Bug Report 2110 }
{ Submitted by "Alex" on  2002-09-05 }
{ e-mail: bjer@freemail.hu }
program BugReport;
{$mode objfpc}

type
  tsmallarray = array[0..3] of byte;

function Y32(Value: Longint): Longint;
var
  t1: array[1..4] of Byte absolute Value;
  t2: array[1..4] of Byte absolute Result;
begin
  t2[1] := t1[4];
  t2[2] := t1[3];
  t2[3] := t1[2];
  t2[4] := t1[1];
end;

function Y3210(Value : longint) : tsmallarray;
 var
  l : longint;
 begin
   l:=value;
   Y3210[0]:=0;
   Y3210[1]:=1;
   Y3210[2]:=2;
   Y3210[3]:=3;
 end;


Var
 l : longint;
 smallarray : tsmallarray;
BEGIN
 l:=$12345678;
 if Y32(l) <> $78563412 then
  Begin
    WriteLn('Error!');
    Halt(1);
  End;
 smallarray:=Y3210(l);
 if (smallarray[0] <> 0) or
    (smallarray[1] <> 1) or
    (smallarray[2] <> 2) or
    (smallarray[3] <> 3) then
  Begin
    WriteLn('Error!');
    Halt(1);
  end;
  WriteLn('Ok!');
END.
