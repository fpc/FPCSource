{ Old file: tbs0204.pp }
{ can typecast the result var in an assignment          OK 0.99.11 (PM) }

{ boolean(byte) byte(boolean)
  word(wordbool) wordbool(word)
  longint(longbool) and longbool(longint)
  must be accepted as var parameters
  or a left of an assignment }

procedure error;
begin
   Writeln('Error in tb0172a');
   Halt(1);
end;

procedure test;
var
  b : shortint;
  wb : smallint;
  lb : longint;

begin
  b:=0;
  wb:=0;
  lb:=0;

  byte(b):=128;
  word(wb):=32768;
  cardinal(lb):=$80000000;
  if (b<>low(shortint)) or (wb<>low(smallint)) or (lb<>low(longint)) then
    error;
end;

begin
  test;
end.
