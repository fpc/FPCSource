{ Old file: tbs0204.pp }
{ can typecast the result var in an assignment          OK 0.99.11 (PM) }

{ boolean(byte) byte(boolean)
  word(wordbool) wordbool(word)
  longint(longbool) and longbool(longint)
  must be accepted as var parameters
  or a left of an assignment }

procedure error;
begin
   Writeln('Error in tbs0204');
   Halt(1);
end;

var
  b : boolean;
  wb : wordbool;
  lb : longbool;

begin
  byte(b):=1;
  word(wb):=1;
  longint(lb):=1;
  if (not b) or (not wb) or (not lb) then
    error;
  byte(b):=2;
  Writeln('if a boolean contains 2 it is considered as ',b);
  byte(b):=3;
  Writeln('if a boolean contains 3 it is considered as ',b);
  shortint(b):=-1;
  Writeln('if a boolean contains shortint(-1) it is considered as ',b);
end.
