{ %norun }
{ the code makes no sense, but it should compile }
{$MODE OBJFPC}
{$MODESWITCH ADVANCEDRECORDS+}
program test;

procedure Resolve(out Unsigned: array of QWord);
var
   Signed: array of Int64 absolute Unsigned;
begin
   SetLength(Signed, 0);
end;

begin
end.
