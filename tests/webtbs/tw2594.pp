{ Source provided for Free Pascal Bug Report 2594 }
{ Submitted by "Pavel V. Ozerski" on  2003-07-24 }
{ e-mail: ozerski@list.ru }

{$ifndef MACOS}
{$APPTYPE CONSOLE}
{$else}
{$APPTYPE TOOL}
{$endif}

{$ifdef fpc}
{$mode delphi}
{$endif}
type
 tp=procedure;
var
  err : boolean;
procedure expect(l1,l2:longint);
begin
  if l1<>l2 then
    begin
      writeln('ERROR got ',l1,' expected ',l2);
      err:=true;
    end
  else
    writeln(l1);
end;

var
 p:tp;
 pp:pointer absolute p;
begin
 expect(longint(@p),0);
 expect(longint(@pp),longint(@@p));
 expect(longint(addr(@p)),longint(@@p));
 expect(longint(@addr(p)),longint(@@p));
 expect(longint(@(addr(p))),longint(@@p));
 expect(longint(@(@p)),longint(@@p));
 if err then
   halt(1);
end.
