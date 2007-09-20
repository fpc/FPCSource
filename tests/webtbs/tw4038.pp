{ Source provided for Free Pascal Bug Report 4038 }
{ Submitted by "Antonio Marti" on  2005-06-01 }
{ e-mail: windowze2000@yahoo.es }

{ %skiptarget=wince }

uses sysutils;
var b: byte;
    s : string;
begin
{$ifdef unix}
  s:='/bin/echo';
{$else}
{$ifdef windows}
  s:='gecho';
{$else windows}
  s:='echo';
{$endif windows}
{$endif}
  writeln(executeprocess(s,'works1 works2 works3'));
  writeln(executeprocess(s,'works1 works2 works3'));
  writeln;
  for b:=1 to 2 do
    writeln(executeprocess(s,'fails1 fails2 fails3'));
end.
