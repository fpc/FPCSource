{ Source provided for Free Pascal Bug Report 3594 }
{ Submitted by "Thomas Schatzl" on  2005-01-24 }
{ e-mail:  }
{$mode delphi}
program Project2;

{$APPTYPE CONSOLE}

uses
  SysUtils;

var 
  s1,s2 : String;
  n : tdatetime;

begin
  n:=Now;
  DateTimeToString( s1, 'YYYYY-MM-DD HH:MM:SS', n );
  DateTimeToString( s2, 'YYYY-MM-DD HH:NN:SS', n );
  if s1<>s2 then
    begin
      writeln('error');
      halt(1);
    end;
end.
