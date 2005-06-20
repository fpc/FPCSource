{ Source provided for Free Pascal Bug Report 3124 }
{ Submitted by "Radoslaw Stachowiak" on  2004-05-29 }
{ e-mail: zenek_tm@tenbit.pl }
program strtest;
{$apptype console}
{$ifdef fpc}
  {$mode objfpc}
{$endif}

uses strutils;

var
  a, b: ansistring;

begin
  a:='aaaa';
  b:='AaAa';
  if AnsiStartsText(a, b) then
    writeln('ok')
  else
    halt(1);
end.
