{ %fail }

{ Source provided for Free Pascal Bug Report 2046 }
{ Submitted by "Mattias Gaertner" on  2002-07-17 }
{ e-mail: nc-gaertnma@netcologne.de }
program printftest;

{$mode objfpc}{$H+}

const
{$ifdef win32}
  libc='msvcrt';
{$else}
  libc='c';
{$endif}

procedure sprintf(a, fm: pchar; args: array of const); cdecl; external libc;

procedure print(args: array of const);
var
  a : array[0..255] of char;
begin
  { THis is not supported. It needs runtime support that will iterate through all
    array of const elements and pass them on the stack dependent on the type

    For now it should print an error, because it generates invalid code }
  sprintf(a,'a number %i',args);
  writeln(a);
  if a<>'a number 3333' then
   begin
     writeln('Error!');
     halt(1);
   end;
end;

begin
  print([3333]);
end.
