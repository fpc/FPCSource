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

procedure printf(fm: pchar; args: array of const); cdecl; external libc;

procedure print(args: array of const);
begin
  printf('a number %i'#13#10,args);
end;

begin
  print([3333]);
end.

