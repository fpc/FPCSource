program scanf_example2;

{$mode objfpc}{$H+}

uses
  sysutils, gmp;

var
  n, sqr: MPInteger;
  s: AnsiString;
begin
  write('Please enter an integer of any length: ');
  readln(s);
  z_init(n);
  if mp_sscanf(PAnsiChar(s), '%Zd', n.ptr) = 1 then begin
    sqr := n ** 2;
    writeln(format('%s^2 = %s', [AnsiString(n), AnsiString(sqr)]));
  end else
    writeln('Failed to parse an integer from your input');
end.

