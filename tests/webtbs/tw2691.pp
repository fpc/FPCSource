{ Source provided for Free Pascal Bug Report 2691 }
{ Submitted by "Luk Vandelaer" on  2003-09-22 }
{ e-mail: luk.vandelaer@wisa.be }
program int64prob;

uses sysutils;

var
  x : int64;

begin
  x := $FFFF shl 32;
  writeln (format ('%d %d %d',
    [x mod $10000, x div $10000, x div $10000]));
end.
