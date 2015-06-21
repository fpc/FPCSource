program bug_StrPLCopy;

{$APPTYPE CONSOLE}

uses
 SysUtils;

var
 Buf: array[0..10] of Char;

begin
 Buf[0] := 'A';
 writeln(ord(Buf[0]));
 StrPLCopy(Buf, '', 0);
 writeln(ord(Buf[0]));
 readln;
end.
