program bug_StrPLCopy;

{$APPTYPE CONSOLE}

uses
 SysUtils;

var
 Buf: array[0..10] of Char;

begin
 Buf[0] := 'A';
 StrPLCopy(Buf, '', 0);
 if Buf[0]<>#0 then
   halt(1);
end.
