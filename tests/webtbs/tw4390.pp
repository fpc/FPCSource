{ Source provided for Free Pascal Bug Report 4390 }
{ Submitted by "Benjamin Rosseaux" on  2005-09-28 }
{ e-mail: benjamin@0ok.de }
PROGRAM Test;
{$IFDEF FPC}
 {$MODE DELPHI}
{$ENDIF}

PROCEDURE WriteToFile(CONST Buf;Size:INTEGER);
var
  s : shortstring;
BEGIN
  move(Buf,s[1],size);
  s[0]:=chr(size);
//  writeln('Writing: "',s,'"');
  if s<>'TEST' then
    halt(1);
END;

BEGIN
 WriteToFile('TEST',4);
END.
