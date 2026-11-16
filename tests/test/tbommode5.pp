{ A UTF-8 signature says how the file's literals are read; it does not change what an AnsiString is. After a
  mode that turns on the system code page, a literal assigned to an AnsiString is in the system code page, as
  the same text converted at run time is. The test can tell only where the program runs with a system code
  page other than UTF-8 that its conversions use (Windows with an ANSI code page); elsewhere it passes. }
{$mode delphiunicode}
program tbommode5;

var
  lWide: UnicodeString;
  lLiteral, lConverted: AnsiString;
begin
  lWide := 'é';
  if (Length(lWide) <> 1) or (Ord(lWide[1]) <> $E9) then
    Halt(1);
  lLiteral := 'é';
  lConverted := AnsiString(lWide);
  if (DefaultSystemCodePage = CP_UTF8) or (DefaultSystemCodePage = CP_ACP) or
     (StringCodePage(lConverted) <> DefaultSystemCodePage) then
    begin
      WriteLn('ok');
      Halt(0);
    end;
  if StringCodePage(lLiteral) <> StringCodePage(lConverted) then
    begin
      Writeln('the literal has code page ', StringCodePage(lLiteral), ', the converted text ', StringCodePage(lConverted));
      Halt(2);
    end;
  if (Length(lLiteral) <> Length(lConverted)) or (lLiteral <> lConverted) then
    begin
      Writeln('the literal has ', Length(lLiteral), ' bytes, the converted text ', Length(lConverted));
      Halt(3);
    end;
  WriteLn('ok');
end.
