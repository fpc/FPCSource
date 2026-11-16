{ The file starts with a UTF-8 signature, so its literals are UTF-8 even after
  a mode that turns on the system code page, here delphiunicode }
{$mode delphiunicode}
program tbommode1;

const
  cOwn = 'é€';

procedure Check(const aWhat: string; const aText: UnicodeString; aExit: Integer);
begin
  if (Length(aText)<>2) or (Ord(aText[1])<>$E9) or (Ord(aText[2])<>$20AC) then
    begin
      Writeln(aWhat, ': length ', Length(aText), ', expected U+00E9 U+20AC');
      Halt(aExit);
    end;
end;

begin
  Check('the file''s literal', cOwn, 1);
  Writeln('ok');
end.
