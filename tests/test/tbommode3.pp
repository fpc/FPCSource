{ As tbommode2, with the mode set by an included file without a signature }
{$i ibommode2.inc}
program tbommode3;

uses
  SysUtils;

const
  cOwn = 'é€';

{$i ibommode1.inc}

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
  Check('the included file''s literal', cInc, 2);
  Writeln('ok');
end.
