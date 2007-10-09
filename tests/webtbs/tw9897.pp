program widecharbug;

var
  AText : WideString;
  AWideChar: PWideChar;

begin
  AText := '';
  AWideChar := PWideChar(AText);
  if PWord(AWideChar)^<>0 then
    begin
      writeln(PWord(AWideChar)^);
     halt(1);
    end;
end.
