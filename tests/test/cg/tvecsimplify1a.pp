program tvecsimplify1a;
{$H+}

const
  SPangram = 'The quick brown fox jumps over the lazy dog';

function GetChar: AnsiChar;
begin
  GetChar := SPangram[39];
end;

begin
  if GetChar() <> 'y' then
    Halt(1);
	
  WriteLn('ok');
end.