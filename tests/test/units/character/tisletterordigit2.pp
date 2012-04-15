program tisletterordigit2;
{$ifndef FPC}
  {$APPTYPE CONSOLE}    
{$endif}
  
uses     
  SysUtils,
  character;
    
{$ifndef FPC}
  type UnicodeChar = WideChar;   
{$endif} 

procedure DoError(ACode : Integer); 
begin
  WriteLn('Error #',ACode);
  Halt(Acode);
end;  

var
  s : UnicodeString;
begin 
  s := UnicodeChar($D835) + UnicodeChar($DD75); //1D575;MATHEMATICAL BOLD FRAKTUR CAPITAL J
  if not TCharacter.IsLetterOrDigit(s,1) then
    DoError(1);

  s := UnicodeChar($D835) + UnicodeChar($DFED); //1D7ED;MATHEMATICAL SANS-SERIF BOLD DIGIT ONE;Nd;0;EN;<font> 0031;1;1;1;N;;;;;
  if not TCharacter.IsLetterOrDigit(s,1) then
    DoError(2);

  s := UnicodeChar($D83C) + UnicodeChar($DC00); //1F000;MAHJONG TILE EAST WIND;So;0;ON;;;;;N;;;;;
  if TCharacter.IsLetterOrDigit(s,1) then
    DoError(3);

  WriteLn('ok');
end.