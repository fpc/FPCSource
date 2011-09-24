program tislower2;
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
  s := UnicodeChar($D835) + UnicodeChar($DFCB); //1D7CB;MATHEMATICAL BOLD SMALL DIGAMMA;Ll;0;L;<font> 03DD;;;;N;;;;;
  if not TCharacter.IsLower(s,1) then
    DoError(1);

  s := UnicodeChar($D835) + UnicodeChar($DFED); //1D7ED;MATHEMATICAL SANS-SERIF BOLD DIGIT ONE;Nd;0;EN;<font> 0031;1;1;1;N;;;;;
  if TCharacter.IsLower(s,1) then
    DoError(2);

  WriteLn('ok');
end.
