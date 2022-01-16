program tisdigit3;

{$ifdef FPC}
  {$mode objfpc}
  {$H+}
  {$PACKENUM 1}
{$endif fpc} 

{$ifndef FPC}
  {$APPTYPE CONSOLE}    
{$endif}
  
uses     
  SysUtils,
  unicodedata,character;
    
{$ifndef FPC}
  type UnicodeChar = WideChar;   
{$endif} 

function DumpStr(a : UnicodeString) : UnicodeString;
var
  i : Integer;
  s : UnicodeString;
begin
  s := '';
  for i := 1 to Length(a) do
    s := s + Format('#%x',[Word(a[i])]);
  Result := s; 
end;
    
procedure DoError(ACode : Integer; ACodePoint : UnicodeString); overload;
begin
  WriteLn('Error #',ACode,' ; String = ',DumpStr(ACodePoint));
  Halt(Acode);
end;         

var
  e : Integer;
  s : UnicodeString;
begin  
  e := 1; 
  s := UnicodeChar(Word($D801)) + UnicodeChar(Word($DCA1));
  if not TCharacter.IsDigit(s,1) then begin
    WriteLn('s=',DumpStr(s),' ; TCharacter.IsDigit(s) = ',TCharacter.IsDigit(s,1));
    DoError(e,s);
  end;  

  Inc(e);
  s := UnicodeChar(Word($D801)) + UnicodeChar(Word($DCA3));
  if not TCharacter.IsDigit(s,1) then begin
    WriteLn('s=',DumpStr(s),' ; TCharacter.IsDigit(s) = ',TCharacter.IsDigit(s,1));
    DoError(e,s);
  end;  
  
  WriteLn('ok');
end.

