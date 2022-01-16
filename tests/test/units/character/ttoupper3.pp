program ttoupper3;

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
  s, s2, s3 : UnicodeString;
begin  
  e := 1; 
  s := UnicodeChar(Word($D801)) + UnicodeChar(Word($DC16));
  s2 := TCharacter.ToUpper(s);
  if (s2 <> s) then begin
    WriteLn('s=',DumpStr(s),' ; TCharacter.ToUpper(s) = ',DumpStr(s2));
    DoError(e,TCharacter.ToUpper(s));
  end;  

  Inc(e);
  s := UnicodeChar(Word($D801)) + UnicodeChar(Word($DC40));
  s2 := TCharacter.ToUpper(s);
  s3 := UnicodeChar(Word($D801)) + UnicodeChar(Word($DC18));//Actual 
  if (s2 <> s3) then begin
    WriteLn('s=',DumpStr(s),' ; TCharacter.ToUpper(s) = ',DumpStr(s2),' ; Expected = ',DumpStr(s3));
    DoError(e,TCharacter.ToUpper(s));
  end;  
  
  WriteLn('ok');
end.

