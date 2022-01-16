program ttolower2;

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

procedure DoError(ACode : Integer); overload;
begin
  WriteLn('Error #',ACode);
  Halt(Acode);
end;         
    
procedure DoError(ACode : Integer; ACodePoint : Integer); overload;
begin
  WriteLn('Error #',ACode,' ; CodePoint = ',IntToHex(ACodePoint,4));
  Halt(Acode);
end;          
    
procedure DoError(ACode : Integer; ACodePoint : UnicodeString); overload;
begin
  WriteLn('Error #',ACode,' ; String = ',ACodePoint);
  Halt(Acode);
end;         

var
  e, i, j : Integer;
  uc, s, s2 : UnicodeString;
begin  
  e := 1;
  s := 'azerty';
  if (TCharacter.ToLower(s) <> s) then begin
    WriteLn(s);
    s2 := TCharacter.ToLower(s);
    WriteLn('"',s2,'"');
    DoError(e,s2);
  end;  

  Inc(e);
  s := '0123456789';
  if (TCharacter.ToLower(s) <> s) then
    DoError(e,s);

  Inc(e);  
  s := 'AZERTY'; s2:= 'azerty';
  if (TCharacter.ToLower(s) <> s2) then begin
    WriteLn(s);
    s2 := TCharacter.ToLower(s);
    WriteLn('"',s2,'"');
    DoError(e,s2);
  end;  
  s := 'AzERty';
  if (TCharacter.ToLower(s) <> s2) then begin
    WriteLn(s);
    s2 := TCharacter.ToLower(s);
    WriteLn('"',s2,'"');
    DoError(e,s2);
  end;  
  
  WriteLn('ok');
end.

