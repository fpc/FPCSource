program ttoupper2;

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
  character;
    
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
  uc, s : UnicodeString;
begin  
  e := 1;
  for i := Ord('A') to Ord('Z') do begin
    uc := UnicodeChar(i);
    if (TCharacter.ToUpper(uc) <> uc) then
      DoError(e,i);
  end;

  Inc(e);
  for i := Ord('0') to Ord('9') do begin
    uc := UnicodeChar(i);
    if (TCharacter.ToUpper(uc) <> uc) then
      DoError(e,i);
  end;

  Inc(e);  
  if (TCharacter.ToUpper('azerty') <> 'AZERTY') then
    DoError(e,'azerty');
  if (TCharacter.ToUpper('AZERTY') <> 'AZERTY') then
    DoError(e,'AZERTY');
  if (TCharacter.ToUpper('AzERty') <> 'AZERTY') then
    DoError(e,'AzERty');
  
  Inc(e);
  j := Ord('A');
  for i := Ord('a') to Ord('z') do begin
    uc := UnicodeChar(i);
    s := UnicodeChar(j);
    if (TCharacter.ToUpper(uc) <> s) then
      DoError(e,i);
    Inc(j);
  end; 

  WriteLn('ok');
end.

