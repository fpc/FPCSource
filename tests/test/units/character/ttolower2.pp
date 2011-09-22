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
  for i := Ord('a') to Ord('z') do begin
    uc := UnicodeChar(i);
    if (TCharacter.ToLower(uc) <> uc) then
      DoError(e,i);
  end;

  Inc(e);
  for i := Ord('0') to Ord('9') do begin
    uc := UnicodeChar(i);
    if (TCharacter.ToLower(uc) <> uc) then
      DoError(e,i);
  end;

  Inc(e);  
  if (TCharacter.ToLower('azerty') <> 'azerty') then
    DoError(e,'azerty');
  if (TCharacter.ToLower('AZERTY') <> 'azerty') then
    DoError(e,'AZERTY');
  if (TCharacter.ToLower('AzERty') <> 'azerty') then
    DoError(e,'AzERty');
  
  Inc(e);
  j := Ord('a');
  for i := Ord('A') to Ord('Z') do begin
    uc := UnicodeChar(i);
    s := UnicodeChar(j);
    if (TCharacter.ToLower(uc) <> s) then
      DoError(e,i);
    Inc(j);
  end; 

  WriteLn('ok');
end.

