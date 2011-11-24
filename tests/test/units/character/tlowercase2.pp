program tlowercase2;

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
    
procedure DoError(ACode : Integer; ACodePoint : UnicodeChar); overload;
begin
  WriteLn('Error #',ACode,' ; CodePoint = ',IntToHex(Ord(ACodePoint),4));
  Halt(Acode);
end;

procedure DoError(ACode : Integer; AStr : UnicodeString; AIndex : Integer); overload;
begin
  WriteLn('Error #',ACode,' ; CodePoint = ',IntToHex(Ord(AStr[AIndex]),4));
  Halt(Acode);
end;

var
  e, i : Integer;
  strPrefix, uc : UnicodeString;
  locCharPos : Integer;
begin
  strPrefix := '012345AZERT ';
  locCharPos := Length(strPrefix) + 1;
  e := 1;
  for i := Ord('a') to Ord('z') do begin
    uc := strPrefix + UnicodeChar(i) + strPrefix;
    if not TCharacter.IsLower(uc,locCharPos) then
      DoError(e,i);
  end;
  
  Inc(e);
  for i := Ord('A') to Ord('Z') do begin
    uc := strPrefix + UnicodeChar(i) + strPrefix;
    if TCharacter.IsLower(uc,locCharPos) then
      DoError(e,i);
  end; 
  
  Inc(e);     
  for i := Low(Word) to High(Word) do begin
    uc := strPrefix + UnicodeChar(i) + strPrefix;
    if (TCharacter.GetUnicodeCategory(uc,locCharPos) = TUnicodeCategory.ucLowercaseLetter) then begin
      if not TCharacter.IsLower(uc,locCharPos) then
        DoError(e,uc,locCharPos);
    end;
  end;   
  
  Inc(e);
  for i := Low(Word) to High(Word) do begin
    uc := strPrefix + UnicodeChar(i) + strPrefix;
    if (TCharacter.GetUnicodeCategory(uc,locCharPos) <> TUnicodeCategory.ucLowercaseLetter) then begin
      if TCharacter.IsLower(uc,locCharPos) then
        DoError(e,uc,locCharPos);
    end;
  end;  

  WriteLn('ok');
end.

