program tiscontrol;

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
    
procedure DoError(ACode : Integer; ACodePoint : UnicodeChar); overload;
begin
  WriteLn('Error #',ACode,' ; CodePoint = ',IntToHex(Ord(ACodePoint),4));
  Halt(Acode);
end;         

var
  e, i , k: Integer;
  uc : UnicodeChar;
begin  
  e := 1;
  for i := $0000 to $001F do begin
    uc := UnicodeChar(i);
    if not TCharacter.IsControl(uc) then
      DoError(e,uc);
  end;

  Inc(e);
  for i := $0080 to $009F do begin
    uc := UnicodeChar(i);
    if not TCharacter.IsControl(uc) then
      DoError(e,uc);
  end;

  Inc(e);
  for i := Low(Word) to High(Word) do begin
    { Skip all surrogate values }
    if (i>=HIGH_SURROGATE_BEGIN) and (i<=LOW_SURROGATE_END) then continue;
    uc := UnicodeChar(i);
    if (TCharacter.GetUnicodeCategory(uc) = TUnicodeCategory.ucControl) then begin
      if not TCharacter.IsControl(uc) then
        DoError(e,uc);
    end;
  end;

  Inc(e);
  for i := Ord('a') to Ord('z') do begin
    uc := UnicodeChar(i);
    if TCharacter.IsControl(uc) then
      DoError(e,uc);
  end;

  WriteLn('ok');
end.
