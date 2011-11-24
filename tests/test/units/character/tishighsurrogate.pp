program tishighsurrogate;

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

const
  LOW_SURROGATE_BEGIN  = Word($DC00);
  LOW_SURROGATE_END    = Word($DFFF);
  HIGH_SURROGATE_BEGIN = Word($D800);
  HIGH_SURROGATE_END   = Word($DBFF);
var
  e, i , k: Integer;
  uc : UnicodeChar;
begin  
  e := 1;
  for i := HIGH_SURROGATE_BEGIN to HIGH_SURROGATE_END do begin
    uc := UnicodeChar(i);
    if not TCharacter.IsHighSurrogate(uc) then
      DoError(e,uc);
  end;

  Inc(e);
  for i := LOW_SURROGATE_BEGIN to LOW_SURROGATE_END do begin
    uc := UnicodeChar(i);
    if TCharacter.IsHighSurrogate(uc) then
      DoError(e,uc);
  end;

  Inc(e);
  for i := Low(Word) to High(Word) do begin
    if (i < $D800) or (i > $DFFF) then begin
      uc := UnicodeChar(i);
      if TCharacter.IsHighSurrogate(uc) then
        DoError(e,uc);
    end;
  end;

  WriteLn('ok');
end.
