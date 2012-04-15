program tisletterordigit;

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

var
  e, i , k: Integer;
  uc : UnicodeChar;
begin  
  e := 1;
  for i := Ord('a') to Ord('z') do begin
    uc := UnicodeChar(i);
    if not TCharacter.IsLetterOrDigit(uc) then
      DoError(e,uc);
  end;

  Inc(e);
  for i := Ord('A') to Ord('Z') do begin
    uc := UnicodeChar(i);
    if not TCharacter.IsLetterOrDigit(uc) then
      DoError(e,uc);
  end;

  Inc(e);
  for i := Ord('0') to Ord('9') do begin
    uc := UnicodeChar(i);
    if not TCharacter.IsLetterOrDigit(uc) then
      DoError(e,uc);
  end;

  Inc(e);
  for i := Low(Word) to High(Word) do begin
    uc := UnicodeChar(i);
    if (TCharacter.GetUnicodeCategory(uc) in
          [ TUnicodeCategory.ucUppercaseLetter, TUnicodeCategory.ucLowercaseLetter,
            TUnicodeCategory.ucTitlecaseLetter, TUnicodeCategory.ucModifierLetter,
            TUnicodeCategory.ucOtherLetter,
            TUnicodeCategory.ucDecimalNumber,TUnicodeCategory.ucLetterNumber
          ]
       )
    then begin
      if not TCharacter.IsLetterOrDigit(uc) then
        DoError(e,uc);
    end;
  end;

  Inc(e);
  for i := Low(Word) to High(Word) do begin
    uc := UnicodeChar(i);
    if not (TCharacter.GetUnicodeCategory(uc) in
              [ TUnicodeCategory.ucUppercaseLetter, TUnicodeCategory.ucLowercaseLetter,
                TUnicodeCategory.ucTitlecaseLetter, TUnicodeCategory.ucModifierLetter,
                TUnicodeCategory.ucOtherLetter,
                TUnicodeCategory.ucDecimalNumber,TUnicodeCategory.ucLetterNumber
              ]
           )
    then begin
      if TCharacter.IsLetterOrDigit(uc) then
        DoError(e,uc);
    end;
  end;

  WriteLn('ok');
end.
