program tisseparator;

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

procedure CheckItems(AItems : array of Word; ADoCheck : Boolean; AError : Integer); overload;
var
  q : Integer;
  locItem : UnicodeChar;
begin
  for q := Low(AItems) to High(AItems) do begin
    locItem := UnicodeChar(AItems[q]);
    if TCharacter.IsSeparator(locItem) <> ADoCheck then
      DoError(AError,locItem);
  end;
end;

var
  e, i , k: Integer;
  uc : UnicodeChar;
begin  
  e := 1;
  CheckItems([$0020,$2028,$2029],True,e);

  Inc(e);
  CheckItems([$000A,$000C,$000D],False,e);

  Inc(e);
  for i := Low(Word) to High(Word) do begin
    uc := UnicodeChar(i);
    if (TCharacter.GetUnicodeCategory(uc) in
        [ TUnicodeCategory.ucSpaceSeparator,
          TUnicodeCategory.ucLineSeparator,
          TUnicodeCategory.ucParagraphSeparator
        ]
       )
    then begin
      if not TCharacter.IsSeparator(uc) then
        DoError(e,uc);
    end;
  end;

  Inc(e);
  for i := Low(Word) to High(Word) do begin
    uc := UnicodeChar(i);
    if not (TCharacter.GetUnicodeCategory(uc) in
            [ TUnicodeCategory.ucSpaceSeparator,
              TUnicodeCategory.ucLineSeparator,
              TUnicodeCategory.ucParagraphSeparator
            ]
           )
    then begin
      if TCharacter.IsSeparator(uc) then
        DoError(e,uc);
    end;
  end;

  WriteLn('ok');
end.
