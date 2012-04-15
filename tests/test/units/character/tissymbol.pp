program tissymbol;

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
    if TCharacter.IsSymbol(locItem) <> ADoCheck then
      DoError(AError,locItem);
  end;
end;

procedure CheckItems(AItems : array of UnicodeChar; ADoCheck : Boolean; AError : Integer); overload;
var
  q : Integer;
  locItem : UnicodeChar;
begin
  for q := Low(AItems) to High(AItems) do begin
    locItem := AItems[q];
    if TCharacter.IsSymbol(locItem) <> ADoCheck then
      DoError(AError,locItem);
  end;
end;

procedure CheckItems(AStart, AEnd : Word; ADoCheck : Boolean; AError : Integer); overload;
var
  q : Integer;
  locItem : UnicodeChar;
begin
  for q := AStart to AEnd do begin
    locItem := UnicodeChar(q);
    if TCharacter.IsSymbol(locItem) <> ADoCheck then
      DoError(AError,locItem);
  end;
end;

var
  e, i , k: Integer;
  uc : UnicodeChar;
begin  
  e := 1;
  //Currency
  CheckItems([$0024,$00A2,$00A3,$00A4,$00A5,$060B,$09F2,$09F3],True,e);
  CheckItems([$09FB,$0AF1,$0BF9,$0E3F,$17DB],True,e);
  CheckItems([$A838,$FDFC],True,e);
  CheckItems([$20A4,$20AC],True,e);
  CheckItems($FFE0,$FFE6,True,e);
  //Letterlike symbol
  Inc(e);
//  CheckItems($2100,$214F,True,e);

  Inc(e);
  for i := Low(Word) to High(Word) do begin
    uc := UnicodeChar(i);
    if (TCharacter.GetUnicodeCategory(uc) in
        [ TUnicodeCategory.ucMathSymbol,
          TUnicodeCategory.ucCurrencySymbol,
          TUnicodeCategory.ucModifierSymbol,
          TUnicodeCategory.ucOtherSymbol
        ]
       )
    then begin
      if not TCharacter.IsSymbol(uc) then
        DoError(e,uc);
    end;
  end;

  Inc(e);
  for i := Low(Word) to High(Word) do begin
    uc := UnicodeChar(i);
    if not (TCharacter.GetUnicodeCategory(uc) in
            [ TUnicodeCategory.ucMathSymbol,
              TUnicodeCategory.ucCurrencySymbol,
              TUnicodeCategory.ucModifierSymbol,
              TUnicodeCategory.ucOtherSymbol
            ]
           )
    then begin
      if TCharacter.IsSymbol(uc) then
        DoError(e,uc);
    end;
  end;

  WriteLn('ok');
end.
