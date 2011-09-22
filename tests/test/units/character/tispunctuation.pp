program tispunctuation;

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

procedure CheckItems(AStart, AEnd : Word; ADoCheck : Boolean; AError : Integer); overload;
var
  q : Integer;
  locItem : UnicodeChar;
begin
  for q := AStart to AEnd do begin
    locItem := UnicodeChar(q);
    if TCharacter.IsPunctuation(locItem) <> ADoCheck then
      DoError(AError,locItem);
  end;
end;

procedure CheckItems(AItems : array of Word; ADoCheck : Boolean; AError : Integer); overload;
var
  q : Integer;
  locItem : UnicodeChar;
begin
  for q := Low(AItems) to High(AItems) do begin
    locItem := UnicodeChar(AItems[q]);
    if TCharacter.IsPunctuation(locItem) <> ADoCheck then
      DoError(AError,locItem);
  end;
end;

var
  e, i , k: Integer;
  uc : UnicodeChar;
begin  
  e := 1;
  CheckItems($0021,$0023,True,e);
  CheckItems($0025,$002A,True,e);
  CheckItems($002C,$002F,True,e);
  CheckItems($003A,$003B,True,e);
  CheckItems($003F,$0040,True,e);
  CheckItems($005B,$005D,True,e);

  CheckItems([$005F,$007B,$007D,$00A1,$00AB,{ $00AD,}$00B7,$00BB,$00BF,$037E],True,e);
  CheckItems($055A,$055F,True,e);
  CheckItems([$0589,$058A],True,e);

  CheckItems($FF5F,$FF65,True,e);

  Inc(e);
  for i := Low(Word) to High(Word) do begin
    uc := UnicodeChar(i);
    if (TCharacter.GetUnicodeCategory(uc) in
        [ TUnicodeCategory.ucConnectPunctuation, TUnicodeCategory.ucDashPunctuation,
          TUnicodeCategory.ucOpenPunctuation, TUnicodeCategory.ucClosePunctuation,
          TUnicodeCategory.ucInitialPunctuation, TUnicodeCategory.ucFinalPunctuation,
          TUnicodeCategory.ucOtherPunctuation
        ]
       )
    then begin
      if not TCharacter.IsPunctuation(uc) then
        DoError(e,uc);
    end;
  end;

  WriteLn('ok');
end.
