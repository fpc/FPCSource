{ %norun }

{$mode delphi}

uses
  sysutils;

{$define use_inline }

function IndyMin(const AValueOne, AValueTwo: Int32): Int32;
{$IFDEF USE_INLINE}inline;{$ENDIF} overload;
begin
  if AValueOne > AValueTwo then begin
    Result := AValueTwo;
  end else begin
    Result := AValueOne;
  end;
end;

function IndyMin(const AValueOne, AValueTwo: Int64): Int64;
{$IFDEF USE_INLINE}inline;{$ENDIF} overload;
begin
  if AValueOne > AValueTwo then begin
    Result := AValueTwo;
  end else begin
    Result := AValueOne;
  end;
end;

function IndyMin(const AValueOne, AValueTwo: UInt16): UInt16;
{$IFDEF USE_INLINE}inline;{$ENDIF} overload;
begin
  if AValueOne > AValueTwo then begin
    Result := AValueTwo;
  end else begin
    Result := AValueOne;
  end;
end;


function IndyMax(const AValueOne, AValueTwo: Int64): Int64;
{$IFDEF USE_INLINE}inline;{$ENDIF} overload;
begin
  if AValueOne < AValueTwo then begin
    Result := AValueTwo;
  end else begin
    Result := AValueOne;
  end;
end;

function IndyMax(const AValueOne, AValueTwo: Int32): Int32;
{$IFDEF USE_INLINE}inline;{$ENDIF} overload;
begin
  if AValueOne < AValueTwo then begin
    Result := AValueTwo;
  end else begin
    Result := AValueOne;
  end;
end;

function IndyMax(const AValueOne, AValueTwo: UInt16): UInt16;
{$IFDEF USE_INLINE}inline;{$ENDIF} overload;
begin
  if AValueOne < AValueTwo then begin
    Result := AValueTwo;
  end else begin
    Result := AValueOne;
  end;
end;



function IndyLength(const ABuffer: String; const ALength: Integer = -1; const AIndex: Integer = 1): Integer;
{$IFDEF USE_INLINE}inline;{$ENDIF}
var
  LAvailable: Integer;
begin
  Assert(AIndex >= 1);
  LAvailable := IndyMax(Length(ABuffer)-AIndex+1, 0);
  if ALength < 0 then begin
    Result := LAvailable;
  end else begin
    Result := IndyMin(LAvailable, ALength);
  end;
end;


function CharEquals(const AString: string; const ACharPos: Integer; const AValue: Char): Boolean;
{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  if ACharPos < 1 then begin
    raise Exception.Create('Invalid ACharPos');{ do not localize }
  end;
  Result := ACharPos <= Length(AString);
  if Result then begin
    Result := AString[ACharPos] = AValue;
  end;
end;


{$HINTS OFF}
function IsNumeric(const AString: string): Boolean; overload;
var
  LCode: Integer;
  LVoid: Int64;
begin
  Val(AString, LVoid, LCode);
  Result := LCode = 0;
end;
{$HINTS ON}

function IsNumeric(const AString: string; const ALength: Integer; const AIndex: Integer = 1): Boolean; overload;
var
  I: Integer;
  LLen: Integer;
begin
  Result := False;
  LLen := IndyLength(AString, ALength, AIndex);
  if LLen > 0 then begin
    for I := 0 to LLen-1 do begin
      if not IsNumeric(AString[AIndex+i]) then begin
        Exit;
      end;
    end;
    Result := True;
  end;
end;

function IsNumeric(const AChar: Char): Boolean; overload;
{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  // TODO: under XE3.5+, use TCharHelper.IsDigit() instead
  // TODO: under D2009+, use TCharacter.IsDigit() instead

  // Do not use IsCharAlpha or IsCharAlphaNumeric - they are Win32 routines
  Result := (AChar >= '0') and (AChar <= '9'); {Do not Localize}
end;


function StripNo(const AData : String): String; inline;
var
  i : Integer;
  LPos : Integer;
begin
  LPos := 1;
  for i := 1 to Length(AData) do begin
    LPos := i;
    if (not IsNumeric(AData[i])) and (not CharEquals(AData, i, ',')) then begin
      Break;
    end;
  end;
  Result := Copy(AData, LPos, Length(AData));
end;

function TextStartsWith(const S, SubS: string): Boolean;
var
  LLen: Integer;
  {$IFDEF WINDOWS}
    {$IFDEF COMPARE_STRING_MISMATCH}
  LS, LSubS: {$IFDEF WINCE}TIdUnicodeString{$ELSE}TIdPlatformString{$ENDIF};
  P1, P2: {$IFDEF WINCE}PIdWideChar{$ELSE}PIdPlatformChar{$ENDIF};
    {$ENDIF}
  {$ENDIF}
begin
  LLen := Length(SubS);
  Result := LLen <= Length(S);
  if Result then
  begin
    {$IFDEF DOTNET}
    Result := System.String.Compare(S, 0, SubS, 0, LLen, True) = 0;
    {$ELSE}
      {$IFDEF WINDOWS}
        {$IFDEF COMPARE_STRING_MISMATCH}
    // explicit convert to Ansi/Unicode
    LS := {$IFDEF WINCE}TIdUnicodeString{$ELSE}TIdPlatformString{$ENDIF}(S);
    LSubS := {$IFDEF WINCE}TIdUnicodeString{$ELSE}TIdPlatformString{$ENDIF}(SubS);
    LLen := Length(LSubS);
    Result := LLen <= Length(LS);
    if Result then begin
      P1 := {$IFDEF WINCE}PIdWideChar{$ELSE}PIdPlatformChar{$ENDIF}(LS);
      P2 := {$IFDEF WINCE}PIdWideChar{$ELSE}PIdPlatformChar{$ENDIF}(LSubS);
      Result := CompareString(LOCALE_USER_DEFAULT, NORM_IGNORECASE, P1, LLen, P2, LLen) = 2;
    end;
        {$ELSE}
    Result := CompareString(LOCALE_USER_DEFAULT, NORM_IGNORECASE, PChar(S), LLen, PChar(SubS), LLen) = 2;
        {$ENDIF}
      {$ELSE}
    Result := AnsiCompareText(Copy(S, 1, LLen), SubS) = 0;
      {$ENDIF}
    {$ENDIF}
  end;
end;

procedure IdDelete(var s: string; AOffset, ACount: Integer);
{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  Delete(s, AOffset, ACount);
end;

function TextEndsWith(const S, SubS: string): Boolean;
var
  LLen: Integer;
  {$IFDEF WINDOWS}
    {$IFDEF COMPARE_STRING_MISMATCH}
  LS, LSubS: {$IFDEF WINCE}TIdUnicodeString{$ELSE}TIdPlatformString{$ENDIF};
  P1, P2: {$IFDEF WINCE}PIdWideChar{$ELSE}PIdPlatformChar{$ENDIF};
    {$ELSE}
  P: PChar;
    {$ENDIF}
  {$ENDIF}
begin
  LLen := Length(SubS);
  Result := LLen <= Length(S);
  if Result then
  begin
    {$IFDEF DOTNET}
    Result := System.String.Compare(S, Length(S)-LLen, SubS, 0, LLen, True) = 0;
    {$ELSE}
      {$IFDEF WINDOWS}
        {$IFDEF COMPARE_STRING_MISMATCH}
    // explicit convert to Ansi/Unicode
    LS := {$IFDEF WINCE}TIdUnicodeString{$ELSE}TIdPlatformString{$ENDIF}(S);
    LSubS := {$IFDEF WINCE}TIdUnicodeString{$ELSE}TIdPlatformString{$ENDIF}(SubS);
    LLen := Length(LSubS);
    Result := LLen <= Length(S);
    if Result then begin
      P1 := {$IFDEF WINCE}PIdWideChar{$ELSE}PIdPlatformChar{$ENDIF}(LS);
      P2 := {$IFDEF WINCE}PIdWideChar{$ELSE}PIdPlatformChar{$ENDIF}(LSubS);
      Inc(P1, Length(LS)-LLen);
      Result := CompareString(LOCALE_USER_DEFAULT, NORM_IGNORECASE, P1, LLen, P2, LLen) = 2;
    end;
        {$ELSE}
    P := PChar(S);
    Inc(P, Length(S)-LLen);
    Result := CompareString(LOCALE_USER_DEFAULT, NORM_IGNORECASE, P, LLen, PChar(SubS), LLen) = 2;
        {$ENDIF}
      {$ELSE}
    Result := AnsiCompareText(Copy(S, Length(S)-LLen+1, LLen), SubS) = 0;
      {$ENDIF}
    {$ENDIF}
  end;
end;

const
  IdFetchDelimDefault = ' ';    {Do not Localize}
  IdFetchDeleteDefault = True;
  IdFetchCaseSensitiveDefault = True;

function FetchCaseInsensitive(var AInput: string; const ADelim: string;
  const ADelete: Boolean): string;
{$IFDEF USE_INLINE}inline;{$ENDIF}
var
  LPos: Integer;
begin
  if ADelim = #0 then begin
    // AnsiPos does not work with #0
    LPos := Pos(ADelim, AInput);
  end else begin
    //? may be AnsiUpperCase?
    LPos := Pos(UpperCase(ADelim), UpperCase(AInput));
  end;
  if LPos = 0 then begin
    Result := AInput;
    if ADelete then begin
      AInput := '';    {Do not Localize}
    end;
  end else begin
    Result := Copy(AInput, 1, LPos - 1);
    if ADelete then begin
      //faster than Delete(AInput, 1, LPos + Length(ADelim) - 1); because the
      //remaining part is larger than the deleted
      AInput := Copy(AInput, LPos + Length(ADelim), MaxInt);
    end;
  end;
end;

function Fetch(var AInput: string; const ADelim: string = IdFetchDelimDefault;
  const ADelete: Boolean = IdFetchDeleteDefault;
	  const ACaseSensitive: Boolean = IdFetchCaseSensitiveDefault): string;
{$IFDEF USE_INLINE}inline;{$ENDIF}
var
  LPos: Integer;
begin
  if ACaseSensitive then begin
    if ADelim = #0 then begin
      // AnsiPos does not work with #0
      LPos := Pos(ADelim, AInput);
    end else begin
      LPos := Pos(ADelim, AInput);
    end;
    if LPos = 0 then begin
      Result := AInput;
      if ADelete then begin
        AInput := '';    {Do not Localize}
      end;
    end
    else begin
      Result := Copy(AInput, 1, LPos - 1);
      if ADelete then begin
        //slower Delete(AInput, 1, LPos + Length(ADelim) - 1); because the
        //remaining part is larger than the deleted
        AInput := Copy(AInput, LPos + Length(ADelim), MaxInt);
      end;
    end;
  end else begin
    Result := FetchCaseInsensitive(AInput, ADelim, ADelete);
  end;
end;

function ExtractRecFormat(const ARecFM : String): String;
  {$IFDEF USE_INLINE} inline; {$ENDIF}
begin
  Result := ARecFM;
  if TextStartsWith(Result, '<') then begin
    IdDelete(Result, 1, 1);
  end;
  if TextEndsWith(Result, '>') then begin
    Result := Fetch(Result, '>');
  end;
end;


procedure test;
var
  LTmp: string;
  s: string;
begin
  LTmp:='ac';
  s:=ExtractRecFormat(StripNo(LTmp));
end;

begin
end.
