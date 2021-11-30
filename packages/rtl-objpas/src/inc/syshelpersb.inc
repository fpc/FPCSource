class function TBOOLHELPER.ToString(const aValue: boolean; const aTrueStr, aFalseStr: string; const aCharsCase: TStringCaseFormat = scfUnchangedCase): string; overload; static;
begin
  if aValue then
    case aCharsCase of
      scfLowerCase:        Result := aTrueStr.ToLower;
      scfUpperCase:        Result := aTrueStr.ToUpper;
    else
      {scfUnchangedCase:}  Result := aTrueStr;
    end
  else
    case aCharsCase of
      scfLowerCase:        Result := aFalseStr.ToLower;
      scfUpperCase:        Result := aFalseStr.ToUpper;
    else
      {scfUnchangedCase:}  Result := aFalseStr;
    end;
end;

function TBOOLHELPER.ToString(const aBitFormatSettings: TBitFormatSettings; const aCharsCase: TStringCaseFormat = scfUnchangedCase): string; overload; inline;
begin
  Result := ToString(Self, aBitFormatSettings.BitTrueString, aBitFormatSettings.BitFalseString, aCharsCase);
end;

function TBOOLHELPER.ToString(const aTrueStr, aFalseStr: string; const aCharsCase: TStringCaseFormat = scfUnchangedCase): string; overload; inline;
begin
  Result := ToString(Self, aTrueStr, aFalseStr, aCharsCase);
end;

function TBOOLHELPER.ToString(const aCharsCase: TStringCaseFormat = scfUnchangedCase): string; overload; inline;
begin
  Result := ToString(Self, BitTrueString, BitFalseString, aCharsCase);
end;

function TBOOLHELPER.ToOneZeroString(const aCharsCase: TStringCaseFormat = scfUnchangedCase): string; inline;
begin
  Result := ToString(BitOneString, BitZeroString, aCharsCase);
end;

function TBOOLHELPER.ToOnOffString(const aCharsCase: TStringCaseFormat = scfUnchangedCase): string; inline;
begin
  Result := ToString(BitOnString, BitOffString, aCharsCase);
end;

function TBOOLHELPER.ToTrueFalseString(const aCharsCase: TStringCaseFormat = scfUnchangedCase): string; inline;
begin
  Result := ToString(BitTrueString, BitFalseString, aCharsCase);
end;

