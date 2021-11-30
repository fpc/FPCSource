public
  class function ToString(const aValue: boolean; const aTrueStr, aFalseStr: string; const aCharsCase: TStringCaseFormat = scfUnchangedCase): string; overload; static;
public
  function ToString(const aBitFormatSettings: TBitFormatSettings; const aCharsCase: TStringCaseFormat = scfUnchangedCase): string; overload; inline;
  function ToString(const aTrueStr, aFalseStr: string; const aCharsCase: TStringCaseFormat = scfUnchangedCase): string; overload; inline;
  function ToString(const aCharsCase: TStringCaseFormat = scfUnchangedCase): string; overload; inline;
  function ToTrueFalseString(const aCharsCase: TStringCaseFormat = scfUnchangedCase): string; inline;
  function ToOnOffString(const aCharsCase: TStringCaseFormat = scfUnchangedCase): string; inline;
  function ToOneZeroString(const aCharsCase: TStringCaseFormat = scfUnchangedCase): string; inline;

