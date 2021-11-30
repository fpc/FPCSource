protected
  function ToBinString(const aBinFormatSettings: TBinFormatSettings; const aShowLeadingZeros: boolean; const aMinDigits: integer): string; overload;
  function ToHexString(const aHexFormatSettings: THexFormatSettings; const aShowLeadingZeros: boolean; const aMinDigits: integer): string; overload;
public
  function ToBinString(const aBinFormatSettings: TBinFormatSettings; const aMinDigits: integer): string; overload; inline;
  function ToBinString(const aBinFormatSettings: TBinFormatSettings; aShowLeadingZeros: boolean = true): string; overload; inline;
  function ToBinString(const aMinDigits: integer): string; overload; inline;
  function ToBinString(const aShowLeadingZeros: boolean = true): string; overload; inline;
  function ToHexString(const aHexFormatSettings: THexFormatSettings; const aMinDigits: integer): string; overload; inline;
  function ToHexString(const aHexFormatSettings: THexFormatSettings; aShowLeadingZeros: boolean = true): string; overload; inline;
  function ToHexString(const aMinDigits: integer): string; overload; inline;
  function ToHexString(const aShowLeadingZeros: boolean = true): string; overload; inline;
