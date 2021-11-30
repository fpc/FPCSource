function TORDINALHELPER.ToBinString(const aBinFormatSettings: TBinFormatSettings; const aShowLeadingZeros: boolean; const aMinDigits: integer): string; overload;
var
  BitCounter: TORDINALBITINDEX;
  LeadingZeros: boolean;
begin
  Result := aBinFormatSettings.BinPrefixString;
  LeadingZeros := true;
  for BitCounter := MaxBit downto MinBit do
  begin
    LeadingZeros := LeadingZeros and not Self.Bits[BitCounter];
    if aShowLeadingZeros or (not LeadingZeros) or (BitCounter = MinBit) or (BitCounter < aMinDigits) then
    begin
      if BitCounter <> MaxBit then // time for separator ?
        if ((BitCounter + 1) mod 32 = 0) then // every 32 bits we have a dword
          Result := Result + aBinFormatSettings.BinDwordSeparator
        else
          if ((BitCounter + 1) mod 16 = 0) then // every 16 bits we have a word
            Result := Result + aBinFormatSettings.BinWordSeparator
          else
            if ((BitCounter + 1) mod 8 = 0) then // every 8 bits we have a byte
              Result := Result + aBinFormatSettings.BinByteSeparator
            else
              if ((BitCounter + 1) mod 4 = 0) then // every 4 bits we have a nibble
                Result := Result + aBinFormatSettings.BinNibbleSeparator;
      Result := Result + Self.Bits[BitCounter].ToOneZeroString;
    end;
  end;
  Result := Result + aBinFormatSettings.BinSufixString;
end;

function TORDINALHELPER.ToBinString(const aBinFormatSettings: TBinFormatSettings; const aMinDigits: integer): string; overload;
begin
  Result := ToBinString(aBinFormatSettings, false, aMinDigits);
end;

function TORDINALHELPER.ToBinString(const aBinFormatSettings: TBinFormatSettings; aShowLeadingZeros: boolean = true): string; overload;
begin
  Result := ToBinString(aBinFormatSettings, aShowLeadingZeros, 0);
end;

function TORDINALHELPER.ToBinString(const aMinDigits: integer): string; overload;
begin
  Result := ToBinString(DefaultBinFormatSettings, false, aMinDigits);
end;

function TORDINALHELPER.ToBinString(const aShowLeadingZeros: boolean = true): string; overload;
begin
  Result := ToBinString(DefaultBinFormatSettings, aShowLeadingZeros, 0);
end;

function TORDINALHELPER.ToHexString(const aHexFormatSettings: THexFormatSettings; const aShowLeadingZeros: boolean; const aMinDigits: integer): string; overload;
var
  NibbleCounter: TORDINALNIBBLEINDEX;
  LeadingZeros, PassedFirstNibble: boolean;
begin
  Result := aHexFormatSettings.HexPrefixString;
  LeadingZeros := true;
  PassedFirstNibble := false;
  for NibbleCounter := MaxNibble downto MinNibble do
  begin
    LeadingZeros := LeadingZeros and (Self.Nibbles[NibbleCounter] = 0);
    if aShowLeadingZeros or (not LeadingZeros) or (NibbleCounter = MinNibble) or (NibbleCounter < aMinDigits) then
    begin
      if PassedFirstNibble then // time for separator ?
        if ((NibbleCounter + 1) mod 8 = 0) then // every 8 nibbles we have a dword
          Result := Result + aHexFormatSettings.HexDwordSeparator
        else
          if ((NibbleCounter + 1) mod 4 = 0) then // every 4 nibbles we have a word
            Result := Result + aHexFormatSettings.HexWordSeparator
          else
            if ((NibbleCounter + 1) mod 2 = 0) then // every 2 nibbles we have a byte
              Result := Result + aHexFormatSettings.HexByteSeparator
            else
              if PassedFirstNibble then
                Result := Result + aHexFormatSettings.HexNibbleSeparator;
      Result := Result + HexDigits[Self.Nibbles[NibbleCounter]];
      PassedFirstNibble := true;
    end;
  end;
  Result := Result + aHexFormatSettings.HexSufixString;
end;

function TORDINALHELPER.ToHexString(const aHexFormatSettings: THexFormatSettings; const aMinDigits: integer): string; overload;
begin
  Result := ToHexString(aHexFormatSettings, false, aMinDigits);
end;

function TORDINALHELPER.ToHexString(const aHexFormatSettings: THexFormatSettings; aShowLeadingZeros: boolean = true): string; overload;
begin
  Result := ToHexString(aHexFormatSettings, aShowLeadingZeros, 0);
end;

function TORDINALHELPER.ToHexString(const aMinDigits: integer): string; overload;
begin
  Result := ToHexString(DefaultHexFormatSettings, false, aMinDigits);
end;

function TORDINALHELPER.ToHexString(const aShowLeadingZeros: boolean = true): string; overload;
begin
  Result := ToHexString(DefaultHexFormatSettings, aShowLeadingZeros, 0);
end;

