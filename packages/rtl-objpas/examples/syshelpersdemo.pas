program syshelpersconsoledemo; // syshelpers demonstration of customizable boolean, binary and hexadecimal data localized string representation

uses
  {$IFDEF WINDOWS}windows,{$ENDIF}
  sysutils, syshelpers;

procedure Print(aValue: boolean);
begin
  if aValue then
    Write('TRUE  : ')
  else
    Write('FALSE : ');
  WriteLn(aValue.ToTrueFalseString, ', ', aValue.ToString, ', ', aValue.ToOneZeroString, ', ', aValue.ToOnOffString);
end;

var
  MyBitFormatSettings: TBitFormatSettings = (
    BitTrueString:  'MyTrue';
    BitFalseString: 'MyFalse';
    //
    BitOnString:    '<On>';
    BitOffString:   '<Off>';
    //
    BitOneString:   '-';
    BitZeroString:  '_';
  );

  OriginalBitFormatSettings: TBitFormatSettings;

  MyHexFormatSettings: THexFormatSettings = (
    HexPrefixString    : '$';
    HexSufixString     : '';
    HexNibbleSeparator : '';
    HexByteSeparator   : '';
    HexWordSeparator   : ':';
    HexDwordSeparator  : ':';
  );

  {$IFDEF WINDOWS}PreviousValue: uint;{$ENDIF}
  MyBool: boolean;
  MyDword: dword;
  MyQword: qword;
  MyNativeInt: NativeInt;

begin
  {$IFDEF WINDOWS}
    PreviousValue := GetConsoleOutputCP;
    DefaultSystemCodePage := CP_UTF8;
    SetConsoleOutputCP(CP_UTF8);
    SetTextCodePage(Output, CP_UTF8);
  {$ENDIF}

  WriteLn('True.ToString                                          = ', True.ToString);
  WriteLn('True.ToTrueFalseString(scfUpperCase)                   = ', True.ToTrueFalseString(scfUpperCase));
  WriteLn('True.ToString(''OnState'', ''OffState'')                   = ', True.ToString('OnState', 'OffState'));
  WriteLn('True.ToString(''Running'', ''Stopped'', scfUpperCase)      = ', True.ToString('Running', 'Stopped', scfUpperCase));
  WriteLn('True.ToString(''Ради'', ''Не ради'', scfUpperCase)         = ', True.ToString('Ради', 'Не ради', scfUpperCase), ' (console uppers ASCII only, gui uppers unicode as well)');
  WriteLn('True.ToString(MyBitFormatSettings)                     = ', True.ToString((MyBitFormatSettings)));

  MyNativeInt := -10000;
  WriteLn('MyNativeInt                                            = ', MyNativeInt);
  WriteLn('MyNativeInt.ToBinString                                = ', MyNativeInt.ToBinString);
  WriteLn('MyNativeInt.Bits[MyNativeInt.MaxBit].ToTrueFalseString = ', MyNativeInt.Bits[MyNativeInt.MaxBit].ToTrueFalseString);

  MyDword            := %111000111000111000111;
  MyDword.Words[0]   := 77;                 // ordinary type helpers have been extended
  MyDword.Bits[9]    := true;               // ordinary type helpers have been extended
  MyDword.Nibbles[0] := $A;                 // ordinary type helpers have been extended
  TDwordOverlay(MyDword).AsNibble[0] := $A; // the same as MyDword.Nibbles[0] := $A
  WriteLn('MyDword.ToBinString          = ', MyDword.ToBinString);
  WriteLn('MyDword.ToBinString(true)    = ', MyDword.ToBinString(true));
  WriteLn('MyDword.ToBinString(false)   = ', MyDword.ToBinString(false));
  BinNibbleSeparator := '_';
  WriteLn('MyDword.Bytes[0].ToBinString = ', MyDword.Bytes[0].ToBinString, '   (BinNibbleSeparator = "_")');
  BinNibbleSeparator := '';
  // MyQword := 0; // Beware: when value is 0, result of HighestSetBitPos and LowestSetBitPos will be -1
  WriteLn('MyDword.HighestSetBitPos     = ' + MyDword.HighestSetBitPos.ToString);
  WriteLn('MyDword.LowestSetBitPos      = ' + MyDword.LowestSetBitPos.ToString);
  WriteLn('MyDword.SetBitsCount         = ' + MyDword.SetBitsCount.ToString);
  WriteLn('MyDword.ToHexString          = ', MyDword.ToHexString);
  WriteLn('MyDword.ToHexString(7)       = ', MyDword.ToHexString(7));
  WriteLn('MyDword.ToHexString(false)   = ', MyDword.ToHexString(false));
  WriteLn;

  WriteLn('DefaultBitFormatSettings:');
  Print(False);
  Print(True);

  WriteLn('MyDword.Bits[5].ToTrueFalseString = ', MyDword.Bits[5].ToTrueFalseString, ' (BitTrueString = ', BitTrueString, ')');
  WriteLn('MyDword.Bits[5].ToString          = ', MyDword.Bits[5].ToString{, ' ', BitTrueString});
  WriteLn('TryStrToBool(''trUE'', MyBool)      = ', TryStrToBool('trUE', MyBool){.ToString, ' ', MyBool});
  WriteLn('BoolToStr(true, true)             = ', BoolToStr(true, true), ', TrueBoolStrs[0] = ', TrueBoolStrs[0]);
  WriteLn;

  with MyBitFormatSettings do
  begin
    BitTrueString  := 'Истина';                 // This changes boolean to string text outputs in syshelpers and some other places (TryStrToBool compares string against upcase[BitTrueString, BitOnString, BitOneString], but upcase() is by default good only for ASCII on console, while GUI is good for unicode)
    BitFalseString := 'Неистина';               // This changes boolean to string text outputs in syshelpers and some other places (TryStrToBool compares string against upcase[BitFalseString, BitOffString, BitZeroString], but upcase() is by default good only for ASCII on console, while GUI is good for unicode)
    BitOnString    := 'Укључено';
    BitOffString   := 'Искључено';
    BitOneString   := 'X';
    BitZeroString  := 'O';
  end;
  MyBitFormatSettings.CopyToDefaultBoolStrings; // This changes boolean to string text outputs in syshelpers and some other places (TryStrToBool compares string against upcase[BitTrueString, BitOnString, BitOneString], but upcase() is by default good only for ASCII on console, while GUI is good for unicode)
  MyBitFormatSettings.CopyToDefaultBitFormatSettings;

  WriteLn('CustomBitFormatSettings:');
  Print(False);
  Print(True);

  WriteLn('MyDword.Bits[5].ToTrueFalseString = ', MyDword.Bits[5].ToTrueFalseString, ' (BitTrueString = ', BitTrueString, ')');
  WriteLn('MyDword.Bits[5].ToString          = ', MyDword.Bits[5].ToString{, ', BitTrueString = ', BitTrueString});
  WriteLn('TryStrToBool(''Истина'', MyBool)    = ', TryStrToBool('Истина', MyBool).ToString, ', (MyBool = ', MyBool, ')');
  WriteLn('TryStrToBool(''ИСТИна'', MyBool)    = ', TryStrToBool('ИСТИна', MyBool).ToString, ' (console uppers ASCII only, gui uppers unicode as well)');
  WriteLn('BoolToStr(true, true)             = ', BoolToStr(true, true), ', TrueBoolStrs[0] = ', TrueBoolStrs[0]);
  WriteLn('True.ToString                     = ', True.ToString);
  WriteLn;

  MyQword := $ABCDEFFFFFF;

  OriginalBitFormatSettings.CopyToDefaultBitFormatSettings;
  OriginalBitFormatSettings.CopyToDefaultBoolStrings;

  WriteLn('DefaultBinFormatSettings:');
  WriteLn('MyQword.ToBinString              = ',                     MyQword.ToBinString);
  WriteLn('MyQword.ToBinString(false)       =                     ', MyQword.ToBinString(false));
  WriteLn('MyQword.ToBinString(50)          =               ',       MyQword.ToBinString(50));
  WriteLn;

  // BinNibbleSeparator := '^';
  BinByteSeparator   := '.';
  BinWordSeparator   := '-';
  BinDwordSeparator  := '_';
  BitZeroString      := 'O';
  BitOneString       := 'X';

  WriteLn('CustomBinFormatSettings:');
  WriteLn('MyQword.ToBinString       = ',                       MyQword.ToBinString);
  WriteLn('MyQword.ToBinString(false)       =                ', MyQword.ToBinString(false));
  WriteLn('MyQword.ToBinString(50)          =         ',        MyQword.ToBinString(50));
  WriteLn;

  WriteLn('DefaultHexFormatSettings:');
  WriteLn('MyQword.ToHexString                             = ',      MyQword.ToHexString);
  WriteLn('MyQword.ToHexString(3)                          =      ', MyQword.ToHexString(3));
  WriteLn('MyQword.ToHexString(13)                         =    ',   MyQword.ToHexString(13));
  WriteLn('MyQword.ToHexString(true)                       = ',      MyQword.ToHexString(true));
  WriteLn('MyQword.ToHexString(false)                      =      ', MyQword.ToHexString(false));
  WriteLn;

  HexPrefixString    := '[$';
  HexSufixString     := ']';
  HexNibbleSeparator := '.';
  HexByteSeparator   := '^';
  HexWordSeparator   := '_';
  HexDwordSeparator  := '--';

  WriteLn('CustomHexFormatSettings1:');
  WriteLn('MyQword.ToHexString                             = ',           MyQword.ToHexString);
  WriteLn('MyQword.ToHexString(true)                       = ',           MyQword.ToHexString(true));
  WriteLn('MyQword.ToHexString(false)                      =           ', MyQword.ToHexString(false));
  WriteLn;

  WriteLn('CustomHexFormatSettings2:');
  WriteLn('MyQword.ToHexString(MyHexFormatSettings)        = ',       MyQword.ToHexString(MyHexFormatSettings));
  WriteLn('MyQword.ToHexString(MyHexFormatSettings, false) =       ', MyQword.ToHexString(MyHexFormatSettings, false));
  WriteLn('MyQword.ToHexString(MyHexFormatSettings, 13)    =    ',    MyQword.ToHexString(MyHexFormatSettings, 13));
  WriteLn;

  {$IFDEF WINDOWS}
    WriteLn('Press Enter...');
    ReadLn;
    SetConsoleOutputCP(PreviousValue);
  {$ENDIF}
end.
