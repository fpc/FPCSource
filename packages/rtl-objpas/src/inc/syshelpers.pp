{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2021 by Zeljko Avramovic (user avra in Lazarus forum)

    syshelpers - Type helpers for customizable boolean, binary and hexadecimal data internationalized string representation

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$IFNDEF FPC_DOTTEDUNITS}
unit syshelpers;
{$ENDIF FPC_DOTTEDUNITS}

{$mode objfpc}
{$H+}
{$modeswitch typehelpers}
{$modeswitch advancedrecords}
{$modeswitch allowinline}
{$macro on}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.Classes, System.SysUtils;
{$ELSE FPC_DOTTEDUNITS}
uses
  classes, sysutils;
{$ENDIF FPC_DOTTEDUNITS}

///////////////////////
//                   //
//  Format settings  //
//                   //
///////////////////////

type
  TStringCaseFormat = (scfUnchangedCase, scfLowerCase, scfUpperCase);

  TBitFormatSettings = record                        // for boolean to string conversion
    BitTrueString:      string;
    BitFalseString:     string;
    //
    BitOnString:        string;
    BitOffString:       string;
    //
    BitOneString:       string;
    BitZeroString:      string;
    //
    class operator Initialize(var aNewSettings: TBitFormatSettings);
    //
    procedure CopyToDefaultBoolStrings;
    procedure CopyToDefaultBitFormatSettings; inline;
  end;

  TBinFormatSettings = record                        // for number to binary conversion
    BinPrefixString:    string;
    BinSufixString:     string;
    BinNibbleSeparator: string;
    BinByteSeparator:   string;
    BinWordSeparator:   string;
    BinDwordSeparator:  string;
    //
    class operator Initialize(var aNewSettings: TBinFormatSettings);
    //
    procedure CopyToDefaultBinFormatSettings; inline;
  end;

  THexFormatSettings = record                        // for number to hex conversion
    HexPrefixString:    string;                      // $
    HexSufixString:     string;
    HexNibbleSeparator: string;                      // between hex digits
    HexByteSeparator:   string;                      // between byte pairs of hex digits
    HexWordSeparator:   string;                      // between word quads of hex digits
    HexDwordSeparator:  string;                      // between dword octets of hex digits
    //
    class operator Initialize(var aNewSettings: THexFormatSettings);
    //
    procedure CopyToDefaultHexFormatSettings; inline;
  end;

var
  DefaultBitFormatSettings: TBitFormatSettings;      // global boolean to string conversion defaults
  DefaultBinFormatSettings: TBinFormatSettings;      // global number to binary conversion defaults
  DefaultHexFormatSettings: THexFormatSettings;      // global number to hex conversion defaults

  // global easy access boolean to string conversion defaults
  BitOnString:          string absolute DefaultBitFormatSettings.BitOnString;
  BitOffString:         string absolute DefaultBitFormatSettings.BitOffString;
  //
  BitTrueString:        string absolute DefaultBitFormatSettings.BitTrueString;
  BitFalseString:       string absolute DefaultBitFormatSettings.BitFalseString;
  //
  BitOneString:         string absolute DefaultBitFormatSettings.BitOneString;
  BitZeroString:        string absolute DefaultBitFormatSettings.BitZeroString;

  // global easy access number to binary conversion defaults
  BinPrefixString:      string absolute DefaultBinFormatSettings.BinPrefixString;
  BinSufixString:       string absolute DefaultBinFormatSettings.BinSufixString;
  BinNibbleSeparator:   string absolute DefaultBinFormatSettings.BinNibbleSeparator;
  BinByteSeparator:     string absolute DefaultBinFormatSettings.BinByteSeparator;
  BinWordSeparator:     string absolute DefaultBinFormatSettings.BinWordSeparator;
  BinDwordSeparator:    string absolute DefaultBinFormatSettings.BinDwordSeparator;

  // global easy access number to hex conversion defaults
  HexPrefixString:      string absolute DefaultHexFormatSettings.HexPrefixString;
  HexSufixString:       string absolute DefaultHexFormatSettings.HexSufixString;
  HexNibbleSeparator:   string absolute DefaultHexFormatSettings.HexNibbleSeparator;
  HexByteSeparator:     string absolute DefaultHexFormatSettings.HexByteSeparator;
  HexWordSeparator:     string absolute DefaultHexFormatSettings.HexWordSeparator;
  HexDwordSeparator:    string absolute DefaultHexFormatSettings.HexDwordSeparator;

//////////////////////
//                  //
//  System helpers  //
//                  //
//////////////////////

type

  TByteSysHelper = type helper(TByteHelper) for Byte
    {$i syshelpersoh.inc}
  end;

  TShortIntSysHelper = type helper(TShortIntHelper) for ShortInt
    {$i syshelpersoh.inc}
  end;

  TWordSysHelper = type helper(TWordHelper) for Word
    {$i syshelpersoh.inc}
  end;

  TSmallIntSysHelper = type helper(TSmallIntHelper) for SmallInt
    {$i syshelpersoh.inc}
  end;

  TCardinalSysHelper = type helper(TCardinalHelper) for Cardinal
    {$i syshelpersoh.inc}
  end;

  TIntegerSysHelper = type helper(TIntegerHelper) for Integer
    {$i syshelpersoh.inc}
  end;

  TQwordSysHelper = type helper(TQwordHelper) for Qword
    {$i syshelpersoh.inc}
  end;

  TInt64SysHelper = type helper(TInt64Helper) for Int64
    {$i syshelpersoh.inc}
  end;

  TNativeIntSysHelper = type helper(TNativeIntHelper) for NativeInt
    {$i syshelpersoh.inc}
  end;

  TNativeUIntSysHelper = type helper(TNativeUIntHelper) for NativeUInt
    {$i syshelpersoh.inc}
  end;


  TBooleanSysHelper = type helper(TBooleanHelper) for Boolean
    {$i syshelpersbh.inc}
  end;

  TBoolean8SysHelper = type helper(TBoolean8Helper) for Boolean8
    {$i syshelpersbh.inc}
  end;

  TBoolean16SysHelper = type helper(TBoolean16Helper) for Boolean16
    {$i syshelpersbh.inc}
  end;

  TBoolean32SysHelper = type helper(TBoolean32Helper) for Boolean32
    {$i syshelpersbh.inc}
  end;

  TBoolean64SysHelper = type helper(TBoolean64Helper) for Boolean64
    {$i syshelpersbh.inc}
  end;

  TByteBoolSysHelper = type helper(TByteBoolHelper) for ByteBool
    {$i syshelpersbh.inc}
  end;

  TWordBoolSysHelper = type helper(TWordBoolHelper) for WordBool
    {$i syshelpersbh.inc}
  end;

  TLongBoolSysHelper = type helper(TLongBoolHelper) for LongBool
    {$i syshelpersbh.inc}
  end;

  TQWordBoolSysHelper = type helper(TQWordBoolHelper) for QWordBool
    {$i syshelpersbh.inc}
  end;


implementation

{$IFDEF FPC_DOTTEDUNITS}
uses System.SysConst;
{$ELSE FPC_DOTTEDUNITS}
uses sysconst;
{$ENDIF FPC_DOTTEDUNITS}

///////////////////////
//                   //
//  Format settings  //
//                   //
///////////////////////

class operator TBitFormatSettings.Initialize(var aNewSettings: TBitFormatSettings);
begin
  aNewSettings.BitTrueString  := 'True';
  aNewSettings.BitFalseString := 'False';
  //
  aNewSettings.BitOnString    := 'On';
  aNewSettings.BitOffString   := 'Off';
  //
  aNewSettings.BitOneString   := '1';
  aNewSettings.BitZeroString  := '0';
end;

procedure TBitFormatSettings.CopyToDefaultBoolStrings;
begin // without this call, new bit strings will not be used in BoolToStr() and TryStrToBool()
  if Length(TrueBoolStrs) < 3 then
    SetLength(TrueBoolStrs, 3);

  TrueBoolStrs[0] := Self.BitTrueString;   // used in BoolToStr() and TryStrToBool()
  TrueBoolStrs[1] := Self.BitOnString;     // used in TryStrToBool()
  TrueBoolStrs[2] := Self.BitOneString;    // used in TryStrToBool()

  If Length(FalseBoolStrs) < 3 then
    SetLength(FalseBoolStrs, 3);

  FalseBoolStrs[0] := Self.BitFalseString; // used in BoolToStr() and TryStrToBool()
  FalseBoolStrs[1] := Self.BitOffString;   // used in TryStrToBool()
  FalseBoolStrs[2] := Self.BitZeroString;  // used in TryStrToBool()
end;

procedure TBitFormatSettings.CopyToDefaultBitFormatSettings;
begin
  DefaultBitFormatSettings := Self;
end;

class operator TBinFormatSettings.Initialize(var aNewSettings: TBinFormatSettings);
begin
  aNewSettings  := Default(TBinFormatSettings);
end;

procedure TBinFormatSettings.CopyToDefaultBinFormatSettings;
begin
  DefaultBinFormatSettings := Self;
end;

class operator THexFormatSettings.Initialize(var aNewSettings: THexFormatSettings);
begin
  aNewSettings  := Default(THexFormatSettings);
end;

procedure THexFormatSettings.CopyToDefaultHexFormatSettings;
begin
  DefaultHexFormatSettings := Self;
end;


//////////////////////
//                  //
//  System helpers  //
//                  //
//////////////////////


{ ---------------------------------------------------------------------
  TBooleanSysHelper
  ---------------------------------------------------------------------}

{$define TBOOLHELPER:=TBooleanSysHelper}
{$define TBOOLTYPE:=Boolean}
{$i syshelpersb.inc}

{ ---------------------------------------------------------------------
  TBoolean8SysHelper
  ---------------------------------------------------------------------}

{$define TBOOLHELPER:=TBoolean8SysHelper}
{$define TBOOLTYPE:=Boolean8}
{$i syshelpersb.inc}

{ ---------------------------------------------------------------------
  TBoolean16SysHelper
  ---------------------------------------------------------------------}

{$define TBOOLHELPER:=TBoolean16SysHelper}
{$define TBOOLTYPE:=Boolean16}
{$i syshelpersb.inc}

{ ---------------------------------------------------------------------
  TBoolean32SysHelper
  ---------------------------------------------------------------------}

{$define TBOOLHELPER:=TBoolean32SysHelper}
{$define TBOOLTYPE:=Boolean32}
{$i syshelpersb.inc}

{ ---------------------------------------------------------------------
  TBoolean64SysHelper
  ---------------------------------------------------------------------}

{$define TBOOLHELPER:=TBoolean64SysHelper}
{$define TBOOLTYPE:=Boolean64}
{$i syshelpersb.inc}

{ ---------------------------------------------------------------------
  TByteBoolSysHelper
  ---------------------------------------------------------------------}

{$define TBOOLHELPER:=TByteBoolSysHelper}
{$define TBOOLTYPE:=ByteBool}
{$i syshelpersb.inc}

{ ---------------------------------------------------------------------
  TWordBoolSysHelper
  ---------------------------------------------------------------------}

{$define TBOOLHELPER:=TWordBoolSysHelper}
{$define TBOOLTYPE:=WordBool}
{$i syshelpersb.inc}

{ ---------------------------------------------------------------------
  TLongBoolSysHelper
  ---------------------------------------------------------------------}


{$define TBOOLHELPER:=TLongBoolSysHelper}
{$define TBOOLTYPE:=LongBool}
{$i syshelpersb.inc}

{ ---------------------------------------------------------------------
  TLongBoolSysHelper
  ---------------------------------------------------------------------}

{$define TBOOLHELPER:=TQWordBoolSysHelper}
{$define TBOOLTYPE:=QWordBool}
{$i syshelpersb.inc}


{ ---------------------------------------------------------------------
  TByteSysHelper
  ---------------------------------------------------------------------}

{$define TORDINALHELPER:=TByteSysHelper}
{$define TORDINALBITINDEX:=TByteBitIndex}
{$define TORDINALNIBBLEINDEX:=TByteNibbleIndex}
{$i syshelperso.inc}

{ ---------------------------------------------------------------------
  TShortintSysHelper
  ---------------------------------------------------------------------}

{$define TORDINALHELPER:=TShortIntSysHelper}
{$define TORDINALBITINDEX:=TShortIntBitIndex}
{$define TORDINALNIBBLEINDEX:=TShortIntNibbleIndex}
{$i syshelperso.inc}

{ ---------------------------------------------------------------------
  TSmallintSysHelper
  ---------------------------------------------------------------------}

{$define TORDINALHELPER:=TSmallIntSysHelper}
{$define TORDINALBITINDEX:=TSmallIntBitIndex}
{$define TORDINALNIBBLEINDEX:=TSmallIntNibbleIndex}
{$i syshelperso.inc}

{ ---------------------------------------------------------------------
  TWordSysHelper
  ---------------------------------------------------------------------}

{$define TORDINALHELPER:=TWordSysHelper}
{$define TORDINALBITINDEX:=TWordBitIndex}
{$define TORDINALNIBBLEINDEX:=TWordNibbleIndex}
{$i syshelperso.inc}

{ ---------------------------------------------------------------------
  TCardinalSysHelper
  ---------------------------------------------------------------------}

{$define TORDINALHELPER:=TCardinalSysHelper}
{$define TORDINALBITINDEX:=TCardinalBitIndex}
{$define TORDINALNIBBLEINDEX:=TCardinalNibbleIndex}
{$i syshelperso.inc}


{ ---------------------------------------------------------------------
  TIntegerSysHelper
  ---------------------------------------------------------------------}

{$define TORDINALHELPER:=TIntegerSysHelper}
{$define TORDINALBITINDEX:=TIntegerBitIndex}
{$define TORDINALNIBBLEINDEX:=TIntegerNibbleIndex}
{$i syshelperso.inc}


{ ---------------------------------------------------------------------
  TInt64SysHelper
  ---------------------------------------------------------------------}

{$define TORDINALHELPER:=TInt64SysHelper}
{$define TORDINALBITINDEX:=TInt64BitIndex}
{$define TORDINALNIBBLEINDEX:=TInt64NibbleIndex}
{$i syshelperso.inc}


{ ---------------------------------------------------------------------
  TQWordSysHelper
  ---------------------------------------------------------------------}

{$define TORDINALHELPER:=TQWordSysHelper}
{$define TORDINALBITINDEX:=TQwordBitIndex}
{$define TORDINALNIBBLEINDEX:=TQwordNibbleIndex}
{$i syshelperso.inc}


{ ---------------------------------------------------------------------
  TNativeIntSysHelper
  ---------------------------------------------------------------------}

{$define TORDINALHELPER:=TNativeIntSysHelper}
{$define TORDINALBITINDEX:=TNativeIntBitIndex}
{$define TORDINALNIBBLEINDEX:=TNativeIntNibbleIndex}
{$i syshelperso.inc}

{ ---------------------------------------------------------------------
  TNativeUIntSysHelper
  ---------------------------------------------------------------------}

{$define TORDINALHELPER:=TNativeUIntSysHelper}
{$define TORDINALBITINDEX:=TNativeUIntBitIndex}
{$define TORDINALNIBBLEINDEX:=TNativeUIntNibbleIndex}
{$i syshelperso.inc}

end.

