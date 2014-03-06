unit uthlp;

{$ifdef fpc}
  {$mode delphi}{$H+}
  {$modeswitch typehelpers}
{$endif}

interface

type
  TTestEnum = (
    teOne,
    teTwo,
    teThree
  );
  TTestSet = set of TTestEnum;
  TTestArray = array of LongInt;
  MyLongInt = type LongInt;

  TUInt8Helper = record helper for UInt8
    function Test: LongInt;
    constructor Create(aArg: UInt8);
  end;

  TUInt16Helper = record helper for UInt16
    function Test: LongInt;
    constructor Create(aArg: UInt16);
  end;

  TUInt32Helper = record helper for UInt32
    function Test: LongInt;
    constructor Create(aArg: UInt32);
  end;

  TUInt64Helper = record helper for UInt64
    function Test: LongInt;
    constructor Create(aArg: UInt64);
  end;

  TInt8Helper = record helper for Int8
    function Test: LongInt;
    constructor Create(aArg: Int8);
  end;

  TInt16Helper = record helper for Int16
    function Test: LongInt;
    constructor Create(aArg: Int16);
  end;

  TInt32Helper = record helper for Int32
    function Test: LongInt;
    constructor Create(aArg: Int32);
  end;

  TInt64Helper = record helper for Int64
    function Test: LongInt;
    constructor Create(aArg: Int64);
  end;

  TBooleanHelper = record helper for Boolean
    function Test: LongInt;
    constructor Create(aArg: Boolean);
  end;

  TBoolean16Helper = record helper for Boolean16
    function Test: LongInt;
    constructor Create(aArg: Boolean16);
  end;

  TBoolean32Helper = record helper for Boolean32
    function Test: LongInt;
    constructor Create(aArg: Boolean32);
  end;

  TBoolean64Helper = record helper for Boolean64
    function Test: LongInt;
    constructor Create(aArg: Boolean64);
  end;

  TByteBoolHelper = record helper for ByteBool
    function Test: LongInt;
    constructor Create(aArg: ByteBool);
  end;

  TWordBoolHelper = record helper for WordBool
    function Test: LongInt;
    constructor Create(aArg: WordBool);
  end;

  TLongBoolHelper = record helper for LongBool
    function Test: LongInt;
    constructor Create(aArg: LongBool);
  end;

  TQWordBoolHelper = record helper for QWordBool
    function Test: LongInt;
    constructor Create(aArg: QWordBool);
  end;

  TShortStringHelper = record helper for ShortString
    function Test: LongInt;
    constructor Create(aArg: ShortString);
  end;

  TAnsiStringHelper = record helper for AnsiString
    function Test: LongInt;
    constructor Create(aArg: AnsiString);
  end;

  TWideStringHelper = record helper for WideString
    function Test: LongInt;
    constructor Create(aArg: WideString);
  end;

  TUnicodeStringHelper = record helper for UnicodeString
    function Test: LongInt;
    constructor Create(aArg: UnicodeString);
  end;

  TAnsiCharHelper = record helper for AnsiChar
    function Test: LongInt;
    constructor Create(aArg: AnsiChar);
  end;

  TWideCharHelper = record helper for WideChar
    function Test: LongInt;
    constructor Create(aArg: WideChar);
  end;

  TPointerHelper = record helper for Pointer
    function Test: LongInt;
    constructor Create(aArg: Pointer);
  end;

  TPLongIntHelper = record helper for PLongInt
    function Test: LongInt;
    constructor Create(aArg: PLongInt);
  end;

  TMyLongIntHelper = record helper for MyLongInt
    function Test: LongInt;
    constructor Create(aArg: MyLongInt);
  end;

  TTestEnumHelper = record helper for TTestEnum
    function Test: LongInt;
    constructor Create(aArg: TTestEnum);
  end;

  TTestSetHelper = record helper for TTestSet
    function Test: LongInt;
    constructor Create(aArg: TTestSet);
  end;

  TTestArrayHelper = record helper for TTestArray
    function Test: LongInt;
    constructor Create(aArg: TTestArray);
  end;

  TVariantHelper = record helper for Variant
    function Test: LongInt;
    constructor Create(aArg: Variant);
  end;

  TSingleHelper = record helper for Single
    function Test: LongInt;
    constructor Create(aArg: Single);
  end;

  TDoubleHelper = record helper for Double
    function Test: LongInt;
    constructor Create(aArg: Double);
  end;

{$if sizeof(extended) <> sizeof(double)}
  TExtendedHelper = record helper for Extended
    function Test: LongInt;
    constructor Create(aArg: Extended);
  end;
{$endif}

implementation

{$if sizeof(extended) <> sizeof(double)}
{ TExtendedHelper }

function TExtendedHelper.Test: LongInt;
begin
  Result := SizeOf(Self);
end;

constructor TExtendedHelper.Create(aArg: Extended);
begin
  Self := aArg;
end;
{$endif}

{ TDoubleHelper }

function TDoubleHelper.Test: LongInt;
begin
  Result := SizeOf(Self);
end;

constructor TDoubleHelper.Create(aArg: Double);
begin
  Self := aArg;
end;

{ TSingleHelper }

function TSingleHelper.Test: LongInt;
begin
  Result := SizeOf(Self);
end;

constructor TSingleHelper.Create(aArg: Single);
begin
  Self := aArg;
end;

{ TVariantHelper }

function TVariantHelper.Test: LongInt;
begin
  Result := 3;
end;

constructor TVariantHelper.Create(aArg: Variant);
begin
  Self := aArg;
end;

{ TTestArrayHelper }

function TTestArrayHelper.Test: LongInt;
begin
  Result := Length(Self);
end;

constructor TTestArrayHelper.Create(aArg: TTestArray);
begin
  Self := aArg;
end;

{ TTestSetHelper }

function TTestSetHelper.Test: LongInt;
begin
  Result := 2;
end;

constructor TTestSetHelper.Create(aArg: TTestSet);
begin
  Self := aArg;
end;

{ TTestEnumHelper }

function TTestEnumHelper.Test: LongInt;
begin
  Result := 1;
end;

constructor TTestEnumHelper.Create(aArg: TTestEnum);
begin
  Self := aArg;
end;

{ TMyLongIntHelper }

function TMyLongIntHelper.Test: LongInt;
begin
  Result := 42;
end;

constructor TMyLongIntHelper.Create(aArg: MyLongInt);
begin
  Self := aArg;
end;

{ TPLongIntHelper }

function TPLongIntHelper.Test: LongInt;
begin
  Result := 4;
end;

constructor TPLongIntHelper.Create(aArg: PLongInt);
begin
  Self := aArg;
end;

{ TPointerHelper }

function TPointerHelper.Test: LongInt;
begin
  Result := 1;
end;

constructor TPointerHelper.Create(aArg: Pointer);
begin
  Self := aArg;
end;

{ TWideCharHelper }

function TWideCharHelper.Test: LongInt;
begin
  Result := - 2;
end;

constructor TWideCharHelper.Create(aArg: WideChar);
begin
  Self := aArg;
end;

{ TAnsiCharHelper }

function TAnsiCharHelper.Test: LongInt;
begin
  Result := - 1;
end;

constructor TAnsiCharHelper.Create(aArg: AnsiChar);
begin
  Self := aArg;
end;

{ TUnicodeStringHelper }

function TUnicodeStringHelper.Test: LongInt;
begin
  Result := 4;
end;

constructor TUnicodeStringHelper.Create(aArg: UnicodeString);
begin
  Self := aArg;
end;

{ TWideStringHelper }

function TWideStringHelper.Test: LongInt;
begin
  Result := 3;
end;

constructor TWideStringHelper.Create(aArg: WideString);
begin
  Self := aArg;
end;

{ TAnsiStringHelper }

function TAnsiStringHelper.Test: LongInt;
begin
  Result := 2;
end;

constructor TAnsiStringHelper.Create(aArg: AnsiString);
begin
  Self := aArg;
end;

{ TShortStringHelper }

function TShortStringHelper.Test: LongInt;
begin
  Result := 1;
end;

constructor TShortStringHelper.Create(aArg: ShortString);
begin
  Self := aArg;
end;

{ TQWordBoolHelper }

function TQWordBoolHelper.Test: LongInt;
begin
  Result := - SizeOf(Self);
end;

constructor TQWordBoolHelper.Create(aArg: QWordBool);
begin
  Self := aArg;
end;

{ TLongBoolHelper }

function TLongBoolHelper.Test: LongInt;
begin
  Result := - SizeOf(Self);
end;

constructor TLongBoolHelper.Create(aArg: LongBool);
begin
  Self := aArg;
end;

{ TWordBoolHelper }

function TWordBoolHelper.Test: LongInt;
begin
  Result := - SizeOf(Self);
end;

constructor TWordBoolHelper.Create(aArg: WordBool);
begin
  Self := aArg;
end;

{ TByteBoolHelper }

function TByteBoolHelper.Test: LongInt;
begin
  Result := - SizeOf(Self);
end;

constructor TByteBoolHelper.Create(aArg: ByteBool);
begin
  Self := aArg;
end;

{ TBoolean64Helper }

function TBoolean64Helper.Test: LongInt;
begin
  Result := SizeOf(Self);
end;

constructor TBoolean64Helper.Create(aArg: Boolean64);
begin
  Self := aArg;
end;

{ TBoolean32Helper }

function TBoolean32Helper.Test: LongInt;
begin
  Result := SizeOf(Self);
end;

constructor TBoolean32Helper.Create(aArg: Boolean32);
begin
  Self := aArg;
end;

{ TBoolean16Helper }

function TBoolean16Helper.Test: LongInt;
begin
  Result := SizeOf(Self);
end;

constructor TBoolean16Helper.Create(aArg: Boolean16);
begin
  Self := aArg;
end;

{ TBooleanHelper }

function TBooleanHelper.Test: LongInt;
begin
  Result := SizeOf(Self);
end;

constructor TBooleanHelper.Create(aArg: Boolean);
begin
  Self := aArg;
end;

{ TInt64Helper }

function TInt64Helper.Test: LongInt;
begin
  Result := - SizeOf(Self);
end;

constructor TInt64Helper.Create(aArg: Int64);
begin
  Self := aArg;
end;

{ TInt32Helper }

function TInt32Helper.Test: LongInt;
begin
  Result := - SizeOf(Self);
end;

constructor TInt32Helper.Create(aArg: Int32);
begin
  Self := aArg;
end;

{ TInt16Helper }

function TInt16Helper.Test: LongInt;
begin
  Result := - SizeOf(Self);
end;

constructor TInt16Helper.Create(aArg: Int16);
begin
  Self := aArg;
end;

{ TInt8Helper }

function TInt8Helper.Test: LongInt;
begin
  Result := - SizeOf(Self);
end;

constructor TInt8Helper.Create(aArg: Int8);
begin
  Self := aArg;
end;

{ TUInt64Helper }

function TUInt64Helper.Test: LongInt;
begin
  Result := SizeOf(Self);
end;

constructor TUInt64Helper.Create(aArg: UInt64);
begin
  Self := aArg;
end;

{ TUInt32Helper }

function TUInt32Helper.Test: LongInt;
begin
  Result := SizeOf(Self);
end;

constructor TUInt32Helper.Create(aArg: UInt32);
begin
  Self := aArg;
end;

{ TUInt16Helper }

function TUInt16Helper.Test: LongInt;
begin
  Result := SizeOf(Self);
end;

constructor TUInt16Helper.Create(aArg: UInt16);
begin
  Self := aArg;
end;

{ TUInt8Helper }

function TUInt8Helper.Test: LongInt;
begin
  Result := SizeOf(Self);
end;

constructor TUInt8Helper.Create(aArg: UInt8);
begin
  Self := aArg;
end;

end.

