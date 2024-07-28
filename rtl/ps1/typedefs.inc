Type
  { The compiler has all integer types defined internally. Here
    we define only aliases }
  DWord    = LongWord;
  Cardinal = LongWord;
  Integer  = SmallInt;
  UInt64   = QWord;

  SizeInt = Longint;
  SizeUInt = DWord;
  PtrInt = Longint;
  PtrUInt = DWord;
  ValSInt = Longint;
  ValUInt = Cardinal;
  CodePointer = Pointer;
  CodePtrInt = PtrInt;
  CodePtrUInt = PtrUInt;
  TExitCode = Longint;

  { NativeInt and NativeUInt are Delphi compatibility types. Even though Delphi
    has IntPtr and UIntPtr, the Delphi documentation for NativeInt states that
    'The size of NativeInt is equivalent to the size of the pointer on the
    current platform'. Because of the misleading names, these types shouldn't be
    used in the FPC RTL. Note that on i8086 their size changes between 16-bit
    and 32-bit according to the memory model, so they're not really a 'native
    int' type there at all. }
  NativeInt  = Type PtrInt;
  NativeUInt = Type PtrUInt;

  Int8    = ShortInt;
  Int16   = SmallInt;
  Int32   = Longint;
  IntPtr  = PtrInt;
  UInt8   = Byte;
  UInt16  = Word;
  UInt32  = Cardinal;
  UIntPtr = PtrUInt;

{ Zero - terminated strings }

{$IF DECLARED(AnsiChar)}
// Compiler defines AnsiChar and WideChar, not AnsiChar
{$IFDEF UNICODERTL}
  Char = WideChar;
{$ElSE}
  Char = AnsiChar;
{$ENDIF}

{$ELSE}
  // Compiler defines Char, we make AnsiChar an alias
  AnsiChar = char;
{$ENDIF}

  PChar               = ^Char;
  PPChar              = ^PChar;
  PPPChar             = ^PPChar;

  TAnsiChar           = AnsiChar;
  PAnsiChar           = ^AnsiChar;
  PPAnsiChar          = ^PAnsiChar;
  PPPAnsiChar         = ^PPAnsiChar;

  UTF8Char = AnsiChar;
  PUTF8Char = PAnsiChar;

  UCS4Char            = 0..$10ffff;
  PUCS4Char           = ^UCS4Char;
{$ifdef CPU16}
  TUCS4CharArray      = array[0..32767 div sizeof(UCS4Char)-1] of UCS4Char;
{$else CPU16}
  TUCS4CharArray      = array[0..$effffff] of UCS4Char;
{$endif CPU16}
  PUCS4CharArray      = ^TUCS4CharArray;
{$ifdef FPC_HAS_FEATURE_DYNARRAYS}
  UCS4String          = array of UCS4Char;
{$endif}


Comp = type Int64;


PCurrency           = ^Currency;

  UTF8String          = type ansistring;
  PUTF8String         = ^UTF8String;

  RawByteString       = ansistring;

  HRESULT             = type Longint;
  TError               = type Longint;

  PSmallInt           = ^Smallint;
  PShortInt           = ^Shortint;
  PInteger            = ^Integer;
  PByte               = ^Byte;
  PWord               = ^word;
  PDWord              = ^DWord;
  PLongWord           = ^LongWord;
  PLongint            = ^Longint;
  PCardinal           = ^Cardinal;
  PQWord              = ^QWord;
  PInt64              = ^Int64;
  PUInt64             = ^UInt64;
  PPtrInt             = ^PtrInt;
  PPtrUInt            = ^PtrUInt;
  PSizeInt            = ^SizeInt;
  PSizeUInt           = ^SizeUInt;

  PPByte              = ^PByte;
  PPLongint           = ^PLongint;

  PPointer            = ^Pointer;
  PPPointer           = ^PPointer;

  PCodePointer        = ^CodePointer;
  PPCodePointer       = ^PCodePointer;

  PBoolean            = ^Boolean;

{$IFNDEF VER3_0}
  PBoolean8           = ^Boolean8;
{$ENDIF VER3_0}
  PBoolean16          = ^Boolean16;
  PBoolean32          = ^Boolean32;
  PBoolean64          = ^Boolean64;

  PByteBool           = ^ByteBool;
  PWordBool           = ^WordBool;
  PLongBool           = ^LongBool;
  PQWordBool          = ^QWordBool;

  PNativeInt 	      = ^NativeInt;
  PNativeUInt	      = ^NativeUint;
  PInt8   	      = PShortInt;
  PInt16  	      = PSmallint;
  PInt32  	      = PLongint;
  PIntPtr 	      = PPtrInt;
  PUInt8  	      = PByte;
  PUInt16 	      = PWord;
  PUInt32 	      = PDWord;
  PUintPtr	      = PPtrUInt;

  PShortString        = ^ShortString;
  PAnsiString         = ^AnsiString;
  PRawByteString      = ^RawByteString;

{$ifndef FPUNONE}
  PDate               = ^TDateTime;
  PDateTime           = ^TDateTime;
{$endif}
  PError              = ^TError;

{$ifdef FPC_HAS_FEATURE_VARIANTS}
  PVariant            = ^Variant;
  POleVariant         = ^OleVariant;
{$endif FPC_HAS_FEATURE_VARIANTS}

  PWideChar           = ^WideChar;
  PPWideChar          = ^PWideChar;
  PPPWideChar         = ^PPWideChar;
  WChar               = Widechar;
  UCS2Char            = WideChar;
  PUCS2Char           = PWideChar;
  PWideString         = ^WideString;

  UnicodeChar         = WideChar;
  PUnicodeChar        = ^UnicodeChar;
  PUnicodeString      = ^UnicodeString;

  PMarshaledString    = ^PWideChar;
  PMarshaledAString   = ^PAnsiChar;

  MarshaledString     = PWideChar;
  MarshaledAString    = PAnsiChar;

  TSystemCodePage     = Word;

  TFileTextRecChar    = {$if defined(FPC_ANSI_TEXTFILEREC) or not(defined(FPC_HAS_FEATURE_WIDESTRINGS))}AnsiChar{$else}UnicodeChar{$endif};
  PFileTextRecChar    = ^TFileTextRecChar;

  TTextLineBreakStyle = (tlbsLF,tlbsCRLF,tlbsCR);

{ opaque data type and related opaque pointer }
  TOpaqueData = record end;
  POpaqueData = ^TOpaqueData;
  OpaquePointer = type POpaqueData;

{ procedure type }
  TProcedure  = Procedure;
 
