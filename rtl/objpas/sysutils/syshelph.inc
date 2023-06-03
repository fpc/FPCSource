{%mainunit syshelpers.pp}

Type
  TStringArray = Array of string;
  TCharArray = Array of char;
  TEndian = ObjPas.TEndian;
  TByteBitIndex = 0..7;
  TShortIntBitIndex = 0..7;
  TWordBitIndex = 0..15;
  TSmallIntBitIndex = 0..15;
  TCardinalBitIndex = 0..31;
{$if sizeof(Integer)=2}
  TIntegerBitIndex = 0..15;
{$elseif sizeof(Integer)=4}
  TIntegerBitIndex = 0..31;
{$else}
  {$fatal Unsupported Integer type size}
{$endif}
  TLongIntBitIndex = 0..31;
  TQwordBitIndex = 0..63;
  TInt64BitIndex = 0..63;
{$IFDEF cpu16}
  TNativeUIntBitIndex = TWordBitIndex;
  TNativeIntBitIndex = TSmallIntBitIndex;
{$ENDIF}
{$IFDEF cpu32}
  TNativeUIntBitIndex = TCardinalBitIndex;
  TNativeIntBitIndex = TIntegerBitIndex;
{$ENDIF}
{$IFDEF cpu64}
  TNativeUIntBitIndex = TQWordBitIndex;
  TNativeIntBitIndex = TInt64BitIndex;
{$ENDIF}

  
Const
  CPUEndian = {$IFDEF FPC_LITTLE_ENDIAN}TEndian.Little{$ELSE}TEndian.Big{$ENDIF};

Type
  TGuidHelper = record helper for TGUID
    Class function Create(const Data; BigEndian: Boolean): TGUID; overload; static;
    class function Create(const Data: array of Byte; AStartIndex: Cardinal; BigEndian: Boolean): TGUID; overload; static;
    Class Function Create(const Data; DataEndian: TEndian = CPUEndian): TGUID; overload; static; inline;
    Class Function Create(const B: TBytes; DataEndian: TEndian = CPUEndian): TGUID; overload; static; inline;
    Class Function Create(const B: TBytes; AStartIndex: Cardinal; DataEndian: TEndian = CPUEndian): TGUID; overload; static;
    Class Function Create(const S: string): TGUID; overload; static;
    Class Function Create(A: Integer; B: SmallInt; C: SmallInt; const D: TBytes): TGUID; overload; static;
    Class Function Create(A: Integer; B: SmallInt; C: SmallInt; D, E, F, G, H, I, J, K: Byte): TGUID; overload; static;
    Class Function Create(A: Cardinal; B: Word; C: Word; D, E, F, G, H, I, J, K: Byte): TGUID; overload; static;
    Class Function NewGuid: TGUID; static;
    Function ToByteArray(DataEndian: TEndian = CPUEndian): TBytes;
    Function ToString(SkipBrackets: Boolean = False): string;
  end;


  TCompareOption = system.TCompareOption;
  TCompareOptions = system.TCompareOptions;

{$SCOPEDENUMS ON}
  TStringSplitOptions = (None, ExcludeEmpty, ExcludeLastEmpty);
{$SCOPEDENUMS OFF}

  { TStringHelper }

  TStringHelper = Type Helper for AnsiString
  Private
    Function GetChar(AIndex : SizeInt) : Char;
    Function GetLength : SizeInt;
  public
    const Empty = '';
    // Methods
    Class Function Compare(const A: string; const B: string): Integer; overload; static; //inline;
    Class Function Compare(const A: string; const B: string; IgnoreCase: Boolean): Integer; overload; static; //inline; //deprecated 'Use same with TCompareOptions';
    Class Function Compare(const A: string; const B: string; Options: TCompareOptions): Integer; overload; static; // inline;
    Class Function Compare(const A: string; IndexA: SizeInt; const B: string; IndexB: SizeInt; ALen: SizeInt): Integer; overload; static; // inline;
    Class Function Compare(const A: string; IndexA: SizeInt; const B: string; IndexB: SizeInt; ALen: SizeInt; IgnoreCase: Boolean): Integer; overload; static; // inline; //deprecated 'Use same with TCompareOptions';
    Class Function Compare(const A: string; IndexA: SizeInt; const B: string; IndexB: SizeInt; ALen: SizeInt; Options: TCompareOptions): Integer; overload; static;//  inline;
    Class Function CompareOrdinal(const A: string; const B: string): Integer; overload; static;
    Class Function CompareOrdinal(const A: string; IndexA: SizeInt; const B: string; IndexB: SizeInt; ALen: SizeInt): Integer; overload; static;
    Class Function CompareText(const A: string; const B: string): Integer; static; inline;
    Class Function Copy(const Str: string): string; inline; static;
    Class Function Create(AChar: Char; ACount: SizeInt): string; overload; inline; static;
    Class Function Create(const AValue: array of Char): string; overload; static;
    Class Function Create(const AValue: array of Char; StartIndex: SizeInt; ALen: SizeInt): string; overload; static;
    Class Function EndsText(const ASubText, AText: string): Boolean; static;
    Class Function Equals(const a: string; const b: string): Boolean; overload; static;
    Class Function Format(const AFormat: string; const args: array of const): string; overload; static;
    Class Function IsNullOrEmpty(const AValue: string): Boolean; static;
    Class Function IsNullOrWhiteSpace(const AValue: string): Boolean; static;
    Class Function Join(const Separator: string; const Values: array of const): string; overload; static;
    Class Function Join(const Separator: string; const Values: array of string): string; overload; static;
    Class Function Join(const Separator: string; const Values: array of string; StartIndex: SizeInt; ACount: SizeInt): string; overload; static;
    Class Function LowerCase(const S: string): string; overload; static; inline;
    Class Function Parse(const AValue: Boolean): string; overload; static; inline;
    Class Function Parse(const AValue: Extended): string; overload; static;inline;
    Class Function Parse(const AValue: Int64): string; overload; static; inline;
    Class Function Parse(const AValue: Integer): string; overload; static; inline;
    Class Function ToBoolean(const S: string): Boolean; overload; static; inline;
    Class Function ToDouble(const S: string): Double; overload; static; inline;
    Class Function ToExtended(const S: string): Extended; overload; static; inline;
    Class Function ToInt64(const S: string): Int64; overload; static; inline;
    Class Function ToInteger(const S: string): Integer; overload; static; inline;
    Class Function ToSingle(const S: string): Single; overload; static; inline;
    Class Function UpperCase(const S: string): string; overload; static; inline;
    Function CompareTo(const B: string): Integer;
    Function Contains(const AValue: string; IgnoreCase: Boolean = False): Boolean;
    procedure CopyTo(SourceIndex: SizeInt; var destination: array of Char; DestinationIndex: SizeInt; ACount: SizeInt);
    Function CountChar(const C: Char): SizeInt;
    Function DeQuotedString: string; overload;
    Function DeQuotedString(const AQuoteChar: Char): string; overload;
    Function EndsWith(const AValue: string): Boolean; overload; inline;
    Function EndsWith(const AValue: string; IgnoreCase: Boolean): Boolean; overload;
    Function Equals(const AValue: string; IgnoreCase: Boolean = False): Boolean; overload;
    Function Format(const args: array of const): string; overload;
    Function GetHashCode: Integer;
    Function IndexOf(AValue: Char): SizeInt; overload; inline;
    Function IndexOf(const AValue: string): SizeInt; overload; inline;
    Function IndexOf(AValue: Char; StartIndex: SizeInt): SizeInt; overload;
    Function IndexOf(const AValue: string; StartIndex: SizeInt): SizeInt; overload;
    Function IndexOf(AValue: Char; StartIndex: SizeInt; ACount: SizeInt): SizeInt; overload;
    Function IndexOf(const AValue: string; StartIndex: SizeInt; ACount: SizeInt): SizeInt; overload;
    Function IndexOfUnQuoted(const AValue: string; StartQuote, EndQuote: Char; StartIndex: SizeInt = 0): SizeInt; overload;
    Function IndexOfAny(const AnyOf: array of Char): SizeInt; overload;
    Function IndexOfAny(const AnyOf: array of Char; StartIndex: SizeInt): SizeInt; overload;
    Function IndexOfAny(const AnyOf: array of Char; StartIndex: SizeInt; ACount: SizeInt): SizeInt; overload;
    Function IndexOfAny(const AnyOf: array of String): SizeInt; overload;
    Function IndexOfAny(const AnyOf: array of String; StartIndex: SizeInt): SizeInt; overload;
    Function IndexOfAny(const AnyOf: array of String; StartIndex: SizeInt; ACount: SizeInt): SizeInt; overload;
    Function IndexOfAny(const AnyOf: array of String; StartIndex: SizeInt; ACount: SizeInt; Out AMatch : SizeInt): SizeInt; overload;
    Function IndexOfAnyUnquoted(const AnyOf: array of Char; StartQuote, EndQuote: Char): SizeInt; overload;
    Function IndexOfAnyUnquoted(const AnyOf: array of Char; StartQuote, EndQuote: Char; StartIndex: SizeInt): SizeInt; overload;
    Function IndexOfAnyUnquoted(const AnyOf: array of Char; StartQuote, EndQuote: Char; StartIndex: SizeInt; ACount: SizeInt): SizeInt; overload;
    function IndexOfAnyUnquoted(const AnyOf: array of string; StartQuote, EndQuote: Char; StartIndex: SizeInt; Out Matched: SizeInt): SizeInt; overload;
    Function Insert(StartIndex: SizeInt; const AValue: string): string;
    Function IsDelimiter(const Delimiters: string; Index: SizeInt): Boolean;
    Function IsEmpty: Boolean;
    Function LastDelimiter(const Delims: string): SizeInt;
    Function LastIndexOf(AValue: Char): SizeInt; overload;
    Function LastIndexOf(const AValue: string): SizeInt; overload;
    Function LastIndexOf(AValue: Char; AStartIndex: SizeInt): SizeInt; overload;
    Function LastIndexOf(const AValue: string; AStartIndex: SizeInt): SizeInt; overload;
    Function LastIndexOf(AValue: Char; AStartIndex: SizeInt; ACount: SizeInt): SizeInt; overload;
    Function LastIndexOf(const AValue: string; AStartIndex: SizeInt; ACount: SizeInt): SizeInt; overload;
    Function LastIndexOfAny(const AnyOf: array of Char): SizeInt; overload;
    Function LastIndexOfAny(const AnyOf: array of Char; AStartIndex: SizeInt): SizeInt; overload;
    Function LastIndexOfAny(const AnyOf: array of Char; AStartIndex: SizeInt; ACount: SizeInt): SizeInt; overload;
    Function PadLeft(ATotalWidth: SizeInt): string; overload; inline;
    Function PadLeft(ATotalWidth: SizeInt; PaddingChar: Char): string; overload; inline;
    Function PadRight(ATotalWidth: SizeInt): string; overload; inline;
    Function PadRight(ATotalWidth: SizeInt; PaddingChar: Char): string; overload; inline;
    Function QuotedString: string; overload;
    Function QuotedString(const AQuoteChar: Char): string; overload;
    Function Remove(StartIndex: SizeInt): string; overload; inline;
    Function Remove(StartIndex: SizeInt; ACount: SizeInt): string; overload; inline;
    Function Replace(OldChar: Char; NewChar: Char): string; overload;
    Function Replace(OldChar: Char; NewChar: Char; ReplaceFlags: TReplaceFlags): string; overload;
    Function Replace(const OldValue: string; const NewValue: string): string; overload;
    Function Replace(const OldValue: string; const NewValue: string; ReplaceFlags: TReplaceFlags): string; overload;
    Function Split(const Separators: array of Char): TStringArray; overload;
    Function Split(const Separators: array of Char; ACount: SizeInt): TStringArray; overload;
    Function Split(const Separators: array of Char; Options: TStringSplitOptions): TStringArray; overload;
    Function Split(const Separators: array of Char; ACount: SizeInt; Options: TStringSplitOptions): TStringArray; overload;
    Function Split(const Separators: array of string): TStringArray; overload;
    Function Split(const Separators: array of string; ACount: SizeInt): TStringArray; overload;
    Function Split(const Separators: array of string; Options: TStringSplitOptions): TStringArray; overload;
    Function Split(const Separators: array of string; ACount: SizeInt; Options: TStringSplitOptions): TStringArray; overload;
    Function Split(const Separators: array of Char; AQuote: Char): TStringArray; overload;
    Function Split(const Separators: array of Char; AQuoteStart, AQuoteEnd: Char): TStringArray; overload;
    Function Split(const Separators: array of Char; AQuoteStart, AQuoteEnd: Char; Options: TStringSplitOptions): TStringArray; overload;
    Function Split(const Separators: array of Char; AQuoteStart, AQuoteEnd: Char; ACount: SizeInt): TStringArray; overload;
    Function Split(const Separators: array of Char; AQuoteStart, AQuoteEnd: Char; ACount: SizeInt; Options: TStringSplitOptions): TStringArray; overload;
    Function Split(const Separators: array of string; AQuote: Char): TStringArray; overload;
    Function Split(const Separators: array of string; AQuoteStart, AQuoteEnd: Char): TStringArray; overload;
    Function Split(const Separators: array of string; AQuoteStart, AQuoteEnd: Char; Options: TStringSplitOptions): TStringArray; overload;
    Function Split(const Separators: array of string; AQuoteStart, AQuoteEnd: Char; ACount: SizeInt): TStringArray; overload;
    Function Split(const Separators: array of string; AQuoteStart, AQuoteEnd: Char; ACount: SizeInt; Options: TStringSplitOptions): TStringArray; overload;
    Function StartsWith(const AValue: string): Boolean; overload; inline;
    Function StartsWith(const AValue: string; IgnoreCase: Boolean): Boolean; overload;
    Function Substring(AStartIndex: SizeInt): string; overload;
    Function Substring(AStartIndex: SizeInt; ALen: SizeInt): string; overload;
    Function ToBoolean: Boolean; overload; inline;
    Function ToInteger: Integer; overload; inline;
    Function ToInt64: Int64; overload; inline;
    Function ToSingle: Single; overload; inline;
    Function ToDouble: Double; overload; inline;
    Function ToExtended: Extended; overload; inline;
    Function ToCharArray: TCharArray; overload;
    Function ToCharArray(AStartIndex: SizeInt; ALen: SizeInt): TCharArray; overload;
    Function ToLower: string; overload; inline;
    Function ToLowerInvariant: string;
    Function ToUpper: string; overload; inline;
    Function ToUpperInvariant: string; inline;
    Function Trim: string; overload;
    Function TrimLeft: string; overload;
    Function TrimRight: string; overload;
    Function Trim(const ATrimChars: array of Char): string; overload;
    Function TrimLeft(const ATrimChars: array of Char): string; overload;
    Function TrimRight(const ATrimChars: array of Char): string; overload;
    Function TrimEnd(const ATrimChars: array of Char): string; deprecated 'Use TrimRight';
    Function TrimStart(const ATrimChars: array of Char): string; deprecated 'Use TrimLeft';
    property Chars[AIndex: SizeInt]: Char read GetChar;
    property Length: SizeInt read GetLength;
  end;

  TCurrencyHelper = type helper for Currency
  private
    function GetMaxValue: Currency; inline;
    function GetMinValue: Currency; inline;
  public
    function Ceil: Int64; inline;
    function Floor: Int64; inline;
    function Frac: Currency; inline;
    class function Parse(const S: string; const AFormatSettings: TFormatSettings): Currency; overload; static;
    class function Parse(const S: string): Currency; overload; inline; static;
    class function Size: Integer; inline; static;
    function ToString(const AFormatSettings: TFormatSettings): string; overload; inline;
    function ToString: string; overload; inline;
    class function ToString(const Value: Currency; const AFormatSettings: TFormatSettings): string; overload; inline; static;
    class function ToString(const Value: Currency): string; overload; inline; static;
    function Trunc: Int64; inline;
    class function TryParse(const S: string; out Value: Currency; const AFormatSettings: TFormatSettings): Boolean; overload; inline; static;
    class function TryParse(const S: string; out Value: Currency): Boolean; overload; inline; static;

    property MaxValue: Currency read GetMaxValue;
    property MinValue: Currency read GetMinValue;
  end;

{$IFDEF FPC_HAS_TYPE_SINGLE}
  TSingleHelper = Type Helper for Single
  Private
    Function GetB(AIndex: Cardinal): Byte;
    Function GetW(AIndex: Cardinal): Word;
    Function GetE: QWord; inline;
    Function GetF: QWord; inline;
    Function GetS: Boolean; inline;
    procedure SetB(AIndex: Cardinal; const AValue: Byte);
    procedure SetW(AIndex: Cardinal; const AValue: Word);
    procedure SetE(AValue: QWord);
    procedure SetF(AValue: QWord);
    procedure SetS(AValue: Boolean);
  public
{$push}
{$R-}
{$Q-}
    const
      Epsilon          : Single = 1.4012984643248170709e-45;
      MaxValue         : Single = 340282346638528859811704183484516925440.0;
      MinValue         : Single = -340282346638528859811704183484516925440.0;
      PositiveInfinity : Single = 1.0/0.0;
      NegativeInfinity : Single = -1.0/0.0;
      NaN              : Single = 0.0/0.0;
{$POP}
    Class Function IsNan(const AValue: Single): Boolean; overload; inline; static;
    Class Function IsInfinity(const AValue: Single): Boolean; overload; inline; static;
    Class Function IsNegativeInfinity(const AValue: Single): Boolean; overload; inline; static;
    Class Function IsPositiveInfinity(const AValue: Single): Boolean; overload; inline; static;
    Class Function Parse(const AString: string): Single; overload; inline; static;
    Class Function Parse(const AString: string; const AFormatSettings: TFormatSettings): Single; overload; inline; static;
    Class Function Size: Integer; inline; static;
    Class Function ToString(const AValue: Single): string; overload; inline; static;
    Class Function ToString(const AValue: Single; const AFormatSettings: TFormatSettings): string; overload; inline; static;
    Class Function ToString(const AValue: Single; const AFormat: TFloatFormat; const APrecision, ADigits: Integer): string; overload; inline; static;
    Class Function ToString(const AValue: Single; const AFormat: TFloatFormat; const APrecision, ADigits: Integer;
                               const AFormatSettings: TFormatSettings): string; overload; inline; static;
    Class Function TryParse(const AString: string; out AValue: Single): Boolean; overload; inline; static;
    Class Function TryParse(const AString: string; out AValue: Single; const AFormatSettings: TFormatSettings): Boolean; overload; inline; static;

    Procedure BuildUp(const ASignFlag: Boolean; const AMantissa: QWord; const AExponent: Integer);
    Function Exponent: Integer;
    Function Fraction: Extended;
    Function IsInfinity: Boolean; overload; inline;
    Function IsNan: Boolean; overload; inline;
    Function IsNegativeInfinity: Boolean; overload; inline;
    Function IsPositiveInfinity: Boolean; overload; inline;
    Function Mantissa: QWord;
    Function SpecialType: TFloatSpecial;
    Function ToString(const AFormat: TFloatFormat; const APrecision, ADigits: Integer): string; overload; inline;
    Function ToString(const AFormat: TFloatFormat; const APrecision, ADigits: Integer; const AFormatSettings: TFormatSettings): string; overload; inline;
    Function ToString(const AFormatSettings: TFormatSettings): string; overload; inline;
    Function ToString: string; overload; inline;

    property Bytes [AIndex: Cardinal]: Byte read GetB write SetB;
    property Words[AIndex: Cardinal]: Word read GetW write SetW;
    property Sign: Boolean read GetS write SetS;
    property Exp: QWord read GetE write SetE;
    property Frac: QWord read GetF write SetF;
  end;
{$ENDIF FPC_HAS_TYPE_SINGLE}

{$IFDEF FPC_HAS_TYPE_DOUBLE}
  TDoubleHelper = Type Helper for Double
  private
    Function GetB(AIndex: Cardinal): Byte;
    Function GetW(AIndex: Cardinal): Word;
    Function GetE: QWord; inline;
    Function GetF: QWord; inline;
    Function GetS: Boolean; inline;
    procedure SetB(AIndex: Cardinal; const AValue: Byte);
    procedure SetW(AIndex: Cardinal; const AValue: Word);
    procedure SetE(AValue: QWord);
    procedure SetF(AValue: QWord);
    procedure SetS(AValue: Boolean);
  public
    const
    {$push}
    {$R-}
    {$Q-}
      Epsilon          : Double = 4.9406564584124654418e-324;
      MaxValue         : Double = 1.7976931348623157081e+308;
      MinValue         : Double = -1.7976931348623157081e+308;
      PositiveInfinity : Double = 1.0/0.0;
      NegativeInfinity : Double = -1.0/0.0;
      NaN              : Double = 0.0/0.0;
    {$POP}
    Class Function IsInfinity(const AValue: Double): Boolean; overload; inline; static;
    Class Function IsNan(const AValue: Double): Boolean; overload; inline; static;
    Class Function IsNegativeInfinity(const AValue: Double): Boolean; overload; inline; static;
    Class Function IsPositiveInfinity(const AValue: Double): Boolean; overload; inline; static;
    Class Function Parse(const AString: string): Double; overload; inline; static;
    Class Function Parse(const AString: string; const AFormatSettings: TFormatSettings): Double; overload; inline; static;
    Class Function Size: Integer; inline; static;
    Class Function ToString(const AValue: Double): string; overload; inline; static;
    Class Function ToString(const AValue: Double; const AFormat: TFloatFormat; const APrecision, ADigits: Integer): string; overload; inline; static;
    Class Function ToString(const AValue: Double; const AFormat: TFloatFormat; const APrecision, ADigits: Integer;  const AFormatSettings: TFormatSettings): string; overload; inline; static;
    Class Function ToString(const AValue: Double; const AFormatSettings: TFormatSettings): string; overload; inline; static;
    Class Function TryParse(const AString: string; out AValue: Double): Boolean; overload; inline; static;
    Class Function TryParse(const AString: string; out AValue: Double; const AFormatSettings: TFormatSettings): Boolean; overload; inline; static;

    Procedure BuildUp(const ASignFlag: Boolean; const AMantissa: QWord; const AExponent: Integer);
    Function Exponent: Integer;
    Function Fraction: Extended;
    Function IsInfinity: Boolean; overload; inline;
    Function IsNan: Boolean; overload; inline;
    Function IsNegativeInfinity: Boolean; overload; inline;
    Function IsPositiveInfinity: Boolean; overload; inline;
    Function Mantissa: QWord;
    Function SpecialType: TFloatSpecial;
    Function ToString(const AFormat: TFloatFormat; const APrecision, ADigits: Integer): string; overload; inline;
    Function ToString(const AFormat: TFloatFormat; const APrecision, ADigits: Integer; const AFormatSettings: TFormatSettings): string; overload; inline;
    Function ToString(const AFormatSettings: TFormatSettings): string; overload; inline;
    Function ToString: string; overload; inline;

    property Bytes [AIndex: Cardinal]: Byte read GetB write SetB;  // 0..7
    property Words [AIndex: Cardinal]: Word read GetW write SetW; // 0..3
    property Sign: Boolean read GetS write SetS;
    property Exp: QWord read GetE write SetE;
    property Frac: QWord read GetF write SetF;
  end;
{$ENDIF FPC_HAS_TYPE_DOUBLE}

{$ifdef FPC_HAS_TYPE_EXTENDED}
  TExtendedHelper = Type Helper for Extended
  private
    Function GetB(AIndex: Cardinal): Byte;
    Function GetW(AIndex: Cardinal): Word;
    Function GetE: QWord; inline;
    Function GetF: QWord; inline;
    Function GetS: Boolean; inline;
    procedure SetB(AIndex: Cardinal; const AValue: Byte);
    procedure SetW(AIndex: Cardinal; const AValue: Word);
    procedure SetE(AValue: QWord);
    procedure SetF(AValue: QWord);
    procedure SetS(AValue: Boolean);
  public
    {$push}
    {$R-}
    {$Q-}
    const
      Epsilon           : Extended = 3.64519953188247460253e-4951;
      MaxValue          : Extended = 1.18973149535723176505e+4932;
      MinValue          : Extended = -1.18973149535723176505e+4932;
      PositiveInfinity  : Extended = 1.0/0.0;
      NegativeInfinity  : Extended = -1.0/0.0;
      NaN               : Extended = 0.0/0.0;
    {$POP}
    Class Function ToString(const AValue: Extended): string; overload; inline; static;
    Class Function ToString(const AValue: Extended; const AFormatSettings: TFormatSettings): string; overload; inline; static;
    Class Function ToString(const AValue: Extended; const AFormat: TFloatFormat; const APrecision, ADigits: Integer): string; overload; inline; static;
    Class Function ToString(const AValue: Extended; const AFormat: TFloatFormat; const APrecision, ADigits: Integer; const AFormatSettings: TFormatSettings): string; overload; inline; static;
    Class Function Parse(const AString: string): Extended; overload; inline; static;
    Class Function Parse(const AString: string; const AFormatSettings: TFormatSettings): Extended; overload; inline; static;
    Class Function TryParse(const AString: string; out AValue: Extended): Boolean; overload; inline; static;
    Class Function TryParse(const AString: string; out AValue: Extended; const AFormatSettings: TFormatSettings): Boolean; overload; inline; static;
    Class Function IsNan(const AValue: Extended): Boolean; overload; inline; static;
    Class Function IsInfinity(const AValue: Extended): Boolean; overload; inline; static;
    Class Function IsNegativeInfinity(const AValue: Extended): Boolean; overload; inline; static;
    Class Function IsPositiveInfinity(const AValue: Extended): Boolean; overload; inline; static;
    Class Function Size: Integer; inline; static;

    procedure BuildUp(const ASignFlag: Boolean; const AMantissa: QWord; Const AExponent: Integer);
    Function Exponent: Integer;
    Function Fraction: Extended;
    Function IsInfinity: Boolean; overload; inline;
    Function IsNan: Boolean; overload; inline;
    Function IsNegativeInfinity: Boolean; overload; inline;
    Function IsPositiveInfinity: Boolean; overload; inline;
    Function Mantissa: QWord;
    Function SpecialType: TFloatSpecial;
    Function ToString(const AFormat: TFloatFormat; const APrecision, ADigits: Integer): string; overload; inline;
    Function ToString(const AFormat: TFloatFormat; const APrecision, ADigits: Integer;  const AFormatSettings: TFormatSettings): string; overload; inline;
    Function ToString(const AFormatSettings: TFormatSettings): string; overload; inline;
    Function ToString: string; overload; inline;

    property Bytes [AIndex: Cardinal]: Byte read GetB write SetB;
    property Words [AIndex: Cardinal]: Word read GetW write SetW;
    property Sign: Boolean read GetS write SetS;
    property Exp: QWord read GetE write SetE;
    property Frac: QWord read GetF write SetF;
  end;
{$ENDIF FPC_HAS_TYPE_EXTENDED}

  nibble           = 0..15;
  TNibbleBitIndex  = 0..3;

  // TByteBitIndex is already defined
  TByteNibbleIndex = 0..1;

  TByteOverlay = bitpacked record case integer of     // for fast extraction of bits
    0: (AsBit:    bitpacked array[TByteBitIndex]    of boolean);
    1: (AsNibble: bitpacked array[TByteNibbleIndex] of nibble);
    2: (AsByte:   byte);
  end;
  
  TByteHelper = Type Helper for Byte
  const
    MaxValue  = High(byte);
    MinValue  = Low(byte);
    MaxBit    = High(TByteBitIndex);
    MinBit    = Low(TByteBitIndex);
    MaxNibble = High(TByteNibbleIndex);
    MinNibble = Low(TByteNibbleIndex);
  public
    Class Function Parse(const AString: string): Byte; inline; static;
    Class Function Size: Integer; inline; static;
    Class Function ToString(const AValue: Byte): string; overload; inline; static;
    Class Function TryParse(const AString: string; out AValue: Byte): Boolean; inline; static;
  protected
    function  GetBit(const aIndex: TByteBitIndex): boolean; inline;
    procedure PutBit(const aIndex: TByteBitIndex; const aNewValue: boolean); inline;
    function  GetNibble(const aIndex: TByteNibbleIndex): nibble; inline;
    procedure PutNibble(const aIndex: TByteNibbleIndex; const aNewValue: nibble); inline;
  public
    Function ToBoolean: Boolean; inline;
    Function ToDouble: Double; inline;
    Function ToExtended: Extended; inline;
    Function ToBinString:string;
    Function ToHexString(const AMinDigits: Integer): string; overload; inline;
    Function ToHexString: string; overload; inline;
    Function ToSingle: Single; inline;
    Function ToString: string; overload; inline;
    Function SetBit(const Index: TByteBitIndex) : Byte; inline;
    Function ClearBit(const Index: TByteBitIndex) : Byte; inline;
    Function ToggleBit(const Index: TByteBitIndex) : Byte; inline;
    Function TestBit(const Index:TByteBitIndex):Boolean; inline;
    procedure Clear; inline;
    function  HighestSetBitPos: int8; inline;
    function  LowestSetBitPos: int8; inline;
    function  SetBitsCount: byte; inline;
    property  Bits [aIndex: TByteBitIndex]: boolean   read GetBit    write PutBit;
    property  Nibbles [aIndex: TByteNibbleIndex]: nibble read GetNibble write PutNibble;
  end;

  // TShortIntBitIndex is already defined

  TShortIntNibbleIndex = TByteNibbleIndex;

  TShortIntOverlay = TByteOverlay;

  TShortIntHelper = Type Helper for ShortInt
  const
    MaxValue  = High(shortint);
    MinValue  = Low(shortint);
    MaxBit    = High(TShortIntBitIndex);
    MinBit    = Low(TShortIntBitIndex);
    MaxNibble = High(TShortIntNibbleIndex);
    MinNibble = Low(TShortIntNibbleIndex);
  public
    Class Function Parse(const AString: string): ShortInt; inline; static;
    Class Function Size: Integer; inline; static;
    Class Function ToString(const AValue: ShortInt): string; overload; inline; static;
    Class Function TryParse(const AString: string; out AValue: ShortInt): Boolean; inline; static;
  protected
    function  GetBit(const aIndex: TShortIntBitIndex): boolean; inline;
    procedure PutBit(const aIndex: TShortIntBitIndex; const aNewValue: boolean); inline;
    function  GetNibble(const aIndex: TShortIntNibbleIndex): nibble; inline;
    procedure PutNibble(const aIndex: TShortIntNibbleIndex; const aNewValue: nibble); inline;
  public
    Function ToBoolean: Boolean; inline;
    Function ToDouble: Double; inline;
    Function ToExtended: Extended; inline;
    Function ToBinString:string; inline;
    Function ToHexString(const AMinDigits: Integer): string; overload; inline;
    Function ToHexString: string; overload; inline;
    Function ToSingle: Single; inline;
    Function ToString: string; overload; inline;
    Function SetBit(const Index: TShortIntBitIndex): Shortint; inline;
    Function ClearBit(const Index: TShortIntBitIndex): Shortint; inline;
    Function ToggleBit(const Index: TShortIntBitIndex): Shortint; inline;
    Function TestBit(const Index:TShortIntBitIndex):Boolean;
    procedure Clear; inline;
    function  HighestSetBitPos: int8; inline;
    function  LowestSetBitPos: int8; inline;
    function  SetBitsCount: byte; inline;
    property  Bits [aIndex: TShortIntBitIndex]: boolean   read GetBit    write PutBit;
    property  Nibbles [aIndex: TShortIntNibbleIndex]: nibble read GetNibble write PutNibble;
  end;

  // TWordBitIndex is already defined
  TWordByteIndex   = 0..1;
  TWordNibbleIndex = 0..3;

  TWordOverlay = bitpacked record case integer of     // for fast extraction of bytes and bits
    0: (AsBit:    bitpacked array[TWordBitIndex]    of boolean);
    1: (AsNibble: bitpacked array[TWordNibbleIndex] of nibble);
    2: (AsByte:             array[TWordByteIndex]   of byte);
    3: (AsWord:             word);
    // recursive overlay:
    4: (AsByteOverlay:      array[TWordByteIndex]   of TByteOverlay);
  end;
  
  TWordHelper = Type Helper for Word
  const
    MaxValue  = High(word);
    MinValue  = Low(word);
    MaxBit    = High(TWordBitIndex);
    MinBit    = Low(TWordBitIndex);
    MaxNibble = High(TWordNibbleIndex);
    MinNibble = Low(TWordNibbleIndex);
    MaxByte   = High(TWordByteIndex);
    MinByte   = Low(TWordByteIndex);
  public
    Class Function Parse(const AString: string): Word; inline; static;
    Class Function Size: Integer; inline; static;
    Class Function ToString(const AValue: Word): string; overload; inline; static;
    Class Function TryParse(const AString: string; out AValue: Word): Boolean; inline; static;
  protected
    function  GetBit(const aIndex: TWordBitIndex): boolean; inline;
    procedure PutBit(const aIndex: TWordBitIndex; const aNewValue: boolean); inline;
    function  GetNibble(const aIndex: TWordNibbleIndex): nibble; inline;
    procedure PutNibble(const aIndex: TWordNibbleIndex; const aNewValue: nibble); inline;
    function  GetByte(const aIndex: TWordByteIndex): byte;
    procedure PutByte(const aIndex: TWordByteIndex; const aNewValue: byte);
  public
    Function ToBoolean: Boolean; inline;
    Function ToDouble: Double; inline;
    Function ToExtended: Extended; inline;
    Function ToBinString:string; inline;
    Function ToHexString(const AMinDigits: Integer): string; overload; inline;
    Function ToHexString: string; overload; inline;
    Function ToSingle: Single; inline;
    Function ToString: string; overload; inline;
    Function SetBit(const Index: TWordBitIndex) : Word; inline;
    Function ClearBit(const Index: TWordBitIndex) : Word; inline;
    Function ToggleBit(const Index: TWordBitIndex) : Word; inline;
    Function TestBit(const Index:TWordBitIndex):Boolean; inline;   
    procedure Clear; inline;
    function  HighestSetBitPos: int8; inline;
    function  LowestSetBitPos: int8; inline;
    function  SetBitsCount: byte; inline;
    property  Bits [aIndex: TWordBitIndex]: boolean   read GetBit  write PutBit;
    property  Nibbles [aIndex: TWordNibbleIndex]: nibble read GetNibble write PutNibble;
    property  Bytes [aIndex: TWordByteIndex]: byte     read GetByte write PutByte;
  end;

  // TSmallIntBitIndex is already defined
  TSmallIntByteIndex   = TWordByteIndex;
  TSmallIntNibbleIndex = TWordNibbleIndex;

  TSmallIntOverlay = TWordOverlay;
  
  TSmallIntHelper = Type Helper for SmallInt
  const
    MaxValue  = High(SmallInt);
    MinValue  = Low(SmallInt);
    MaxBit    = High(TSmallIntBitIndex);
    MinBit    = Low(TSmallIntBitIndex);
    MaxNibble = High(TSmallIntNibbleIndex);
    MinNibble = Low(TSmallIntNibbleIndex);
    MaxByte   = High(TSmallIntByteIndex);
    MinByte   = Low(TSmallIntByteIndex);
  public
    Class Function Parse(const AString: string): SmallInt; inline; static;
    Class Function Size: Integer; inline; static;
    Class Function ToString(const AValue: SmallInt): string; overload; inline; static;
    Class Function TryParse(const AString: string; out AValue: SmallInt): Boolean; inline; static;
  protected
    function  GetBit(const aIndex: TSmallIntBitIndex): boolean; inline;
    procedure PutBit(const aIndex: TSmallIntBitIndex; const aNewValue: boolean); inline;
    function  GetNibble(const aIndex: TSmallIntNibbleIndex): nibble; inline;
    procedure PutNibble(const aIndex: TSmallIntNibbleIndex; const aNewValue: nibble); inline;
    function  GetByte(const aIndex: TSmallIntByteIndex): byte;
    procedure PutByte(const aIndex: TSmallIntByteIndex; const aNewValue: byte);
  public
    Function ToString: string; overload; inline;
    Function ToBoolean: Boolean; inline;
    Function ToBinString:string; inline;
    Function ToHexString: string; overload; inline;
    Function ToHexString(const AMinDigits: Integer): string; overload; inline;
    Function ToSingle: Single; inline;
    Function ToDouble: Double; inline;
    Function ToExtended: Extended; inline;
    Function SetBit(const Index: TSmallIntBitIndex) : Smallint; inline;
    Function ClearBit(const Index: TSmallIntBitIndex) : Smallint; inline;
    Function ToggleBit(const Index: TSmallIntBitIndex) : Smallint; inline;
    Function TestBit(const Index:TSmallIntBitIndex):Boolean;    
    procedure Clear; inline;
    function  HighestSetBitPos: int8; inline;
    function  LowestSetBitPos: int8; inline;
    function  SetBitsCount: byte; inline;
    property  Bits [aIndex: TSmallIntBitIndex]: boolean   read GetBit  write PutBit;
    property  Nibbles [aIndex: TSmallIntNibbleIndex]: nibble read GetNibble write PutNibble;
    property  Bytes [aIndex: TSmallIntByteIndex]: byte     read GetByte write PutByte;
  end;
  
  TDwordBitIndex    = TCardinalBitIndex;
  TDwordNibbleIndex = 0..7;
  TDwordByteIndex   = 0..3;
  TDwordWordIndex   = 0..1;

  TDwordOverlay = bitpacked record case integer of     // for fast extraction of words, bytes and bits
    0: (AsBit:    bitpacked array[TDwordBitIndex]    of boolean);
    1: (AsNibble: bitpacked array[TDwordNibbleIndex] of nibble);
    2: (AsByte:             array[TDwordByteIndex]   of byte);
    3: (AsWord:             array[TDwordWordIndex]   of word);
    4: (AsDword:            dword);
    // recursive overlays:
    5: (AsByteOverlay:      array[TDwordByteIndex]   of TByteOverlay);
    6: (AsWordOverlay:      array[TDwordWordIndex]   of TWordOverlay);
  end;

  // TCardinalBitIndex is already defined
  TCardinalNibbleIndex = TDwordNibbleIndex;
  TCardinalByteIndex   = TDwordByteIndex;
  TCardinalWordIndex   = TDwordWordIndex;

  TCardinalOverlay = TDwordOverlay;
  
  TCardinalHelper = Type Helper for Cardinal { for LongWord Type too }
  const
    MaxValue  = High(Cardinal);
    MinValue  = Low(Cardinal);
    MaxBit    = High(TCardinalBitIndex);
    MinBit    = Low(TCardinalBitIndex);
    MaxNibble = High(TCardinalNibbleIndex);
    MinNibble = Low(TCardinalNibbleIndex);
    MaxByte   = High(TCardinalByteIndex);
    MinByte   = Low(TCardinalByteIndex);
    MaxWord   = High(TCardinalWordIndex);
    MinWord   = Low(TCardinalWordIndex);
  public
    Class Function Parse(const AString: string): Cardinal; inline; static;
    Class Function Size: Integer; inline; static;
    Class Function ToString(const AValue: Cardinal): string; overload; inline; static;
    Class Function TryParse(const AString: string; out AValue: Cardinal): Boolean; inline; static;
  protected
    function  GetBit(const aIndex: TCardinalBitIndex): boolean; inline;
    procedure PutBit(const aIndex: TCardinalBitIndex; const aNewValue: boolean); inline;
    function  GetNibble(const aIndex: TCardinalNibbleIndex): nibble; inline;
    procedure PutNibble(const aIndex: TCardinalNibbleIndex; const aNewValue: nibble); inline;
    function  GetByte(const aIndex: TCardinalByteIndex): byte;
    procedure PutByte(const aIndex: TCardinalByteIndex; const aNewValue: byte);
    function  GetWord(const aIndex: TCardinalWordIndex): word;
    procedure PutWord(const aIndex: TCardinalWordIndex; const aNewValue: word);
  public
    Function ToBoolean: Boolean; inline;
    Function ToDouble: Double; inline;
    Function ToExtended: Extended; inline;
    Function ToBinString:string; inline;
    Function ToHexString(const AMinDigits: Integer): string; overload; inline;
    Function ToHexString: string; overload; inline;
    Function ToSingle: Single; inline;
    Function ToString: string; overload; inline;
    Function SetBit(const Index: TCardinalBitIndex) : Cardinal; inline;
    Function ClearBit(const Index: TCardinalBitIndex) : Cardinal; inline;
    Function ToggleBit(const Index: TCardinalBitIndex) : Cardinal; inline;
    Function TestBit(const Index:TCardinalBitIndex):Boolean; inline;   
    procedure Clear; inline;
    function  HighestSetBitPos: int8; inline;
    function  LowestSetBitPos: int8; inline;
    function  SetBitsCount: byte; inline;
    property  Bits   [aIndex: TCardinalBitIndex]: boolean   read GetBit    write PutBit;
    property  Nibbles[aIndex: TCardinalNibbleIndex]: nibble read GetNibble write PutNibble;
    property  Bytes  [aIndex: TCardinalByteIndex]: byte     read GetByte   write PutByte;
    property  Words  [aIndex: TCardinalWordIndex]: word     read GetWord   write PutWord;
  end;

  // TIntegerBitIndex is already defined
{$if sizeof(Integer)=2}
  TIntegerNibbleIndex = TWordNibbleIndex;
  TIntegerByteIndex   = TWordByteIndex;

  TIntegerOverlay = TWordOverlay;
{$elseif sizeof(Integer)=4}
  TIntegerNibbleIndex = TDwordNibbleIndex;
  TIntegerByteIndex   = TDwordByteIndex;
  TIntegerWordIndex   = TDwordWordIndex;

  TIntegerOverlay = TDwordOverlay;
{$else}
  {$fatal Unsupported Integer type size}
{$endif}

  TIntegerHelper = Type Helper for Integer
  const
    MaxValue  = High(integer);
    MinValue  = Low(integer);
    MaxBit    = High(TIntegerBitIndex);
    MinBit    = Low(TIntegerBitIndex);
    MaxNibble = High(TIntegerNibbleIndex);
    MinNibble = Low(TIntegerNibbleIndex);
    MaxByte   = High(TIntegerByteIndex);
    MinByte   = Low(TIntegerByteIndex);
{$if sizeof(Integer)>=4}
    MaxWord   = High(TIntegerWordIndex);
    MinWord   = Low(TIntegerWordIndex);
{$endif}
  public
    Class Function Size: Integer; inline; static;
    Class Function ToString(const AValue: Integer): string; overload; inline; static;
    Class Function Parse(const AString: string): Integer; inline; static;
    Class Function TryParse(const AString: string; out AValue: Integer): Boolean; inline; static;
  protected
    function  GetBit(const aIndex: TIntegerBitIndex): boolean; inline;
    procedure PutBit(const aIndex: TIntegerBitIndex; const aNewValue: boolean); inline;
    function  GetNibble(const aIndex: TIntegerNibbleIndex): nibble; inline;
    procedure PutNibble(const aIndex: TIntegerNibbleIndex; const aNewValue: nibble); inline;
    function  GetByte(const aIndex: TIntegerByteIndex): byte;
    procedure PutByte(const aIndex: TIntegerByteIndex; const aNewValue: byte);
{$if sizeof(Integer)>=4}
    function  GetWord(const aIndex: TIntegerWordIndex): word;
    procedure PutWord(const aIndex: TIntegerWordIndex; const aNewValue: word);
{$endif}
  public
    Function ToBoolean: Boolean; inline;
    Function ToDouble: Double; inline;
    Function ToExtended: Extended; inline;
    Function ToBinString:string; inline;
    Function ToHexString(const AMinDigits: Integer): string; overload; inline;
    Function ToHexString: string; overload; inline;
    Function ToSingle: Single; inline;
    Function ToString: string; overload; inline;
    Function SetBit(const Index: TIntegerBitIndex) : Integer; inline;
    Function ClearBit(const Index: TIntegerBitIndex) : Integer; inline;
    Function ToggleBit(const Index: TIntegerBitIndex) : Integer; inline;
    Function TestBit(const Index:TIntegerBitIndex):Boolean; inline;   
    procedure Clear; inline;
    function  HighestSetBitPos: int8; inline;
    function  LowestSetBitPos: int8; inline;
    function  SetBitsCount: byte; inline;
    property  Bits   [aIndex: TIntegerBitIndex]: boolean    read GetBit    write PutBit;
    property  Nibbles[aIndex: TIntegerNibbleIndex]: nibble  read GetNibble write PutNibble;
    property  Bytes  [aIndex: TIntegerByteIndex]:  byte     read GetByte   write PutByte;
{$if sizeof(Integer)>=4}
    property  Words  [aIndex: TIntegerWordIndex]:  word     read GetWord   write PutWord;
{$endif}
  end;

  // TLongIntBitIndex is already defined
  TLongIntNibbleIndex = TDwordNibbleIndex;
  TLongIntByteIndex   = TDwordByteIndex;
  TLongIntWordIndex   = TDwordWordIndex;

  TLongIntOverlay = TDwordOverlay;

  TLongIntHelper = Type Helper for LongInt
  const
    MaxValue  = High(LongInt);
    MinValue  = Low(LongInt);
    MaxBit    = High(TLongIntBitIndex);
    MinBit    = Low(TLongIntBitIndex);
    MaxNibble = High(TLongIntNibbleIndex);
    MinNibble = Low(TLongIntNibbleIndex);
    MaxByte   = High(TLongIntByteIndex);
    MinByte   = Low(TLongIntByteIndex);
    MaxWord   = High(TLongIntWordIndex);
    MinWord   = Low(TLongIntWordIndex);
  public
    Class Function Size: Integer; inline; static;
    Class Function ToString(const AValue: LongInt): string; overload; inline; static;
    Class Function Parse(const AString: string): LongInt; inline; static;
    Class Function TryParse(const AString: string; out AValue: LongInt): Boolean; inline; static;
  protected
    function  GetBit(const aIndex: TLongIntBitIndex): boolean; inline;
    procedure PutBit(const aIndex: TLongIntBitIndex; const aNewValue: boolean); inline;
    function  GetNibble(const aIndex: TLongIntNibbleIndex): nibble; inline;
    procedure PutNibble(const aIndex: TLongIntNibbleIndex; const aNewValue: nibble); inline;
    function  GetByte(const aIndex: TLongIntByteIndex): byte;
    procedure PutByte(const aIndex: TLongIntByteIndex; const aNewValue: byte);
    function  GetWord(const aIndex: TLongIntWordIndex): word;
    procedure PutWord(const aIndex: TLongIntWordIndex; const aNewValue: word);
  public
    Function ToBoolean: Boolean; inline;
    Function ToDouble: Double; inline;
    Function ToExtended: Extended; inline;
    Function ToBinString:string; inline;
    Function ToHexString(const AMinDigits: Integer): string; overload; inline;
    Function ToHexString: string; overload; inline;
    Function ToSingle: Single; inline;
    Function ToString: string; overload; inline;
    Function SetBit(const Index: TLongIntBitIndex) : LongInt; inline;
    Function ClearBit(const Index: TLongIntBitIndex) : LongInt; inline;
    Function ToggleBit(const Index: TLongIntBitIndex) : LongInt; inline;
    Function TestBit(const Index:TLongIntBitIndex):Boolean; inline;   
    procedure Clear; inline;
    function  HighestSetBitPos: int8; inline;
    function  LowestSetBitPos: int8; inline;
    function  SetBitsCount: byte; inline;
    property  Bits   [aIndex: TLongIntBitIndex]: boolean    read GetBit    write PutBit;
    property  Nibbles[aIndex: TLongIntNibbleIndex]: nibble  read GetNibble write PutNibble;
    property  Bytes  [aIndex: TLongIntByteIndex]:  byte     read GetByte   write PutByte;
    property  Words  [aIndex: TLongIntWordIndex]:  word     read GetWord   write PutWord;
  end;

  // TQwordBitIndex is already defined
  TQwordNibbleIndex = 0..15;
  TQwordByteIndex   = 0..7;
  TQwordWordIndex   = 0..3;
  TQwordDwordIndex  = 0..1;

  TQwordOverlay = bitpacked record case integer of     // for fast extraction of longwords, words, bytes and bits
    0: (AsBit:    bitpacked array[TQwordBitIndex]    of boolean);
    1: (AsNibble: bitpacked array[TQwordNibbleIndex] of nibble);
    2: (AsByte:             array[TQwordByteIndex]   of byte);
    3: (AsWord:             array[TQwordWordIndex]   of word);
    4: (AsDword:            array[TQwordDwordIndex]  of dword);
    5: (AsQword:            qword);
    // recursive overlays:
    6: (AsByteOverlay:      array[TQwordByteIndex]   of TByteOverlay);
    7: (AsWordOverlay:      array[TQwordWordIndex]   of TWordOverlay);
    8: (AsDwordOverlay:     array[TQwordDwordIndex]  of TDwordOverlay);
  end;

  TQWordHelper = Type Helper for QWord
  const
    MaxValue  = High(qword);
    MinValue  = Low(qword);
    MaxBit    = High(TQwordBitIndex);
    MinBit    = Low(TQwordBitIndex);
    MaxNibble = High(TQwordNibbleIndex);
    MinNibble = Low(TQwordNibbleIndex);
    MaxByte   = High(TQwordByteIndex);
    MinByte   = Low(TQwordByteIndex);
    MaxWord   = High(TQwordWordIndex);
    MinWord   = Low(TQwordWordIndex);
    MaxDword  = High(TQwordDwordIndex);
    MinDword  = Low(TQwordDwordIndex);
  public
    Class Function Parse(const AString: string): Qword; inline; static;
    Class Function Size: Integer; inline; static;
    Class Function ToString(const AValue: Qword): string; overload; inline; static;
    Class Function TryParse(const AString: string; out AValue: Qword): Boolean; inline; static;
  protected
    function  GetBit(const aIndex: TQwordBitIndex): boolean; inline;
    procedure PutBit(const aIndex: TQwordBitIndex; const aNewValue: boolean); inline;
    function  GetNibble(const aIndex: TQwordNibbleIndex): nibble; inline;
    procedure PutNibble(const aIndex: TQwordNibbleIndex; const aNewValue: nibble); inline;
    function  GetByte(const aIndex: TQwordByteIndex): byte;
    procedure PutByte(const aIndex: TQwordByteIndex; const aNewValue: byte);
    function  GetWord(const aIndex: TQwordWordIndex): word;
    procedure PutWord(const aIndex: TQwordWordIndex; const aNewValue: word);
    function  GetDword(const aIndex: TQwordDwordIndex): dword;
    procedure PutDword(const aIndex: TQwordDwordIndex; const aNewValue: dword);
  public
    Function ToBoolean: Boolean; inline;
    Function ToDouble: Double; inline;
    Function ToExtended: Extended; inline;
    Function ToBinString:string; inline;
    Function ToHexString(const AMinDigits: Integer): string; overload; inline;
    Function ToHexString: string; overload; inline;
    Function ToSingle: Single; inline;
    Function ToString: string; overload; inline;
    Function SetBit(const Index: TQwordBitIndex): Qword; inline;
    Function ClearBit(const Index: TQwordBitIndex): Qword; inline;
    Function ToggleBit(const Index: TQwordBitIndex): Qword; inline;
    Function TestBit(const Index:TQwordBitIndex): Boolean; inline;
    procedure Clear; inline;
    function  HighestSetBitPos: int8; inline;
    function  LowestSetBitPos: int8; inline;
    function  SetBitsCount: byte; inline;
    property  Bits   [aIndex: TQwordBitIndex]: boolean   read GetBit    write PutBit;
    property  Nibbles[aIndex: TQwordNibbleIndex]: nibble read GetNibble write PutNibble;
    property  Bytes  [aIndex: TQwordByteIndex]: byte     read GetByte   write PutByte;
    property  Words  [aIndex: TQwordWordIndex]: word     read GetWord   write PutWord;
    property  DWords [aIndex: TQwordDwordIndex]: dword   read GetDWord  write PutDWord;
  end;

  // TInt64BitIndex is already defined
  TInt64NibbleIndex = TQwordNibbleIndex;
  TInt64ByteIndex   = TQwordByteIndex;
  TInt64WordIndex   = TQwordWordIndex;
  TInt64DwordIndex  = TQwordDwordIndex;

  TInt64Overlay = TQwordOverlay;

  TInt64Helper = Type Helper for Int64
  const
    MaxValue  = High(int64);
    MinValue  = Low(int64);
    MaxBit    = High(TInt64BitIndex);
    MinBit    = Low(TInt64BitIndex);
    MaxNibble = High(TInt64NibbleIndex);
    MinNibble = Low(TInt64NibbleIndex);
    MaxByte   = High(TInt64ByteIndex);
    MinByte   = Low(TInt64ByteIndex);
    MaxWord   = High(TInt64WordIndex);
    MinWord   = Low(TInt64WordIndex);
    MaxDword  = High(TInt64DwordIndex);
    MinDword  = Low(TInt64DwordIndex);
  public
    Class Function Parse(const AString: string): Int64; inline; static;
    Class Function Size: Integer; inline; static;
    Class Function ToString(const AValue: Int64): string; overload; inline; static;
    Class Function TryParse(const AString: string; out AValue: Int64): Boolean; inline; static;
  protected
    function  GetBit(const aIndex: TInt64BitIndex): boolean; inline;
    procedure PutBit(const aIndex: TInt64BitIndex; const aNewValue: boolean); inline;
    function  GetNibble(const aIndex: TInt64NibbleIndex): nibble; inline;
    procedure PutNibble(const aIndex: TInt64NibbleIndex; const aNewValue: nibble); inline;
    function  GetByte(const aIndex: TInt64ByteIndex): byte;
    procedure PutByte(const aIndex: TInt64ByteIndex; const aNewValue: byte);
    function  GetWord(const aIndex: TInt64WordIndex): word;
    procedure PutWord(const aIndex: TInt64WordIndex; const aNewValue: word);
    function  GetDword(const aIndex: TInt64DwordIndex): dword;
    procedure PutDword(const aIndex: TInt64DwordIndex; const aNewValue: dword);
  public
    Function ToBoolean: Boolean; inline;
    Function ToDouble: Double; inline;
    Function ToExtended: Extended; inline;
    Function ToBinString:string; inline;
    Function ToHexString(const AMinDigits: Integer): string; overload; inline;
    Function ToHexString: string; overload; inline;
    Function ToSingle: Single; inline;
    Function ToString: string; overload; inline;
    Function SetBit(const Index: TInt64BitIndex) : Int64; inline;
    Function ClearBit(const Index: TInt64BitIndex) : Int64; inline;
    Function ToggleBit(const Index: TInt64BitIndex) : Int64; inline;
    Function TestBit(const Index: TInt64BitIndex): Boolean; inline;
    procedure Clear; inline;
    function  HighestSetBitPos: int8; inline;
    function  LowestSetBitPos: int8; inline;
    function  SetBitsCount: byte; inline;
    property  Bits   [aIndex: TInt64BitIndex]: boolean   read GetBit    write PutBit;
    property  Nibbles[aIndex: TInt64NibbleIndex]: nibble read GetNibble write PutNibble;
    property  Bytes  [aIndex: TInt64ByteIndex]: byte     read GetByte   write PutByte;
    property  Words  [aIndex: TInt64WordIndex]: word     read GetWord   write PutWord;
    property  DWords [aIndex: TInt64DwordIndex]: dword   read GetDWord  write PutDWord;
  end;

  // TNativeIntBitIndex is already defined
  {$if sizeof(NativeInt)=2}
    TNativeIntNibbleIndex = TSmallIntNibbleIndex;
    TNativeIntByteIndex   = TSmallIntByteIndex;
    TNativeIntOverlay     = TSmallIntOverlay;
  {$elseif sizeof(NativeInt)=4}
    TNativeIntNibbleIndex = TLongIntNibbleIndex;
    TNativeIntByteIndex   = TLongIntByteIndex;
    TNativeIntWordIndex   = TLongIntWordIndex;
    TNativeIntOverlay     = TLongIntOverlay;
  {$elseif sizeof(NativeInt)=8}
    TNativeIntNibbleIndex = TInt64NibbleIndex;
    TNativeIntByteIndex   = TInt64ByteIndex;
    TNativeIntWordIndex   = TInt64WordIndex;
    TNativeIntDwordIndex  = TInt64DWordIndex;
    TNativeIntOverlay     = TInt64Overlay;
  {$else}
    {$fatal Unsupported NativeInt type size}
  {$endif}

  TNativeIntHelper = Type Helper for NativeInt
  const
    MaxValue  = High(NativeInt);
    MinValue  = Low(NativeInt);
    MaxBit    = High(TNativeIntBitIndex);
    MinBit    = Low(TNativeIntBitIndex);
    MaxNibble = High(TNativeIntNibbleIndex);
    MinNibble = Low(TNativeIntNibbleIndex);
    // NativeInt on cpu16 has 2 bytes, so no need for cpu16 check because we always need MaxByte and MinByte
    MaxByte   = High(TNativeIntByteIndex);
    MinByte   = Low(TNativeIntByteIndex);
    {$if sizeof(NativeInt)>=4}
    MaxWord   = High(TNativeIntWordIndex);
    MinWord   = Low(TNativeIntWordIndex);
    {$endif}
    {$ifdef sizeof(NativeInt)>=8}
    MaxDword  = High(TNativeIntDwordIndex);
    MinDword  = Low(TNativeIntDwordIndex);
    {$endif}
  public
    Class Function Parse(const AString: string): NativeInt; inline; static;
    Class Function Size: Integer; inline; static;
    Class Function ToString(const AValue: NativeInt): string; overload; inline; static;
    Class Function TryParse(const AString: string; out AValue: NativeInt): Boolean; inline; static;
  protected
    function  GetBit(const aIndex: TNativeIntBitIndex): boolean; inline;
    procedure PutBit(const aIndex: TNativeIntBitIndex; const aNewValue: boolean); inline;
    function  GetNibble(const aIndex: TNativeIntNibbleIndex): nibble; inline;
    procedure PutNibble(const aIndex: TNativeIntNibbleIndex; const aNewValue: nibble); inline;
    // NativeInt on cpu16 has 2 bytes, so no need for cpu16 check because we always need GetByte and PutByte
    function  GetByte(const aIndex: TNativeIntByteIndex): byte;
    procedure PutByte(const aIndex: TNativeIntByteIndex; const aNewValue: byte);
    {$if sizeof(NativeInt)>=4}
    function  GetWord(const aIndex: TNativeIntWordIndex): word;
    procedure PutWord(const aIndex: TNativeIntWordIndex; const aNewValue: word);
    {$endif}
    {$if sizeof(NativeInt)>=8}
    function  GetDword(const aIndex: TNativeIntDwordIndex): dword;
    procedure PutDword(const aIndex: TNativeIntDwordIndex; const aNewValue: dword);
    {$endif}
  public
    Function ToBoolean: Boolean; inline;
    Function ToDouble: Double; inline;
    Function ToExtended: Extended; inline;
    Function ToBinString:string; inline;
    Function ToHexString(const AMinDigits: Integer): string; overload; inline;
    Function ToHexString: string; overload; inline;
    Function ToSingle: Single; inline;
    Function ToString: string; overload; inline;
    Function SetBit(const Index: TNativeIntBitIndex) : NativeInt; inline;
    Function ClearBit(const Index: TNativeIntBitIndex) : NativeInt; inline;
    Function ToggleBit(const Index: TNativeIntBitIndex) : NativeInt; inline;
    Function TestBit(const Index:TNativeIntBitIndex):Boolean; inline; 
    procedure Clear; inline;
    function  HighestSetBitPos: int8; inline;
    function  LowestSetBitPos: int8; inline;
    function  SetBitsCount: byte; inline;
    property  Bits [aIndex: TNativeIntBitIndex]: boolean   read GetBit    write PutBit;
    property  Nibbles [aIndex: TNativeIntNibbleIndex]: nibble read GetNibble write PutNibble;
    // NativeInt on cpu16 has 2 bytes, so no need for cpu16 check because we always need Byte[]
    property  Bytes [aIndex: TNativeIntByteIndex]: byte     read GetByte   write PutByte;
    {$if sizeof(NativeInt)>=4}
    property  Words [aIndex: TNativeIntWordIndex]: word     read GetWord   write PutWord;
    {$endif}
    {$if sizeof(NativeInt)>=8}
    property  DWords [aIndex: TNativeIntDwordIndex]: dword   read GetDWord  write PutDWord;
    {$endif}
  end;

  // TNativeUIntBitIndex is already defined
  {$if sizeof(NativeUInt)=2}
    TNativeUIntNibbleIndex = TWordNibbleIndex;
    TNativeUIntByteIndex   = TWordByteIndex;
    TNativeUIntOverlay     = TWordOverlay;
  {$elseif sizeof(NativeUInt)=4}
    TNativeUIntNibbleIndex = TDwordNibbleIndex;
    TNativeUIntByteIndex   = TDwordByteIndex;
    TNativeUIntWordIndex   = TDwordWordIndex;
    TNativeUIntOverlay     = TDwordOverlay;
  {$elseif sizeof(NativeUInt)=8}
    TNativeUIntNibbleIndex = TQwordNibbleIndex;
    TNativeUIntByteIndex   = TQwordByteIndex;
    TNativeUIntWordIndex   = TQwordWordIndex;
    TNativeUIntDwordIndex  = TQwordDWordIndex;
    TNativeUIntOverlay     = TQwordOverlay;
  {$else}
    {$fatal Unsupported NativeUInt type size}
  {$endif}

  TNativeUIntHelper = Type Helper for NativeUInt
  const
    MaxValue  = High(NativeUInt);
    MinValue  = 0;
    MaxBit    = High(TNativeUIntBitIndex);
    MinBit    = Low(TNativeUIntBitIndex);
    MaxNibble = High(TNativeUIntNibbleIndex);
    MinNibble = Low(TNativeUIntNibbleIndex);
    // NativeInt on cpu16 has 2 bytes, so no need for cpu16 check because we always need MaxByte and MinByte
    MaxByte   = High(TNativeUIntByteIndex);
    MinByte   = Low(TNativeUIntByteIndex);
    {$if sizeof(NativeUInt)>=4}
    MaxWord   = High(TNativeUIntWordIndex);
    MinWord   = Low(TNativeUIntWordIndex);
    {$endif}
    {$if sizeof(NativeUInt)>=8}
    MaxDword  = High(TNativeUIntDwordIndex);
    MinDword  = Low(TNativeUIntDwordIndex);
    {$endif}
  public
    Class Function Parse(const AString: string): NativeUInt; inline; static;
    Class Function Size: Integer; inline; static;
    Class Function ToString(const AValue: NativeUInt): string; overload; inline; static;
    Class Function TryParse(const AString: string; out AValue: NativeUInt): Boolean; inline; static;
  protected
    function  GetBit(const aIndex: TNativeUIntBitIndex): boolean; inline;
    procedure PutBit(const aIndex: TNativeUIntBitIndex; const aNewValue: boolean); inline;
    function  GetNibble(const aIndex: TNativeUIntNibbleIndex): nibble; inline;
    procedure PutNibble(const aIndex: TNativeUIntNibbleIndex; const aNewValue: nibble); inline;
    // NativeInt on cpu16 has 2 bytes, so no need for cpu16 check because we always need GetByte and PutByte
    function  GetByte(const aIndex: TNativeUIntByteIndex): byte;
    procedure PutByte(const aIndex: TNativeUIntByteIndex; const aNewValue: byte);
    {$if sizeof(NativeUInt)>=4}
    function  GetWord(const aIndex: TNativeUIntWordIndex): word;
    procedure PutWord(const aIndex: TNativeUIntWordIndex; const aNewValue: word);
    {$endif}
    {$if sizeof(NativeUInt)>=8}
    function  GetDword(const aIndex: TNativeUIntDwordIndex): dword;
    procedure PutDword(const aIndex: TNativeUIntDwordIndex; const aNewValue: dword);
    {$endif}
  public
    Function ToBoolean: Boolean; inline;
    Function ToDouble: Double; inline;
    Function ToExtended: Extended; inline;
    Function ToBinString:string; inline;
    Function ToHexString(const AMinDigits: Integer): string; overload; inline;
    Function ToHexString: string; overload; inline;
    Function ToSingle: Single; inline;
    Function ToString: string; overload; inline;
    Function SetBit(const Index: TNativeUIntBitIndex) : NativeUint; inline;
    Function ClearBit(const Index: TNativeUIntBitIndex): NativeUint; inline;
    Function ToggleBit(const Index: TNativeUIntBitIndex) : NativeUint; inline;
    Function TestBit(const Index:TNativeUIntBitIndex) :Boolean; inline;
    procedure Clear; inline;
    function  HighestSetBitPos: int8; inline;
    function  LowestSetBitPos: int8; inline;
    function  SetBitsCount: byte; inline;
    property  Bits [aIndex: TNativeUIntBitIndex]: boolean   read GetBit    write PutBit;
    property  Nibbles [aIndex: TNativeUIntNibbleIndex]: nibble read GetNibble write PutNibble;
    // NativeInt on cpu16 has 2 bytes, so no need for cpu16 check because we always need Byte[]
    property  Bytes [aIndex: TNativeUIntByteIndex]: byte     read GetByte   write PutByte;
    {$if sizeof(NativeUInt)>=4}
    property  Words [aIndex: TNativeUIntWordIndex]: word     read GetWord   write PutWord;
    {$endif}
    {$if sizeof(NativeUInt)>=8}
    property  DWords [aIndex: TNativeUIntDwordIndex]: dword   read GetDWord  write PutDWord;
    {$endif}
  end;

  {$SCOPEDENUMS ON}
    TUseBoolStrs = (False, True);
  {$SCOPEDENUMS OFF}

  TBooleanHelper = Type Helper for Boolean
  public
    Class Function Parse(const S: string): Boolean; inline; static;
    Class Function Size: Integer; inline; static;
    Class Function ToString(const AValue: Boolean; UseBoolStrs: TUseBoolStrs = TUseBoolStrs.False): string; overload; inline; static;
    Class Function TryToParse(const S: string; out AValue: Boolean): Boolean; inline; static;
  Public
    Function ToInteger: Integer; inline;
    Function ToString(UseBoolStrs: TUseBoolStrs = TUseBoolStrs.False): string; overload; inline;
  end;

  TBoolean8Helper = Type Helper for Boolean8
  public
    Class Function Parse(const S: string): Boolean; inline; static;
    Class Function Size: Integer; inline; static;
    Class Function ToString(const AValue: Boolean; UseBoolStrs: TUseBoolStrs = TUseBoolStrs.False): string; overload; inline; static;
    Class Function TryToParse(const S: string; out AValue: Boolean): Boolean; inline; static;
  Public
    Function ToInteger: Integer; inline;
    Function ToString(UseBoolStrs: TUseBoolStrs = TUseBoolStrs.False): string; overload; inline;
  end;

  TBoolean16Helper = Type Helper for Boolean16
  public
    Class Function Parse(const S: string): Boolean; inline; static;
    Class Function Size: Integer; inline; static;
    Class Function ToString(const AValue: Boolean; UseBoolStrs: TUseBoolStrs = TUseBoolStrs.False): string; overload; inline; static;
    Class Function TryToParse(const S: string; out AValue: Boolean): Boolean; inline; static;
  Public
    Function ToInteger: Integer; inline;
    Function ToString(UseBoolStrs: TUseBoolStrs = TUseBoolStrs.False): string; overload; inline;
  end;

  TBoolean32Helper = Type Helper for Boolean32
  public
    Class Function Parse(const S: string): Boolean; inline; static;
    Class Function Size: Integer; inline; static;
    Class Function ToString(const AValue: Boolean; UseBoolStrs: TUseBoolStrs = TUseBoolStrs.False): string; overload; inline; static;
    Class Function TryToParse(const S: string; out AValue: Boolean): Boolean; inline; static;
  Public
    Function ToInteger: Integer; inline;
    Function ToString(UseBoolStrs: TUseBoolStrs = TUseBoolStrs.False): string; overload; inline;
  end;

  TBoolean64Helper = Type Helper for Boolean64
  public
    Class Function Parse(const S: string): Boolean; inline; static;
    Class Function Size: Integer; inline; static;
    Class Function ToString(const AValue: Boolean; UseBoolStrs: TUseBoolStrs = TUseBoolStrs.False): string; overload; inline; static;
    Class Function TryToParse(const S: string; out AValue: Boolean): Boolean; inline; static;
  Public
    Function ToInteger: Integer; inline;
    Function ToString(UseBoolStrs: TUseBoolStrs = TUseBoolStrs.False): string; overload; inline;
  end;

  TByteBoolHelper = Type Helper for ByteBool
  public
    Class Function Parse(const S: string): Boolean; inline; static;
    Class Function Size: Integer; inline; static;
    Class Function ToString(const AValue: Boolean; UseBoolStrs: TUseBoolStrs = TUseBoolStrs.False): string; overload; inline; static;
    Class Function TryToParse(const S: string; out AValue: Boolean): Boolean; inline; static;
  Public
    Function ToInteger: Integer; inline;
    Function ToString(UseBoolStrs: TUseBoolStrs = TUseBoolStrs.False): string; overload; inline;
  end;

  TWordBoolHelper = Type Helper for WordBool
  public
    Class Function Parse(const S: string): Boolean; inline; static;
    Class Function Size: Integer; inline; static;
    Class Function ToString(const AValue: Boolean; UseBoolStrs: TUseBoolStrs = TUseBoolStrs.False): string; overload; inline; static;
    Class Function TryToParse(const S: string; out AValue: Boolean): Boolean; inline; static;
  Public
    Function ToInteger: Integer; inline;
    Function ToString(UseBoolStrs: TUseBoolStrs = TUseBoolStrs.False): string; overload; inline;
  end;

  TLongBoolHelper = Type Helper for LongBool
  public
    Class Function Parse(const S: string): Boolean; inline; static;
    Class Function Size: Integer; inline; static;
    Class Function ToString(const AValue: Boolean; UseBoolStrs: TUseBoolStrs = TUseBoolStrs.False): string; overload; inline; static;
    Class Function TryToParse(const S: string; out AValue: Boolean): Boolean; inline; static;
  public
    Function ToInteger: Integer; inline;
    Function ToString(UseBoolStrs: TUseBoolStrs = TUseBoolStrs.False): string; overload; inline;
  end;

  TQWordBoolHelper = Type Helper for QWordBool
  public
    Class Function Parse(const S: string): Boolean; inline; static;
    Class Function Size: Integer; inline; static;
    Class Function ToString(const AValue: Boolean; UseBoolStrs: TUseBoolStrs = TUseBoolStrs.False): string; overload; inline; static;
    Class Function TryToParse(const S: string; out AValue: Boolean): Boolean; inline; static;
  public
    Function ToInteger: Integer; inline;
    Function ToString(UseBoolStrs: TUseBoolStrs = TUseBoolStrs.False): string; overload; inline;
  end;
