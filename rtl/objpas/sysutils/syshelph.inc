{%MainUnit sysutils.pp}

Type
  TStringArray = Array of string;
  TAnsiStringArray = Array of Ansistring;
  TUnicodeStringArray = Array of UnicodeString;
  TWideStringArray = Array of WideString;
  TShortStringArray = Array of ShortString;
  TCharArray = Array of AnsiChar;
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

  { TAnsiStringHelper }

  TAnsiStringHelper = Type Helper for AnsiString
  Private
    Function GetChar(AIndex : SizeInt) : AnsiChar;
    Function GetLength : SizeInt;
  public
    const Empty = '';
    // Methods
    Class Function Compare(const A: AnsiString; const B: AnsiString): Integer; overload; static; //inline;
    Class Function Compare(const A: AnsiString; const B: AnsiString; IgnoreCase: Boolean): Integer; overload; static; //inline; //deprecated 'Use same with TCompareOptions';
    Class Function Compare(const A: AnsiString; const B: AnsiString; Options: TCompareOptions): Integer; overload; static; // inline;
    Class Function Compare(const A: AnsiString; IndexA: SizeInt; const B: AnsiString; IndexB: SizeInt; ALen: SizeInt): Integer; overload; static; // inline;
    Class Function Compare(const A: AnsiString; IndexA: SizeInt; const B: AnsiString; IndexB: SizeInt; ALen: SizeInt; IgnoreCase: Boolean): Integer; overload; static; // inline; //deprecated 'Use same with TCompareOptions';
    Class Function Compare(const A: AnsiString; IndexA: SizeInt; const B: AnsiString; IndexB: SizeInt; ALen: SizeInt; Options: TCompareOptions): Integer; overload; static;//  inline;
    Class Function CompareOrdinal(const A: AnsiString; const B: AnsiString): Integer; overload; static;
    Class Function CompareOrdinal(const A: AnsiString; IndexA: SizeInt; const B: AnsiString; IndexB: SizeInt; ALen: SizeInt): Integer; overload; static;
    Class Function CompareText(const A: AnsiString; const B: AnsiString): Integer; static; inline;
    Class Function Copy(const Str: AnsiString): AnsiString; inline; static;
    Class Function Create(AChar: AnsiChar; ACount: SizeInt): AnsiString; overload; inline; static;
    Class Function Create(const AValue: array of AnsiChar): AnsiString; overload; static;
    Class Function Create(const AValue: array of AnsiChar; StartIndex: SizeInt; ALen: SizeInt): AnsiString; overload; static;
    Class Function EndsText(const ASubText, AText: AnsiString): Boolean; static;
    Class Function Equals(const a: AnsiString; const b: AnsiString): Boolean; overload; static;
    Class Function Format(const AFormat: AnsiString; const args: array of const): AnsiString; overload; static;
    Class Function IsNullOrEmpty(const AValue: AnsiString): Boolean; static;
    Class Function IsNullOrWhiteSpace(const AValue: AnsiString): Boolean; static;
    Class Function Join(const Separator: AnsiString; const Values: array of const): AnsiString; overload; static;
    Class Function Join(const Separator: AnsiString; const Values: array of AnsiString): AnsiString; overload; static;
    Class Function Join(const Separator: AnsiString; const Values: array of AnsiString; StartIndex: SizeInt; ACount: SizeInt): AnsiString; overload; static;
    Class Function LowerCase(const S: AnsiString): AnsiString; overload; static; inline;
    Class Function Parse(const AValue: Boolean): AnsiString; overload; static; inline;
    Class Function Parse(const AValue: Extended): AnsiString; overload; static;inline;
    Class Function Parse(const AValue: Int64): AnsiString; overload; static; inline;
    Class Function Parse(const AValue: Integer): AnsiString; overload; static; inline;
    Class Function ToBoolean(const S: AnsiString): Boolean; overload; static; inline;
    Class Function ToDouble(const S: AnsiString): Double; overload; static; inline;
    Class Function ToExtended(const S: AnsiString): Extended; overload; static; inline;
    Class Function ToInt64(const S: AnsiString): Int64; overload; static; inline;
    Class Function ToInteger(const S: AnsiString): Integer; overload; static; inline;
    Class Function ToSingle(const S: AnsiString): Single; overload; static; inline;
    Class Function UpperCase(const S: AnsiString): AnsiString; overload; static; inline;
    Function CompareTo(const B: AnsiString): Integer;
    Function Contains(const AValue: AnsiString; IgnoreCase: Boolean = False): Boolean;
    procedure CopyTo(SourceIndex: SizeInt; var destination: array of AnsiChar; DestinationIndex: SizeInt; ACount: SizeInt);
    Function CountChar(const C: AnsiChar): SizeInt;
    Function DeQuotedString: AnsiString; overload;
    Function DeQuotedString(const AQuoteChar: AnsiChar): AnsiString; overload;
    Function EndsWith(const AValue: AnsiString): Boolean; overload; inline;
    Function EndsWith(const AValue: AnsiString; IgnoreCase: Boolean): Boolean; overload;
    Function Equals(const AValue: AnsiString; IgnoreCase: Boolean = False): Boolean; overload;
    Function Format(const args: array of const): AnsiString; overload;
    Function GetHashCode: Integer;
    Function IndexOf(AValue: AnsiChar): SizeInt; overload; inline;
    Function IndexOf(const AValue: AnsiString): SizeInt; overload; inline;
    Function IndexOf(AValue: AnsiChar; StartIndex: SizeInt): SizeInt; overload;
    Function IndexOf(const AValue: AnsiString; StartIndex: SizeInt): SizeInt; overload;
    Function IndexOf(AValue: AnsiChar; StartIndex: SizeInt; ACount: SizeInt): SizeInt; overload;
    Function IndexOf(const AValue: AnsiString; StartIndex: SizeInt; ACount: SizeInt): SizeInt; overload;
    Function IndexOfUnQuoted(const AValue: AnsiString; StartQuote, EndQuote: AnsiChar; StartIndex: SizeInt = 0): SizeInt; overload;
    Function IndexOfAny(const AnyOf: array of AnsiChar): SizeInt; overload;
    Function IndexOfAny(const AnyOf: array of AnsiChar; StartIndex: SizeInt): SizeInt; overload;
    Function IndexOfAny(const AnyOf: array of AnsiChar; StartIndex: SizeInt; ACount: SizeInt): SizeInt; overload;
    Function IndexOfAny(const AnyOf: array of AnsiString): SizeInt; overload;
    Function IndexOfAny(const AnyOf: array of AnsiString; StartIndex: SizeInt): SizeInt; overload;
    Function IndexOfAny(const AnyOf: array of AnsiString; StartIndex: SizeInt; ACount: SizeInt): SizeInt; overload;
    Function IndexOfAny(const AnyOf: array of AnsiString; StartIndex: SizeInt; ACount: SizeInt; Out AMatch : SizeInt): SizeInt; overload;
    Function IndexOfAnyUnquoted(const AnyOf: array of AnsiChar; StartQuote, EndQuote: AnsiChar): SizeInt; overload;
    Function IndexOfAnyUnquoted(const AnyOf: array of AnsiChar; StartQuote, EndQuote: AnsiChar; StartIndex: SizeInt): SizeInt; overload;
    Function IndexOfAnyUnquoted(const AnyOf: array of AnsiChar; StartQuote, EndQuote: AnsiChar; StartIndex: SizeInt; ACount: SizeInt): SizeInt; overload;
    function IndexOfAnyUnquoted(const AnyOf: array of AnsiString; StartQuote, EndQuote: AnsiChar; StartIndex: SizeInt; Out Matched: SizeInt): SizeInt; overload;
    Function Insert(StartIndex: SizeInt; const AValue: AnsiString): AnsiString;
    Function IsDelimiter(const Delimiters: AnsiString; Index: SizeInt): Boolean;
    Function IsEmpty: Boolean;
    Function LastDelimiter(const Delims: AnsiString): SizeInt;
    Function LastIndexOf(AValue: AnsiChar): SizeInt; overload;
    Function LastIndexOf(const AValue: AnsiString): SizeInt; overload;
    Function LastIndexOf(AValue: AnsiChar; AStartIndex: SizeInt): SizeInt; overload;
    Function LastIndexOf(const AValue: AnsiString; AStartIndex: SizeInt): SizeInt; overload;
    Function LastIndexOf(AValue: AnsiChar; AStartIndex: SizeInt; ACount: SizeInt): SizeInt; overload;
    Function LastIndexOf(const AValue: AnsiString; AStartIndex: SizeInt; ACount: SizeInt): SizeInt; overload;
    Function LastIndexOfAny(const AnyOf: array of AnsiChar): SizeInt; overload;
    Function LastIndexOfAny(const AnyOf: array of AnsiChar; AStartIndex: SizeInt): SizeInt; overload;
    Function LastIndexOfAny(const AnyOf: array of AnsiChar; AStartIndex: SizeInt; ACount: SizeInt): SizeInt; overload;
    Function PadLeft(ATotalWidth: SizeInt): AnsiString; overload; inline;
    Function PadLeft(ATotalWidth: SizeInt; PaddingChar: AnsiChar): AnsiString; overload; inline;
    Function PadRight(ATotalWidth: SizeInt): AnsiString; overload; inline;
    Function PadRight(ATotalWidth: SizeInt; PaddingChar: AnsiChar): AnsiString; overload; inline;
    Function QuotedString: AnsiString; overload;
    Function QuotedString(const AQuoteChar: AnsiChar): AnsiString; overload;
    Function Remove(StartIndex: SizeInt): AnsiString; overload; inline;
    Function Remove(StartIndex: SizeInt; ACount: SizeInt): AnsiString; overload; inline;
    Function Replace(OldChar: AnsiChar; NewChar: AnsiChar): AnsiString; overload;
    Function Replace(OldChar: AnsiChar; NewChar: AnsiChar; ReplaceFlags: TReplaceFlags): AnsiString; overload;
    Function Replace(const OldValue: AnsiString; const NewValue: AnsiString): AnsiString; overload;
    Function Replace(const OldValue: AnsiString; const NewValue: AnsiString; ReplaceFlags: TReplaceFlags): AnsiString; overload;
    Function Split(const Separators: array of AnsiChar): TAnsiStringArray; overload;
    Function Split(const Separators: array of AnsiChar; ACount: SizeInt): TAnsiStringArray; overload;
    Function Split(const Separators: array of AnsiChar; Options: TStringSplitOptions): TAnsiStringArray; overload;
    Function Split(const Separators: array of AnsiChar; ACount: SizeInt; Options: TStringSplitOptions): TAnsiStringArray; overload;
    Function Split(const Separators: array of AnsiString): TAnsiStringArray; overload;
    Function Split(const Separators: array of AnsiString; ACount: SizeInt): TAnsiStringArray; overload;
    Function Split(const Separators: array of AnsiString; Options: TStringSplitOptions): TAnsiStringArray; overload;
    Function Split(const Separators: array of AnsiString; ACount: SizeInt; Options: TStringSplitOptions): TAnsiStringArray; overload;
    Function Split(const Separators: array of AnsiChar; AQuote: AnsiChar): TAnsiStringArray; overload;
    Function Split(const Separators: array of AnsiChar; AQuoteStart, AQuoteEnd: AnsiChar): TAnsiStringArray; overload;
    Function Split(const Separators: array of AnsiChar; AQuoteStart, AQuoteEnd: AnsiChar; Options: TStringSplitOptions): TAnsiStringArray; overload;
    Function Split(const Separators: array of AnsiChar; AQuoteStart, AQuoteEnd: AnsiChar; ACount: SizeInt): TAnsiStringArray; overload;
    Function Split(const Separators: array of AnsiChar; AQuoteStart, AQuoteEnd: AnsiChar; ACount: SizeInt; Options: TStringSplitOptions): TAnsiStringArray; overload;
    Function Split(const Separators: array of AnsiString; AQuote: AnsiChar): TAnsiStringArray; overload;
    Function Split(const Separators: array of AnsiString; AQuoteStart, AQuoteEnd: AnsiChar): TAnsiStringArray; overload;
    Function Split(const Separators: array of AnsiString; AQuoteStart, AQuoteEnd: AnsiChar; Options: TStringSplitOptions): TAnsiStringArray; overload;
    Function Split(const Separators: array of AnsiString; AQuoteStart, AQuoteEnd: AnsiChar; ACount: SizeInt): TAnsiStringArray; overload;
    Function Split(const Separators: array of AnsiString; AQuoteStart, AQuoteEnd: AnsiChar; ACount: SizeInt; Options: TStringSplitOptions): TAnsiStringArray; overload;
    Function StartsWith(const AValue: AnsiString): Boolean; overload; inline;
    Function StartsWith(const AValue: AnsiString; IgnoreCase: Boolean): Boolean; overload;
    Function Substring(AStartIndex: SizeInt): AnsiString; overload;
    Function Substring(AStartIndex: SizeInt; ALen: SizeInt): AnsiString; overload;
    Function ToBoolean: Boolean; overload; inline;
    Function ToInteger: Integer; overload; inline;
    Function ToInt64: Int64; overload; inline;
    Function ToSingle: Single; overload; inline;
    Function ToDouble: Double; overload; inline;
    Function ToExtended: Extended; overload; inline;
    Function ToCharArray: TCharArray; overload;
    Function ToCharArray(AStartIndex: SizeInt; ALen: SizeInt): TCharArray; overload;
    Function ToLower: AnsiString; overload; inline;
    Function ToLowerInvariant: AnsiString;
    Function ToUpper: AnsiString; overload; inline;
    Function ToUpperInvariant: AnsiString; inline;
    Function Trim: AnsiString; overload;
    Function TrimLeft: AnsiString; overload;
    Function TrimRight: AnsiString; overload;
    Function Trim(const ATrimChars: array of AnsiChar): AnsiString; overload;
    Function TrimLeft(const ATrimChars: array of AnsiChar): AnsiString; overload;
    Function TrimRight(const ATrimChars: array of AnsiChar): AnsiString; overload;
    Function TrimEnd(const ATrimChars: array of AnsiChar): AnsiString; deprecated 'Use TrimRight';
    Function TrimStart(const ATrimChars: array of AnsiChar): AnsiString; deprecated 'Use TrimLeft';
    property Chars[AIndex: SizeInt]: AnsiChar read GetChar;
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

  { TWideStringHelper }

  TWideStringHelper = Type Helper for WideString
  Private
    Function GetChar(AIndex : SizeInt) : WideChar;
    Function GetLength : SizeInt;
  public
    const Empty = '';
    // Methods
    Class Function Compare(const A: WideString; const B: WideString): Integer; overload; static; //inline;
    Class Function Compare(const A: WideString; const B: WideString; IgnoreCase: Boolean): Integer; overload; static; //inline; //deprecated 'Use same with TCompareOptions';
    Class Function Compare(const A: WideString; const B: WideString; Options: TCompareOptions): Integer; overload; static; // inline;
    Class Function Compare(const A: WideString; IndexA: SizeInt; const B: WideString; IndexB: SizeInt; ALen: SizeInt): Integer; overload; static; // inline;
    Class Function Compare(const A: WideString; IndexA: SizeInt; const B: WideString; IndexB: SizeInt; ALen: SizeInt; IgnoreCase: Boolean): Integer; overload; static; // inline; //deprecated 'Use same with TCompareOptions';
    Class Function Compare(const A: WideString; IndexA: SizeInt; const B: WideString; IndexB: SizeInt; ALen: SizeInt; Options: TCompareOptions): Integer; overload; static;//  inline;
    Class Function CompareOrdinal(const A: WideString; const B: WideString): Integer; overload; static;
    Class Function CompareOrdinal(const A: WideString; IndexA: SizeInt; const B: WideString; IndexB: SizeInt; ALen: SizeInt): Integer; overload; static;
    Class Function CompareText(const A: WideString; const B: WideString): Integer; static; inline;
    Class Function Copy(const Str: WideString): WideString; inline; static;
    Class Function Create(AChar: WideChar; ACount: SizeInt): WideString; overload; inline; static;
    Class Function Create(const AValue: array of WideChar): WideString; overload; static;
    Class Function Create(const AValue: array of WideChar; StartIndex: SizeInt; ALen: SizeInt): WideString; overload; static;
    Class Function EndsText(const ASubText, AText: WideString): Boolean; static;
    Class Function Equals(const a: WideString; const b: WideString): Boolean; overload; static;
    Class Function Format(const AFormat: WideString; const args: array of const): WideString; overload; static;
    Class Function IsNullOrEmpty(const AValue: WideString): Boolean; static;
    Class Function IsNullOrWhiteSpace(const AValue: WideString): Boolean; static;
    Class Function Join(const Separator: WideString; const Values: array of const): WideString; overload; static;
    Class Function Join(const Separator: WideString; const Values: array of WideString): WideString; overload; static;
    Class Function Join(const Separator: WideString; const Values: array of WideString; StartIndex: SizeInt; ACount: SizeInt): WideString; overload; static;
    Class Function LowerCase(const S: WideString): WideString; overload; static; inline;
    Class Function Parse(const AValue: Boolean): WideString; overload; static; inline;
    Class Function Parse(const AValue: Extended): WideString; overload; static;inline;
    Class Function Parse(const AValue: Int64): WideString; overload; static; inline;
    Class Function Parse(const AValue: Integer): WideString; overload; static; inline;
    Class Function ToBoolean(const S: WideString): Boolean; overload; static; inline;
    Class Function ToDouble(const S: WideString): Double; overload; static; inline;
    Class Function ToExtended(const S: WideString): Extended; overload; static; inline;
    Class Function ToInt64(const S: WideString): Int64; overload; static; inline;
    Class Function ToInteger(const S: WideString): Integer; overload; static; inline;
    Class Function ToSingle(const S: WideString): Single; overload; static; inline;
    Class Function UpperCase(const S: WideString): WideString; overload; static; inline;
    Function CompareTo(const B: WideString): Integer;
    Function Contains(const AValue: WideString; IgnoreCase: Boolean = False): Boolean;
    procedure CopyTo(SourceIndex: SizeInt; var destination: array of WideChar; DestinationIndex: SizeInt; ACount: SizeInt);
    Function CountChar(const C: WideChar): SizeInt;
    Function DeQuotedString: WideString; overload;
    Function DeQuotedString(const AQuoteChar: WideChar): WideString; overload;
    Function EndsWith(const AValue: WideString): Boolean; overload; inline;
    Function EndsWith(const AValue: WideString; IgnoreCase: Boolean): Boolean; overload;
    Function Equals(const AValue: WideString; IgnoreCase: Boolean = False): Boolean; overload;
    Function Format(const args: array of const): WideString; overload;
    Function GetHashCode: Integer;
    Function IndexOf(AValue: WideChar): SizeInt; overload; inline;
    Function IndexOf(const AValue: WideString): SizeInt; overload; inline;
    Function IndexOf(AValue: WideChar; StartIndex: SizeInt): SizeInt; overload;
    Function IndexOf(const AValue: WideString; StartIndex: SizeInt): SizeInt; overload;
    Function IndexOf(AValue: WideChar; StartIndex: SizeInt; ACount: SizeInt): SizeInt; overload;
    Function IndexOf(const AValue: WideString; StartIndex: SizeInt; ACount: SizeInt): SizeInt; overload;
    Function IndexOfUnQuoted(const AValue: WideString; StartQuote, EndQuote: WideChar; StartIndex: SizeInt = 0): SizeInt; overload;
    Function IndexOfAny(const AnyOf: array of WideChar): SizeInt; overload;
    Function IndexOfAny(const AnyOf: array of WideChar; StartIndex: SizeInt): SizeInt; overload;
    Function IndexOfAny(const AnyOf: array of WideChar; StartIndex: SizeInt; ACount: SizeInt): SizeInt; overload;
    Function IndexOfAny(const AnyOf: array of WideString): SizeInt; overload;
    Function IndexOfAny(const AnyOf: array of WideString; StartIndex: SizeInt): SizeInt; overload;
    Function IndexOfAny(const AnyOf: array of WideString; StartIndex: SizeInt; ACount: SizeInt): SizeInt; overload;
    Function IndexOfAny(const AnyOf: array of WideString; StartIndex: SizeInt; ACount: SizeInt; Out AMatch : SizeInt): SizeInt; overload;
    Function IndexOfAnyUnquoted(const AnyOf: array of WideChar; StartQuote, EndQuote: WideChar): SizeInt; overload;
    Function IndexOfAnyUnquoted(const AnyOf: array of WideChar; StartQuote, EndQuote: WideChar; StartIndex: SizeInt): SizeInt; overload;
    Function IndexOfAnyUnquoted(const AnyOf: array of WideChar; StartQuote, EndQuote: WideChar; StartIndex: SizeInt; ACount: SizeInt): SizeInt; overload;
    function IndexOfAnyUnquoted(const AnyOf: array of WideString; StartQuote, EndQuote: WideChar; StartIndex: SizeInt; Out Matched: SizeInt): SizeInt; overload;
    Function Insert(StartIndex: SizeInt; const AValue: WideString): WideString;
    Function IsDelimiter(const Delimiters: WideString; Index: SizeInt): Boolean;
    Function IsEmpty: Boolean;
    Function LastDelimiter(const Delims: WideString): SizeInt;
    Function LastIndexOf(AValue: WideChar): SizeInt; overload;
    Function LastIndexOf(const AValue: WideString): SizeInt; overload;
    Function LastIndexOf(AValue: WideChar; AStartIndex: SizeInt): SizeInt; overload;
    Function LastIndexOf(const AValue: WideString; AStartIndex: SizeInt): SizeInt; overload;
    Function LastIndexOf(AValue: WideChar; AStartIndex: SizeInt; ACount: SizeInt): SizeInt; overload;
    Function LastIndexOf(const AValue: WideString; AStartIndex: SizeInt; ACount: SizeInt): SizeInt; overload;
    Function LastIndexOfAny(const AnyOf: array of WideChar): SizeInt; overload;
    Function LastIndexOfAny(const AnyOf: array of WideChar; AStartIndex: SizeInt): SizeInt; overload;
    Function LastIndexOfAny(const AnyOf: array of WideChar; AStartIndex: SizeInt; ACount: SizeInt): SizeInt; overload;
    Function PadLeft(ATotalWidth: SizeInt): WideString; overload; inline;
    Function PadLeft(ATotalWidth: SizeInt; PaddingChar: WideChar): WideString; overload; inline;
    Function PadRight(ATotalWidth: SizeInt): WideString; overload; inline;
    Function PadRight(ATotalWidth: SizeInt; PaddingChar: WideChar): WideString; overload; inline;
    Function QuotedString: WideString; overload;
    Function QuotedString(const AQuoteChar: WideChar): WideString; overload;
    Function Remove(StartIndex: SizeInt): WideString; overload; inline;
    Function Remove(StartIndex: SizeInt; ACount: SizeInt): WideString; overload; inline;
    Function Replace(OldChar: WideChar; NewChar: WideChar): WideString; overload;
    Function Replace(OldChar: WideChar; NewChar: WideChar; ReplaceFlags: TReplaceFlags): WideString; overload;
    Function Replace(const OldValue: WideString; const NewValue: WideString): WideString; overload;
    Function Replace(const OldValue: WideString; const NewValue: WideString; ReplaceFlags: TReplaceFlags): WideString; overload;
    Function Split(const Separators: array of WideChar): TWideStringArray; overload;
    Function Split(const Separators: array of WideChar; ACount: SizeInt): TWideStringArray; overload;
    Function Split(const Separators: array of WideChar; Options: TStringSplitOptions): TWideStringArray; overload;
    Function Split(const Separators: array of WideChar; ACount: SizeInt; Options: TStringSplitOptions): TWideStringArray; overload;
    Function Split(const Separators: array of WideString): TWideStringArray; overload;
    Function Split(const Separators: array of WideString; ACount: SizeInt): TWideStringArray; overload;
    Function Split(const Separators: array of WideString; Options: TStringSplitOptions): TWideStringArray; overload;
    Function Split(const Separators: array of WideString; ACount: SizeInt; Options: TStringSplitOptions): TWideStringArray; overload;
    Function Split(const Separators: array of WideChar; AQuote: WideChar): TWideStringArray; overload;
    Function Split(const Separators: array of WideChar; AQuoteStart, AQuoteEnd: WideChar): TWideStringArray; overload;
    Function Split(const Separators: array of WideChar; AQuoteStart, AQuoteEnd: WideChar; Options: TStringSplitOptions): TWideStringArray; overload;
    Function Split(const Separators: array of WideChar; AQuoteStart, AQuoteEnd: WideChar; ACount: SizeInt): TWideStringArray; overload;
    Function Split(const Separators: array of WideChar; AQuoteStart, AQuoteEnd: WideChar; ACount: SizeInt; Options: TStringSplitOptions): TWideStringArray; overload;
    Function Split(const Separators: array of WideString; AQuote: WideChar): TWideStringArray; overload;
    Function Split(const Separators: array of WideString; AQuoteStart, AQuoteEnd: WideChar): TWideStringArray; overload;
    Function Split(const Separators: array of WideString; AQuoteStart, AQuoteEnd: WideChar; Options: TStringSplitOptions): TWideStringArray; overload;
    Function Split(const Separators: array of WideString; AQuoteStart, AQuoteEnd: WideChar; ACount: SizeInt): TWideStringArray; overload;
    Function Split(const Separators: array of WideString; AQuoteStart, AQuoteEnd: WideChar; ACount: SizeInt; Options: TStringSplitOptions): TWideStringArray; overload;
    Function StartsWith(const AValue: WideString): Boolean; overload; inline;
    Function StartsWith(const AValue: WideString; IgnoreCase: Boolean): Boolean; overload;
    Function Substring(AStartIndex: SizeInt): WideString; overload;
    Function Substring(AStartIndex: SizeInt; ALen: SizeInt): WideString; overload;
    Function ToBoolean: Boolean; overload; inline;
    Function ToInteger: Integer; overload; inline;
    Function ToInt64: Int64; overload; inline;
    Function ToSingle: Single; overload; inline;
    Function ToDouble: Double; overload; inline;
    Function ToExtended: Extended; overload; inline;
    Function ToCharArray: TCharArray; overload;
    Function ToCharArray(AStartIndex: SizeInt; ALen: SizeInt): TCharArray; overload;
    Function ToLower: WideString; overload; inline;
    Function ToLowerInvariant: WideString;
    Function ToUpper: WideString; overload; inline;
    Function ToUpperInvariant: WideString; inline;
    Function Trim: WideString; overload;
    Function TrimLeft: WideString; overload;
    Function TrimRight: WideString; overload;
    Function Trim(const ATrimChars: array of WideChar): WideString; overload;
    Function TrimLeft(const ATrimChars: array of WideChar): WideString; overload;
    Function TrimRight(const ATrimChars: array of WideChar): WideString; overload;
    Function TrimEnd(const ATrimChars: array of WideChar): WideString; deprecated 'Use TrimRight';
    Function TrimStart(const ATrimChars: array of WideChar): WideString; deprecated 'Use TrimLeft';
    property Chars[AIndex: SizeInt]: WideChar read GetChar;
    property Length: SizeInt read GetLength;
  end;

  { TUnicodeStringHelper }

  TUnicodeStringHelper = Type Helper for UnicodeString
  Private
    Function GetChar(AIndex : SizeInt) : UnicodeChar;
    Function GetLength : SizeInt;
  public
    const Empty = '';
    // Methods
    Class Function Compare(const A: UnicodeString; const B: UnicodeString): Integer; overload; static; //inline;
    Class Function Compare(const A: UnicodeString; const B: UnicodeString; IgnoreCase: Boolean): Integer; overload; static; //inline; //deprecated 'Use same with TCompareOptions';
    Class Function Compare(const A: UnicodeString; const B: UnicodeString; Options: TCompareOptions): Integer; overload; static; // inline;
    Class Function Compare(const A: UnicodeString; IndexA: SizeInt; const B: UnicodeString; IndexB: SizeInt; ALen: SizeInt): Integer; overload; static; // inline;
    Class Function Compare(const A: UnicodeString; IndexA: SizeInt; const B: UnicodeString; IndexB: SizeInt; ALen: SizeInt; IgnoreCase: Boolean): Integer; overload; static; // inline; //deprecated 'Use same with TCompareOptions';
    Class Function Compare(const A: UnicodeString; IndexA: SizeInt; const B: UnicodeString; IndexB: SizeInt; ALen: SizeInt; Options: TCompareOptions): Integer; overload; static;//  inline;
    Class Function CompareOrdinal(const A: UnicodeString; const B: UnicodeString): Integer; overload; static;
    Class Function CompareOrdinal(const A: UnicodeString; IndexA: SizeInt; const B: UnicodeString; IndexB: SizeInt; ALen: SizeInt): Integer; overload; static;
    Class Function CompareText(const A: UnicodeString; const B: UnicodeString): Integer; static; inline;
    Class Function Copy(const Str: UnicodeString): UnicodeString; inline; static;
    Class Function Create(AChar: UnicodeChar; ACount: SizeInt): UnicodeString; overload; inline; static;
    Class Function Create(const AValue: array of UnicodeChar): UnicodeString; overload; static;
    Class Function Create(const AValue: array of UnicodeChar; StartIndex: SizeInt; ALen: SizeInt): UnicodeString; overload; static;
    Class Function EndsText(const ASubText, AText: UnicodeString): Boolean; static;
    Class Function Equals(const a: UnicodeString; const b: UnicodeString): Boolean; overload; static;
    Class Function Format(const AFormat: UnicodeString; const args: array of const): UnicodeString; overload; static;
    Class Function IsNullOrEmpty(const AValue: UnicodeString): Boolean; static;
    Class Function IsNullOrWhiteSpace(const AValue: UnicodeString): Boolean; static;
    Class Function Join(const Separator: UnicodeString; const Values: array of const): UnicodeString; overload; static;
    Class Function Join(const Separator: UnicodeString; const Values: array of UnicodeString): UnicodeString; overload; static;
    Class Function Join(const Separator: UnicodeString; const Values: array of UnicodeString; StartIndex: SizeInt; ACount: SizeInt): UnicodeString; overload; static;
    Class Function LowerCase(const S: UnicodeString): UnicodeString; overload; static; inline;
    Class Function Parse(const AValue: Boolean): UnicodeString; overload; static; inline;
    Class Function Parse(const AValue: Extended): UnicodeString; overload; static;inline;
    Class Function Parse(const AValue: Int64): UnicodeString; overload; static; inline;
    Class Function Parse(const AValue: Integer): UnicodeString; overload; static; inline;
    Class Function ToBoolean(const S: UnicodeString): Boolean; overload; static; inline;
    Class Function ToDouble(const S: UnicodeString): Double; overload; static; inline;
    Class Function ToExtended(const S: UnicodeString): Extended; overload; static; inline;
    Class Function ToInt64(const S: UnicodeString): Int64; overload; static; inline;
    Class Function ToInteger(const S: UnicodeString): Integer; overload; static; inline;
    Class Function ToSingle(const S: UnicodeString): Single; overload; static; inline;
    Class Function UpperCase(const S: UnicodeString): UnicodeString; overload; static; inline;
    Function CompareTo(const B: UnicodeString): Integer;
    Function Contains(const AValue: UnicodeString; IgnoreCase: Boolean = False): Boolean;
    procedure CopyTo(SourceIndex: SizeInt; var destination: array of UnicodeChar; DestinationIndex: SizeInt; ACount: SizeInt);
    Function CountChar(const C: UnicodeChar): SizeInt;
    Function DeQuotedString: UnicodeString; overload;
    Function DeQuotedString(const AQuoteChar: UnicodeChar): UnicodeString; overload;
    Function EndsWith(const AValue: UnicodeString): Boolean; overload; inline;
    Function EndsWith(const AValue: UnicodeString; IgnoreCase: Boolean): Boolean; overload;
    Function Equals(const AValue: UnicodeString; IgnoreCase: Boolean = False): Boolean; overload;
    Function Format(const args: array of const): UnicodeString; overload;
    Function GetHashCode: Integer;
    Function IndexOf(AValue: UnicodeChar): SizeInt; overload; inline;
    Function IndexOf(const AValue: UnicodeString): SizeInt; overload; inline;
    Function IndexOf(AValue: UnicodeChar; StartIndex: SizeInt): SizeInt; overload;
    Function IndexOf(const AValue: UnicodeString; StartIndex: SizeInt): SizeInt; overload;
    Function IndexOf(AValue: UnicodeChar; StartIndex: SizeInt; ACount: SizeInt): SizeInt; overload;
    Function IndexOf(const AValue: UnicodeString; StartIndex: SizeInt; ACount: SizeInt): SizeInt; overload;
    Function IndexOfUnQuoted(const AValue: UnicodeString; StartQuote, EndQuote: UnicodeChar; StartIndex: SizeInt = 0): SizeInt; overload;
    Function IndexOfAny(const AnyOf: array of UnicodeChar): SizeInt; overload;
    Function IndexOfAny(const AnyOf: array of UnicodeChar; StartIndex: SizeInt): SizeInt; overload;
    Function IndexOfAny(const AnyOf: array of UnicodeChar; StartIndex: SizeInt; ACount: SizeInt): SizeInt; overload;
    Function IndexOfAny(const AnyOf: array of UnicodeString): SizeInt; overload;
    Function IndexOfAny(const AnyOf: array of UnicodeString; StartIndex: SizeInt): SizeInt; overload;
    Function IndexOfAny(const AnyOf: array of UnicodeString; StartIndex: SizeInt; ACount: SizeInt): SizeInt; overload;
    Function IndexOfAny(const AnyOf: array of UnicodeString; StartIndex: SizeInt; ACount: SizeInt; Out AMatch : SizeInt): SizeInt; overload;
    Function IndexOfAnyUnquoted(const AnyOf: array of UnicodeChar; StartQuote, EndQuote: UnicodeChar): SizeInt; overload;
    Function IndexOfAnyUnquoted(const AnyOf: array of UnicodeChar; StartQuote, EndQuote: UnicodeChar; StartIndex: SizeInt): SizeInt; overload;
    Function IndexOfAnyUnquoted(const AnyOf: array of UnicodeChar; StartQuote, EndQuote: UnicodeChar; StartIndex: SizeInt; ACount: SizeInt): SizeInt; overload;
    function IndexOfAnyUnquoted(const AnyOf: array of UnicodeString; StartQuote, EndQuote: UnicodeChar; StartIndex: SizeInt; Out Matched: SizeInt): SizeInt; overload;
    Function Insert(StartIndex: SizeInt; const AValue: UnicodeString): UnicodeString;
    Function IsDelimiter(const Delimiters: UnicodeString; Index: SizeInt): Boolean;
    Function IsEmpty: Boolean;
    Function LastDelimiter(const Delims: UnicodeString): SizeInt;
    Function LastIndexOf(AValue: UnicodeChar): SizeInt; overload;
    Function LastIndexOf(const AValue: UnicodeString): SizeInt; overload;
    Function LastIndexOf(AValue: UnicodeChar; AStartIndex: SizeInt): SizeInt; overload;
    Function LastIndexOf(const AValue: UnicodeString; AStartIndex: SizeInt): SizeInt; overload;
    Function LastIndexOf(AValue: UnicodeChar; AStartIndex: SizeInt; ACount: SizeInt): SizeInt; overload;
    Function LastIndexOf(const AValue: UnicodeString; AStartIndex: SizeInt; ACount: SizeInt): SizeInt; overload;
    Function LastIndexOfAny(const AnyOf: array of UnicodeChar): SizeInt; overload;
    Function LastIndexOfAny(const AnyOf: array of UnicodeChar; AStartIndex: SizeInt): SizeInt; overload;
    Function LastIndexOfAny(const AnyOf: array of UnicodeChar; AStartIndex: SizeInt; ACount: SizeInt): SizeInt; overload;
    Function PadLeft(ATotalWidth: SizeInt): UnicodeString; overload; inline;
    Function PadLeft(ATotalWidth: SizeInt; PaddingChar: UnicodeChar): UnicodeString; overload; inline;
    Function PadRight(ATotalWidth: SizeInt): UnicodeString; overload; inline;
    Function PadRight(ATotalWidth: SizeInt; PaddingChar: UnicodeChar): UnicodeString; overload; inline;
    Function QuotedString: UnicodeString; overload;
    Function QuotedString(const AQuoteChar: UnicodeChar): UnicodeString; overload;
    Function Remove(StartIndex: SizeInt): UnicodeString; overload; inline;
    Function Remove(StartIndex: SizeInt; ACount: SizeInt): UnicodeString; overload; inline;
    Function Replace(OldChar: UnicodeChar; NewChar: UnicodeChar): UnicodeString; overload;
    Function Replace(OldChar: UnicodeChar; NewChar: UnicodeChar; ReplaceFlags: TReplaceFlags): UnicodeString; overload;
    Function Replace(const OldValue: UnicodeString; const NewValue: UnicodeString): UnicodeString; overload;
    Function Replace(const OldValue: UnicodeString; const NewValue: UnicodeString; ReplaceFlags: TReplaceFlags): UnicodeString; overload;
    Function Split(const Separators: array of UnicodeChar): TUnicodeStringArray; overload;
    Function Split(const Separators: array of UnicodeChar; ACount: SizeInt): TUnicodeStringArray; overload;
    Function Split(const Separators: array of UnicodeChar; Options: TStringSplitOptions): TUnicodeStringArray; overload;
    Function Split(const Separators: array of UnicodeChar; ACount: SizeInt; Options: TStringSplitOptions): TUnicodeStringArray; overload;
    Function Split(const Separators: array of UnicodeString): TUnicodeStringArray; overload;
    Function Split(const Separators: array of UnicodeString; ACount: SizeInt): TUnicodeStringArray; overload;
    Function Split(const Separators: array of UnicodeString; Options: TStringSplitOptions): TUnicodeStringArray; overload;
    Function Split(const Separators: array of UnicodeString; ACount: SizeInt; Options: TStringSplitOptions): TUnicodeStringArray; overload;
    Function Split(const Separators: array of UnicodeChar; AQuote: UnicodeChar): TUnicodeStringArray; overload;
    Function Split(const Separators: array of UnicodeChar; AQuoteStart, AQuoteEnd: UnicodeChar): TUnicodeStringArray; overload;
    Function Split(const Separators: array of UnicodeChar; AQuoteStart, AQuoteEnd: UnicodeChar; Options: TStringSplitOptions): TUnicodeStringArray; overload;
    Function Split(const Separators: array of UnicodeChar; AQuoteStart, AQuoteEnd: UnicodeChar; ACount: SizeInt): TUnicodeStringArray; overload;
    Function Split(const Separators: array of UnicodeChar; AQuoteStart, AQuoteEnd: UnicodeChar; ACount: SizeInt; Options: TStringSplitOptions): TUnicodeStringArray; overload;
    Function Split(const Separators: array of UnicodeString; AQuote: UnicodeChar): TUnicodeStringArray; overload;
    Function Split(const Separators: array of UnicodeString; AQuoteStart, AQuoteEnd: UnicodeChar): TUnicodeStringArray; overload;
    Function Split(const Separators: array of UnicodeString; AQuoteStart, AQuoteEnd: UnicodeChar; Options: TStringSplitOptions): TUnicodeStringArray; overload;
    Function Split(const Separators: array of UnicodeString; AQuoteStart, AQuoteEnd: UnicodeChar; ACount: SizeInt): TUnicodeStringArray; overload;
    Function Split(const Separators: array of UnicodeString; AQuoteStart, AQuoteEnd: UnicodeChar; ACount: SizeInt; Options: TStringSplitOptions): TUnicodeStringArray; overload;
    Function StartsWith(const AValue: UnicodeString): Boolean; overload; inline;
    Function StartsWith(const AValue: UnicodeString; IgnoreCase: Boolean): Boolean; overload;
    Function Substring(AStartIndex: SizeInt): UnicodeString; overload;
    Function Substring(AStartIndex: SizeInt; ALen: SizeInt): UnicodeString; overload;
    Function ToBoolean: Boolean; overload; inline;
    Function ToInteger: Integer; overload; inline;
    Function ToInt64: Int64; overload; inline;
    Function ToSingle: Single; overload; inline;
    Function ToDouble: Double; overload; inline;
    Function ToExtended: Extended; overload; inline;
    Function ToCharArray: TCharArray; overload;
    Function ToCharArray(AStartIndex: SizeInt; ALen: SizeInt): TCharArray; overload;
    Function ToLower: UnicodeString; overload; inline;
    Function ToLowerInvariant: UnicodeString;
    Function ToUpper: UnicodeString; overload; inline;
    Function ToUpperInvariant: UnicodeString; inline;
    Function Trim: UnicodeString; overload;
    Function TrimLeft: UnicodeString; overload;
    Function TrimRight: UnicodeString; overload;
    Function Trim(const ATrimChars: array of UnicodeChar): UnicodeString; overload;
    Function TrimLeft(const ATrimChars: array of UnicodeChar): UnicodeString; overload;
    Function TrimRight(const ATrimChars: array of UnicodeChar): UnicodeString; overload;
    Function TrimEnd(const ATrimChars: array of UnicodeChar): UnicodeString; deprecated 'Use TrimRight';
    Function TrimStart(const ATrimChars: array of UnicodeChar): UnicodeString; deprecated 'Use TrimLeft';
    property Chars[AIndex: SizeInt]: UnicodeChar read GetChar;
    property Length: SizeInt read GetLength;
  end;

    { TShortStringHelper }

    TShortStringHelper = Type Helper for ShortString
    Private
      Function GetChar(AIndex : SizeInt) : AnsiChar;
      Function GetLength : SizeInt;
    public
      const Empty = '';
      // Methods
      Class Function Compare(const A: ShortString; const B: ShortString): Integer; overload; static; //inline;
      Class Function Compare(const A: ShortString; const B: ShortString; IgnoreCase: Boolean): Integer; overload; static; //inline; //deprecated 'Use same with TCompareOptions';
      Class Function Compare(const A: ShortString; const B: ShortString; Options: TCompareOptions): Integer; overload; static; // inline;
      Class Function Compare(const A: ShortString; IndexA: SizeInt; const B: ShortString; IndexB: SizeInt; ALen: SizeInt): Integer; overload; static; // inline;
      Class Function Compare(const A: ShortString; IndexA: SizeInt; const B: ShortString; IndexB: SizeInt; ALen: SizeInt; IgnoreCase: Boolean): Integer; overload; static; // inline; //deprecated 'Use same with TCompareOptions';
      Class Function Compare(const A: ShortString; IndexA: SizeInt; const B: ShortString; IndexB: SizeInt; ALen: SizeInt; Options: TCompareOptions): Integer; overload; static;//  inline;
      Class Function CompareOrdinal(const A: ShortString; const B: ShortString): Integer; overload; static;
      Class Function CompareOrdinal(const A: ShortString; IndexA: SizeInt; const B: ShortString; IndexB: SizeInt; ALen: SizeInt): Integer; overload; static;
      Class Function CompareText(const A: ShortString; const B: ShortString): Integer; static; inline;
      Class Function Copy(const Str: ShortString): ShortString; inline; static;
      Class Function Create(AChar: AnsiChar; ACount: SizeInt): ShortString; overload; inline; static;
      Class Function Create(const AValue: array of AnsiChar): ShortString; overload; static;
      Class Function Create(const AValue: array of AnsiChar; StartIndex: SizeInt; ALen: SizeInt): ShortString; overload; static;
      Class Function EndsText(const ASubText, AText: ShortString): Boolean; static;
      Class Function Equals(const a: ShortString; const b: ShortString): Boolean; overload; static;
      Class Function Format(const AFormat: ShortString; const args: array of const): ShortString; overload; static;
      Class Function IsNullOrEmpty(const AValue: ShortString): Boolean; static;
      Class Function IsNullOrWhiteSpace(const AValue: ShortString): Boolean; static;
      Class Function Join(const Separator: ShortString; const Values: array of const): ShortString; overload; static;
      Class Function Join(const Separator: ShortString; const Values: array of ShortString): ShortString; overload; static;
      Class Function Join(const Separator: ShortString; const Values: array of ShortString; StartIndex: SizeInt; ACount: SizeInt): ShortString; overload; static;
      Class Function LowerCase(const S: ShortString): ShortString; overload; static; inline;
      Class Function Parse(const AValue: Boolean): ShortString; overload; static; inline;
      Class Function Parse(const AValue: Extended): ShortString; overload; static;inline;
      Class Function Parse(const AValue: Int64): ShortString; overload; static; inline;
      Class Function Parse(const AValue: Integer): ShortString; overload; static; inline;
      Class Function ToBoolean(const S: ShortString): Boolean; overload; static; inline;
      Class Function ToDouble(const S: ShortString): Double; overload; static; inline;
      Class Function ToExtended(const S: ShortString): Extended; overload; static; inline;
      Class Function ToInt64(const S: ShortString): Int64; overload; static; inline;
      Class Function ToInteger(const S: ShortString): Integer; overload; static; inline;
      Class Function ToSingle(const S: ShortString): Single; overload; static; inline;
      Class Function UpperCase(const S: ShortString): ShortString; overload; static; inline;
      Function CompareTo(const B: ShortString): Integer;
      Function Contains(const AValue: ShortString; IgnoreCase: Boolean = False): Boolean;
      procedure CopyTo(SourceIndex: SizeInt; var destination: array of AnsiChar; DestinationIndex: SizeInt; ACount: SizeInt);
      Function CountChar(const C: AnsiChar): SizeInt;
      Function DeQuotedString: ShortString; overload;
      Function DeQuotedString(const AQuoteChar: AnsiChar): ShortString; overload;
      Function EndsWith(const AValue: ShortString): Boolean; overload; inline;
      Function EndsWith(const AValue: ShortString; IgnoreCase: Boolean): Boolean; overload;
      Function Equals(const AValue: ShortString; IgnoreCase: Boolean = False): Boolean; overload;
      Function Format(const args: array of const): ShortString; overload;
      Function GetHashCode: Integer;
      Function IndexOf(AValue: AnsiChar): SizeInt; overload; inline;
      Function IndexOf(const AValue: ShortString): SizeInt; overload; inline;
      Function IndexOf(AValue: AnsiChar; StartIndex: SizeInt): SizeInt; overload;
      Function IndexOf(const AValue: ShortString; StartIndex: SizeInt): SizeInt; overload;
      Function IndexOf(AValue: AnsiChar; StartIndex: SizeInt; ACount: SizeInt): SizeInt; overload;
      Function IndexOf(const AValue: ShortString; StartIndex: SizeInt; ACount: SizeInt): SizeInt; overload;
      Function IndexOfUnQuoted(const AValue: ShortString; StartQuote, EndQuote: AnsiChar; StartIndex: SizeInt = 0): SizeInt; overload;
      Function IndexOfAny(const AnyOf: array of AnsiChar): SizeInt; overload;
      Function IndexOfAny(const AnyOf: array of AnsiChar; StartIndex: SizeInt): SizeInt; overload;
      Function IndexOfAny(const AnyOf: array of AnsiChar; StartIndex: SizeInt; ACount: SizeInt): SizeInt; overload;
      Function IndexOfAny(const AnyOf: array of ShortString): SizeInt; overload;
      Function IndexOfAny(const AnyOf: array of ShortString; StartIndex: SizeInt): SizeInt; overload;
      Function IndexOfAny(const AnyOf: array of ShortString; StartIndex: SizeInt; ACount: SizeInt): SizeInt; overload;
      Function IndexOfAny(const AnyOf: array of ShortString; StartIndex: SizeInt; ACount: SizeInt; Out AMatch : SizeInt): SizeInt; overload;
      Function IndexOfAnyUnquoted(const AnyOf: array of AnsiChar; StartQuote, EndQuote: AnsiChar): SizeInt; overload;
      Function IndexOfAnyUnquoted(const AnyOf: array of AnsiChar; StartQuote, EndQuote: AnsiChar; StartIndex: SizeInt): SizeInt; overload;
      Function IndexOfAnyUnquoted(const AnyOf: array of AnsiChar; StartQuote, EndQuote: AnsiChar; StartIndex: SizeInt; ACount: SizeInt): SizeInt; overload;
      function IndexOfAnyUnquoted(const AnyOf: array of ShortString; StartQuote, EndQuote: AnsiChar; StartIndex: SizeInt; Out Matched: SizeInt): SizeInt; overload;
      Function Insert(StartIndex: SizeInt; const AValue: ShortString): ShortString;
      Function IsDelimiter(const Delimiters: ShortString; Index: SizeInt): Boolean;
      Function IsEmpty: Boolean;
      Function LastDelimiter(const Delims: ShortString): SizeInt;
      Function LastIndexOf(AValue: AnsiChar): SizeInt; overload;
      Function LastIndexOf(const AValue: ShortString): SizeInt; overload;
      Function LastIndexOf(AValue: AnsiChar; AStartIndex: SizeInt): SizeInt; overload;
      Function LastIndexOf(const AValue: ShortString; AStartIndex: SizeInt): SizeInt; overload;
      Function LastIndexOf(AValue: AnsiChar; AStartIndex: SizeInt; ACount: SizeInt): SizeInt; overload;
      Function LastIndexOf(const AValue: ShortString; AStartIndex: SizeInt; ACount: SizeInt): SizeInt; overload;
      Function LastIndexOfAny(const AnyOf: array of AnsiChar): SizeInt; overload;
      Function LastIndexOfAny(const AnyOf: array of AnsiChar; AStartIndex: SizeInt): SizeInt; overload;
      Function LastIndexOfAny(const AnyOf: array of AnsiChar; AStartIndex: SizeInt; ACount: SizeInt): SizeInt; overload;
      Function PadLeft(ATotalWidth: SizeInt): ShortString; overload; inline;
      Function PadLeft(ATotalWidth: SizeInt; PaddingChar: AnsiChar): ShortString; overload; inline;
      Function PadRight(ATotalWidth: SizeInt): ShortString; overload; inline;
      Function PadRight(ATotalWidth: SizeInt; PaddingChar: AnsiChar): ShortString; overload; inline;
      Function QuotedString: ShortString; overload;
      Function QuotedString(const AQuoteChar: AnsiChar): ShortString; overload;
      Function Remove(StartIndex: SizeInt): ShortString; overload; inline;
      Function Remove(StartIndex: SizeInt; ACount: SizeInt): ShortString; overload; inline;
      Function Replace(OldChar: AnsiChar; NewChar: AnsiChar): ShortString; overload;
      Function Replace(OldChar: AnsiChar; NewChar: AnsiChar; ReplaceFlags: TReplaceFlags): ShortString; overload;
      Function Replace(const OldValue: ShortString; const NewValue: ShortString): ShortString; overload;
      Function Replace(const OldValue: ShortString; const NewValue: ShortString; ReplaceFlags: TReplaceFlags): ShortString; overload;
      Function Split(const Separators: array of AnsiChar): TShortStringArray; overload;
      Function Split(const Separators: array of AnsiChar; ACount: SizeInt): TShortStringArray; overload;
      Function Split(const Separators: array of AnsiChar; Options: TStringSplitOptions): TShortStringArray; overload;
      Function Split(const Separators: array of AnsiChar; ACount: SizeInt; Options: TStringSplitOptions): TShortStringArray; overload;
      Function Split(const Separators: array of ShortString): TShortStringArray; overload;
      Function Split(const Separators: array of ShortString; ACount: SizeInt): TShortStringArray; overload;
      Function Split(const Separators: array of ShortString; Options: TStringSplitOptions): TShortStringArray; overload;
      Function Split(const Separators: array of ShortString; ACount: SizeInt; Options: TStringSplitOptions): TShortStringArray; overload;
      Function Split(const Separators: array of AnsiChar; AQuote: AnsiChar): TShortStringArray; overload;
      Function Split(const Separators: array of AnsiChar; AQuoteStart, AQuoteEnd: AnsiChar): TShortStringArray; overload;
      Function Split(const Separators: array of AnsiChar; AQuoteStart, AQuoteEnd: AnsiChar; Options: TStringSplitOptions): TShortStringArray; overload;
      Function Split(const Separators: array of AnsiChar; AQuoteStart, AQuoteEnd: AnsiChar; ACount: SizeInt): TShortStringArray; overload;
      Function Split(const Separators: array of AnsiChar; AQuoteStart, AQuoteEnd: AnsiChar; ACount: SizeInt; Options: TStringSplitOptions): TShortStringArray; overload;
      Function Split(const Separators: array of ShortString; AQuote: AnsiChar): TShortStringArray; overload;
      Function Split(const Separators: array of ShortString; AQuoteStart, AQuoteEnd: AnsiChar): TShortStringArray; overload;
      Function Split(const Separators: array of ShortString; AQuoteStart, AQuoteEnd: AnsiChar; Options: TStringSplitOptions): TShortStringArray; overload;
      Function Split(const Separators: array of ShortString; AQuoteStart, AQuoteEnd: AnsiChar; ACount: SizeInt): TShortStringArray; overload;
      Function Split(const Separators: array of ShortString; AQuoteStart, AQuoteEnd: AnsiChar; ACount: SizeInt; Options: TStringSplitOptions): TShortStringArray; overload;
      Function StartsWith(const AValue: ShortString): Boolean; overload; inline;
      Function StartsWith(const AValue: ShortString; IgnoreCase: Boolean): Boolean; overload;
      Function Substring(AStartIndex: SizeInt): ShortString; overload;
      Function Substring(AStartIndex: SizeInt; ALen: SizeInt): ShortString; overload;
      Function ToBoolean: Boolean; overload; inline;
      Function ToInteger: Integer; overload; inline;
      Function ToInt64: Int64; overload; inline;
      Function ToSingle: Single; overload; inline;
      Function ToDouble: Double; overload; inline;
      Function ToExtended: Extended; overload; inline;
      Function ToCharArray: TCharArray; overload;
      Function ToCharArray(AStartIndex: SizeInt; ALen: SizeInt): TCharArray; overload;
      Function ToLower: ShortString; overload; inline;
      Function ToLowerInvariant: ShortString;
      Function ToUpper: ShortString; overload; inline;
      Function ToUpperInvariant: ShortString; inline;
      Function Trim: ShortString; overload;
      Function TrimLeft: ShortString; overload;
      Function TrimRight: ShortString; overload;
      Function Trim(const ATrimChars: array of AnsiChar): ShortString; overload;
      Function TrimLeft(const ATrimChars: array of AnsiChar): ShortString; overload;
      Function TrimRight(const ATrimChars: array of AnsiChar): ShortString; overload;
      Function TrimEnd(const ATrimChars: array of AnsiChar): ShortString; deprecated 'Use TrimRight';
      Function TrimStart(const ATrimChars: array of AnsiChar): ShortString; deprecated 'Use TrimLeft';
      property Chars[AIndex: SizeInt]: AnsiChar read GetChar;
      property Length: SizeInt read GetLength;
    end;

{$IF SIZEOF(CHAR)=2}
  TStringHelper = Type helper(TUnicodeStringHelper) for UnicodeString
  end;
{$ELSE}
  TStringHelper = Type helper(TAnsiStringHelper) for ansistring
  end;  
{$ENDIF}

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
