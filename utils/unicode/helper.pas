{   Unicode parser helper unit.

    Copyright (c) 2012 by Inoussa OUEDRAOGO

    The source code is distributed under the Library GNU
    General Public License with the following modification:

        - object files and libraries linked into an application may be
          distributed without source code.

    If you didn't receive a copy of the file COPYING, contact:
          Free Software Foundation
          675 Mass Ave
          Cambridge, MA  02139
          USA

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. }
unit helper;

{$mode delphi}
{$H+}
{$PACKENUM 1}
{$pointermath on}
{$typedaddress on}
{$warn 4056 off}  //Conversion between ordinals and pointers is not portable

{$macro on}
{$ifdef FPC_REQUIRES_PROPER_ALIGNMENT}
  {$define X_PACKED:=}
{$else FPC_REQUIRES_PROPER_ALIGNMENT}
  {$define X_PACKED:=packed}
{$endif FPC_REQUIRES_PROPER_ALIGNMENT}

interface

uses
  Classes, SysUtils, StrUtils;

const
  SLicenseText =
    '    {   Unicode implementation tables. ' + sLineBreak +
    ' ' + sLineBreak +
    '        Copyright (c) 2013 by Inoussa OUEDRAOGO ' + sLineBreak +
    ' ' + sLineBreak +
    '        Permission is hereby granted, free of charge, to any person ' + sLineBreak +
    '        obtaining a copy of the Unicode data files and any associated ' + sLineBreak +
    '        documentation (the "Data Files") or Unicode software and any ' + sLineBreak +
    '        associated documentation (the "Software") to deal in the Data ' + sLineBreak +
    '        Files or Software without restriction, including without ' + sLineBreak +
    '        limitation the rights to use, copy, modify, merge, publish, ' + sLineBreak +
    '        distribute, and/or sell copies of the Data Files or Software, ' + sLineBreak +
    '        and to permit persons to whom the Data Files or Software are ' + sLineBreak +
    '        furnished to do so, provided that (a) the above copyright ' + sLineBreak +
    '        notice(s) and this permission notice appear with all copies ' + sLineBreak +
    '        of the Data Files or Software, (b) both the above copyright ' + sLineBreak +
    '        notice(s) and this permission notice appear in associated ' + sLineBreak +
    '        documentation, and (c) there is clear notice in each modified ' + sLineBreak +
    '        Data File or in the Software as well as in the documentation ' + sLineBreak +
    '        associated with the Data File(s) or Software that the data or ' + sLineBreak +
    '        software has been modified. ' + sLineBreak +
    ' ' + sLineBreak +
    ' ' + sLineBreak +
    '        This program is distributed in the hope that it will be useful, ' + sLineBreak +
    '        but WITHOUT ANY WARRANTY; without even the implied warranty of ' + sLineBreak +
    '        MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. }';


type
  // Unicode General Category
  TUnicodeCategory = (
    ucUppercaseLetter,             // Lu = Letter, uppercase
    ucLowercaseLetter,             //  Ll = Letter, lowercase
    ucTitlecaseLetter,             //  Lt = Letter, titlecase
    ucModifierLetter,              //  Lm = Letter, modifier
    ucOtherLetter,                 //  Lo = Letter, other

    ucNonSpacingMark,              //  Mn = Mark, nonspacing
    ucCombiningMark,               //  Mc = Mark, spacing combining
    ucEnclosingMark,               //  Me = Mark, enclosing

    ucDecimalNumber,               //  Nd = Number, decimal digit
    ucLetterNumber,                //  Nl = Number, letter
    ucOtherNumber,                 //  No = Number, other

    ucConnectPunctuation,          //  Pc = Punctuation, connector
    ucDashPunctuation,             //  Pd = Punctuation, dash
    ucOpenPunctuation,             //  Ps = Punctuation, open
    ucClosePunctuation,            //  Pe = Punctuation, close
    ucInitialPunctuation,          //  Pi = Punctuation, initial quote (may behave like Ps or Pe depending on usage)
    ucFinalPunctuation,            //  Pf = Punctuation, final quote (may behave like Ps or Pe depending on usage)
    ucOtherPunctuation,            //  Po = Punctuation, other

    ucMathSymbol,                  //  Sm = Symbol, math
    ucCurrencySymbol,              //  Sc = Symbol, currency
    ucModifierSymbol,              //  Sk = Symbol, modifier
    ucOtherSymbol,                 //  So = Symbol, other

    ucSpaceSeparator,              //  Zs = Separator, space
    ucLineSeparator,               //  Zl = Separator, line
    ucParagraphSeparator,          //  Zp = Separator, paragraph

    ucControl,                     //  Cc = Other, control
    ucFormat,                      //  Cf = Other, format
    ucSurrogate,                   //  Cs = Other, surrogate
    ucPrivateUse,                  //  Co = Other, private use
    ucUnassigned                   //  Cn = Other, not assigned (including noncharacters)
  );


  TUInt24Rec = packed record
  public
  {$ifdef FPC_LITTLE_ENDIAN}
    byte0, byte1, byte2 : Byte;
  {$else FPC_LITTLE_ENDIAN}
    byte2, byte1, byte0 : Byte;
  {$endif FPC_LITTLE_ENDIAN}
  public
    class operator Implicit(a : TUInt24Rec) : Cardinal;{$ifdef USE_INLINE}inline;{$ENDIF}
    class operator Implicit(a : TUInt24Rec) : LongInt;{$ifdef USE_INLINE}inline;{$ENDIF}
    class operator Implicit(a : TUInt24Rec) : Word;{$ifdef USE_INLINE}inline;{$ENDIF}
    class operator Implicit(a : TUInt24Rec) : Byte;{$ifdef USE_INLINE}inline;{$ENDIF}
    class operator Implicit(a : Cardinal) : TUInt24Rec;{$ifdef USE_INLINE}inline;{$ENDIF}

    class operator Explicit(a : TUInt24Rec) : Cardinal;{$ifdef USE_INLINE}inline;{$ENDIF}

    class operator Equal(a, b: TUInt24Rec): Boolean;{$ifdef USE_INLINE}inline;{$ENDIF}

    class operator Equal(a : TUInt24Rec; b : Cardinal): Boolean;{$ifdef USE_INLINE}inline;{$ENDIF}
    class operator Equal(a : Cardinal; b : TUInt24Rec): Boolean;{$ifdef USE_INLINE}inline;{$ENDIF}

    class operator Equal(a : TUInt24Rec; b : LongInt): Boolean;{$ifdef USE_INLINE}inline;{$ENDIF}
    class operator Equal(a : LongInt; b : TUInt24Rec): Boolean;{$ifdef USE_INLINE}inline;{$ENDIF}

    class operator Equal(a : TUInt24Rec; b : Word): Boolean;{$ifdef USE_INLINE}inline;{$ENDIF}
    class operator Equal(a : Word; b : TUInt24Rec): Boolean;{$ifdef USE_INLINE}inline;{$ENDIF}

    class operator Equal(a : TUInt24Rec; b : Byte): Boolean;{$ifdef USE_INLINE}inline;{$ENDIF}
    class operator Equal(a : Byte; b : TUInt24Rec): Boolean;{$ifdef USE_INLINE}inline;{$ENDIF}

    class operator NotEqual(a, b: TUInt24Rec): Boolean;{$ifdef USE_INLINE}inline;{$ENDIF}
    class operator NotEqual(a : TUInt24Rec; b : Cardinal): Boolean;{$ifdef USE_INLINE}inline;{$ENDIF}
    class operator NotEqual(a : Cardinal; b : TUInt24Rec): Boolean;{$ifdef USE_INLINE}inline;{$ENDIF}
    class operator GreaterThan(a, b: TUInt24Rec): Boolean;{$ifdef USE_INLINE}inline;{$ENDIF}
    class operator GreaterThan(a : TUInt24Rec; b : Cardinal): Boolean;{$ifdef USE_INLINE}inline;{$ENDIF}
    class operator GreaterThan(a : Cardinal; b : TUInt24Rec): Boolean;{$ifdef USE_INLINE}inline;{$ENDIF}
    class operator GreaterThanOrEqual(a, b: TUInt24Rec): Boolean;{$ifdef USE_INLINE}inline;{$ENDIF}
    class operator GreaterThanOrEqual(a : TUInt24Rec; b : Cardinal): Boolean;{$ifdef USE_INLINE}inline;{$ENDIF}
    class operator GreaterThanOrEqual(a : Cardinal; b : TUInt24Rec): Boolean;{$ifdef USE_INLINE}inline;{$ENDIF}
    class operator LessThan(a, b: TUInt24Rec): Boolean;{$ifdef USE_INLINE}inline;{$ENDIF}
    class operator LessThan(a : TUInt24Rec; b : Cardinal): Boolean;{$ifdef USE_INLINE}inline;{$ENDIF}
    class operator LessThan(a : Cardinal; b : TUInt24Rec): Boolean;{$ifdef USE_INLINE}inline;{$ENDIF}
    class operator LessThanOrEqual(a, b: TUInt24Rec): Boolean;{$ifdef USE_INLINE}inline;{$ENDIF}
    class operator LessThanOrEqual(a : TUInt24Rec; b : Cardinal): Boolean;{$ifdef USE_INLINE}inline;{$ENDIF}
    class operator LessThanOrEqual(a : Cardinal; b : TUInt24Rec): Boolean;{$ifdef USE_INLINE}inline;{$ENDIF}
  end;

  UInt24 = TUInt24Rec;
  PUInt24 = ^UInt24;
  TUnicodeCodePoint = Cardinal;
  TUnicodeCodePointArray = array of TUnicodeCodePoint;
  TDecompositionArray = array of TUnicodeCodePointArray;
  TNumericValue = Double;
  TNumericValueArray = array of TNumericValue;

  TBlockItemRec = packed record
    RangeStart    : TUnicodeCodePoint;
    RangeEnd      : TUnicodeCodePoint;
    Name          : string[120];
    CanonicalName : string[120];
  end;
  TBlocks = array of TBlockItemRec;

  PPropRec = ^TPropRec;

  { TPropRec }

  TPropRec = packed record
  private
    function GetCategory : TUnicodeCategory;inline;
    procedure SetCategory(AValue : TUnicodeCategory);
    function GetWhiteSpace : Boolean;inline;
    procedure SetWhiteSpace(AValue : Boolean);
    function GetHangulSyllable : Boolean;inline;
    procedure SetHangulSyllable(AValue : Boolean);
  public
    CategoryData    : Byte;

    PropID          : Word;
    CCC             : Byte; // Canonical Combining Class
    NumericIndex    : Byte;
    SimpleUpperCase : UInt24;
    SimpleLowerCase : UInt24;
    DecompositionID : SmallInt;
  public
    property Category : TUnicodeCategory read GetCategory write SetCategory;
    property WhiteSpace : Boolean read GetWhiteSpace write SetWhiteSpace;
    property HangulSyllable : Boolean read GetHangulSyllable write SetHangulSyllable;
  end;
  TPropRecArray = array of TPropRec;

  TDecompositionIndexRec = packed record
    StartPosition : Word;
    Length        : Byte;
  end;
  TDecompositionBook = X_PACKED record
    Index      : array of TDecompositionIndexRec;
    CodePoints : array of TUnicodeCodePoint;
  end;

  PDataLineRec = ^TDataLineRec;
  TDataLineRec = record
    PropID    : Integer;
    case LineType : Byte of
      0 : (CodePoint : TUnicodeCodePoint);
      1 : (StartCodePoint, EndCodePoint : TUnicodeCodePoint);
  end;
  TDataLineRecArray = array of TDataLineRec;

  TCodePointRec = record
    case LineType : Byte of
      0 : (CodePoint : TUnicodeCodePoint);
      1 : (StartCodePoint, EndCodePoint : TUnicodeCodePoint);
  end;
  TCodePointRecArray = array of TCodePointRec;

  TPropListLineRec = packed record
    CodePoint : TCodePointRec;
    PropName  : string[123];
  end;
  TPropListLineRecArray = array of TPropListLineRec;

  TUCA_WeightRec = packed record
    Weights  : array[0..3] of Cardinal;
    Variable : Boolean;
  end;
  TUCA_WeightRecArray = array of TUCA_WeightRec;

  TUCA_LineContextItemRec = X_PACKED record
  public
    CodePoints : TUnicodeCodePointArray;
    Weights    : TUCA_WeightRecArray;
  public
    procedure Clear();
    procedure Assign(ASource : TUCA_LineContextItemRec);
    function Clone() : TUCA_LineContextItemRec;
  end;
  PUCA_LineContextItemRec = ^TUCA_LineContextItemRec;

  TUCA_LineContextRec = X_PACKED record
  public
    Data : array of TUCA_LineContextItemRec;
  public
    procedure Clear();
    procedure Assign(ASource : TUCA_LineContextRec);
    function Clone() : TUCA_LineContextRec;
  end;
  PUCA_LineContextRec = ^TUCA_LineContextRec;

  { TUCA_LineRec }

  TUCA_LineRec = X_PACKED record
  public
    CodePoints : TUnicodeCodePointArray;
    Weights    : TUCA_WeightRecArray;
    Context    : TUCA_LineContextRec;
    //Variable   : Boolean;
    Deleted    : Boolean;
    Stored     : Boolean;
  public
    procedure Clear();
    procedure Assign(ASource : TUCA_LineRec);
    function Clone() : TUCA_LineRec;
    function HasContext() : Boolean;
  end;
  PUCA_LineRec = ^TUCA_LineRec;
  TUCA_VariableKind = (
    ucaShifted, ucaNonIgnorable, ucaBlanked, ucaShiftedTrimmed,
    ucaIgnoreSP
  );
  TUCA_DataBook = X_PACKED record
    Version        : string;
    VariableWeight : TUCA_VariableKind;
    Backwards      : array[0..3] of Boolean;
    Lines          : array of TUCA_LineRec;
  end;
  PUCA_DataBook = ^TUCA_DataBook;
  TUCA_DataBookIndex = array of Integer;

type
  TUCA_PropWeights = packed record
    Weights  : array[0..2] of Word;
    //Variable : Byte;
  end;
  PUCA_PropWeights = ^TUCA_PropWeights;

  TUCA_PropItemContextRec = packed record
    CodePointCount : Byte;
    WeightCount    : Byte;
    //CodePoints     : UInt24;
    //Weights        : TUCA_PropWeights;
  end;
  PUCA_PropItemContextRec = ^TUCA_PropItemContextRec;
  TUCA_PropItemContextTreeNodeRec = packed record
    Left    : Word;
    Right   : Word;
    Data    : TUCA_PropItemContextRec;
  end;
  PUCA_PropItemContextTreeNodeRec = ^TUCA_PropItemContextTreeNodeRec;

  TUCA_PropItemContextTreeRec = packed record
  public
    Size : UInt24;
  public
    function GetData:PUCA_PropItemContextTreeNodeRec;inline;
    property Data : PUCA_PropItemContextTreeNodeRec read GetData;
  end;
  PUCA_PropItemContextTreeRec = ^TUCA_PropItemContextTreeRec;

  { TUCA_PropItemRec }

  TUCA_PropItemRec = packed record
  private
    const FLAG_VALID      = 0;
    const FLAG_CODEPOINT  = 1;
    const FLAG_CONTEXTUAL = 2;
    const FLAG_DELETION   = 3;
    const FLAG_COMPRESS_WEIGHT_1 = 6;
    const FLAG_COMPRESS_WEIGHT_2 = 7;
  private
    function GetWeightSize : Word;inline;
  public
    WeightLength : Byte;
    ChildCount   : Byte;
    Size         : Word;
    Flags        : Byte;
  public
    function HasCodePoint() : Boolean;inline;
    function GetCodePoint() : UInt24;//inline;
    property CodePoint : UInt24 read GetCodePoint;
    //Weights    : array[0..WeightLength] of TUCA_PropWeights;
    procedure GetWeightArray(ADest : PUCA_PropWeights);
    function GetSelfOnlySize() : Cardinal;inline;

    procedure SetContextual(AValue : Boolean);inline;
    function GetContextual() : Boolean;inline;
    property Contextual : Boolean read GetContextual write setContextual;
    function GetContext() : PUCA_PropItemContextTreeRec;
    procedure SetDeleted(AValue : Boolean);inline;
    function IsDeleted() : Boolean;inline;
    function IsValid() : Boolean;inline;
    function IsWeightCompress_1() : Boolean;inline;
    function IsWeightCompress_2() : Boolean;inline;
  end;
  PUCA_PropItemRec = ^TUCA_PropItemRec;
  TUCA_PropIndexItem = packed record
    CodePoint : Cardinal;
    Position  : Integer;
  end;
  PUCA_PropIndexItem = ^TUCA_PropIndexItem;
  TUCA_PropBook = X_PACKED record
    ItemSize      : Integer;
    Index         : array of TUCA_PropIndexItem;
    Items         : PUCA_PropItemRec; //Native Endian
    ItemsOtherEndian  : PUCA_PropItemRec;//Non Native Endian
    VariableLowLimit  : Word;
    VariableHighLimit : Word;
  end;
  PUCA_PropBook = ^TUCA_PropBook;

  TBmpFirstTable = array[0..255] of Byte;
  TBmpSecondTableItem = array[0..255] of Word;
  TBmpSecondTable = array of TBmpSecondTableItem;

  T3lvlBmp1Table = array[0..255] of Byte;
  T3lvlBmp2TableItem = array[0..15] of Word;
  T3lvlBmp2Table = array of T3lvlBmp2TableItem;
  T3lvlBmp3TableItem = array[0..15] of Word;
  T3lvlBmp3Table = array of T3lvlBmp3TableItem;

  TucaBmpFirstTable = array[0..255] of Byte;
  TucaBmpSecondTableItem = array[0..255] of Cardinal;
  TucaBmpSecondTable = array of TucaBmpSecondTableItem;
  PucaBmpFirstTable = ^TucaBmpFirstTable;
  PucaBmpSecondTable = ^TucaBmpSecondTable;

const
  LOW_SURROGATE_BEGIN  = Word($DC00);
  LOW_SURROGATE_END    = Word($DFFF);
  LOW_SURROGATE_COUNT  = LOW_SURROGATE_END - LOW_SURROGATE_BEGIN + 1;

  HIGH_SURROGATE_BEGIN = Word($D800);
  HIGH_SURROGATE_END   = Word($DBFF);
  HIGH_SURROGATE_COUNT = HIGH_SURROGATE_END - HIGH_SURROGATE_BEGIN + 1;
type
  TOBmpFirstTable = array[0..(HIGH_SURROGATE_COUNT-1)] of Word;
  TOBmpSecondTableItem = array[0..(LOW_SURROGATE_COUNT-1)] of Word;
  TOBmpSecondTable = array of TOBmpSecondTableItem;

  T3lvlOBmp1Table = array[0..1023] of Byte;
  T3lvlOBmp2TableItem = array[0..31] of Word;
  T3lvlOBmp2Table = array of T3lvlOBmp2TableItem;
  T3lvlOBmp3TableItem = array[0..31] of Word;
  T3lvlOBmp3Table = array of T3lvlOBmp3TableItem;

  TucaOBmpFirstTable = array[0..(HIGH_SURROGATE_COUNT-1)] of Word;
  TucaOBmpSecondTableItem = array[0..(LOW_SURROGATE_COUNT-1)] of Cardinal;
  TucaOBmpSecondTable = array of TucaOBmpSecondTableItem;
  PucaOBmpFirstTable = ^TucaOBmpFirstTable;
  PucaOBmpSecondTable = ^TucaOBmpSecondTable;

type
  TEndianKind = (ekLittle, ekBig);
const
  ENDIAN_SUFFIX : array[TEndianKind] of string[2] = ('le','be');
{$IFDEF ENDIAN_LITTLE}
  ENDIAN_NATIVE     = ekLittle;
  ENDIAN_NON_NATIVE = ekBig;
{$ENDIF ENDIAN_LITTLE}
{$IFDEF ENDIAN_BIG}
  ENDIAN_NATIVE = ekBig;
  ENDIAN_NON_NATIVE = ekLittle;
{$ENDIF ENDIAN_BIG}

  procedure GenerateLicenceText(ADest : TStream);

  function BoolToByte(AValue : Boolean): Byte;inline;

  function IsHangulSyllable(
    const ACodePoint  : TUnicodeCodePoint;
    const AHangulList : TCodePointRecArray
  ) : Boolean;
  procedure ParseHangulSyllableTypes(
        ADataAStream   : TMemoryStream;
    var ACodePointList : TCodePointRecArray
  );

  procedure ParseProps(
        ADataAStream   : TMemoryStream;
    var APropList      : TPropListLineRecArray
  );
  function FindCodePointsByProperty(
    const APropName : string;
    const APropList : TPropListLineRecArray
  ) : TCodePointRecArray;

  procedure ParseBlokcs(
        ADataAStream   : TMemoryStream;
    var ABlocks        : TBlocks
  );
  procedure ParseUCAFile(
        ADataAStream : TMemoryStream;
    var ABook        : TUCA_DataBook
  );
  procedure MakeUCA_Props(
          ABook         : PUCA_DataBook;
    out   AProps        : PUCA_PropBook
  );
  procedure FreeUcaBook(var ABook : PUCA_PropBook);
  procedure MakeUCA_BmpTables(
    var   AFirstTable   : TucaBmpFirstTable;
    var   ASecondTable  : TucaBmpSecondTable;
    const APropBook     : PUCA_PropBook
  );
  procedure MakeUCA_OBmpTables(
    var   AFirstTable   : TucaOBmpFirstTable;
    var   ASecondTable  : TucaOBmpSecondTable;
    const APropBook     : PUCA_PropBook
  );
  function GetPropPosition(
    const AHighS,
          ALowS         : Word;
    const AFirstTable   : PucaOBmpFirstTable;
    const ASecondTable  : PucaOBmpSecondTable
  ): Integer;inline;overload;

  procedure GenerateUCA_Head(
    ADest  : TStream;
    ABook  : PUCA_DataBook;
    AProps : PUCA_PropBook
  );
  procedure GenerateUCA_BmpTables(
          AStream,
          ANativeEndianStream,
          ANonNativeEndianStream : TStream;
    var   AFirstTable            : TucaBmpFirstTable;
    var   ASecondTable           : TucaBmpSecondTable
  );
  procedure GenerateBinaryUCA_BmpTables(
          ANativeEndianStream,
          ANonNativeEndianStream : TStream;
    var   AFirstTable            : TucaBmpFirstTable;
    var   ASecondTable           : TucaBmpSecondTable
  );
  procedure GenerateUCA_PropTable(
          ADest     : TStream;
    const APropBook : PUCA_PropBook;
    const AEndian   : TEndianKind
  );
  procedure GenerateBinaryUCA_PropTable(
  // WARNING : files must be generated for each endianess (Little / Big)
          ANativeEndianStream,
          ANonNativeEndianStream : TStream;
    const APropBook              : PUCA_PropBook
  );
  procedure GenerateUCA_OBmpTables(
          AStream,
          ANativeEndianStream,
          ANonNativeEndianStream : TStream;
    var   AFirstTable            : TucaOBmpFirstTable;
    var   ASecondTable           : TucaOBmpSecondTable
  );
  procedure GenerateBinaryUCA_OBmpTables(
          ANativeEndianStream,
          ANonNativeEndianStream : TStream;
    var   AFirstTable            : TucaOBmpFirstTable;
    var   ASecondTable           : TucaOBmpSecondTable
  );

  procedure Parse_UnicodeData(
          ADataAStream   : TMemoryStream;
    var   APropList      : TPropRecArray;
    var   ANumericTable  : TNumericValueArray;
    var   ADataLineList  : TDataLineRecArray;
    var   ADecomposition : TDecompositionArray;
    const AHangulList    : TCodePointRecArray;
    const AWhiteSpaces   : TCodePointRecArray
  );
  procedure MakeDecomposition(
    const ARawData : TDecompositionArray;
    var   ABook    : TDecompositionBook
  );

  procedure MakeBmpTables(
    var   AFirstTable   : TBmpFirstTable;
    var   ASecondTable  : TBmpSecondTable;
    const ADataLineList : TDataLineRecArray
  );
  procedure MakeBmpTables3Levels(
    var   AFirstTable   : T3lvlBmp1Table;
    var   ASecondTable  : T3lvlBmp2Table;
    var   AThirdTable  : T3lvlBmp3Table;
    const ADataLineList : TDataLineRecArray
  );
  procedure GenerateBmpTables(
          ADest : TStream;
    var   AFirstTable   : TBmpFirstTable;
    var   ASecondTable  : TBmpSecondTable
  );
  procedure Generate3lvlBmpTables(
          ADest : TStream;
    var   AFirstTable   : T3lvlBmp1Table;
    var   ASecondTable  : T3lvlBmp2Table;
    var   AThirdTable   : T3lvlBmp3Table
  );
  procedure GeneratePropTable(
          ADest     : TStream;
    const APropList : TPropRecArray;
    const AEndian   : TEndianKind
  );
  procedure GenerateNumericTable(
          ADest         : TStream;
    const ANumList      : TNumericValueArray;
    const ACompleteUnit : Boolean
  );
  procedure GenerateDecompositionBookTable(
          ADest   : TStream;
    const ABook   : TDecompositionBook;
    const AEndian : TEndianKind
  );
  procedure GenerateOutBmpTable(
          ADest     : TStream;
    const AList : TDataLineRecArray
  );

  function Compress(const AData : TDataLineRecArray) : TDataLineRecArray;

  function EvaluateFloat(const AStr : string) : Double;
  function StrToCategory(const AStr : string) : TUnicodeCategory;
  function StringToCodePoint(ACP : string) : TUnicodeCodePoint;
  function IsWhiteSpace(
    const ACodePoint   : TUnicodeCodePoint;
    const AWhiteSpaces : TCodePointRecArray
  ) : Boolean;

  function GetPropID(
          ACodePoint    : TUnicodeCodePoint;
    const ADataLineList : TDataLineRecArray
  ) : Cardinal;

//--------------------
  procedure MakeOBmpTables(
    var   AFirstTable   : TOBmpFirstTable;
    var   ASecondTable  : TOBmpSecondTable;
    const ADataLineList : TDataLineRecArray
  );
  procedure MakeOBmpTables3Levels(
    var   AFirstTable   : T3lvlOBmp1Table;
    var   ASecondTable  : T3lvlOBmp2Table;
    var   AThirdTable  : T3lvlOBmp3Table;
    const ADataLineList : TDataLineRecArray
  );
  procedure GenerateOBmpTables(
          ADest : TStream;
    var   AFirstTable   : TOBmpFirstTable;
    var   ASecondTable  : TOBmpSecondTable
  );
  procedure Generate3lvlOBmpTables(
          ADest : TStream;
    var   AFirstTable   : T3lvlOBmp1Table;
    var   ASecondTable  : T3lvlOBmp2Table;
    var   AThirdTable   : T3lvlOBmp3Table
  );
  function GetProp(
    const AHighS,
          ALowS         : Word;
    const AProps        : TPropRecArray;
    var   AFirstTable   : TOBmpFirstTable;
    var   ASecondTable  : TOBmpSecondTable
  ): PPropRec; inline;overload;
  function GetProp(
    const AHighS,
          ALowS         : Word;
    const AProps        : TPropRecArray;
    var   AFirstTable   : T3lvlOBmp1Table;
    var   ASecondTable  : T3lvlOBmp2Table;
    var   AThirdTable   : T3lvlOBmp3Table
  ): PPropRec; inline;overload;
  procedure FromUCS4(const AValue : TUnicodeCodePoint; var AHighS, ALowS : Word);inline;
  function ToUCS4(const AHighS, ALowS : Word) : TUnicodeCodePoint; inline;

type
  TBitOrder = 0..7;

  function IsBitON(const AData : Byte; const ABit : TBitOrder) : Boolean ;{$IFDEF USE_INLINE}inline;{$ENDIF}
  procedure SetBit(var AData : Byte; const ABit : TBitOrder; const AValue : Boolean);{$IFDEF USE_INLINE}inline;{$ENDIF}

  function GenerateEndianIncludeFileName(
    const AStoreName : string;
    const AEndian    : TEndianKind
  ): string;inline;

  procedure ReverseFromNativeEndian(
    const AData    : PUCA_PropItemRec;
    const ADataLen : Cardinal;
    const ADest    : PUCA_PropItemRec
  );
  procedure ReverseToNativeEndian(
    const AData    : PUCA_PropItemRec;
    const ADataLen : Cardinal;
    const ADest    : PUCA_PropItemRec
  );
  procedure CompareProps(
    const AProp1,
          AProp2   : PUCA_PropItemRec;
    const ADataLen : Integer
  );

type
  TCollationName = string[128];
  TSerializedCollationHeader = packed record
    Base               : TCollationName;
    Version            : TCollationName;
    CollationName      : TCollationName;
    VariableWeight     : Byte;
    Backwards          : Byte;
    BMP_Table1Length   : DWord;
    BMP_Table2Length   : DWord;
    OBMP_Table1Length  : DWord;
    OBMP_Table2Length  : DWord;
    PropCount          : DWord;
    VariableLowLimit   : Word;
    VariableHighLimit  : Word;
    ChangedFields      : Byte;
  end;
  PSerializedCollationHeader = ^TSerializedCollationHeader;

  procedure ReverseRecordBytes(var AItem : TSerializedCollationHeader);
  procedure ReverseBytes(var AData; const ALength : Integer);
  procedure ReverseArray(var AValue; const AArrayLength, AItemSize : PtrInt);

resourcestring
  SInsufficientMemoryBuffer = 'Insufficient Memory Buffer';

implementation
uses
  typinfo, Math, AVL_Tree,
  trie;


type

  TCardinalRec = packed record
  {$ifdef FPC_LITTLE_ENDIAN}
    byte0, byte1, byte2, byte3 : Byte;
  {$else FPC_LITTLE_ENDIAN}
    byte3, byte2, byte1, byte0 : Byte;
  {$endif FPC_LITTLE_ENDIAN}
  end;

  TWordRec = packed record
  {$ifdef FPC_LITTLE_ENDIAN}
    byte0, byte1 : Byte;
  {$else FPC_LITTLE_ENDIAN}
    byte1, byte0 : Byte;
  {$endif FPC_LITTLE_ENDIAN}
  end;

{ TUInt24Rec }

class operator TUInt24Rec.Explicit(a : TUInt24Rec) : Cardinal;
begin
  TCardinalRec(Result).byte0 := a.byte0;
  TCardinalRec(Result).byte1 := a.byte1;
  TCardinalRec(Result).byte2 := a.byte2;
  TCardinalRec(Result).byte3 := 0;
end;

class operator TUInt24Rec.Implicit(a : TUInt24Rec) : Cardinal;
begin
  TCardinalRec(Result).byte0 := a.byte0;
  TCardinalRec(Result).byte1 := a.byte1;
  TCardinalRec(Result).byte2 := a.byte2;
  TCardinalRec(Result).byte3 := 0;
end;

class operator TUInt24Rec.Implicit(a : TUInt24Rec) : LongInt;
begin
  Result := Cardinal(a);
end;

class operator TUInt24Rec.Implicit(a : TUInt24Rec) : Word;
begin
{$IFOPT R+}
  if (a.byte2 > 0) then
    Error(reIntOverflow);
{$ENDIF R+}
  TWordRec(Result).byte0 := a.byte0;
  TWordRec(Result).byte1 := a.byte1;
end;

class operator TUInt24Rec.Implicit(a : TUInt24Rec) : Byte;
begin
{$IFOPT R+}
  if (a.byte1 > 0) or (a.byte2 > 0) then
    Error(reIntOverflow);
{$ENDIF R+}
  Result := a.byte0;
end;

class operator TUInt24Rec.Implicit(a : Cardinal) : TUInt24Rec;
begin
{$IFOPT R+}
  if (a > $FFFFFF) then
    Error(reIntOverflow);
{$ENDIF R+}
  Result.byte0 := TCardinalRec(a).byte0;
  Result.byte1 := TCardinalRec(a).byte1;
  Result.byte2 := TCardinalRec(a).byte2;
end;

class operator TUInt24Rec.Equal(a, b : TUInt24Rec) : Boolean;
begin
  Result := (a.byte0 = b.byte0) and (a.byte1 = b.byte1) and (a.byte2 = b.byte2);
end;

class operator TUInt24Rec.Equal(a : TUInt24Rec; b : Cardinal) : Boolean;
begin
  Result := (TCardinalRec(b).byte3 = 0) and
            (a.byte0 = TCardinalRec(b).byte0) and
            (a.byte1 = TCardinalRec(b).byte1) and
            (a.byte2 = TCardinalRec(b).byte2);
end;

class operator TUInt24Rec.Equal(a : Cardinal; b : TUInt24Rec) : Boolean;
begin
  Result := (b = a);
end;

class operator TUInt24Rec.Equal(a : TUInt24Rec; b : LongInt) : Boolean;
begin
  Result := (LongInt(a) = b);
end;

class operator TUInt24Rec.Equal(a : LongInt; b : TUInt24Rec) : Boolean;
begin
  Result := (b = a);
end;

class operator TUInt24Rec.Equal(a : TUInt24Rec; b : Word) : Boolean;
begin
  Result := (a.byte2 = 0) and
            (a.byte0 = TWordRec(b).byte0) and
            (a.byte1 = TWordRec(b).byte1);
end;

class operator TUInt24Rec.Equal(a : Word; b : TUInt24Rec) : Boolean;
begin
  Result := (b = a);
end;

class operator TUInt24Rec.Equal(a : TUInt24Rec; b : Byte) : Boolean;
begin
  Result := (a.byte2 = 0) and
            (a.byte1 = 0) and
            (a.byte0 = b);
end;

class operator TUInt24Rec.Equal(a : Byte; b : TUInt24Rec) : Boolean;
begin
  Result := (b = a);
end;

class operator TUInt24Rec.NotEqual(a, b : TUInt24Rec) : Boolean;
begin
  Result := (a.byte0 <> b.byte0) or (a.byte1 <> b.byte1) or (a.byte2 <> b.byte2);
end;

class operator TUInt24Rec.NotEqual(a : TUInt24Rec; b : Cardinal) : Boolean;
begin
  Result := (TCardinalRec(b).byte3 <> 0) or
            (a.byte0 <> TCardinalRec(b).byte0) or
            (a.byte1 <> TCardinalRec(b).byte1) or
            (a.byte2 <> TCardinalRec(b).byte2);
end;

class operator TUInt24Rec.NotEqual(a : Cardinal; b : TUInt24Rec) : Boolean;
begin
  Result := (b <> a);
end;

class operator TUInt24Rec.GreaterThan(a, b: TUInt24Rec): Boolean;
begin
  Result := (a.byte2 > b.byte2) or
            ((a.byte2 = b.byte2) and (a.byte1 > b.byte1)) or
            ((a.byte2 = b.byte2) and (a.byte1 = b.byte1) and (a.byte0 > b.byte0));
end;

class operator TUInt24Rec.GreaterThan(a: TUInt24Rec; b: Cardinal): Boolean;
begin
  Result := Cardinal(a) > b;
end;

class operator TUInt24Rec.GreaterThan(a: Cardinal; b: TUInt24Rec): Boolean;
begin
  Result := a > Cardinal(b);
end;

class operator TUInt24Rec.GreaterThanOrEqual(a, b: TUInt24Rec): Boolean;
begin
  Result := (a.byte2 > b.byte2) or
            ((a.byte2 = b.byte2) and (a.byte1 > b.byte1)) or
            ((a.byte2 = b.byte2) and (a.byte1 = b.byte1) and (a.byte0 >= b.byte0));
end;

class operator TUInt24Rec.GreaterThanOrEqual(a: TUInt24Rec; b: Cardinal): Boolean;
begin
  Result := Cardinal(a) >= b;
end;

class operator TUInt24Rec.GreaterThanOrEqual(a: Cardinal; b: TUInt24Rec): Boolean;
begin
  Result := a >= Cardinal(b);
end;

class operator TUInt24Rec.LessThan(a, b: TUInt24Rec): Boolean;
begin
  Result := (b > a);
end;

class operator TUInt24Rec.LessThan(a: TUInt24Rec; b: Cardinal): Boolean;
begin
  Result := Cardinal(a) < b;
end;

class operator TUInt24Rec.LessThan(a: Cardinal; b: TUInt24Rec): Boolean;
begin
  Result := a < Cardinal(b);
end;

class operator TUInt24Rec.LessThanOrEqual(a, b: TUInt24Rec): Boolean;
begin
  Result := (b >= a);
end;

class operator TUInt24Rec.LessThanOrEqual(a: TUInt24Rec; b: Cardinal): Boolean;
begin
  Result := Cardinal(a) <= b;
end;

class operator TUInt24Rec.LessThanOrEqual(a: Cardinal; b: TUInt24Rec): Boolean;
begin
  Result := a <= Cardinal(b);
end;

function GenerateEndianIncludeFileName(
  const AStoreName : string;
  const AEndian    : TEndianKind
): string;inline;
begin
  Result := ExtractFilePath(AStoreName) +
            ChangeFileExt(ExtractFileName(AStoreName),Format('_%s.inc',[ENDIAN_SUFFIX[AEndian]]));
end;

function IsBitON(const AData : Byte; const ABit : TBitOrder) : Boolean ;
begin
  Result := ( ( AData and ( 1 shl ABit ) ) <> 0 );
end;

procedure SetBit(var AData : Byte; const ABit : TBitOrder; const AValue : Boolean);
begin
  if AValue then
    AData := AData or (1 shl (ABit mod 8))
  else
    AData := AData and ( not ( 1 shl ( ABit mod 8 ) ) );
end;

var
  FS : TFormatSettings;

function EvaluateFloat(const AStr : string) : Double;
var
  s, n, d : string;
  i : Integer;
begin
  Result := 0;
  s := Trim(AStr);
  if (Length(s) > 0) then begin
    i := Pos('/',s);
    if (i < 1) then
      Result := StrToFloat(s,FS)
    else begin
      n := Copy(s,1,i-1);
      d := Copy(s,i+1,MaxInt);
      Result := StrToInt(n) / StrToInt(d);
    end;
  end;
end;

function StrToCategory(const AStr : string) : TUnicodeCategory;
var
  s : string;
begin
  s := UpperCase(Trim(AStr));
  if (s = 'LU') then
    Result := ucUppercaseLetter
  else if (s = 'LL') then
    Result := ucLowercaseLetter
  else if (s = 'LT') then
    Result := ucTitlecaseLetter
  else if (s = 'LM') then
    Result := ucModifierLetter
  else if (s = 'LO') then
    Result := ucOtherLetter
  else

  if (s = 'MN') then
    Result := ucNonSpacingMark
  else if (s = 'MC') then
    Result := ucCombiningMark
  else if (s = 'ME') then
    Result := ucEnclosingMark
  else

  if (s = 'ND') then
    Result := ucDecimalNumber
  else if (s = 'NL') then
    Result := ucLetterNumber
  else if (s = 'NO') then
    Result := ucOtherNumber
  else

  if (s = 'PC') then
    Result := ucConnectPunctuation
  else if (s = 'PD') then
    Result := ucDashPunctuation
  else if (s = 'PS') then
    Result := ucOpenPunctuation
  else if (s = 'PE') then
    Result := ucClosePunctuation
  else if (s = 'PI') then
    Result := ucInitialPunctuation
  else     if (s = 'PF') then
    Result := ucFinalPunctuation
  else if (s = 'PO') then
    Result := ucOtherPunctuation
  else

  if (s = 'SM') then
    Result := ucMathSymbol
  else if (s = 'SC') then
    Result := ucCurrencySymbol
  else if (s = 'SK') then
    Result := ucModifierSymbol
  else if (s = 'SO') then
    Result := ucOtherSymbol
  else

  if (s = 'ZS') then
    Result := ucSpaceSeparator
  else if (s = 'ZL') then
    Result := ucLineSeparator
  else if (s = 'ZP') then
    Result := ucParagraphSeparator
  else

  if (s = 'CC') then
    Result := ucControl
  else if (s = 'CF') then
    Result := ucFormat
  else if (s = 'CS') then
    Result := ucSurrogate
  else if (s = 'CO') then
    Result := ucPrivateUse
  else
    Result := ucUnassigned;
end;

function StringToCodePoint(ACP : string) : TUnicodeCodePoint;
var
  s : string;
begin
  s := Trim(ACP);
  Result := 0;
  if (Length(s) > 0) and (s <> '#') then
    Result := StrToInt('$' + s);
end;

{function IsWhiteSpace(const ACodePoint : TUnicodeCodePoint) : Boolean;
begin
  case ACodePoint of
    $0009..$000D  : Result := True;// White_Space # Cc   [5] <control-0009>..<control-000D>
    $0020          : Result := True;// White_Space # Zs       SPACE
    $0085          : Result := True;// White_Space # Cc       <control-0085>
    $00A0          : Result := True;// White_Space # Zs       NO-BREAK SPACE
    $1680          : Result := True;// White_Space # Zs       OGHAM SPACE MARK
    $180E          : Result := True;// White_Space # Zs       MONGOLIAN VOWEL SEPARATOR
    $2000..$200A   : Result := True;// White_Space # Zs  [11] EN QUAD..HAIR SPACE
    $2028          : Result := True;// White_Space # Zl       LINE SEPARATOR
    $2029          : Result := True;// White_Space # Zp       PARAGRAPH SEPARATOR
    $202F          : Result := True;// White_Space # Zs       NARROW NO-BREAK SPACE
    $205F          : Result := True;// White_Space # Zs       MEDIUM MATHEMATICAL SPACE
    $3000          : Result := True;// White_Space # Zs       IDEOGRAPHIC SPACE
    else
      Result := False;
  end;
end;}
function IsWhiteSpace(
  const ACodePoint   : TUnicodeCodePoint;
  const AWhiteSpaces : TCodePointRecArray
) : Boolean;
var
  i : Integer;
  p : ^TCodePointRec;
begin
  p := @AWhiteSpaces[Low(AWhiteSpaces)];
  for i := Low(AWhiteSpaces) to High(AWhiteSpaces) do begin
    if (p^.LineType = 0) then begin
      if (p^.CodePoint = ACodePoint) then
        exit(True);
    end else begin
      if (ACodePoint >= p^.StartCodePoint) and (ACodePoint <= p^.EndCodePoint) then
        exit(True);
    end;
    Inc(p);
  end;
  Result := False;
end;

function NormalizeBlockName(const AName : string) : string;
var
  i, c, k : Integer;
  s : string;
begin
  c := Length(AName);
  SetLength(Result,c);
  s := LowerCase(AName);
  k := 0;
  for i := 1 to c do begin
    if (s[1] in ['a'..'z','0'..'9','-']) then begin
      k := k + 1;
      Result[k] := s[i];
    end;
  end;
  SetLength(Result,k);
end;

procedure ParseBlokcs(
      ADataAStream   : TMemoryStream;
  var ABlocks        : TBlocks
);
const
  LINE_LENGTH        = 1024;
  DATA_LENGTH        = 25000;
var
  p : PAnsiChar;
  actualDataLen : Integer;
  bufferLength, bufferPos, lineLength, linePos : Integer;
  line : ansistring;

  function NextLine() : Boolean;
  var
    locOldPos : Integer;
    locOldPointer : PAnsiChar;
  begin
    Result := False;
    locOldPointer := p;
    locOldPos := bufferPos;
    while (bufferPos < bufferLength) and (p^ <> #10) do begin
      Inc(p);
      Inc(bufferPos);
    end;
    if (locOldPos = bufferPos) and (p^ = #10) then begin
      lineLength := 0;
      Inc(p);
      Inc(bufferPos);
      linePos := 1;
      Result := True;
    end else  if (locOldPos < bufferPos) then begin
      lineLength := (bufferPos - locOldPos);
      Move(locOldPointer^,line[1],lineLength);
      if (p^ = #10) then begin
        Dec(lineLength);
        Inc(p);
        Inc(bufferPos);
      end;
      linePos := 1;
      Result := True;
    end;
  end;

  function NextToken() : ansistring;
  var
    k : Integer;
  begin
    k := linePos;
    if (linePos < lineLength) and (line[linePos] in [';','#','.']) then begin
      Inc(linePos);
      Result := Copy(line,k,(linePos-k));
      exit;
    end;
    while (linePos < lineLength) and not(line[linePos] in [';','#','.']) do
      Inc(linePos);
    if (linePos > k) then begin
      if (line[linePos] in [';','#','.']) then
        Result := Copy(line,k,(linePos-k))
      else
        Result := Copy(line,k,(linePos-k+1));
      Result := Trim(Result);
    end else begin
      Result := '';
    end;
  end;

  procedure ParseLine();
  var
    locData : TBlockItemRec;
    s : ansistring;
  begin
    s := NextToken();
    if (s = '') or (s[1] = '#') then
      exit;
    locData.RangeStart := StrToInt('$'+s);
    s := NextToken();
    if (s <> '.') then
      raise Exception.CreateFmt('"." expected but "%s" found.',[s]);
    s := NextToken();
    if (s <> '.') then
      raise Exception.CreateFmt('"." expected but "%s" found.',[s]);
    s := NextToken();
    locData.RangeEnd := StrToInt('$'+s);
    s := NextToken();
    if (s <> ';') then
      raise Exception.CreateFmt('";" expected but "%s" found.',[s]);
    locData.Name := Trim(NextToken());
    locData.CanonicalName := NormalizeBlockName(locData.Name);
    if (Length(ABlocks) <= actualDataLen) then
      SetLength(ABlocks,Length(ABlocks)*2);
    ABlocks[actualDataLen] := locData;
    Inc(actualDataLen);
  end;

  procedure Prepare();
  begin
    SetLength(ABlocks,DATA_LENGTH);
    actualDataLen := 0;
    bufferLength := ADataAStream.Size;
    bufferPos := 0;
    p := ADataAStream.Memory;
    lineLength := 0;
    SetLength(line,LINE_LENGTH);
  end;

begin
  Prepare();
  while NextLine() do
    ParseLine();
  SetLength(ABlocks,actualDataLen);
end;

procedure ParseProps(
      ADataAStream   : TMemoryStream;
  var APropList      : TPropListLineRecArray
);
const
  LINE_LENGTH        = 1024;
  DATA_LENGTH        = 25000;
var
  p : PAnsiChar;
  actualDataLen : Integer;
  bufferLength, bufferPos, lineLength, linePos : Integer;
  line : ansistring;

  function NextLine() : Boolean;
  var
    locOldPos : Integer;
    locOldPointer : PAnsiChar;
  begin
    Result := False;
    locOldPointer := p;
    locOldPos := bufferPos;
    while (bufferPos < bufferLength) and (p^ <> #10) do begin
      Inc(p);
      Inc(bufferPos);
    end;
    if (locOldPos = bufferPos) and (p^ = #10) then begin
      lineLength := 0;
      Inc(p);
      Inc(bufferPos);
      linePos := 1;
      Result := True;
    end else  if (locOldPos < bufferPos) then begin
      lineLength := (bufferPos - locOldPos);
      Move(locOldPointer^,line[1],lineLength);
      if (p^ = #10) then begin
        Dec(lineLength);
        Inc(p);
        Inc(bufferPos);
      end;
      linePos := 1;
      Result := True;
    end;
  end;

  function NextToken() : ansistring;
  var
    k : Integer;
  begin
    k := linePos;
    if (linePos < lineLength) and (line[linePos] in [';','#','.']) then begin
      Inc(linePos);
      Result := Copy(line,k,(linePos-k));
      exit;
    end;
    while (linePos < lineLength) and not(line[linePos] in [';','#','.']) do
      Inc(linePos);
    if (linePos > k) then begin
      if (line[linePos] in [';','#','.']) then
        Result := Copy(line,k,(linePos-k))
      else
        Result := Copy(line,k,(linePos-k+1));
      Result := Trim(Result);
    end else begin
      Result := '';
    end;
  end;

  procedure ParseLine();
  var
    locCP : Cardinal;
    locData : TPropListLineRec;
    s : ansistring;
  begin
    s := NextToken();
    if (s = '') or (s[1] = '#') then
      exit;
    locCP := StrToInt('$'+s);
    s := NextToken();
    if (s = ';') then begin
      locData.CodePoint.LineType := 0;
      locData.CodePoint.CodePoint := locCP;
    end else begin
      if (s = '') or (s <> '.') or (NextToken() <> '.') then
        raise Exception.CreateFmt('Invalid line : "%s".',[Copy(line,1,lineLength)]);
      locData.CodePoint.LineType := 1;
      locData.CodePoint.StartCodePoint := locCP;
      locData.CodePoint.EndCodePoint := StrToInt('$'+NextToken());
      s := NextToken();
      if (s <> ';') then
        raise Exception.CreateFmt('"." expected but "%s" found.',[s]);
    end;
    locData.PropName := Trim(NextToken());
    if (Length(APropList) <= actualDataLen) then
      SetLength(APropList,Length(APropList)*2);
    APropList[actualDataLen] := locData;
    Inc(actualDataLen);
  end;

  procedure Prepare();
  begin
    SetLength(APropList,DATA_LENGTH);
    actualDataLen := 0;
    bufferLength := ADataAStream.Size;
    bufferPos := 0;
    p := ADataAStream.Memory;
    lineLength := 0;
    SetLength(line,LINE_LENGTH);
  end;

begin
  Prepare();
  while NextLine() do
    ParseLine();
  SetLength(APropList,actualDataLen);
end;

function FindCodePointsByProperty(
  const APropName : string;
  const APropList : TPropListLineRecArray
) : TCodePointRecArray;
var
  r : TCodePointRecArray;
  i, k : Integer;
  s : string;
begin
  k := 0;
  r := nil;
  s := LowerCase(Trim(APropName));
  for i := Low(APropList) to High(APropList) do begin
    if (LowerCase(APropList[i].PropName) = s) then begin
      if (k >= Length(r)) then begin
        if (k = 0) then
          SetLength(r,24)
        else
          SetLength(r,(2*Length(r)));
      end;
      r[k] := APropList[i].CodePoint;
      Inc(k);
    end;
  end;
  SetLength(r,k);
  Result := r;
end;

procedure ParseHangulSyllableTypes(
      ADataAStream   : TMemoryStream;
  var ACodePointList : TCodePointRecArray
);
const
  LINE_LENGTH        = 1024;
  DATA_LENGTH        = 25000;
var
  p : PAnsiChar;
  actualDataLen : Integer;
  bufferLength, bufferPos, lineLength, linePos : Integer;
  line : ansistring;

  function NextLine() : Boolean;
  var
    locOldPos : Integer;
    locOldPointer : PAnsiChar;
  begin
    Result := False;
    locOldPointer := p;
    locOldPos := bufferPos;
    while (bufferPos < bufferLength) and (p^ <> #10) do begin
      Inc(p);
      Inc(bufferPos);
    end;
    if (locOldPos = bufferPos) and (p^ = #10) then begin
      lineLength := 0;
      Inc(p);
      Inc(bufferPos);
      linePos := 1;
      Result := True;
    end else  if (locOldPos < bufferPos) then begin
      lineLength := (bufferPos - locOldPos);
      Move(locOldPointer^,line[1],lineLength);
      if (p^ = #10) then begin
        Dec(lineLength);
        Inc(p);
        Inc(bufferPos);
      end;
      linePos := 1;
      Result := True;
    end;
  end;

  function NextToken() : ansistring;
  var
    k : Integer;
  begin
    k := linePos;
    if (linePos < lineLength) and (line[linePos] = '.') then begin
      Inc(linePos);
      while (linePos < lineLength) and (line[linePos] = '.') do begin
        Inc(linePos);
      end;
      Result := Copy(line,k,(linePos-k));
      exit;
    end;
    while (linePos < lineLength) and not(line[linePos] in [';','#','.']) do
      Inc(linePos);
    if (linePos > k) then begin
      if (line[linePos] in [';','#','.']) then
        Result := Copy(line,k,(linePos-k))
      else
        Result := Copy(line,k,(linePos-k+1));
      Result := Trim(Result);
    end else begin
      Result := '';
    end;
    //Inc(linePos);
  end;

  procedure ParseLine();
  var
    locData : TCodePointRec;
    s : ansistring;
  begin
    s := NextToken();
    if (s = '') or (s[1] = '#') then
      exit;
    locData.CodePoint := StrToInt('$'+s);
    s := NextToken();
    if (s = '') or (s[1] in [';','#']) then begin
      locData.LineType := 0;
    end else begin
      if (s <> '..') then
        raise Exception.CreateFmt('Unknown line type : "%s"',[Copy(line,1,lineLength)]);
      locData.StartCodePoint := locData.CodePoint;
      locData.EndCodePoint := StrToInt('$'+NextToken());
      locData.LineType := 1;
    end;
    if (Length(ACodePointList) <= actualDataLen) then
      SetLength(ACodePointList,Length(ACodePointList)*2);
    ACodePointList[actualDataLen] := locData;
    Inc(actualDataLen);
  end;

  procedure Prepare();
  begin
    SetLength(ACodePointList,DATA_LENGTH);
    actualDataLen := 0;
    bufferLength := ADataAStream.Size;
    bufferPos := 0;
    p := ADataAStream.Memory;
    lineLength := 0;
    SetLength(line,LINE_LENGTH);
  end;

begin
  Prepare();
  while NextLine() do
    ParseLine();
  SetLength(ACodePointList,actualDataLen);
end;

function IsHangulSyllable(
  const ACodePoint  : TUnicodeCodePoint;
  const AHangulList : TCodePointRecArray
) : Boolean;
var
  i : Integer;
  p : ^TCodePointRec;
begin
  Result := False;
  p := @AHangulList[Low(AHangulList)];
  for i := Low(AHangulList) to High(AHangulList) do begin
    if ( (p^.LineType = 0) and (ACodePoint = p^.CodePoint) ) or
       ( (p^.LineType = 1) and (ACodePoint >= p^.StartCodePoint) and (ACodePoint <= p^.EndCodePoint) )
    then begin
      Result := True;
      Break;
    end;
    Inc(p);
  end;
end;

function IndexOf(
  const AProp     : TPropRec;
  const APropList : TPropRecArray;
  const AActualLen : Integer
) : Integer;overload;
var
  i : Integer;
  p : PPropRec;
begin
  Result := -1;
  if (AActualLen > 0) then begin
    p := @APropList[0];
    for i := 0 to AActualLen - 1 do begin
      if (AProp.Category = p^.Category) and
         (AProp.CCC = p^.CCC) and
         (AProp.NumericIndex = p^.NumericIndex) and
         (AProp.SimpleUpperCase = p^.SimpleUpperCase) and
         (AProp.SimpleLowerCase = p^.SimpleLowerCase) and
         (AProp.WhiteSpace = p^.WhiteSpace) and
         //
         (AProp.DecompositionID =  p^.DecompositionID) and
         (*   ( (AProp.DecompositionID = -1 ) and (p^.DecompositionID = -1) ) or
             ( (AProp.DecompositionID <> -1 ) and (p^.DecompositionID <> -1) )
         *)
         (AProp.HangulSyllable = p^.HangulSyllable)
      then begin
        Result := i;
        Break;
      end;
      Inc(p);
    end;
  end;
end;

function IndexOf(
  const AItem : TUnicodeCodePointArray;
  const AList : TDecompositionArray
) : Integer;overload;
var
  p : TUnicodeCodePointArray;
  i : Integer;
begin
  Result := -1;
  if (Length(AList) = 0) then
    exit;
  for i := Low(AList) to High(AList) do begin
    p := AList[i];
    if (Length(p) = Length(AItem)) then begin
      if CompareMem(@p[0],@AItem[0],Length(AItem)*SizeOf(TUnicodeCodePoint)) then
        exit(i);
    end;
  end;
  Result := -1;
end;

function IndexOf(
  const AItem : TNumericValue;
  const AList : TNumericValueArray;
  const AActualLen : Integer
) : Integer;overload;
var
  p : ^TNumericValue;
  i : Integer;
begin
  Result := -1;
  if (AActualLen = 0) then
    exit;
  p := @AList[Low(AList)];
  for i := Low(AList) to AActualLen - 1 do begin
    if (AItem = p^) then
      exit(i);
    Inc(p);
  end;
  Result := -1;
end;

procedure Parse_UnicodeData(
        ADataAStream   : TMemoryStream;
  var   APropList      : TPropRecArray;
  var   ANumericTable  : TNumericValueArray;
  var   ADataLineList  : TDataLineRecArray;
  var   ADecomposition : TDecompositionArray;
  const AHangulList    : TCodePointRecArray;
  const AWhiteSpaces   : TCodePointRecArray
);
const
  LINE_LENGTH        = 1024;
  PROP_LENGTH        = 5000;
  DATA_LENGTH        = 25000;
var
  p : PAnsiChar;
  bufferLength, bufferPos : Integer;
  actualPropLen, actualDataLen, actualNumLen : Integer;
  line : ansistring;
  lineLength, linePos : Integer;

  function NextLine() : Boolean;
  var
    locOldPos : Integer;
    locOldPointer : PAnsiChar;
  begin
    Result := False;
    locOldPointer := p;
    locOldPos := bufferPos;
    while (bufferPos < bufferLength) and (p^ <> #10) do begin
      Inc(p);
      Inc(bufferPos);
    end;
    if (locOldPos < bufferPos) then begin
      lineLength := (bufferPos - locOldPos);
      Move(locOldPointer^,line[1],lineLength);
      if (p^ = #10) then begin
        Dec(lineLength);
        Inc(p);
        Inc(bufferPos);
      end;
      if (lineLength > 7) then begin
        linePos := 1;
        Result := True;
      end;
    end;
  end;

  function NextToken() : ansistring;
  var
    k : Integer;
  begin
    k := linePos;
    while (linePos < lineLength) and not(line[linePos] in [';','#']) do
      Inc(linePos);
    if (linePos > k) then begin
      if (line[linePos] in [';','#']) then
        Result := Copy(line,k,(linePos-k))
      else
        Result := Copy(line,k,(linePos-k+1));
      Result := Trim(Result);
    end else begin
      Result := '';
    end;
    Inc(linePos);
  end;

  function ParseCanonicalDecomposition(AStr : ansistring) : TUnicodeCodePointArray;
  var
    locStr, ks : ansistring;
    k0,k : Integer;
  begin
    SetLength(Result,0);
    locStr := UpperCase(Trim(AStr));
    if (locStr = '') or (locStr[1] = '<') then
      exit;
    k0 := 1;
    k := 1;
    while (k <= Length(locStr)) do begin
      while (k <= Length(locStr)) and (locStr[k] in ['0'..'9','A'..'F']) do
        inc(k);
      ks := Trim(Copy(locStr,k0,k-k0));
      SetLength(Result,Length(Result)+1);
      Result[Length(Result)-1] := StringToCodePoint(ks);
      Inc(k);
      k0 := k;
    end;
  end;

  procedure ParseLine();
  var
    locCP : TUnicodeCodePoint;
    locProp : TPropRec;
    locData : TDataLineRec;
    s : ansistring;
    locRangeStart, locRangeEnd : Boolean;
    k : Integer;
    locDecompItem : TUnicodeCodePointArray;
    numVal : TNumericValue;
  begin
    FillChar(locData,SizeOf(locData),#0);
    FillChar(locProp,SizeOf(locProp),#0);
    locCP := StrToInt('$'+NextToken());
    s := NextToken();
    locRangeStart := AnsiEndsText(', First>',s);
    if locRangeStart then
      locRangeEnd := False
    else
      locRangeEnd := AnsiEndsText(', Last>',s);
    if locRangeStart then begin
      locData.LineType := 1;
      locData.StartCodePoint := locCP;
    end else if locRangeEnd then begin
      ADataLineList[actualDataLen - 1].EndCodePoint := locCP;
      exit;
      //locData.EndCodePoint := locCP;
    end else begin
      locData.LineType := 0;
      locData.CodePoint := locCP;
    end;
    locProp.Category := StrToCategory(NextToken());
    locProp.CCC := StrToInt(NextToken());//Canonical_Combining_Class
    NextToken();//Bidi_Class
    s := NextToken();//Decomposition_Type
    locDecompItem := ParseCanonicalDecomposition(s);
    if (Length(locDecompItem) = 0) then
      locProp.DecompositionID := -1
    else begin
      locProp.DecompositionID := IndexOf(locDecompItem,ADecomposition);
      if (locProp.DecompositionID = -1) then begin
        k := Length(ADecomposition);
        locProp.DecompositionID := k;
        SetLength(ADecomposition,k+1);
        ADecomposition[k] := locDecompItem;
      end;
    end;

    numVal := EvaluateFloat(NextToken());
    if (numVal <> Double(0.0)) then begin
      NextToken();
      NextToken();
    end else begin
      s := NextToken();
      if (s <> '') then
        numVal := EvaluateFloat(s);
      s := NextToken();
      if (numVal = Double(0.0)) then
        numVal := EvaluateFloat(s);
    end;
    k := IndexOf(numVal,ANumericTable,actualNumLen);
    if (k = -1) then begin
      if (actualNumLen >= Length(ANumericTable)) then
        SetLength(ANumericTable,(actualNumLen*2));
      ANumericTable[actualNumLen] := numVal;
      k := actualNumLen;
      Inc(actualNumLen);
    end;
    locProp.NumericIndex := k;

    NextToken();//Bidi_Mirroed
    NextToken();//Unicode_l_Name
    NextToken();//ISO_Comment
    locProp.SimpleUpperCase := StringToCodePoint(NextToken());
    locProp.SimpleLowerCase := StringToCodePoint(NextToken());
    NextToken();//Simple_Title_Case_Mapping
    locProp.WhiteSpace := IsWhiteSpace(locCP,AWhiteSpaces);
    locProp.HangulSyllable := IsHangulSyllable(locCP,AHangulList);
    k := IndexOf(locProp,APropList,actualPropLen);
    if (k = -1) then begin
      k := actualPropLen;
      locProp.PropID := k{ + 1};
      APropList[k] := locProp;
      Inc(actualPropLen);
    end;
    locData.PropID := k;
    ADataLineList[actualDataLen] := locData;
    Inc(actualDataLen);
  end;

  procedure Prepare();
  var
    r : TPropRec;
  begin
    SetLength(APropList,PROP_LENGTH);
    actualPropLen := 0;
    SetLength(ADataLineList,DATA_LENGTH);
    actualDataLen := 0;
    bufferLength := ADataAStream.Size;
    bufferPos := 0;
    p := ADataAStream.Memory;
    lineLength := 0;
    SetLength(line,LINE_LENGTH);
    SetLength(ANumericTable,500);
    actualNumLen := 0;

    FillChar(r,SizeOf(r),#0);
    r.PropID := 0;
    r.Category := ucUnassigned;
    r.DecompositionID := -1;
    r.NumericIndex := 0;
    APropList[0] := r;
    Inc(actualPropLen);
    ANumericTable[0] := 0;
    Inc(actualNumLen);
  end;

begin
  Prepare();
  while NextLine() do
    ParseLine();
  SetLength(APropList,actualPropLen);
  SetLength(ADataLineList,actualDataLen);
  SetLength(ANumericTable,actualNumLen);
end;

function GetPropID(
        ACodePoint    : TUnicodeCodePoint;
  const ADataLineList : TDataLineRecArray
) : Cardinal;
var
  i : Integer;
  p : PDataLineRec;
begin
  Result := 0;
  p := @ADataLineList[Low(ADataLineList)];
  for i := Low(ADataLineList) to High(ADataLineList) do begin
    if (p^.LineType = 0) then begin
      if (p^.CodePoint = ACodePoint) then begin
        Result := p^.PropID;
        Break;
      end;
    end else begin
      if (p^.StartCodePoint <= ACodePoint) and (p^.EndCodePoint >= ACodePoint) then begin
        Result := p^.PropID;
        Break;
      end;
    end;
    Inc(p);
  end;
end;

procedure MakeDecomposition(
  const ARawData : TDecompositionArray;
  var   ABook    : TDecompositionBook
);
var
  i, c, locPos : Integer;
  locItem : TUnicodeCodePointArray;
begin
  c := 0;
  for i := Low(ARawData) to High(ARawData) do
    c := c + Length(ARawData[i]);
  SetLength(ABook.CodePoints,c);
  SetLength(ABook.Index,Length(ARawData));
  locPos := 0;
  for i := Low(ARawData) to High(ARawData) do begin
    locItem := ARawData[i];
    ABook.Index[i].StartPosition := locPos;
    ABook.Index[i].Length := Length(locItem);
    Move(locItem[0],ABook.CodePoints[locPos],(Length(locItem) * SizeOf(TUnicodeCodePoint)));
    locPos := locPos + Length(locItem);
  end;
end;

type
  PBmpSecondTableItem = ^TBmpSecondTableItem;
function IndexOf(
  const AItem  : PBmpSecondTableItem;
  const ATable : TBmpSecondTable;
  const ATableActualLength : Integer
) : Integer;overload;
var
  i : Integer;
  p : PBmpSecondTableItem;
begin
  Result := -1;
  if (ATableActualLength > 0) then begin
    p := @ATable[0];
    for i := 0 to ATableActualLength - 1 do begin
      if CompareMem(p,AItem,SizeOf(TBmpSecondTableItem)) then begin
        Result := i;
        Break;
      end;
      Inc(p);
    end;
  end;
end;

procedure MakeBmpTables(
  var   AFirstTable   : TBmpFirstTable;
  var   ASecondTable  : TBmpSecondTable;
  const ADataLineList : TDataLineRecArray
);
var
  locLowByte, locHighByte : Byte;
  locTableItem : TBmpSecondTableItem;
  locCP : TUnicodeCodePoint;
  i, locSecondActualLen : Integer;
begin
  SetLength(ASecondTable,120);
  locSecondActualLen := 0;
  for locHighByte := 0 to 255 do begin
    FillChar(locTableItem,SizeOf(locTableItem),#0);
    for locLowByte := 0 to 255 do begin
      locCP := (locHighByte * 256) + locLowByte;
      locTableItem[locLowByte] := GetPropID(locCP,ADataLineList)// - 1;
    end;
    i := IndexOf(@locTableItem,ASecondTable,locSecondActualLen);
    if (i = -1) then begin
      if (locSecondActualLen = Length(ASecondTable)) then
        SetLength(ASecondTable,locSecondActualLen + 50);
      i := locSecondActualLen;
      ASecondTable[i] := locTableItem;
      Inc(locSecondActualLen);
    end;
    AFirstTable[locHighByte] := i;
  end;
  SetLength(ASecondTable,locSecondActualLen);
end;

type
  P3lvlBmp3TableItem = ^T3lvlBmp3TableItem;
function IndexOf(
  const AItem  : P3lvlBmp3TableItem;
  const ATable : T3lvlBmp3Table;
  const ATableActualLength : Integer
) : Integer;overload;
var
  i : Integer;
  p : P3lvlBmp3TableItem;
begin
  Result := -1;
  if (ATableActualLength > 0) then begin
    p := @ATable[0];
    for i := 0 to ATableActualLength - 1 do begin
      if CompareMem(p,AItem,SizeOf(T3lvlBmp3TableItem)) then begin
        Result := i;
        Break;
      end;
      Inc(p);
    end;
  end;
end;

type
  P3lvlBmp2TableItem = ^T3lvlBmp2TableItem;
function IndexOf(
  const AItem  : P3lvlBmp2TableItem;
  const ATable : T3lvlBmp2Table
) : Integer;overload;
var
  i : Integer;
  p : P3lvlBmp2TableItem;
begin
  Result := -1;
  if (Length(ATable) > 0) then begin
    p := @ATable[0];
    for i := 0 to Length(ATable) - 1 do begin
      if CompareMem(p,AItem,SizeOf(T3lvlBmp2TableItem)) then begin
        Result := i;
        Break;
      end;
      Inc(p);
    end;
  end;
end;
procedure MakeBmpTables3Levels(
  var   AFirstTable   : T3lvlBmp1Table;
  var   ASecondTable  : T3lvlBmp2Table;
  var   AThirdTable  : T3lvlBmp3Table;
  const ADataLineList : TDataLineRecArray
);
var
  locLowByte0, locLowByte1, locHighByte : Byte;
  locTableItem2 : T3lvlBmp2TableItem;
  locTableItem3 : T3lvlBmp3TableItem;
  locCP : TUnicodeCodePoint;
  i, locThirdActualLen : Integer;
begin
  SetLength(AThirdTable,120);
  locThirdActualLen := 0;
  for locHighByte := 0 to 255 do begin
    FillChar(locTableItem2,SizeOf(locTableItem2),#0);
    for locLowByte0 := 0 to 15 do begin
      FillChar(locTableItem3,SizeOf(locTableItem3),#0);
      for locLowByte1 := 0 to 15 do begin
        locCP := (locHighByte * 256) + (locLowByte0*16) + locLowByte1;
        locTableItem3[locLowByte1] := GetPropID(locCP,ADataLineList);
      end;
      i := IndexOf(@locTableItem3,AThirdTable,locThirdActualLen);
      if (i = -1) then begin
        if (locThirdActualLen = Length(AThirdTable)) then
          SetLength(AThirdTable,locThirdActualLen + 50);
        i := locThirdActualLen;
        AThirdTable[i] := locTableItem3;
        Inc(locThirdActualLen);
      end;
      locTableItem2[locLowByte0] := i;
    end;
    i := IndexOf(@locTableItem2,ASecondTable);
    if (i = -1) then begin
      i := Length(ASecondTable);
      SetLength(ASecondTable,(i + 1));
      ASecondTable[i] := locTableItem2;
    end;
    AFirstTable[locHighByte] := i;
  end;
  SetLength(AThirdTable,locThirdActualLen);
end;

procedure GenerateLicenceText(ADest : TStream);
var
  s : ansistring;
begin
  s := SLicenseText + sLineBreak + sLineBreak;
  ADest.Write(s[1],Length(s));
end;

procedure GenerateBmpTables(
        ADest : TStream;
  var   AFirstTable   : TBmpFirstTable;
  var   ASecondTable  : TBmpSecondTable
);
  procedure AddLine(const ALine : ansistring);
  var
    buffer : ansistring;
  begin
    buffer := ALine + sLineBreak;
    ADest.Write(buffer[1],Length(buffer));
  end;

var
  i, j, c : Integer;
  locLine : string;
begin
  AddLine('const');
  AddLine('  UC_TABLE_1 : array[0..255] of Byte = (');
  locLine := '';
  for i := Low(AFirstTable) to High(AFirstTable) - 1 do begin
    locLine := locLine + IntToStr(AFirstTable[i]) + ',';
    if (((i+1) mod 16) = 0) then begin
      locLine := '    ' + locLine;
      AddLine(locLine);
      locLine := '';
    end;
  end;
  locLine := locLine + IntToStr(AFirstTable[High(AFirstTable)]);
  locLine := '    ' + locLine;
  AddLine(locLine);
  AddLine('  );' + sLineBreak);

  AddLine('  UC_TABLE_2 : array[0..(256*' + IntToStr(Length(ASecondTable)) +'-1)] of Word =(');
  c := High(ASecondTable);
  for i := Low(ASecondTable) to c do begin
    locLine := '';
    for j := Low(TBmpSecondTableItem) to High(TBmpSecondTableItem) do begin
      locLine := locLine + IntToStr(ASecondTable[i][j]) + ',';
      if (((j+1) mod 16) = 0) then begin
        if (i = c) and (j = 255) then
          Delete(locLine,Length(locLine),1);
        locLine := '    ' + locLine;
        AddLine(locLine);
        locLine := '';
      end;
    end;
  end;
  AddLine('  );' + sLineBreak);
end;

//----------------------------------
procedure Generate3lvlBmpTables(
        ADest : TStream;
  var   AFirstTable   : T3lvlBmp1Table;
  var   ASecondTable  : T3lvlBmp2Table;
  var   AThirdTable   : T3lvlBmp3Table
);

  procedure AddLine(const ALine : ansistring);
  var
    buffer : ansistring;
  begin
    buffer := ALine + sLineBreak;
    ADest.Write(buffer[1],Length(buffer));
  end;

var
  i, j, c : Integer;
  locLine : string;
begin
  AddLine('const');
  AddLine('  UC_TABLE_1 : array[0..255] of Byte = (');
  locLine := '';
  for i := Low(AFirstTable) to High(AFirstTable) - 1 do begin
    locLine := locLine + IntToStr(AFirstTable[i]) + ',';
    if (((i+1) mod 16) = 0) then begin
      locLine := '    ' + locLine;
      AddLine(locLine);
      locLine := '';
    end;
  end;
  locLine := locLine + IntToStr(AFirstTable[High(AFirstTable)]);
  locLine := '    ' + locLine;
  AddLine(locLine);
  AddLine('  );' + sLineBreak);

  AddLine('  UC_TABLE_2 : array[0..' + IntToStr(Length(ASecondTable)-1) +'] of array[0..15] of Word = (');
  c := High(ASecondTable);
  for i := Low(ASecondTable) to c do begin
    locLine := '(';
    for j := Low(T3lvlBmp2TableItem) to High(T3lvlBmp2TableItem) do
      locLine := locLine + IntToStr(ASecondTable[i][j]) + ',';
    Delete(locLine,Length(locLine),1);
    locLine := '    ' + locLine + ')';
    if (i < c) then
      locLine := locLine + ',';
    AddLine(locLine);
  end;
  AddLine('  );' + sLineBreak);

  AddLine('  UC_TABLE_3 : array[0..' + IntToStr(Length(AThirdTable)-1) +'] of array[0..15] of Word = (');
  c := High(AThirdTable);
  for i := Low(AThirdTable) to c do begin
    locLine := '(';
    for j := Low(T3lvlBmp3TableItem) to High(T3lvlBmp3TableItem) do
      locLine := locLine + IntToStr(AThirdTable[i][j]) + ',';
    Delete(locLine,Length(locLine),1);
    locLine := '    ' + locLine + ')';
    if (i < c) then
      locLine := locLine + ',';
    AddLine(locLine);
  end;
  AddLine('  );' + sLineBreak);
end;

function UInt24ToStr(const AValue : UInt24; const AEndian : TEndianKind): string;inline;
begin
  if (AEndian = ekBig) then
    Result := Format(
                '(byte2 : $%s; byte1 : $%s; byte0 : $%s;)',
                [ IntToHex(AValue.byte2,2), IntToHex(AValue.byte1,2),
                  IntToHex(AValue.byte0,2)
                ]
              )
  else
    Result := Format(
                '(byte0 : $%s; byte1 : $%s; byte2 : $%s;)',
                [ IntToHex(AValue.byte0,2), IntToHex(AValue.byte1,2),
                  IntToHex(AValue.byte2,2)
                ]
              );
end;

procedure GeneratePropTable(
        ADest     : TStream;
  const APropList : TPropRecArray;
  const AEndian   : TEndianKind
);

  procedure AddLine(const ALine : ansistring);
  var
    buffer : ansistring;
  begin
    buffer := ALine + sLineBreak;
    ADest.Write(buffer[1],Length(buffer));
  end;

var
  i : Integer;
  locLine : string;
  p : PPropRec;
begin
  AddLine('');
  AddLine('const');
  AddLine('  UC_PROP_REC_COUNT = ' + IntToStr(Length(APropList)) + ';');
  AddLine('  UC_PROP_ARRAY : array[0..(UC_PROP_REC_COUNT-1)] of TUC_Prop = (');
  p := @APropList[0];
  for i := Low(APropList) to High(APropList) - 1 do begin
    locLine := '    (CategoryData : ' + IntToStr(p^.CategoryData) + ';' +
               ' CCC : ' + IntToStr(p^.CCC) + ';' +
               ' NumericIndex : ' + IntToStr(p^.NumericIndex) + ';' +
               ' SimpleUpperCase : ' + UInt24ToStr(p^.SimpleUpperCase,AEndian) + ';' +
               ' SimpleLowerCase : ' + UInt24ToStr(p^.SimpleLowerCase,AEndian) + ';' +
               ' DecompositionID : ' + IntToStr(p^.DecompositionID) + '),';
    AddLine(locLine);
    Inc(p);
  end;
  locLine := //'    (Category : TUnicodeCategory.' + GetEnumName(pti,Ord(p^.Category)) + ';' +
             '    (CategoryData : ' + IntToStr(p^.CategoryData) + ';' +
             ' CCC : ' + IntToStr(p^.CCC) + ';' +
             ' NumericIndex : ' + IntToStr(p^.NumericIndex) + ';' +
             ' SimpleUpperCase : ' + UInt24ToStr(p^.SimpleUpperCase,AEndian) + ';' +
             ' SimpleLowerCase : ' + UInt24ToStr(p^.SimpleLowerCase,AEndian) + ';' +
             ' DecompositionID : ' + IntToStr(p^.DecompositionID) + ')';
  AddLine(locLine);
  AddLine('  );' + sLineBreak);
end;

procedure GenerateNumericTable(
        ADest         : TStream;
  const ANumList      : TNumericValueArray;
  const ACompleteUnit : Boolean
);

  procedure AddLine(const ALine : ansistring);
  var
    buffer : ansistring;
  begin
    buffer := ALine + sLineBreak;
    ADest.Write(buffer[1],Length(buffer));
  end;

var
  i : Integer;
  locLine : string;
  p : ^TNumericValue;
begin
  if ACompleteUnit then begin
    GenerateLicenceText(ADest);
    AddLine('unit unicodenumtable;');
    AddLine('interface');
    AddLine('');
  end;
  AddLine('');
  AddLine('const');
  AddLine('  UC_NUMERIC_COUNT = ' + IntToStr(Length(ANumList)) + ';');
  AddLine('  UC_NUMERIC_ARRAY : array[0..(UC_NUMERIC_COUNT-1)] of Double = (');
  locLine := '';
  p := @ANumList[0];
  for i := Low(ANumList) to High(ANumList) - 1 do begin
    locLine := locLine + FloatToStr(p^,FS) + ' ,';
    if (i > 0) and ((i mod 8) = 0) then begin
      AddLine('    ' + locLine);
      locLine := '';
    end;
    Inc(p);
  end;
  locLine := locLine + FloatToStr(p^,FS);
  AddLine('    ' + locLine);
  AddLine('  );' + sLineBreak);
  if ACompleteUnit then begin
    AddLine('');
    AddLine('implementation');
    AddLine('');
    AddLine('end.');
  end;
end;

procedure GenerateDecompositionBookTable(
        ADest   : TStream;
  const ABook   : TDecompositionBook;
  const AEndian : TEndianKind
);

  procedure AddLine(const ALine : ansistring);
  var
    buffer : ansistring;
  begin
    buffer := ALine + sLineBreak;
    ADest.Write(buffer[1],Length(buffer));
  end;

var
  i, k : Integer;
  p : ^TDecompositionIndexRec;
  cp : ^TUnicodeCodePoint;
  cp24 : UInt24;
  locLine : string;
begin
  AddLine('const');
  AddLine('  UC_DEC_BOOK_INDEX_LENGTH = ' + IntToStr(Length(ABook.Index)) + ';');
  AddLine('  UC_DEC_BOOK_DATA_LENGTH = ' + IntToStr(Length(ABook.CodePoints)) + ';');
  AddLine('type');
  AddLine('  TDecompositionIndexRec = packed record');
  AddLine('    StartPosition : Word;');
  AddLine('    Length        : Byte;');
  AddLine('  end;');
  AddLine('  TDecompositionBookRec = packed record');
  AddLine('    Index      : array[0..(UC_DEC_BOOK_INDEX_LENGTH-1)] of TDecompositionIndexRec;');
  AddLine('    CodePoints : array[0..(UC_DEC_BOOK_DATA_LENGTH-1)] of UInt24;');
  AddLine('  end;');
  AddLine('const');
  AddLine('  UC_DEC_BOOK_DATA : TDecompositionBookRec = (');
  p := @ABook.Index[0];
  AddLine('    Index : (// Index BEGIN');
  k := 0;
  locLine := '      ';
  for i := Low(ABook.Index) to High(ABook.Index) - 1 do begin
    locLine := locLine + '(StartPosition : ' + IntToStr(p^.StartPosition) + ';' +
               ' Length : ' + IntToStr(p^.Length)  + '), ';
    k := k + 1;
    if (k >= 2) then begin
      AddLine(locLine);
      locLine := '      ';
      k := 0;
    end;
    Inc(p);
  end;
  locLine := locLine + '(StartPosition : ' + IntToStr(p^.StartPosition) + ';' +
             ' Length : ' + IntToStr(p^.Length)  + ')';
  AddLine(locLine);
  AddLine('    ); // Index END');

  cp := @ABook.CodePoints[0];
  AddLine('    CodePoints : (// CodePoints BEGIN');
  k := 0;
  locLine := '      ';
  for i := Low(ABook.CodePoints) to High(ABook.CodePoints) - 1 do begin
    cp24 := cp^;
    locLine := locLine + Format('%s,',[UInt24ToStr(cp24,AEndian)]);
    Inc(k);
    if (k >= 16) then begin
      AddLine(locLine);
      k := 0;
      locLine := '      ';
    end;
    Inc(cp);
  end;
  cp24 := cp^;
  locLine := locLine + Format('%s',[UInt24ToStr(cp24,AEndian)]);
  AddLine(locLine);
  AddLine('    ); // CodePoints END');
  AddLine('  );' + sLineBreak);
end;

procedure GenerateOutBmpTable(
        ADest     : TStream;
  const AList : TDataLineRecArray
);
  procedure AddLine(const ALine : ansistring);
  var
    buffer : ansistring;
  begin
    buffer := ALine + sLineBreak;
    ADest.Write(buffer[1],Length(buffer));
  end;

var
  i, j : Integer;
  locLine : string;
  p : PDataLineRec;
begin
  AddLine('');
  //AddLine('  UC_PROP_REC_COUNT = ' + IntToStr(Length(APropList)) + ';');
  //AddLine('  UC_PROP_ARRAY : array[0..(UC_PROP_REC_COUNT-1)] of TUC_Prop = (');
  j := -1;
  p := @AList[0];
  for i := 0 to Length(AList) - 1 do begin
    if ((p^.LineType = 0) and (p^.CodePoint >$FFFF)) or
       (p^.StartCodePoint > $FFFF)
    then begin
      j := i;
      Break;
    end;
    Inc(p);
  end;
  if (j < 0) then
    exit;

  for i := j to Length(AList) - 2 do begin
    locLine := '    (PropID : ' + IntToStr(p^.PropID) + ';' +
               ' CodePoint : ' + IntToStr(p^.CodePoint) + ';' +
               ' RangeEnd : ' + IntToStr(p^.EndCodePoint) +  '),' ;
    AddLine(locLine);
    Inc(p);
  end;
  locLine := '    (PropID : ' + IntToStr(p^.PropID) + ';' +
             ' CodePoint : ' + IntToStr(p^.CodePoint) + ';' +
             ' RangeEnd : ' + IntToStr(p^.EndCodePoint) +  ')' ;
  AddLine(locLine);
  AddLine('  );' + sLineBreak);
end;

function Compress(const AData : TDataLineRecArray) : TDataLineRecArray;
var
  k, i, locResLen : Integer;
  q, p, pr : PDataLineRec;
  k_end : TUnicodeCodePoint;
begin
  locResLen := 1;
  SetLength(Result,Length(AData));
  FillChar(Result[0],Length(Result),#0);
  Result[0] := AData[0];
  q := @AData[0];
  k := 0;
  while (k < Length(AData)) do begin
    if (q^.LineType = 0) then
      k_end := q^.CodePoint
    else
      k_end := q^.EndCodePoint;
    if ((k+1) = Length(AData)) then begin
      i := k;
    end else begin
      p := @AData[k+1];
      i := k +1;
      while (i < (Length(AData) {- 1})) do begin
        if (p^.PropID <> q^.PropID) then begin
          i := i - 1;
          Break;
        end;
        if (p^.LineType = 0) then begin
          if (p^.CodePoint <> (k_end + 1)) then begin
            i := i - 1;
            Break;
          end;
          Inc(k_end);
        end else begin
          if (p^.StartCodePoint <> (k_end + 1)) then begin
            i := i - 1;
            Break;
          end;
          k_end := p^.EndCodePoint;
        end;
        Inc(i);
        Inc(p);
      end;
    end;
    {if (i = k) then begin
      Result[locResLen] := q^;
      Inc(locResLen);
    end else begin }
      p := @AData[i];
      pr := @Result[locResLen];
      pr^.PropID := q^.PropID;
      if (q^.LineType = 0) then
        pr^.StartCodePoint := q^.CodePoint
      else
        pr^.StartCodePoint := q^.StartCodePoint;
      pr^.LineType := 1;
      if (p^.LineType = 0) then
        pr^.EndCodePoint := p^.CodePoint
      else
        pr^.EndCodePoint := p^.EndCodePoint;
      Inc(locResLen);
    //end;
    k := i + 1;
    if (k = Length(AData)) then
      Break;
    q := @AData[k];
  end;
  SetLength(Result,locResLen);
end;

procedure ParseUCAFile(
      ADataAStream : TMemoryStream;
  var ABook        : TUCA_DataBook
);
const
  LINE_LENGTH        = 1024;
  DATA_LENGTH        = 25000;
var
  p : PAnsiChar;
  actualDataLen : Integer;
  bufferLength, bufferPos, lineLength, linePos : Integer;
  line : ansistring;

  function NextLine() : Boolean;
  var
    locOldPos : Integer;
    locOldPointer : PAnsiChar;
  begin
    Result := False;
    locOldPointer := p;
    locOldPos := bufferPos;
    while (bufferPos < bufferLength) and (p^ <> #10) do begin
      Inc(p);
      Inc(bufferPos);
    end;
    if (locOldPos = bufferPos) and (p^ = #10) then begin
      lineLength := 0;
      Inc(p);
      Inc(bufferPos);
      linePos := 1;
      Result := True;
    end else  if (locOldPos < bufferPos) then begin
      lineLength := (bufferPos - locOldPos) + 1;
      Move(locOldPointer^,line[1],lineLength);
      if (p^ = #10) then begin
        Dec(lineLength);
        Inc(p);
        Inc(bufferPos);
      end;
      linePos := 1;
      Result := True;
    end;
  end;

  procedure SkipSpace();
  begin
    while (linePos < lineLength) and (line[linePos] in [' ',#9]) do
      Inc(linePos);
  end;

  function NextToken() : ansistring;
  const C_SEPARATORS  = [';','#','.','[',']','*','@'];
  var
    k : Integer;
  begin
    SkipSpace();
    k := linePos;
    if (linePos <= lineLength) and (line[linePos] in C_SEPARATORS) then begin
      Result := line[linePos];
      Inc(linePos);
      exit;
    end;
    while (linePos <= lineLength) and not(line[linePos] in (C_SEPARATORS+[' '])) do
      Inc(linePos);
    if (linePos > k) then begin
      if (line[Min(linePos,lineLength)] in C_SEPARATORS) then
        Result := Copy(line,k,(linePos-k))
      else
        Result := Copy(line,k,(linePos-k+1));
      Result := Trim(Result);
    end else begin
      Result := '';
    end;
  end;

  procedure CheckToken(const AToken : string);
  var
    a, b : string;
  begin
    a := LowerCase(Trim(AToken));
    b := LowerCase(Trim(NextToken()));
    if (a <> b) then
      raise Exception.CreateFmt('Expected token "%s" but found "%s".',[a,b]);
  end;

  function ReadWeightBlock(var ADest : TUCA_WeightRec) : Boolean;
  var
    s :AnsiString;
    k : Integer;
  begin
    Result := False;
    s := NextToken();
    if (s <> '[') then
      exit;
    s := NextToken();
    if (s = '.') then
      ADest.Variable := False
    else begin
      if (s <> '*') then
        raise Exception.CreateFmt('Expected "%s" but found "%s".',['*',s]);
      ADest.Variable := True;
    end;
    ADest.Weights[0] := StrToInt('$'+NextToken());
    for k := 1 to 3 do begin
      CheckToken('.');
      ADest.Weights[k] := StrToInt('$'+NextToken());
    end;
    CheckToken(']');
    Result := True;
  end;

  procedure ParseHeaderVar();
  var
    s,ss : string;
    k : Integer;
  begin
    s := NextToken();
    if (s = 'version') then begin
      ss := '';
      while True do begin
        s := NextToken();
        if (s = '') then
          Break;
        ss := ss + s;
      end;
      ABook.Version := ss;
    end else if (s = 'variable') then begin
      if (s = 'blanked') then
        ABook.VariableWeight := ucaBlanked
      else if (s = 'non-ignorable') then
        ABook.VariableWeight := ucaNonIgnorable
      else if (s = 'shifted') then
        ABook.VariableWeight := ucaShifted
      else if (s = 'shift-trimmed') then
        ABook.VariableWeight := ucaShiftedTrimmed
      else if (s = 'ignoresp') then
        ABook.VariableWeight := ucaIgnoreSP
      else
        raise Exception.CreateFmt('Unknown "@variable" type : "%s".',[s]);
    end else if (s = 'backwards') or (s = 'forwards') then begin
      ss := s;
      s := NextToken();
      k := StrToInt(s);
      if (k < 1) or (k > 4) then
        raise Exception.CreateFmt('Invalid "%s" position : %d.',[ss,s]);
      ABook.Backwards[k] := (s = 'backwards');
    end;
  end;

  procedure ParseLine();
  var
    locData : ^TUCA_LineRec;
    s : ansistring;
    kc : Integer;
  begin
    if (Length(ABook.Lines) <= actualDataLen) then
      SetLength(ABook.Lines,Length(ABook.Lines)*2);
    locData := @ABook.Lines[actualDataLen];
    s := NextToken();
    if (s = '') or (s[1] = '#') then
      exit;
    if (s[1] = '@') then begin
      ParseHeaderVar();
      exit;
    end;
    SetLength(locData^.CodePoints,10);
    locData^.CodePoints[0] := StrToInt('$'+s);
    kc := 1;
    while True do begin
      s := Trim(NextToken());
      if (s = '') then
        exit;
      if (s = ';') then
        Break;
      locData^.CodePoints[kc] := StrToInt('$'+s);
      Inc(kc);
    end;
    if (kc = 0) then
      exit;
    SetLength(locData^.CodePoints,kc);
    SetLength(locData^.Weights,24);
    kc := 0;
    while ReadWeightBlock(locData^.Weights[kc]) do begin
      Inc(kc);
    end;
    SetLength(locData^.Weights,kc);
    Inc(actualDataLen);
  end;

  procedure Prepare();
  var
    k : Integer;
  begin
    ABook.VariableWeight := ucaShifted;
    for k := Low(ABook.Backwards) to High(ABook.Backwards) do
      ABook.Backwards[k] := False;
    SetLength(ABook.Lines,DATA_LENGTH);
    actualDataLen := 0;
    bufferLength := ADataAStream.Size;
    bufferPos := 0;
    p := ADataAStream.Memory;
    lineLength := 0;
    SetLength(line,LINE_LENGTH);
  end;

begin
  Prepare();
  while NextLine() do
    ParseLine();
  SetLength(ABook.Lines,actualDataLen);
end;

procedure Dump(X : array of TUnicodeCodePoint; const ATitle : string = '');
var
  i : Integer;
begin
  Write(ATitle, ' ');
  for i := 0 to Length(X) - 1 do
    Write(X[i],' ');
  WriteLn();
end;

function IsGreaterThan(A, B : PUCA_LineRec) : Integer;
var
  i, hb : Integer;
begin
  if (A=B) then
    exit(0);
  Result := 1;
  hb := Length(B^.CodePoints) - 1;
  for i := 0 to Length(A^.CodePoints) - 1 do begin
    if (i > hb) then
      exit;
    if (A^.CodePoints[i] < B^.CodePoints[i]) then
      exit(-1);
    if (A^.CodePoints[i] > B^.CodePoints[i]) then
      exit(1);
  end;
  if (Length(A^.CodePoints) = Length(B^.CodePoints)) then
    exit(0);
  exit(-1);
end;

Procedure QuickSort(var AList: TUCA_DataBookIndex; L, R : Longint;
                     ABook : PUCA_DataBook);
var
  I, J : Longint;
  P, Q : Integer;
begin
 repeat
   I := L;
   J := R;
   P := AList[ (L + R) div 2 ];
   repeat
     while IsGreaterThan(@ABook^.Lines[P], @ABook^.Lines[AList[i]]) > 0 do
       I := I + 1;
     while IsGreaterThan(@ABook^.Lines[P], @ABook^.Lines[AList[J]]) < 0 do
       J := J - 1;
     If I <= J then
     begin
       Q := AList[I];
       AList[I] := AList[J];
       AList[J] := Q;
       I := I + 1;
       J := J - 1;
     end;
   until I > J;
   // sort the smaller range recursively
   // sort the bigger range via the loop
   // Reasons: memory usage is O(log(n)) instead of O(n) and loop is faster than recursion
   if J - L < R - I then
   begin
     if L < J then
       QuickSort(AList, L, J, ABook);
     L := I;
   end
   else
   begin
     if I < R then
       QuickSort(AList, I, R, ABook);
     R := J;
   end;
 until L >= R;
end;

function CreateIndex(ABook : PUCA_DataBook) : TUCA_DataBookIndex;
var
  r : TUCA_DataBookIndex;
  i, c : Integer;
begin
  c := Length(ABook^.Lines);
  SetLength(r,c);
  for i := 0 to c - 1 do
    r[i] := i;
  QuickSort(r,0,c-1,ABook);
  Result := r;
end;

function ConstructContextTree(
  const AContext : PUCA_LineContextRec;
  var   ADestBuffer;
  const ADestBufferLength : Cardinal
) : PUCA_PropItemContextTreeRec;forward;
function ConstructItem(
        AItem         : PUCA_PropItemRec;
        ACodePoint    : Cardinal;
        AValid        : Byte;
        AChildCount   : Byte;
  const AWeights      : array of TUCA_WeightRec;
  const AStoreCP      : Boolean;
  const AContext      : PUCA_LineContextRec;
  const ADeleted      : Boolean
) : Cardinal;
var
  i : Integer;
  p : PUCA_PropItemRec;
  pw : PUCA_PropWeights;
  pb : PByte;
  hasContext : Boolean;
  contextTree : PUCA_PropItemContextTreeRec;
  wl : Integer;
begin
  p := AItem;
  p^.Size := 0;
  p^.Flags := 0;
  p^.WeightLength := 0;
  SetBit(p^.Flags,AItem^.FLAG_VALID,(AValid <> 0));
  p^.ChildCount := AChildCount;
  hasContext := (AContext <> nil) and (Length(AContext^.Data) > 0);
  if hasContext then
    wl := 0
  else
    wl := Length(AWeights);
  p^.WeightLength := wl;
  if (wl = 0) then begin
    Result := SizeOf(TUCA_PropItemRec);
    if ADeleted then
      SetBit(AItem^.Flags,AItem^.FLAG_DELETION,True);
  end else begin
    Result := SizeOf(TUCA_PropItemRec) + (wl*SizeOf(TUCA_PropWeights));
    pb := PByte(PtrUInt(p) + SizeOf(TUCA_PropItemRec));
    Unaligned(PWord(pb)^) := AWeights[0].Weights[0];
    pb := pb + 2;
    if (AWeights[0].Weights[1] > High(Byte)) then begin
      Unaligned(PWord(pb)^) := AWeights[0].Weights[1];
      pb := pb + 2;
    end else begin
      SetBit(p^.Flags,p^.FLAG_COMPRESS_WEIGHT_1,True);
      pb^ := AWeights[0].Weights[1];
      pb := pb + 1;
      Result := Result - 1;
    end;
    if (AWeights[0].Weights[2] > High(Byte)) then begin
      Unaligned(PWord(pb)^) := AWeights[0].Weights[2];
      pb := pb + 2;
    end else begin
      SetBit(p^.Flags,p^.FLAG_COMPRESS_WEIGHT_2,True);
      pb^ := AWeights[0].Weights[2];
      pb := pb + 1;
      Result := Result - 1;
    end;
    pw := PUCA_PropWeights(pb);
    for i := 1 to wl - 1 do begin
      pw^.Weights[0] := AWeights[i].Weights[0];
      pw^.Weights[1] := AWeights[i].Weights[1];
      pw^.Weights[2] := AWeights[i].Weights[2];
      //pw^.Variable := BoolToByte(AWeights[i].Variable);
      Inc(pw);
    end;
  end;
  hasContext := (AContext <> nil) and (Length(AContext^.Data) > 0);
  if AStoreCP or hasContext then begin
    Unaligned(PUInt24(PtrUInt(AItem)+Result)^) := ACodePoint;
    Result := Result + SizeOf(UInt24);
    SetBit(AItem^.Flags,AItem^.FLAG_CODEPOINT,True);
  end;
  if hasContext then begin
    contextTree := ConstructContextTree(AContext,Unaligned(Pointer(PtrUInt(AItem)+Result)^),MaxInt);
    Result := Result + Cardinal(contextTree^.Size);
    SetBit(AItem^.Flags,AItem^.FLAG_CONTEXTUAL,True);
  end;
  p^.Size := Result;
end;

function CalcCharChildCount(
  const ASearchStartPos : Integer;
  const ALinePos        : Integer;
  const ABookLines      : PUCA_LineRec;
  const AMaxLength      : Integer;
  const ABookIndex      : TUCA_DataBookIndex;
  out   ALineCount      : Word
) : Byte;
var
  locLinePos : Integer;
  p : PUCA_LineRec;

  procedure IncP();
  begin
    Inc(locLinePos);
    p := @ABookLines[ABookIndex[locLinePos]];
  end;

var
  i, locTargetLen, locTargetBufferSize, r : Integer;
  locTarget : array[0..127] of Cardinal;
  locLastChar : Cardinal;
begin
  locLinePos := ALinePos;
  p := @ABookLines[ABookIndex[locLinePos]];
  locTargetLen := ASearchStartPos;
  locTargetBufferSize := (locTargetLen*SizeOf(Cardinal));
  Move(p^.CodePoints[0],locTarget[0],locTargetBufferSize);
  if (Length(p^.CodePoints) = ASearchStartPos) then begin
    r := 0;
    locLastChar := High(Cardinal);
  end else begin
    r := 1;
    locLastChar := p^.CodePoints[ASearchStartPos];
  end;
  i := 1;
  while (i < AMaxLength) do begin
    IncP();
    if (Length(p^.CodePoints) < locTargetLen) then
      Break;
    if not CompareMem(@locTarget[0],@p^.CodePoints[0],locTargetBufferSize) then
      Break;
    if (p^.CodePoints[ASearchStartPos] <> locLastChar) then begin
      Inc(r);
      locLastChar := p^.CodePoints[ASearchStartPos];
    end;
    Inc(i);
  end;
  ALineCount := i;
  Result := r;
end;

function BuildTrie(
  const ALinePos        : Integer;
  const ABookLines      : PUCA_LineRec;
  const AMaxLength      : Integer;
  const ABookIndex      : TUCA_DataBookIndex
) : PTrieNode;
var
  p : PUCA_LineRec;
  root : PTrieNode;
  ki, k, i : Integer;
  key : array of TKeyType;
begin
  k := ABookIndex[ALinePos];
  p := @ABookLines[k];
  if (Length(p^.CodePoints) = 1) then
    root := CreateNode(p^.CodePoints[0],k)
  else
    root := CreateNode(p^.CodePoints[0]);

  for i := ALinePos to ALinePos + AMaxLength - 1 do begin
    k := ABookIndex[i];
    p := @ABookLines[k];
    if (Length(p^.CodePoints) = 1) then begin
      InsertWord(root,p^.CodePoints[0],k);
    end else begin
      SetLength(key,Length(p^.CodePoints));
      for ki := 0 to Length(p^.CodePoints) - 1 do
        key[ki] := p^.CodePoints[ki];
      InsertWord(root,key,k);
    end;
  end;
  Result := root;
end;

function BoolToByte(AValue : Boolean): Byte;inline;
begin
  if AValue then
    Result := 1
  else
    Result := 0;
end;

function InternalConstructFromTrie(
  const ATrie  : PTrieNode;
  const AItem  : PUCA_PropItemRec;
  const ALines : PUCA_LineRec;
  const AStoreCp : Boolean
) : Cardinal;
var
  i : Integer;
  size : Cardinal;
  p : PUCA_PropItemRec;
  n : PTrieNode;
begin
  if (ATrie = nil) then
    exit(0);
  p := AItem;
  n := ATrie;
  if n^.DataNode then
    size := ConstructItem(p,n^.Key,1,n^.ChildCount,ALines[n^.Data].Weights,AStoreCp,@(ALines[n^.Data].Context),ALines[n^.Data].Deleted)
  else
    size := ConstructItem(p,n^.Key,0,n^.ChildCount,[],AStoreCp,nil,False);
  Result := size;
  if (n^.ChildCount > 0) then begin
    for i := 0 to n^.ChildCount - 1 do begin
      p := PUCA_PropItemRec(PtrUInt(p) + size);
      size := InternalConstructFromTrie(n^.Children[i],p,ALines,True);
      Result := Result + size;
    end;
  end;
  AItem^.Size := Result;
end;

function ConstructFromTrie(
  const ATrie  : PTrieNode;
  const AItem  : PUCA_PropItemRec;
  const ALines : PUCA_LineRec
) : Integer;
begin
  Result := InternalConstructFromTrie(ATrie,AItem,ALines,False);
end;

procedure MakeUCA_Props(
        ABook         : PUCA_DataBook;
  out   AProps        : PUCA_PropBook
);
var
  propIndexCount : Integer;

  procedure CapturePropIndex(AItem : PUCA_PropItemRec; ACodePoint : Cardinal);
  begin
    AProps^.Index[propIndexCount].CodePoint := ACodePoint;
    AProps^.Index[propIndexCount].Position := PtrUInt(AItem) - PtrUInt(AProps^.Items);
    propIndexCount := propIndexCount + 1;
  end;

var
  locIndex : TUCA_DataBookIndex;
  i, c, k, kc : Integer;
  p, p1, p2 : PUCA_PropItemRec;
  lines, pl1, pl2 : PUCA_LineRec;
  childCount, lineCount : Word;
  size : Cardinal;
  trieRoot : PTrieNode;
  MaxChildCount, MaxSize : Cardinal;
  childList : array of PUCA_PropItemRec;
begin
  locIndex := CreateIndex(ABook);
  i := Length(ABook^.Lines);
  i := 30 * i * (SizeOf(TUCA_PropItemRec) + SizeOf(TUCA_PropWeights));
  AProps := AllocMem(SizeOf(TUCA_DataBook));
  AProps^.ItemSize := i;
  AProps^.Items := AllocMem(i);
  propIndexCount := 0;
  SetLength(AProps^.Index,Length(ABook^.Lines));
  p := AProps^.Items;
  lines := @ABook^.Lines[0];
  c := Length(locIndex);
  i := 0;
  MaxChildCount := 0; MaxSize := 0;
  while (i < (c-1)) do begin
    pl1 := @lines[locIndex[i]];
    if not pl1^.Stored then begin
      i := i + 1;
      Continue;
    end;
    pl2 := @lines[locIndex[i+1]];
    if (pl1^.CodePoints[0] <> pl2^.CodePoints[0]) then begin
      if (Length(pl1^.CodePoints) = 1) then begin
        size := ConstructItem(p,pl1^.CodePoints[0],1,0,pl1^.Weights,False,@pl1^.Context,pl1^.Deleted);
        CapturePropIndex(p,pl1^.CodePoints[0]);
        p := PUCA_PropItemRec(PtrUInt(p) + size);
        if (size > MaxSize) then
          MaxSize := size;
      end else begin
        kc := Length(pl1^.CodePoints);
        SetLength(childList,kc);
        for k := 0 to kc - 2 do begin
          size := ConstructItem(p,pl1^.CodePoints[k],0,1,[],(k>0),nil,False);
          if (k = 0) then
            CapturePropIndex(p,pl1^.CodePoints[k]);
          childList[k] := p;
          p := PUCA_PropItemRec(PtrUInt(p) + size);
        end;
        size := ConstructItem(p,pl1^.CodePoints[kc-1],1,0,pl1^.Weights,True,@pl1^.Context,pl1^.Deleted);
        childList[kc-1] := p;
        p := PUCA_PropItemRec(PtrUInt(p) + size);
        for k := kc - 2 downto 0 do begin
          p1 := childList[k];
          p2 := childList[k+1];
          p1^.Size := p1^.Size + p2^.Size;
        end;
        if (p1^.Size > MaxSize) then
          MaxSize := p1^.Size;
      end;
      lineCount := 1;
    end else begin
      childCount := CalcCharChildCount(1,i,lines,c,locIndex,lineCount);
      if (childCount < 1) then
        raise Exception.CreateFmt('Expected "child count > 1" but found %d.',[childCount]);
      if (lineCount < 2) then
        raise Exception.CreateFmt('Expected "line count > 2" but found %d.',[lineCount]);
      if (childCount > MaxChildCount) then
        MaxChildCount := childCount;
      trieRoot := BuildTrie(i,lines,lineCount,locIndex);
      size := ConstructFromTrie(trieRoot,p,lines);
      CapturePropIndex(p,pl1^.CodePoints[0]);
      FreeNode(trieRoot);
      p := PUCA_PropItemRec(PtrUInt(p) + size);
      if (size > MaxSize) then
        MaxSize := size;
    end;
    i := i + lineCount;
  end;
  if (i = (c-1)) then begin
    pl1 := @lines[locIndex[i]];
    if (Length(pl1^.CodePoints) = 1) then begin
      size := ConstructItem(p,pl1^.CodePoints[0],1,0,pl1^.Weights,False,@pl1^.Context,pl1^.Deleted);
      CapturePropIndex(p,pl1^.CodePoints[0]);
      p := PUCA_PropItemRec(PtrUInt(p) + size);
      if (size > MaxSize) then
        MaxSize := size;
    end else begin
      kc := Length(pl1^.CodePoints);
      SetLength(childList,kc);
      for k := 0 to kc - 2 do begin
        size := ConstructItem(p,pl1^.CodePoints[k],0,1,[],(k>0),@pl1^.Context,pl1^.Deleted);
        if (k = 0) then
          CapturePropIndex(p,pl1^.CodePoints[0]);
        childList[k] := p;
        p := PUCA_PropItemRec(PtrUInt(p) + size);
      end;
      size := ConstructItem(p,pl1^.CodePoints[kc-1],1,0,pl1^.Weights,True,@pl1^.Context,pl1^.Deleted);
      childList[kc-1] := p;
      p := PUCA_PropItemRec(PtrUInt(p) + size);
      for i := kc - 2 downto 0 do begin
        p1 := childList[i];
        p2 := childList[i+1];
        p1^.Size := p1^.Size + p2^.Size;
      end;
      if (size > MaxSize) then
        MaxSize := size;
    end;
  end;
  c := Int64(PtrUInt(p)) - Int64(PtrUInt(AProps^.Items));
  ReAllocMem(AProps^.Items,c);
  AProps^.ItemSize := c;
  SetLength(AProps^.Index,propIndexCount);
  AProps^.ItemsOtherEndian := AllocMem(AProps^.ItemSize);
  ReverseFromNativeEndian(AProps^.Items,AProps^.ItemSize,AProps^.ItemsOtherEndian);

  k := 0;
  c := High(Word);
  for i := 0 to Length(ABook^.Lines) - 1 do begin
    if (Length(ABook^.Lines[i].Weights) > 0) then begin
      if (ABook^.Lines[i].Weights[0].Variable) then begin
        if (ABook^.Lines[i].Weights[0].Weights[0] > k) then
          k := ABook^.Lines[i].Weights[0].Weights[0];
        if (ABook^.Lines[i].Weights[0].Weights[0] < c) then
          c := ABook^.Lines[i].Weights[0].Weights[0];
      end;
    end;
  end;
  AProps^.VariableHighLimit := k;
  AProps^.VariableLowLimit := c;
end;

procedure FreeUcaBook(var ABook : PUCA_PropBook);
var
  p : PUCA_PropBook;
begin
  if (ABook = nil) then
    exit;
  p := ABook;
  ABook := nil;
  p^.Index := nil;
  FreeMem(p^.Items,p^.ItemSize);
  FreeMem(p,SizeOf(p^));
end;

function IndexOf(const ACodePoint : Cardinal; APropBook : PUCA_PropBook): Integer;overload;
var
  i : Integer;
begin
  for i := 0 to Length(APropBook^.Index) - 1 do begin
    if (ACodePoint = APropBook^.Index[i].CodePoint) then
      exit(i);
  end;
  Result := -1;
end;

type
  PucaBmpSecondTableItem = ^TucaBmpSecondTableItem;
function IndexOf(
  const AItem  : PucaBmpSecondTableItem;
  const ATable : TucaBmpSecondTable;
  const ATableActualLength : Integer
) : Integer;overload;
var
  i : Integer;
  p : PucaBmpSecondTableItem;
begin
  Result := -1;
  if (ATableActualLength > 0) then begin
    p := @ATable[0];
    for i := 0 to ATableActualLength - 1 do begin
      if CompareMem(p,AItem,SizeOf(TucaBmpSecondTableItem)) then begin
        Result := i;
        Break;
      end;
      Inc(p);
    end;
  end;
end;

procedure MakeUCA_BmpTables(
  var   AFirstTable   : TucaBmpFirstTable;
  var   ASecondTable  : TucaBmpSecondTable;
  const APropBook     : PUCA_PropBook
);
var
  locLowByte, locHighByte : Byte;
  locTableItem : TucaBmpSecondTableItem;
  locCP : TUnicodeCodePoint;
  i, locSecondActualLen : Integer;
  k : Integer;
begin
  SetLength(ASecondTable,120);
  locSecondActualLen := 0;
  for locHighByte := 0 to 255 do begin
    FillChar(locTableItem,SizeOf(locTableItem),#0);
    for locLowByte := 0 to 255 do begin
      locCP := (locHighByte * 256) + locLowByte;
      k := IndexOf(locCP,APropBook);
      if (k = -1) then
        k := 0
      else
        k := APropBook^.Index[k].Position + 1;
      locTableItem[locLowByte] := k;
    end;
    i := IndexOf(@locTableItem,ASecondTable,locSecondActualLen);
    if (i = -1) then begin
      if (locSecondActualLen = Length(ASecondTable)) then
        SetLength(ASecondTable,locSecondActualLen + 50);
      i := locSecondActualLen;
      ASecondTable[i] := locTableItem;
      Inc(locSecondActualLen);
    end;
    AFirstTable[locHighByte] := i;
  end;
  SetLength(ASecondTable,locSecondActualLen);
end;

function ToUCS4(const AHighS, ALowS : Word) : TUnicodeCodePoint; inline;
begin
  //copied from utf16toutf32
  Result := (UCS4Char(AHighS)-$d800) shl 10 + (UCS4Char(ALowS)-$dc00) + $10000;
end;

procedure FromUCS4(const AValue : TUnicodeCodePoint; var AHighS, ALowS : Word);
begin
  AHighS := Word((AValue - $10000) shr 10 + $d800);
  ALowS := Word((AValue - $10000) and $3ff + $dc00);
end;

type
  PucaOBmpSecondTableItem = ^TucaOBmpSecondTableItem;
function IndexOf(
  const AItem  : PucaOBmpSecondTableItem;
  const ATable : TucaOBmpSecondTable;
  const ATableActualLength : Integer
) : Integer;overload;
var
  i : Integer;
  p : PucaOBmpSecondTableItem;
begin
  Result := -1;
  if (ATableActualLength > 0) then begin
    p := @ATable[0];
    for i := 0 to ATableActualLength - 1 do begin
      if CompareMem(p,AItem,SizeOf(TucaOBmpSecondTableItem)) then begin
        Result := i;
        Break;
      end;
      Inc(p);
    end;
  end;
end;

procedure MakeUCA_OBmpTables(
  var   AFirstTable   : TucaOBmpFirstTable;
  var   ASecondTable  : TucaOBmpSecondTable;
  const APropBook     : PUCA_PropBook
);
var
  locLowByte, locHighByte : Word;
  locTableItem : TucaOBmpSecondTableItem;
  locCP : TUnicodeCodePoint;
  i, locSecondActualLen : Integer;
  k : Integer;
begin
  if (Length(ASecondTable) = 0) then
    SetLength(ASecondTable,2000);
  locSecondActualLen := 0;
  for locHighByte := 0 to HIGH_SURROGATE_COUNT - 1 do begin
    FillChar(locTableItem,SizeOf(locTableItem),#0);
    for locLowByte := 0 to LOW_SURROGATE_COUNT - 1 do begin
      locCP := ToUCS4(HIGH_SURROGATE_BEGIN + locHighByte,LOW_SURROGATE_BEGIN + locLowByte);
      k := IndexOf(locCP,APropBook);
      if (k = -1) then
        k := 0
      else
        k := APropBook^.Index[k].Position + 1;
      locTableItem[locLowByte] := k;
    end;
    i := IndexOf(@locTableItem,ASecondTable,locSecondActualLen);
    if (i = -1) then begin
      if (locSecondActualLen = Length(ASecondTable)) then
        SetLength(ASecondTable,locSecondActualLen + 50);
      i := locSecondActualLen;
      ASecondTable[i] := locTableItem;
      Inc(locSecondActualLen);
    end;
    AFirstTable[locHighByte] := i;
  end;
  SetLength(ASecondTable,locSecondActualLen);
end;

function GetPropPosition(
  const AHighS,
        ALowS         : Word;
  const AFirstTable   : PucaOBmpFirstTable;
  const ASecondTable  : PucaOBmpSecondTable
): Integer;inline;overload;
begin
  Result := ASecondTable^[AFirstTable^[AHighS-HIGH_SURROGATE_BEGIN]][ALowS-LOW_SURROGATE_BEGIN] - 1;
end;

procedure GenerateUCA_Head(
  ADest  : TStream;
  ABook  : PUCA_DataBook;
  AProps : PUCA_PropBook
);

  procedure AddLine(const ALine : ansistring);
  var
    buffer : ansistring;
  begin
    buffer := ALine + sLineBreak;
    ADest.Write(buffer[1],Length(buffer));
  end;

begin
  AddLine('const');
  AddLine('  VERSION_STRING = ' + QuotedStr(ABook^.Version) + ';');
  AddLine('  VARIABLE_LOW_LIMIT = ' + IntToStr(AProps^.VariableLowLimit) + ';');
  AddLine('  VARIABLE_HIGH_LIMIT = ' + IntToStr(AProps^.VariableHighLimit) + ';');
  AddLine('  VARIABLE_WEIGHT = ' + IntToStr(Ord(ABook^.VariableWeight)) + ';');
  AddLine('  BACKWARDS_0 = ' + BoolToStr(ABook^.Backwards[0],'True','False') + ';');
  AddLine('  BACKWARDS_1 = ' + BoolToStr(ABook^.Backwards[1],'True','False') + ';');
  AddLine('  BACKWARDS_2 = ' + BoolToStr(ABook^.Backwards[2],'True','False') + ';');
  AddLine('  BACKWARDS_3 = ' + BoolToStr(ABook^.Backwards[3],'True','False') + ';');
  AddLine('  PROP_COUNT  = ' + IntToStr(Ord(AProps^.ItemSize)) + ';');

  AddLine('');
end;

procedure GenerateUCA_BmpTables(
        AStream,
        ANativeEndianStream,
        ANonNativeEndianStream : TStream;
  var   AFirstTable            : TucaBmpFirstTable;
  var   ASecondTable           : TucaBmpSecondTable
);

  procedure AddLine(AOut : TStream; const ALine : ansistring);
  var
    buffer : ansistring;
  begin
    buffer := ALine + sLineBreak;
    AOut.Write(buffer[1],Length(buffer));
  end;

var
  i, j, c : Integer;
  locLine : string;
  value : UInt24;
begin
  AddLine(AStream,'const');
  AddLine(AStream,'  UCA_TABLE_1 : array[0..255] of Byte = (');
  locLine := '';
  for i := Low(AFirstTable) to High(AFirstTable) - 1 do begin
    locLine := locLine + IntToStr(AFirstTable[i]) + ',';
    if (((i+1) mod 16) = 0) then begin
      locLine := '    ' + locLine;
      AddLine(AStream,locLine);
      locLine := '';
    end;
  end;
  locLine := locLine + IntToStr(AFirstTable[High(AFirstTable)]);
  locLine := '    ' + locLine;
  AddLine(AStream,locLine);
  AddLine(AStream,'  );' + sLineBreak);

  AddLine(ANativeEndianStream,'const');
  AddLine(ANativeEndianStream,'  UCA_TABLE_2 : array[0..(256*' + IntToStr(Length(ASecondTable)) +'-1)] of UInt24 =(');
  c := High(ASecondTable);
  for i := Low(ASecondTable) to c do begin
    locLine := '';
    for j := Low(TucaBmpSecondTableItem) to High(TucaBmpSecondTableItem) do begin
      value := ASecondTable[i][j];
      locLine := locLine + UInt24ToStr(value,ENDIAN_NATIVE) + ',';
      if (((j+1) mod 2) = 0) then begin
        if (i = c) and (j = 255) then
          Delete(locLine,Length(locLine),1);
        locLine := '    ' + locLine;
        AddLine(ANativeEndianStream,locLine);
        locLine := '';
      end;
    end;
  end;
  AddLine(ANativeEndianStream,'  );' + sLineBreak);

  AddLine(ANonNativeEndianStream,'const');
  AddLine(ANonNativeEndianStream,'  UCA_TABLE_2 : array[0..(256*' + IntToStr(Length(ASecondTable)) +'-1)] of UInt24 =(');
  c := High(ASecondTable);
  for i := Low(ASecondTable) to c do begin
    locLine := '';
    for j := Low(TucaBmpSecondTableItem) to High(TucaBmpSecondTableItem) do begin
      value := ASecondTable[i][j];
      locLine := locLine + UInt24ToStr(value,ENDIAN_NON_NATIVE) + ',';
      if (((j+1) mod 2) = 0) then begin
        if (i = c) and (j = 255) then
          Delete(locLine,Length(locLine),1);
        locLine := '    ' + locLine;
        AddLine(ANonNativeEndianStream,locLine);
        locLine := '';
      end;
    end;
  end;
  AddLine(ANonNativeEndianStream,'  );' + sLineBreak);
end;

procedure GenerateBinaryUCA_BmpTables(
        ANativeEndianStream,
        ANonNativeEndianStream : TStream;
  var   AFirstTable            : TucaBmpFirstTable;
  var   ASecondTable           : TucaBmpSecondTable
);
var
  i, j : Integer;
  value : UInt24;
begin
  ANativeEndianStream.Write(AFirstTable[0],Length(AFirstTable));
  ANonNativeEndianStream.Write(AFirstTable[0],Length(AFirstTable));
  for i := Low(ASecondTable) to High(ASecondTable) do begin
    for j := Low(TucaBmpSecondTableItem) to High(TucaBmpSecondTableItem) do begin
      value := ASecondTable[i][j];
      ANativeEndianStream.Write(value,SizeOf(value));
      ReverseBytes(value,SizeOf(value));
      ANonNativeEndianStream.Write(value,SizeOf(value));
    end;
  end;
end;

procedure GenerateUCA_PropTable(
// WARNING : files must be generated for each endianess (Little / Big)
        ADest     : TStream;
  const APropBook : PUCA_PropBook;
  const AEndian   : TEndianKind
);

  procedure AddLine(const ALine : ansistring);
  var
    buffer : ansistring;
  begin
    buffer := ALine + sLineBreak;
    ADest.Write(buffer[1],Length(buffer));
  end;

var
  i, c : Integer;
  locLine : string;
  p : PByte;
begin
  c := APropBook^.ItemSize;
  AddLine('const');
  AddLine('  UCA_PROPS : array[0..' + IntToStr(c-1) + '] of Byte = (');
  locLine := '';
  if (AEndian = ENDIAN_NATIVE) then
    p := PByte(APropBook^.Items)
  else
    p := PByte(APropBook^.ItemsOtherEndian);
  for i := 0 to c - 2 do begin
    locLine := locLine + IntToStr(p[i]) + ',';
    if (((i+1) mod 60) = 0) then begin
      locLine := '    ' + locLine;
      AddLine(locLine);
      locLine := '';
    end;
  end;
  locLine := locLine + IntToStr(p[c-1]);
  locLine := '    ' + locLine;
  AddLine(locLine);
  AddLine('  );' + sLineBreak);
end;

procedure GenerateBinaryUCA_PropTable(
// WARNING : files must be generated for each endianess (Little / Big)
        ANativeEndianStream,
        ANonNativeEndianStream : TStream;
  const APropBook              : PUCA_PropBook
);
begin
  ANativeEndianStream.Write(APropBook^.Items^,APropBook^.ItemSize);
  ANonNativeEndianStream.Write(APropBook^.ItemsOtherEndian^,APropBook^.ItemSize);
end;

procedure GenerateUCA_OBmpTables(
        AStream,
        ANativeEndianStream,
        ANonNativeEndianStream : TStream;
  var   AFirstTable            : TucaOBmpFirstTable;
  var   ASecondTable           : TucaOBmpSecondTable
);

  procedure AddLine(AOut : TStream; const ALine : ansistring);
  var
    buffer : ansistring;
  begin
    buffer := ALine + sLineBreak;
    AOut.Write(buffer[1],Length(buffer));
  end;

var
  i, j, c : Integer;
  locLine : string;
  value : UInt24;
begin
  AddLine(AStream,'const');
  AddLine(AStream,'  UCAO_TABLE_1 : array[0..' + IntToStr(HIGH_SURROGATE_COUNT-1) + '] of Word = (');
  locLine := '';
  for i := Low(AFirstTable) to High(AFirstTable) - 1 do begin
    locLine := locLine + IntToStr(AFirstTable[i]) + ',';
    if (((i+1) mod 16) = 0) then begin
      locLine := '    ' + locLine;
      AddLine(AStream,locLine);
      locLine := '';
    end;
  end;
  locLine := locLine + IntToStr(AFirstTable[High(AFirstTable)]);
  locLine := '    ' + locLine;
  AddLine(AStream,locLine);
  AddLine(AStream,'  );' + sLineBreak);

  AddLine(ANativeEndianStream,'  UCAO_TABLE_2 : array[0..('+IntToStr(LOW_SURROGATE_COUNT)+'*' + IntToStr(Length(ASecondTable)) +'-1)] of UInt24 =(');
  c := High(ASecondTable);
  for i := Low(ASecondTable) to c do begin
    locLine := '';
    for j := Low(TucaOBmpSecondTableItem) to High(TucaOBmpSecondTableItem) do begin
      value := ASecondTable[i][j];
      locLine := locLine + UInt24ToStr(value,ENDIAN_NATIVE) + ',';
      if (((j+1) mod 2) = 0) then begin
        if (i = c) and (j = High(TucaOBmpSecondTableItem)) then
          Delete(locLine,Length(locLine),1);
        locLine := '    ' + locLine;
        AddLine(ANativeEndianStream,locLine);
        locLine := '';
      end;
    end;
  end;
  AddLine(ANativeEndianStream,'  );' + sLineBreak);

  AddLine(ANonNativeEndianStream,'  UCAO_TABLE_2 : array[0..('+IntToStr(LOW_SURROGATE_COUNT)+'*' + IntToStr(Length(ASecondTable)) +'-1)] of UInt24 =(');
  c := High(ASecondTable);
  for i := Low(ASecondTable) to c do begin
    locLine := '';
    for j := Low(TucaOBmpSecondTableItem) to High(TucaOBmpSecondTableItem) do begin
      value := ASecondTable[i][j];
      locLine := locLine + UInt24ToStr(value,ENDIAN_NON_NATIVE) + ',';
      if (((j+1) mod 2) = 0) then begin
        if (i = c) and (j = High(TucaOBmpSecondTableItem)) then
          Delete(locLine,Length(locLine),1);
        locLine := '    ' + locLine;
        AddLine(ANonNativeEndianStream,locLine);
        locLine := '';
      end;
    end;
  end;
  AddLine(ANonNativeEndianStream,'  );' + sLineBreak);
end;

procedure GenerateBinaryUCA_OBmpTables(
        ANativeEndianStream,
        ANonNativeEndianStream : TStream;
  var   AFirstTable            : TucaOBmpFirstTable;
  var   ASecondTable           : TucaOBmpSecondTable
);
var
  i, j : Integer;
  locLine : string;
  wordValue : Word;
  value : UInt24;
begin
  for i := Low(AFirstTable) to High(AFirstTable) do begin
    wordValue := AFirstTable[i];
    ANativeEndianStream.Write(wordValue,SizeOf(wordValue));
    ReverseBytes(wordValue,SizeOf(wordValue));
    ANonNativeEndianStream.Write(wordValue,SizeOf(wordValue));
  end;

  for i := Low(ASecondTable) to High(ASecondTable) do begin
    for j := Low(TucaOBmpSecondTableItem) to High(TucaOBmpSecondTableItem) do begin
      value := ASecondTable[i][j];
      ANativeEndianStream.Write(value,SizeOf(value));
      ReverseBytes(value,SizeOf(value));
      ANonNativeEndianStream.Write(value,SizeOf(value));
    end;
  end;
end;

type
  POBmpSecondTableItem = ^TOBmpSecondTableItem;
function IndexOf(
  const AItem  : POBmpSecondTableItem;
  const ATable : TOBmpSecondTable;
  const ATableActualLength : Integer
) : Integer;overload;
var
  i : Integer;
  p : POBmpSecondTableItem;
begin
  Result := -1;
  if (ATableActualLength > 0) then begin
    p := @ATable[0];
    for i := 0 to ATableActualLength - 1 do begin
      if CompareMem(p,AItem,SizeOf(TOBmpSecondTableItem)) then begin
        Result := i;
        Break;
      end;
      Inc(p);
    end;
  end;
end;

procedure MakeOBmpTables(
  var   AFirstTable   : TOBmpFirstTable;
  var   ASecondTable  : TOBmpSecondTable;
  const ADataLineList : TDataLineRecArray
);
var
  locLowByte, locHighByte : Word;
  locTableItem : TOBmpSecondTableItem;
  locCP : TUnicodeCodePoint;
  i, locSecondActualLen : Integer;
begin
  SetLength(ASecondTable,2000);
  locSecondActualLen := 0;
  for locHighByte := 0 to HIGH_SURROGATE_COUNT - 1 do begin
    FillChar(locTableItem,SizeOf(locTableItem),#0);
    for locLowByte := 0 to LOW_SURROGATE_COUNT - 1 do begin
      locCP := ToUCS4(HIGH_SURROGATE_BEGIN + locHighByte,LOW_SURROGATE_BEGIN + locLowByte);
      locTableItem[locLowByte] := GetPropID(locCP,ADataLineList)// - 1;
    end;
    i := IndexOf(@locTableItem,ASecondTable,locSecondActualLen);
    if (i = -1) then begin
      if (locSecondActualLen = Length(ASecondTable)) then
        SetLength(ASecondTable,locSecondActualLen + 50);
      i := locSecondActualLen;
      ASecondTable[i] := locTableItem;
      Inc(locSecondActualLen);
    end;
    AFirstTable[locHighByte] := i;
  end;
  SetLength(ASecondTable,locSecondActualLen);
end;

type
  P3lvlOBmp3TableItem = ^T3lvlOBmp3TableItem;
function IndexOf(
  const AItem  : P3lvlOBmp3TableItem;
  const ATable : T3lvlOBmp3Table;
  const ATableActualLength : Integer
) : Integer;overload;
var
  i : Integer;
  p : P3lvlOBmp3TableItem;
begin
  Result := -1;
  if (ATableActualLength > 0) then begin
    p := @ATable[0];
    for i := 0 to ATableActualLength - 1 do begin
      if CompareMem(p,AItem,SizeOf(T3lvlOBmp3TableItem)) then begin
        Result := i;
        Break;
      end;
      Inc(p);
    end;
  end;
end;

type
  P3lvlOBmp2TableItem = ^T3lvlOBmp2TableItem;
function IndexOf(
  const AItem  : P3lvlOBmp2TableItem;
  const ATable : T3lvlOBmp2Table
) : Integer;overload;
var
  i : Integer;
  p : P3lvlOBmp2TableItem;
begin
  Result := -1;
  if (Length(ATable) > 0) then begin
    p := @ATable[0];
    for i := 0 to Length(ATable) - 1 do begin
      if CompareMem(p,AItem,SizeOf(T3lvlOBmp2TableItem)) then begin
        Result := i;
        Break;
      end;
      Inc(p);
    end;
  end;
end;
procedure MakeOBmpTables3Levels(
  var   AFirstTable   : T3lvlOBmp1Table;
  var   ASecondTable  : T3lvlOBmp2Table;
  var   AThirdTable  : T3lvlOBmp3Table;
  const ADataLineList : TDataLineRecArray
);
var
  locLowByte0, locLowByte1, locHighByte : Word;
  locTableItem2 : T3lvlOBmp2TableItem;
  locTableItem3 : T3lvlOBmp3TableItem;
  locCP : TUnicodeCodePoint;
  i, locThirdActualLen : Integer;
begin
  SetLength(AThirdTable,120);
  locThirdActualLen := 0;
  for locHighByte := 0 to 1023 do begin
    FillChar(locTableItem2,SizeOf(locTableItem2),#0);
    for locLowByte0 := 0 to 31 do begin
      FillChar(locTableItem3,SizeOf(locTableItem3),#0);
      for locLowByte1 := 0 to 31 do begin
        locCP := ToUCS4(HIGH_SURROGATE_BEGIN + locHighByte,LOW_SURROGATE_BEGIN + (locLowByte0*32) + locLowByte1);
        locTableItem3[locLowByte1] := GetPropID(locCP,ADataLineList);
      end;
      i := IndexOf(@locTableItem3,AThirdTable,locThirdActualLen);
      if (i = -1) then begin
        if (locThirdActualLen = Length(AThirdTable)) then
          SetLength(AThirdTable,locThirdActualLen + 50);
        i := locThirdActualLen;
        AThirdTable[i] := locTableItem3;
        Inc(locThirdActualLen);
      end;
      locTableItem2[locLowByte0] := i;
    end;
    i := IndexOf(@locTableItem2,ASecondTable);
    if (i = -1) then begin
      i := Length(ASecondTable);
      SetLength(ASecondTable,(i + 1));
      ASecondTable[i] := locTableItem2;
    end;
    AFirstTable[locHighByte] := i;
  end;
  SetLength(AThirdTable,locThirdActualLen);
end;

procedure GenerateOBmpTables(
        ADest : TStream;
  var   AFirstTable   : TOBmpFirstTable;
  var   ASecondTable  : TOBmpSecondTable
);
  procedure AddLine(const ALine : ansistring);
  var
    buffer : ansistring;
  begin
    buffer := ALine + sLineBreak;
    ADest.Write(buffer[1],Length(buffer));
  end;

var
  i, j, c : Integer;
  locLine : string;
begin
  AddLine('const');
  AddLine('  UCO_TABLE_1 : array[0..' + IntToStr(HIGH_SURROGATE_COUNT-1) + '] of Word = (');
  locLine := '';
  for i := Low(AFirstTable) to High(AFirstTable) - 1 do begin
    locLine := locLine + IntToStr(AFirstTable[i]) + ',';
    if (((i+1) mod 16) = 0) then begin
      locLine := '    ' + locLine;
      AddLine(locLine);
      locLine := '';
    end;
  end;
  locLine := locLine + IntToStr(AFirstTable[High(AFirstTable)]);
  locLine := '    ' + locLine;
  AddLine(locLine);
  AddLine('  );' + sLineBreak);

  AddLine('  UCO_TABLE_2 : array[0..('+IntToStr(LOW_SURROGATE_COUNT)+'*' + IntToStr(Length(ASecondTable)) +'-1)] of Word =(');
  c := High(ASecondTable);
  for i := Low(ASecondTable) to c do begin
    locLine := '';
    for j := Low(TOBmpSecondTableItem) to High(TOBmpSecondTableItem) do begin
      locLine := locLine + IntToStr(ASecondTable[i][j]) + ',';
      if (((j+1) mod 16) = 0) then begin
        if (i = c) and (j = High(TOBmpSecondTableItem)) then
          Delete(locLine,Length(locLine),1);
        locLine := '    ' + locLine;
        AddLine(locLine);
        locLine := '';
      end;
    end;
  end;
  AddLine('  );' + sLineBreak);
end;


//----------------------------------
procedure Generate3lvlOBmpTables(
        ADest : TStream;
  var   AFirstTable   : T3lvlOBmp1Table;
  var   ASecondTable  : T3lvlOBmp2Table;
  var   AThirdTable   : T3lvlOBmp3Table
);

  procedure AddLine(const ALine : ansistring);
  var
    buffer : ansistring;
  begin
    buffer := ALine + sLineBreak;
    ADest.Write(buffer[1],Length(buffer));
  end;

var
  i, j, c : Integer;
  locLine : string;
begin
  AddLine('const');
  AddLine('  UCO_TABLE_1 : array[0..1023] of Word = (');
  locLine := '';
  for i := Low(AFirstTable) to High(AFirstTable) - 1 do begin
    locLine := locLine + IntToStr(AFirstTable[i]) + ',';
    if (((i+1) mod 16) = 0) then begin
      locLine := '    ' + locLine;
      AddLine(locLine);
      locLine := '';
    end;
  end;
  locLine := locLine + IntToStr(AFirstTable[High(AFirstTable)]);
  locLine := '    ' + locLine;
  AddLine(locLine);
  AddLine('  );' + sLineBreak);

  AddLine('  UCO_TABLE_2 : array[0..' + IntToStr(Length(ASecondTable)-1) +'] of array[0..31] of Word = (');
  c := High(ASecondTable);
  for i := Low(ASecondTable) to c do begin
    locLine := '(';
    for j := Low(T3lvlOBmp2TableItem) to High(T3lvlOBmp2TableItem) do
      locLine := locLine + IntToStr(ASecondTable[i][j]) + ',';
    Delete(locLine,Length(locLine),1);
    locLine := '    ' + locLine + ')';
    if (i < c) then
      locLine := locLine + ',';
    AddLine(locLine);
  end;
  AddLine('  );' + sLineBreak);

  AddLine('  UCO_TABLE_3 : array[0..' + IntToStr(Length(AThirdTable)-1) +'] of array[0..31] of Word = (');
  c := High(AThirdTable);
  for i := Low(AThirdTable) to c do begin
    locLine := '(';
    for j := Low(T3lvlOBmp3TableItem) to High(T3lvlOBmp3TableItem) do
      locLine := locLine + IntToStr(AThirdTable[i][j]) + ',';
    Delete(locLine,Length(locLine),1);
    locLine := '    ' + locLine + ')';
    if (i < c) then
      locLine := locLine + ',';
    AddLine(locLine);
  end;
  AddLine('  );' + sLineBreak);
end;

function GetProp(
  const AHighS,
        ALowS         : Word;
  const AProps        : TPropRecArray;
  var   AFirstTable   : TOBmpFirstTable;
  var   ASecondTable  : TOBmpSecondTable
): PPropRec;
begin
  Result := @AProps[ASecondTable[AFirstTable[AHighS-HIGH_SURROGATE_BEGIN]][ALowS-LOW_SURROGATE_BEGIN]];
end;

function GetProp(
  const AHighS,
        ALowS         : Word;
  const AProps        : TPropRecArray;
  var   AFirstTable   : T3lvlOBmp1Table;
  var   ASecondTable  : T3lvlOBmp2Table;
  var   AThirdTable   : T3lvlOBmp3Table
): PPropRec;
begin
  Result := @AProps[AThirdTable[ASecondTable[AFirstTable[AHighS]][ALowS div 32]][ALowS mod 32]];
  //Result := @AProps[ASecondTable[AFirstTable[AHighS-HIGH_SURROGATE_BEGIN]][ALowS-LOW_SURROGATE_BEGIN]];
end;

{ TUCA_PropItemContextTreeRec }

function TUCA_PropItemContextTreeRec.GetData : PUCA_PropItemContextTreeNodeRec;
begin
  if (Size = 0) then
    Result := nil
  else
    Result := PUCA_PropItemContextTreeNodeRec(
                PtrUInt(
                  PtrUInt(@Self) + SizeOf(UInt24){Size}
                )
              );
end;

{ TUCA_LineContextRec }

procedure TUCA_LineContextRec.Clear;
begin
  Data := nil
end;

procedure TUCA_LineContextRec.Assign(ASource : TUCA_LineContextRec);
var
  c, i : Integer;
begin
  c := Length(ASource.Data);
  SetLength(Self.Data,c);
  for i := 0 to c-1 do
    Self.Data[i].Assign(ASource.Data[i]);
end;

function TUCA_LineContextRec.Clone : TUCA_LineContextRec;
begin
  Result.Clear();
  Result.Assign(Self);
end;

{ TUCA_LineContextItemRec }

procedure TUCA_LineContextItemRec.Clear();
begin
  CodePoints := nil;
  Weights := nil;
end;

procedure TUCA_LineContextItemRec.Assign(ASource : TUCA_LineContextItemRec);
begin
  Self.CodePoints := Copy(ASource.CodePoints);
  Self.Weights := Copy(ASource.Weights);
end;

function TUCA_LineContextItemRec.Clone() : TUCA_LineContextItemRec;
begin
  Result.Clear();
  Result.Assign(Self);
end;

{ TUCA_LineRec }

procedure TUCA_LineRec.Clear;
begin
  CodePoints := nil;
  Weights := nil;
  Deleted := False;
  Stored := False;
  Context.Clear();
end;

procedure TUCA_LineRec.Assign(ASource : TUCA_LineRec);
begin
  Self.CodePoints := Copy(ASource.CodePoints);
  Self.Weights := Copy(ASource.Weights);
  Self.Deleted := ASource.Deleted;
  Self.Stored := ASource.Stored;
  Self.Context.Assign(ASource.Context);
end;

function TUCA_LineRec.Clone : TUCA_LineRec;
begin
  Result.Clear();
  Result.Assign(Self);
end;

function TUCA_LineRec.HasContext() : Boolean;
begin
  Result := (Length(Context.Data) > 0);
end;

{ TPropRec }

function TPropRec.GetCategory: TUnicodeCategory;
begin
  Result := TUnicodeCategory((CategoryData and Byte($F8)) shr 3);
end;

procedure TPropRec.SetCategory(AValue: TUnicodeCategory);
var
  b : Byte;
begin
  b := Ord(AValue);
  b := b shl 3;
  CategoryData := CategoryData or b;
  //CategoryData := CategoryData or Byte(Byte(Ord(AValue)) shl 3);
end;

function TPropRec.GetWhiteSpace: Boolean;
begin
  Result := IsBitON(CategoryData,0);
end;

procedure TPropRec.SetWhiteSpace(AValue: Boolean);
begin
  SetBit(CategoryData,0,AValue);
end;

function TPropRec.GetHangulSyllable: Boolean;
begin
  Result := IsBitON(CategoryData,1);
end;

procedure TPropRec.SetHangulSyllable(AValue: Boolean);
begin
   SetBit(CategoryData,1,AValue);
end;

{ TUCA_PropItemRec }

function TUCA_PropItemRec.GetWeightSize : Word;
var
  c : Integer;
begin
  c := WeightLength;
  if (c = 0) then
    exit(0);
  Result := c*SizeOf(TUCA_PropWeights);
  if IsWeightCompress_1() then
    Result := Result - 1;
  if IsWeightCompress_2() then
    Result := Result - 1;
end;

function TUCA_PropItemRec.HasCodePoint(): Boolean;
begin
  Result := IsBitON(Flags,FLAG_CODEPOINT);
end;

procedure TUCA_PropItemRec.GetWeightArray(ADest: PUCA_PropWeights);
var
  c : Integer;
  p : PByte;
  pd : PUCA_PropWeights;
begin
  c := WeightLength;
  p := PByte(PtrUInt(@Self) + SizeOf(TUCA_PropItemRec));
  pd := ADest;
  pd^.Weights[0] := PWord(p)^;
  p := p + 2;
  if not IsWeightCompress_1() then begin
    pd^.Weights[1] := PWord(p)^;
    p := p + 2;
  end else begin
    pd^.Weights[1] := p^;
    p := p + 1;
  end;
  if not IsWeightCompress_2() then begin
    pd^.Weights[2] := PWord(p)^;
    p := p + 2;
  end else begin
    pd^.Weights[2] := p^;
    p := p + 1;
  end;
  if (c > 1) then
    Move(p^, (pd+1)^, ((c-1)*SizeOf(TUCA_PropWeights)));
end;

function TUCA_PropItemRec.GetSelfOnlySize() : Cardinal;
begin
  Result := SizeOf(TUCA_PropItemRec);
  if (WeightLength > 0) then begin
    Result := Result + (WeightLength * Sizeof(TUCA_PropWeights));
    if IsWeightCompress_1() then
      Result := Result - 1;
    if IsWeightCompress_2() then
      Result := Result - 1;
  end;
  if HasCodePoint() then
    Result := Result + SizeOf(UInt24);
  if Contextual then
    Result := Result + Cardinal(GetContext()^.Size);
end;

procedure TUCA_PropItemRec.SetContextual(AValue : Boolean);
begin
  SetBit(Flags,FLAG_CONTEXTUAL,AValue);
end;

function TUCA_PropItemRec.GetContextual : Boolean;
begin
  Result := IsBitON(Flags,FLAG_CONTEXTUAL);
end;

function TUCA_PropItemRec.GetContext() : PUCA_PropItemContextTreeRec;
var
  p : PtrUInt;
begin
  if not Contextual then
    exit(nil);
  p := PtrUInt(@Self) + SizeOf(TUCA_PropItemRec);
  if IsBitON(Flags,FLAG_CODEPOINT) then
    p := p + SizeOf(UInt24);
  Result := PUCA_PropItemContextTreeRec(p);
end;

procedure TUCA_PropItemRec.SetDeleted(AValue: Boolean);
begin
  SetBit(Flags,FLAG_DELETION,AValue);
end;

function TUCA_PropItemRec.IsDeleted: Boolean;
begin
  Result := IsBitON(Flags,FLAG_DELETION);
end;

function TUCA_PropItemRec.IsValid() : Boolean;
begin
  Result := IsBitON(Flags,FLAG_VALID);
end;

function TUCA_PropItemRec.IsWeightCompress_1 : Boolean;
begin
  Result := IsBitON(Flags,FLAG_COMPRESS_WEIGHT_1);
end;

function TUCA_PropItemRec.IsWeightCompress_2 : Boolean;
begin
  Result := IsBitON(Flags,FLAG_COMPRESS_WEIGHT_2);
end;

function TUCA_PropItemRec.GetCodePoint: UInt24;
begin
  if HasCodePoint() then begin
    if Contextual then
      Result := PUInt24(
                  PtrUInt(@Self) + Self.GetSelfOnlySize()- SizeOf(UInt24) -
                  Cardinal(GetContext()^.Size)
                )^
    else
      Result := PUInt24(PtrUInt(@Self) + Self.GetSelfOnlySize() - SizeOf(UInt24))^
  end else begin
    raise Exception.Create('TUCA_PropItemRec.GetCodePoint : "No code point available."');
  end
end;

function avl_CompareCodePoints(Item1, Item2: Pointer): Integer;
var
  a, b : PUCA_LineContextItemRec;
  i, hb : Integer;
begin
  if (Item1 = Item2) then
    exit(0);
  if (Item1 = nil) then
    exit(-1);
  if (Item2 = nil) then
    exit(1);
  a := Item1;
  b := Item2;
  if (a^.CodePoints = b^.CodePoints) then
    exit(0);
  Result := 1;
  hb := Length(b^.CodePoints) - 1;
  for i := 0 to Length(a^.CodePoints) - 1 do begin
    if (i > hb) then
      exit;
    if (a^.CodePoints[i] < b^.CodePoints[i]) then
      exit(-1);
    if (a^.CodePoints[i] > b^.CodePoints[i]) then
      exit(1);
  end;
  if (Length(a^.CodePoints) = Length(b^.CodePoints)) then
    exit(0);
  exit(-1);
end;

function ConstructAvlContextTree(AContext : PUCA_LineContextRec) : TAVLTree;
var
  r : TAVLTree;
  i : Integer;
begin
  r := TAVLTree.Create(@avl_CompareCodePoints);
  try
    for i := 0 to Length(AContext^.Data) - 1 do
      r.Add(@AContext^.Data[i]);
    Result := r;
  except
    FreeAndNil(r);
    raise;
  end;
end;

function ConstructContextTree(
  const AContext : PUCA_LineContextRec;
  var   ADestBuffer;
  const ADestBufferLength : Cardinal
) : PUCA_PropItemContextTreeRec;

  function CalcItemOnlySize(AItem : TAVLTreeNode) : Cardinal;
  var
    kitem : PUCA_LineContextItemRec;
  begin
    if (AItem = nil) then
      exit(0);
    kitem := AItem.Data;
    Result := SizeOf(PUCA_PropItemContextTreeNodeRec^.Left) +
              SizeOf(PUCA_PropItemContextTreeNodeRec^.Right) +
              SizeOf(PUCA_PropItemContextRec^.CodePointCount) +
                (Length(kitem^.CodePoints)*SizeOf(UInt24)) +
              SizeOf(PUCA_PropItemContextRec^.WeightCount) +
                (Length(kitem^.Weights)*SizeOf(TUCA_PropWeights));
  end;

  function CalcItemSize(AItem : TAVLTreeNode) : Cardinal;
  begin
    if (AItem = nil) then
      exit(0);
    Result := CalcItemOnlySize(AItem);
    if (AItem.Left <> nil) then
      Result := Result + CalcItemSize(AItem.Left);
    if (AItem.Right <> nil) then
      Result := Result + CalcItemSize(AItem.Right);
  end;

  function CalcSize(AData : TAVLTree) : Cardinal;
  begin
    Result := SizeOf(PUCA_PropItemContextTreeRec^.Size) + CalcItemSize(AData.Root);
  end;

  function ConstructItem(ASource : TAVLTreeNode; ADest : PUCA_PropItemContextTreeNodeRec) : Cardinal;
  var
    k : Integer;
    kitem : PUCA_LineContextItemRec;
    kpcp : PUInt24;
    kpw : PUCA_PropWeights;
    pextra : PtrUInt;
    pnext : PUCA_PropItemContextTreeNodeRec;
  begin
    kitem := ASource.Data;
    ADest^.Data.CodePointCount := Length(kitem^.CodePoints);
    ADest^.Data.WeightCount := Length(kitem^.Weights);
    pextra := PtrUInt(ADest)+SizeOf(ADest^.Left)+SizeOf(ADest^.Right)+
              SizeOf(ADest^.Data.CodePointCount)+SizeOf(ADest^.Data.WeightCount);
    if (ADest^.Data.CodePointCount > 0) then begin
      kpcp := PUInt24(pextra);
      for k := 0 to ADest^.Data.CodePointCount - 1 do begin
        kpcp^ := kitem^.CodePoints[k];
        Inc(kpcp);
      end;
    end;
    if (ADest^.Data.WeightCount > 0) then begin
      kpw := PUCA_PropWeights(pextra + (ADest^.Data.CodePointCount*SizeOf(UInt24)));
      for k := 0 to ADest^.Data.WeightCount - 1 do begin
        kpw^.Weights[0] := kitem^.Weights[k].Weights[0];
        kpw^.Weights[1] := kitem^.Weights[k].Weights[1];
        kpw^.Weights[2] := kitem^.Weights[k].Weights[2];
        Inc(kpw);
      end;
    end;
    Result := CalcItemOnlySize(ASource);
    if (ASource.Left <> nil) then begin
      pnext := PUCA_PropItemContextTreeNodeRec(PtrUInt(ADest) + Result);
      ADest^.Left := Result;
      Result := Result + ConstructItem(ASource.Left,pnext);
    end else begin
      ADest^.Left := 0;
    end;
    if (ASource.Right <> nil) then begin
      pnext := PUCA_PropItemContextTreeNodeRec(PtrUInt(ADest) + Result);
      ADest^.Right := Result;
      Result := Result + ConstructItem(ASource.Right,pnext);
    end else begin
      ADest^.Right := 0;
    end;
  end;

var
  c : PtrUInt;
  r : PUCA_PropItemContextTreeRec;
  p : PUCA_PropItemContextTreeNodeRec;
  tempTree : TAVLTree;
begin
  tempTree := ConstructAvlContextTree(AContext);
  try
    c := CalcSize(tempTree);
    if (ADestBufferLength > 0) and (c > ADestBufferLength) then
      raise Exception.Create(SInsufficientMemoryBuffer);
    r := @ADestBuffer;
    r^.Size := c;
    p := PUCA_PropItemContextTreeNodeRec(PtrUInt(r) + SizeOf(r^.Size));
    ConstructItem(tempTree.Root,p);
  finally
    tempTree.Free();
  end;
  Result := r;
end;

procedure ReverseRecordBytes(var AItem : TSerializedCollationHeader);
begin
  ReverseBytes(AItem.BMP_Table1Length,SizeOf(AItem.BMP_Table1Length));
  ReverseBytes(AItem.BMP_Table2Length,SizeOf(AItem.BMP_Table2Length));
  ReverseBytes(AItem.OBMP_Table1Length,SizeOf(AItem.OBMP_Table1Length));
  ReverseBytes(AItem.OBMP_Table2Length,SizeOf(AItem.OBMP_Table2Length));
  ReverseBytes(AItem.PropCount,SizeOf(AItem.PropCount));
  ReverseBytes(AItem.VariableLowLimit,SizeOf(AItem.VariableLowLimit));
  ReverseBytes(AItem.VariableHighLimit,SizeOf(AItem.VariableHighLimit));
end;

procedure ReverseBytes(var AData; const ALength : Integer);
var
  i,j : PtrInt;
  c : Byte;
  p : PByte;
begin
  if (ALength = 1) then
    exit;
  p := @AData;
  j := ALength div 2;
  for i := 0 to Pred(j) do begin
    c := p[i];
    p[i] := p[(ALength - 1 ) - i];
    p[(ALength - 1 ) - i] := c;
  end;
end;

procedure ReverseArray(var AValue; const AArrayLength, AItemSize : PtrInt);
var
  p : PByte;
  i : PtrInt;
begin
  if ( AArrayLength > 0 ) and ( AItemSize > 1 ) then begin
    p := @AValue;
    for i := 0 to Pred(AArrayLength) do begin
      ReverseBytes(p^,AItemSize);
      Inc(p,AItemSize);
    end;
  end;
end;

procedure ReverseContextNodeFromNativeEndian(s, d : PUCA_PropItemContextTreeNodeRec);
var
  k : PtrUInt;
  p_s, p_d : PByte;
begin
  d^.Left := s^.Left;
    ReverseBytes(d^.Left,SizeOf(d^.Left));
  d^.Right := s^.Right;
    ReverseBytes(d^.Right,SizeOf(d^.Right));
  d^.Data.CodePointCount := s^.Data.CodePointCount;
    ReverseBytes(d^.Data.CodePointCount,SizeOf(d^.Data.CodePointCount));
  d^.Data.WeightCount := s^.Data.WeightCount;
    ReverseBytes(d^.Data.WeightCount,SizeOf(d^.Data.WeightCount));

  k := SizeOf(TUCA_PropItemContextTreeNodeRec);
  p_s := PByte(PtrUInt(s) + k);
  p_d := PByte(PtrUInt(d) + k);
  k := (s^.Data.CodePointCount*SizeOf(UInt24));
  Move(p_s^,p_d^, k);
    ReverseArray(p_d^,s^.Data.CodePointCount,SizeOf(UInt24));
  p_s := PByte(PtrUInt(p_s) + k);
  p_d := PByte(PtrUInt(p_d) + k);
  k := (s^.Data.WeightCount*SizeOf(TUCA_PropWeights));
  Move(p_s^,p_d^,k);
    ReverseArray(p_d^,(s^.Data.WeightCount*Length(TUCA_PropWeights.Weights)),SizeOf(TUCA_PropWeights.Weights[0]));
  if (s^.Left > 0) then
    ReverseContextNodeFromNativeEndian(
      PUCA_PropItemContextTreeNodeRec(PtrUInt(s) + s^.Left),
      PUCA_PropItemContextTreeNodeRec(PtrUInt(d) + s^.Left)
    );
  if (s^.Right > 0) then
    ReverseContextNodeFromNativeEndian(
      PUCA_PropItemContextTreeNodeRec(PtrUInt(s) + s^.Right),
      PUCA_PropItemContextTreeNodeRec(PtrUInt(d) + s^.Right)
    );
end;

procedure ReverseContextFromNativeEndian(s, d : PUCA_PropItemContextTreeRec);
var
  k : PtrUInt;
begin
  d^.Size := s^.Size;
    ReverseBytes(d^.Size,SizeOf(d^.Size));
  if (s^.Size = 0) then
    exit;
  k := SizeOf(s^.Size);
  ReverseContextNodeFromNativeEndian(
    PUCA_PropItemContextTreeNodeRec(PtrUInt(s)+k),
    PUCA_PropItemContextTreeNodeRec(PtrUInt(d)+k)
  );
end;

procedure ReverseFromNativeEndian(
  const AData    : PUCA_PropItemRec;
  const ADataLen : Cardinal;
  const ADest    : PUCA_PropItemRec
);
var
  s, d : PUCA_PropItemRec;
  sCtx, dCtx : PUCA_PropItemContextTreeRec;
  dataEnd : PtrUInt;
  k, i : PtrUInt;
  p_s, p_d : PByte;
  pw_s, pw_d : PUCA_PropWeights;
begin
  dataEnd := PtrUInt(AData) + ADataLen;
  s := AData;
  d := ADest;
  while True do begin
    d^.WeightLength := s^.WeightLength;
      ReverseBytes(d^.WeightLength,SizeOf(d^.WeightLength));
    d^.ChildCount := s^.ChildCount;
      ReverseBytes(d^.ChildCount,SizeOf(d^.ChildCount));
    d^.Size := s^.Size;
      ReverseBytes(d^.Size,SizeOf(d^.Size));
    d^.Flags := s^.Flags;
      ReverseBytes(d^.Flags,SizeOf(d^.Flags));
    if s^.Contextual then begin
      k := SizeOf(TUCA_PropItemRec);
      if s^.HasCodePoint() then
        k := k + SizeOf(UInt24);
      sCtx := PUCA_PropItemContextTreeRec(PtrUInt(s) + k);
      dCtx := PUCA_PropItemContextTreeRec(PtrUInt(d) + k);
      ReverseContextFromNativeEndian(sCtx,dCtx);
    end;
    if s^.HasCodePoint() then begin
      if s^.Contextual then
        k := s^.GetSelfOnlySize()- SizeOf(UInt24) - Cardinal(s^.GetContext()^.Size)
      else
        k := s^.GetSelfOnlySize() - SizeOf(UInt24);
      p_s := PByte(PtrUInt(s) + k);
      p_d := PByte(PtrUInt(d) + k);
      Unaligned(PUInt24(p_d)^) := Unaligned(PUInt24(p_s)^);
        ReverseBytes(p_d^,SizeOf(UInt24));
    end;
    if (s^.WeightLength > 0) then begin
      k := SizeOf(TUCA_PropItemRec);
      p_s := PByte(PtrUInt(s) + k);
      p_d := PByte(PtrUInt(d) + k);
      k := SizeOf(Word);
      Unaligned(PWord(p_d)^) := Unaligned(PWord(p_s)^);
        ReverseBytes(Unaligned(p_d^),k);
      p_s := PByte(PtrUInt(p_s) + k);
      p_d := PByte(PtrUInt(p_d) + k);
      if s^.IsWeightCompress_1() then begin
        k := SizeOf(Byte);
        PByte(p_d)^ := PByte(p_s)^;
      end else begin
        k := SizeOf(Word);
        Unaligned(PWord(p_d)^) := Unaligned(PWord(p_s)^);
      end;
      ReverseBytes(p_d^,k);
      p_s := PByte(PtrUInt(p_s) + k);
      p_d := PByte(PtrUInt(p_d) + k);
      if s^.IsWeightCompress_2() then begin
        k := SizeOf(Byte);
        PByte(p_d)^ := PByte(p_s)^;
      end else begin
        k := SizeOf(Word);
        Unaligned(PWord(p_d)^) := Unaligned(PWord(p_s)^);
      end;
      ReverseBytes(p_d^,k);
      if (s^.WeightLength > 1) then begin
        pw_s := PUCA_PropWeights(PtrUInt(p_s) + k);
        pw_d := PUCA_PropWeights(PtrUInt(p_d) + k);
        for i := 1 to s^.WeightLength - 1 do begin
          pw_d^.Weights[0] := pw_s^.Weights[0];
          pw_d^.Weights[1] := pw_s^.Weights[1];
          pw_d^.Weights[2] := pw_s^.Weights[2];
          ReverseArray(pw_d^,3,SizeOf(pw_s^.Weights[0]));
          Inc(pw_s);
          Inc(pw_d);
        end;
      end;
    end;
    k := s^.GetSelfOnlySize();
    s := PUCA_PropItemRec(PtrUInt(s)+k);
    d := PUCA_PropItemRec(PtrUInt(d)+k);
    if (PtrUInt(s) >= dataEnd) then
      Break;
  end;
  if ( (PtrUInt(s)-PtrUInt(AData)) <> (PtrUInt(d)-PtrUInt(ADest)) ) then
    raise Exception.CreateFmt('Read data length(%d) differs from written data length(%d).',[(PtrUInt(s)-PtrUInt(AData)), (PtrUInt(d)-PtrUInt(ADest))]);
end;
//------------------------------------------------------------------------------

procedure ReverseContextNodeToNativeEndian(s, d : PUCA_PropItemContextTreeNodeRec);
var
  k : PtrUInt;
  p_s, p_d : PByte;
begin
  d^.Left := s^.Left;
    ReverseBytes(d^.Left,SizeOf(d^.Left));
  d^.Right := s^.Right;
    ReverseBytes(d^.Right,SizeOf(d^.Right));
  d^.Data.CodePointCount := s^.Data.CodePointCount;
    ReverseBytes(d^.Data.CodePointCount,SizeOf(d^.Data.CodePointCount));
  d^.Data.WeightCount := s^.Data.WeightCount;
    ReverseBytes(d^.Data.WeightCount,SizeOf(d^.Data.WeightCount));

  k := SizeOf(TUCA_PropItemContextTreeNodeRec);
  p_s := PByte(PtrUInt(s) + k);
  p_d := PByte(PtrUInt(d) + k);
  k := (d^.Data.CodePointCount*SizeOf(UInt24));
  Move(p_s^,p_d^, k);
    ReverseArray(p_d^,d^.Data.CodePointCount,SizeOf(UInt24));
  p_s := PByte(PtrUInt(p_s) + k);
  p_d := PByte(PtrUInt(p_d) + k);
  k := (d^.Data.WeightCount*SizeOf(TUCA_PropWeights));
  Move(p_s^,p_d^,k);
    ReverseArray(p_d^,(d^.Data.WeightCount*Length(TUCA_PropWeights.Weights)),SizeOf(TUCA_PropWeights.Weights[0]));
  if (d^.Left > 0) then
    ReverseContextNodeToNativeEndian(
      PUCA_PropItemContextTreeNodeRec(PtrUInt(s) + d^.Left),
      PUCA_PropItemContextTreeNodeRec(PtrUInt(d) + d^.Left)
    );
  if (d^.Right > 0) then
    ReverseContextNodeToNativeEndian(
      PUCA_PropItemContextTreeNodeRec(PtrUInt(s) + d^.Right),
      PUCA_PropItemContextTreeNodeRec(PtrUInt(d) + d^.Right)
    );
end;

procedure ReverseContextToNativeEndian(s, d : PUCA_PropItemContextTreeRec);
var
  k : PtrUInt;
begin
  d^.Size := s^.Size;
    ReverseBytes(d^.Size,SizeOf(d^.Size));
  if (s^.Size = 0) then
    exit;
  k := SizeOf(s^.Size);
  ReverseContextNodeToNativeEndian(
    PUCA_PropItemContextTreeNodeRec(PtrUInt(s)+k),
    PUCA_PropItemContextTreeNodeRec(PtrUInt(d)+k)
  );
end;

procedure ReverseToNativeEndian(
  const AData    : PUCA_PropItemRec;
  const ADataLen : Cardinal;
  const ADest    : PUCA_PropItemRec
);
var
  s, d : PUCA_PropItemRec;
  sCtx, dCtx : PUCA_PropItemContextTreeRec;
  dataEnd : PtrUInt;
  k, i : PtrUInt;
  p_s, p_d : PByte;
  pw_s, pw_d : PUCA_PropWeights;
begin
  dataEnd := PtrUInt(AData) + ADataLen;
  s := AData;
  d := ADest;
  while True do begin
    d^.WeightLength := s^.WeightLength;
      ReverseBytes(d^.WeightLength,SizeOf(d^.WeightLength));
    d^.ChildCount := s^.ChildCount;
      ReverseBytes(d^.ChildCount,SizeOf(d^.ChildCount));
    d^.Size := s^.Size;
      ReverseBytes(d^.Size,SizeOf(d^.Size));
    d^.Flags := s^.Flags;
      ReverseBytes(d^.Flags,SizeOf(d^.Flags));
    if d^.Contextual then begin
      k := SizeOf(TUCA_PropItemRec);
      if d^.HasCodePoint() then
        k := k + SizeOf(UInt24);
      sCtx := PUCA_PropItemContextTreeRec(PtrUInt(s) + k);
      dCtx := PUCA_PropItemContextTreeRec(PtrUInt(d) + k);
      ReverseContextToNativeEndian(sCtx,dCtx);
    end;
    if d^.HasCodePoint() then begin
      if d^.Contextual then
        k := d^.GetSelfOnlySize()- SizeOf(UInt24) - Cardinal(d^.GetContext()^.Size)
      else
        k := d^.GetSelfOnlySize() - SizeOf(UInt24);
      p_s := PByte(PtrUInt(s) + k);
      p_d := PByte(PtrUInt(d) + k);
      Unaligned(PUInt24(p_d)^) := Unaligned(PUInt24(p_s)^);
        ReverseBytes(p_d^,SizeOf(UInt24));
    end;
    if (d^.WeightLength > 0) then begin
      k := SizeOf(TUCA_PropItemRec);
      p_s := PByte(PtrUInt(s) + k);
      p_d := PByte(PtrUInt(d) + k);
      k := SizeOf(Word);
      Unaligned(PWord(p_d)^) := Unaligned(PWord(p_s)^);
        ReverseBytes(p_d^,k);
      p_s := PByte(PtrUInt(p_s) + k);
      p_d := PByte(PtrUInt(p_d) + k);
      if d^.IsWeightCompress_1() then begin
        k := SizeOf(Byte);
        PByte(p_d)^ := PByte(p_s)^;
      end else begin
        k := SizeOf(Word);
        Unaligned(PWord(p_d)^) := Unaligned(PWord(p_s)^);
      end;
      ReverseBytes(p_d^,k);
      p_s := PByte(PtrUInt(p_s) + k);
      p_d := PByte(PtrUInt(p_d) + k);
      if d^.IsWeightCompress_2() then begin
        k := SizeOf(Byte);
        PByte(p_d)^ := PByte(p_s)^;
      end else begin
        k := SizeOf(Word);
        Unaligned(PWord(p_d)^) := Unaligned(PWord(p_s)^);
      end;
      ReverseBytes(p_d^,k);
      if (d^.WeightLength > 1) then begin
        pw_s := PUCA_PropWeights(PtrUInt(p_s) + k);
        pw_d := PUCA_PropWeights(PtrUInt(p_d) + k);
        for i := 1 to d^.WeightLength - 1 do begin
          pw_d^.Weights[0] := pw_s^.Weights[0];
          pw_d^.Weights[1] := pw_s^.Weights[1];
          pw_d^.Weights[2] := pw_s^.Weights[2];
          ReverseArray(pw_d^,3,SizeOf(pw_s^.Weights[0]));
          Inc(pw_s);
          Inc(pw_d);
        end;
      end;
    end;
    k := d^.GetSelfOnlySize();
    s := PUCA_PropItemRec(PtrUInt(s)+k);
    d := PUCA_PropItemRec(PtrUInt(d)+k);
    if (PtrUInt(s) >= dataEnd) then
      Break;
  end;
  if ( (PtrUInt(s)-PtrUInt(AData)) <> (PtrUInt(d)-PtrUInt(ADest)) ) then
    raise Exception.CreateFmt('Read data length(%d) differs from written data length(%d).',[(PtrUInt(s)-PtrUInt(AData)), (PtrUInt(d)-PtrUInt(ADest))]);
end;

procedure Check(const ACondition : Boolean; const AMsg : string);overload;
begin
  if not ACondition then
    raise Exception.Create(AMsg);
end;

procedure Check(
  const ACondition : Boolean;
  const AFormatMsg : string;
  const AArgs      : array of const
);overload;
begin
  Check(ACondition,Format(AFormatMsg,AArgs));
end;

procedure Check(const ACondition : Boolean);overload;
begin
  Check(ACondition,'Check failed.')
end;

procedure CompareWeights(a, b : PUCA_PropWeights; const ALength : Integer);
var
  i : Integer;
begin
  if (ALength > 0) then begin
    for i := 0 to ALength - 1 do begin
      Check(a[i].Weights[0]=b[i].Weights[0]);
      Check(a[i].Weights[1]=b[i].Weights[1]);
      Check(a[i].Weights[2]=b[i].Weights[2]);
    end;
  end;
end;

procedure CompareCodePoints(a, b : PUInt24; const ALength : Integer);
var
  i : Integer;
begin
  if (ALength > 0) then begin
    for i := 0 to ALength - 1 do
      Check(a[i]=b[i]);
  end;
end;

procedure CompareContextNode(AProp1, AProp2 : PUCA_PropItemContextTreeNodeRec);
var
  a, b : PUCA_PropItemContextTreeNodeRec;
  k : Cardinal;
begin
  if (AProp1=nil) then begin
    Check(AProp2=nil);
    exit;
  end;
  a := AProp1;
  b := AProp2;
  Check(a^.Left=b^.Left);
  Check(a^.Right=b^.Right);
  Check(a^.Data.CodePointCount=b^.Data.CodePointCount);
  Check(a^.Data.WeightCount=b^.Data.WeightCount);
  k := SizeOf(a^.Data);
  CompareCodePoints(
    PUInt24(PtrUInt(a)+k),
    PUInt24(PtrUInt(b)+k),
    a^.Data.CodePointCount
  );
  k := SizeOf(a^.Data)+ (a^.Data.CodePointCount*SizeOf(UInt24));
  CompareWeights(
    PUCA_PropWeights(PtrUInt(a)+k),
    PUCA_PropWeights(PtrUInt(b)+k),
    a^.Data.WeightCount
  );
  if (a^.Left > 0) then begin
    k := a^.Left;
    CompareContextNode(
      PUCA_PropItemContextTreeNodeRec(PtrUInt(a)+k),
      PUCA_PropItemContextTreeNodeRec(PtrUInt(b)+k)
    );
  end;
  if (a^.Right > 0) then begin
    k := a^.Right;
    CompareContextNode(
      PUCA_PropItemContextTreeNodeRec(PtrUInt(a)+k),
      PUCA_PropItemContextTreeNodeRec(PtrUInt(b)+k)
    );
  end;
end;

procedure CompareContext(AProp1, AProp2 : PUCA_PropItemContextTreeRec);
var
  a, b : PUCA_PropItemContextTreeNodeRec;
  k : Integer;
begin
  if (AProp1=nil) then begin
    Check(AProp2=nil);
    exit;
  end;
  Check(AProp1^.Size=AProp2^.Size);
  k := Cardinal(AProp1^.Size);
  a := PUCA_PropItemContextTreeNodeRec(PtrUInt(AProp1)+k);
  b := PUCA_PropItemContextTreeNodeRec(PtrUInt(AProp2)+k);
  CompareContextNode(a,b);
end;

procedure CompareProps(const AProp1, AProp2 : PUCA_PropItemRec; const ADataLen : Integer);
var
  a, b, pend : PUCA_PropItemRec;
  wa, wb : array of TUCA_PropWeights;
  k : Integer;
begin
  if (ADataLen <= 0) then
    exit;
  a := PUCA_PropItemRec(AProp1);
  b := PUCA_PropItemRec(AProp2);
  pend := PUCA_PropItemRec(PtrUInt(AProp1)+ADataLen);
  while (a<pend) do begin
    Check(a^.WeightLength=b^.WeightLength);
    Check(a^.ChildCount=b^.ChildCount);
    Check(a^.Size=b^.Size);
    Check(a^.Flags=b^.Flags);
    if a^.HasCodePoint() then
      Check(a^.CodePoint = b^.CodePoint);
    if (a^.WeightLength > 0) then begin
      k := a^.WeightLength;
      SetLength(wa,k);
      SetLength(wb,k);
      a^.GetWeightArray(@wa[0]);
      b^.GetWeightArray(@wb[0]);
      CompareWeights(@wa[0],@wb[0],k);
    end;
    if a^.Contextual then
      CompareContext(a^.GetContext(),b^.GetContext());
    Check(a^.GetSelfOnlySize()=b^.GetSelfOnlySize());
    k := a^.GetSelfOnlySize();
    a := PUCA_PropItemRec(PtrUInt(a)+k);
    b := PUCA_PropItemRec(PtrUInt(b)+k);
  end;
end;

initialization
  FS := DefaultFormatSettings;
  FS.DecimalSeparator := '.';

end.
