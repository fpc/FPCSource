unit character;

interface
{$ifndef VER2_4}
{$mode objfpc}
{$H+}
{$PACKENUM 1}
{$SCOPEDENUMS ON}

type
  // Unicode General Category
  TUnicodeCategory = (
    ucUppercaseLetter,             // Lu = Letter, uppercase
    ucLowercaseLetter,             // Ll = Letter, lowercase
    ucTitlecaseLetter,             // Lt = Letter, titlecase
    ucModifierLetter,              // Lm = Letter, modifier
    ucOtherLetter,                 // Lo = Letter, other
    
    ucNonSpacingMark,              // Mn = Mark, nonspacing
    ucCombiningMark,               // Mc = Mark, spacing combining
    ucEnclosingMark,               // Me = Mark, enclosing
    
    ucDecimalNumber,               // Nd = Number, decimal digit
    ucLetterNumber,                // Nl = Number, letter
    ucOtherNumber,                 // No = Number, other
    
    ucConnectPunctuation,          // Pc = Punctuation, connector
    ucDashPunctuation,             // Pd = Punctuation, dash
    ucOpenPunctuation,             // Ps = Punctuation, open
    ucClosePunctuation,            // Pe = Punctuation, close
    ucInitialPunctuation,          // Pi = Punctuation, initial quote (may behave like Ps or Pe depending on usage)
    ucFinalPunctuation,            // Pf = Punctuation, final quote (may behave like Ps or Pe depending on usage)
    ucOtherPunctuation,            // Po = Punctuation, other
    
    ucMathSymbol,                  // Sm = Symbol, math
    ucCurrencySymbol,              // Sc = Symbol, currency
    ucModifierSymbol,              // Sk = Symbol, modifier
    ucOtherSymbol,                 // So = Symbol, other
    
    ucSpaceSeparator,              // Zs = Separator, space
    ucLineSeparator,               // Zl = Separator, line
    ucParagraphSeparator,          // Zp = Separator, paragraph
    
    ucControl,                     // Cc = Other, control
    ucFormat,                      // Cf = Other, format
    ucSurrogate,                   // Cs = Other, surrogate
    ucPrivateUse,                  // Co = Other, private use
    ucUnassigned                   // Cn = Other, not assigned (including noncharacters)  
  );

  { TCharacter }

  TCharacter = class sealed
  public
    constructor Create;

    class function ConvertFromUtf32(AChar : UCS4Char) : UnicodeString; static;
    class function ConvertToUtf32(const AString : UnicodeString; AIndex : Integer) : UCS4Char; overload; static;
    class function ConvertToUtf32(const AString : UnicodeString; AIndex : Integer; out ACharLength : Integer) : UCS4Char; overload; static;
    class function ConvertToUtf32(const AHighSurrogate, ALowSurrogate : UnicodeChar) : UCS4Char; overload; static;
    
    class function GetNumericValue(AChar : UnicodeChar) : Double; static; overload;
    class function GetNumericValue(const AString : UnicodeString; AIndex : Integer) : Double; overload; static;
    
    class function GetUnicodeCategory(AChar : UnicodeChar) : TUnicodeCategory; overload; static;
    class function GetUnicodeCategory(const AString : UnicodeString; AIndex : Integer) : TUnicodeCategory; overload; static;
    
    class function IsControl(AChar : UnicodeChar) : Boolean; overload; static;
    class function IsControl(const AString : UnicodeString; AIndex : Integer) : Boolean; overload; static;
    
    class function IsDigit(AChar : UnicodeChar) : Boolean; overload; static;
    class function IsDigit(const AString : UnicodeString; AIndex : Integer) : Boolean; overload; static;                  
    
    class function IsSurrogate(AChar : UnicodeChar) : Boolean; overload; static;
    class function IsSurrogate(const AString : UnicodeString; AIndex : Integer) : Boolean; overload; static;  
    class function IsHighSurrogate(AChar : UnicodeChar) : Boolean; overload; static;
    class function IsHighSurrogate(const AString : UnicodeString; AIndex : Integer) : Boolean; overload; static;  
    class function IsLowSurrogate(AChar : UnicodeChar) : Boolean; overload; static;
    class function IsLowSurrogate(const AString : UnicodeString; AIndex : Integer) : Boolean; overload; static;
    class function IsSurrogatePair(const AHighSurrogate, ALowSurrogate : UnicodeChar) : Boolean; overload; static; inline;
    class function IsSurrogatePair(const AString : UnicodeString; AIndex : Integer) : Boolean; overload; static;
    
    class function IsLetter(AChar : UnicodeChar) : Boolean; overload; static;
    class function IsLetter(const AString : UnicodeString; AIndex : Integer) : Boolean; overload; static;  
    
    class function IsLetterOrDigit(AChar : UnicodeChar) : Boolean; overload; static;
    class function IsLetterOrDigit(const AString : UnicodeString; AIndex : Integer) : Boolean; overload; static;         
    
    class function IsLower(AChar : UnicodeChar) : Boolean; overload; static;
    class function IsLower(const AString : UnicodeString; AIndex : Integer) : Boolean; overload; static;

    class function IsNumber(AChar : UnicodeChar) : Boolean; overload; static;
    class function IsNumber(const AString : UnicodeString; AIndex : Integer) : Boolean; overload; static;

    class function IsPunctuation(AChar : UnicodeChar) : Boolean; overload; static;
    class function IsPunctuation(const AString : UnicodeString; AIndex : Integer) : Boolean; overload; static;

    class function IsSeparator(AChar : UnicodeChar) : Boolean; overload; static;
    class function IsSeparator(const AString : UnicodeString; AIndex : Integer) : Boolean; overload; static;

    class function IsSymbol(AChar : UnicodeChar) : Boolean; overload; static;
    class function IsSymbol(const AString : UnicodeString; AIndex : Integer) : Boolean; overload; static;

    class function IsUpper(AChar : UnicodeChar) : Boolean; overload; static;
    class function IsUpper(const AString : UnicodeString; AIndex : Integer) : Boolean; overload; static;

    class function IsWhiteSpace(AChar : UnicodeChar) : Boolean; overload; static;
    class function IsWhiteSpace(const AString : UnicodeString; AIndex : Integer) : Boolean; overload; static;

    class function ToLower(AChar : UnicodeChar) : UnicodeChar; overload; static;
    class function ToLower(const AString : UnicodeString) : UnicodeString; overload; static;

    class function ToUpper(AChar : UnicodeChar) : UnicodeChar; overload; static;
    class function ToUpper(const AString : UnicodeString) : UnicodeString; overload; static;
  end;

{$endif VER2_4}

implementation
{$ifndef VER2_4}
uses
  SysUtils,
  RtlConsts;

type  
  PUC_Prop = ^TUC_Prop;
  TUC_Prop = packed record
    Category        : TUnicodeCategory;
    NumericValue    : Double;
    SimpleUpperCase : DWord;
    SimpleLowerCase : DWord;
    WhiteSpace      : Boolean;
  end; 
  
  {$INCLUDE unicodedata.inc}
  
const              
  LOW_SURROGATE_BEGIN  = Word($DC00); 
  LOW_SURROGATE_END    = Word($DFFF); 
  
  HIGH_SURROGATE_BEGIN = Word($D800); 
  HIGH_SURROGATE_END   = Word($DBFF);

  UCS4_HALF_BASE       = LongWord($10000);
  UCS4_HALF_MASK       = Word($3FF);
  MAX_LEGAL_UTF32      = $10FFFF;
    
const
  LETTER_CATEGORIES = [
    TUnicodeCategory.ucUppercaseLetter, TUnicodeCategory.ucLowercaseLetter,
    TUnicodeCategory.ucTitlecaseLetter, TUnicodeCategory.ucModifierLetter,
    TUnicodeCategory.ucOtherLetter
  ];
  LETTER_OR_DIGIT_CATEGORIES =
    LETTER_CATEGORIES +
    [TUnicodeCategory.ucDecimalNumber,TUnicodeCategory.ucLetterNumber];
  NUMBER_CATEGORIES =
    [ TUnicodeCategory.ucDecimalNumber, TUnicodeCategory.ucLetterNumber,
      TUnicodeCategory.ucOtherNumber
    ];
  PUNCTUATION_CATEGORIES = [
    TUnicodeCategory.ucConnectPunctuation, TUnicodeCategory.ucDashPunctuation,
    TUnicodeCategory.ucOpenPunctuation, TUnicodeCategory.ucClosePunctuation,
    TUnicodeCategory.ucInitialPunctuation, TUnicodeCategory.ucFinalPunctuation,
    TUnicodeCategory.ucOtherPunctuation
  ];
  SEPARATOR_CATEGORIES =
    [ TUnicodeCategory.ucSpaceSeparator, TUnicodeCategory.ucLineSeparator,
      TUnicodeCategory.ucParagraphSeparator
    ];
  SYMBOL_CATEGORIES =
    [ TUnicodeCategory.ucMathSymbol, TUnicodeCategory.ucCurrencySymbol,
      TUnicodeCategory.ucModifierSymbol, TUnicodeCategory.ucOtherSymbol
    ];

class function GetProps(const ACodePoint : Word) : PUC_Prop; inline;
begin
  Result:=
    @UC_PROP_ARRAY[
       UC_TABLE_2[
         (UC_TABLE_1[WordRec(ACodePoint).Hi] * 256) +
         WordRec(ACodePoint).Lo
       ]
     ];
end;

{ TCharacter }

constructor TCharacter.Create;
begin
  raise ENoConstructException.CreateFmt(SClassCantBeConstructed, [ClassName]);
end;

class function TCharacter.ConvertFromUtf32(AChar : UCS4Char) : UnicodeString; static;
begin
  if AChar < UCS4_HALF_BASE then
  begin
    if IsSurrogate(UnicodeChar(AChar)) then
      raise EArgumentOutOfRangeException.CreateFmt(SInvalidUTF32Char, [AChar]);
    Result := UnicodeChar(AChar);
  end
  else
  begin
    if AChar > MAX_LEGAL_UTF32 then
      raise EArgumentOutOfRangeException.CreateFmt(SInvalidUTF32Char, [AChar]);
    SetLength(Result, 2);
    AChar := AChar - UCS4_HALF_BASE;
    Result[1] := UnicodeChar((AChar shr 10) + HIGH_SURROGATE_BEGIN);
    Result[2] := UnicodeChar((AChar and UCS4_HALF_MASK) + LOW_SURROGATE_BEGIN);
  end;
end;

class function TCharacter.ConvertToUtf32(const AString : UnicodeString; AIndex : Integer) : UCS4Char; overload; static;
begin
  if (AIndex < 1) or (AIndex > Length(AString)) then
    raise EArgumentOutOfRangeException.CreateFmt(SStringIndexOutOfRange, [AIndex, Length(AString)]);
  Result := Word(AString[AIndex]);
  if IsHighSurrogate(UnicodeChar(Result)) then
  begin
    if Length(AString) < Succ(AIndex) then
      raise EArgumentException.CreateFmt(SInvalidHighSurrogate, [AIndex]);
    Result := ConvertToUtf32(UnicodeChar(Result), AString[Succ(AIndex)]);
  end;
end;

class function TCharacter.ConvertToUtf32(const AString : UnicodeString; AIndex : Integer; out ACharLength : Integer) : UCS4Char; overload; static;
begin
  if (AIndex < 1) or (AIndex > Length(AString)) then
    raise EArgumentOutOfRangeException.CreateFmt(SStringIndexOutOfRange, [AIndex, Length(AString)]);
  Result := Word(AString[AIndex]);
  if IsHighSurrogate(UnicodeChar(Result)) then
  begin
    if Length(AString) < Succ(AIndex) then
      raise EArgumentException.CreateFmt(SInvalidHighSurrogate, [AIndex]);
    Result := ConvertToUtf32(UnicodeChar(Result), AString[Succ(AIndex)]);
    ACharLength := 2;
  end
  else
    ACharLength := 1;
end;

class function TCharacter.ConvertToUtf32(const AHighSurrogate, ALowSurrogate : UnicodeChar) : UCS4Char; overload; static;
begin
  if not IsHighSurrogate(AHighSurrogate) then
    raise EArgumentOutOfRangeException.CreateFmt(SHighSurrogateOutOfRange, [Word(AHighSurrogate)]);
  if not IsLowSurrogate(ALowSurrogate) then
    raise EArgumentOutOfRangeException.CreateFmt(SLowSurrogateOutOfRange, [Word(ALowSurrogate)]);
  Result := (UCS4Char(AHighSurrogate) - HIGH_SURROGATE_BEGIN) shl 10 + (UCS4Char(ALowSurrogate) - LOW_SURROGATE_BEGIN) + UCS4_HALF_BASE;
end;

class function TCharacter.GetNumericValue(AChar : UnicodeChar) : Double; static;
begin
  Result := GetProps(Word(AChar))^.NumericValue;
end;

class function TCharacter.GetNumericValue(
  const AString : UnicodeString;  
        AIndex  : Integer
) : Double; static;
begin
  if (AIndex < 1) or (AIndex > Length(AString)) then
    raise EArgumentOutOfRangeException.CreateFmt(SStringIndexOutOfRange, [AIndex, Length(AString)]);
  Result := GetNumericValue(AString[AIndex]);
end;  

class function TCharacter.GetUnicodeCategory(AChar : UnicodeChar) : TUnicodeCategory; static;
begin   
  Result := GetProps(Word(AChar))^.Category;  
end;

class function TCharacter.GetUnicodeCategory(
  const AString : UnicodeString;  
        AIndex  : Integer
) : TUnicodeCategory; static;
begin   
  if (AIndex < 1) or (AIndex > Length(AString)) then
    raise EArgumentOutOfRangeException.CreateFmt(SStringIndexOutOfRange, [AIndex, Length(AString)]);
  Result := GetUnicodeCategory(AString[AIndex]);
end;

class function TCharacter.IsControl(AChar : UnicodeChar) : Boolean; static;
begin  
  Result := (GetProps(Word(AChar))^.Category = TUnicodeCategory.ucControl);
end;

class function TCharacter.IsControl(
  const AString : UnicodeString;  
        AIndex  : Integer
) : Boolean; static;
begin        
  if (AIndex < 1) or (AIndex > Length(AString)) then
    raise EArgumentOutOfRangeException.CreateFmt(SStringIndexOutOfRange, [AIndex, Length(AString)]);
  Result := IsControl(AString[AIndex]);
end;

class function TCharacter.IsDigit(AChar : UnicodeChar) : Boolean; static;
begin 
  Result := (GetProps(Word(AChar))^.Category = TUnicodeCategory.ucDecimalNumber);
end;

class function TCharacter.IsDigit(
  const AString : UnicodeString;  
        AIndex  : Integer
) : Boolean; static;
begin        
  if (AIndex < 1) or (AIndex > Length(AString)) then
    raise EArgumentOutOfRangeException.CreateFmt(SStringIndexOutOfRange, [AIndex, Length(AString)]);
  Result := IsDigit(AString[AIndex]);
end;

class function TCharacter.IsSurrogate(AChar : UnicodeChar) : Boolean; static;
begin   
  Result := (GetProps(Word(AChar))^.Category = TUnicodeCategory.ucSurrogate);
end;

class function TCharacter.IsSurrogate(
  const AString : UnicodeString;  
        AIndex  : Integer
) : Boolean; static;
begin        
  if (AIndex < 1) or (AIndex > Length(AString)) then
    raise EArgumentOutOfRangeException.CreateFmt(SStringIndexOutOfRange, [AIndex, Length(AString)]);
  Result := IsSurrogate(AString[AIndex]);
end;

class function TCharacter.IsHighSurrogate(AChar : UnicodeChar) : Boolean; static;
begin 
  Result := (GetProps(Word(AChar))^.Category = TUnicodeCategory.ucSurrogate) and
            (Word(AChar) >= HIGH_SURROGATE_BEGIN) and 
            (Word(AChar) <= HIGH_SURROGATE_END);
end;

class function TCharacter.IsHighSurrogate(
  const AString : UnicodeString;  
        AIndex  : Integer
) : Boolean; static;
begin        
  if (AIndex < 1) or (AIndex > Length(AString)) then
    raise EArgumentOutOfRangeException.CreateFmt(SStringIndexOutOfRange, [AIndex, Length(AString)]);
  Result := IsHighSurrogate(AString[AIndex]);
end;

class function TCharacter.IsLowSurrogate(AChar : UnicodeChar) : Boolean; static;
begin   
  Result := (GetProps(Word(AChar))^.Category = TUnicodeCategory.ucSurrogate) and
            (Word(AChar) >= LOW_SURROGATE_BEGIN) and 
            (Word(AChar) <= LOW_SURROGATE_END); 
end;

class function TCharacter.IsLowSurrogate(
  const AString : UnicodeString;  
        AIndex  : Integer
) : Boolean; static;
begin        
  if (AIndex < 1) or (AIndex > Length(AString)) then
    raise EArgumentOutOfRangeException.CreateFmt(SStringIndexOutOfRange, [AIndex, Length(AString)]);
  Result := IsLowSurrogate(AString[AIndex]);
end;

class function TCharacter.IsSurrogatePair(
  const AHighSurrogate,
        ALowSurrogate   : UnicodeChar
) : Boolean;static;
begin
  Result :=
    ( (Word(AHighSurrogate) >= HIGH_SURROGATE_BEGIN) and
      (Word(AHighSurrogate) <= HIGH_SURROGATE_END)
    ) and
    ( (Word(ALowSurrogate) >= LOW_SURROGATE_BEGIN) and
      (Word(ALowSurrogate) <= LOW_SURROGATE_END)
    )
end;

class function TCharacter.IsSurrogatePair(
  const AString : UnicodeString;
        AIndex  : Integer
) : Boolean;static;
begin
  if (AIndex < 1) or (AIndex > Length(AString)) then
    raise EArgumentOutOfRangeException.CreateFmt(SStringIndexOutOfRange, [AIndex, Length(AString)]);
  Result := IsSurrogatePair(AString[AIndex],AString[AIndex+1]);
end;

class function TCharacter.IsLetter(AChar : UnicodeChar) : Boolean; static;
begin 
  Result := (GetProps(Word(AChar))^.Category in LETTER_CATEGORIES);
end;

class function TCharacter.IsLetter(
  const AString : UnicodeString;  
        AIndex  : Integer
) : Boolean; static;
begin        
  if (AIndex < 1) or (AIndex > Length(AString)) then
    raise EArgumentOutOfRangeException.CreateFmt(SStringIndexOutOfRange, [AIndex, Length(AString)]);
  Result := IsLetter(AString[AIndex]);
end;

class function TCharacter.IsLetterOrDigit(AChar : UnicodeChar) : Boolean; static;
begin 
  Result := (GetProps(Word(AChar))^.Category in LETTER_OR_DIGIT_CATEGORIES);
end;

class function TCharacter.IsLetterOrDigit(
  const AString : UnicodeString;  
        AIndex  : Integer
) : Boolean; static;
begin        
  if (AIndex < 1) or (AIndex > Length(AString)) then
    raise EArgumentOutOfRangeException.CreateFmt(SStringIndexOutOfRange, [AIndex, Length(AString)]);
  Result := IsLetterOrDigit(AString[AIndex]);
end;

class function TCharacter.IsLower(AChar : UnicodeChar) : Boolean; static;
begin        
  Result := (GetProps(Word(AChar))^.Category = TUnicodeCategory.ucLowercaseLetter);
end;

class function TCharacter.IsLower(
  const AString : UnicodeString;  
        AIndex  : Integer
) : Boolean; static;
begin        
  if (AIndex < 1) or (AIndex > Length(AString)) then
    raise EArgumentOutOfRangeException.CreateFmt(SStringIndexOutOfRange, [AIndex, Length(AString)]);
  Result := IsLower(AString[AIndex]);
end;

class function TCharacter.IsNumber(AChar : UnicodeChar) : Boolean; static;
begin
  Result := (GetProps(Word(AChar))^.Category in NUMBER_CATEGORIES);
end;

class function TCharacter.IsNumber(
  const AString : UnicodeString;
        AIndex  : Integer
) : Boolean;static;
begin
  if (AIndex < 1) or (AIndex > Length(AString)) then
    raise EArgumentOutOfRangeException.CreateFmt(SStringIndexOutOfRange, [AIndex, Length(AString)]);
  Result := IsNumber(AString[AIndex]);
end;

class function TCharacter.IsPunctuation(AChar : UnicodeChar) : Boolean;static;
begin
  Result := (GetProps(Word(AChar))^.Category in PUNCTUATION_CATEGORIES);
end;

class function TCharacter.IsPunctuation(
  const AString : UnicodeString;
        AIndex  : Integer
) : Boolean;static;
begin
  if (AIndex < 1) or (AIndex > Length(AString)) then
    raise EArgumentOutOfRangeException.CreateFmt(SStringIndexOutOfRange, [AIndex, Length(AString)]);
  Result := IsPunctuation(AString[AIndex]);
end;

class function TCharacter.IsSeparator(AChar: UnicodeChar): Boolean;static;
begin
  Result := (GetProps(Word(AChar))^.Category in SEPARATOR_CATEGORIES);
end;

class function TCharacter.IsSeparator(
  const AString : UnicodeString;
        AIndex  : Integer
) : Boolean;static;
begin
  if (AIndex < 1) or (AIndex > Length(AString)) then
    raise EArgumentOutOfRangeException.CreateFmt(SStringIndexOutOfRange, [AIndex, Length(AString)]);
  Result := IsSeparator(AString[AIndex]);
end;

class function TCharacter.IsSymbol(AChar: UnicodeChar): Boolean;static;
begin
  Result := (GetProps(Word(AChar))^.Category in SYMBOL_CATEGORIES);
end;

class function TCharacter.IsSymbol(
  const AString : UnicodeString;
        AIndex  : Integer
) : Boolean;static;
begin
  if (AIndex < 1) or (AIndex > Length(AString)) then
    raise EArgumentOutOfRangeException.CreateFmt(SStringIndexOutOfRange, [AIndex, Length(AString)]);
  Result := IsSymbol(AString[AIndex]);
end;

class function TCharacter.IsUpper(AChar : UnicodeChar) : Boolean;static;
begin
  Result := (GetProps(Word(AChar))^.Category = TUnicodeCategory.ucUppercaseLetter);
end;

class function TCharacter.IsUpper(
  const AString : UnicodeString;
        AIndex  : Integer
) : Boolean;static;
begin
  if (AIndex < 1) or (AIndex > Length(AString)) then
    raise EArgumentOutOfRangeException.CreateFmt(SStringIndexOutOfRange, [AIndex, Length(AString)]);
  Result := IsUpper(AString[AIndex]);
end;

class function TCharacter.IsWhiteSpace(AChar : UnicodeChar) : Boolean;static;
begin
  Result := GetProps(Word(AChar))^.WhiteSpace;
end;

class function TCharacter.IsWhiteSpace(
  const AString : UnicodeString;
        AIndex  : Integer
) : Boolean;static;
begin
  if (AIndex < 1) or (AIndex > Length(AString)) then
    raise EArgumentOutOfRangeException.CreateFmt(SStringIndexOutOfRange, [AIndex, Length(AString)]);
  Result := IsWhiteSpace(AString[AIndex]);
end;

class function TCharacter.ToLower(AChar : UnicodeChar) : UnicodeChar;static;
begin
  Result := UnicodeChar(GetProps(Word(AChar))^.SimpleLowerCase);
  if (Result = UnicodeChar(0)) then
    Result := AChar;
end;

class function TCharacter.ToLower(const AString : UnicodeString) : UnicodeString;static;
var
  i, c : SizeInt;
  pp, pr : PUnicodeChar;
begin
  c := Length(AString);
  SetLength(Result,c);
  if (c > 0) then begin
    pp := @AString[1];
    pr := @Result[1];
    for i := 1 to c do begin
      pr^ := ToLower(pp^);
      Inc(pp);
      Inc(pr);
    end;
  end;
end;

class function TCharacter.ToUpper(AChar : UnicodeChar) : UnicodeChar;static;
begin
  Result := UnicodeChar(GetProps(Word(AChar))^.SimpleUpperCase);
  if (Result = UnicodeChar(0)) then
    Result := AChar;
end;

class function TCharacter.ToUpper(const AString : UnicodeString) : UnicodeString;static;
var
  i, c : SizeInt;
  pp, pr : PUnicodeChar;
begin
  c := Length(AString);
  SetLength(Result,c);
  if (c > 0) then begin
    pp := @AString[1];
    pr := @Result[1];
    for i := 1 to c do begin
      pr^ := ToUpper(pp^);
      Inc(pp);
      Inc(pr);
    end;
  end;
end;
{$endif VER2_4}
end.
