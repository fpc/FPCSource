{   Unicode "Character" properties handler.

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
{$IFNDEF FPC_DOTTEDUNITS}
unit Character;
{$ENDIF FPC_DOTTEDUNITS}

interface

{$mode objfpc}
{$H+}
{$PACKENUM 1}
{$SCOPEDENUMS ON}
{$modeswitch typehelpers}

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.CodePages.unicodedata;
{$ELSE FPC_DOTTEDUNITS}
uses
  unicodedata;
{$ENDIF FPC_DOTTEDUNITS}

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
  TUnicodeCategorySet = set of TUnicodeCategory;

  TCharacterOption = (coIgnoreInvalidSequence);
  TCharacterOptions = set of TCharacterOption;

  { TCharacter }

  TCharacter = class sealed
  private
    class function TestCategory(const AString : UnicodeString; AIndex : Integer; ACategory : TUnicodeCategory) : Boolean; overload; static;
    class function TestCategory(const AString : UnicodeString; AIndex : Integer; ACategory : TUnicodeCategorySet) : Boolean; overload; static;
  public  
    const
      MaxHighSurrogate       = UnicodeChar(HIGH_SURROGATE_END);
      MaxLowSurrogate        = UnicodeChar(LOW_SURROGATE_END);
      MaxSurrogate           = UnicodeChar(LOW_SURROGATE_END);
      MinHighSurrogate       = UnicodeChar(HIGH_SURROGATE_BEGIN);
      MinLowSurrogate        = UnicodeChar(LOW_SURROGATE_BEGIN);
      MinSurrogate           = UnicodeChar(HIGH_SURROGATE_BEGIN);

  public
    constructor Create;

    class function ConvertFromUtf32(AChar : UCS4Char) : UnicodeString; static;
    class function ConvertToUtf32(const AString : UnicodeString; AIndex : Integer) : UCS4Char; overload; static;
    class function ConvertToUtf32(const AString : UnicodeString; AIndex : Integer; out ACharLength : Integer) : UCS4Char; overload; static;
    class function ConvertToUtf32(const AHighSurrogate, ALowSurrogate : UnicodeChar) : UCS4Char; overload; static;
    
    class function GetNumericValue(AChar : UnicodeChar) : Double; static; overload;
    class function GetNumericValue(const AString : UnicodeString; AIndex : Integer) : Double; overload; static;
    class function GetNumericValue(aChar: UCS4Char): Double; overload; inline; static;

    class function GetUnicodeCategory(AChar : UnicodeChar) : TUnicodeCategory; overload; static; inline;
    class function GetUnicodeCategory(const AString : UnicodeString; AIndex : Integer) : TUnicodeCategory; overload; static;
    class function GetUnicodeCategory(aChar: UCS4Char): TUnicodeCategory; overload; inline; static;

    class function IsControl(AChar : UnicodeChar) : Boolean; overload; static; inline;
    class function IsControl(const AString : UnicodeString; AIndex : Integer) : Boolean; overload; static; inline;
    class function IsControl(aChar: UCS4Char): Boolean; overload; inline; static;

    class function IsDigit(AChar : UnicodeChar) : Boolean; overload; static; inline;
    class function IsDigit(const AString : UnicodeString; AIndex : Integer) : Boolean; overload; static; inline;
    class function IsDigit(aChar: UCS4Char): Boolean; overload; inline; static;

    class function IsSurrogate(AChar : UnicodeChar) : Boolean; overload; static; inline;
    class function IsSurrogate(const AString : UnicodeString; AIndex : Integer) : Boolean; overload; static;
    class function IsSurrogate(aChar: UCS4Char): Boolean; overload; inline; static;

    class function IsHighSurrogate(AChar : UnicodeChar) : Boolean; overload; static; inline;
    class function IsHighSurrogate(const AString : UnicodeString; AIndex : Integer) : Boolean; overload; static;
    class function IsHighSurrogate(aChar: UCS4Char): Boolean; overload; inline; static;

    class function IsLowSurrogate(AChar : UnicodeChar) : Boolean; overload; static; inline;
    class function IsLowSurrogate(const AString : UnicodeString; AIndex : Integer) : Boolean; overload; static;
    class function IsLowSurrogate(aChar: UCS4Char): Boolean; overload; inline; static;

    class function IsSurrogatePair(const AHighSurrogate, ALowSurrogate : UnicodeChar) : Boolean; overload; static; inline;
    class function IsSurrogatePair(const AString : UnicodeString; AIndex : Integer) : Boolean; overload; static;
    class function IsLetter(aChar: UCS4Char): Boolean; overload; inline; static;

    class function IsLetter(AChar : UnicodeChar) : Boolean; overload; static; inline;
    class function IsLetter(const AString : UnicodeString; AIndex : Integer) : Boolean; overload; static; inline;
    class function IsLetterOrDigit(aChar: UCS4Char): Boolean; overload; inline; static;

    class function IsLetterOrDigit(AChar : UnicodeChar) : Boolean; overload; static; inline;
    class function IsLetterOrDigit(const AString : UnicodeString; AIndex : Integer) : Boolean; overload; static; inline;
    
    class function IsLower(AChar : UnicodeChar) : Boolean; overload; static; inline;
    class function IsLower(const AString : UnicodeString; AIndex : Integer) : Boolean; overload; static; inline;
    class function IsLower(aChar: UCS4Char): Boolean; overload; inline; static;

    class function IsNumber(AChar : UnicodeChar) : Boolean; overload; static; inline;
    class function IsNumber(const AString : UnicodeString; AIndex : Integer) : Boolean; overload; static;
    class function IsNumber(aChar: UCS4Char): Boolean; overload; inline; static;

    class function IsPunctuation(AChar : UnicodeChar) : Boolean; overload; static; inline;
    class function IsPunctuation(const AString : UnicodeString; AIndex : Integer) : Boolean; overload; static; inline;
    class function IsPunctuation(aChar: UCS4Char): Boolean; overload; inline; static;

    class function IsSeparator(AChar : UnicodeChar) : Boolean; overload; static; inline;
    class function IsSeparator(const AString : UnicodeString; AIndex : Integer) : Boolean; overload; static; inline;
    class function IsSeparator(aChar: UCS4Char): Boolean; overload; inline; static;

    class function IsSymbol(AChar : UnicodeChar) : Boolean; overload; static; inline;
    class function IsSymbol(const AString : UnicodeString; AIndex : Integer) : Boolean; overload; static; inline;
    class function IsSymbol(aChar: UCS4Char): Boolean; overload; inline; static;

    class function IsUpper(AChar : UnicodeChar) : Boolean; overload; static; inline;
    class function IsUpper(const AString : UnicodeString; AIndex : Integer) : Boolean; overload; static; inline;
    class function IsUpper(aChar: UCS4Char): Boolean; overload; inline; static;

    class function IsWhiteSpace(AChar : UnicodeChar) : Boolean; overload; static; inline;
    class function IsWhiteSpace(const AString : UnicodeString; AIndex : Integer) : Boolean; overload; static;
    class function IsWhiteSpace(aChar: UCS4Char): Boolean; overload; inline; static;

    class function ToLower(AChar : UnicodeChar) : UnicodeChar; overload; static;
    class function ToLower(const AString : UnicodeString) : UnicodeString; inline;overload; static;
    class function ToLower(const AString : UnicodeString; const AOptions : TCharacterOptions) : UnicodeString; overload; static;
    class function ToLower(aChar: UCS4Char): UCS4Char; overload; inline; static;

    class function ToUpper(AChar : UnicodeChar) : UnicodeChar; overload; static;
    class function ToUpper(const AString : UnicodeString) : UnicodeString; inline; overload; static;
    class function ToUpper(const AString : UnicodeString; const AOptions : TCharacterOptions) : UnicodeString; overload; static;
    class function ToUpper(aChar: UCS4Char): UCS4Char; overload; inline; static;

    class function MaxCodePoint: Integer; static;
    class function UnicodeDataVersion: UnicodeString; static;

    class function IsDefined(aChar: UnicodeChar): Boolean; overload; inline; static;
    class function IsDefined(aChar: UCS4Char): Boolean; overload; inline; static;
    class function IsDefined(const S: UnicodeString; Index: Integer): Boolean; overload; inline; static;



  end;

    { TCharHelper }

    TCharHelper = type helper for UnicodeChar
    public
      const
        MaxHighSurrogate       = UnicodeChar(HIGH_SURROGATE_END);
        MaxLowSurrogate        = UnicodeChar(LOW_SURROGATE_END);
        MaxSurrogate           = UnicodeChar(LOW_SURROGATE_END);
        MinHighSurrogate       = UnicodeChar(HIGH_SURROGATE_BEGIN);
        MinLowSurrogate        = UnicodeChar(LOW_SURROGATE_BEGIN);
        MinSurrogate           = UnicodeChar(HIGH_SURROGATE_BEGIN);
      class function MaxCodePoint: Integer; static;
      class function UnicodeDataVersion: UnicodeString; static;

      class function ConvertFromUtf32(aChar: UCS4Char): UnicodeString; static;
      class function ConvertToUtf32(const S: UnicodeString; Index: Integer): UCS4Char; overload; inline; static;
      class function ConvertToUtf32(const S: UnicodeString; Index: Integer; out CharLength: Integer): UCS4Char; overload; static;
      class function ConvertToUtf32(const HighSurrogate, LowSurrogate: UnicodeChar): UCS4Char; overload; static;

      function GetNumericValue: Double; overload;
      class function GetNumericValue(const S: UnicodeString; Index: Integer): Double; overload; static;

      function GetUnicodeCategory: TUnicodeCategory; overload;
      class function GetUnicodeCategory(const S: UnicodeString; Index: Integer): TUnicodeCategory; overload; static;

      function IsControl: Boolean; overload;

      function IsDefined: Boolean; overload;
      function IsDigit: Boolean; overload;
      function IsHighSurrogate: Boolean; overload; inline;
      function IsInArray(const SomeChars: array of UnicodeChar): Boolean; overload;
      function IsLetter: Boolean; overload;
      function IsLetterOrDigit: Boolean; overload;
      function IsLower: Boolean; overload;
      function IsLowSurrogate: Boolean; overload; inline;
      function IsNumber: Boolean; overload;
      function IsPunctuation: Boolean; overload;
      function IsSeparator: Boolean; overload;
      function IsSurrogate: Boolean; overload; inline;
      function IsSymbol: Boolean; overload;
      function IsUpper: Boolean; overload;
      function IsWhiteSpace: Boolean; overload;
      function ToLower: UnicodeChar; overload;
      function ToUpper: UnicodeChar; overload;
      function ToUCS4Char: UCS4Char; inline;

      class function IsControl(const S: UnicodeString; Index: Integer): Boolean; overload; static; inline;
      class function IsDefined(const S: UnicodeString; Index: Integer): Boolean; overload; static; inline;
      class function IsDigit(const S: UnicodeString; Index: Integer): Boolean; overload; static; inline;
      class function IsHighSurrogate(const S: UnicodeString; Index: Integer): Boolean; overload; inline; static;
      class function IsInArray(const S: UnicodeString; Index: Integer; const SomeChars: array of UnicodeChar): Boolean; overload; static; inline;
      class function IsLetter(const S: UnicodeString; Index: Integer): Boolean; overload; static;inline;
      class function IsLetterOrDigit(const S: UnicodeString; Index: Integer): Boolean; overload; static;
      class function IsLower(const S: UnicodeString; Index: Integer): Boolean; overload; static;
      class function IsLowSurrogate(const S: UnicodeString; Index: Integer): Boolean; overload; inline; static;
      class function IsNumber(const S: UnicodeString; Index: Integer): Boolean; overload; static;
      class function IsPunctuation(const S: UnicodeString; Index: Integer): Boolean; overload; static;
      class function IsSeparator(const S: UnicodeString; Index: Integer): Boolean; overload; static;
      class function IsSurrogate(const S: UnicodeString; Index: Integer): Boolean; overload; static;
      class function IsSurrogatePair(const HighSurrogate, LowSurrogate: UnicodeChar): Boolean; overload; inline; static;
      class function IsSurrogatePair(const S: UnicodeString; Index: Integer): Boolean; overload; static;
      class function IsSymbol(const S: UnicodeString; Index: Integer): Boolean; overload; static;
      class function IsUpper(const S: UnicodeString; Index: Integer): Boolean; overload; static;
      class function IsWhiteSpace(const S: UnicodeString; Index: Integer): Boolean; overload; static;
      class function ToLower(const S: UnicodeString): UnicodeString; overload; static;
      class function ToUpper(const S: UnicodeString): UnicodeString; overload; static;

      class function GetNumericValue(aChar: UCS4Char): Double; overload; static;
      class function GetUnicodeCategory(aChar: UCS4Char): TUnicodeCategory; overload; static;

      class function IsControl(aChar: UCS4Char): Boolean; overload; static;
      class function IsDefined(aChar: UCS4Char): Boolean; overload; static;
      class function IsDigit(aChar: UCS4Char): Boolean; overload; static;
      class function IsHighSurrogate(aChar: UCS4Char): Boolean; overload; inline; static;
      class function IsLetter(aChar: UCS4Char): Boolean; overload; static;
      class function IsLetterOrDigit(aChar: UCS4Char): Boolean; overload; static;
      class function IsLower(aChar: UCS4Char): Boolean; overload; static;
      class function IsLowSurrogate(aChar: UCS4Char): Boolean; overload; inline; static;
      class function IsNumber(aChar: UCS4Char): Boolean; overload; static;
      class function IsPunctuation(aChar: UCS4Char): Boolean; overload; static;
      class function IsSeparator(aChar: UCS4Char): Boolean; overload; static;
      class function IsSurrogate(Surrogate: UCS4Char): Boolean; overload; inline; static;
      class function IsSymbol(aChar: UCS4Char): Boolean; overload; static;
      class function IsUpper(aChar: UCS4Char): Boolean; overload; static;
      class function IsWhiteSpace(aChar: UCS4Char): Boolean; overload; static;
      class function ToLower(aChar: UCS4Char): UCS4Char; overload; static;
      class function ToUpper(aChar: UCS4Char): UCS4Char; overload; static;

    end;


  // flat functions
  function ConvertFromUtf32(AChar : UCS4Char) : UnicodeString;

  function ConvertToUtf32(const AString : UnicodeString; AIndex : Integer) : UCS4Char; overload;
  function ConvertToUtf32(const AString : UnicodeString; AIndex : Integer; out ACharLength : Integer) : UCS4Char; overload;
  function ConvertToUtf32(const AHighSurrogate, ALowSurrogate : UnicodeChar) : UCS4Char; overload;

  function GetNumericValue(AChar : UnicodeChar) : Double; overload;
  function GetNumericValue(const AString : UnicodeString; AIndex : Integer) : Double; overload;
  function GetNumericValue(aChar: UCS4Char): Double; overload; inline;

  function GetUnicodeCategory(AChar : UnicodeChar) : TUnicodeCategory; overload;
  function GetUnicodeCategory(const AString : UnicodeString; AIndex : Integer) : TUnicodeCategory; overload;
  function GetUnicodeCategory(aChar: UCS4Char): TUnicodeCategory; overload; inline;

  function IsControl(AChar : UnicodeChar) : Boolean; overload;
  function IsControl(const AString : UnicodeString; AIndex : Integer) : Boolean; overload;
  function IsControl(aChar: UCS4Char): Boolean; overload; inline;

  function IsDigit(AChar : UnicodeChar) : Boolean; overload;
  function IsDigit(const AString : UnicodeString; AIndex : Integer) : Boolean; overload;
  function IsDigit(aChar: UCS4Char): Boolean; overload; inline;

  function IsSurrogate(AChar : UnicodeChar) : Boolean; overload;
  function IsSurrogate(const AString : UnicodeString; AIndex : Integer) : Boolean; overload;
  function IsSurrogate(Surrogate: UCS4Char): Boolean; overload; inline;

  function IsHighSurrogate(AChar : UnicodeChar) : Boolean; overload;
  function IsHighSurrogate(const AString : UnicodeString; AIndex : Integer) : Boolean; overload;
  function IsHighSurrogate(aChar: UCS4Char): Boolean; overload; inline;

  function IsLowSurrogate(AChar : UnicodeChar) : Boolean; overload;
  function IsLowSurrogate(const AString : UnicodeString; AIndex : Integer) : Boolean; overload;
  function IsLowSurrogate(aChar: UCS4Char): Boolean; overload; inline;

  function IsSurrogatePair(const AHighSurrogate, ALowSurrogate : UnicodeChar) : Boolean; overload;
  function IsSurrogatePair(const AString : UnicodeString; AIndex : Integer) : Boolean; overload;

  function IsLetter(AChar : UnicodeChar) : Boolean; overload;
  function IsLetter(const AString : UnicodeString; AIndex : Integer) : Boolean; overload;
  function IsLetter(aChar: UCS4Char): Boolean; overload; inline;

  function IsLetterOrDigit(AChar : UnicodeChar) : Boolean; overload;
  function IsLetterOrDigit(const AString : UnicodeString; AIndex : Integer) : Boolean; overload;
  function IsLetterOrDigit(aChar: UCS4Char): Boolean; overload; inline;

  function IsLower(AChar : UnicodeChar) : Boolean; overload;
  function IsLower(const AString : UnicodeString; AIndex : Integer) : Boolean; overload;
  function IsLower(aChar: UCS4Char): Boolean; overload; inline;

  function IsNumber(AChar : UnicodeChar) : Boolean; overload;
  function IsNumber(const AString : UnicodeString; AIndex : Integer) : Boolean; overload;
  function IsNumber(aChar: UCS4Char): Boolean; overload; inline;

  function IsPunctuation(AChar : UnicodeChar) : Boolean; overload;
  function IsPunctuation(const AString : UnicodeString; AIndex : Integer) : Boolean; overload;
  function IsPunctuation(aChar: UCS4Char): Boolean; overload; inline;

  function IsSeparator(AChar : UnicodeChar) : Boolean; overload;
  function IsSeparator(const AString : UnicodeString; AIndex : Integer) : Boolean; overload;
  function IsSeparator(aChar: UCS4Char): Boolean; overload; inline;

  function IsSymbol(AChar : UnicodeChar) : Boolean; overload;
  function IsSymbol(const AString : UnicodeString; AIndex : Integer) : Boolean; overload;
  function IsSymbol(aChar: UCS4Char): Boolean; overload; inline;

  function IsUpper(AChar : UnicodeChar) : Boolean; overload;
  function IsUpper(const AString : UnicodeString; AIndex : Integer) : Boolean; overload;
  function IsUpper(aChar: UCS4Char): Boolean; overload; inline;

  function IsWhiteSpace(AChar : UnicodeChar) : Boolean; overload;
  function IsWhiteSpace(const AString : UnicodeString; AIndex : Integer) : Boolean; overload;
  function IsWhiteSpace(aChar: UCS4Char): Boolean; overload; inline;

  function ToLower(AChar : UnicodeChar) : UnicodeChar; overload;
  function ToLower(const AString : UnicodeString) : UnicodeString; overload;
  function ToLower(aChar: UCS4Char): UCS4Char; overload; inline;

  function ToUpper(AChar : UnicodeChar) : UnicodeChar; overload;
  function ToUpper(const AString : UnicodeString) : UnicodeString; overload;
  function ToUpper(aChar: UCS4Char): UCS4Char; overload; inline;

  function IsDefined(aChar: UnicodeChar): Boolean; overload; inline;
  function IsDefined(aChar: UCS4Char): Boolean; overload; inline;
  function IsDefined(const S: Unicodestring; Index: Integer): Boolean; overload; inline;

implementation

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.SysUtils,
  System.RtlConsts;
{$ELSE FPC_DOTTEDUNITS}
uses
  SysUtils,
  RtlConsts;
{$ENDIF FPC_DOTTEDUNITS}

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

Function GetProps(aChar: UCS4Char) : PUC_Prop; inline; overload;

var
  aHigh,aLow : UnicodeChar;

begin
  FromUCS4(aChar,aHigh,aLow);
  Result:=GetProps(aHigh,aLow);
end;

function IsDefined(aChar: UnicodeChar): Boolean; overload; inline;

begin
  Result:=TCharacter.IsDefined(aChar);
end;

function IsDefined(aChar: UCS4Char): Boolean; overload; inline;

begin
  Result:=TCharacter.IsDefined(aChar);
end;

function IsDefined(const S: UnicodeString; Index: Integer): Boolean; overload; inline;

begin
  Result :=TCharacter.IsDefined(S,Index);
end;


function ConvertFromUtf32(AChar: UCS4Char): UnicodeString;
begin
  Result := TCharacter.ConvertFromUtf32(AChar);
end;

function ConvertToUtf32(const AString: UnicodeString; AIndex: Integer): UCS4Char;
begin
  Result := TCharacter.ConvertToUtf32(AString, AIndex);
end;

function ConvertToUtf32(const AString: UnicodeString; AIndex: Integer; out ACharLength: Integer): UCS4Char;
begin
  Result := TCharacter.ConvertToUtf32(AString, AIndex, ACharLength);
end;

function ConvertToUtf32(const AHighSurrogate, ALowSurrogate: UnicodeChar): UCS4Char;
begin
  Result := TCharacter.ConvertToUtf32(AHighSurrogate, ALowSurrogate);
end;

function GetNumericValue(AChar: UnicodeChar): Double;
begin
  Result := TCharacter.GetNumericValue(AChar);
end;

function GetNumericValue(const AString: UnicodeString; AIndex: Integer): Double;
begin
  Result := TCharacter.GetNumericValue(AString, AIndex);
end;

function GetNumericValue(aChar: UCS4Char): Double;
begin
  Result:=TCharacter.GetNumericValue(aChar);
end;

function GetUnicodeCategory(AChar: UnicodeChar): TUnicodeCategory;
begin
  Result := TCharacter.GetUnicodeCategory(AChar);
end;

function GetUnicodeCategory(const AString: UnicodeString; AIndex: Integer): TUnicodeCategory;
begin
  Result := TCharacter.GetUnicodeCategory(AString, AIndex);
end;

function GetUnicodeCategory(aChar: UCS4Char): TUnicodeCategory; overload; inline;

begin
  Result:=TCharacter.GetUnicodeCategory(aChar);
end;

function IsControl(AChar: UnicodeChar): Boolean;
begin
  Result := TCharacter.IsControl(AChar);
end;

function IsControl(const AString: UnicodeString; AIndex: Integer): Boolean;
begin
  Result := TCharacter.IsControl(AString, AIndex);
end;

function IsControl(aChar: UCS4Char): Boolean;
begin
  Result := TCharacter.IsControl(aChar);
end;

function IsDigit(AChar: UnicodeChar): Boolean;
begin
  Result := TCharacter.IsDigit(AChar);
end;

function IsDigit(const AString: UnicodeString; AIndex: Integer): Boolean;
begin
  Result := TCharacter.IsDigit(AString, AIndex);
end;

function IsDigit(aChar: UCS4Char): Boolean;
begin
  Result := TCharacter.IsDigit(aChar);
end;

function IsSurrogate(AChar: UnicodeChar): Boolean;
begin
  Result := TCharacter.IsSurrogate(AChar);
end;

function IsSurrogate(const AString: UnicodeString; AIndex: Integer): Boolean;
begin
  Result := TCharacter.IsSurrogate(AString, AIndex);
end;

function IsSurrogate(Surrogate: UCS4Char): Boolean;
begin
  Result := TCharacter.IsSurrogate(Surrogate);
end;

function IsHighSurrogate(AChar: UnicodeChar): Boolean;
begin
  Result := TCharacter.IsHighSurrogate(AChar);
end;

function IsHighSurrogate(const AString: UnicodeString; AIndex: Integer): Boolean;
begin
  Result := TCharacter.IsHighSurrogate(AString, AIndex);
end;

function IsHighSurrogate(aChar: UCS4Char): Boolean;
begin
  Result := TCharacter.IsHighSurrogate(aChar);
end;

function IsLowSurrogate(AChar: UnicodeChar): Boolean;
begin
  Result := TCharacter.IsLowSurrogate(AChar);
end;

function IsLowSurrogate(const AString: UnicodeString; AIndex: Integer): Boolean;
begin
  Result := TCharacter.IsLowSurrogate(AString, AIndex);
end;

function IsLowSurrogate(aChar: UCS4Char): Boolean;
begin
  Result := TCharacter.IsLowSurrogate(aChar);
end;

function IsSurrogatePair(const AHighSurrogate, ALowSurrogate: UnicodeChar): Boolean;
begin
  Result := TCharacter.IsSurrogatePair(AHighSurrogate, ALowSurrogate);
end;

function IsSurrogatePair(const AString: UnicodeString; AIndex: Integer): Boolean;
begin
  Result := TCharacter.IsSurrogatePair(AString, AIndex);
end;

function IsLetter(AChar: UnicodeChar): Boolean;
begin
  Result := TCharacter.IsLetter(AChar);
end;

function IsLetter(const AString: UnicodeString; AIndex: Integer): Boolean;
begin
  Result := TCharacter.IsLetter(AString, AIndex);
end;

function IsLetter(aChar: UCS4Char): Boolean;
begin
  Result := TCharacter.IsLetter(aChar);
end;

function IsLetterOrDigit(AChar: UnicodeChar): Boolean;
begin
  Result := TCharacter.IsLetterOrDigit(AChar);
end;

function IsLetterOrDigit(const AString: UnicodeString; AIndex: Integer): Boolean;
begin
  Result := TCharacter.IsLetterOrDigit(AString, AIndex);
end;

function IsLetterOrDigit(aChar: UCS4Char): Boolean;
begin
  Result := TCharacter.IsLetterOrDigit(aChar);
end;

function IsLower(AChar: UnicodeChar): Boolean;
begin
  Result := TCharacter.IsLower(AChar);
end;

function IsLower(const AString: UnicodeString; AIndex: Integer): Boolean;
begin
  Result := TCharacter.IsLower(AString, AIndex);
end;

function IsLower(aChar: UCS4Char): Boolean;
begin
  Result := TCharacter.IsLower(aChar);
end;

function IsNumber(AChar: UnicodeChar): Boolean;
begin
  Result := TCharacter.IsNumber(AChar);
end;

function IsNumber(const AString: UnicodeString; AIndex: Integer): Boolean;
begin
  Result := TCharacter.IsNumber(AString, AIndex);
end;

function IsNumber(aChar: UCS4Char): Boolean;
begin
  Result := TCharacter.IsNumber(aChar);
end;

function IsPunctuation(AChar: UnicodeChar): Boolean;
begin
  Result := TCharacter.IsPunctuation(AChar);
end;

function IsPunctuation(const AString: UnicodeString; AIndex: Integer): Boolean;
begin
  Result := TCharacter.IsPunctuation(AString, AIndex);
end;

function IsPunctuation(aChar: UCS4Char): Boolean;
begin
  Result := TCharacter.IsPunctuation(aChar);
end;

function IsSeparator(AChar: UnicodeChar): Boolean;
begin
  Result := TCharacter.IsSeparator(AChar);
end;

function IsSeparator(const AString: UnicodeString; AIndex: Integer): Boolean;
begin
  Result := TCharacter.IsSeparator(AString, AIndex);
end;

function IsSeparator(aChar: UCS4Char): Boolean;
begin
  Result := TCharacter.IsSeparator(aChar);
end;

function IsSymbol(AChar: UnicodeChar): Boolean;
begin
  Result := TCharacter.IsSymbol(AChar);
end;

function IsSymbol(const AString: UnicodeString; AIndex: Integer): Boolean;
begin
  Result := TCharacter.IsSymbol(AString, AIndex);
end;

function IsSymbol(aChar: UCS4Char): Boolean;
begin
  Result := TCharacter.IsSymbol(aChar);
end;

function IsUpper(aChar: UCS4Char): Boolean;
begin
  Result := TCharacter.IsUpper(aChar);
end;

function IsUpper(AChar: UnicodeChar): Boolean;
begin
  Result := TCharacter.IsUpper(AChar);
end;

function IsUpper(const AString: UnicodeString; AIndex: Integer): Boolean;
begin
  Result := TCharacter.IsUpper(AString, AIndex);
end;

function IsWhiteSpace(AChar: UnicodeChar): Boolean;
begin
  Result := TCharacter.IsWhiteSpace(AChar);
end;

function IsWhiteSpace(const AString: UnicodeString; AIndex: Integer): Boolean;
begin
  Result := TCharacter.IsWhiteSpace(AString, AIndex);
end;

function IsWhiteSpace(aChar: UCS4Char): Boolean;
begin
  Result := TCharacter.IsWhiteSpace(aChar);
end;

function ToLower(AChar: UnicodeChar): UnicodeChar;
begin
  Result := TCharacter.ToLower(AChar);
end;

function ToLower(const AString: UnicodeString): UnicodeString;
begin
  Result := TCharacter.ToLower(AString);
end;

function ToLower(aChar: UCS4Char): UCS4Char;
begin
  Result := TCharacter.ToLower(aChar);
end;

function ToUpper(AChar: UnicodeChar): UnicodeChar;
begin
  Result := TCharacter.ToUpper(AChar);
end;

function ToUpper(const AString: UnicodeString): UnicodeString;
begin
  Result := TCharacter.ToUpper(AString);
end;

function ToUpper(aChar: UCS4Char): UCS4Char;
begin
  Result := TCharacter.ToUpper(aChar);
end;




{ TCharacter }

class function TCharacter.TestCategory(
  const AString : UnicodeString;
        AIndex  : Integer;
        ACategory : TUnicodeCategory
) : Boolean;
var
  pu : PUC_Prop;
begin
  if (AIndex < 1) or (AIndex > Length(AString)) then
    raise EArgumentOutOfRangeException.CreateFmt(SStringIndexOutOfRange, [AIndex, Length(AString)]);
  pu := GetProps(Word(AString[AIndex]));
  if (TUnicodeCategory(pu^.Category) = TUnicodeCategory.ucSurrogate) then begin
    if not IsSurrogatePair(AString,AIndex) then
      raise EArgumentException.Create(SInvalidUnicodeCodePointSequence);
    pu := GetProps(AString[AIndex],AString[AIndex+1]);
  end;
  Result := (TUnicodeCategory(pu^.Category) = ACategory);
end;

class function TCharacter.TestCategory(
  const AString : UnicodeString;
        AIndex : Integer;
        ACategory : TUnicodeCategorySet
) : Boolean;
var
  pu : PUC_Prop;
begin
  if (AIndex < 1) or (AIndex > Length(AString)) then
    raise EArgumentOutOfRangeException.CreateFmt(SStringIndexOutOfRange, [AIndex, Length(AString)]);
  pu := GetProps(Word(AString[AIndex]));
  if (TUnicodeCategory(pu^.Category) = TUnicodeCategory.ucSurrogate) then begin
    if not IsSurrogatePair(AString,AIndex) then
      raise EArgumentException.Create(SInvalidUnicodeCodePointSequence);
    pu := GetProps(AString[AIndex],AString[AIndex+1]);
  end;
  Result := (TUnicodeCategory(pu^.Category) in ACategory);
end;

constructor TCharacter.Create;
begin
  raise ENoConstructException.CreateFmt(SClassCantBeConstructed, [ClassName]);
end;

class function TCharacter.ConvertFromUtf32(AChar : UCS4Char) : UnicodeString;
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

class function TCharacter.ConvertToUtf32(const AString : UnicodeString; AIndex : Integer) : UCS4Char; overload;
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

class function TCharacter.ConvertToUtf32(const AString : UnicodeString; AIndex : Integer; out ACharLength : Integer) : UCS4Char; overload;
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

class function TCharacter.ConvertToUtf32(const AHighSurrogate, ALowSurrogate : UnicodeChar) : UCS4Char; overload;
begin
  if not IsHighSurrogate(AHighSurrogate) then
    raise EArgumentOutOfRangeException.CreateFmt(SHighSurrogateOutOfRange, [Word(AHighSurrogate)]);
  if not IsLowSurrogate(ALowSurrogate) then
    raise EArgumentOutOfRangeException.CreateFmt(SLowSurrogateOutOfRange, [Word(ALowSurrogate)]);
  Result := ToUCS4(AHighSurrogate, ALowSurrogate);
end;

class function TCharacter.GetNumericValue(AChar : UnicodeChar) : Double;
begin
  Result := GetProps(Word(AChar))^.NumericValue;
end;

class function TCharacter.GetNumericValue(
  const AString : UnicodeString;  
        AIndex  : Integer
) : Double;
var
  pu : PUC_Prop;
begin
  if (AIndex < 1) or (AIndex > Length(AString)) then
    raise EArgumentOutOfRangeException.CreateFmt(SStringIndexOutOfRange, [AIndex, Length(AString)]);
  pu := GetProps(Word(AString[AIndex]));
  if (TUnicodeCategory(pu^.Category) = TUnicodeCategory.ucSurrogate) then begin
    if not IsSurrogatePair(AString,AIndex) then
      raise EArgumentException.Create(SInvalidUnicodeCodePointSequence);
    pu := GetProps(AString[AIndex],AString[AIndex+1]);
  end;
  Result := pu^.NumericValue;
end;

class function TCharacter.GetNumericValue(aChar: UCS4Char): Double;
begin
  Result := GetProps(AChar)^.NumericValue;
end;



class function TCharacter.GetUnicodeCategory(AChar : UnicodeChar) : TUnicodeCategory;
begin   
  Result := TUnicodeCategory(GetProps(Word(AChar))^.Category);
end;

class function TCharacter.GetUnicodeCategory(
  const AString : UnicodeString;  
        AIndex  : Integer
) : TUnicodeCategory;
var
  pu : PUC_Prop;
begin   
  if (AIndex < 1) or (AIndex > Length(AString)) then
    raise EArgumentOutOfRangeException.CreateFmt(SStringIndexOutOfRange, [AIndex, Length(AString)]);
  pu := GetProps(Word(AString[AIndex]));
  if (TUnicodeCategory(pu^.Category) = TUnicodeCategory.ucSurrogate) then begin
    if not IsSurrogatePair(AString,AIndex) then
      raise EArgumentException.Create(SInvalidUnicodeCodePointSequence);
    pu := GetProps(AString[AIndex],AString[AIndex+1]);
  end;
  Result := TUnicodeCategory(pu^.Category);
end;


class function TCharacter.GetUnicodeCategory(aChar: UCS4Char): TUnicodeCategory;

var
  pr: PUC_Prop;

begin
  Result:=TUnicodeCategory.ucUnassigned;
  if Cardinal(Ord(aChar))>MAX_LEGAL_UTF32 then
    Exit;
  Pr:=GetProps(aChar);
  if assigned(pr) then
    Result:=TUnicodeCategory(Pr^.Category);
end;

class function TCharacter.IsControl(AChar : UnicodeChar) : Boolean;
begin  
  Result := (TUnicodeCategory(GetProps(Word(AChar))^.Category) = TUnicodeCategory.ucControl);
end;

class function TCharacter.IsControl(
  const AString : UnicodeString;  
        AIndex  : Integer
) : Boolean;
begin
  Result := TestCategory(AString,AIndex,TUnicodeCategory.ucControl);
end;

class function TCharacter.IsControl(aChar: UCS4Char): Boolean;
begin
  Result := (TUnicodeCategory(GetProps(aChar)^.Category) = TUnicodeCategory.ucControl);
end;

class function TCharacter.IsDigit(AChar : UnicodeChar) : Boolean;
begin 
  Result := (TUnicodeCategory(GetProps(Word(AChar))^.Category) = TUnicodeCategory.ucDecimalNumber);
end;

class function TCharacter.IsDigit(
  const AString : UnicodeString;  
        AIndex  : Integer
) : Boolean;
begin
  Result := TestCategory(AString,AIndex,TUnicodeCategory.ucDecimalNumber);
end;

class function TCharacter.IsDigit(aChar: UCS4Char): Boolean;
begin
  Result := (TUnicodeCategory(GetProps(aChar)^.Category) = TUnicodeCategory.ucDecimalNumber);
end;

class function TCharacter.IsSurrogate(AChar : UnicodeChar) : Boolean;
begin   
  Result := (TUnicodeCategory(GetProps(Word(AChar))^.Category) = TUnicodeCategory.ucSurrogate);
end;

class function TCharacter.IsSurrogate(
  const AString : UnicodeString;  
        AIndex  : Integer
) : Boolean;
begin        
  if (AIndex < 1) or (AIndex > Length(AString)) then
    raise EArgumentOutOfRangeException.CreateFmt(SStringIndexOutOfRange, [AIndex, Length(AString)]);
  Result := IsSurrogate(AString[AIndex]);
end;

class function TCharacter.IsSurrogate(aChar: UCS4Char): Boolean;
begin
  Result:=(aChar>=UCS4Char(MinSurrogate)) and (aChar <= UCS4Char(MaxSurrogate));
end;


class function TCharacter.IsHighSurrogate(AChar : UnicodeChar) : Boolean;
begin 
  Result := (TUnicodeCategory(GetProps(Word(AChar))^.Category) = TUnicodeCategory.ucSurrogate) and
            (Word(AChar) >= HIGH_SURROGATE_BEGIN) and 
            (Word(AChar) <= HIGH_SURROGATE_END);
end;

class function TCharacter.IsHighSurrogate(
  const AString : UnicodeString;  
        AIndex  : Integer
) : Boolean;
begin        
  if (AIndex < 1) or (AIndex > Length(AString)) then
    raise EArgumentOutOfRangeException.CreateFmt(SStringIndexOutOfRange, [AIndex, Length(AString)]);
  Result := IsHighSurrogate(AString[AIndex]);
end;

class function TCharacter.IsHighSurrogate(aChar: UCS4Char): Boolean;
begin
  Result:=(aChar >= UCS4Char(MinHighSurrogate)) and (aChar <= UCS4Char(MaxHighSurrogate));
end;

class function TCharacter.IsLowSurrogate(AChar : UnicodeChar) : Boolean;
begin   
  Result := (TUnicodeCategory(GetProps(Word(AChar))^.Category) = TUnicodeCategory.ucSurrogate) and
            (Word(AChar) >= LOW_SURROGATE_BEGIN) and 
            (Word(AChar) <= LOW_SURROGATE_END); 
end;

class function TCharacter.IsLowSurrogate(
  const AString : UnicodeString;  
        AIndex  : Integer
) : Boolean;
begin        
  if (AIndex < 1) or (AIndex > Length(AString)) then
    raise EArgumentOutOfRangeException.CreateFmt(SStringIndexOutOfRange, [AIndex, Length(AString)]);
  Result := IsLowSurrogate(AString[AIndex]);
end;


class function TCharacter.IsLowSurrogate(aChar: UCS4Char): Boolean;
begin
  Result := (aChar >= UCS4Char(MinLowSurrogate)) and (aChar <= UCS4Char(MaxLowSurrogate));
end;

class function TCharacter.IsSurrogatePair(
  const AHighSurrogate,
        ALowSurrogate   : UnicodeChar
) : Boolean;
begin
  Result := UnicodeIsSurrogatePair(AHighSurrogate,ALowSurrogate);
end;

class function TCharacter.IsSurrogatePair(
  const AString : UnicodeString;
        AIndex  : Integer
) : Boolean;
begin
  if (AIndex < 1) or (AIndex > Length(AString)) then
    raise EArgumentOutOfRangeException.CreateFmt(SStringIndexOutOfRange, [AIndex, Length(AString)]);
  if not IsHighSurrogate(AString[AIndex]) then begin
    Result := False;
    exit;
  end;
  if ((AIndex+1) > Length(AString)) then
    raise EArgumentOutOfRangeException.CreateFmt(SStringIndexOutOfRange, [AIndex+1, Length(AString)]);
  Result := IsSurrogatePair(AString[AIndex],AString[AIndex+1]);
end;

class function TCharacter.IsLetter(AChar : UnicodeChar) : Boolean;
begin 
  Result := (TUnicodeCategory(GetProps(Word(AChar))^.Category) in LETTER_CATEGORIES);
end;

class function TCharacter.IsLetter(
  const AString : UnicodeString;  
        AIndex  : Integer
) : Boolean;
begin
  Result := TestCategory(AString,AIndex,LETTER_CATEGORIES);
end;

class function TCharacter.IsLetter(aChar: UCS4Char): Boolean;
begin
  Result := (TUnicodeCategory(GetProps(aChar)^.Category) in LETTER_CATEGORIES);
end;


class function TCharacter.IsLetterOrDigit(AChar : UnicodeChar) : Boolean;
begin 
  Result := (TUnicodeCategory(GetProps(Word(AChar))^.Category) in LETTER_OR_DIGIT_CATEGORIES);
end;

class function TCharacter.IsLetterOrDigit(
  const AString : UnicodeString;  
        AIndex  : Integer
) : Boolean;
begin
  Result := TestCategory(AString,AIndex,LETTER_OR_DIGIT_CATEGORIES);
end;

class function TCharacter.IsLetterOrDigit(aChar: UCS4Char): Boolean;
begin
  Result := (TUnicodeCategory(GetProps(aChar)^.Category) in LETTER_OR_DIGIT_CATEGORIES);
end;

class function TCharacter.IsLower(AChar : UnicodeChar) : Boolean;
begin        
  Result := (TUnicodeCategory(GetProps(Word(AChar))^.Category) = TUnicodeCategory.ucLowercaseLetter);
end;

class function TCharacter.IsLower(
  const AString : UnicodeString;  
        AIndex  : Integer
) : Boolean;
begin
  Result := TestCategory(AString,AIndex,TUnicodeCategory.ucLowercaseLetter);
end;

class function TCharacter.IsLower(aChar: UCS4Char): Boolean;
begin
  Result := (TUnicodeCategory(GetProps(aChar)^.Category) = TUnicodeCategory.ucLowercaseLetter);
end;


class function TCharacter.IsNumber(AChar : UnicodeChar) : Boolean;
begin
  Result := (TUnicodeCategory(GetProps(Word(AChar))^.Category) in NUMBER_CATEGORIES);
end;

class function TCharacter.IsNumber(
  const AString : UnicodeString;
        AIndex  : Integer
) : Boolean;
begin
  Result := TestCategory(AString,AIndex,NUMBER_CATEGORIES);
end;

class function TCharacter.IsNumber(aChar: UCS4Char): Boolean;
begin
 Result := (TUnicodeCategory(GetProps(aChar)^.Category) in NUMBER_CATEGORIES);
end;

class function TCharacter.IsPunctuation(AChar : UnicodeChar) : Boolean;
begin
  Result := (TUnicodeCategory(GetProps(Word(AChar))^.Category) in PUNCTUATION_CATEGORIES);
end;

class function TCharacter.IsPunctuation(
  const AString : UnicodeString;
        AIndex  : Integer
) : Boolean;
begin
  Result := TestCategory(AString,AIndex,PUNCTUATION_CATEGORIES);
end;

class function TCharacter.IsPunctuation(aChar: UCS4Char): Boolean;
begin
 Result := (TUnicodeCategory(GetProps(aChar)^.Category) in PUNCTUATION_CATEGORIES);
end;

class function TCharacter.IsSeparator(AChar: UnicodeChar): Boolean;
begin
  Result := (TUnicodeCategory(GetProps(Word(AChar))^.Category) in SEPARATOR_CATEGORIES);
end;

class function TCharacter.IsSeparator(
  const AString : UnicodeString;
        AIndex  : Integer
) : Boolean;
begin
  Result := TestCategory(AString,AIndex,SEPARATOR_CATEGORIES);
end;


class function TCharacter.IsSeparator(aChar: UCS4Char): Boolean;
begin
 Result := (TUnicodeCategory(GetProps(Word(aChar))^.Category) in SEPARATOR_CATEGORIES);
end;


class function TCharacter.IsSymbol(AChar: UnicodeChar): Boolean;
begin
  Result := (TUnicodeCategory(GetProps(Word(AChar))^.Category) in SYMBOL_CATEGORIES);
end;

class function TCharacter.IsSymbol(aChar: UCS4Char): Boolean;
begin
  Result := (TUnicodeCategory(GetProps(aChar)^.Category) in SYMBOL_CATEGORIES);
end;

class function TCharacter.IsSymbol(
  const AString : UnicodeString;
        AIndex  : Integer
) : Boolean;
begin
  Result := TestCategory(AString,AIndex,SYMBOL_CATEGORIES);
end;



class function TCharacter.IsUpper(AChar : UnicodeChar) : Boolean;
begin
  Result := (TUnicodeCategory(GetProps(Word(AChar))^.Category) = TUnicodeCategory.ucUppercaseLetter);
end;

class function TCharacter.IsUpper(
  const AString : UnicodeString;
        AIndex  : Integer
) : Boolean;
begin
  Result := TestCategory(AString,AIndex,TUnicodeCategory.ucUppercaseLetter);
end;

class function TCharacter.IsUpper(aChar: UCS4Char): Boolean;
begin
  Result := (TUnicodeCategory(GetProps(Word(aChar))^.Category) = TUnicodeCategory.ucUppercaseLetter);
end;

class function TCharacter.IsWhiteSpace(AChar : UnicodeChar) : Boolean;
begin
  Result := GetProps(Word(AChar))^.WhiteSpace;
end;

class function TCharacter.IsWhiteSpace(
  const AString : UnicodeString;
        AIndex  : Integer
) : Boolean;
var
  pu : PUC_Prop;
begin
  if (AIndex < 1) or (AIndex > Length(AString)) then
    raise EArgumentOutOfRangeException.CreateFmt(SStringIndexOutOfRange, [AIndex, Length(AString)]);
  pu := GetProps(Word(AString[AIndex]));
  if (TUnicodeCategory(pu^.Category) = TUnicodeCategory.ucSurrogate) then begin
    if not IsSurrogatePair(AString,AIndex) then
      raise EArgumentException.Create(SInvalidUnicodeCodePointSequence);
    pu := GetProps(AString[AIndex],AString[AIndex+1]);
  end;
  Result := pu^.WhiteSpace;
end;

class function TCharacter.IsWhiteSpace(aChar: UCS4Char): Boolean;
begin
  Result := GetProps(Word(aChar))^.WhiteSpace;
end;

class function TCharacter.ToLower(AChar : UnicodeChar) : UnicodeChar;
begin
  Result := UnicodeChar(Word(GetProps(Word(AChar))^.SimpleLowerCase));
  if (Result = UnicodeChar(0)) then
    Result := AChar;
end;

class function TCharacter.ToLower(const AString : UnicodeString) : UnicodeString;
begin
  Result := ToLower(AString,[]);
end;

class function TCharacter.ToLower(const AString : UnicodeString; const AOptions : TCharacterOptions) : UnicodeString;
begin
  if (UnicodeToLower(
       AString,(TCharacterOption.coIgnoreInvalidSequence in AOptions),Result
       ) <> 0
     )
  then
    raise EArgumentException.Create(SInvalidUnicodeCodePointSequence);
end;

class function TCharacter.ToLower(aChar: UCS4Char): UCS4Char;
begin
  Result := UCS4Char(Cardinal(GetProps(aChar)^.SimpleLowerCase));
  if (Result = UCS4Char(0)) then
    Result := aChar;
end;

class function TCharacter.ToUpper(AChar : UnicodeChar) : UnicodeChar;
begin
  Result := UnicodeChar(Word(GetProps(Word(AChar))^.SimpleUpperCase));
  if (Result = UnicodeChar(0)) then
    Result := AChar;
end;

class function TCharacter.ToUpper(const AString : UnicodeString) : UnicodeString;
begin
  Result := ToUpper(AString,[]);
end;

class function TCharacter.ToUpper(const AString : UnicodeString; const AOptions : TCharacterOptions) : UnicodeString;
begin
  if (UnicodeToUpper(
       AString,(TCharacterOption.coIgnoreInvalidSequence in AOptions),Result
       ) <> 0
     )
  then
    raise EArgumentException.Create(SInvalidUnicodeCodePointSequence);
end;

class function TCharacter.ToUpper(aChar: UCS4Char): UCS4Char;
begin
  Result := UCS4Char(Cardinal(GetProps(aChar)^.SimpleUpperCase));
  if (Result = UCS4Char(0)) then
    Result := aChar;
end;


class function TCharacter.MaxCodePoint: Integer;
begin
  Result := MAX_LEGAL_UTF32;
end;

class function TCharacter.UnicodeDataVersion: UnicodeString;
begin
  Result := '';
end;



class function TCharacter.IsDefined(aChar: UnicodeChar): Boolean;
begin
  Result:=GetProps(Word(aChar))<>Nil;
end;

class function TCharacter.IsDefined(aChar: UCS4Char): Boolean;
begin
  Result:=GetProps(aChar)<>nil;
end;

class function TCharacter.IsDefined(const S: unicodestring; Index: Integer): Boolean;
begin
  Result:=IsDefined(S[Index]);
end;

{ TCharHelper }

class function TCharHelper.MaxCodePoint: Integer;
begin
  Result := MAX_LEGAL_UTF32;
end;

class function TCharHelper.UnicodeDataVersion: UnicodeString;
begin
  Result := '';
end;

function TCharHelper.IsHighSurrogate: Boolean;
begin
  Result:=TCharacter.IsHighSurrogate(Self);
end;

function TCharHelper.IsLowSurrogate: Boolean;
begin
  Result:=TCharacter.IsLowSurrogate(Self);
end;

function TCharHelper.IsSurrogate: Boolean;
begin
  Result:=TCharacter.IsSurrogate(Self);
end;

class function TCharHelper.IsSurrogatePair(const HighSurrogate, LowSurrogate: UnicodeChar): Boolean;
begin
  Result:=TCharacter.IsSurrogatePair(HighSurrogate, LowSurrogate);
end;

function TCharHelper.ToUCS4Char: UCS4Char;
begin
  Result:=UCS4Char(Self);
end;

class function TCharHelper.ConvertToUtf32(const S: UnicodeString; Index: Integer; out CharLength: Integer): UCS4Char;

begin
  Result:=TCharacter.ConvertToUtf32(S,Index,CharLength);
end;

class function TCharHelper.ConvertToUtf32(const S: UnicodeString; Index: Integer): UCS4Char;

begin
  Result:=TCharacter.ConvertToUtf32(S,Index);
end;

class function TCharHelper.ConvertFromUtf32(aChar: UCS4Char): UnicodeString;
begin
  Result:=TCharacter.ConvertFromUtf32(aChar);
end;

class function TCharHelper.ConvertToUtf32(const HighSurrogate, LowSurrogate: UnicodeChar): UCS4Char;
begin
  Result:=TCharacter.ConvertToUtf32(HighSurrogate,LowSurrogate);
end;

function TCharHelper.IsLetter: Boolean;

begin
  Result:=TCharacter.IsLetter(Self);
end;

function TCharHelper.IsLetterOrDigit: Boolean;
begin
  Result:=TCharacter.IsLetterOrDigit(Self);
end;

class function TCharHelper.IsLetter(const S: UnicodeString; Index: Integer): Boolean;

begin
  Result:=TCharacter.IsLetter(S,Index);
end;

class function TCharHelper.IsLetterOrDigit(const S: UnicodeString; Index: Integer): Boolean;

begin
  Result:=TCharacter.IsLetterOrDigit(S,Index);
end;

class function TCharHelper.IsControl(const S: UnicodeString; Index: Integer): Boolean;

begin
  Result:=TCharacter.IsControl(S,Index);
end;

class function TCharHelper.IsDefined(const S: UnicodeString; Index: Integer): Boolean;

begin
  Result:=TCharacter.IsDefined(S,Index);
end;

function TCharHelper.IsControl: Boolean;
begin
  Result:=TCharacter.IsControl(Self);
end;

function TCharHelper.IsDefined: Boolean;
begin
  Result:=TCharacter.IsDefined(Self);
end;

function TCharHelper.IsDigit: Boolean;

begin
  Result:=TCharacter.IsDigit(Self);
end;

class function TCharHelper.IsDigit(const S: UnicodeString; Index: Integer): Boolean;

begin
  Result:=TCharacter.IsDigit(S,Index);
end;

function TCharHelper.IsInArray(const SomeChars: array of UnicodeChar): Boolean;
var
  AChar: UnicodeChar;
begin
  for AChar in SomeChars do
    if AChar=Self then
      Exit(True);
  Result := False;
end;

function TCharHelper.IsLower: Boolean;
begin
  Result:=TCharacter.IsLower(Self);
end;

function TCharHelper.GetUnicodeCategory: TUnicodeCategory;
begin
  Result:=TCharacter.GetUnicodeCategory(Self);
end;

function TCharHelper.GetNumericValue: Double;
begin
  Result:=TCharacter.GetNumericValue(Self);
end;

class function TCharHelper.GetNumericValue(const S: UnicodeString; Index: Integer): Double;
begin
  Result:=TCharacter.GetNumericValue(S,Index);
end;

class function TCharHelper.GetUnicodeCategory(const S: UnicodeString; Index: Integer): TUnicodeCategory;
begin
  Result:=TCharacter.GetUnicodeCategory(S,Index);
end;

class function TCharHelper.IsHighSurrogate(const S: UnicodeString; Index: Integer): Boolean;
begin
  Result:=TCharacter.IsHighSurrogate(S,Index);
end;

class function TCharHelper.IsInArray(const S: UnicodeString; Index: Integer; const SomeChars: array of UnicodeChar): Boolean;
begin
  Result:=S[Index].IsInArray(SomeChars);
end;

class function TCharHelper.IsLower(const S: UnicodeString; Index: Integer): Boolean;

begin
  Result:=TCharacter.IsLower(S,Index);
end;

class function TCharHelper.IsLowSurrogate(const S: UnicodeString; Index: Integer): Boolean;
begin
  Result:=S[Index].IsLowSurrogate;
end;

function TCharHelper.IsNumber: Boolean;
begin
  Result:=TCharacter.IsNumber(Self);
end;

function TCharHelper.IsPunctuation: Boolean;
begin
  Result:=TCharacter.IsPunctuation(Self);
end;

class function TCharHelper.IsNumber(const S: UnicodeString; Index: Integer): Boolean;

begin
  Result:=TCharacter.IsNumber(S,Index);
end;

class function TCharHelper.IsPunctuation(const S: UnicodeString; Index: Integer): Boolean;

begin
  Result:=TCharacter.IsPunctuation(S,Index);
end;

function TCharHelper.IsSeparator: Boolean;
begin
  Result:=TCharacter.IsSeparator(Self);
end;

class function TCharHelper.IsSeparator(const S: UnicodeString; Index: Integer): Boolean;

begin
  Result:=TCharacter.IsSeparator(S,Index);
end;

class function TCharHelper.IsSurrogate(const S: UnicodeString; Index: Integer): Boolean;
begin
  Result:=TCharacter.IsSurrogate(S,Index);
end;

class function TCharHelper.IsSurrogatePair(const S: UnicodeString; Index: Integer): Boolean;
begin
  Result:=TCharacter.IsSurrogatePair(S,Index);
end;

function TCharHelper.IsSymbol: Boolean;
begin
  Result:=TCharacter.IsSymbol(Self);
end;

class function TCharHelper.IsSymbol(const S: UnicodeString; Index: Integer): Boolean;

begin
  Result:=TCharacter.IsSymbol(S,Index);
end;

class function TCharHelper.IsUpper(const S: UnicodeString; Index: Integer): Boolean;

begin
  Result:=TCharacter.IsUpper(S,Index);
end;

function TCharHelper.IsUpper: Boolean;
begin
  Result:=TCharacter.IsUpper(Self);
end;

class function TCharHelper.IsWhiteSpace(const S: UnicodeString; Index: Integer): Boolean;

begin
  Result:=TCharacter.IsWhiteSpace(S,Index);
end;

function TCharHelper.ToLower: UnicodeChar;

begin
  Result:=TCharacter.ToLower(Self);
end;

class function TCharHelper.ToLower(const S: UnicodeString): UnicodeString;

begin
  Result:=TCharacter.ToLower(S);
end;


function TCharHelper.ToUpper: UnicodeChar;
begin
  Result:=TCharacter.ToUpper(Self);
end;

class function TCharHelper.ToUpper(const S: UnicodeString): UnicodeString;

begin
  Result:=TCharacter.ToUpper(S);
end;


function TCharHelper.IsWhiteSpace: Boolean;
begin
  Result:=TCharacter.IsWhiteSpace(Self);
end;


class function TCharHelper.IsLetterOrDigit(aChar: UCS4Char): Boolean;
begin
  Result:=TCharacter.IsLetterOrDigit(aChar);
end;

class function TCharHelper.IsControl(aChar: UCS4Char): Boolean;
begin
  Result:=TCharacter.IsControl(aChar);
end;

class function TCharHelper.IsDefined(aChar: UCS4Char): Boolean;
begin
  Result:=TCharacter.IsDefined(aChar);
end;

class function TCharHelper.IsDigit(aChar: UCS4Char): Boolean;
begin
  Result:=TCharacter.IsDigit(aChar);
end;

class function TCharHelper.IsHighSurrogate(aChar: UCS4Char): Boolean;
begin
  Result:=TCharacter.IsHighSurrogate(aChar);
end;

class function TCharHelper.IsLetter(aChar: UCS4Char): Boolean;
begin
  Result:=TCharacter.IsLetter(aChar);
end;

class function TCharHelper.IsLowSurrogate(aChar: UCS4Char): Boolean;
begin
  Result:=TCharacter.IsLowSurrogate(aChar);
end;

class function TCharHelper.IsSurrogate(Surrogate: UCS4Char): Boolean;
begin
  Result:=TCharacter.IsSurrogate(Surrogate);
end;

class function TCharHelper.GetUnicodeCategory(aChar: UCS4Char): TUnicodeCategory;
begin
  Result:=TCharacter.GetUnicodeCategory(aChar);
end;

class function TCharHelper.GetNumericValue(aChar: UCS4Char): Double;
begin
  Result:=TCharacter.GetNumericValue(aChar);
end;

class function TCharHelper.IsLower(aChar: UCS4Char): Boolean;
begin
  Result:=TCharacter.IsLower(aChar);
end;

class function TCharHelper.IsNumber(aChar: UCS4Char): Boolean;
begin
  Result:=TCharacter.IsNumber(aChar);
end;

class function TCharHelper.IsPunctuation(aChar: UCS4Char): Boolean;
begin
  Result:=TCharacter.IsPunctuation(aChar);
end;

class function TCharHelper.IsSeparator(aChar: UCS4Char): Boolean;
begin
  Result:=TCharacter.IsSeparator(aChar);
end;

class function TCharHelper.IsSymbol(aChar: UCS4Char): Boolean;
begin
  Result:=TCharacter.IsSymbol(aChar);
end;

class function TCharHelper.IsUpper(aChar: UCS4Char): Boolean;
begin
  Result:=TCharacter.IsUpper(aChar);
end;

class function TCharHelper.ToLower(aChar: UCS4Char): UCS4Char;
begin
  Result:=TCharacter.ToLower(aChar);
end;

class function TCharHelper.ToUpper(aChar: UCS4Char): UCS4Char;
begin
  Result:=TCharacter.ToUpper(aChar);
end;

class function TCharHelper.IsWhiteSpace(aChar: UCS4Char): Boolean;
begin
  Result:=TCharacter.IsWhiteSpace(aChar);
end;



end.
