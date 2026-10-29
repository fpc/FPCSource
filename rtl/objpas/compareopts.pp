{
  Character mappings and digit comparison for the TCompareOption values, shared by widestring managers.
  This file is part of the Free Pascal run time library.
  See the file COPYING.FPC, included in this distribution, for details about the copyright.
}
{$IFNDEF FPC_DOTTEDUNITS}
unit compareopts;
{$ENDIF FPC_DOTTEDUNITS}

{$MODE objfpc}

interface

type
  // Compares two strings; the sign of the result gives their order.
  TStringCollateFunc = function(const s1, s2 : UnicodeString) : PtrInt;

// Normal width form of a fullwidth or halfwidth character, aChar itself for other characters.
function NormalWidthChar(aChar : UnicodeChar) : UnicodeChar;
// Hiragana for a katakana character, aChar itself for other characters.
function HiraganaChar(aChar : UnicodeChar) : UnicodeChar;
// Base letter of a Latin letter with stroke (Ø ø Đ đ Ħ ħ Ł ł), aChar itself for other characters.
function StrokelessChar(aChar : UnicodeChar) : UnicodeChar;
{ Compare s1 and s2 with runs of ASCII digits compared by numeric value and the text
  between them with aCollate; numerically equal strings are compared with aCollate. }
function CompareDigitsAsNumbers(const s1, s2 : UnicodeString; aCollate : TStringCollateFunc) : PtrInt;

implementation

const
  // Fullwidth forms of U+FF61..U+FF9F, from the NFKC mappings of Unicode 15.0.0
  HalfwidthKana : array[$FF61..$FF9F] of Word = (
    $3002,$300C,$300D,$3001,$30FB,$30F2,$30A1,$30A3,$30A5,$30A7,
    $30A9,$30E3,$30E5,$30E7,$30C3,$30FC,$30A2,$30A4,$30A6,$30A8,
    $30AA,$30AB,$30AD,$30AF,$30B1,$30B3,$30B5,$30B7,$30B9,$30BB,
    $30BD,$30BF,$30C1,$30C4,$30C6,$30C8,$30CA,$30CB,$30CC,$30CD,
    $30CE,$30CF,$30D2,$30D5,$30D8,$30DB,$30DE,$30DF,$30E0,$30E1,
    $30E2,$30E4,$30E6,$30E8,$30E9,$30EA,$30EB,$30EC,$30ED,$30EF,
    $30F3,$309B,$309C);


function NormalWidthChar(aChar : UnicodeChar) : UnicodeChar;

begin
  case Word(aChar) of
    $FF01..$FF5E: Result:=UnicodeChar(Word(aChar)-$FEE0);
    $3000: Result:=UnicodeChar($0020);
    $FF61..$FF9F: Result:=UnicodeChar(HalfwidthKana[Word(aChar)]);
    $FFE0: Result:=UnicodeChar($00A2);
    $FFE1: Result:=UnicodeChar($00A3);
    $FFE2: Result:=UnicodeChar($00AC);
    $FFE3: Result:=UnicodeChar($00AF);
    $FFE4: Result:=UnicodeChar($00A6);
    $FFE5: Result:=UnicodeChar($00A5);
    $FFE6: Result:=UnicodeChar($20A9);
  else
    Result:=aChar;
  end;
end;


function HiraganaChar(aChar : UnicodeChar) : UnicodeChar;

begin
  if (Word(aChar)>=$30A1) and (Word(aChar)<=$30F6) then
    Result:=UnicodeChar(Word(aChar)-$60)
  else
    Result:=aChar;
end;


function StrokelessChar(aChar : UnicodeChar) : UnicodeChar;

begin
  case Word(aChar) of
    $00D8: Result:='O';
    $00F8: Result:='o';
    $0110: Result:='D';
    $0111: Result:='d';
    $0126: Result:='H';
    $0127: Result:='h';
    $0141: Result:='L';
    $0142: Result:='l';
  else
    Result:=aChar;
  end;
end;


// True if aChar is an ASCII digit
function IsAsciiDigit(aChar : UnicodeChar) : Boolean; inline;

begin
  Result:=(aChar>='0') and (aChar<='9');
end;


function CompareDigitsAsNumbers(const s1, s2 : UnicodeString; aCollate : TStringCollateFunc) : PtrInt;

var
  i1, i2, lStart1, lStart2, lLen1, lLen2, k : SizeInt;

begin
  i1:=1;
  i2:=1;
  while (i1<=Length(s1)) and (i2<=Length(s2)) do
    begin
    lStart1:=i1;
    lStart2:=i2;
    if IsAsciiDigit(s1[i1]) and IsAsciiDigit(s2[i2]) then
      begin
      while (i1<=Length(s1)) and IsAsciiDigit(s1[i1]) do
        Inc(i1);
      while (i2<=Length(s2)) and IsAsciiDigit(s2[i2]) do
        Inc(i2);
      while (lStart1<i1-1) and (s1[lStart1]='0') do
        Inc(lStart1);
      while (lStart2<i2-1) and (s2[lStart2]='0') do
        Inc(lStart2);
      lLen1:=i1-lStart1;
      lLen2:=i2-lStart2;
      if lLen1<>lLen2 then
        Exit(lLen1-lLen2);
      for k:=0 to lLen1-1 do
        if s1[lStart1+k]<>s2[lStart2+k] then
          Exit(Ord(s1[lStart1+k])-Ord(s2[lStart2+k]));
      end
    else if not IsAsciiDigit(s1[i1]) and not IsAsciiDigit(s2[i2]) then
      begin
      while (i1<=Length(s1)) and not IsAsciiDigit(s1[i1]) do
        Inc(i1);
      while (i2<=Length(s2)) and not IsAsciiDigit(s2[i2]) do
        Inc(i2);
      Result:=aCollate(Copy(s1,lStart1,i1-lStart1),Copy(s2,lStart2,i2-lStart2));
      if Result<>0 then
        Exit;
      end
    else
      Exit(aCollate(Copy(s1,i1,Length(s1)),Copy(s2,i2,Length(s2))));
    end;
  if (i1<=Length(s1)) or (i2<=Length(s2)) then
    Result:=aCollate(Copy(s1,i1,Length(s1)),Copy(s2,i2,Length(s2)))
  else
    Result:=aCollate(s1,s2);
end;

end.
