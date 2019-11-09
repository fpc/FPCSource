unit utstringhelp;

{$mode objfpc}{$H+}
//{$modeswitch advancedrecords}
//{$modeswitch typehelpers}

interface

uses
  Classes, SysUtils;

implementation

uses punit, utrtl;

Const
  TBI = 'To be implemented';

Function TestCompare : String;

Var
  r : Integer;

begin
  Result:='';
  // Simple cases
  R:=AnsiString.Compare('A','B');
  if not AssertTrue('1. Simple Compare strings (A,B) :'+IntToStr(R)+'<0',R<0) then exit;
  R:=AnsiString.Compare('B','A');
  if not AssertTrue('2. Simple Compare strings (B,A) :'+IntToStr(R)+'>0',R>0) then exit;
  R:=AnsiString.Compare('A','A');
  if not AssertTrue('3. Simple Compare strings (A,A) :'+IntToStr(R)+'=0',R=0) then exit;
  R:=AnsiString.Compare('A','a', true);
  if not AssertTrue('4. Simple ignore case Compare strings (A,a) : '+IntToStr(R)+'=0',R=0) then exit;
  R:=AnsiString.Compare('b','a',True);
  if not AssertTrue('5. Simple ignore case Compare strings (b,a) : '+IntToStr(R)+'>0',R>0) then exit;
  R:=AnsiString.Compare('A','a',[coIgnoreCase]);
  if not AssertTrue('6. [coIgnoreCase] Compare strings (A,a) : '+IntToStr(R)+'=0',R=0) then exit;
  R:=AnsiString.Compare('b','a',[coIgnoreCase]);
  if not AssertTrue('7. [coIgnoreCase] Compare strings (b,a) : '+IntToStr(R)+'>0',R>0) then exit;
  // Check whether index is observed.
  R:=AnsiString.Compare('AA',1,'AB',1,1);
  if not AssertTrue('8. Compare(''AA'',1,''AB'',1,1) :'+IntToStr(R)+'<0',R<0) then exit;
  R:=AnsiString.Compare('AB',1,'AA',1,1);
  if not AssertTrue('9. Compare(''AB'',1,''AA'',1,1) :'+IntToStr(R)+'>0',R>0) then exit;
  R:=AnsiString.Compare('AA',1,'AA',1,1);
  if not AssertTrue('10. Compare(''AA'',1,''AA'',1,1) :'+IntToStr(R)+'=0',R=0) then exit;
  // Make sure only len chars are used.
  R:=AnsiString.Compare('AAC',1,'ABD',1,1);
  if not AssertTrue('11. Compare(''AAC'',1,''ABD'',1,1) :'+IntToStr(R)+'<0',R<0) then exit;
  R:=AnsiString.Compare('ABC',1,'AAD',1,1);
  if not AssertTrue('12 Compare(''ABC'',1,''AAD'',1,1) :'+IntToStr(R)+'>0',R>0) then exit;
  R:=AnsiString.Compare('AAC',1,'AAD',1,1);
  if not AssertTrue('13. Compare(''AAC'',1,''AAD'',1,1) :'+IntToStr(R)+'=0',R=0) then exit;
  // Index, case insensitive
  R:=AnsiString.Compare('AA',1,'Aa',1,1,true);
  if not AssertTrue('14. Compare(''AA'',1,''Aa'',1,1,true) : '+IntToStr(R)+'=0',R=0) then exit;
  R:=AnsiString.Compare('Ab',1,'Aa',1,1,True);
  if not AssertTrue('15. Compare(''Ab'',1,''Aa'',1,1,True) : '+IntToStr(R)+'>0',R>0) then exit;
  R:=AnsiString.Compare('A',1,'a',1,1,[coIgnoreCase]);
  if not AssertTrue('16. Compare(''A'',1,''a'',1,1,[coIgnoreCase]) : '+IntToStr(R)+'=0',R=0) then exit;
  // Index, maxlen, case insensitive
  R:=AnsiString.Compare('AAC',1,'AaD',1,1,true);
  if not AssertTrue('17. Compare(''AAC'',1,''AaD'',1,1,true) : '+IntToStr(R)+'=0',R=0) then exit;
  R:=AnsiString.Compare('AbC',1,'AaD',1,1,True);
  if not AssertTrue('18. Compare(''AbC'',1,''AaD'',1,1,True) : '+IntToStr(R)+'>0',R>0) then exit;
  R:=AnsiString.Compare('AAC',1,'AaD',1,1,[coIgnoreCase]);
  if not AssertTrue('19. Compare(''AAC'',1,''AaD'',1,1,[coIgnoreCase]) : '+IntToStr(R)+'=0',R=0) then exit;
end;

Function TestCompareOrdinal : String;

Var
  r : Integer;

begin
  Result:='';
  // Simple
  R:=AnsiString.CompareOrdinal('A','B');
  if not AssertTrue('1. Simple Compare strings (A,B) :'+IntToStr(R)+'<0',R<0) then exit;
  R:=AnsiString.CompareOrdinal('B','A');
  if not AssertTrue('2. Simple Compare strings (B,A) :'+IntToStr(R)+'>0',R>0) then exit;
  R:=AnsiString.CompareOrdinal('A','A');
  if not AssertTrue('3. Simple Compare strings (A,A) :'+IntToStr(R)+'=0',R=0) then exit;
  // Index
  R:=AnsiString.CompareOrdinal('AA',1,'AB',1,1);
  if not AssertTrue('4. Simple Compare strings (AA,1,AB,1,1) :'+IntToStr(R)+'<0',R<0) then exit;
  R:=AnsiString.CompareOrdinal('AB',1,'AA',1,1);
  if not AssertTrue('5. Simple Compare strings (AB,1,AA,1,1) :'+IntToStr(R)+'>0',R>0) then exit;
  R:=AnsiString.CompareOrdinal('AA',1,'AA',1,1);
  if not AssertTrue('6. Simple Compare strings (AA,1,AA,1,1) :'+IntToStr(R)+'=0',R=0) then exit;
  // Index, maxlen
  R:=AnsiString.CompareOrdinal('AAC',1,'ABD',1,1);
  if not AssertTrue('7. Simple Compare strings (AAC,1,ABD,1,1) :'+IntToStr(R)+'<0',R<0) then exit;
  R:=AnsiString.CompareOrdinal('ABC',1,'AAD',1,1);
  if not AssertTrue('8. Simple Compare strings (ABC,1,AAD,1,1) :'+IntToStr(R)+'>0',R>0) then exit;
  R:=AnsiString.CompareOrdinal('AAD',1,'AAD',1,1);
  if not AssertTrue('9. Simple Compare strings (AAC,1,AAD,1,1) :'+IntToStr(R)+'=0',R=0) then exit;
end;

Function TestCompareText : String;

Var
  r : Integer;

begin
  Result:='';
  R:=AnsiString.CompareText('A','B');
  if not AssertTrue('1. Simple Compare strings (A,B) :'+IntToStr(R)+'<0',R<0) then exit;
  R:=AnsiString.CompareText('B','A');
  if not AssertTrue('Simple Compare strings (B,A) :'+IntToStr(R)+'>0',R>0) then exit;
  R:=AnsiString.CompareText('A','A');
  if not AssertTrue('Simple Compare strings (A,A) :'+IntToStr(R)+'=0',R=0) then exit;
  //
  R:=AnsiString.CompareText('A','b');
  if not AssertTrue('Simple Compare strings (A,b) :'+IntToStr(R)+'<0',R<0) then exit;
  R:=AnsiString.CompareText('B','a');
  if not AssertTrue('Simple Compare strings (B,a) :'+IntToStr(R)+'>0',R>0) then exit;
  R:=AnsiString.CompareText('A','a');
  if not AssertTrue('Simple Compare strings (A,a) :'+IntToStr(R)+'=0',R=0) then exit;
end;

Function TestCopy : String;

var
  A,S : String;

begin
  Result:='';
  A:=TBI;
  S:=AnsiString.Copy(A);
  if not AssertEquals('Copy creates equal copy',A,S) then exit;
end;

Function TestCreate : String;

Var
  A : String;

begin
  Result:='';
  A:=AnsiString.Create('*',5);
  if not AssertEquals('Create with char and length','*****',A) then exit;
  A:=AnsiString.Create(['a','b','c','d','e']);
  if not AssertEquals('Create with array of char','abcde',A) then exit;
  A:=AnsiString.Create(['a','b','c','d','e'],1,3);
  if not AssertEquals('Create with array of char and index, len','bcd',A) then exit;
end;

Function TestEndsText : String;

begin
  Result:='';
  if not AssertTrue('1. EndsText, correct',AnsiString.EndsText('be','to be or not to be')) then exit;
  if not AssertTrue('2. EndsText, correct, case insensitive',AnsiString.EndsText('BE','to be or not to be')) then exit;
  if not AssertFalse('3. EndsText, not correct',AnsiString.EndsText('de','to be or not to be')) then exit;
  if not AssertFalse('4. EndsText, empty',Ansistring.EndsText('','to be or not to be')) then exit;
end;

Function TestEquals : String;

Var
  A,B : String;

begin
  Result:='';
  A:='Yes';
  B:='No';
  if not AssertFalse('1. Equals(A,B)',AnsiString.Equals(A,B)) then exit;
  B:='Yes';
  if not AssertTrue('2. Equals(A,B)',AnsiString.Equals(A,B)) then exit;
  B:='No';
  if not AssertFalse('3. A.Equals(B)',A.Equals(B)) then exit;
  B:='Yes';
  if not AssertTrue('4. A.Equals(B)',A.Equals(B)) then exit;
end;

Function TestFormat : String;


begin
  Result:='';
  if not AssertEquals('1. Format as class function','A1 BC', AnsiString.Format('A%d B%s',[1,'C'])) then exit;
  if not AssertEquals('2. Format function','A1 BC', 'A%d B%s'.Format([1,'C'])) then exit;
end;

Function TestIsNullOrEmpty : String;

begin
  Result:='';
  If Not AssertTrue('1. Empty string returns true',AnsiString.IsNullOrEmpty('')) then exit;
end;

Function IsNullOrWhiteSpace : String;

Var
  C : Char;
begin
  Result:='';
  If Not AssertTrue('2. Empty string returns true',AnsiString.IsNullOrEmpty('')) then exit;
  For C:=#1 to #32 do
    If Not AssertTrue('Char '+IntToStr(Ord(C))+' string returns true',AnsiString.IsNullOrEmpty(C)) then exit;
end;

Function TestJoin : String;

Var
  C : Char;
  Cu : Currency;
  I6 : Int64;
  Q : QWord;
  F : Extended;
  W : Widestring;
  U : UnicodeString;
  S : AnsiString;
  P : PChar;
  PW : PWideChar;
// Variants unit used when enabling this.
//  V : Variant;



begin
  Result:='';
  C:='3';
  Cu:=12.3;
  I6:=1234;
  F:=1234.5;
  Q:=123456;
  S:='AS';
  W:='WS';
  U:='US';
  P:=PChar(S);
  PW:=PWideChar(U);
//  V:='Var';
  if not AssertEquals('1 element','ABC',AnsiString.Join(' ',['ABC'])) then exit;
  if not AssertEquals('2 elements','ABC DEF',AnsiString.Join(' ',['ABC','DEF'])) then exit;
  if not AssertEquals('3 elements','ABC DEF GHI',AnsiString.Join(' ',['ABC','DEF','GHI'])) then exit;
  if not AssertEquals('5 elements, index','ABC DEF GHI',AnsiString.Join(' ',['NONO','ABC','DEF','GHI','nono'],1,3)) then exit;
{   if not AssertEquals('Array of const','ABC 1 True 3 12.3 1234 '+FloatToStr(F)+' 123456 AS US WS AS US Var',
                      AnsiString.Join(' ',['ABC',1,True,C,CU,I6,F,Q,S,U,W,P,PW,V])) then exit;}
end;

Function TestLowerCase : String;

begin
  Result:='';
  if not AssertEquals('1. Simple Lowercase','abc',AnsiString.Lowercase('ABC')) then exit;
end;

Function TestParse : String;

Var
  E : Extended;
begin
  Result:='';
  E:=12.3;
  if not AssertEquals('Boolean','-1',AnsiString.Parse(True))then exit;
  if not AssertEquals('Integer','12',AnsiString.Parse(Integer(12)))then exit;
  if not AssertEquals('Int64','45',AnsiString.Parse(Int64(45)))then exit;
  if not AssertEquals('Extended',FloatToStr(E),AnsiString.Parse(E)) then exit;
end;

Function TestToBoolean : String;

begin
  Result:='';
  If not AssertTrue('Class function, true',AnsiString.ToBoolean('True')) then exit;
  If not AssertTrue('function 1',AnsiString.ToBoolean('1')) then exit;
  If not AssertFalse('Class function false',AnsiString.ToBoolean('False')) then exit;
  If not AssertFalse('function 0',AnsiString.ToBoolean('0')) then exit;
end;

Function TestToDouble : String;

begin
  Result:='';
  If not AssertEquals('Class function, 0',0.0,AnsiString.ToDouble('0.0')) then exit;
  If not AssertEquals('Class function, 1.2',1.2,AnsiString.ToDouble('1.2')) then exit;
  If not AssertEquals('function, 0',0.0,'0.0'.ToDouble) then exit;
  If not AssertEquals('function, 1.2',1.2,'1.2'.ToDouble) then exit;
end;

Function TestToExtended : String;

begin
  Result:='';
  If not AssertEquals('Class function, 0',0.0,AnsiString.ToExtended('0.0')) then exit;
  If not AssertEquals('Class function, 1.2',1.2,AnsiString.ToExtended('1.2')) then exit;
  If not AssertEquals('function, 0',0.0,'0.0'.ToExtended) then exit;
  If not AssertEquals('function, 1.2',1.2,'1.2'.ToExtended) then exit;
end;

Function TestToInt64 : String;

begin
  Result:='';
  If not AssertEquals('Class function, 0',0,AnsiString.ToInt64('0')) then exit;
  If not AssertEquals('Class function, 12',12,AnsiString.ToInt64('12')) then exit;
  If not AssertEquals('Class function, 1234567890123',1234567890123,AnsiString.ToInt64('1234567890123')) then exit;
  // 2 characters because it does not work on length 1, compiler assumes Char as in Delphi
  If not AssertEquals('Class function, 0',0,'00'.ToInt64) then exit;
  If not AssertEquals('Class function, 12',12,'12'.ToInt64) then exit;
  If not AssertEquals('Class function, 1234567890123',1234567890123,'1234567890123'.ToInt64) then exit;
end;

Function TestToInteger : String;

begin
  Result:='';
  If not AssertEquals('Class function, 0',0,AnsiString.ToInteger('0')) then exit;
  If not AssertEquals('Class function, 12',12,AnsiString.ToInteger('12')) then exit;
  If not AssertEquals('Class function, 123456789',123456789,AnsiString.ToInteger('123456789')) then exit;
  // 2 characters because it does not work on length 1, compiler assumes Char as in Delphi
  If not AssertEquals('Class function, 0',0,'00'.ToInteger) then exit;
  If not AssertEquals('Class function, 12',12,'12'.ToInteger) then exit;
  If not AssertEquals('Class function, 123456789',123456789,'123456789'.ToInteger) then exit;
end;

Function TestToSingle : String;

begin
  Result:='';
  If not AssertEquals('Class function, 0',Single(0.0),AnsiString.ToSingle('0.0')) then exit;
  If not AssertEquals('Class function, 1.2',Single(1.2),AnsiString.ToSingle('1.2')) then exit;
  If not AssertEquals('function, 0',Single(0.0),'0.0'.ToSingle) then exit;
  If not AssertEquals('function, 1.2',Single(1.2),'1.2'.ToSingle) then exit;
end;

Function TestUppercase : String;

begin
  Result:='';
  if not AssertEquals('1. Simple Lowercase','ABC',AnsiString.UpperCase('abc')) then exit;
end;

Function TestCompareTo : String;

begin
  Result:='';
  // 2 characters because it does not work on length 1, compiler assumes Char as in Delphi
  if not AssertTrue('1. A<B','AA'.CompareTo('AB')<0) then exit;
  if not AssertTrue('1. A=A','AA'.CompareTo('AA')=0) then exit;
  if not AssertTrue('1. B>A','AB'.CompareTo('AA')>0) then exit;
end;

Function TestContains : String;

begin
  Result:='';
  if not AssertTrue('ABC contains AB','ABC'.Contains('AB')) then exit;
  if not AssertTrue('ABC contains BC','ABC'.Contains('BC')) then exit;
  if not AssertTrue('ABC contains B','ABC'.Contains('B')) then exit;
  if not AssertFalse('ABC does not contain empty','ABC'.Contains('')) then exit;
  if not AssertFalse('ABC does not contain DEF','ABC'.Contains('DEF')) then exit;
  if not AssertFalse('ABC does not contain a','ABC'.Contains('a')) then exit;
end;

Function TestCopyTo : String;

Type
  TCharArray = Array Of Char;

Const
  Res1 : Array[0..4] of Char = ('T','o',' ','b','e');

Var
  S : AnsiString;
  A : TCharArray;
  I : Integer;

begin
  Result:='';
  A:=Default(TCharArray);
  S:=TBI;
  SetLength(A,5);
  S.CopyTo(0,A,0,5);
  For I:=0 to 4 do
    if not AssertEquals('Zero indexes, Char '+IntToStr(i),Res1[I],A[I]) then exit;
  S:='AB'+S;
  S.CopyTo(2,A,0,5);
  For I:=0 to 4 do
    if not AssertEquals('Source index, zero dest index, Char '+IntToStr(i),Res1[I],A[I]) then exit;
  SetLength(A,8);
  S.CopyTo(2,A,3,5);
  For I:=0 to 4 do
    if not AssertEquals('Source index, dest index, Char '+IntToStr(i),Res1[I],A[I+3]) then exit;
end;

Function TestCountChar : String;

begin
  Result:='';
  if not AssertEquals('Empty string',0,''.CountChar(' ')) then exit;
  if not AssertEquals('Start and end ',2,' ** '.CountChar(' ')) then exit;
  if not AssertEquals('Middle',2,'*  *'.CountChar(' ')) then exit;
end;

Function TestDeQuotedString : String;

Const
  C =  TBI;
  C2 =  'To be ''implemented';
  C3 =  'To be "implemented';

Var
  S : String;

begin
  Result:='';
  S:=''''+C+'''';
  If Not AssertEquals('Simple case of '+S,C,S.DequotedString) then exit;
  S:=''''+StringReplace(C2,'''','''''',[rfReplaceAll])+'''';
  If Not AssertEquals('Quoted case of '+S,C2,S.DequotedString) then exit;
  S:='"'+C+'"';
  If Not AssertEquals('Simple case of '+S,C,S.DequotedString('"')) then exit;
  S:='"'+StringReplace(C3,'"','""',[rfReplaceAll])+'"';
  If Not AssertEquals('Quoted case of '+S,C3,S.DequotedString('"')) then exit;
end;

Function TestEndsWith : String;

Var
  S : AnsiString;

begin
  Result:='';
  S:=TBI;
  If not AssertTrue('length 0', S.EndsWith('')) then exit;
  If not AssertTrue('length 1', S.EndsWith('d')) then exit;
  If not AssertTrue('length 2', S.EndsWith('ed')) then exit;
  If not AssertTrue('equal length (same)', S.EndsWith(S)) then exit;
  If not AssertFalse('length+2', S.EndsWith(S+'ed')) then exit;
  If not AssertFalse('Random string', S.EndsWith('erd')) then exit;
  If not AssertTrue('match case ', S.EndsWith('ed',False)) then exit;
  If not AssertFalse('match case, no match ', S.EndsWith('eD',False)) then exit;
  If not AssertTrue('no match case, match ', S.EndsWith('ED',True)) then exit;
  If not AssertFalse('no match case, no match ', S.EndsWith('DED',True)) then exit;
end;
Function TestGetHashCode : String;
{
Function GetHashCode: Integer;
}

Var
  S : AnsiString;

begin
  Result:='';
  S:=TBI;
  if not AssertTrue('Nonzero hashcode',S.GetHashCode<>0) then exit;
  // A more meaningful test would be nice...
end;

Function TestIndexOf : String;

Var
  S : String;

begin
  Result:='';
  S:=TBI+' To perfection';
  // Char based.
  If not AssertEquals('Char, Nonexisting returns -1',-1,S.IndexOf('a')) then exit;
  If not AssertEquals('Char, Existing, zero based',0,S.IndexOf('T')) then exit;
  If not AssertEquals('Char, Case sensitive',-1,S.IndexOf('I')) then exit;
  If not AssertEquals('Char, using start index',10,S.IndexOf('e',5)) then exit;
  If not AssertEquals('Char, using start index and count, not found',-1,S.IndexOf('e',5,5)) then exit;
  If not AssertEquals('Char, using start index and count,found',10,S.IndexOf('e',5,6)) then exit;
  // String based.
  If not AssertEquals('String, Nonexisting returns -1',-1,S.IndexOf('a')) then exit;
  If not AssertEquals('String, zero based',0,S.IndexOf('T')) then exit;
  If not AssertEquals('String, case sensitive',-1,S.IndexOf('I')) then exit;
  If not AssertEquals('String, using start index',18,S.IndexOf('To',2)) then exit;
  If not AssertEquals('String, using start index and count ',-1,S.IndexOf('To be',2,4)) then exit;
  If not AssertEquals('String, using start index and count (partial overlap)',-1,S.IndexOf('To be',16,4)) then exit;
end;

Function TestIndexOfAny : String;

Var
  S : String;
  ES : Array of Char;
begin
  Result:='';
  S:=TBI;
  es:=[];
  // Just a set
  SetLength(ES,0);
  if not AssertEquals('Empty set',-1,S.IndexOfAny(ES)) then exit;
  if not AssertEquals('Single char in set, no match',-1,S.IndexOfAny(['a'])) then exit;
  if not AssertEquals('Single char in set, no match (wrong case)',-1,S.IndexOfAny(['O'])) then exit;
  if not AssertEquals('2 chars in set, no match',-1,S.IndexOfAny(['a','z'])) then exit;
  if not AssertEquals('Single char in set, match',4,S.IndexOfAny(['e'])) then exit;
  if not AssertEquals('2 chars in set, 2nd matches',3,S.IndexOfAny(['a','b'])) then exit;
  // Start index
  if not AssertEquals('StartIndex, Empty set',-1,S.IndexOfAny(ES,2)) then exit;
  if not AssertEquals('StartIndex, Single char in set, no match',-1,S.IndexOfAny(['a'],2)) then exit;
  if not AssertEquals('StartIndex, Single char in set, no match (wrong case)',-1,S.IndexOfAny(['O'],1)) then exit;
  if not AssertEquals('StartIndex, Single char in set, no match (index too big)',-1,S.IndexOfAny(['o'],2)) then exit;
  if not AssertEquals('StartIndex, 2 chars in set, no match',-1,S.IndexOfAny(['a','z'],4)) then exit;
  if not AssertEquals('StartIndex, Single char in set, match',4,S.IndexOfAny(['e'],3)) then exit;
  if not AssertEquals('StartIndex, 2 chars in set, 2nd matches',3,S.IndexOfAny(['a','b'],2)) then exit;
  // Start index, count
  if not AssertEquals('StartIndex, count, Empty set',-1,S.IndexOfAny(ES,2,3)) then exit;
  if not AssertEquals('StartIndex, count, Single char in set, no match',-1,S.IndexOfAny(['a'],2)) then exit;
  if not AssertEquals('StartIndex, count, Single char in set, no match (wrong case)',-1,S.IndexOfAny(['O'],1)) then exit;
  if not AssertEquals('StartIndex, count, Single char in set, no match (index too big)',-1,S.IndexOfAny(['o'],2,4)) then exit;
  if not AssertEquals('StartIndex, count, Single char in set, no match (index too big, count too small)',-1,S.IndexOfAny(['o'],5,5)) then exit;
  if not AssertEquals('StartIndex, count, 2 chars in set, no match',-1,S.IndexOfAny(['a','z'],4,3)) then exit;
  if not AssertEquals('StartIndex, count, Single char in set, match',4,S.IndexOfAny(['e'],3,4)) then exit;
  if not AssertEquals('StartIndex, count, Single char in set, match in range',10,S.IndexOfAny(['e'],5,6)) then exit;
  if not AssertEquals('StartIndex, count, 2 chars in set, 2nd matches',3,S.IndexOfAny(['a','b'],2,3)) then exit;
end;

Function TestIndexOfAnyString : String;

Var
  S : String;
  ES : Array of String;
begin
  Result:='';
  S:=TBI;
  ES:=[];
  // Just a set
  SetLength(ES,0);
  if not AssertEquals('Empty set',-1,S.IndexOfAny(ES)) then exit;
  if not AssertEquals('Single string in set, no match',-1,S.IndexOfAny(['ab'])) then exit;
  if not AssertEquals('Single string in set, no match (wrong case)',-1,S.IndexOfAny(['TO'])) then exit;
  if not AssertEquals('2 strings in set, no match',-1,S.IndexOfAny(['ab','yz'])) then exit;
  if not AssertEquals('Single string in set, match',4,S.IndexOfAny(['e '])) then exit;
  if not AssertEquals('2 strings in set, 2nd matches',3,S.IndexOfAny(['ee','be'])) then exit;
  // Start index
  if not AssertEquals('StartIndex, Empty set',-1,S.IndexOfAny(ES,2)) then exit;
  if not AssertEquals('StartIndex, Single string in set, no match',-1,S.IndexOfAny(['aa'],2)) then exit;
  if not AssertEquals('StartIndex, Single string in set, no match (wrong case)',-1,S.IndexOfAny(['TO'],1)) then exit;
  if not AssertEquals('StartIndex, Single string in set, no match (index too big)',-1,S.IndexOfAny(['To'],2)) then exit;
  if not AssertEquals('StartIndex, 2 strings in set, no match',-1,S.IndexOfAny(['aa','zz'],4)) then exit;
  if not AssertEquals('StartIndex, Single string in set, match',4,S.IndexOfAny(['e '],3)) then exit;
  if not AssertEquals('StartIndex, 2 strings in set, 2nd matches',3,S.IndexOfAny(['aa','be'],2)) then exit;
  // Start index, count
  if not AssertEquals('StartIndex, count, Empty set',-1,S.IndexOfAny(ES,2,3)) then exit;
  if not AssertEquals('StartIndex, count, Single string in set, no match',-1,S.IndexOfAny(['aa'],2)) then exit;
  if not AssertEquals('StartIndex, count, Single string in set, no match (wrong case)',-1,S.IndexOfAny(['tO'],1)) then exit;
  if not AssertEquals('StartIndex, count, Single string in set, no match (index too big)',-1,S.IndexOfAny(['To'],2,4)) then exit;
  if not AssertEquals('StartIndex, count, Single string in set, no match (index too big, count too small)',-1,S.IndexOfAny(['To'],5,5)) then exit;
  if not AssertEquals('StartIndex, count, 2 strings in set, no match',-1,S.IndexOfAny(['aa','zz'],4,3)) then exit;
  if not AssertEquals('StartIndex, count, Single string in set, match',4,S.IndexOfAny(['e '],3,4)) then exit;
  if not AssertEquals('StartIndex, count, Single string in set, match in range',10,S.IndexOfAny(['em'],5,7)) then exit;
  if not AssertEquals('StartIndex, count, 2 strings in set, 2nd matches',3,S.IndexOfAny(['aa','be'],2,3)) then exit;
end;

Function TestIndexOfUnquoted : String;

Var
  S : String;

begin
  // Tests created from special cases in Embarcadero docs.
  Result:='';
  S:='"Thias" ias iat';
  If not AssertEquals('Simple case, match',8,S.IndexOfUnquoted('ia','"','"')) then exit;
  S:='"This  is"  it';
  If not AssertEquals('Simple case, match',10,S.IndexOfUnquoted('  ','"','"')) then exit;
  S:='"Thias ias iat';
  If not AssertEquals('Opening but not closed',-1,S.IndexOfAnyUnquoted('ia','"','"')) then exit;
  S:='"Thias" "ias" "iat"';
  If not AssertEquals('Only spaces unquoted',-1,S.IndexOfAnyUnquoted('ia','"','"')) then exit;
  S:='<Thias <ias>> iat';
  If not AssertEquals('Different start/end quotes',14,S.IndexOfAnyUnquoted('ia','<','>')) then exit;
  S:='"Thias" ias iat';
  If not AssertEquals('Start index',3,S.IndexOfAnyUnquoted('ia','"','"',1)) then exit;
  S:='Thias" "ias" "iat';
  If not AssertEquals('Start index',-1,S.IndexOfAnyUnquoted('ia','"','"',6)) then exit;
end;

Function TestIndexOfAnyUnquoted : String;

Var
  S : String;

begin
  // Tests created from special cases in Embarcadero docs.
  Result:='';
  S:='"This" is it';
  If not AssertEquals('Simple case, match',7,S.IndexOfAnyUnquoted(['i'],'"','"')) then exit;
  If not AssertEquals('Simple case 2, match',7,S.IndexOfAnyUnquoted(['a','i'],'"','"')) then exit;
  S:='"This is it';
  If not AssertEquals('Opening but not closed',-1,S.IndexOfAnyUnquoted(['i'],'"','"')) then exit;
  S:='"This" "is" "it"';
  If not AssertEquals('Only spaces unquoted',-1,S.IndexOfAnyUnquoted(['i'],'"','"')) then exit;
  S:='<This <is>> it';
  If not AssertEquals('Different start/end quotes',12,S.IndexOfAnyUnquoted(['i'],'<','>')) then exit;
  S:='"This" is it';
  // The documentation is WRONG on this one. Delphi prints 3, not 2 as in the docs.
  If not AssertEquals('Start index',3,S.IndexOfAnyUnquoted(['i'],'"','"',1)) then exit;
  S:='This" "is" "it';
  If not AssertEquals('Start index',-1,S.IndexOfAnyUnquoted(['i'],'"','"',5)) then exit;
end;

Function TestInsert : String;

Var
  S : String;

begin
  Result:='';
  S:=TBI;
  If not AssertEquals('0 based (1) (result)','All To be implemented',S.Insert(0,'All ')) then exit;
  If not AssertEquals('0 based (1) (self)','All To be implemented',S) then exit;
  S:=TBI;
  If not AssertEquals('0 based (2)','To be completely implemented',S.Insert(6,'completely ')) then exit;
  S:=TBI;
  If not AssertEquals('Negative index','completely '+TBI,S.Insert(-3,'completely ')) then exit;
  S:=TBI;
  If not AssertEquals('Too big index',TBI+'completely ',S.Insert(Length(S)+1,'completely ')) then exit;
end;

Function TestIsDelimiter : String;

Var
  S : String;

begin
  Result:='';
  S:=TBI;
  if not AssertTrue('Simple case, true',S.IsDelimiter('be',3)) then exit;
  if not AssertFalse('Simple case, false',S.IsDelimiter('ba',4)) then exit;
end;

Function TestIsEmpty : String;

Var
  S : String;

begin
  Result:='';
  S:='';
  if not AssertTrue('Simple case, true',S.IsEmpty) then exit;
  S:='abc';
  if not AssertFalse('Simple case, false',S.IsEmpty) then exit;
end;

Function TestLastDelimiter : String;

Var
  S : String;
begin
  Result:='';
  S:=TBI;
  if not AssertEquals('Simple case, match, zero based ',0,S.LastDelimiter('T')) then exit;
  if not AssertEquals('Simple case, no match ',-1,S.LastDelimiter('a')) then exit;
  if not AssertEquals('Simple case',3,S.LastDelimiter('b')) then exit;
  if not AssertEquals('Simple, check last match ',Length(TBI)-2,S.LastDelimiter('e')) then exit;
  if not AssertEquals('Multi, no match ',-1,S.LastDelimiter('qy')) then exit;
  if not AssertEquals('Multi, last match 1',Length(TBI)-1,S.LastDelimiter('ed')) then exit;
  if not AssertEquals('Multi, last match 2',Length(TBI)-2,S.LastDelimiter('eb')) then exit;
end;

Function TestLastIndexOf : String;

Var
  S : String;

begin
  Result:='';
  S:=TBI;
  if not AssertEquals('Simple case, no match',-1,S.LastIndexOf('a')) then exit;
  if not AssertEquals('Simple case, zero based',0,S.LastIndexOf('T')) then exit;
  if not AssertEquals('Simple case last',Length(TBI)-2,S.LastIndexOf('e')) then exit;
  if not AssertEquals('Simple case, startindex too low',-1,S.LastIndexOf('e',3)) then exit;
  if not AssertEquals('Simple case, startindex OK ',4,S.LastIndexOf('e',7)) then exit;
  if not AssertEquals('Simple case, startindex OK, count too small ',-1,S.LastIndexOf('e',7,3)) then exit;
  if not AssertEquals('Simple case, startindex OK, count border',4,S.LastIndexOf('e',7,4)) then exit;
end;

Function TestLastIndexOfString : String;

Var
  S : String;

begin
  Result:='';
  S:=TBI;
  if not AssertEquals('Simple case, no match',-1,S.LastIndexOf('aa')) then exit;
  if not AssertEquals('Simple case, zero based',0,S.LastIndexOf('To')) then exit;
  if not AssertEquals('Simple case last',Length(TBI)-2,S.LastIndexOf('ed')) then exit;
  if not AssertEquals('Simple case, startindex too low',-1,S.LastIndexOf('ed',3)) then exit;
  if not AssertEquals('Simple case, startindex OK ',3,S.LastIndexOf('be',7)) then exit;
  if not AssertEquals('Simple case, startindex OK, count too small ',-1,S.LastIndexOf('be',7,3)) then exit;
  if not AssertEquals('Simple case, startindex OK, count border',3,S.LastIndexOf('be',7,4)) then exit;
end;

Function TestLastIndexOfAny : String;

Var
  S : String;

begin
  Result:='';
  S:=TBI;
  if not AssertEquals('Simple case, no match',-1,S.LastIndexOfAny(['x'])) then exit;
  if not AssertEquals('Double case, no match',-1,S.LastIndexOfAny(['a','x'])) then exit;
  if not AssertEquals('Simple case, zero based',0,S.LastIndexOfAny(['T'])) then exit;
  if not AssertEquals('Double case, zero based',0,S.LastIndexOfAny(['T','q'])) then exit;
  if not AssertEquals('Simple case last',Length(TBI)-2,S.LastIndexOf('e')) then exit;
  if not AssertEquals('Simple case, startindex too low',-1,S.LastIndexOf('e',3)) then exit;
  if not AssertEquals('Simple case, startindex OK ',4,S.LastIndexOf('e',7)) then exit;
  if not AssertEquals('Simple case, startindex OK, count too small ',-1,S.LastIndexOf('e',7,3)) then exit;
  if not AssertEquals('Simple case, startindex OK, count border',4,S.LastIndexOf('e',7,4)) then exit;
end;

Function TestPadLeft : String;

Var
  S : String;

begin
  Result:='';
  S:='TBI';
  if not AssertEquals('Default char','  TBI',S.PadLeft(5)) then exit;
  if not AssertEquals('Length reached','TBI',S.PadLeft(3)) then exit;
  if not AssertEquals('Length over','TBI',S.PadLeft(2)) then exit;
  if not AssertEquals('Alternate char','**TBI',S.PadLeft(5,'*')) then exit;
end;
Function TestPadRight : String;

Var
  S : String;

begin
  Result:='';
  S:='TBI';
  if not AssertEquals('Default char','TBI  ',S.PadRight(5)) then exit;
  if not AssertEquals('Original remains untouched','TBI',S) then exit;
  if not AssertEquals('Length reached','TBI',S.PadRight(3)) then exit;
  if not AssertEquals('Original remains untouched','TBI',S) then exit;
  if not AssertEquals('Length over','TBI',S.PadRight(2)) then exit;
  if not AssertEquals('Original remains untouched','TBI',S) then exit;
  if not AssertEquals('Alternate char','TBI**',S.PadRight(5,'*')) then exit;
  if not AssertEquals('Original remains untouched','TBI',S) then exit;
end;

Function TestQuotedString : String;

Const
  TII = '''This'' is it';
  TII2 = '"This" is it';

Var
  S : String;

begin
  Result:='';
  S:=TBI;
  if not AssertEquals('Default case',''''+TBI+'''',S.QuotedString) then exit;
  if not AssertEquals('Original remains untouched',TBI,S) then exit;
  S:=TII;
  if not AssertEquals('Quotes present, doubled','''''''This'''' is it''',S.QuotedString) then exit;
  if not AssertEquals('Original remains untouched',TII,S) then exit;
  // Other quote char
  S:=TBI;
  if not AssertEquals('Quote ", Default case','"'+TBI+'"',S.QuotedString('"')) then exit;
  if not AssertEquals('Quote ", Original remains untouched',TBI,S) then exit;
  S:=TII2;
  if not AssertEquals('Quote ", Quotes present, doubled','"""This"" is it"',S.QuotedString('"')) then exit;
  if not AssertEquals('Quote ", Original remains untouched',TII2,S) then exit;
end;

Function TestRemove : String;

Var
  S : String;

begin
  Result:='';
  S:=TBI;
  if not AssertEquals('Delete all','',S.Remove(0)) then exit;
  if not AssertEquals('Delete all, original unchanged',TBI,S) then exit;
  if not AssertEquals('Delete from index','To',S.Remove(2)) then exit;
  if not AssertEquals('Delete all, original unchanged',TBI,S) then exit;
  if not AssertEquals('Delete from negative index',TBI,S.Remove(-1)) then exit;
  if not AssertEquals('Delete from negative, original unchanged',TBI,S) then exit;
  // Count
  if not AssertEquals('Delete N from start','be implemented',S.Remove(0,3)) then exit;
  if not AssertEquals('Delete all, original unchanged',TBI,S) then exit;
  if not AssertEquals('Delete from start index, count','To implemented',S.Remove(2,3)) then exit;
  if not AssertEquals('Delete from start index, count, original unchanged',TBI,S) then exit;
  if not AssertEquals('Delete from negative index, count',TBI,S.Remove(-1,4)) then exit;
  if not AssertEquals('Delete from negative index, count, original unchanged',TBI,S) then exit;

end;

Function TestReplace : String;
{
Function Replace(OldChar: Char; NewChar: Char): string; overload;
Function Replace(OldChar: Char; NewChar: Char; ReplaceFlags: TReplaceFlags): string; overload;
Function Replace(const OldValue: string; const NewValue: string): string; overload;
Function Replace(const OldValue: string; const NewValue: string; ReplaceFlags: TReplaceFlags): string; overload;
}

Var
  S : String;

begin
  Result:='';
  S:=TBI;
  // Char
  if not AssertEquals('Simple char','to be implemented',S.Replace('T','t')) then exit;
  if not AssertEquals('Simple char, original unchanged',TBI,S) then exit;
  if not AssertEquals('Simple char is case sensitive',TBI,S.Replace('t','t')) then exit;
  if not AssertEquals('Simple char is replace all','To ba implamantad',S.Replace('e','a')) then exit;
  if not AssertEquals('Simple char, case insensitive','to be implemented',S.Replace('t','t',[rfIgnoreCase])) then exit;
  if not AssertEquals('Simple char, only first','To ba implemented',S.Replace('e','a',[])) then exit;
  if not AssertEquals('Simple char, replace all','To ba implamantad',S.Replace('e','a',[rfReplaceAll])) then exit;
  // String
  if not AssertEquals('Simple string','ta be implemented',S.Replace('To','ta')) then exit;
  if not AssertEquals('Simple string, case sensitive',TBI,S.Replace('to','ta')) then exit;
  S:='AB AB';
  if not AssertEquals('Simple string is replace all','cd cd',S.Replace('AB','cd')) then exit;
  S:=TBI;
  if not AssertEquals('Simple string, case insensitive','to be implemented',S.Replace('to','to',[rfIgnoreCase])) then exit;
  S:='AB AB AB';
  if not AssertEquals('Simple sting, only first','cd AB AB',S.Replace('AB','cd',[])) then exit;
  S:='AB AB AB';
  if not AssertEquals('Simple string, replace all','cd cd cd',S.Replace('AB','cd',[rfReplaceAll])) then exit;
end;

Function TestSplit : String;

  Function TestArray(Msg : string; Aexpected : Array of string; AActual : TStringArray) : boolean;

  Var
    I : integer;

  begin
    Result:=False;
    if not AssertEquals(Msg+': Length correct',Length(AExpected),Length(AActual)) then exit;
    for I:=0 to Length(AExpected)-1 do
      if not AssertEquals(Msg+': Element '+IntToStr(i)+' correct',AExpected[i],AActual[i]) then exit;
    Result:=True;
  end;

Const
  TII  = '"This is" it' ;
  TII2 = '<This is> it' ;
  TII3 = '<This is>  it' ;
  CA: array[0..7] of string = ('F0;F1;F2', ';F1;F2', ';;F2', 'F0;;F2', ';;', 'F0;F1;', 'F0;;', ';F1;');

Var
  S : String;
  C: TStringArray;

begin
  Result:='';
  S:='a b';
  C:=S.Split([' ']);
  if not TestArray('One letter',['a','b'],C) then exit;
  S:=TBI;
  C:=S.Split([' ']);
  if not TestArray('Simple case',['To','be','implemented'],C) then exit;
  C:=S.Split([' '],2);
  if not TestArray('Simple case, count',['To','be'],C) then exit;
  S:=TII;
  C:=S.Split([' ','"']);
  if not TestArray('Quote and space',['','This','is','','it'],C) then exit;
  C:=S.Split([' ','"'],TStringSplitOptions.ExcludeEmpty);
  if not TestArray('Quote and space, exclude empty',['This','is','it'],C) then exit;
  C:=S.Split([' '],2);
  if not TestArray('Quote and space, count 2',['"This','is"'],C) then exit;
  C:=S.Split([' ','"'],2,TStringSplitOptions.ExcludeEmpty);
  if not TestArray('Quote and space, exclude empty,count 2',['This','is'],C) then exit;
  C:=S.Split([' ','"'],1,TStringSplitOptions.ExcludeEmpty);
  if not TestArray('Quote and space, exclude empty, count 1',['This'],C) then exit;
  C:=S.Split([' '],'"','"');
  if not TestArray('Quoted, space only',['"This is"','it'],C) then exit;
  C:=S.Split([' '],'"','"',1);
  if not TestArray('Quoted, space only; count',['"This is"'],C) then exit;
  S:=TII2;
  C:=S.Split([' '],'<','>');
  if not TestArray('Quoted <>, space only',['<This is>','it'],C) then exit;
  S:=TII3;
  C:=S.Split([' '],'<','>');
  if not TestArray('Quoted <>, space only, have space',['<This is>','','it'],C) then exit;
  S:=TII3;
  C:=S.Split([' '],'<','>',TStringSplitOptions.ExcludeEmpty);
  if not TestArray('Quoted <>, space only, have space, exclude empty',['<This is>','it'],C) then exit;
  for S in CA do
    begin
    C := S.Split([';']);
    if Length(C)<>3 then
      exit('Error : expect 3 elements when splitting string '+S);
    end;
end;

Function TestSplitString : String;

  Function TestArray(Msg : string; Aexpected : Array of string; AActual : TStringArray) : boolean;

  Var
    I : integer;

  begin
    Result:=False;
    if not AssertEquals(Msg+': Length correct',Length(AExpected),Length(AActual)) then exit;
    for I:=0 to Length(AExpected)-1 do
      if not AssertEquals(Msg+': Element '+IntToStr(i)+' correct',AExpected[i],AActual[i]) then exit;
    Result:=True;
  end;

Const
  TII  = '"This  is"  it' ;
  TII2 = '<This  is>  it' ;
  TII3 = '<This  is>    it' ;

Var
  S : String;
  C: TStringArray;

begin
  Result:='';
  S:=StringReplace(TBI,' ','  ',[rfReplaceAll]);
{  C:=S.Split(['  ']);
  if not TestArray('Simple case',['To','be','implemented'],C) then exit;
  C:=S.Split(['  '],2);
  if not TestArray('Simple case, count',['To','be'],C) then exit;
  S:=TII;
  C:=S.Split(['  ','"']);
  if not TestArray('Quote and space',['','This','is','','it'],C) then exit;
  C:=S.Split(['  ','"'],ExcludeEmpty);
  if not TestArray('Quote and space, exclude empty',['This','is','it'],C) then exit;
  C:=S.Split(['  '],2);
  if not TestArray('Quote and space, count 2',['"This','is"'],C) then exit;
  C:=S.Split(['  ','"'],2,ExcludeEmpty);
  if not TestArray('Quote and space, exclude empty,count 2',['This','is'],C) then exit;
  C:=S.Split(['  ','"'],1,ExcludeEmpty);
  if not TestArray('Quote and space, exclude empty, count 1',['This'],C) then exit;
  }
  S:=TII;
  C:=S.Split(['  '],'"','"');
  if not TestArray('Quoted, space only',['"This  is"','it'],C) then exit;
  C:=S.Split(['  '],'"','"',1);
  if not TestArray('Quoted, space only; count',['"This  is"'],C) then exit;
  S:=TII2;
  C:=S.Split(['  '],'<','>');
  if not TestArray('Quoted <>, space only',['<This  is>','it'],C) then exit;
  S:=TII3;
  C:=S.Split(['  '],'<','>');
  if not TestArray('Quoted <>, space only, have space',['<This  is>','','it'],C) then exit;
  S:=TII3;
  C:=S.Split(['  '],'<','>',TStringSplitOptions.ExcludeEmpty);
  if not TestArray('Quoted <>, space only, have space, exclude empty',['<This  is>','it'],C) then exit;
end;


Function TestStartsWith : String;

Var
  S : String;

begin
  Result:='';
  S:=TBI;
  if not AssertTrue('Match empty',S.StartsWith('')) then exit;
  if not AssertTrue('Match',S.StartsWith('To')) then exit;
  if not AssertFalse('Match, case sensitive',S.StartsWith('to')) then exit;
  if not AssertFalse('No Match',S.StartsWith('ab')) then exit;
  if not AssertFalse('No Match, complete',S.StartsWith('To n')) then exit;
  if not AssertFalse('Match, only start',S.StartsWith('be')) then exit;
  if not AssertTrue('Match, case insensitive',S.StartsWith('To')) then exit;
end;

Function TestSubstring : String;

Var
  S : String;

begin
  Result:='';
  S:=TBI;
  // No length
  if not AssertEquals('0 based','ed',S.SubString(Length(S)-2)) then exit;
  if not AssertEquals('0 based, original untouched',TBI,S) then exit;
  if not AssertEquals('Index too big','',S.SubString(Length(S)+2)) then exit;
  if not AssertEquals('Index negative',TBI,S.SubString(-1)) then exit;
  // Length
  if not AssertEquals('0 based','To',S.SubString(0,2)) then exit;
  if not AssertEquals('0 based, original untouched',TBI,S) then exit;
  if not AssertEquals('Index too big','',S.SubString(Length(S)+2,3)) then exit;
  if not AssertEquals('Index negative','To',S.SubString(-1,2)) then exit;
  if not AssertEquals('Sub, index','be',S.SubString(3,2)) then exit;
end;

Function TestToCharArray : String;

Var
  S : String;
  C : TCharArray;
  I : integer;

begin
  Result:='';
  S:=TBI;
  C:=S.ToCharArray;
  if not AssertEquals('No args, length',Length(S),Length(C)) then exit;
  For I:=1 to Length(S) do
    if not AssertEquals('No args, character (1-based) : '+IntToStr(i),S[i],C[i-1]) then exit;
  C:=S.ToCharArray(3,Length(S)-3);
  if not AssertEquals('No args, length',Length(S)-3,Length(C)) then exit;
  For I:=4 to Length(S) do
    if not AssertEquals('Args(3,len), character (1-based) : '+IntToStr(i),S[i],C[i-4]) then exit;
end;

Function TestToLower : String;

Var
  S : String;

begin
  Result:='';
  S:=TBI;
  if not AssertEquals('Lowercase','to be implemented',S.ToLower) then exit;
  if not AssertEquals('Lowercase, original unmodified',TBI,S) then exit;
end;

Function TestToLowerInvariant : String;

Var
  S : String;

begin
  Result:='';
  S:=TBI;
  if not AssertEquals('Lowercase','to be implemented',S.ToLowerInvariant) then exit;
  if not AssertEquals('Lowercase, original unmodified',TBI,S) then exit;
  // This probably needs testing of some special cases.
end;

Function TestToUpper : String;

Var
  S : String;

begin
  Result:='';
  S:=TBI;
  if not AssertEquals('Lowercase','TO BE IMPLEMENTED',S.ToUpper) then exit;
  if not AssertEquals('Lowercase, original unmodified',TBI,S) then exit;
end;

Function TestToUpperInvariant : String;

Var
  S : String;

begin
  Result:='';
  S:=TBI;
  if not AssertEquals('Lowercase','TO BE IMPLEMENTED',S.ToUpperInvariant) then exit;
  if not AssertEquals('Lowercase, original unmodified',TBI,S) then exit;
  // This probably needs testing of some special cases.
end;

Function TestTrim : String;
Var
  T,S : String;
  C : Char;

begin
  Result:='';
  S:=TBI;
  For C:=#0 to #32 do
    S:=C+S+C;
  T:=S;
  if not AssertEquals('By default all chars below #32 stripped',TBI,S.Trim) then exit;
  if not AssertEquals('Original unmodified',T,S) then exit;
  S:='lmn'+TBI+'lmn';
  T:=S;
  if not AssertEquals('Strip all indicated chars',TBI,S.Trim(['l','m','n'])) then exit;
  if not AssertEquals('Strip all indicated chars, Original unmodified',T,S) then exit;
end;

Function TestTrimLeft : String;

Var
  O,T,S : String;
  C : Char;

begin
  Result:='';
  S:=TBI;
  T:=TBI;
  For C:=#0 to #32 do
    begin
    S:=C+S+C;
    T:=T+C;
    end;
  O:=S;
  if not AssertEquals('By default all chars below #32 stripped',T,S.TrimLeft) then exit;
  if not AssertEquals('Original unmodified',O,S) then exit;
  S:='lmn'+TBI+'lmn';
  T:=TBI+'lmn';
  O:=S;
  if not AssertEquals('Strip all indicated chars',T,S.TrimLeft(['l','m','n'])) then exit;
  if not AssertEquals('Strip all indicated chars, Original unmodified',O,S) then exit;
end;

Function TestTrimRight : String;

Var
  O,T,S : String;
  C : Char;

begin
  Result:='';
  S:=TBI;
  T:=TBI;
  For C:=#0 to #32 do
    begin
    S:=C+S+C;
    T:=C+T;
    end;
  O:=S;
  if not AssertEquals('By default all chars below #32 stripped',T,S.TrimRight) then exit;
  if not AssertEquals('Original unmodified',O,S) then exit;
  S:='lmn'+TBI+'lmn';
  T:='lmn'+TBI;
  O:=S;
  if not AssertEquals('Strip all indicated chars',T,S.TrimRight(['l','m','n'])) then exit;
  if not AssertEquals('Strip all indicated chars, Original unmodified',O,S) then exit;
end;

Function TestTrimEnd : String;

Var
  O,T,S : String;

begin
  Result:='';
  S:='lmn'+TBI+'lmn';
  T:='lmn'+TBI;
  O:=S;
  if not AssertEquals('Strip all indicated chars',T,S.TrimRight(['l','m','n'])) then exit;
  if not AssertEquals('Strip all indicated chars, Original unmodified',O,S) then exit;
end;

Function TestTrimStart : String;

Var
  O,T,S : String;

begin
  Result:='';
  S:='lmn'+TBI+'lmn';
  T:=TBI+'lmn';
  O:=S;
  if not AssertEquals('Strip all indicated chars',T,S.TrimLeft(['l','m','n'])) then exit;
  if not AssertEquals('Strip all indicated chars, Original unmodified',O,S) then exit;
end;

Function TestChars : String;

Var
  S : String;
  I : Integer;
begin
  Result:='';
  S:=TBI;
  For I:=1 to Length(S) do
    if not AssertEquals('Character (1-based)'+IntToStr(i),S[i],S.Chars[i-1]) then exit;
end;

Function TestLength : String;

Var
  S : String;

begin
  Result:='';
  S:=TBI;
  if not AssertEquals('Correct length',Length(TBI),S.Length) then exit;
end;

(* // Template code;
Function Test : String;

begin
  Result:='To be implemented';
end;
*)

Procedure RegisterStringHelperTests;

Var
  P : PSuite;

begin
  P:=AddSuite('StringHelper',EnsureSuite('SysUtils'));
  AddTest('TestCompare',@TestCompare,P);
  AddTest('TestCompareOrdinal',@TestCompareOrdinal,P);
  AddTest('TestCompareText',@TestCompareText,P);
  AddTest('TestCopy',@TestCopy,P);
  AddTest('TestCreate',@TestCreate,P);
  AddTest('TestEndsText',@TestEndsText,P);
  AddTest('TestEquals',@TestEquals,P);
  AddTest('TestFormat',@TestFormat,P);
  AddTest('TestIsNullOrEmpty',@TestIsNullOrEmpty,P);
  AddTest('TestJoin',@TestJoin,P);
  AddTest('TestLowerCase',@TestLowerCase,P);
  AddTest('TestParse',@TestParse,P);
  AddTest('TestToBoolean',@TestToBoolean,P);
  AddTest('TestToDouble',@TestToDouble,P);
  AddTest('TestToExtended',@TestToExtended,P);
  AddTest('TestToInt64',@TestToInt64,P);
  AddTest('TestToInteger',@TestToInteger,P);
  AddTest('TestToSingle',@TestToSingle,P);
  AddTest('TestUppercase',@TestUppercase,P);
  AddTest('TestCompareTo',@TestCompareTo,P);
  AddTest('TestCopyTo',@TestCopyTo,P);
  AddTest('TestContains',@TestContains,P);
  AddTest('TestCountChar',@TestCountChar,P);
  AddTest('TestDeQuotedString',@TestDeQuotedString,P);
  AddTest('TestEndsWith',@TestEndsWith,P);
  AddTest('TestGetHashCode',@TestGetHashCode,P);
  AddTest('TestIndexOf',@TestIndexOf,P);
  AddTest('TestIndexOfAny',@TestIndexOfAny,P);
  AddTest('TestIndexOfAnyString',@TestIndexOfAnyString,P);
  AddTest('TestIndexOfUnQuoted',@TestIndexOfUnQuoted,P);
  AddTest('TestIndexOfAnyUnquoted',@TestIndexOfAnyUnquoted,P);
  AddTest('TestInsert',@TestInsert,P);
  AddTest('TestIsDelimiter',@TestIsDelimiter,P);
  AddTest('TestIsEmpty',@TestIsEmpty,P);
  AddTest('TestLastDelimiter',@TestLastDelimiter,P);
  AddTest('TestLastIndexOf',@TestLastIndexOf,P);
  AddTest('TestLastIndexOfString',@TestLastIndexOfString,P);
  AddTest('TestLastIndexOfAny',@TestLastIndexOfAny,P);
  AddTest('TestPadLeft',@TestPadLeft,P);
  AddTest('TestPadRight',@TestPadRight,P);
  AddTest('TestQuotedString',@TestQuotedString,P);
  AddTest('TestRemove',@TestRemove,P);
  AddTest('TestReplace',@TestReplace,P);
  AddTest('TestSplit',@TestSplit,P);
  AddTest('TestSplitString',@TestSplitString,P);
  AddTest('TestStartsWith',@TestStartsWith,P);
  AddTest('TestSubstring',@TestSubstring,P);
  AddTest('TestToCharArray',@TestToCharArray,P);
  AddTest('TestToLower',@TestToLower,P);
  AddTest('TestToLowerInvariant',@TestToLowerInvariant,P);
  AddTest('TestToUpper',@TestToUpper,P);
  AddTest('TestToUpperInvariant',@TestToUpperInvariant,P);
  AddTest('TestTrim',@TestTrim,P);
  AddTest('TestTrimLeft',@TestTrimLeft,P);
  AddTest('TestTrimRight',@TestTrimRight,P);
  AddTest('TestTrimEnd',@TestTrimEnd,P);
  AddTest('TestTrimStart',@TestTrimStart,P);
  AddTest('TestChars',@TestChars,P);
  AddTest('TestLength',@TestLength,P);
//  AddTest('Test',@Test,P);
end;

initialization
  RegisterStringHelperTests;
end.

