unit utcregexapi;

{$mode objfpc}{$H+}
{ $DEFINE USEWIDESTRING}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, system.regularexpressionscore, system.regularexpressions;

type

  { TTestRegExpCore }

  TTestRegExp = class(TTestCase)
  private
    FRegex: TRegEx;
    function DoReplacer(const Match: TMatch): TREString;
  Protected
    Property Regex : TRegEx Read FRegex Write FRegex;
  Public
    Procedure SetUp; override;
    Procedure TearDown; override;
  Published
    Procedure TestIsMatch;
    Procedure TestIsMatchStartPos;
    Procedure TestClassIsMatch;
    Procedure TestClassIsMatchOptions;
    Procedure TestEscape;
    Procedure TestMatch;
    Procedure TestMatchNoMatch;
    Procedure TestMatchStartPos;
    Procedure TestMatchStartPosLength;
    Procedure TestClassMatch;
    Procedure TestClassMatchOptions;
    Procedure TestMatches;
    Procedure TestMatchesStartPos;
    Procedure TestClassMatches;
    Procedure TestClassMatchesOptions;
    Procedure TestReplace;
    Procedure TestReplaceEval;
    Procedure TestReplaceCount;
    Procedure TestReplaceEvalCount;
    Procedure TestClassReplace;
    Procedure TestClassReplaceEval;
    Procedure TestClassReplaceOptions;
    Procedure TestClassReplaceEvalOptions;
{


    function Split(const aInput: TREString): TREStringDynArray; overload; inline;
    function Split(const aInput: TREString; aCount: Integer): TREStringDynArray; overload; inline;
    function Split(const aInput: TREString; aCount, aStartPos: Integer): TREStringDynArray; overload;
    class function Split(const aInput, aPattern: TREString): TREStringDynArray; overload; static;
    class function Split(const aInput, aPattern: TREString; aOptions: TRegExOptions): TREStringDynArray; overload; static;
}
  end;

implementation

Const
  TestStr = 'xyz abba abbba abbbba zyx';
  TestExpr = 'a(b*)a';

{ TTestRegExpr}

procedure TTestRegExp.SetUp;
begin
  inherited SetUp;
  FRegex:=Default(TRegex);
end;

procedure TTestRegExp.TearDown;
begin
  FRegex:=Default(TRegex);
  inherited TearDown;
end;

procedure TTestRegExp.TestIsMatch;
begin
 // function IsMatch(const aInput: TREString): Boolean; overload;
  Regex:=TRegex.Create(TestExpr);
  AssertTrue('Correct match',Regex.IsMatch(TestStr));
end;

procedure TTestRegExp.TestIsMatchStartPos;
begin
//  function IsMatch(const aInput: TREString; aStartPos: Integer): Boolean; overload;
  Regex:=TRegex.Create(TestExpr);
  AssertTrue('Correct match',Regex.IsMatch(TestStr,Pos('abbba',TestStr)));
  AssertFalse('No match match at pos',Regex.IsMatch(TestStr,Pos('zyx',TestStr)));
end;

procedure TTestRegExp.TestClassIsMatch;
begin
//  class function IsMatch(const aInput, aPattern: TREString): Boolean;overload; static;
  AssertTrue('Correct match',TRegex.IsMatch(TestStr,TestExpr));
  AssertFalse('No match',TRegex.IsMatch(TestStr,TestExpr+'xyz'));

end;

procedure TTestRegExp.TestClassIsMatchOptions;
begin
//  class function IsMatch(const aInput, aPattern: TREString; aOptions: TRegExOptions): Boolean; overload; static;
  AssertTrue('Correct match',TRegex.IsMatch(UpperCase(TestStr),TestExpr,[roIgnoreCase]));
  AssertFalse('No match',TRegex.IsMatch(UpperCase(TestStr),TestExpr+'xyz',[roIgnoreCase]));
end;

procedure TTestRegExp.TestEscape;
begin
//  class function Escape(const aString: TREString; aUseWildCards: Boolean = False): TREString; static;
  AssertEquals('Wildcard ?','(.)',TRegex.Escape('?',True));
  AssertEquals('Wildcard ?','\?',TRegex.Escape('??',True));
  AssertEquals('Wildcard *','(.*)',TRegex.Escape('*',True));
  AssertEquals('Wildcard ?','\*',TRegex.Escape('**',True));
  AssertEquals('CRLF','\r\n',TRegex.Escape(#13#10,True));
end;

Procedure DumpMatch(M : TMatch);

var
  I : Integer;

begin
  Writeln('Match value: ',M.Value);
  Writeln('Match index: ',M.Index);
  Writeln('Match length: ',M.Length);
  Writeln('Match group count: ',M.Groups.Count);
  for I:=0 to M.Groups.Count-1 do
    begin
    Writeln('Group ',I);
    Writeln(Format('Match group %d value: ',[i]),M.Groups[i].Value);
    Writeln(Format('Match group %d index: ',[i]),M.Groups[i].Index);
    Writeln(Format('Match group %d length: ',[i]),M.Groups[i].Length);
    end;
end;

procedure TTestRegExp.TestMatch;

var
  M : TMatch;

begin
//  function Match(const aInput: TREString): TMatch; overload;
  RegEx:=TRegex.Create(TestExpr);
  M:=RegEx.Match(TestStr);
  AssertTrue('Match 0 result: ',M.Success);
  AssertEquals('Match 0 value: ','abba',M.Value);
  AssertEquals('Match 0 index: ',5,M.Index);
  AssertEquals('Match 0 length: ',4,M.Length);
  AssertEquals('Match 0 group count: ',2,M.Groups.Count);
  AssertEquals('Match 0 group 0 value: ','abba',M.Groups[0].Value);
  AssertEquals('Match 0 group 0 index: ',5,M.Groups[0].Index);
  AssertEquals('Match 0 group 0 length: ',4,M.Groups[0].Length);
  AssertEquals('Match 0 group 1 value: ','bb',M.Groups[1].Value);
  AssertEquals('Match 0 group 1 index: ',6,M.Groups[1].Index);
  AssertEquals('Match 0 group 1 length: ',2,M.Groups[1].Length);
  M:=M.NextMatch;
  AssertTrue('Match 1 result: ',M.Success);
  AssertEquals('Match 1 value: ','abbba',M.Value);
  AssertEquals('Match 1 index: ',10,M.Index);
  AssertEquals('Match 1 length: ',5,M.Length);
  M:=M.NextMatch;
  AssertTrue('Match 2 result: ',M.Success);
  AssertEquals('Match 2 value: ','abbbba',M.Value);
  AssertEquals('Match 2 index: ',16,M.Index);
  AssertEquals('Match 2 length: ',6,M.Length);
  M:=M.NextMatch;
  AssertFalse('Match 3 value: ',M.Success);
end;

procedure TTestRegExp.TestMatchNoMatch;

var
  M : TMatch;

begin
  RegEx:=TRegex.Create(TestExpr+'xyz');
  M:=RegEx.Match(TestStr);
  AssertFalse('Success',M.Success);
  AssertEquals('No match value','',M.Value);
  AssertEquals('No match Index',0,M.Index);
  AssertEquals('No match legth',0,M.Length);
end;

procedure TTestRegExp.TestMatchStartPos;

var
  M : TMatch;
  P : Integer;

begin
//  function Match(const aInput: TREString): TMatch; overload;
  RegEx:=TRegex.Create(TestExpr);
  P:=Pos('abbba',TestStr);
  M:=RegEx.Match(TestStr,P);
  //  DumpMatch(M);
  AssertTrue('Match value: ',M.Success);
  AssertEquals('Match value: ','abbba',M.Value);
  AssertEquals('Match index: ',10,M.Index);
  AssertEquals('Match length: ',5,M.Length);
  AssertEquals('Match group count: ',2,M.Groups.Count);
  AssertEquals('Match group 0 value: ','abbba',M.Groups[0].Value);
  AssertEquals('Match group 0 index: ',10,M.Groups[0].Index);
  AssertEquals('Match group 0 length: ',5,M.Groups[0].Length);
  AssertEquals('Match group 1 value: ','bbb',M.Groups[1].Value);
  AssertEquals('Match group 1 index: ',11,M.Groups[1].Index);
  AssertEquals('Match group 1 length: ',3,M.Groups[1].Length);
  M:=M.NextMatch;
  AssertTrue('Match value: ',M.Success);
end;

procedure TTestRegExp.TestMatchStartPosLength;
var
  M : TMatch;
  P : Integer;

begin
//  function Match(const aInput: TREString): TMatch; overload;
  RegEx:=TRegex.Create(TestExpr);
  P:=Pos('abbba',TestStr);
  M:=RegEx.Match(TestStr,P,5);
  //  DumpMatch(M);
  AssertTrue('Match value: ',M.Success);
  AssertEquals('Match value: ','abbba',M.Value);
  AssertEquals('Match index: ',10,M.Index);
  AssertEquals('Match length: ',5,M.Length);
  M:=M.NextMatch;
  AssertFalse('No more matches: ',M.Success);
end;

procedure TTestRegExp.TestClassMatch;

var
  M : TMatch;

begin
//    class function Match(const aInput, aPattern: TREString): TMatch; overload; static;
  M:=TRegex.Match(TestStr,TestExpr);
  AssertTrue('Match result: ',M.Success);
  AssertEquals('Match value: ','abba',M.Value);

end;

procedure TTestRegExp.TestClassMatchOptions;

//  class function Match(const aInput, aPattern: TREString; aOptions: TRegExOptions): TMatch; overload; static;
var
  M : TMatch;

begin
//    class function Match(const aInput, aPattern: TREString): TMatch; overload; static;
  M:=TRegex.Match(UpperCase(TestStr),TestExpr,[roIgnoreCase]);
  AssertTrue('Match result: ',M.Success);
  AssertEquals('Match value: ','ABBA',M.Value);
end;

procedure TTestRegExp.TestMatches;

var
  MS : TMatchCollection;
  M,M2 : TMatch;

begin
//  function Matches(const aInput: TREString): TMatchCollection; overload;
  RegEx:=TRegex.Create(TestExpr);
  MS:=RegEx.Matches(TestStr);
  AssertEquals('Match count',3,MS.Count);
  M:=MS[0];
  AssertTrue('Match 0 result: ',M.Success);
  AssertEquals('Match 0 value: ','abba',M.Value);
  AssertEquals('Match 0 index: ',5,M.Index);
  AssertEquals('Match 0 length: ',4,M.Length);
  AssertEquals('Match 0 group count: ',2,M.Groups.Count);
  AssertEquals('Match 0 group 0 value: ','abba',M.Groups[0].Value);
  AssertEquals('Match 0 group 0 index: ',5,M.Groups[0].Index);
  AssertEquals('Match 0 group 0 length: ',4,M.Groups[0].Length);
  AssertEquals('Match 0 group 1 value: ','bb',M.Groups[1].Value);
  AssertEquals('Match 0 group 1 index: ',6,M.Groups[1].Index);
  AssertEquals('Match 0 group 1 length: ',2,M.Groups[1].Length);
  M2:=M.NextMatch;
  M:=MS[1];
  AssertTrue('Match 1 resul: ',M.Success);
  AssertEquals('Match 1 value: ','abbba',M.Value);
  AssertEquals('NextMatch value: ','abbba',M2.Value);
  AssertEquals('Match 1 index: ',10,M.Index);
  AssertEquals('Match 1 length: ',5,M.Length);
  M:=MS[2];
  AssertTrue('Match 2 result: ',M.Success);
  AssertEquals('Match 2 value: ','abbbba',M.Value);
  AssertEquals('Match 2 index: ',16,M.Index);
  AssertEquals('Match 2 length: ',6,M.Length);
  M:=M.NextMatch;
  AssertFalse('Match value: ',M.Success);
end;

procedure TTestRegExp.TestMatchesStartPos;
var
  MS : TMatchCollection;
  M : TMatch;

begin
//  function Matches(const aInput: TREString; aStartPos: Integer): TMatchCollection; overload;
  RegEx:=TRegex.Create(TestExpr);
  MS:=RegEx.Matches(TestStr,9);
  AssertEquals('Match count',2,MS.Count);
  M:=MS[0];
  AssertTrue('Match 1 resul: ',M.Success);
  AssertEquals('Match 1 value: ','abbba',M.Value);
  M:=MS[1];
  AssertTrue('Match 1 resul: ',M.Success);
  AssertEquals('Match 1 value: ','abbbba',M.Value);

end;

procedure TTestRegExp.TestClassMatches;
var
  MS : TMatchCollection;
  M : TMatch;
begin
//  class function Matches(const aInput, aPattern: TREString): TMatchCollection; overload; static;
  MS:=TRegEx.Matches(TestStr,TestExpr);
  AssertEquals('Match count',3,MS.Count);
  M:=MS[0];
  AssertTrue('Match 0 result: ',M.Success);
  AssertEquals('Match 0 value: ','abba',M.Value);
  M:=MS[1];
  AssertTrue('Match 0 result: ',M.Success);
  AssertEquals('Match 0 value: ','abbba',M.Value);
  M:=MS[2];
  AssertTrue('Match 0 result: ',M.Success);
  AssertEquals('Match 0 value: ','abbbba',M.Value);

end;

procedure TTestRegExp.TestClassMatchesOptions;


var
  MS : TMatchCollection;
  M : TMatch;
begin
  //  class function Matches(const aInput, aPattern: TREString; aOptions: TRegExOptions): TMatchCollection; overload; static;
  MS:=TRegEx.Matches(TestStr,UpperCase(TestExpr),[roIgnoreCase]);
  AssertEquals('Match count',3,MS.Count);
  M:=MS[0];
  AssertTrue('Match 0 result: ',M.Success);
  AssertEquals('Match 0 value: ','abba',M.Value);
  M:=MS[1];
  AssertTrue('Match 0 result: ',M.Success);
  AssertEquals('Match 0 value: ','abbba',M.Value);
  M:=MS[2];
  AssertTrue('Match 0 result: ',M.Success);
  AssertEquals('Match 0 value: ','abbbba',M.Value);
end;

procedure TTestRegExp.TestReplace;
begin
  //  function Replace(const aInput, aReplacement: TREString): TREString; overload;
  RegEx:=TRegex.Create(TestExpr);
  AssertEquals('Result','xyz c c c zyx',RegEx.Replace(TestStr,'c'));
end;

function TTestRegExp.DoReplacer(const Match: TMatch): TREString;

begin
  Result:='<'+Match.Value+'>';
//  Writeln('Replace "',Match.Value,'" -> "',Result,'"')
end;

procedure TTestRegExp.TestReplaceEval;
begin
//  function Replace(const aInput: TREString; aEvaluator: TMatchEvaluator): TREString; overload;
  RegEx:=TRegex.Create(TestExpr);
  AssertEquals('Result','xyz <abba> <abbba> <abbbba> zyx',RegEx.Replace(TestStr,@DoReplacer));
end;

procedure TTestRegExp.TestReplaceCount;
begin
//  function Replace(const aInput, aReplacement: TREString; aCount: Integer): TREString; overload;
  RegEx:=TRegex.Create(TestExpr);
  AssertEquals('Result','xyz c c abbbba zyx',RegEx.Replace(TestStr,'c',2));
end;

procedure TTestRegExp.TestReplaceEvalCount;
begin
//  function Replace(const aInput: TREString; aEvaluator: TMatchEvaluator; aCount: Integer): TREString; overload;
  RegEx:=TRegex.Create(TestExpr);
  AssertEquals('Result','xyz <abba> <abbba> abbbba zyx',RegEx.Replace(TestStr,@DoReplacer,2));

end;

procedure TTestRegExp.TestClassReplace;
begin
//  class function Replace(const aInput, aPattern, aReplacement: TREString): TREString; overload; static;
  AssertEquals('Result','xyz c c c zyx',TRegEx.Replace(TestStr,TestExpr,'c'));

end;

procedure TTestRegExp.TestClassReplaceEval;
begin
//  class function Replace(const aInput, aPattern: TREString; aEvaluator: TMatchEvaluator): TREString; overload; static;
  AssertEquals('Result','xyz <abba> <abbba> <abbbba> zyx',TRegEx.Replace(TestStr,TestExpr,@DoReplacer));
end;

procedure TTestRegExp.TestClassReplaceOptions;
begin
//  class function Replace(const aInput, aPattern, aReplacement: TREString; aOptions: TRegExOptions): TREString; overload; static;
  AssertEquals('Result','xyz c c c zyx',TRegEx.Replace(TestStr,UpperCase(TestExpr),'c',[roIgnoreCase]));
end;

procedure TTestRegExp.TestClassReplaceEvalOptions;
begin
//  class function Replace(const aInput, aPattern: TREString; aEvaluator: TMatchEvaluator; aOptions: TRegExOptions): TREString; overload; static;
  AssertEquals('Result','xyz <abba> <abbba> <abbbba> zyx',TRegEx.Replace(TestStr,UpperCase(TestExpr),@DoReplacer,[roIgnoreCase]));

end;

initialization
  RegisterTest(TTestRegExp);
end.

