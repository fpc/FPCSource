unit utcregex;

{$mode objfpc}{$H+}
{ $DEFINE USEWIDESTRING}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, system.regularexpressionscore;

type

  { TTestRegExpCore }

  TTestRegExpCore = class(TTestCase)
  private
    FRegex: TPerlRegEx;
    FMatchEventCount : Integer;
    FSplitSubject: TStrings;
    procedure AssertMatch(const Msg, aMatch: TREString; aPos, aLength: Integer; Groups: array of TREString);
    procedure DoMatch(Sender: TObject);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
    Property RegEx : TPerlRegEx Read FRegex;
    Property SplitSubject : TStrings Read FSplitSubject;
  published
    Procedure TestHookup;
    procedure TestMatch;
    procedure TestMatchStart;
    procedure TestMatchStop;
    procedure TestNamedGroups;
    procedure TestReplace;
    procedure TestReplaceAll;
    procedure TestSplitAll;
    procedure TestSplitLimit;
    procedure TestInitialCaps;
    procedure TestReplaceGroupBackslash;
    procedure TestReplaceGroupDollar;
    procedure TestReplaceGroupQuoted;
    procedure TestReplaceGroupNamed;
    procedure TestReplaceGroupNamed2;
    procedure TestReplaceGroupNamedInvalidName;
    procedure TestReplaceWholeSubject;
    procedure TestReplaceLeftOfMatch;
    procedure TestReplaceRightOfMatch;
    procedure TestReplaceWholeMatch;
    procedure TestReplaceLastMatch;
  end;

implementation

Const
  TestStr = 'xyz abba abbba abbbba zyx';
  TestExpr = 'a(b*)a';

procedure TTestRegExpCore.AssertMatch(const Msg, aMatch: TREString; aPos, aLength: Integer; Groups: array of TREString);

var
  I : Integer;
begin
  AssertTrue(Msg+': Found match',Regex.FoundMatch);
  AssertEquals(Msg+': matched text',aMatch,Regex.MatchedText);
  AssertEquals(Msg+': offset',aPos,Regex.MatchedOffset);
  AssertEquals(Msg+': length',aLength,Regex.MatchedLength);
  AssertEquals(Msg+': group count',Length(Groups),Regex.GroupCount);
  For I:=1 to Regex.GroupCount do
    AssertEquals(Msg+' group['+IntToStr(I)+']',Groups[I-1],Regex.Groups[I]);
end;

procedure TTestRegExpCore.DoMatch(Sender: TObject);
begin
  Inc(FMatchEventCount);
end;

procedure TTestRegExpCore.TestMatch;

begin
  Regex.subject:=TestStr;
  Regex.RegEx:=TestExpr;
  AssertTrue('First match found',Regex.Match);
  AssertEquals('Match event called',1,FMatchEventCount);
  AssertMatch('Match 1','abba',5,4,['bb']);
  AssertEquals('Left of match','xyz ',Regex.SubjectLeft);
  AssertEquals('Right of match',' abbba abbbba zyx',Regex.SubjectRight);
  AssertTrue('Second match found',Regex.MatchAgain);
  AssertMatch('Match 2','abbba',10,5,['bbb']);
  AssertTrue('Third match found',Regex.MatchAgain);
  AssertMatch('Match 3','abbbba',16,6,['bbbb']);
  AssertFalse('No more matches',Regex.MatchAgain);
  AssertEquals('Match event called',3,FMatchEventCount);
end;

procedure TTestRegExpCore.TestMatchStart;

begin
  Regex.subject:=TestStr;
  Regex.RegEx:=TestExpr;
  Regex.Start:=Pos('abbba',TestStr);
  AssertTrue('First match found',Regex.Match);
  AssertMatch('Match 1','abbba',10,5,['bbb']);

  AssertTrue('Second match found',Regex.MatchAgain);
  AssertMatch('Match 3','abbbba',16,6,['bbbb']);
  AssertFalse('No more matches',Regex.MatchAgain);

end;

procedure TTestRegExpCore.TestMatchStop;
begin
  Regex.subject:=TestStr;
  Regex.RegEx:=TestExpr;
  Regex.Stop:=4;
  AssertFalse('No match found',Regex.Match);
  Regex.Stop:=9;
  AssertTrue('First match found',Regex.Match);
  AssertEquals('Match event called',1,FMatchEventCount);
  AssertMatch('Match 1','abba',5,4,['bb']);
  AssertFalse('No more matches',Regex.MatchAgain);
  AssertEquals('Match event not called again',1,FMatchEventCount);
end;

procedure TTestRegExpCore.TestNamedGroups;

Const
  Rec1 = 'Name:"John" Surname:"Doe" Email:"john@example.com"';
  Rec2 = 'Name:"Jane" Surname:"Dolina" Email:"jane@doe.com"';

begin
  Regex.Subject:=Rec1+#10+Rec2;
  Regex.RegEx:='Name:"(?<Name>[\w]+?)".*?Surname:"(?<Surname>[\w]+?)".*?Email:"(?<Email>\b[\w.%+-]+@[A-Za-z0-9.-]+\.[A-Za-z]{2,6}\b)"';
  AssertTrue('First match found',Regex.Match);
  AssertMatch('Match 1',Rec1,1,Length(Rec1),['John','Doe','john@example.com']);
  AssertEquals('Nonexisting group','',Regex.NamedGroups['nonexisting']);
  AssertEquals('Name group','John',Regex.NamedGroups['Name']);
  AssertEquals('Surname group','Doe',Regex.NamedGroups['Surname']);
  AssertEquals('Email group','john@example.com',Regex.NamedGroups['Email']);
  AssertTrue('Second match found',Regex.MatchAgain);
  AssertMatch('Match 2',Rec2,Length(Rec1)+2,Length(Rec2),['Jane','Dolina','jane@doe.com']);
  AssertFalse('No more matches',Regex.MatchAgain);
end;

procedure TTestRegExpCore.TestReplace;
begin
  Regex.subject:=TestStr;
  Regex.RegEx:=TestExpr;
  Regex.Replacement:='c';
  AssertTrue('First match found',Regex.Match);
  AssertEquals('Replace','c',Regex.Replace);
  AssertEquals('Replace result','xyz c abbba abbbba zyx',Regex.Subject);
  AssertTrue('Second match found',Regex.MatchAgain);
  AssertEquals('Replace 2','c',Regex.Replace);
  AssertEquals('Replace 2 result','xyz c c abbbba zyx',Regex.Subject);
  AssertTrue('Third match found',Regex.MatchAgain);
  AssertEquals('Replace 3','c',Regex.Replace);
  AssertEquals('Replace 3 result','xyz c c c zyx',Regex.Subject);
  AssertFalse('No more matches',Regex.MatchAgain);
end;

procedure TTestRegExpCore.TestReplaceAll;
begin
  Regex.subject:=TestStr;
  Regex.RegEx:=TestExpr;
  Regex.Replacement:='c';
  AssertTrue('Replacements done',Regex.ReplaceAll);
  AssertEquals('ReplaceAll result','xyz c c c zyx',Regex.Subject);
end;


procedure TTestRegExpCore.TestReplaceGroupBackslash;
// \n
begin
  Regex.subject:='*abba*';
  Regex.RegEx:=TestExpr;
  Regex.Replacement:='\1';
  AssertTrue('Match',Regex.Match);
  AssertEquals('ReplaceText','bb',Regex.Replace);
  AssertEquals('Result','*bb*',Regex.Subject);
end;

procedure TTestRegExpCore.TestReplaceGroupDollar;
// $N
begin
  Regex.subject:='*abba*';
  Regex.RegEx:=TestExpr;
  Regex.Replacement:='$1';
  AssertTrue('Match',Regex.Match);
  AssertEquals('ReplaceText','bb',Regex.Replace);
  AssertEquals('Result','*bb*',Regex.Subject);
end;

procedure TTestRegExpCore.TestReplaceGroupQuoted;
// \{N}
begin
  Regex.subject:='*abba*';
  Regex.RegEx:=TestExpr;
  Regex.Replacement:='\{1}';
  AssertTrue('Match',Regex.Match);
  AssertEquals('ReplaceText','bb',Regex.Replace);
  AssertEquals('Result','*bb*',Regex.Subject);
end;

procedure TTestRegExpCore.TestReplaceGroupNamed;
// \{name}

begin
  Regex.subject:='*abba*';
  Regex.RegEx:='a(?<Name>b*?)a';
  Regex.Replacement:='\{Name}';
  AssertTrue('Match',Regex.Match);
  AssertEquals('ReplaceText','bb',Regex.Replace);
  AssertEquals('Result','*bb*',Regex.Subject);
end;

procedure TTestRegExpCore.TestReplaceGroupNamed2;
// \{name}
begin
  Regex.subject:='*abba*';
  Regex.RegEx:='a(?<Name>b*?)a';
  Regex.Replacement:='<\{Name}>';
  AssertTrue('Match',Regex.Match);
  AssertEquals('ReplaceText','<bb>',Regex.Replace);
  AssertEquals('Result','*<bb>*',Regex.Subject);
end;

procedure TTestRegExpCore.TestReplaceGroupNamedInvalidName;
// \{name} with invalid name
begin
  Regex.subject:='*abba*';
  Regex.RegEx:='a(?<Name>b*?)a';
  Regex.Replacement:='<\{NameX}>';
  AssertTrue('Match',Regex.Match);
  AssertEquals('ReplaceText','<>',Regex.Replace);
  AssertEquals('Result','*<>*',Regex.Subject);
end;

procedure TTestRegExpCore.TestReplaceWholeSubject;
begin
  Regex.subject:='*abba*';
  Regex.RegEx:=TestExpr;
  Regex.Replacement:='<\_>';
  AssertTrue('Match',Regex.Match);
  AssertEquals('ReplaceText','<*abba*>',Regex.Replace);
  AssertEquals('Result','*<*abba*>*',Regex.Subject);
end;

procedure TTestRegExpCore.TestReplaceLeftOfMatch;
// \`
begin
  Regex.subject:='x*abba*';
  Regex.RegEx:=TestExpr;
  Regex.Replacement:='<\`>';
  AssertTrue('Match',Regex.Match);
  AssertEquals('ReplaceText','<x*>',Regex.Replace);
  AssertEquals('Result','x*<x*>*',Regex.Subject);
end;

procedure TTestRegExpCore.TestReplaceRightOfMatch;
// \'
begin
  Regex.subject:='*abba*x';
  Regex.RegEx:=TestExpr;
  Regex.Replacement:='<\''>';
  AssertTrue('Match',Regex.Match);
  AssertEquals('ReplaceText','<*x>',Regex.Replace);
  AssertEquals('Result','*<*x>*x',Regex.Subject);
end;

procedure TTestRegExpCore.TestReplaceWholeMatch;
// \&
begin
  Regex.subject:='*abba*';
  Regex.RegEx:=TestExpr;
  Regex.Replacement:='<\&>';
  AssertTrue('Match',Regex.Match);
  AssertEquals('ReplaceText','<abba>',Regex.Replace);
  AssertEquals('Result','*<abba>*',Regex.Subject);
end;

procedure TTestRegExpCore.TestReplaceLastMatch;
// \&
begin
  Regex.subject:='*abbcca*';
  Regex.RegEx:='a(b*)(c*)a';
  Regex.Replacement:='<\+>';
  AssertTrue('Match',Regex.Match);
  AssertEquals('ReplaceText','<cc>',Regex.Replace);
  AssertEquals('Result','*<cc>*',Regex.Subject);
end;


procedure TTestRegExpCore.TestSplitAll;
begin
  Regex.subject:=TestStr;
  Regex.RegEx:='\s';
  Regex.Split(SplitSubject,0);
  AssertEquals('Count',5,SplitSubject.Count);
  AssertEquals('Item 0','xyz',SplitSubject[0]);
  AssertEquals('Item 1','abba',SplitSubject[1]);
  AssertEquals('Item 2','abbba',SplitSubject[2]);
  AssertEquals('Item 3','abbbba',SplitSubject[3]);
  AssertEquals('Item 4','zyx',SplitSubject[4]);
end;

procedure TTestRegExpCore.TestSplitLimit;

begin
  Regex.subject:=TestStr;
  Regex.RegEx:='\s';
  Regex.Split(SplitSubject,2);
  AssertEquals('Count',2,SplitSubject.Count);
  AssertEquals('Item 0','xyz',SplitSubject[0]);
  AssertEquals('Item 1','abba abbba abbbba zyx',SplitSubject[1]);
end;

procedure TTestRegExpCore.TestInitialCaps;
begin
  AssertEquals('Initialcaps 1','Abc',InitialCaps('aBc'));
  AssertEquals('Initialcaps 2',' Abc',InitialCaps(' aBc'));
  AssertEquals('Initialcaps 3','Dad Abc',InitialCaps('dAd aBc'));
  AssertEquals('Initialcaps 4','Dad Abc ',InitialCaps('dAd aBc '));
end;

procedure TTestRegExpCore.SetUp;
begin
  FRegex:=TPerlRegEx.Create;
  FRegEx.OnMatch:=@DoMatch;
  FMatchEventCount:=0;
  FSplitSubject:=TStringList.Create;
end;

procedure TTestRegExpCore.TearDown;

begin
  FreeAndNil(FSplitSubject);
  FreeAndNil(FRegex);
end;

procedure TTestRegExpCore.TestHookup;
begin
  AssertNotNull('Regex',Regex);
  AssertTrue('Assigned OnMatch event',Assigned(Regex.OnMatch));
  AssertEquals('Match event count',0,FMatchEventCount);

end;

initialization

  RegisterTest(TTestRegExpCore);
end.

