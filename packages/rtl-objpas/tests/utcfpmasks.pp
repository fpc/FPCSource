{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2026 by the Free Pascal development team

    Tests for the FPMasks unit: wildcards, sets, options, escapes, Unicode and mask lists.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit utcfpmasks;

{$mode objfpc}{$H+}
{$codepage utf8}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, fpmasks;

type

  { TTestFPMasks }

  TTestFPMasks = class(TTestCase)
  private
    FMask: UnicodeString;
    procedure CreateMask;
    procedure CreateMaskWithEscapes;
    procedure AssertMatch(const aFileName, aMask: UnicodeString);
    procedure AssertNoMatch(const aFileName, aMask: UnicodeString);
    procedure AssertInvalid(const aMask: UnicodeString; aPosition: Integer);
  published
    procedure TestLiteral;
    procedure TestEmpty;
    procedure TestCaseInsensitiveByDefault;
    procedure TestCaseSensitive;
    procedure TestStar;
    procedure TestStarBacktracking;
    procedure TestManyStars;
    procedure TestQuestionMark;
    procedure TestStarQuestionMark;
    procedure TestSet;
    procedure TestSetRange;
    procedure TestSetNegated;
    procedure TestSetDashLiteral;
    procedure TestSetCaseInsensitive;
    procedure TestSetCaseSensitive;
    procedure TestInvalidMasks;
    procedure TestInvalidMaskMessage;
    procedure TestDisableSets;
    procedure TestEscapes;
    procedure TestEscapeInSet;
    procedure TestEscapeAtEnd;
    procedure TestCustomEscapeChar;
    procedure TestBackslashLiteralByDefault;
    procedure TestUnicodeLiteral;
    procedure TestUnicodeCaseInsensitive;
    procedure TestUnicodeSet;
    procedure TestSurrogatePair;
    procedure TestMaskObjectReuse;
    procedure TestProperties;
    procedure TestMaskList;
    procedure TestMaskListEmptyParts;
    procedure TestMaskListSeparator;
    procedure TestMaskListCaseSensitive;
    procedure TestMaskListOptions;
    procedure TestMatchesMaskList;
    procedure TestParseStringList;
    procedure TestUTF8StringArguments;
  end;

implementation


procedure TTestFPMasks.CreateMask;

begin
  TMask.Create(FMask).Free;
end;


procedure TTestFPMasks.CreateMaskWithEscapes;

begin
  TMask.Create(FMask,[moUseEscapes]).Free;
end;


procedure TTestFPMasks.AssertMatch(const aFileName, aMask: UnicodeString);

begin
  AssertTrue(UTF8Encode('"'+aFileName+'" matches "'+aMask+'"'),MatchesMask(aFileName,aMask));
end;


procedure TTestFPMasks.AssertNoMatch(const aFileName, aMask: UnicodeString);

begin
  AssertFalse(UTF8Encode('"'+aFileName+'" does not match "'+aMask+'"'),MatchesMask(aFileName,aMask));
end;


procedure TTestFPMasks.AssertInvalid(const aMask: UnicodeString; aPosition: Integer);

begin
  FMask:=aMask;
  AssertException(UTF8Encode('Mask "'+aMask+'" is invalid'),EMaskException,@CreateMask,
    Format('"%s" is not a valid mask at (%d)',[UTF8Encode(aMask),aPosition]));
end;


procedure TTestFPMasks.TestLiteral;

begin
  AssertMatch('abc','abc');
  AssertNoMatch('abd','abc');
  AssertNoMatch('abcd','abc');
  AssertNoMatch('ab','abc');
end;


procedure TTestFPMasks.TestEmpty;

begin
  AssertMatch('','');
  AssertNoMatch('a','');
  AssertMatch('','*');
  AssertNoMatch('','?');
end;


procedure TTestFPMasks.TestCaseInsensitiveByDefault;

var
  lMask: TMask;

begin
  AssertMatch('README.PAS','*.pas');
  AssertMatch('readme.pas','*.PAS');
  lMask:=TMask.Create('Abc*');
  try
    AssertFalse('TMask ignores case by default',lMask.CaseSensitive);
    AssertTrue('ABCDEF matches Abc*',lMask.Matches('ABCDEF'));
  finally
    lMask.Free;
  end;
end;


procedure TTestFPMasks.TestCaseSensitive;

begin
  AssertFalse('README.PAS does not match *.pas case-sensitively',MatchesMask('README.PAS','*.pas',True));
  AssertTrue('readme.pas matches *.pas case-sensitively',MatchesMask('readme.pas','*.pas',True));
  AssertTrue('README.PAS matches *.pas when CaseSensitive is False',MatchesMask('README.PAS','*.pas',False));
end;


procedure TTestFPMasks.TestStar;

begin
  AssertMatch('anything','*');
  AssertMatch('abc','a*');
  AssertMatch('a','a*');
  AssertMatch('cab','*b');
  AssertMatch('file.txt','*.*');
  AssertNoMatch('file','*.*');
  AssertMatch('abc','**c');
  AssertMatch('ac','a*c');
end;


procedure TTestFPMasks.TestStarBacktracking;

begin
  AssertMatch('axxbyyc','a*b*c');
  AssertNoMatch('axxbyy','a*b*c');
  AssertMatch('abcabcabd','*abd');
  AssertMatch('mississippi','m*iss*ppi');
  AssertNoMatch('mississippi','m*iss*ppx');
end;


procedure TTestFPMasks.TestManyStars;

begin
  AssertMatch('a',StringOfChar(WideChar('*'),40)+'a');
  AssertMatch('abcdefghijklmnopqrst','*a*b*c*d*e*f*g*h*i*j*k*l*m*n*o*p*q*r*s*t*');
  AssertNoMatch('abcdefghijklmnopqrs','*a*b*c*d*e*f*g*h*i*j*k*l*m*n*o*p*q*r*s*t*');
end;


procedure TTestFPMasks.TestQuestionMark;

begin
  AssertMatch('abc','a?c');
  AssertNoMatch('ac','a?c');
  AssertNoMatch('abbc','a?c');
  AssertMatch('abc','???');
  AssertNoMatch('ab','???');
end;


procedure TTestFPMasks.TestStarQuestionMark;

begin
  AssertNoMatch('','*?');
  AssertMatch('x','*?');
  AssertNoMatch('','?*');
  AssertMatch('xyz','?*');
  AssertNoMatch('a','*??');
  AssertMatch('ab','*??');
end;


procedure TTestFPMasks.TestSet;

begin
  AssertMatch('ax','[abc]x');
  AssertMatch('cx','[abc]x');
  AssertNoMatch('dx','[abc]x');
  AssertNoMatch('x','[abc]x');
end;


procedure TTestFPMasks.TestSetRange;

begin
  AssertMatch('b1','[a-c][0-9]');
  AssertNoMatch('d1','[a-c][0-9]');
  AssertMatch('y','[a-cx-z]');
  AssertNoMatch('m','[a-cx-z]');
end;


procedure TTestFPMasks.TestSetNegated;

begin
  AssertMatch('d','[!a-c]');
  AssertNoMatch('b','[!a-c]');
  AssertNoMatch('','[!a-c]');
end;


procedure TTestFPMasks.TestSetDashLiteral;

begin
  AssertMatch('-','[-a]');
  AssertMatch('a','[-a]');
  AssertMatch('-','[a-]');
  AssertNoMatch('b','[a-]');
end;


procedure TTestFPMasks.TestSetCaseInsensitive;

begin
  AssertMatch('b','[A-C]');
  AssertMatch('B','[a-c]');
  AssertNoMatch('D','[a-c]');
end;


procedure TTestFPMasks.TestSetCaseSensitive;

begin
  AssertTrue('a matches [a] case-sensitively',MatchesMask('a','[a]',True));
  AssertFalse('A does not match [a] case-sensitively',MatchesMask('A','[a]',True));
  AssertTrue('B matches [A-C] case-sensitively',MatchesMask('B','[A-C]',True));
  AssertFalse('b does not match [A-C] case-sensitively',MatchesMask('b','[A-C]',True));
end;


procedure TTestFPMasks.TestInvalidMasks;

begin
  AssertInvalid('[',1);
  AssertInvalid('a[bc',2);
  AssertInvalid('[]',2);
  AssertInvalid('[!]',3);
  AssertInvalid('x[c-a]',5);
end;


procedure TTestFPMasks.TestInvalidMaskMessage;

var
  lMessage: String;

begin
  lMessage:='';
  try
    TMask.Create('*.[pas').Free;
  except
    on E: EMaskException do
      lMessage:=E.Message;
  end;
  AssertEquals('Message of an unclosed set','"*.[pas" is not a valid mask at (3)',lMessage);
end;


procedure TTestFPMasks.TestDisableSets;

var
  lMask: TMask;

begin
  lMask:=TMask.Create('[a]*.txt',[moDisableSets]);
  try
    AssertTrue('[a] is literal with moDisableSets',lMask.Matches('[a]1.txt'));
    AssertFalse('[a] is not a set with moDisableSets',lMask.Matches('a1.txt'));
  finally
    lMask.Free;
  end;
  lMask:=TMask.Create('[',[moDisableSets]);
  try
    AssertTrue('A lone [ is valid with moDisableSets',lMask.Matches('['));
  finally
    lMask.Free;
  end;
end;


procedure TTestFPMasks.TestEscapes;

var
  lMask: TMask;

begin
  lMask:=TMask.Create('a\*b',[moUseEscapes]);
  try
    AssertTrue('Escaped * is literal',lMask.Matches('a*b'));
    AssertFalse('Escaped * is not a wildcard',lMask.Matches('axb'));
  finally
    lMask.Free;
  end;
  lMask:=TMask.Create('\[x\]\?',[moUseEscapes]);
  try
    AssertTrue('Escaped [, ] and ? are literal',lMask.Matches('[x]?'));
    AssertFalse('Escaped ? does not match any character',lMask.Matches('[x]a'));
  finally
    lMask.Free;
  end;
  lMask:=TMask.Create('a\\b',[moUseEscapes]);
  try
    AssertTrue('Escaped escape character is literal',lMask.Matches('a\b'));
  finally
    lMask.Free;
  end;
end;


procedure TTestFPMasks.TestEscapeInSet;

var
  lMask: TMask;

begin
  lMask:=TMask.Create('[\]\-a]',[moUseEscapes]);
  try
    AssertTrue('Escaped ] in a set',lMask.Matches(']'));
    AssertTrue('Escaped - in a set',lMask.Matches('-'));
    AssertTrue('a in the set',lMask.Matches('a'));
    AssertFalse('b is not in the set',lMask.Matches('b'));
  finally
    lMask.Free;
  end;
end;


procedure TTestFPMasks.TestEscapeAtEnd;

begin
  FMask:='abc\';
  AssertException('An escape character at the end is invalid',EMaskException,@CreateMaskWithEscapes);
end;


procedure TTestFPMasks.TestCustomEscapeChar;

var
  lMask: TMask;

begin
  lMask:=TMask.Create('#*\*',[moUseEscapes],'#');
  try
    AssertTrue('# escapes *, \ is literal',lMask.Matches('*\abc'));
    AssertFalse('The escaped * must be present',lMask.Matches('x\abc'));
    AssertEquals('EscapeChar property','#',lMask.EscapeChar);
  finally
    lMask.Free;
  end;
end;


procedure TTestFPMasks.TestBackslashLiteralByDefault;

begin
  AssertMatch('dir\file.pas','dir\*.pas');
  AssertNoMatch('dirxfile.pas','dir\*.pas');
end;


procedure TTestFPMasks.TestUnicodeLiteral;

begin
  AssertMatch('καλημέρα.txt','καλη*.txt');
  AssertNoMatch('καλησπέρα.txt','καλημ*.txt');
  AssertMatch('日本語.doc','日本?.doc');
end;


procedure TTestFPMasks.TestUnicodeCaseInsensitive;

begin
  AssertMatch('ΚΑΛΗΜΕΡΑ.TXT','καλημερα.*');
  AssertMatch('Ärger.txt','ä*');
  AssertFalse('Greek case differs case-sensitively',MatchesMask('ΚΑΛΗΜΕΡΑ','καλημερα',True));
end;


procedure TTestFPMasks.TestUnicodeSet;

begin
  AssertMatch('λ','[α-ω]');
  AssertNoMatch('a','[α-ω]');
  AssertMatch('x中','x[中文]');
  AssertMatch('Λ','[α-ω]');
end;


procedure TTestFPMasks.TestSurrogatePair;

begin
  AssertMatch('a'#$D83D#$DE00'b','a?b');
  AssertNoMatch('a'#$D83D#$DE00'b','a??b');
  AssertMatch(#$D83D#$DE00,'['#$D83D#$DE00']');
end;


procedure TTestFPMasks.TestMaskObjectReuse;

var
  lMask: TMask;

begin
  lMask:=TMask.Create('*.p?s');
  try
    AssertTrue('a.pas',lMask.Matches('a.pas'));
    AssertTrue('b.pps',lMask.Matches('b.pps'));
    AssertFalse('c.pp',lMask.Matches('c.pp'));
  finally
    lMask.Free;
  end;
end;


procedure TTestFPMasks.TestProperties;

var
  lMask: TMask;

begin
  lMask:=TMask.Create('*.PAS',True);
  try
    AssertEquals('Mask property','*.PAS',lMask.Mask);
    AssertTrue('CaseSensitive property',lMask.CaseSensitive);
    AssertTrue('Options contain moCaseSensitive',moCaseSensitive in lMask.Options);
    AssertEquals('Default escape character','\',lMask.EscapeChar);
  finally
    lMask.Free;
  end;
end;


procedure TTestFPMasks.TestMaskList;

var
  lList: TMaskList;

begin
  lList:=TMaskList.Create('*.pas;*.pp');
  try
    AssertEquals('Two masks',2,lList.Count);
    AssertEquals('First mask','*.pas',lList.Items[0].Mask);
    AssertEquals('Mask property of the list','*.pas;*.pp',lList.Mask);
    AssertTrue('a.pp matches the list',lList.Matches('a.pp'));
    AssertTrue('A.PAS matches the list',lList.Matches('A.PAS'));
    AssertFalse('a.inc does not match the list',lList.Matches('a.inc'));
  finally
    lList.Free;
  end;
end;


procedure TTestFPMasks.TestMaskListEmptyParts;

var
  lList: TMaskList;

begin
  lList:=TMaskList.Create(';*.pas;;');
  try
    AssertEquals('Empty parts are left out',1,lList.Count);
  finally
    lList.Free;
  end;
  lList:=TMaskList.Create('');
  try
    AssertEquals('An empty list has no masks',0,lList.Count);
    AssertFalse('An empty list matches nothing',lList.Matches('a'));
  finally
    lList.Free;
  end;
end;


procedure TTestFPMasks.TestMaskListSeparator;

var
  lList: TMaskList;

begin
  lList:=TMaskList.Create('*.pas,*.inc',',');
  try
    AssertEquals('Two masks separated by a comma',2,lList.Count);
    AssertTrue('a.inc matches',lList.Matches('a.inc'));
  finally
    lList.Free;
  end;
end;


procedure TTestFPMasks.TestMaskListCaseSensitive;

var
  lList: TMaskList;

begin
  lList:=TMaskList.Create('*.pas;*.pp',';',True);
  try
    AssertFalse('A.PAS does not match case-sensitively',lList.Matches('A.PAS'));
    AssertTrue('a.pas matches case-sensitively',lList.Matches('a.pas'));
  finally
    lList.Free;
  end;
end;


procedure TTestFPMasks.TestMaskListOptions;

var
  lList: TMaskList;

begin
  lList:=TMaskList.Create('[1].txt;\*.log',';',[moDisableSets,moUseEscapes]);
  try
    AssertTrue('[1].txt is literal',lList.Matches('[1].txt'));
    AssertTrue('Escaped * in the second mask',lList.Matches('*.log'));
    AssertFalse('The escaped * is not a wildcard',lList.Matches('a.log'));
  finally
    lList.Free;
  end;
end;


procedure TTestFPMasks.TestMatchesMaskList;

begin
  AssertTrue('a.pp matches *.pas;*.pp',MatchesMaskList('a.pp','*.pas;*.pp'));
  AssertFalse('a.inc does not match *.pas;*.pp',MatchesMaskList('a.inc','*.pas;*.pp'));
  AssertTrue('Custom separator',MatchesMaskList('a.inc','*.pas|*.inc','|'));
  AssertFalse('Case-sensitive list',MatchesMaskList('A.PAS','*.pas','|',True));
end;


procedure TTestFPMasks.TestParseStringList;

var
  lList: TParseStringList;

begin
  lList:=TParseStringList.Create('a;b,,c;',';,');
  try
    AssertEquals('Three parts',3,lList.Count);
    AssertEquals('Third part','c',lList[2]);
  finally
    lList.Free;
  end;
end;


procedure TTestFPMasks.TestUTF8StringArguments;

var
  lName, lMask: UTF8String;

begin
  lName:='καλημέρα.pas';
  lMask:='*.PAS';
  AssertTrue('UTF8String arguments',MatchesMask(lName,lMask));
  lMask:='καλη*';
  AssertTrue('UTF8String mask with non-ASCII characters',MatchesMask(lName,lMask));
end;


initialization
  RegisterTest(TTestFPMasks);
end.
