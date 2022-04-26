{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2022 by Michael van Canneyt and other members of the
    Free Pascal development team

    report html parser test

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit tchtmlparser;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  fpcunit,
  testregistry,
  fpReportHTMLParser;

type
  TTestHTMLParser = class(TTestCase)
  private
    FParser: THTMLParser;
    FTags: TStringList;
    FText: TStringList;
    procedure InitParser(const AText: string);
    procedure CaptureTagsFound(NoCaseTag, ActualTag: string);
    procedure CaptureTextFound(Text: string);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  public
  published
    procedure TestOneTagPair;
    procedure TestNoTags;
    procedure TestTagAndRemainingText;
    procedure TestRegularTextAndTag;
    procedure TestTagNoText;
    procedure TestGetTagName;
    procedure TestGetNameValPair;
    procedure TestGetValFromNameVal;
    procedure TestGetVal;
  end;

implementation

{ TTestHTMLParser }

procedure TTestHTMLParser.InitParser(const AText: string);
begin
  FParser := THTMLParser.Create(AText);
  FParser.OnFoundTag := @CaptureTagsFound;
  FParser.OnFoundText := @CaptureTextFound;
  FParser.Exec;
end;

procedure TTestHTMLParser.CaptureTagsFound(NoCaseTag, ActualTag: string);
begin
  FTags.Add(NoCaseTag);
end;

procedure TTestHTMLParser.CaptureTextFound(Text: string);
begin
  FText.Add(Text);
end;

procedure TTestHTMLParser.SetUp;
begin
  inherited SetUp;
  FParser := nil;
  FTags := TStringList.Create;
  FText := TStringList.Create;
end;

procedure TTestHTMLParser.TearDown;
begin
  FParser.Free;
  FTags.Free;
  FText.Free;
  inherited TearDown;
end;

procedure TTestHTMLParser.TestOneTagPair;
begin
  InitParser('<i>italics</i>');
  AssertEquals('Failed on 1', FTags[0], '<I>');
  AssertEquals('Failed on 2', FTags[1], '</I>');
  AssertEquals('Failed on 3', FText[0], 'italics');
end;

procedure TTestHTMLParser.TestNoTags;
begin
  InitParser('italics');
  AssertEquals('Failed on 1', FTags.Text, '');
  AssertEquals('Failed on 2', FText[0], 'italics');
end;

procedure TTestHTMLParser.TestTagAndRemainingText;
begin
  InitParser('<i>italics</i> regular text');
  AssertEquals('Failed on 1', FTags[0], '<I>');
  AssertEquals('Failed on 2', FTags[1], '</I>');
  AssertEquals('Failed on 3', FText[0], 'italics');
  AssertEquals('Failed on 4', FText[1], ' regular text');
end;

procedure TTestHTMLParser.TestRegularTextAndTag;
begin
  InitParser('regular text <i>italics</i>');
  AssertEquals('Failed on 1', FTags[0], '<I>');
  AssertEquals('Failed on 2', FTags[1], '</I>');
  AssertEquals('Failed on 3', FText[0], 'regular text ');
  AssertEquals('Failed on 4', FText[1], 'italics');
end;

procedure TTestHTMLParser.TestTagNoText;
begin
  InitParser('<i></i>');
  AssertEquals('Failed on 1', FTags[0], '<I>');
  AssertEquals('Failed on 2', FTags[1], '</I>');
  AssertEquals('Failed on 3', FText.Text, '');
end;

procedure TTestHTMLParser.TestGetTagName;
begin
  AssertEquals('failed on 1', 'I', FParser.GetTagName('<I>'));
  AssertTrue('failed on 2 - case preserved', FParser.GetTagName('<I>') <> 'i');
  AssertEquals('failed on 3', '/I', FParser.GetTagName('</I>'));
  AssertEquals('failed on 4', 'i', FParser.GetTagName('<i>'));
  AssertEquals('failed on 5', 'a', FParser.GetTagName('<a href="#hello">'));
  AssertEquals('failed on 6', 'a', FParser.GetTagName('<a href="http://www.freepascal.org">'));
  AssertEquals('failed on 7 - multi character tag', 'table', FParser.GetTagName('<table cellpadding=5 cellspacing=10 class="main">'));
end;

procedure TTestHTMLParser.TestGetNameValPair;
begin
  AssertEquals('failed on 1', '', FParser.GetNameValPair('<I>', ''));
  AssertEquals('failed on 2', '', FParser.GetNameValPair('</I>', 'href'));
  AssertEquals('failed on 3', '', FParser.GetNameValPair('<i>', ''));
  AssertEquals('failed on 4', 'href="#hello"', FParser.GetNameValPair('<a href="#hello">', 'href'));
  AssertEquals('failed on 5', 'href="http://www.freepascal.org"', FParser.GetNameValPair('<a href="http://www.freepascal.org">', 'href'));
  AssertEquals('failed on 6', 'cellpadding=5', FParser.GetNameValPair('<table cellpadding=5 cellspacing=10 class="main">', 'cellpadding'));
  AssertEquals('failed on 7', 'cellspacing=10', FParser.GetNameValPair('<table cellpadding=5 cellspacing=10 class="main">', 'cellspacing'));
  AssertEquals('failed on 8', 'class="main"', FParser.GetNameValPair('<table cellpadding=5 cellspacing=10 class="main">', 'class'));
end;

procedure TTestHTMLParser.TestGetValFromNameVal;
begin
  AssertEquals('failed on 1', '#hello', FParser.GetValFromNameVal('href="#hello"'));
  AssertEquals('failed on 2', 'http://www.freepascal.org', FParser.GetValFromNameVal('href="http://www.freepascal.org"'));
  AssertEquals('failed on 3', '5', FParser.GetValFromNameVal('cellpadding=5'));
  AssertEquals('failed on 4', 'black', FParser.GetValFromNameVal('bgcolor=black'));
  AssertEquals('failed on 5', 'main', FParser.GetValFromNameVal('class="main"'));
  AssertEquals('failed on 6', 'http://www.freepascal.org/docs/docs.php?num=10', FParser.GetValFromNameVal('href="http://www.freepascal.org/docs/docs.php?num=10"'));
end;

procedure TTestHTMLParser.TestGetVal;
begin
  AssertEquals('failed on 1', '', FParser.GetVal('<I>', ''));
  AssertEquals('failed on 2', '', FParser.GetVal('</I>', 'href'));
  AssertEquals('failed on 3', '', FParser.GetVal('<i>', ''));
  AssertEquals('failed on 4', '#hello', FParser.GetVal('<a href="#hello">', 'href'));
  AssertEquals('failed on 5', 'http://www.freepascal.org', FParser.GetVal('<a href="http://www.freepascal.org">', 'href'));
  AssertEquals('failed on 6', '5', FParser.GetVal('<table cellpadding=5 cellspacing=10 class="main">', 'cellpadding'));
  AssertEquals('failed on 7', '10', FParser.GetVal('<table cellpadding=5 cellspacing=10 class="main">', 'cellspacing'));
  AssertEquals('failed on 8', 'main', FParser.GetVal('<table cellpadding=5 cellspacing=10 class="main">', 'class'));
  AssertEquals('failed on 9', 'http://www.freepascal.org/docs/docs.php?num=10', FParser.GetVal('<a href="http://www.freepascal.org/docs/docs.php?num=10">', 'href'));
end;


initialization
  RegisterTests([TTestHTMLParser]);
end.


end.

