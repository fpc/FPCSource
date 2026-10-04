{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    Markdown PDF renderer tests

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit UTest.Markdown.PDFRender;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, fppdf,
  Markdown.Elements, Markdown.Render, Markdown.Parser, Markdown.PDFRender;

type

  { TTextCollectingPDFRenderer }

  // Records every text run that is laid out
  TTextCollectingPDFRenderer = class(TMarkDownPDFRenderer)
  private
    FRuns : TStringList;
  protected
    procedure FlushTextRun(const aText: utf8string; const aFontSize: LongInt;
      const aFontStyle: TFontStyles; const aLinkHref: utf8string; const aContext : TFontContext;
      const aDryRun: boolean); override;
  public
    constructor Create(aOwner : TComponent); override;
    destructor Destroy; override;
    // Text of the runs in layout order, with the font size in Objects
    property Runs : TStringList read FRuns;
  end;

  { TTestPDFRender }

  TTestPDFRender = class(TTestCase)
  private
    FRenderer : TTextCollectingPDFRenderer;
    FPDF : TPDFDocument;
    FMarkers : TStringList;
    procedure DoMarker(aRenderer : TMarkdownRenderer; const aName, aArgument, aValue : String);
  protected
    procedure Render(const aMarkdown : String);
    function AllText : String;
    procedure CheckText(const aMsg, aExpected : String);
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestPlainDocument;
    procedure TestSetextHeading;
    procedure TestDefinitionList;
    procedure TestAlert;
    procedure TestFootnotesAtEnd;
    procedure TestTableCaption;
    procedure TestFigureWithoutImage;
    procedure TestCommentWritesNothing;
    procedure TestMarkerEvent;
    procedure TestDestinations;
    procedure TestEmptyLinkWritesTarget;
  end;

implementation

uses
  Markdown.Processors;

{ TTextCollectingPDFRenderer }

constructor TTextCollectingPDFRenderer.Create(aOwner: TComponent);

begin
  inherited Create(aOwner);
  FRuns:=TStringList.Create;
end;


destructor TTextCollectingPDFRenderer.Destroy;

begin
  FreeAndNil(FRuns);
  inherited Destroy;
end;


procedure TTextCollectingPDFRenderer.FlushTextRun(const aText: utf8string; const aFontSize: LongInt;
  const aFontStyle: TFontStyles; const aLinkHref: utf8string; const aContext: TFontContext; const aDryRun: boolean);

begin
  if not aDryRun then
    FRuns.AddObject(aText,TObject(PtrInt(aFontSize)));
  inherited FlushTextRun(aText, aFontSize, aFontStyle, aLinkHref, aContext, aDryRun);
end;

{ TTestPDFRender }

procedure TTestPDFRender.SetUp;

begin
  FRenderer:=TTextCollectingPDFRenderer.Create(Nil);
  FPDF:=TPDFDocument.Create(Nil);
  FPDF.Options:=[poPageOriginAtTop];
  FPDF.DefaultUnitOfMeasure:=uomPixels;
  FMarkers:=TStringList.Create;
end;


procedure TTestPDFRender.TearDown;

begin
  FreeAndNil(FMarkers);
  FreeAndNil(FPDF);
  FreeAndNil(FRenderer);
end;


procedure TTestPDFRender.DoMarker(aRenderer: TMarkdownRenderer; const aName, aArgument, aValue: String);

begin
  FMarkers.Add(aName+'='+aValue);
end;


procedure TTestPDFRender.Render(const aMarkdown: String);

var
  lSource : TStringList;
  lDoc : TMarkdownDocument;
begin
  lSource:=TStringList.Create;
  try
    lSource.Text:=aMarkdown;
    lDoc:=TMarkdownParser.FastParse(lSource,MarkdownDocExtensions);
    try
      FRenderer.RenderDocument(lDoc,FPDF);
    finally
      lDoc.Free;
    end;
  finally
    lSource.Free;
  end;
end;


function TTestPDFRender.AllText: String;

begin
  Result:=String.Join('|',FRenderer.Runs.ToStringArray);
end;


procedure TTestPDFRender.CheckText(const aMsg, aExpected: String);

begin
  AssertTrue(aMsg+': expected "'+aExpected+'" in "'+AllText+'"',Pos(aExpected,AllText)>0);
end;


procedure TTestPDFRender.TestPlainDocument;

begin
  Render('# Title'#10#10'Some text.');
  AssertEquals('One page',1,FRenderer.PageList.Count);
  CheckText('Heading','Title');
  CheckText('Paragraph','Some text.');
end;


procedure TTestPDFRender.TestSetextHeading;

var
  lHeading,lPara : Integer;
begin
  Render('Title'#10'====='#10#10'Body');
  lHeading:=FRenderer.Runs.IndexOf('Title');
  lPara:=FRenderer.Runs.IndexOf('Body');
  AssertTrue('Heading laid out',lHeading>=0);
  AssertTrue('Paragraph laid out',lPara>=0);
  AssertEquals('Heading font size',FRenderer.BaseFontSize+10,PtrInt(FRenderer.Runs.Objects[lHeading]));
  AssertEquals('Paragraph font size',FRenderer.BaseFontSize,PtrInt(FRenderer.Runs.Objects[lPara]));
end;


procedure TTestPDFRender.TestDefinitionList;

begin
  Render('Term'#10': Definition');
  CheckText('Term','Term');
  CheckText('Definition','Definition');
end;


procedure TTestPDFRender.TestAlert;

begin
  FRenderer.AlertTitles[atNote]:='Remark';
  Render('> [!NOTE]'#10'> Text');
  CheckText('Alert title','Remark');
  CheckText('Alert content','Text');
end;


procedure TTestPDFRender.TestFootnotesAtEnd;

begin
  Render('Text.[^n]'#10#10'[^n]: The note.'#10#10'Last paragraph.');
  CheckText('Reference number','Text.|1');
  AssertEquals('Note text is laid out last','The note.',Trim(FRenderer.Runs[FRenderer.Runs.Count-1]));
end;


procedure TTestPDFRender.TestTableCaption;

var
  lCaption : Integer;
begin
  Render('| a |'#10'|---|'#10'| 1 |'#10#10'Table: Values');
  lCaption:=FRenderer.Runs.IndexOf('Values');
  AssertTrue('Caption laid out',lCaption>=0);
  AssertEquals('Cells laid out after the caption','a',FRenderer.Runs[lCaption+1]);
end;


procedure TTestPDFRender.TestFigureWithoutImage;

begin
  Render('![A figure](does-not-exist.png)');
  CheckText('Alt text for missing image','[A figure]');
  CheckText('Caption','A figure');
end;


procedure TTestPDFRender.TestCommentWritesNothing;

begin
  Render('<!-- hidden -->'#10#10'A <!-- also hidden --> b');
  AssertEquals('No comment text',0,Pos('hidden',AllText));
end;


procedure TTestPDFRender.TestMarkerEvent;

begin
  FRenderer.OnMarker:=@DoMarker;
  Render('<!-- index: Block -->'#10#10'A <!-- index: Inline --> b');
  AssertEquals('Two markers',2,FMarkers.Count);
  AssertEquals('Block marker','index=Block',FMarkers[0]);
  AssertEquals('Inline marker','index=Inline',FMarkers[1]);
end;


procedure TTestPDFRender.TestDestinations;

begin
  Render('# Intro'#10#10'Text'#10#10'```pascal {#lst}'#10'x'#10'```');
  AssertTrue('Heading destination',FRenderer.Destinations.IndexOf('intro')>=0);
  AssertTrue('Code destination',FRenderer.Destinations.IndexOf('lst')>=0);
  AssertEquals('Destination page',0,TPDFDestination(FRenderer.Destinations.Objects[0]).PageIndex);
end;


procedure TTestPDFRender.TestEmptyLinkWritesTarget;

begin
  Render('See [](other.md).');
  CheckText('Target as text','other.md');
end;

initialization
  RegisterTest(TTestPDFRender);
end.
