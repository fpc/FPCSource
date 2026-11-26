{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2025 by Michael Van Canneyt

    Markdown code highlighting tests

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 *********************************************************************}
unit UTest.Markdown.CodeHighlight;

{$mode ObjFPC}{$H+}

interface

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.Classes, System.SysUtils, FpcUnit.Test, FpcUnit.Registry, FpPdf.Pdf,
{$ELSE}
  Classes, SysUtils, fpcunit, testregistry, fppdf,
{$ENDIF}
  Markdown.Utils, Markdown.Elements, Markdown.Render, Markdown.Parser, Markdown.CodeHighlight,
  Markdown.HtmlRender, Markdown.AnsiRender, Markdown.LatexRender, Markdown.PDFRender;

type

  { TTestCodeHighlighter }

  // Highlighter for language "test": begin and end are keywords, {...} a comment, digits a number,
  // '...' a string, everything else default text.
  TTestCodeHighlighter = class(TMarkdownCodeHighlighter)
  private
    FCallCount : Integer;
    FLastSource : TMarkdownString;
  public
    // Tokenize aSource when aLang is "test", case insensitive.
    function Highlight(const aLang, aSource : TMarkdownString; out aTokens : TMarkdownCodeTokenArray) : Boolean; override;
    // Number of calls to Highlight
    property CallCount : Integer read FCallCount;
    // Source of the last call to Highlight
    property LastSource : TMarkdownString read FLastSource;
  end;

  { TTokenRecordingPDFRenderer }

  // Records every code token that is laid out, as kind=text
  TTokenRecordingPDFRenderer = class(TMarkDownPDFRenderer)
  private
    FTokens : TStringList;
    FRunColors : TStringList;
  protected
    procedure FlushTextRun(const aText: TMarkdownString; const aFontSize: LongInt;
      const aFontStyle: TFontStyles; const aLinkHref: TMarkdownString; const aContext : TFontContext;
      const aDryRun: boolean); override;
  public
    constructor Create(aOwner : TComponent); override;
    destructor Destroy; override;
    procedure LayoutCodeToken(const aText : TMarkdownString; aKind : TMarkdownCodeTokenKind; const aContext : TFontContext); override;
    // Recorded tokens
    property Tokens : TStringList read FTokens;
    // RunColor of every text run that is laid out, as text=color in hex, or text=none for clNone
    property RunColors : TStringList read FRunColors;
    // Text colour of the items created by FlushTextRun
    property RunColor;
  end;

  { TTestCodeHighlightBase }

  TTestCodeHighlightBase = class(TTestCase)
  private
    FHighlighter : TTestCodeHighlighter;
  protected
    function ParseMarkdown(const aMarkdown : TMarkdownString) : TMarkdownDocument;
    procedure CheckContains(const aMsg, aExpected, aActual : TMarkdownString);
    procedure CheckNotContains(const aMsg, aUnexpected, aActual : TMarkdownString);
  public
    procedure SetUp; override;
    procedure TearDown; override;
    property Highlighter : TTestCodeHighlighter read FHighlighter;
  end;

  { TTestCodeHighlight }

  TTestCodeHighlight = class(TTestCodeHighlightBase)
  private
    function Token(const aText : TMarkdownString; aKind : TMarkdownCodeTokenKind) : TMarkdownCodeToken;
    function Highlight(const aMarkdown : TMarkdownString; out aTokens : TMarkdownCodeTokenArray; aRenderer : TMarkdownRenderer = Nil) : Boolean;
  published
    procedure TestSplitLines;
    procedure TestSplitNoTrailingBreak;
    procedure TestSplitEmptyLine;
    procedure TestSplitCRLF;
    procedure TestSplitCRLFAcrossTokens;
    procedure TestSplitMultiLineToken;
    procedure TestHighlightCodeBlock;
    procedure TestNoLanguage;
    procedure TestUnsupportedLanguage;
    procedure TestCodeHighlightOff;
    procedure TestNoHighlighter;
    procedure TestDefaultHighlighter;
    procedure TestRendererHighlighterBeforeDefault;
    procedure TestCodeTokenKindNames;
  end;

  { TTestHTMLCodeHighlight }

  TTestHTMLCodeHighlight = class(TTestCodeHighlightBase)
  private
    function Render(const aMarkdown : TMarkdownString; aHighlight : Boolean = True) : TMarkdownString;
  published
    procedure TestSpans;
    procedure TestDefaultTextNoSpan;
    procedure TestEscaping;
    procedure TestMultiLineComment;
    procedure TestLanguageClass;
    procedure TestUnsupportedUnchanged;
    procedure TestCodeHighlightOffUnchanged;
  end;

  { TTestANSICodeHighlight }

  TTestANSICodeHighlight = class(TTestCodeHighlightBase)
  private
    function Render(const aMarkdown : TMarkdownString; aColor : Boolean; aWidth : Integer = 80) : TMarkdownString;
  published
    procedure TestKeywordStyle;
    procedure TestCommentStyle;
    procedure TestNoColor;
    procedure TestNoWrapBetweenTokens;
    procedure TestEmptyLine;
    procedure TestCustomStyle;
  end;

  { TTestLaTeXCodeHighlight }

  TTestLaTeXCodeHighlight = class(TTestCodeHighlightBase)
  private
    function Render(const aMarkdown : TMarkdownString; aOptions : TLaTeXOptions = []) : TMarkdownString;
  published
    procedure TestVerbatimEnvironment;
    procedure TestTokenCommands;
    procedure TestEscaping;
    procedure TestMultiLineComment;
    procedure TestPreamble;
    procedure TestNoPreambleWithoutLanguage;
    procedure TestNoPreambleWithoutHighlighter;
    procedure TestUnsupportedPlainVerbatim;
    procedure TestCodeTokenCommand;
  end;

  { TTestPDFCodeHighlight }

  TTestPDFCodeHighlight = class(TTestCodeHighlightBase)
  private
    FRenderer : TTokenRecordingPDFRenderer;
    FPDF : TPDFDocument;
    procedure Render(const aMarkdown : TMarkdownString);
    function RecordedTokens : TMarkdownString;
    function AfterCodeY(const aCode : TMarkdownString; aHighlight : Boolean) : TPDFFloat;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestTokens;
    procedure TestMultiLineComment;
    procedure TestUnsupportedNoTokens;
    procedure TestEmptyLineAdvances;
    procedure TestEmptyLineAdvancesUnhighlighted;
    procedure TestDefaultColors;
    procedure TestTokenRunColors;
    procedure TestWhiteTokenColor;
  end;

implementation

uses
  Markdown.Processors;

{ TTestCodeHighlighter }

function TTestCodeHighlighter.Highlight(const aLang, aSource: TMarkdownString; out aTokens: TMarkdownCodeTokenArray): Boolean;

var
  lCount, I, lStart, lLen : Integer;
  lKind : TMarkdownCodeTokenKind;
  lWord : TMarkdownString;

begin
  aTokens:=[];
  Inc(FCallCount);
  FLastSource:=aSource;
  Result:=SameText(aLang,'test');
  if not Result then
    Exit;
  lCount:=0;
  lLen:=Length(aSource);
  I:=1;
  while I<=lLen do
    begin
    lStart:=I;
    case aSource[I] of
      '{' :
        begin
        while (I<=lLen) and (aSource[I]<>'}') do
          Inc(I);
        Inc(I);
        lKind:=ctkComment;
        end;
      '''' :
        begin
        Inc(I);
        while (I<=lLen) and (aSource[I]<>'''') do
          Inc(I);
        Inc(I);
        lKind:=ctkString;
        end;
      '0'..'9' :
        begin
        while (I<=lLen) and (aSource[I] in ['0'..'9']) do
          Inc(I);
        lKind:=ctkNumber;
        end;
      'a'..'z','A'..'Z' :
        begin
        while (I<=lLen) and (aSource[I] in ['a'..'z','A'..'Z']) do
          Inc(I);
        lWord:=Copy(aSource,lStart,I-lStart);
        if (lWord='begin') or (lWord='end') then
          lKind:=ctkKeyword
        else
          lKind:=ctkDefault;
        end;
    else
      Inc(I);
      lKind:=ctkDefault;
    end;
    if I>lLen+1 then
      I:=lLen+1;
    SetLength(aTokens,lCount+1);
    aTokens[lCount].Text:=Copy(aSource,lStart,I-lStart);
    aTokens[lCount].Kind:=lKind;
    Inc(lCount);
    end;
end;


{ TTokenRecordingPDFRenderer }

constructor TTokenRecordingPDFRenderer.Create(aOwner: TComponent);

begin
  inherited Create(aOwner);
  FTokens:=TStringList.Create;
  FRunColors:=TStringList.Create;
end;


destructor TTokenRecordingPDFRenderer.Destroy;

begin
  FreeAndNil(FRunColors);
  FreeAndNil(FTokens);
  inherited Destroy;
end;


procedure TTokenRecordingPDFRenderer.FlushTextRun(const aText: TMarkdownString; const aFontSize: LongInt;
  const aFontStyle: TFontStyles; const aLinkHref: TMarkdownString; const aContext: TFontContext;
  const aDryRun: boolean);

begin
  if RunColor=clNone then
    FRunColors.Add(MarkdownToString(aText)+'=none')
  else
    FRunColors.Add(MarkdownToString(aText)+'='+HexStr(RunColor,6));
  inherited FlushTextRun(aText,aFontSize,aFontStyle,aLinkHref,aContext,aDryRun);
end;


procedure TTokenRecordingPDFRenderer.LayoutCodeToken(const aText: TMarkdownString; aKind: TMarkdownCodeTokenKind; const aContext: TFontContext);

begin
  FTokens.Add(MarkdownToString(CodeTokenKindNames[aKind]+'='+aText));
  inherited LayoutCodeToken(aText,aKind,aContext);
end;


{ TTestCodeHighlightBase }

procedure TTestCodeHighlightBase.SetUp;

begin
  FHighlighter:=TTestCodeHighlighter.Create;
end;


procedure TTestCodeHighlightBase.TearDown;

begin
  TMarkdownCodeHighlighter.SetDefaultHighlighter(Nil);
  FreeAndNil(FHighlighter);
end;


function TTestCodeHighlightBase.ParseMarkdown(const aMarkdown: TMarkdownString): TMarkdownDocument;

var
  lSource : TStringList;

begin
  lSource:=TStringList.Create;
  try
    lSource.Text:=MarkdownToString(aMarkdown);
    Result:=TMarkdownParser.FastParse(lSource,MarkdownDocExtensions);
  finally
    lSource.Free;
  end;
end;


procedure TTestCodeHighlightBase.CheckContains(const aMsg, aExpected, aActual: TMarkdownString);

begin
  AssertTrue(aMsg+': expected "'+aExpected+'" in "'+aActual+'"',Pos(aExpected,aActual)>0);
end;


procedure TTestCodeHighlightBase.CheckNotContains(const aMsg, aUnexpected, aActual: TMarkdownString);

begin
  AssertTrue(aMsg+': did not expect "'+aUnexpected+'" in "'+aActual+'"',Pos(aUnexpected,aActual)=0);
end;


{ TTestCodeHighlight }

function TTestCodeHighlight.Token(const aText: TMarkdownString; aKind: TMarkdownCodeTokenKind): TMarkdownCodeToken;

begin
  Result.Text:=aText;
  Result.Kind:=aKind;
end;


function TTestCodeHighlight.Highlight(const aMarkdown: TMarkdownString; out aTokens: TMarkdownCodeTokenArray; aRenderer: TMarkdownRenderer): Boolean;

var
  lDoc : TMarkdownDocument;
  lRenderer : TMarkdownRenderer;

begin
  aTokens:=[];
  lDoc:=ParseMarkdown(aMarkdown);
  try
    AssertTrue('Document has a code block',(lDoc.ChildCount>0) and (lDoc.Children[0] is TMarkdownCodeBlock));
    lRenderer:=aRenderer;
    if lRenderer=Nil then
      begin
      lRenderer:=TMarkdownHTMLRenderer.Create(Nil);
      lRenderer.CodeHighlighter:=Highlighter;
      end;
    try
      Result:=lRenderer.HighlightCodeBlock(TMarkdownCodeBlock(lDoc.Children[0]),aTokens);
    finally
      if lRenderer<>aRenderer then
        lRenderer.Free;
    end;
  finally
    lDoc.Free;
  end;
end;


procedure TTestCodeHighlight.TestSplitLines;

var
  lLines : TMarkdownCodeTokenLines;

begin
  lLines:=SplitCodeTokenLines([Token('a',ctkKeyword),Token(#10,ctkDefault),Token('b',ctkString),Token(#10,ctkDefault)]);
  AssertEquals('Line count',2,Length(lLines));
  AssertEquals('Tokens on line 1',1,Length(lLines[0]));
  AssertEquals('Text on line 1','a',lLines[0][0].Text);
  AssertTrue('Kind on line 1',lLines[0][0].Kind=ctkKeyword);
  AssertEquals('Text on line 2','b',lLines[1][0].Text);
  AssertTrue('Kind on line 2',lLines[1][0].Kind=ctkString);
end;


procedure TTestCodeHighlight.TestSplitNoTrailingBreak;

var
  lLines : TMarkdownCodeTokenLines;
  lNoTokens : TMarkdownCodeTokenArray;

begin
  lLines:=SplitCodeTokenLines([Token('a'#10'b',ctkDefault)]);
  AssertEquals('Line count',2,Length(lLines));
  AssertEquals('Text on last line','b',lLines[1][0].Text);
  lNoTokens:=Nil;
  AssertEquals('No lines for no tokens',0,Length(SplitCodeTokenLines(lNoTokens)));
end;


procedure TTestCodeHighlight.TestSplitEmptyLine;

var
  lLines : TMarkdownCodeTokenLines;

begin
  lLines:=SplitCodeTokenLines([Token('a'#10#10'b'#10,ctkDefault)]);
  AssertEquals('Line count',3,Length(lLines));
  AssertEquals('Empty line has no tokens',0,Length(lLines[1]));
  AssertEquals('Text on line 3','b',lLines[2][0].Text);
end;


procedure TTestCodeHighlight.TestSplitCRLF;

var
  lLines : TMarkdownCodeTokenLines;

begin
  lLines:=SplitCodeTokenLines([Token('a'#13#10'b'#13'c'#13#10,ctkDefault)]);
  AssertEquals('Line count',3,Length(lLines));
  AssertEquals('Text on line 1','a',lLines[0][0].Text);
  AssertEquals('Text on line 2','b',lLines[1][0].Text);
  AssertEquals('Text on line 3','c',lLines[2][0].Text);
end;


procedure TTestCodeHighlight.TestSplitCRLFAcrossTokens;

var
  lLines : TMarkdownCodeTokenLines;

begin
  lLines:=SplitCodeTokenLines([Token('a'#13,ctkKeyword),Token(#10'b'#13#10,ctkString)]);
  AssertEquals('CR LF in two tokens is one line break',2,Length(lLines));
  AssertEquals('Text on line 2','b',lLines[1][0].Text);
end;


procedure TTestCodeHighlight.TestSplitMultiLineToken;

var
  lLines : TMarkdownCodeTokenLines;

begin
  lLines:=SplitCodeTokenLines([Token('x ',ctkDefault),Token('{a'#10'b}',ctkComment),Token(' y',ctkDefault)]);
  AssertEquals('Line count',2,Length(lLines));
  AssertEquals('Tokens on line 1',2,Length(lLines[0]));
  AssertEquals('Comment start','{a',lLines[0][1].Text);
  AssertTrue('Comment start kind',lLines[0][1].Kind=ctkComment);
  AssertEquals('Tokens on line 2',2,Length(lLines[1]));
  AssertEquals('Comment end','b}',lLines[1][0].Text);
  AssertTrue('Comment end kind',lLines[1][0].Kind=ctkComment);
  AssertEquals('Text after comment',' y',lLines[1][1].Text);
end;


procedure TTestCodeHighlight.TestHighlightCodeBlock;

var
  lTokens : TMarkdownCodeTokenArray;

begin
  AssertTrue('Highlighted',Highlight('```test'#10'begin 1'#10'end'#10'```',lTokens));
  AssertEquals('Source passed to highlighter','begin 1'+sLineBreak+'end'+sLineBreak,Highlighter.LastSource);
  AssertEquals('First token','begin',lTokens[0].Text);
  AssertTrue('First token kind',lTokens[0].Kind=ctkKeyword);
  AssertEquals('Number token','1',lTokens[2].Text);
  AssertTrue('Number token kind',lTokens[2].Kind=ctkNumber);
end;


procedure TTestCodeHighlight.TestNoLanguage;

var
  lTokens : TMarkdownCodeTokenArray;

begin
  AssertFalse('Fenced block without language',Highlight('```'#10'begin'#10'```',lTokens));
  AssertFalse('Indented block',Highlight('    begin',lTokens));
  AssertEquals('Highlighter not called',0,Highlighter.CallCount);
end;


procedure TTestCodeHighlight.TestUnsupportedLanguage;

var
  lTokens : TMarkdownCodeTokenArray;

begin
  AssertFalse('Unsupported language',Highlight('```other'#10'begin'#10'```',lTokens));
  AssertEquals('No tokens',0,Length(lTokens));
end;


procedure TTestCodeHighlight.TestCodeHighlightOff;

var
  lTokens : TMarkdownCodeTokenArray;
  lRenderer : TMarkdownHTMLRenderer;

begin
  lRenderer:=TMarkdownHTMLRenderer.Create(Nil);
  try
    lRenderer.CodeHighlighter:=Highlighter;
    lRenderer.CodeHighlight:=False;
    AssertFalse('CodeHighlight off',Highlight('```test'#10'begin'#10'```',lTokens,lRenderer));
    AssertEquals('Highlighter not called',0,Highlighter.CallCount);
  finally
    lRenderer.Free;
  end;
end;


procedure TTestCodeHighlight.TestNoHighlighter;

var
  lTokens : TMarkdownCodeTokenArray;
  lRenderer : TMarkdownHTMLRenderer;

begin
  lRenderer:=TMarkdownHTMLRenderer.Create(Nil);
  try
    AssertTrue('CodeHighlight on by default',lRenderer.CodeHighlight);
    AssertNull('No highlighter',lRenderer.CodeHighlighter);
    AssertFalse('No highlighter, no highlighting',Highlight('```test'#10'begin'#10'```',lTokens,lRenderer));
  finally
    lRenderer.Free;
  end;
end;


procedure TTestCodeHighlight.TestDefaultHighlighter;

var
  lTokens : TMarkdownCodeTokenArray;
  lRenderer : TMarkdownHTMLRenderer;
  lDefault : TTestCodeHighlighter;

begin
  lDefault:=TTestCodeHighlighter.Create;
  TMarkdownCodeHighlighter.SetDefaultHighlighter(lDefault);
  AssertSame('Default highlighter installed',lDefault,TMarkdownCodeHighlighter.DefaultHighlighter);
  lRenderer:=TMarkdownHTMLRenderer.Create(Nil);
  try
    AssertSame('Renderer uses default highlighter',lDefault,lRenderer.CodeHighlighter);
    AssertTrue('Highlighted with default highlighter',Highlight('```test'#10'begin'#10'```',lTokens,lRenderer));
    AssertEquals('Default highlighter called',1,lDefault.CallCount);
  finally
    lRenderer.Free;
  end;
  TMarkdownCodeHighlighter.SetDefaultHighlighter(Nil);
  AssertNull('Default highlighter removed',TMarkdownCodeHighlighter.DefaultHighlighter);
end;


procedure TTestCodeHighlight.TestRendererHighlighterBeforeDefault;

var
  lTokens : TMarkdownCodeTokenArray;
  lRenderer : TMarkdownHTMLRenderer;
  lDefault : TTestCodeHighlighter;

begin
  lDefault:=TTestCodeHighlighter.Create;
  TMarkdownCodeHighlighter.SetDefaultHighlighter(lDefault);
  lRenderer:=TMarkdownHTMLRenderer.Create(Nil);
  try
    lRenderer.CodeHighlighter:=Highlighter;
    AssertTrue('Highlighted',Highlight('```test'#10'begin'#10'```',lTokens,lRenderer));
    AssertEquals('Renderer highlighter called',1,Highlighter.CallCount);
    AssertEquals('Default highlighter not called',0,lDefault.CallCount);
  finally
    lRenderer.Free;
  end;
end;


procedure TTestCodeHighlight.TestCodeTokenKindNames;

begin
  AssertEquals('Keyword name','keyword',CodeTokenKindNames[ctkKeyword]);
  AssertEquals('Comment name','comment',CodeTokenKindNames[ctkComment]);
  AssertEquals('Error name','error',CodeTokenKindNames[ctkError]);
end;


{ TTestHTMLCodeHighlight }

function TTestHTMLCodeHighlight.Render(const aMarkdown: TMarkdownString; aHighlight: Boolean): TMarkdownString;

var
  lDoc : TMarkdownDocument;
  lRenderer : TMarkdownHTMLRenderer;
  lHTML : TStringList;

begin
  lDoc:=ParseMarkdown(aMarkdown);
  lRenderer:=TMarkdownHTMLRenderer.Create(Nil);
  lHTML:=TStringList.Create;
  try
    lRenderer.CodeHighlighter:=Highlighter;
    lRenderer.CodeHighlight:=aHighlight;
    lRenderer.RenderDocument(lDoc,lHTML);
    Result:=StringToMarkdown(lHTML.Text);
  finally
    lHTML.Free;
    lRenderer.Free;
    lDoc.Free;
  end;
end;


procedure TTestHTMLCodeHighlight.TestSpans;

var
  S : TMarkdownString;

begin
  S:=Render('```test'#10'begin 12'#10'```');
  CheckContains('Keyword span','<span class="keyword">begin</span>',S);
  CheckContains('Number span','<span class="number">12</span>',S);
end;


procedure TTestHTMLCodeHighlight.TestDefaultTextNoSpan;

begin
  CheckContains('Default text without span','<code class="language-test">x <span class="keyword">end</span>',
                Render('```test'#10'x end'#10'```'));
end;


procedure TTestHTMLCodeHighlight.TestEscaping;

var
  S : TMarkdownString;

begin
  S:=Render('```test'#10'a < b & ''<c>'''#10'```');
  CheckContains('Default text escaped','a &lt; b &amp; ',S);
  CheckContains('String escaped','<span class="string">''&lt;c&gt;''</span>',S);
end;


procedure TTestHTMLCodeHighlight.TestMultiLineComment;

begin
  CheckContains('Comment over two lines in one span','<span class="comment">{a'+sLineBreak+'b}</span>',
                Render('```test'#10'{a'#10'b}'#10'```'));
end;


procedure TTestHTMLCodeHighlight.TestLanguageClass;

begin
  CheckContains('Language class kept','<pre><code class="language-test">',Render('```test'#10'begin'#10'```'));
end;


procedure TTestHTMLCodeHighlight.TestUnsupportedUnchanged;

begin
  AssertEquals('Unsupported language renders as without highlighting',
               Render('```other'#10'a < begin'#10'```',False),Render('```other'#10'a < begin'#10'```'));
  CheckNotContains('No spans','<span',Render('```other'#10'a < begin'#10'```'));
end;


procedure TTestHTMLCodeHighlight.TestCodeHighlightOffUnchanged;

var
  S : TMarkdownString;

begin
  S:=Render('```test'#10'a < begin'#10'```',False);
  CheckNotContains('No spans','<span',S);
  CheckContains('Escaped text','a &lt; begin',S);
  AssertEquals('Highlighter not called',0,Highlighter.CallCount);
end;


{ TTestANSICodeHighlight }

function TTestANSICodeHighlight.Render(const aMarkdown: TMarkdownString; aColor: Boolean; aWidth: Integer): TMarkdownString;

var
  lDoc : TMarkdownDocument;
  lRenderer : TMarkDownANSIRenderer;

begin
  lDoc:=ParseMarkdown(aMarkdown);
  lRenderer:=TMarkDownANSIRenderer.Create(Nil);
  try
    lRenderer.CodeHighlighter:=Highlighter;
    lRenderer.UseColor:=aColor;
    lRenderer.Width:=aWidth;
    Result:=lRenderer.RenderToString(lDoc);
  finally
    lRenderer.Free;
    lDoc.Free;
  end;
end;


procedure TTestANSICodeHighlight.TestKeywordStyle;

begin
  CheckContains('Bold bright blue keyword',#27'[1;38;5;12mbegin'#27'[0m',Render('```test'#10'begin'#10'```',True));
end;


procedure TTestANSICodeHighlight.TestCommentStyle;

var
  S : TMarkdownString;

begin
  S:=Render('```test'#10'{a'#10'b}'#10'```',True);
  CheckContains('Italic grey comment start',#27'[3;38;5;8m{a'#27'[0m',S);
  CheckContains('Italic grey comment end',#27'[3;38;5;8mb}'#27'[0m',S);
end;


procedure TTestANSICodeHighlight.TestNoColor;

var
  S : TMarkdownString;

begin
  S:=Render('```test'#10'begin 1'#10'end'#10'```',False);
  CheckNotContains('No escape sequences',#27,S);
  CheckContains('First line indented','    begin 1',S);
  CheckContains('Second line indented','    end',S);
end;


procedure TTestANSICodeHighlight.TestNoWrapBetweenTokens;

begin
  CheckContains('Long line kept on one line','    begin 1 2 3 4 5 6 7 8 9 end',
                Render('```test'#10'begin 1 2 3 4 5 6 7 8 9 end'#10'```',False,20));
end;


procedure TTestANSICodeHighlight.TestEmptyLine;

begin
  CheckContains('Empty line kept','    begin'+sLineBreak+'    '+sLineBreak+'    end',
                Render('```test'#10'begin'#10#10'end'#10'```',False));
end;


procedure TTestANSICodeHighlight.TestCustomStyle;

var
  lDoc : TMarkdownDocument;
  lRenderer : TMarkDownANSIRenderer;
  lStyle : TRunStyle;

begin
  lDoc:=ParseMarkdown('```test'#10'begin'#10'```');
  lRenderer:=TMarkDownANSIRenderer.Create(Nil);
  try
    lRenderer.CodeHighlighter:=Highlighter;
    lStyle:=lRenderer.DefaultStyle;
    lStyle.Underline:=True;
    lRenderer.CodeTokenStyles[ctkKeyword]:=lStyle;
    CheckContains('Custom keyword style',#27'[4mbegin'#27'[0m',lRenderer.RenderToString(lDoc));
  finally
    lRenderer.Free;
    lDoc.Free;
  end;
end;


{ TTestLaTeXCodeHighlight }

function TTestLaTeXCodeHighlight.Render(const aMarkdown: TMarkdownString; aOptions: TLaTeXOptions): TMarkdownString;

var
  lDoc : TMarkdownDocument;
  lRenderer : TMarkdownLaTeXRenderer;

begin
  lDoc:=ParseMarkdown(aMarkdown);
  lRenderer:=TMarkdownLaTeXRenderer.Create(Nil);
  try
    lRenderer.CodeHighlighter:=Highlighter;
    lRenderer.Options:=aOptions;
    Result:=lRenderer.RenderLaTeX(lDoc);
  finally
    lRenderer.Free;
    lDoc.Free;
  end;
end;


procedure TTestLaTeXCodeHighlight.TestVerbatimEnvironment;

var
  S : TMarkdownString;

begin
  S:=Render('```test'#10'begin'#10'```');
  CheckContains('Verbatim with command characters','\begin{Verbatim}[commandchars=\\\{\}]',S);
  CheckContains('Verbatim closed','\end{Verbatim}',S);
  CheckNotContains('No plain verbatim','\begin{verbatim}',S);
end;


procedure TTestLaTeXCodeHighlight.TestTokenCommands;

begin
  CheckContains('Token commands','\MdTkKeyword{begin} \MdTkNumber{12} x',Render('```test'#10'begin 12 x'#10'```'));
end;


procedure TTestLaTeXCodeHighlight.TestEscaping;

var
  S : TMarkdownString;

begin
  S:=Render('```test'#10'a\b ''x{y}'' % _ & $'#10'```');
  CheckContains('Backslash in default text','a\MdBs{}b',S);
  CheckContains('Braces in string','\MdTkString{''x\MdLb{}y\MdRb{}''}',S);
  CheckContains('Other special characters unchanged',' % _ & $',S);
end;


procedure TTestLaTeXCodeHighlight.TestMultiLineComment;

begin
  CheckContains('Comment command per line','\MdTkComment{\MdLb{}a}'+sLineBreak+'\MdTkComment{b\MdRb{}}',
                Render('```test'#10'{a'#10'b}'#10'```'));
end;


procedure TTestLaTeXCodeHighlight.TestPreamble;

var
  S : TMarkdownString;

begin
  S:=Render('```test'#10'begin'#10'```',[loEnvelope]);
  CheckContains('fancyvrb package','\usepackage{fancyvrb}',S);
  CheckContains('xcolor package','\usepackage{xcolor}',S);
  CheckContains('Backslash command','\newcommand{\MdBs}',S);
  CheckContains('Keyword command','\newcommand{\MdTkKeyword}[1]',S);
  CheckNotContains('No command for default text','\MdTkDefault',S);
  AssertTrue('Preamble before document',Pos('\usepackage{fancyvrb}',S)<Pos('\begin{document}',S));
end;


procedure TTestLaTeXCodeHighlight.TestNoPreambleWithoutLanguage;

begin
  CheckNotContains('No fancyvrb without language','fancyvrb',Render('```'#10'begin'#10'```',[loEnvelope]));
end;


procedure TTestLaTeXCodeHighlight.TestNoPreambleWithoutHighlighter;

var
  lDoc : TMarkdownDocument;
  lRenderer : TMarkdownLaTeXRenderer;

begin
  lDoc:=ParseMarkdown('```test'#10'begin'#10'```');
  lRenderer:=TMarkdownLaTeXRenderer.Create(Nil);
  try
    lRenderer.Options:=[loEnvelope];
    CheckNotContains('No fancyvrb without highlighter','fancyvrb',lRenderer.RenderLaTeX(lDoc));
  finally
    lRenderer.Free;
    lDoc.Free;
  end;
end;


procedure TTestLaTeXCodeHighlight.TestUnsupportedPlainVerbatim;

var
  S : TMarkdownString;

begin
  S:=Render('```other'#10'a < b\c'#10'```');
  CheckContains('Plain verbatim','\begin{verbatim}'+sLineBreak+'a < b\c'+sLineBreak+'\end{verbatim}',S);
end;


procedure TTestLaTeXCodeHighlight.TestCodeTokenCommand;

var
  lRenderer : TMarkdownLaTeXRenderer;

begin
  lRenderer:=TMarkdownLaTeXRenderer.Create(Nil);
  try
    AssertEquals('Keyword command','\MdTkKeyword',lRenderer.CodeTokenCommand(ctkKeyword));
    AssertEquals('Attribute command','\MdTkAttribute',lRenderer.CodeTokenCommand(ctkAttribute));
  finally
    lRenderer.Free;
  end;
end;


{ TTestPDFCodeHighlight }

procedure TTestPDFCodeHighlight.SetUp;

begin
  inherited SetUp;
  FRenderer:=TTokenRecordingPDFRenderer.Create(Nil);
  FRenderer.CodeHighlighter:=Highlighter;
  FPDF:=TPDFDocument.Create(Nil);
  FPDF.Options:=[poPageOriginAtTop];
  FPDF.DefaultUnitOfMeasure:=uomPixels;
end;


procedure TTestPDFCodeHighlight.TearDown;

begin
  FreeAndNil(FPDF);
  FreeAndNil(FRenderer);
  inherited TearDown;
end;


procedure TTestPDFCodeHighlight.Render(const aMarkdown: TMarkdownString);

var
  lDoc : TMarkdownDocument;

begin
  lDoc:=ParseMarkdown(aMarkdown);
  try
    FRenderer.RenderDocument(lDoc,FPDF);
  finally
    lDoc.Free;
  end;
end;


function TTestPDFCodeHighlight.RecordedTokens: TMarkdownString;

begin
  Result:=StringToMarkdown(String.Join('|',FRenderer.Tokens.ToStringArray));
end;


function TTestPDFCodeHighlight.AfterCodeY(const aCode: TMarkdownString; aHighlight: Boolean): TPDFFloat;

var
  lDoc : TMarkdownDocument;
  lRenderer : TMarkDownPDFRenderer;
  lPDF : TPDFDocument;

begin
  lDoc:=ParseMarkdown('```test'#10+aCode+'```'#10#10'# After');
  lPDF:=TPDFDocument.Create(Nil);
  lRenderer:=TMarkDownPDFRenderer.Create(Nil);
  try
    lPDF.Options:=[poPageOriginAtTop];
    lPDF.DefaultUnitOfMeasure:=uomPixels;
    lRenderer.CodeHighlighter:=Highlighter;
    lRenderer.CodeHighlight:=aHighlight;
    lRenderer.RenderDocument(lDoc,lPDF);
    Result:=TPDFDestination(lRenderer.Destinations.Objects[lRenderer.Destinations.IndexOf('after')]).Y;
  finally
    lRenderer.Free;
    lPDF.Free;
    lDoc.Free;
  end;
end;


procedure TTestPDFCodeHighlight.TestTokens;

begin
  Render('```test'#10'begin 1'#10'```');
  AssertEquals('Recorded tokens','keyword=begin|default= |number=1',RecordedTokens);
end;


procedure TTestPDFCodeHighlight.TestMultiLineComment;

begin
  Render('```test'#10'{a'#10'b}'#10'```');
  AssertEquals('Comment split per line','comment={a|comment=b}',RecordedTokens);
end;


procedure TTestPDFCodeHighlight.TestUnsupportedNoTokens;

begin
  Render('```other'#10'begin'#10'```');
  AssertEquals('No code tokens laid out',0,FRenderer.Tokens.Count);
end;


procedure TTestPDFCodeHighlight.TestEmptyLineAdvances;

begin
  AssertTrue('Empty highlighted code line takes vertical space',
             AfterCodeY('begin'#10#10'end'#10,True)>AfterCodeY('begin'#10'end'#10,True));
end;


procedure TTestPDFCodeHighlight.TestEmptyLineAdvancesUnhighlighted;

begin
  AssertTrue('Empty code line takes vertical space',
             AfterCodeY('begin'#10#10'end'#10,False)>AfterCodeY('begin'#10'end'#10,False));
end;


procedure TTestPDFCodeHighlight.TestDefaultColors;

begin
  AssertTrue('Keyword has its own colour',FRenderer.CodeTokenColors[ctkKeyword]<>FRenderer.CodeTokenColors[ctkDefault]);
  AssertTrue('Comment has its own colour',FRenderer.CodeTokenColors[ctkComment]<>FRenderer.CodeTokenColors[ctkKeyword]);
  FRenderer.CodeTokenColors[ctkKeyword]:=$123456;
  AssertTrue('Colour can be set',FRenderer.CodeTokenColors[ctkKeyword]=$123456);
end;


procedure TTestPDFCodeHighlight.TestTokenRunColors;

begin
  FRenderer.CodeTokenColors[ctkKeyword]:=$123456;
  FRenderer.CodeTokenColors[ctkNumber]:=$654321;
  Render('```test'#10'begin 1'#10'```');
  AssertEquals('Run colours','begin=123456| =none|1=654321',
               StringToMarkdown(String.Join('|',FRenderer.RunColors.ToStringArray)));
  AssertTrue('RunColor reset after the code block',FRenderer.RunColor=clNone);
end;


procedure TTestPDFCodeHighlight.TestWhiteTokenColor;

begin
  FRenderer.CodeTokenColors[ctkKeyword]:=$FFFFFF;
  Render('```test'#10'begin'#10'```');
  AssertEquals('White token colour is kept','begin=FFFFFF',
               StringToMarkdown(String.Join('|',FRenderer.RunColors.ToStringArray)));
end;


initialization
  RegisterTests([TTestCodeHighlight,TTestHTMLCodeHighlight,TTestANSICodeHighlight,
                 TTestLaTeXCodeHighlight,TTestPDFCodeHighlight]);
end.
