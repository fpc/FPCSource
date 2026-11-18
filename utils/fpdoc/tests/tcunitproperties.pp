{ Tests for the documentation of unit-level properties and for see-also links in man pages.
  The environment variable FPDOC names the fpdoc binary to test; without it the tests are skipped. }
unit tcUnitProperties;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Process, fpcunit, testregistry, chmreader;

type

  { TTestUnitProperties }

  TTestUnitProperties = class(TTestCase)
  private
    FFPDoc: string;
    FDir: string;
    procedure WriteLines(const aFilename: string; const aLines: array of string);
    procedure DeleteTree(const aDir: string);
    procedure RunFPDoc(const aFormat, aOutput: string; const aExtra: array of string);
    function ReadText(const aFilename: string): string;
    procedure AssertContains(const aMsg, aText, aContent: string);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestHTMLPropertyPage;
    procedure TestHTMLPropertiesSubpage;
    procedure TestHTMLMenuBarLink;
    procedure TestHTMLUnitIndex;
    procedure TestHTMLPackageIndex;
    procedure TestHTMLClassPropertyPage;
    procedure TestTextOutput;
    procedure TestCHMIndex;
    procedure TestManPage;
    procedure TestManSeeAlso;
  end;

implementation

{ TTestUnitProperties }

procedure TTestUnitProperties.WriteLines(const aFilename: string; const aLines: array of string);

var
  lList: TStringList;
  lLine: string;

begin
  lList:=TStringList.Create;
  try
    for lLine in aLines do
      lList.Add(lLine);
    lList.SaveToFile(aFilename);
  finally
    lList.Free;
  end;
end;


procedure TTestUnitProperties.DeleteTree(const aDir: string);

var
  lInfo: TSearchRec;

begin
  if FindFirst(aDir+AllFilesMask,faAnyFile,lInfo)=0 then
    try
      repeat
        if (lInfo.Name='.') or (lInfo.Name='..') then
          continue;
        if (lInfo.Attr and faDirectory)<>0 then
          DeleteTree(aDir+lInfo.Name+PathDelim)
        else
          DeleteFile(aDir+lInfo.Name);
      until FindNext(lInfo)<>0;
    finally
      FindClose(lInfo);
    end;
  RemoveDir(aDir);
end;


procedure TTestUnitProperties.SetUp;

begin
  FFPDoc:=GetEnvironmentVariable('FPDOC');
  if FFPDoc='' then
    Ignore('Set FPDOC to the fpdoc binary to run this test');
  inherited SetUp;
  FDir:=IncludeTrailingPathDelimiter(GetTempDir(false))+'tfpdocprops'+IntToStr(GetProcessID)+PathDelim;
  if DirectoryExists(FDir) then
    DeleteTree(FDir);
  AssertTrue('Create test directory '+FDir,ForceDirectories(FDir));
  WriteLines(FDir+'uprop.pp',[
    'unit uprop;',
    '{$mode objfpc}{$H+}',
    'interface',
    'type',
    '  TFoo = class',
    '  private',
    '    FBar: Integer;',
    '  published',
    '    property Bar: Integer read FBar write FBar;',
    '  end;',
    'function GetDebugLogger: TObject;',
    'procedure SetDebugLogger(aValue: TObject);',
    'property DebugLogger: TObject read GetDebugLogger write SetDebugLogger;',
    'var',
    '  SomeVar: Integer;',
    'implementation',
    'function GetDebugLogger: TObject;',
    'begin',
    '  Result:=nil;',
    'end;',
    'procedure SetDebugLogger(aValue: TObject);',
    'begin',
    'end;',
    'end.']);
  WriteLines(FDir+'uprop.xml',[
    '<?xml version="1.0" encoding="UTF-8"?>',
    '<fpdoc-descriptions><package name="t"><module name="uprop">',
    '<element name="DebugLogger"><short>Logger used for debug output</short>',
    '<descr>Describes the debug logger property.</descr></element>',
    '<element name="GetDebugLogger"><short>Returns the logger</short>',
    '<seealso><link id="DebugLogger"/><link id="SetDebugLogger"/><link id="SomeVar"/>',
    '<link id="TFoo"/><link id="TFoo.Bar"/><link id="NoSuchThing"/></seealso></element>',
    '</module></package></fpdoc-descriptions>']);
end;


procedure TTestUnitProperties.TearDown;

begin
  if (FDir<>'') and DirectoryExists(FDir) then
    DeleteTree(FDir);
  inherited TearDown;
end;


procedure TTestUnitProperties.RunFPDoc(const aFormat, aOutput: string; const aExtra: array of string);

var
  lProc: TProcess;
  lParam: string;

begin
  lProc:=TProcess.Create(nil);
  try
    lProc.Executable:=FFPDoc;
    lProc.Parameters.Add('--package=t');
    lProc.Parameters.Add('--input='+FDir+'uprop.pp');
    lProc.Parameters.Add('--descr='+FDir+'uprop.xml');
    lProc.Parameters.Add('--format='+aFormat);
    lProc.Parameters.Add('--output='+aOutput);
    for lParam in aExtra do
      lProc.Parameters.Add(lParam);
    lProc.CurrentDirectory:=FDir;
    lProc.Options:=[poWaitOnExit,poNoConsole];
    lProc.Execute;
    AssertEquals('fpdoc --format='+aFormat+' exit code',0,lProc.ExitCode);
  finally
    lProc.Free;
  end;
end;


function TTestUnitProperties.ReadText(const aFilename: string): string;

var
  lList: TStringList;

begin
  AssertTrue('File exists: '+aFilename,FileExists(aFilename));
  lList:=TStringList.Create;
  try
    lList.LoadFromFile(aFilename);
    Result:=lList.Text;
  finally
    lList.Free;
  end;
end;


procedure TTestUnitProperties.AssertContains(const aMsg, aText, aContent: string);

begin
  AssertTrue(aMsg+': "'+aText+'" expected',Pos(aText,aContent)>0);
end;


procedure TTestUnitProperties.TestHTMLPropertyPage;

var
  lPage: string;

begin
  RunFPDoc('html','html',[]);
  lPage:=ReadText(FDir+'html'+PathDelim+'uprop'+PathDelim+'debuglogger.html');
  AssertContains('Declaration without a parent prefix','property </span>DebugLogger',lPage);
  AssertContains('Read accessor','GetDebugLogger',lPage);
  AssertContains('Write accessor','SetDebugLogger',lPage);
  AssertContains('Short description','Logger used for debug output',lPage);
  AssertContains('Description','Describes the debug logger property.',lPage);
  AssertTrue('No parent link before the name',Pos('</a><span class="sym">.</span>DebugLogger',lPage)=0);
end;


procedure TTestUnitProperties.TestHTMLPropertiesSubpage;

begin
  RunFPDoc('html','html',[]);
  AssertContains('Properties page links the property','debuglogger.html',
    ReadText(FDir+'html'+PathDelim+'uprop'+PathDelim+'index-18.html'));
end;


procedure TTestUnitProperties.TestHTMLMenuBarLink;

begin
  RunFPDoc('html','html',[]);
  AssertContains('Unit overview menu links the properties page','index-18.html',
    ReadText(FDir+'html'+PathDelim+'uprop'+PathDelim+'index.html'));
end;


procedure TTestUnitProperties.TestHTMLUnitIndex;

begin
  RunFPDoc('html','html',[]);
  AssertContains('Unit identifier index links the property','href="../uprop/debuglogger.html"',
    ReadText(FDir+'html'+PathDelim+'uprop'+PathDelim+'index-8.html'));
end;


procedure TTestUnitProperties.TestHTMLPackageIndex;

begin
  RunFPDoc('html','html',[]);
  AssertContains('Package identifier index links the property','href="uprop/debuglogger.html"',
    ReadText(FDir+'html'+PathDelim+'index-8.html'));
end;


procedure TTestUnitProperties.TestHTMLClassPropertyPage;

var
  lPage: string;

begin
  RunFPDoc('html','html',[]);
  lPage:=ReadText(FDir+'html'+PathDelim+'uprop'+PathDelim+'tfoo.bar.html');
  AssertContains('Class property keeps the class prefix',
    '<a href="../uprop/tfoo.html">TFoo</a><span class="sym">.</span>Bar',lPage);
end;


procedure TTestUnitProperties.TestTextOutput;

var
  lText: string;

begin
  RunFPDoc('txt','uprop.txt',[]);
  lText:=ReadText(FDir+'uprop.txt');
  AssertContains('Declaration in text output','property DebugLogger',lText);
  AssertContains('Description in text output','Describes the debug logger property.',lText);
end;


procedure TTestUnitProperties.TestCHMIndex;

var
  lReader: TChmReader;
  lStream: TFileStream;
  lIndex: TMemoryStream;
  lText: string;

begin
  RunFPDoc('chm','uprop.chm',['--auto-index','--auto-toc']);
  lStream:=TFileStream.Create(FDir+'uprop.chm',fmOpenRead or fmShareDenyWrite);
  lReader:=TChmReader.Create(lStream,True);
  try
    AssertTrue('Page for the property in the CHM file',lReader.ObjectExists('/uprop/debuglogger.html')>0);
    lIndex:=lReader.GetObject('/Default.hhk');
    AssertNotNull('CHM index present',lIndex);
    try
      SetLength(lText,lIndex.Size);
      if lIndex.Size>0 then
        Move(lIndex.Memory^,lText[1],lIndex.Size);
    finally
      lIndex.Free;
    end;
  finally
    lReader.Free;
  end;
  AssertContains('CHM index entry','DebugLogger property',lText);
  AssertContains('CHM index entry target','uprop/debuglogger.html',lText);
end;


procedure TTestUnitProperties.TestManPage;

var
  lPage: string;

begin
  RunFPDoc('man','man',[]);
  lPage:=ReadText(FDir+'man'+PathDelim+'uprop.properties.3');
  AssertContains('Properties section','.SH PROPERTIES',lPage);
  AssertContains('Subsection for the property','.SS DebugLogger',lPage);
  AssertContains('Declaration','property DebugLogger : TObject',lPage);
  AssertContains('Description','Describes the debug logger property.',lPage);
  AssertContains('Unit page refers to the properties page','uprop.properties (3)',
    ReadText(FDir+'man'+PathDelim+'uprop.3'));
end;


procedure TTestUnitProperties.TestManSeeAlso;

var
  lPage, lSeeAlso: string;

begin
  RunFPDoc('man','man',[]);
  lPage:=ReadText(FDir+'man'+PathDelim+'uprop.getdebuglogger.3');
  lSeeAlso:=Copy(lPage,Pos('.SH SEE ALSO',lPage),Length(lPage));
  AssertContains('Function link','uprop.setdebuglogger (3)',lSeeAlso);
  AssertContains('Class link','uprop.tfoo (3)',lSeeAlso);
  AssertContains('Unknown link kept as written','nosuchthing (3)',lSeeAlso);
  AssertTrue('No link without unit prefix: '+lSeeAlso,Pos(LineEnding+'.',lSeeAlso)=0);
  AssertTrue('No link to a missing class member page: '+lSeeAlso,Pos('tfoobar',lSeeAlso)=0);
  AssertTrue('No bare element name for a known element: '+lSeeAlso,Pos(LineEnding+'debuglogger (3)',lSeeAlso)=0);
  AssertTrue('Linked pages exist',
    FileExists(FDir+'man'+PathDelim+'uprop.setdebuglogger.3') and
    FileExists(FDir+'man'+PathDelim+'uprop.tfoo.3') and
    FileExists(FDir+'man'+PathDelim+'uprop.properties.3') and
    FileExists(FDir+'man'+PathDelim+'uprop.variables.3'));
end;


initialization
  RegisterTest(TTestUnitProperties);
end.
