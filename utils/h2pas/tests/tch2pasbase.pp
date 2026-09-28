{
  h2pas test suite: runs h2pas on a header and inspects the generated unit.
  Copyright (c) 2026 by Michael Van Canneyt
  See the file COPYING.FPC for details about the copyright.
}
unit tcH2PasBase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit;

type

  { TH2PasTestCase }

  TH2PasTestCase = class(TTestCase)
  private
    FWorkDir: string;
    FRawOutput: TStringList;
    FOutput: string;
    FToolOutput: string;
    FToolExitCode: integer;
    class var FSequence: integer;
    procedure DeleteTree(const aDir: string);
    function Section(aFromInterface: boolean): string;
    function DumpOutput: string;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  public
    // Returns the h2pas executable under test: $H2PAS, else a build next to the test directory.
    class function H2PasExecutable: string;
    // Returns the compiler for compilation checks: $FPC, else fpc on the PATH; empty when absent.
    class function CompilerExecutable: string;
    // Returns aText with runs of whitespace collapsed, lines trimmed and empty lines dropped.
    class function Normalize(const aText: string): string;
    // Returns aLines joined with line endings.
    class function Lines(const aLines: array of string): string;
    // Writes aContent to aName in the work directory.
    procedure WriteWorkFile(const aName, aContent: string);
    // Runs h2pas with aArgs in the work directory and loads aOutputFile when it was created.
    procedure RunH2Pas(const aArgs: array of string; const aOutputFile: string = 'output.pp');
    // Writes aHeader to input.h and converts it to output.pp using aOptions.
    procedure Convert(const aHeader: array of string; const aOptions: array of string);
    // Writes aHeader to input.h and converts it to output.pp without options.
    procedure Convert(const aHeader: array of string);
    // Returns the normalized output between the interface and implementation keywords.
    function InterfacePart: string;
    // Returns the normalized output after the implementation keyword.
    function ImplementationPart: string;
    // Returns how often aFragment occurs in the normalized output.
    function CountOf(const aFragment: string): integer;
    // Fails unless the normalized lines aExpected occur consecutively in aText.
    // Each line matches a whole output line, except the last which may be a prefix.
    procedure AssertContains(const aMsg: string; const aExpected: array of string; const aText: string);
    // Fails unless the normalized lines aExpected occur consecutively in the output.
    procedure AssertOutput(const aMsg: string; const aExpected: array of string);
    // Fails unless the normalized lines aExpected occur consecutively in the interface part.
    procedure AssertInterface(const aMsg: string; const aExpected: array of string);
    // Fails unless the normalized lines aExpected occur consecutively in the implementation part.
    procedure AssertImplementation(const aMsg: string; const aExpected: array of string);
    // Fails when aUnexpected occurs in the normalized output.
    procedure AssertNotOutput(const aMsg: string; const aUnexpected: string);
    // Fails when aUnexpected occurs in the normalized implementation part.
    procedure AssertNotImplementation(const aMsg: string; const aUnexpected: string);
    // Fails unless aLine occurs verbatim, trailing blanks excepted, as a line of the output.
    procedure AssertRawLine(const aMsg: string; const aLine: string);
    // Fails unless h2pas exited with code 0 and reported no syntax or internal error.
    procedure AssertConverted;
    // Fails unless the compiler accepts output.pp; ignored when no compiler is found.
    procedure AssertCompiles;
    // Directory in which h2pas runs for the current test.
    property WorkDir: string read FWorkDir;
    // Generated file, as written by h2pas.
    property RawOutput: TStringList read FRawOutput;
    // Generated file, normalized.
    property Output: string read FOutput;
    // Text h2pas wrote to stdout and stderr.
    property ToolOutput: string read FToolOutput;
    // Exit code of the last h2pas run.
    property ToolExitCode: integer read FToolExitCode;
  end;

implementation

uses
  process;

const
  NL = #10;
{$if defined(windows) or defined(go32v2) or defined(os2)}
  ExeExt = '.exe';
{$else}
  ExeExt = '';
{$endif}


class function TH2PasTestCase.H2PasExecutable: string;

var
  lDir, lTarget: string;

begin
  Result:=GetEnvironmentVariable('H2PAS');
  if Result<>'' then
    exit;
  lDir:=ExtractFilePath(ExpandFileName(ParamStr(0)));
  lTarget:=LowerCase({$I %FPCTARGETCPU%}+'-'+{$I %FPCTARGETOS%});
  Result:=ExpandFileName(lDir+'../h2pas'+ExeExt);
  if FileExists(Result) then
    exit;
  Result:=ExpandFileName(lDir+'../bin/'+lTarget+'/h2pas'+ExeExt);
  if not FileExists(Result) then
    Result:='';
end;


class function TH2PasTestCase.CompilerExecutable: string;

begin
  Result:=GetEnvironmentVariable('FPC');
  if Result='' then
    Result:=ExeSearch('fpc'+ExeExt,GetEnvironmentVariable('PATH'));
end;


class function TH2PasTestCase.Normalize(const aText: string): string;

var
  lLines: TStringList;
  lLine, lNew: string;
  lIndex: integer;
  lSpace: boolean;
  lChar: char;

begin
  Result:='';
  lLines:=TStringList.Create;
  try
    lLines.Text:=aText;
    for lLine in lLines do
      begin
      lNew:='';
      lSpace:=false;
      for lIndex:=1 to Length(lLine) do
        begin
        lChar:=lLine[lIndex];
        if lChar in [#9,#12,' '] then
          lSpace:=true
        else
          begin
          if lSpace and (lNew<>'') then
            lNew:=lNew+' ';
          lSpace:=false;
          lNew:=lNew+lChar;
          end;
        end;
      if lNew<>'' then
        Result:=Result+lNew+NL;
      end;
  finally
    lLines.Free;
  end;
end;


class function TH2PasTestCase.Lines(const aLines: array of string): string;

var
  lLine: string;

begin
  Result:='';
  for lLine in aLines do
    Result:=Result+lLine+LineEnding;
end;


procedure TH2PasTestCase.SetUp;

begin
  inherited SetUp;
  Inc(FSequence);
  FWorkDir:=IncludeTrailingPathDelimiter(GetTempDir(false))
            +Format('h2pastest-%d-%d',[GetProcessID,FSequence])+PathDelim;
  DeleteTree(FWorkDir);
  if not ForceDirectories(FWorkDir) then
    Fail('Cannot create work directory '+FWorkDir);
  FRawOutput:=TStringList.Create;
  FOutput:='';
  FToolOutput:='';
  FToolExitCode:=-1;
end;


procedure TH2PasTestCase.TearDown;

begin
  FreeAndNil(FRawOutput);
  DeleteTree(FWorkDir);
  inherited TearDown;
end;


procedure TH2PasTestCase.DeleteTree(const aDir: string);

var
  lInfo: TSearchRec;
  lDir: string;

begin
  lDir:=IncludeTrailingPathDelimiter(aDir);
  if not DirectoryExists(lDir) then
    exit;
  if FindFirst(lDir+AllFilesMask,faAnyFile,lInfo)=0 then
    try
      repeat
        if (lInfo.Name='.') or (lInfo.Name='..') then
          continue;
        if (lInfo.Attr and faDirectory)<>0 then
          DeleteTree(lDir+lInfo.Name)
        else
          DeleteFile(lDir+lInfo.Name);
      until FindNext(lInfo)<>0;
    finally
      FindClose(lInfo);
    end;
  RemoveDir(lDir);
end;


procedure TH2PasTestCase.WriteWorkFile(const aName, aContent: string);

var
  lFile: TStringList;

begin
  lFile:=TStringList.Create;
  try
    lFile.Text:=aContent;
    lFile.SaveToFile(FWorkDir+aName);
  finally
    lFile.Free;
  end;
end;


procedure TH2PasTestCase.RunH2Pas(const aArgs: array of string; const aOutputFile: string);

var
  lExe: string;

begin
  lExe:=H2PasExecutable;
  if lExe='' then
    Fail('h2pas executable not found: build h2pas or set the H2PAS environment variable');
  if not FileExists(lExe) then
    Fail('h2pas executable does not exist: '+lExe);
  if RunCommandInDir(FWorkDir,lExe,aArgs,FToolOutput,FToolExitCode,[poStderrToOutPut])=-1 then
    Fail('Cannot run '+lExe);
  FRawOutput.Clear;
  if FileExists(FWorkDir+aOutputFile) then
    FRawOutput.LoadFromFile(FWorkDir+aOutputFile);
  FOutput:=Normalize(FRawOutput.Text);
end;


procedure TH2PasTestCase.Convert(const aHeader: array of string; const aOptions: array of string);

var
  lArgs: array of string;
  lIndex: integer;

begin
  WriteWorkFile('input.h',Lines(aHeader));
  lArgs:=[];
  SetLength(lArgs,Length(aOptions)+3);
  for lIndex:=0 to Length(aOptions)-1 do
    lArgs[lIndex]:=aOptions[lIndex];
  lIndex:=Length(aOptions);
  lArgs[lIndex]:='-o';
  lArgs[lIndex+1]:='output.pp';
  lArgs[lIndex+2]:='input.h';
  RunH2Pas(lArgs);
end;


procedure TH2PasTestCase.Convert(const aHeader: array of string);

begin
  Convert(aHeader,[]);
end;


function TH2PasTestCase.Section(aFromInterface: boolean): string;

const
  SImplementation = NL+'implementation'+NL;

var
  lText: string;
  lPos: integer;

begin
  lText:=NL+FOutput;
  lPos:=Pos(SImplementation,lText);
  if aFromInterface then
    begin
    if lPos>0 then
      lText:=Copy(lText,1,lPos);
    lPos:=Pos(NL+'interface'+NL,lText);
    if lPos>0 then
      Delete(lText,1,lPos+Length('interface'));
    end
  else if lPos>0 then
    Delete(lText,1,lPos+Length(SImplementation)-2)
  else
    lText:='';
  Result:=lText;
end;


function TH2PasTestCase.InterfacePart: string;

begin
  Result:=Section(true);
end;


function TH2PasTestCase.ImplementationPart: string;

begin
  Result:=Section(false);
end;


function TH2PasTestCase.CountOf(const aFragment: string): integer;

var
  lFragment: string;
  lPos: integer;

begin
  Result:=0;
  lFragment:=Trim(Normalize(aFragment));
  if lFragment='' then
    exit;
  lPos:=Pos(lFragment,FOutput);
  while lPos>0 do
    begin
    Inc(Result);
    lPos:=Pos(lFragment,FOutput,lPos+Length(lFragment));
    end;
end;


function TH2PasTestCase.DumpOutput: string;

begin
  Result:=NL+'--- h2pas messages:'+NL+FToolOutput+NL+'--- generated file:'+NL+FOutput;
end;


procedure TH2PasTestCase.AssertContains(const aMsg: string; const aExpected: array of string; const aText: string);

var
  lExpected: string;

begin
  lExpected:=Normalize(Lines(aExpected));
  if (lExpected<>'') and (lExpected[Length(lExpected)]=NL) then
    SetLength(lExpected,Length(lExpected)-1);
  if Pos(NL+lExpected,NL+aText)=0 then
    Fail(aMsg+': expected to find'+NL+lExpected+DumpOutput);
end;


procedure TH2PasTestCase.AssertOutput(const aMsg: string; const aExpected: array of string);

begin
  AssertContains(aMsg,aExpected,FOutput);
end;


procedure TH2PasTestCase.AssertInterface(const aMsg: string; const aExpected: array of string);

begin
  AssertContains(aMsg,aExpected,InterfacePart);
end;


procedure TH2PasTestCase.AssertImplementation(const aMsg: string; const aExpected: array of string);

begin
  AssertContains(aMsg,aExpected,ImplementationPart);
end;


procedure TH2PasTestCase.AssertNotOutput(const aMsg: string; const aUnexpected: string);

begin
  if CountOf(aUnexpected)>0 then
    Fail(aMsg+': did not expect to find'+NL+Trim(Normalize(aUnexpected))+DumpOutput);
end;


procedure TH2PasTestCase.AssertNotImplementation(const aMsg: string; const aUnexpected: string);

var
  lUnexpected: string;

begin
  lUnexpected:=Trim(Normalize(aUnexpected));
  if Pos(lUnexpected,ImplementationPart)>0 then
    Fail(aMsg+': did not expect to find in the implementation'+NL+lUnexpected+DumpOutput);
end;


procedure TH2PasTestCase.AssertRawLine(const aMsg: string; const aLine: string);

var
  lLine: string;

begin
  for lLine in FRawOutput do
    if TrimRight(lLine)=aLine then
      exit;
  Fail(aMsg+': expected the line'+NL+'['+aLine+']'+NL+'--- generated file:'+NL+FRawOutput.Text);
end;


procedure TH2PasTestCase.AssertConverted;

begin
  if FToolExitCode<>0 then
    Fail(Format('h2pas exited with code %d',[FToolExitCode])+DumpOutput);
  if Pos('syntax error',FToolOutput)>0 then
    Fail('h2pas reported a syntax error'+DumpOutput);
  if Pos('Internal error',FToolOutput)>0 then
    Fail('h2pas reported an internal error'+DumpOutput);
  if FRawOutput.Count=0 then
    Fail('h2pas did not write output.pp'+DumpOutput);
end;


procedure TH2PasTestCase.AssertCompiles;

var
  lCompiler, lUnits, lMessages: string;
  lExitCode: integer;

begin
  lCompiler:=CompilerExecutable;
  if lCompiler='' then
    Ignore('No compiler found: set the FPC environment variable');
  lUnits:=FWorkDir+'units';
  ForceDirectories(lUnits);
  if RunCommandInDir(FWorkDir,lCompiler,['-ve','-FU'+lUnits,'-FE'+lUnits,'output.pp'],
                     lMessages,lExitCode,[poStderrToOutPut])=-1 then
    Fail('Cannot run '+lCompiler);
  if lExitCode<>0 then
    Fail('Generated unit does not compile:'+NL+lMessages+NL+'--- generated file:'+NL+FRawOutput.Text);
end;


end.
