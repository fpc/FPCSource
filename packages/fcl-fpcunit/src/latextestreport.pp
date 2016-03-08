{$mode objfpc}
{$h+}
{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2006 by Dean Zobec

    an example of latex report for FPCUnit tests.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit latextestreport;

interface

uses
  classes, SysUtils, fpcunit, fpcunitreport, strutils;

type
   
  { TLatexResultsWriter }

  TLatexResultsWriter = class(TCustomResultsWriter)
  private
    FDoc: TStringList;
    FSuiteHeaderIdx: TFPList;
    FTempFailure: TTestFailure;
    function TimeFormat(ATiming: TDateTime): String;
  protected
    class function EscapeText(const S: string): String; virtual;
    procedure WriteTestHeader(ATest: TTest; ALevel: integer; ACount: integer); override;
    procedure WriteTestFooter(ATest: TTest; ALevel: integer; ATiming: TDateTime); override;
    procedure WriteSuiteHeader(ATestSuite: TTestSuite; ALevel: integer); override;
    procedure WriteSuiteFooter(ATestSuite: TTestSuite; ALevel: integer; 
      ATiming: TDateTime; ANumRuns: integer; ANumErrors: integer; 
      ANumFailures: integer; ANumIgnores: integer); override;
  public
    constructor Create(aOwner: TComponent); override;
    destructor  Destroy; override;
    procedure WriteHeader; override;
    procedure WriteFooter; override;
    procedure WriteResult(aResult: TTestResult); override;
    procedure AddFailure(ATest: TTest; AFailure: TTestFailure); override;
    procedure AddError(ATest: TTest; AError: TTestFailure); override;
    procedure StartTest(ATest: TTest); override;
    procedure EndTest(ATest: TTest); override;
  end;

function TestSuiteAsLatex(aSuite:TTestSuite): string;
function GetSuiteAsLatex(aSuite: TTestSuite): string;

implementation

uses dateutils;

function TLatexResultsWriter.TimeFormat(ATiming: TDateTime): String;
Var
  M : Int64;

begin
  Result:='ss.zzz';
  M:=MinutesBetween(ATiming,0);
  if M>60 then
    Result:='hh:mm:'+Result
  else if M>1 then
   Result:='mm:'+Result;
end;

class function TLatexResultsWriter.EscapeText(const S: string): String;
var
  i: integer;
begin
  SetLength(Result, 0);
    for i := 1 to Length(S) do
      case S[i] of
        '&','{','}','#','_','$','%':     // Escape these characters
          Result := Result + '\' + S[i];
        '~','^':
          Result := Result + '\'+S[i]+' ';
        '\':
          Result := Result + '$\backslash$';
        '<':
          Result := Result + '$<$';
        '>':
          Result := Result + '$>$'
        else
          Result := Result + S[i];
      end;
end;

constructor TLatexResultsWriter.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FDoc := TStringList.Create;
  FSuiteHeaderIdx := TFPList.Create;
  FTempFailure := nil;
end;

destructor  TLatexResultsWriter.Destroy;
begin
  FDoc.Free;
  FSuiteHeaderIdx.Free;
  inherited Destroy;
end;

procedure TLatexResultsWriter.WriteHeader;
begin
  inherited WriteHeader;
  FDoc.Add('\documentclass[a4paper,12pt]{report}');
  FDoc.Add('\usepackage{fullpage}');
  FDoc.Add('\usepackage{color}');
  FDoc.Add('\definecolor{Blue}{rgb}{0.3,0.3,0.9}');
  FDoc.Add('\definecolor{Red}{rgb}{1,0,0}');
  FDoc.Add('\definecolor{Pink}{rgb}{1,0,1}');
  FDoc.Add('\definecolor{Yellow}{rgb}{1,1,0}');
  FDoc.Add('\author{FPCUnit}');
  FDoc.Add('\title{Unit tests run by FPCUnit}');
  FDoc.Add('\begin{document}');
  FDoc.Add('\maketitle');
  FDoc.Add('\flushleft');
end;

procedure TLatexResultsWriter.WriteFooter;
begin
  inherited WriteFooter;
  
end;
    
procedure TLatexResultsWriter.WriteResult(aResult: TTestResult);
var
  f: text;
begin
  inherited WriteResult(aResult);
  with aResult do
  begin
    FDoc.Insert(11, '\begin{tabular}{ll}');
    FDoc.Insert(12, '{\bf Number of run tests:} &' + intToStr(RunTests)+ '\\');
    FDoc.Insert(13, '{\bf Number of errors:} &' + intToStr(NumberOfErrors)+ '\\');
    FDoc.Insert(14, '{\bf Number of failures:} &' + intToStr(NumberOfFailures)+ '\\');
    FDoc.Insert(15, '{\bf Number of ignored tests:} &' + intToStr(NumberOfIgnoredTests)+ '\\');
    FDoc.Insert(16, '\end{tabular}');
  end;
  FDoc.Add('\end{document}');
  system.Assign(f, FileName);
  rewrite(f);
  writeln(f, FDoc.Text);
  close(f);
end;
  
{ITestListener}
    
procedure TLatexResultsWriter.AddFailure(ATest: TTest; AFailure: TTestFailure);
begin
  inherited AddFailure(ATest, AFailure);
  FTempFailure := AFailure;
end;

procedure TLatexResultsWriter.AddError(ATest: TTest; AError: TTestFailure);
begin
  inherited;
  FTempFailure := AError;
end;
    
procedure TLatexResultsWriter.WriteTestHeader(ATest: TTest; ALevel: integer; ACount: integer);
begin
  inherited;
end;

procedure TLatexResultsWriter.WriteTestFooter(ATest: TTest; ALevel: integer; ATiming: TDateTime);

Var
  S : String;
begin
  inherited;
  S:=StringOfChar(' ',ALevel*2)+ '  '+ '\item[-] ';
  if Not SkipTiming then
    S:=S+FormatDateTime(TimeFormat(ATiming), ATiming);
  S:=S+ '  ' + EscapeText(ATest.TestName);
  FDoc.Add(S);
  if Assigned(FTempFailure) then
  begin
    //check if it's an error 
    if not FTempFailure.IsFailure then 
    begin
      FDoc[FDoc.Count -1] := '{\color{Red}'+FDoc[FDoc.Count -1];
      FDoc.Add('\begin{description}');
      FDoc.Add('\item[Error:] '+ EscapeText(FTempFailure.ExceptionClassName));
      FDoc.Add('\item[Exception:]  '+ EscapeText(FTempFailure.ExceptionMessage));
      FDoc.Add('\item[Source unit:] '+ EscapeText(FTempFailure.SourceUnitName));
      FDoc.Add('\item[Method name:] '+ EscapeText(FTempFailure.FailedMethodName));
      FDoc.Add('\item[Line number:] '+ IntToStr(FTempFailure.LineNumber));
      FDoc.Add('\end{description}}');
    end
    else
      if FTempFailure.IsIgnoredTest then
      begin
        FDoc[FDoc.Count -1] := '{\color{Yellow}'+FDoc[FDoc.Count -1] + '  {\bf IGNORED TEST: ' + 
          EscapeText(FTempFailure.ExceptionMessage) +'}}'
      end
      else
        //is a failure
        FDoc[FDoc.Count -1] := '{\color{Pink}'+FDoc[FDoc.Count -1] + '  {\bf FAILED: ' + 
          EscapeText(FTempFailure.ExceptionMessage) +'}}';
  end;
  FTempFailure := nil;
end;

procedure TLatexResultsWriter.WriteSuiteHeader(ATestSuite: TTestSuite; ALevel: integer);
begin
  inherited;
  FDoc.Add('{\bf {\color{Blue}'+ StringOfChar(' ',ALevel*2)+ '\item[-] '+ 
    EscapeText(ATestSuite.TestName)+ '}}');
  FSuiteHeaderIdx.Add(Pointer(FDoc.Count - 1));
  FDoc.Add(StringOfChar(' ',ALevel*2)+ '\begin{itemize}');
end;

procedure TLatexResultsWriter.WriteSuiteFooter(ATestSuite: TTestSuite; ALevel: integer;
  ATiming: TDateTime; ANumRuns: integer; ANumErrors: integer; ANumFailures: integer;
  ANumIgnores: integer);
var
  idx: integer;
  S : String;

begin
  inherited;
  FDoc.Add(StringOfChar(' ',ALevel*2)+ ' \end{itemize}');
  idx := Integer(FSuiteHeaderIdx[FSuiteHeaderIdx.Count -1]);
  S:= ' {\color{Blue}';
  if Not SkipTiming then
    S:=S+ ' Time: '+FormatDateTime('ss.zzz', ATiming);
  S:=S+' N:'+ IntToStr(ANumRuns)+ ' E:'+ IntToStr(ANumErrors)+ ' F:'+ IntToStr(ANumFailures)+
  ' I:'+ IntToStr(ANumIgnores)+'}';
  FDoc[idx] := FDoc[idx] +S;
  FSuiteHeaderIdx.Delete(FSuiteHeaderIdx.Count -1);
end;

procedure TLatexResultsWriter.StartTest(ATest: TTest);
begin
  inherited StartTest(ATest);
end;
    
procedure TLatexResultsWriter.EndTest(ATest: TTest);
begin
  inherited EndTest(ATest);
  
end;

function TestSuiteAsLatex(aSuite:TTestSuite): string;
var
  i,j: integer;
  s: TTestSuite;
begin
  Result := TLatexResultsWriter.EscapeText(aSuite.TestSuiteName) + System.sLineBreak;
  Result := Result + '\begin{itemize}'+ System.sLineBreak;
  for i := 0 to aSuite.ChildTestCount - 1 do
    if ASuite.Test[i] is TTestSuite then
      begin
      Result:=Result + '\item[-] ';
      Result := Result + '\flushleft' + System.sLineBreak;
      Result:=Result+TestSuiteAsLatex(TTestSuite(ASuite.Test[i]))+System.sLineBreak;
      end
    else   
      begin
      Result := Result + '\item[-] ' + 
               TLatexResultsWriter.EscapeText(TTestcase(aSuite.Test[i]).TestName)
               + System.sLineBreak;
      end;    
  Result := Result +'\end{itemize}' + System.sLineBreak;
end;


function GetSuiteAsLatex(aSuite: TTestSuite): string;
begin
  if aSuite <> nil then
    begin
      Result := '\documentclass[a4paper,12pt]{article}' + System.sLineBreak;
      Result := Result + '\usepackage{array}' + System.sLineBreak;
      Result := Result + '\usepackage{mdwlist}' + System.sLineBreak + System.sLineBreak;
      Result := Result + '\begin{document}' + System.sLineBreak + System.sLineBreak;
      if aSuite.TestName = '' then
        aSuite.TestName := 'Test Suite';
      Result := Result + TestSuiteAsLatex(aSuite);
      Result := Result + '\end{document}';
    end
  else
    Result := '';
end;

end.
