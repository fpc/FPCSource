{$mode objfpc}
{$H+}
{
    This file is part of Free Pascal Build tools
    Copyright (c) 2005 by Michael Van Canneyt

    Create a configuration file

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
program fpcmkcfg;

uses SysUtils,Classes,fpTemplate;

{
  The inc files must be built from a template with the data2inc
  command.
  data2inc -b -s fpc.cft fpccfg.inc DefaultConfig
  data2inc -b -s fpinc.ini fpini.inc fpini
  data2inc -b -s fpinc.cfg fpcfg.inc fpcfg
  data2inc -b -s fppkg.cfg fppkg.inc fppkg
  data2inc -b -s default.cft default.inc fppkg_default
}

{$i fpccfg.inc}
{$i fpini.inc}
{$i fpcfg.inc}
{$i fppkg.inc}
{$i default.inc}

Const
  BuildVersion={$I %FPCVERSION%};
  BuildTarget={$I %FPCTARGET%};
  BuildOSTarget={$I %FPCTARGETOS%};


Resourcestring
  SUsage00  = 'Usage: %s [options]';
  SUsage10  = 'Where options is one or more of';
  SUSage20  = '  -t filename   Template file name. Default is built-in';
  SUSage30  = '  -o filename   Set output file. Default is standard output.';
  SUsage40  = '  -d name=value define name=value pair.';
  SUsage50  = '  -h            show this help and exit.';
  SUsage60  = '  -u name       remove name from list of name/value pairs.';
//  SUsage70  = '  -l filename   read name/value pairs from filename';
  SUsage80  = '  -b            show builtin template and exit.';
  SUsage90  = '  -v            be verbose.';
  Susage100 = '  -0            use built in fpc.cfg template (default)';
  Susage110 = '  -1            use built in fp.cfg template';
  Susage120 = '  -2            use built in fp.ini template';
  Susage130 = '  -3            use built in fppkg.cfg template';
  Susage140 = '  -4            use built in fppkg default compiler template';
  SErrUnknownOption   = 'Error: Unknown option.';
  SErrArgExpected     = 'Error: Option "%s" requires an argument.';
  SErrIncompletePair  = 'Error: Incomplete name-value pair "%s".';
  SErrNoSuchFile      = 'Error: File "%s" does not exist.';
  SErrBackupFailed    = 'Error: Backup of file "%s" to "%s" failed.';
  SErrDelBackupFailed = 'Error: Delete of old backup file "%s" failed.';
  SWarnIgnoringFile   = 'Warning: Ignoring non-existent file: ';
  SWarnIgnoringPair   = 'Warning: ignoring wrong name/value pair: ';
  SStats              = 'Replaced %d placeholders in %d lines.';
  SSubstInLine        = 'Replaced %s placeholders in line %d.';


Var
  Verbose : Boolean;
  SkipBackup : Boolean;
  Cfg : TStringList;
  TemplateParser: TTemplateParser;
  TemplateFileName,
  OutputFileName : String;
  IDEBuildin : Integer;

function GetDefaultLocalRepository: string;

begin
{$IFDEF Unix}
  result := '{UserDir}.fppkg'+PathDelim;
{$ELSE Unix}
  result := '{AppConfigDir}';
{$ENDIF Unix}
end;

procedure Init;

begin
  Verbose:=False;
  IDEBuildIn:=0;

  TemplateParser := TTemplateParser.Create;
  TemplateParser.StartDelimiter:='%';
  TemplateParser.EndDelimiter:='%';
  TemplateParser.Values['FPCVERSION'] := BuildVersion;
  TemplateParser.Values['FPCTARGET'] := BuildTarget;
  TemplateParser.Values['FPCTARGETOS'] := BuildOSTarget;
  TemplateParser.Values['PWD'] := GetCurrentDir;
  TemplateParser.Values['BUILDDATE'] := DateToStr(Date);
  TemplateParser.Values['BUILDTIME'] := TimeToStr(Time);

  TemplateParser.Values['LOCALREPOSITORY'] := GetDefaultLocalRepository;

  Cfg:=TStringList.Create;
  Cfg.Text:=StrPas(Addr(DefaultConfig[0][1]));
end;

Procedure Done;

begin
  FreeAndNil(Cfg);
  FreeAndNil(TemplateParser);
end;

Procedure Usage;

begin
  Writeln(Format(SUsage00,[ExtractFileName(Paramstr(0))]));
  Writeln(SUsage10);
  Writeln(SUsage20);
  Writeln(SUsage30);
  Writeln(SUsage40);
  Writeln(SUsage50);
  Writeln(SUsage60);
//  Writeln(SUsage70);
  Writeln(SUsage80);
  Writeln(SUsage90);
  Writeln(SUsage100);
  Writeln(SUsage110);
  Writeln(SUsage120);
  Writeln(SUsage130);
  Writeln(SUsage140);
  Halt(1);
end;

Procedure UnknownOption(Const S : String);

begin
  Writeln(SErrUnknownOption,S);
  Usage;
end;

Procedure ShowBuiltIn;

Var
  I : Integer;


begin
  For I:=0 to Cfg.Count-1 do
    Writeln(Cfg[I]);
end;


Procedure ProcessCommandline;

Var
  I : Integer;
  S : String;

  Function GetOptArg : String;

  begin
    If I=ParamCount then
      begin
      Writeln(StdErr,Format(SErrArgExpected,[S]));
      Halt(1);
      end;
    inc(I);
    Result:=ParamStr(I);
  end;

  procedure AddPair(const Value: String);
  var P: integer;
      N,V: String;
  begin
    P:=Pos('=',Value);
    If p=0 then
      begin
      Writeln(StdErr,Format(SErrIncompletePair,[Value]));
      Halt(1);
      end;
    V:=Value;
    N:=Copy(V,1,P-1);
    Delete(V,1,P);
    TemplateParser.Values[N] := V;
  end;

begin
  I:=1;
  While( I<=ParamCount) do
    begin
    S:=Paramstr(i);
    If Length(S)<=1 then
      UnknownOption(S)
    else
      case S[2] of
        'v' : Verbose:=True;
        'h' : Usage;
        'b' : begin
              ShowBuiltin;
              halt(0);
              end;
        't' : TemplateFileName:=GetOptArg;
        'd' : AddPair(GetOptArg);
        'u' : TemplateParser.Values[GetOptArg]:='';
        'o' : OutputFileName:=GetoptArg;
        's' : SkipBackup:=True;
        '0' : IDEBuildin:=0;
        '1' : IDEBuildin:=1;
        '2' : IDEBuildin:=2;
        '3' : IDEBuildin:=3;
        '4' : IDEBuildin:=4;
      else
        UnknownOption(S);
      end;
    Inc(I);
    end;
  If (TemplateFileName<>'') then
    begin
    If Not FileExists(TemplateFileName) then
      begin
      Writeln(StdErr,Format(SErrNoSuchFile,[TemplateFileName]));
      Halt(1);
      end;
    Cfg.LoadFromFile(TemplateFileName);
    TemplateParser.Values['TEMPLATEFILE'] := TemplateFileName;
    end
  else
    begin
      case IDEBuildin of
        1:
           Cfg.Text:=StrPas(Addr(fpcfg[0][1]));
        2:
           Cfg.Text:=StrPas(Addr(fpini[0][1]));
        3:
           Cfg.Text:=StrPas(Addr(fppkg[0][1]));
        4:
           Cfg.Text:=StrPas(Addr(fppkg_default[0][1]));
      end;

    TemplateParser.Values['TEMPLATEFILE'] := 'builtin';
    end;
end;


Procedure CreateFile;

Var
  Fout : Text;
  S,BFN : String;
  I : Integer;

begin
  If (OutputFileName<>'')
     and FileExists(OutputFileName)
     and not SkipBackup then
    begin
    BFN:=ChangeFileExt(OutputFileName,'.bak');
    If FileExists(BFN) and not DeleteFile(BFN) then
      begin
      Writeln(StdErr,Format(SErrDelBackupFailed,[BFN]));
      Halt(1);
      end;
    If not RenameFile(OutputFileName,BFN) then
      begin
      Writeln(StdErr,Format(SErrBackupFailed,[OutputFileName,BFN]));
      Halt(1);
      end;
    end;
  Assign(Fout,OutputFileName);
  Rewrite(FOut);
  Try
    For I:=0 to Cfg.Count-1 do
      begin
      S:=Cfg[i];
      S := TemplateParser.ParseString(S);
      Writeln(FOut,S);
      end;
  Finally
    Close(Fout);
  end;
end;

begin
  Init;
  Try
    ProcessCommandLine;
    CreateFile;
  Finally
    Done;
  end;
end.
