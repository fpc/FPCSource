{
    This file is part of the Free Component Library
    Copyright (c) 2024 by Michael Van Canneyt (michael@freepascal.org)

    Program to analyse a package file, demo for dpkinfo.pp

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

program showdpk;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, Types, CustApp, dpkinfo;

type


  { TDPK2LPKApplication }

  TDPK2LPKApplication = class(TCustomApplication)
  private
    function ConfigInfo(Info: TPackageInfo): TInfoKind;
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure Usage(const Msg: String); virtual;
  end;


{ TDPK2LPKApplication }

function TDPK2LPKApplication.ConfigInfo(Info : TPackageInfo) : TInfoKind;

  procedure OptToStrings(Opt : string; Strings: TSTrings);
  begin
    begin
    if (Opt[1]='@') then
      begin
      Opt:=Copy(Opt,2);
      if FileExists(Opt) then
        Strings.LoadFromFile(Opt);
      end
    else
      begin
      Strings.Delimiter:=';';
      Strings.DelimitedText:=Opt;
      end;
    end;

  end;

var
  m,Opt : String;

begin
  Opt:=GetOptionValue('k','known');
  if (Opt<>'') then
    OptToStrings(Opt,Info.KnownPackages);
  Opt:=GetOptionValue('d','defines');
  if (Opt<>'') then
    OptToStrings(Opt,Info.Defines);
  info.UseAbsolute:=HasOption('a','absolute');
  m:=GetOptionValue('m','mode');
  case lowercase(m) of
    'requires' : result:=ikRequires;
    'files' : Result:=ikFiles;
    'paths' : Result:=ikPaths;
  else
    Usage('Unknown mode: '+m);
    Result:=ikUnknown;
  end;
end;

procedure TDPK2LPKApplication.DoRun;

const
  Short = 'ho:k:m:d:a';
  Long : Array of string = ('help','output:','known:','mode:','defines:','absolute');

var
  ErrorMsg: String;
  Info : TPackageInfo;
  Kind : TInfoKind;
  aLine,PFN,OFN : String;
  FNS : TStringDynArray;

begin
  Terminate;
  ErrorMsg:=CheckOptions(Short,Long);
  if (ErrorMsg<>'') or HasOption('h','help') then
    begin
    Usage(ErrorMsg);
    Exit;
    end;
  FNS:=GetNonOptions(Short,Long);
  if Length(FNS)<>1 then
    begin
    Usage('Need one input file');
    exit;
    end;
  PFN:=FNS[0];
  OFN:=GetOptionValue('o','output');
  Info:=TPackageInfo.Create(Self);
  try
    Kind:=ConfigInfo(Info);
    if Kind=ikUnknown then
      exit;
    Info.ShowInfo(PFN,Kind);
    if OFN<>'' then
      Info.Output.SaveToFile(OFN)
    else
      for aLine in Info.Output do
        Writeln(aLine);
  finally
    Info.Free;
  end;

end;

constructor TDPK2LPKApplication.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TDPK2LPKApplication.Destroy;
begin
  inherited Destroy;
end;

procedure TDPK2LPKApplication.Usage(Const Msg : String);

begin
  if Msg<>'' then
    Writeln('Error: ',Msg);
  Writeln('Usage: ',ExeName, ' [options] PackageFile');
  Writeln('Where [options] is one or more of:');
  Writeln('-h --help        this help');
  Writeln('-k --known=FILE  File with known packages, they will not appear in output');
  Writeln('-o --output=FILE Write output to file instead of stdout.');
  Writeln('-m --mode=MODE   Select output mode, MODE is one of:');
  Writeln('                 requires    Show requires in package');
  Writeln('                 files       Show files in package');
  Writeln('                 paths       Show paths for source files in package');
  Writeln('-d --defines     Semicolon-separated list of defines for parser');
  Writeln('-a --absolute    Use absolute filenames instead of relative');
  ExitCode:=Ord(Msg<>'');
end;

var
  Application: TDPK2LPKApplication;
begin
  Application:=TDPK2LPKApplication.Create(nil);
  Application.Title:='Show package info';
  Application.Run;
  Application.Free;
end.

