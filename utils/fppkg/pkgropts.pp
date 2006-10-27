{
    This file is part of the Free Pascal Utilities
    Copyright (c) 1999-2000 by the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$mode objfpc}
{$h+}
unit pkgropts;

interface

uses Classes, Sysutils, Inifiles, fpmktype;

Type

  { TPackagerOptions }

  TPackagerOptions = Class(TPersistent)
  private
    FRemoteMirrorsLocation : String;
    FLocalMirrorsLocation : String;
    FRemoteRepository : String;
    FLocalRepository : String;
    FInstallDir : String;
    FBuildDir : String;
    FCompiler : String;
    FCPU: TCPU;
    FDirty: Boolean;
    FOS: TOS;
    FLocalDir : String;
    function GetOptString(Index: integer): String;
    procedure SetCPU(const AValue: TCPU);
    procedure SetOptString(Index: integer; const AValue: String);
    procedure SetOS(const AValue: TOS);
  protected
    Property LocalDir : String Read FLocalDir;
  Public
    Constructor Create;
    Procedure InitDefaults;
    Procedure LoadFromIni(Ini : TCustomIniFile); virtual;
    Procedure SaveToIni(Ini : TCustomIniFile); virtual;
    Procedure LoadFromFile(FileName : String);
    Procedure SaveToFile(FileName : String);
    Property Dirty : Boolean Read FDirty;
  Published
    Property RemoteMirrorsLocation : String Index 0 Read GetOptString Write SetOptString;
    Property LocalMirrorsLocation : String Index 1 Read GetOptString Write SetOptString;
    Property RemoteRepository : String Index 2 Read GetOptString Write SetOptString;
    Property LocalRepository : String Index 3 Read GetOptString Write SetOptString;
    Property InstallDir : String Index 4 Read GetOptString Write SetOptString;
    Property BuildDir : String Index 5 Read GetOptString Write SetOptString;
    Property Compiler : String Index 6 Read GetOptString Write SetOptString;
    Property Target : String Index 7 Read GetOptString Write SetOptString;
    Property OS : TOS Read FOS Write SetOS;
    Property CPU : TCPU Read FCPU Write SetCPU;
  end;

Const
  DefaultCPU = {$I %FPCTARGETCPU%};
  DefaultOS  = {$I %FPCTARGETOS%};
  DefaultMirrorsLocation  = 'http://www.freepascal.org/repository/mirrors.xml';
  DefaultCompiler = 'fpc';
  DefaultRemoteRepository = 'fpc';
  DefaultUnixPrefix = '/usr/local/lib/fpc/fppkg';
  DefaultUnixBuildDir = '/tmp/fppkg/';
  DefaultMirrors = 'mirrors.xml';
  DefaultRepository = 'repository.dat';

  // ini file keys
  SDefaults = 'Defaults';

  KeyLocalMirrorsLocation  = 'LocalMirrors';
  KeyRemoteMirrorsLocation = 'RemoteMirrors';
  KeyRemoteRepository      = 'RemoteRepository';
  KeyLocalRepository       = 'LocalRepository';
  KeyInstallDir            = 'InstallDir';
  KeyBuildDir              = 'BuildDir';
  KeyCompiler              = 'Compiler' ;
  KeyOS                    = 'OS';
  KeyCPU                   = 'CPU';


Implementation

{$ifdef unix}
uses baseunix;
{$endif}
Function FixPath(S : String) : string;

begin
  If (S<>'') then
    Result:=IncludeTrailingPathDelimiter(S)
  else
    Result:='';
end;


{ TPackagerOptions }

function TPackagerOptions.GetOptString(Index: integer): String;
begin
  Case Index of
    0 : Result:=FRemoteMirrorsLocation;
    1 : Result:=FLocalMirrorsLocation;
    2 : Result:=FRemoteRepository;
    3 : Result:=FLocalRepository;
    4 : Result:=FInstallDir;
    5 : Result:=FBuildDir;
    6 : Result:=FCompiler;
    7 : Result:=MakeTargetString(CPU,OS);
  end;
end;

procedure TPackagerOptions.SetCPU(const AValue: TCPU);
begin
  if FCPU=AValue then exit;
  FCPU:=AValue;
  FDirty:=True;
end;

procedure TPackagerOptions.SetOptString(Index: integer; const AValue: String);
begin
  If AValue=GetOptString(Index) then
    Exit;
  Case Index of
    0 : FLocalMirrorsLocation:=AValue;
    1 : FRemoteMirrorsLocation:=AValue;
    2 : FRemoteRepository:=AValue;
    3 : FLocalRepository:=AValue;
    4 : FInstallDir:=FixPath(AValue);
    5 : FBuildDir:=FixPath(AValue);
    6 : FCompiler:=AValue;
    7 : StringToCPUOS(AValue,FCPU,FOS);
  end;
  FDirty:=True;
end;

procedure TPackagerOptions.SetOS(const AValue: TOS);
begin
  if FOS=AValue then exit;
  FOS:=AValue;
  FDirty:=True;
end;

constructor TPackagerOptions.Create;
begin
  InitDefaults;
end;

Procedure TPackagerOptions.InitDefaults;

begin
  FCPU:=StringToCPU(DefaultCPU);
  FOS:=StringToOS(DefaultOS);
  FCompiler:=DefaultCompiler;
  FRemoteMirrorsLocation:=DefaultMirrorsLocation;
  FRemoteRepository:=DefaultRemoteRepository;
{$ifdef unix}
  if (fpGetUID=0) then
    FLocalDir:=DefaultUnixPrefix
  else
    FLocalDir:=IncludeTrailingPathDelimiter(GetEnvironmentVariable('HOME'))+'.fppkg/';
  FBuildDir:=DefaultUnixBuildDir;
{$else}
  // Change as needed on all OS-es...
  FLocalDir:=ExtractFilePath(Paramstr(0));
  FBuildDir:=FLocalDir+'build'+PathDelim;
{$endif}
  FLocalMirrorsLocation:=FLocalDir+DefaultMirrors;
  FLocalRepository:=FLocalDir+DefaultRepository;
end;

procedure TPackagerOptions.LoadFromIni(Ini: TCustomIniFile);



begin
 With Ini do
   begin
   FLocalMirrorsLocation:=ReadString(SDefaults,KeyLocalMirrorsLocation,FLocalMirrorsLocation);
   FRemoteMirrorsLocation:=ReadString(SDefaults,KeyRemoteMirrorsLocation,FRemoteMirrorsLocation);
   FRemoteRepository:=ReadString(SDefaults,KeyRemoteRepository,FRemoteRepository);
   FLocalRepository:=ReadString(SDefaults,KeyLocalRepository,FLocalRepository);
   FInstallDir:=FixPath(ReadString(SDefaults,KeyInstallDir,FInstallDir));
   FBuildDir:=FixPath(ReadString(SDefaults,KeyBuildDir,FBuildDir));
   FCompiler:=ReadString(SDefaults,KeyCompiler,FCompiler);
   FOS:=StringToOS(ReadString(SDefaults,KeyOS,OSToString(OS)));
   FCPU:=StringToCPU(ReadString(SDefaults,KeyCPU,CPUtoString(CPU)));
   end;

end;

procedure TPackagerOptions.SaveToIni(Ini: TCustomIniFile);
begin
 With Ini do
   begin
   WriteString(SDefaults,KeyLocalMirrorsLocation,FLocalMirrorsLocation);
   WriteString(SDefaults,KeyRemoteMirrorsLocation,FRemoteMirrorsLocation);
   WriteString(SDefaults,KeyRemoteRepository,FRemoteRepository);
   WriteString(SDefaults,KeyLocalRepository,FLocalRepository);
   WriteString(SDefaults,KeyInstallDir,FInstallDir);
   WriteString(SDefaults,KeyBuildDir,FBuildDir);
   WriteString(SDefaults,KeyCompiler,FCompiler);
   WriteString(SDefaults,KeyOS,OSToString(OS));
   WriteString(SDefaults,KeyCPU,CPUtoString(CPU));
   end;
end;

procedure TPackagerOptions.LoadFromFile(FileName: String);

Var
  Ini : TMemIniFile;

begin
  Ini:=TMemIniFile.Create(FileName);
  try
    LoadFromIni(Ini);
  finally
    Ini.Free;
  end;
end;

procedure TPackagerOptions.SaveToFile(FileName: String);

Var
  Ini : TIniFile;

begin
  Ini:=TIniFile.Create(FileName);
  try
    SaveToIni(Ini);
    Ini.UpdateFile;
  finally
    Ini.Free;
  end;
end;

end.
