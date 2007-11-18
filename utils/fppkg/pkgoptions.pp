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
unit pkgoptions;

interface

uses Classes, Sysutils, Inifiles, fprepos;

Const
  DefaultManifestFile      = 'manifest.xml';
  CurrentConfigVersion     = '0.1';

Type

  { TGlobalOptions }

  TGlobalOptions = Class(TPersistent)
  private
    FDirty : Boolean;
    FConfigVersion,
    FRemoteMirrorsLocation,
    FLocalMirrorsLocation,
    FRemoteRepository,
    FLocalRepository,
    FCompilerConfigDir,
    FPackagesDir,
    FBuildDir,
    FDefaultVerbosity,
    FDownloader,
    FDefaultCompilerConfig,
    FFPMakeCompilerConfig : String;
    // Parameter options
    FCompilerConfig : String;
    FInstallGlobal  : Boolean;
    function  GetOptString(Index: integer): String;
    procedure SetOptString(Index: integer; const AValue: String);
  Public
    Constructor Create;
    Procedure InitGlobalDefaults;
    Procedure LoadGlobalFromIni(Ini : TCustomIniFile); virtual;
    Procedure SaveGlobalToIni(Ini : TCustomIniFile); virtual;
    Procedure LoadGlobalFromFile(const AFileName : String);
    Procedure SaveGlobalToFile(const AFileName : String);
    Property Dirty : Boolean Read FDirty;
    Property ConfigVersion : String read FConfigVersion;
    function RemotePackagesFile:string;
    function LocalPackagesFile:string;
    function LocalVersionsFile(const ACompilerConfig:String):string;
  Published
    Property RemoteMirrorsLocation : String Index 0 Read GetOptString Write SetOptString;
    Property LocalMirrorsLocation : String Index 1 Read GetOptString Write SetOptString;
    Property RemoteRepository : String Index 2 Read GetOptString Write SetOptString;
    Property LocalRepository : String Index 3 Read GetOptString Write SetOptString;
    Property BuildDir : String Index 4 Read GetOptString Write SetOptString;
    Property PackagesDir : String Index 5 Read GetOptString Write SetOptString;
    Property CompilerConfigDir : String Index 6 Read GetOptString Write SetOptString;
    Property DefaultVerbosity : String Index 7 Read GetOptString Write SetOptString;
    Property DefaultCompilerConfig : String Index 8 Read GetOptString Write SetOptString;
    Property FPMakeCompilerConfig : String Index 9 Read GetOptString Write SetOptString;
    Property Downloader: String Index 10 Read GetOptString Write SetOptString;
    // Parameters
    Property CompilerConfig : String Read FCompilerConfig Write FCompilerConfig;
    Property InstallGlobal : Boolean Read FInstallGlobal Write FInstallGlobal;
  end;


  { TCompilerOptions }

  TCompilerOptions = Class(TPersistent)
  private
    FDirty: Boolean;
    FConfigVersion,
    FCompiler,
    FCompilerVersion,
    FLocalInstallDir,
    FGlobalInstallDir : String;
    FCompilerCPU: TCPU;
    FCompilerOS: TOS;
    function GetOptString(Index: integer): String;
    procedure SetOptString(Index: integer; const AValue: String);
    procedure SetCompilerCPU(const AValue: TCPU);
    procedure SetCompilerOS(const AValue: TOS);
  Public
    Constructor Create;
    Procedure InitCompilerDefaults;
    Procedure LoadCompilerFromIni(Ini : TCustomIniFile); virtual;
    Procedure SaveCompilerToIni(Ini : TCustomIniFile); virtual;
    Procedure LoadCompilerFromFile(const AFileName : String);
    Procedure SaveCompilerToFile(const AFileName : String);
    Property Dirty : Boolean Read FDirty;
    Property ConfigVersion : String read FConfigVersion;
    Function LocalUnitDir:string;
    Function GlobalUnitDir:string;
  Published
    Property Compiler : String Index 1 Read GetOptString Write SetOptString;
    Property CompilerTarget : String Index 2 Read GetOptString Write SetOptString;
    Property CompilerVersion : String Index 3 Read GetOptString Write SetOptString;
    Property GlobalInstallDir : String Index 4 Read GetOptString Write SetOptString;
    Property LocalInstallDir : String Index 5 Read GetOptString Write SetOptString;
    Property CompilerOS : TOS Read FCompilerOS Write SetCompilerOS;
    Property CompilerCPU : TCPU Read FCompilerCPU Write SetCompilerCPU;
  end;

var
  GlobalOptions : TGlobalOptions;
  CompilerOptions : TCompilerOptions;
  FPMakeCompilerOptions : TCompilerOptions;


Implementation

uses
  pkgglobals,
  pkgmessages;

Const
  DefaultMirrorFile       = 'mirrors.xml';
  DefaultPackagesFile     = 'packages.xml';
  DefaultVersionsFile     = 'versions-%s.dat';
  DefaultMirrorsLocation  = 'http://www.freepascal.org/repository/'+DefaultMirrorFile;
{$warning TODO use real repository}
{$ifdef localrepository}
  DefaultRemoteRepository = 'file://'+{$I %HOME%}+'/repository/';
{$else}
  DefaultRemoteRepository = 'http://www.freepascal.org/~peter/repository/';
{$endif}

  // ini file keys
  SDefaults = 'Defaults';

  // All configs
  KeyConfigVersion         = 'ConfigVersion';

  // Global config
  KeyLocalMirrorsLocation  = 'LocalMirrors';
  KeyRemoteMirrorsLocation = 'RemoteMirrors';
  KeyRemoteRepository      = 'RemoteRepository';
  KeyLocalRepository       = 'LocalRepository';
  KeyPackagesDir           = 'PackagesDir';
  KeyBuildDir              = 'BuildDir';
  KeyCompilerConfigDir     = 'CompilerConfigDir';
  KeyVerbosity             = 'Verbosity';
  KeyCompilerConfig        = 'CompilerConfig';
  KeyFPMakeCompilerConfig  = 'FPMakeCompilerConfig';
  KeyDownloader            = 'Downloader';

  // Compiler dependent config
  KeyGlobalInstallDir      = 'GlobalInstallDir';
  KeyLocalInstallDir       = 'LocalInstallDir';
  KeyCompiler              = 'Compiler' ;
  KeyCompilerOS            = 'OS';
  KeyCompilerCPU           = 'CPU';
  KeyCompilerVersion       = 'Version';


{*****************************************************************************
                           TGlobalOptions
*****************************************************************************}

constructor TGlobalOptions.Create;
begin
  InitGlobalDefaults;
end;


function TGlobalOptions.GetOptString(Index: integer): String;
begin
  Case Index of
    0 : Result:=FRemoteMirrorsLocation;
    1 : Result:=FLocalMirrorsLocation;
    2 : Result:=FRemoteRepository;
    3 : Result:=FLocalRepository;
    4 : Result:=FBuildDir;
    5 : Result:=FPackagesDir;
    6 : Result:=FCompilerConfigDir;
    7 : Result:=FDefaultVerbosity;
    8 : Result:=FDefaultCompilerConfig;
    9 : Result:=FFPMakeCompilerConfig;
   10 : Result:=FDownloader;
    else
      Error('Unknown option');
  end;
end;

procedure TGlobalOptions.SetOptString(Index: integer; const AValue: String);
begin
  If AValue=GetOptString(Index) then
    Exit;
  Case Index of
    0 : FLocalMirrorsLocation:=AValue;
    1 : FRemoteMirrorsLocation:=AValue;
    2 : FRemoteRepository:=AValue;
    3 : FLocalRepository:=AValue;
    4 : FBuildDir:=FixPath(AValue);
    5 : FPackagesDir:=FixPath(AValue);
    6 : FCompilerConfigDir:=FixPath(AValue);
    7 : FDefaultVerbosity:=AValue;
    8 : FDefaultCompilerConfig:=AValue;
    9 : FFPMakeCompilerConfig:=AValue;
   10 : FDownloader:=AValue;
    else
      Error('Unknown option');
  end;
  FDirty:=True;
end;


function TGlobalOptions.RemotePackagesFile:string;
begin
  Result:=FRemoteRepository+DefaultPackagesFile;
end;


function TGlobalOptions.LocalPackagesFile:string;
begin
  Result:=FLocalRepository+DefaultPackagesFile;
end;


function TGlobalOptions.LocalVersionsFile(const ACompilerConfig:String):string;
begin
  Result:=FLocalRepository+Format(DefaultVersionsFile,[ACompilerConfig]);
end;


Procedure TGlobalOptions.InitGlobalDefaults;
var
  LocalDir : String;
begin
  FConfigVersion:=CurrentConfigVersion;
  // Retrieve Local fppkg directory
{$ifdef unix}
  if IsSuperUser then
    begin
      if DirectoryExists('/usr/local/lib/fpc') then
        LocalDir:='/usr/local/lib/fpc/fppkg/'
      else
        LocalDir:='/usr/lib/fpc/fppkg/';
    end
  else
    LocalDir:=IncludeTrailingPathDelimiter(GetEnvironmentVariable('HOME'))+'.fppkg/';
{$else}
  // Change as needed on all OS-es...
  LocalDir:=ExtractFilePath(Paramstr(0))+'fppkg'+PathDelim;
{$endif}
  // Directories
  FBuildDir:=LocalDir+'build'+PathDelim;
  FPackagesDir:=LocalDir+'packages'+PathDelim;
  FCompilerConfigDir:=LocalDir+'config'+PathDelim;
  FLocalMirrorsLocation:=LocalDir+DefaultMirrorFile;
  FLocalRepository:=LocalDir;
  // Remote
  FRemoteMirrorsLocation:=DefaultMirrorsLocation;
  FRemoteRepository:=DefaultRemoteRepository;
  // Other config
  FDefaultVerbosity:='error,warning,info,debug,commands';
  FDefaultCompilerConfig:='default';
  FFPMakeCompilerConfig:='default';
  // Downloader
{$if defined(unix) or defined(windows)}
  FDownloader:='lnet';
{$else}
  FDownloader:='base';
{$endif}
  // Parameter defaults
  FCompilerConfig:=FDefaultCompilerConfig;
  FInstallGlobal:=False;
end;


procedure TGlobalOptions.LoadGlobalFromIni(Ini: TCustomIniFile);
begin
 With Ini do
   begin
     FConfigVersion:=ReadString(SDefaults,KeyConfigVersion,'');
     if FConfigVersion<>CurrentConfigVersion then
       Error('Old configuration found, please delete manual');
     FLocalMirrorsLocation:=ReadString(SDefaults,KeyLocalMirrorsLocation,FLocalMirrorsLocation);
     FRemoteMirrorsLocation:=ReadString(SDefaults,KeyRemoteMirrorsLocation,FRemoteMirrorsLocation);
     FRemoteRepository:=ReadString(SDefaults,KeyRemoteRepository,FRemoteRepository);
     FLocalRepository:=ReadString(SDefaults,KeyLocalRepository,FLocalRepository);
     FBuildDir:=FixPath(ReadString(SDefaults,KeyBuildDir,FBuildDir));
     FPackagesDir:=FixPath(ReadString(SDefaults,KeyPackagesDir,FPackagesDir));
     FCompilerConfigDir:=FixPath(ReadString(SDefaults,KeyCompilerConfigDir,FCompilerConfigDir));
     FDefaultVerbosity:=ReadString(SDefaults,KeyVerbosity,FDefaultVerbosity);
     FDefaultCompilerConfig:=ReadString(SDefaults,KeyCompilerConfig,FDefaultCompilerConfig);
     FFPMakeCompilerConfig:=ReadString(SDefaults,KeyFPMakeCompilerConfig,FFPMakeCompilerConfig);
     FDownloader:=ReadString(SDefaults,KeyDownloader,FDownloader);
   end;
end;


procedure TGlobalOptions.SaveGlobalToIni(Ini: TCustomIniFile);
begin
 With Ini do
   begin
     WriteString(SDefaults,KeyConfigVersion,FConfigVersion);
     WriteString(SDefaults,KeyBuildDir,FBuildDir);
     WriteString(SDefaults,KeyPackagesDir,FPackagesDir);
     WriteString(SDefaults,KeyCompilerConfigDir,FCompilerConfigDir);
     WriteString(SDefaults,KeyLocalRepository,FLocalRepository);
     WriteString(SDefaults,KeyLocalMirrorsLocation,FLocalMirrorsLocation);
     WriteString(SDefaults,KeyRemoteMirrorsLocation,FRemoteMirrorsLocation);
     WriteString(SDefaults,KeyRemoteRepository,FRemoteRepository);
     WriteString(SDefaults,KeyVerbosity,FDefaultVerbosity);
     WriteString(SDefaults,KeyCompilerConfig,FDefaultCompilerConfig);
     WriteString(SDefaults,KeyFPMakeCompilerConfig,FFPMakeCompilerConfig);
     WriteString(SDefaults,KeyDownloader,FDownloader);
   end;
end;


procedure TGlobalOptions.LoadGlobalFromFile(const AFileName: String);
Var
  Ini : TMemIniFile;
begin
  Ini:=TMemIniFile.Create(AFileName);
  try
    LoadGlobalFromIni(Ini);
  finally
    Ini.Free;
  end;
end;


procedure TGlobalOptions.SaveGlobalToFile(const AFileName: String);
Var
  Ini : TIniFile;
begin
  if FileExists(AFileName) then
    BackupFile(AFileName);
  Ini:=TIniFile.Create(AFileName);
  try
    SaveGlobalToIni(Ini);
    Ini.UpdateFile;
  finally
    Ini.Free;
  end;
end;


{*****************************************************************************
                           TCompilerOptions
*****************************************************************************}

constructor TCompilerOptions.Create;
begin
end;


function TCompilerOptions.GetOptString(Index: integer): String;
begin
  Case Index of
    1 : Result:=FCompiler;
    2 : Result:=MakeTargetString(CompilerCPU,CompilerOS);
    3 : Result:=FCompilerVersion;
    4 : Result:=FGlobalInstallDir;
    5 : Result:=FLocalInstallDir;
    else
      Error('Unknown option');
  end;
end;

procedure TCompilerOptions.SetOptString(Index: integer; const AValue: String);
begin
  If AValue=GetOptString(Index) then
    Exit;
  Case Index of
    1 : FCompiler:=AValue;
    2 : StringToCPUOS(AValue,FCompilerCPU,FCompilerOS);
    3 : FCompilerVersion:=AValue;
    4 : FGlobalInstallDir:=FixPath(AValue);
    5 : FLocalInstallDir:=FixPath(AValue);
    else
      Error('Unknown option');
  end;
  FDirty:=True;
end;


procedure TCompilerOptions.SetCompilerCPU(const AValue: TCPU);
begin
  if FCompilerCPU=AValue then
    exit;
  FCompilerCPU:=AValue;
  FDirty:=True;
end;


procedure TCompilerOptions.SetCompilerOS(const AValue: TOS);
begin
  if FCompilerOS=AValue then
    exit;
  FCompilerOS:=AValue;
  FDirty:=True;
end;


function TCompilerOptions.LocalUnitDir:string;
begin
  if FLocalInstallDir<>'' then
    result:=FLocalInstallDir+'units'+PathDelim+CompilerTarget+PathDelim
  else
    result:='';
end;


function TCompilerOptions.GlobalUnitDir:string;
begin
  if FGlobalInstallDir<>'' then
    result:=FGlobalInstallDir+'units'+PathDelim+CompilerTarget+PathDelim
  else
    result:='';
end;


procedure TCompilerOptions.InitCompilerDefaults;
var
  infoSL : TStringList;
begin
  FConfigVersion:=CurrentConfigVersion;
  FCompiler:=FileSearch('fpc'+ExeExt,GetEnvironmentVariable('PATH'));
  if FCompiler='' then
    Raise EPackagerError.Create(SErrMissingFPC);
  // Detect compiler version/target from -i option
  infosl:=TStringList.Create;
  infosl.Delimiter:=' ';
  infosl.DelimitedText:=GetCompilerInfo(FCompiler,'-iVTPTO');
  if infosl.Count<>3 then
    Raise EPackagerError.Create(SErrInvalidFPCInfo);
  FCompilerVersion:=infosl[0];
  FCompilerCPU:=StringToCPU(infosl[1]);
  FCompilerOS:=StringToOS(infosl[2]);
  Log(vDebug,SLogDetectedCompiler,[FCompiler,FCompilerVersion,MakeTargetString(FCompilerCPU,FCompilerOS)]);
  // Use the same algorithm as the compiler, see options.pas
{$ifdef Unix}
  FGlobalInstallDir:=FixPath(GetEnvironmentVariable('FPCDIR'));
  if FGlobalInstallDir='' then
    begin
      FGlobalInstallDir:='/usr/local/lib/fpc/'+FCompilerVersion+'/';
      if not DirectoryExists(FGlobalInstallDir) and
         DirectoryExists('/usr/lib/fpc/'+FCompilerVersion) then
        FGlobalInstallDir:='/usr/lib/fpc/'+FCompilerVersion+'/';
    end;
{$else unix}
  FGlobalInstallDir:=FixPath(GetEnvironmentVariable('FPCDIR'));
  if FGlobalInstallDir='' then
    begin
      FGlobalInstallDir:=ExtractFilePath(FCompiler)+'../';
      if not(DirectoryExists(FGlobalInstallDir+'/units')) and
         not(DirectoryExists(FGlobalInstallDir+'/rtl')) then
        FGlobalInstallDir:=FGlobalInstallDir+'../';
    end;
{$endif unix}
  Log(vDebug,SLogDetectedFPCDIR,['global',FGlobalInstallDir]);
  // User writable install directory
  if not IsSuperUser then
    begin
      FLocalInstallDir:=GlobalOptions.LocalRepository+'lib'+PathDelim+FCompilerVersion+PathDelim;
      Log(vDebug,SLogDetectedFPCDIR,['local',FLocalInstallDir]);
    end;
 end;


procedure TCompilerOptions.LoadCompilerFromIni(Ini: TCustomIniFile);
begin
 With Ini do
   begin
     FConfigVersion:=ReadString(SDefaults,KeyConfigVersion,'');
     if FConfigVersion<>CurrentConfigVersion then
       Error('Old configuration found, please delete manual');
     FGlobalInstallDir:=FixPath(ReadString(SDefaults,KeyGlobalInstallDir,FGlobalInstallDir));
     FLocalInstallDir:=FixPath(ReadString(SDefaults,KeyLocalInstallDir,FLocalInstallDir));
     FCompiler:=ReadString(SDefaults,KeyCompiler,FCompiler);
     FCompilerOS:=StringToOS(ReadString(SDefaults,KeyCompilerOS,OSToString(CompilerOS)));
     FCompilerCPU:=StringToCPU(ReadString(SDefaults,KeyCompilerCPU,CPUtoString(CompilerCPU)));
     FCompilerVersion:=ReadString(SDefaults,KeyCompilerVersion,FCompilerVersion);
   end;
end;


procedure TCompilerOptions.SaveCompilerToIni(Ini: TCustomIniFile);
begin
 With Ini do
   begin
     WriteString(SDefaults,KeyConfigVersion,FConfigVersion);
     WriteString(SDefaults,KeyGlobalInstallDir,FGlobalInstallDir);
     WriteString(SDefaults,KeyLocalInstallDir,FLocalInstallDir);
     WriteString(SDefaults,KeyCompiler,FCompiler);
     WriteString(SDefaults,KeyCompilerOS,OSToString(CompilerOS));
     WriteString(SDefaults,KeyCompilerCPU,CPUtoString(CompilerCPU));
     WriteString(SDefaults,KeyCompilerVersion,FCompilerVersion);
   end;
end;


procedure TCompilerOptions.LoadCompilerFromFile(const AFileName: String);
Var
  Ini : TMemIniFile;
begin
  Ini:=TMemIniFile.Create(AFileName);
  try
    LoadCompilerFromIni(Ini);
  finally
    Ini.Free;
  end;
end;


procedure TCompilerOptions.SaveCompilerToFile(const AFileName: String);
Var
  Ini : TIniFile;
begin
  if FileExists(AFileName) then
    BackupFile(AFileName);
  Ini:=TIniFile.Create(AFileName);
  try
    SaveCompilerToIni(Ini);
    Ini.UpdateFile;
  finally
    Ini.Free;
  end;
end;


initialization
  GlobalOptions:=TGlobalOptions.Create;
  CompilerOptions:=TCompilerOptions.Create;
  FPMakeCompilerOptions:=TCompilerOptions.Create;
finalization
  FreeAndNil(GlobalOptions);
  FreeAndNil(CompilerOptions);
  FreeAndNil(FPMakeCompilerOptions);
end.
