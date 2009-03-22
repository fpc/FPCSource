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
  UnitConfigFileName   = 'fpunits.conf';
  ManifestFileName     = 'manifest.xml';
  MirrorsFileName      = 'mirrors.xml';
  PackagesFileName     = 'packages.xml';
  VersionsFileName     = 'versions-%s.dat';
  CurrentConfigVersion = 3;

Type

  { TGlobalOptions }

  TGlobalOptions = Class(TPersistent)
  private
    FDirty : Boolean;
    FConfigVersion : Integer;
    FRemoteMirrorsURL,
    FRemoteRepository,
    FLocalRepository,
    FCompilerConfigDir,
    FArchivesDir,
    FBuildDir,
    FDownloader,
    FDefaultCompilerConfig,
    FFPMakeCompilerConfig : String;
    // Parameter options
    FCompilerConfig : String;
    FAllowBroken,
    FInstallGlobal,
    FRecoveryMode   : Boolean;
    function  GetOptString(Index: integer): String;
    procedure SetOptString(Index: integer; const AValue: String);
  Public
    Constructor Create;
    Procedure InitGlobalDefaults;
    Procedure LoadGlobalFromFile(const AFileName : String);
    Procedure SaveGlobalToFile(const AFileName : String);
    procedure LogValues;
    Property Dirty : Boolean Read FDirty;
    Property ConfigVersion : Integer read FConfigVersion;
    function LocalPackagesFile:string;
    function LocalMirrorsFile:string;
    function LocalVersionsFile(const ACompilerConfig:String):string;
  Published
    Property RemoteMirrorsURL : String Index 0 Read GetOptString Write SetOptString;
    // 1 is unused
    Property RemoteRepository : String Index 2 Read GetOptString Write SetOptString;
    Property LocalRepository : String Index 3 Read GetOptString Write SetOptString;
    Property BuildDir : String Index 4 Read GetOptString Write SetOptString;
    Property ArchivesDir : String Index 5 Read GetOptString Write SetOptString;
    Property CompilerConfigDir : String Index 6 Read GetOptString Write SetOptString;
    Property DefaultCompilerConfig : String Index 8 Read GetOptString Write SetOptString;
    Property FPMakeCompilerConfig : String Index 9 Read GetOptString Write SetOptString;
    Property Downloader: String Index 10 Read GetOptString Write SetOptString;
    // Parameters
    Property CompilerConfig : String Read FCompilerConfig Write FCompilerConfig;
    Property InstallGlobal : Boolean Read FInstallGlobal Write FInstallGlobal;
    Property RecoveryMode : Boolean Read FRecoveryMode Write FRecoveryMode;
    Property AllowBroken : Boolean Read FAllowBroken Write FAllowBroken;
  end;


  { TCompilerOptions }

  TCompilerOptions = Class(TPersistent)
  private
    FDirty: Boolean;
    FConfigVersion : Integer;
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
    Procedure LoadCompilerFromFile(const AFileName : String);
    Procedure SaveCompilerToFile(const AFileName : String);
    procedure LogValues(const ACfgName:string);
    Property Dirty : Boolean Read FDirty;
    Property ConfigVersion : Integer read FConfigVersion;
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
  DefaultMirrorsURL  = 'http://www.freepascal.org/repository/'+MirrorsFileName;
{$ifdef localrepository}
  DefaultRemoteRepository = 'file://'+{$I %HOME%}+'/repository/';
{$else}
  DefaultRemoteRepository = 'auto';
{$endif}

  // ini file keys
  SDefaults = 'Defaults';

  // All configs
  KeyConfigVersion         = 'ConfigVersion';

  // Global config
  KeyRemoteMirrorsURL = 'RemoteMirrors';
  KeyRemoteRepository      = 'RemoteRepository';
  KeyLocalRepository       = 'LocalRepository';
  KeyArchivesDir           = 'ArchivesDir';
  KeyBuildDir              = 'BuildDir';
  KeyCompilerConfigDir     = 'CompilerConfigDir';
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
    0 : Result:=FRemoteMirrorsURL;
    2 : Result:=FRemoteRepository;
    3 : Result:=FLocalRepository;
    4 : Result:=FBuildDir;
    5 : Result:=FArchivesDir;
    6 : Result:=FCompilerConfigDir;
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
    1 : FRemoteMirrorsURL:=AValue;
    2 : FRemoteRepository:=AValue;
    3 : FLocalRepository:=AValue;
    4 : FBuildDir:=FixPath(AValue);
    5 : FArchivesDir:=FixPath(AValue);
    6 : FCompilerConfigDir:=FixPath(AValue);
    8 : FDefaultCompilerConfig:=AValue;
    9 : FFPMakeCompilerConfig:=AValue;
   10 : FDownloader:=AValue;
    else
      Error('Unknown option');
  end;
  FDirty:=True;
end;


function TGlobalOptions.LocalPackagesFile:string;
begin
  Result:=FLocalRepository+PackagesFileName;
end;


function TGlobalOptions.LocalMirrorsFile:string;
begin
  Result:=FLocalRepository+MirrorsFileName;
end;


function TGlobalOptions.LocalVersionsFile(const ACompilerConfig:String):string;
begin
  Result:=FLocalRepository+Format(VersionsFileName,[ACompilerConfig]);
end;


Procedure TGlobalOptions.InitGlobalDefaults;
begin
  FConfigVersion:=CurrentConfigVersion;
  // Retrieve Local fppkg directory
{$ifdef unix}
  if IsSuperUser then
    begin
      if DirectoryExists('/usr/local/lib/fpc') then
        FLocalRepository:='/usr/local/lib/fpc/fppkg/'
      else
        FLocalRepository:='/usr/lib/fpc/fppkg/';
    end
  else
    FLocalRepository:=IncludeTrailingPathDelimiter(GetEnvironmentVariable('HOME'))+'.fppkg/';
{$else}
  FLocalRepository:=IncludeTrailingPathDelimiter(GetAppConfigDir(IsSuperUser));
{$endif}
  // Directories
  FBuildDir:=FLocalRepository+'build'+PathDelim;
  FArchivesDir:=FLocalRepository+'archives'+PathDelim;
  FCompilerConfigDir:=FLocalRepository+'config'+PathDelim;
  // Remote
  FRemoteMirrorsURL:=DefaultMirrorsURL;
  FRemoteRepository:=DefaultRemoteRepository;
  // Other config
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
  FRecoveryMode:=False;
  FAllowBroken:=False;
end;


procedure TGlobalOptions.LoadGlobalFromFile(const AFileName: String);
Var
  Ini : TMemIniFile;
begin
  try
    Ini:=TMemIniFile.Create(AFileName);
    With Ini do
      begin
        FConfigVersion:=ReadInteger(SDefaults,KeyConfigVersion,0);
        if (FConfigVersion<>CurrentConfigVersion) then
          begin
            Log(vlDebug,SLogUpgradingConfig,[AFileName]);
            FDirty:=true;
            if FConfigVersion<1 then
              begin
                FRemoteRepository:='auto';
              end;
            if FConfigVersion<3 then
              begin
                // Directories
                FBuildDir:=FLocalRepository+'build'+PathDelim;
                FArchivesDir:=FLocalRepository+'archives'+PathDelim;
                FCompilerConfigDir:=FLocalRepository+'config'+PathDelim;
              end;
            if (FConfigVersion>CurrentConfigVersion) then
              Error(SErrUnsupportedConfigVersion,[AFileName]);
          end;
        FRemoteMirrorsURL:=ReadString(SDefaults,KeyRemoteMirrorsURL,FRemoteMirrorsURL);
        FRemoteRepository:=ReadString(SDefaults,KeyRemoteRepository,FRemoteRepository);
        FLocalRepository:=ReadString(SDefaults,KeyLocalRepository,FLocalRepository);
        FBuildDir:=FixPath(ReadString(SDefaults,KeyBuildDir,FBuildDir));
        FArchivesDir:=FixPath(ReadString(SDefaults,KeyArchivesDir,FArchivesDir));
        FCompilerConfigDir:=FixPath(ReadString(SDefaults,KeyCompilerConfigDir,FCompilerConfigDir));
        FDefaultCompilerConfig:=ReadString(SDefaults,KeyCompilerConfig,FDefaultCompilerConfig);
        FFPMakeCompilerConfig:=ReadString(SDefaults,KeyFPMakeCompilerConfig,FFPMakeCompilerConfig);
        FDownloader:=ReadString(SDefaults,KeyDownloader,FDownloader);
      end;
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
  try
    Ini:=TIniFile.Create(AFileName);
    With Ini do
      begin
        WriteInteger(SDefaults,KeyConfigVersion,CurrentConfigVersion);
        WriteString(SDefaults,KeyBuildDir,FBuildDir);
        WriteString(SDefaults,KeyArchivesDir,FArchivesDir);
        WriteString(SDefaults,KeyCompilerConfigDir,FCompilerConfigDir);
        WriteString(SDefaults,KeyLocalRepository,FLocalRepository);
        WriteString(SDefaults,KeyRemoteMirrorsURL,FRemoteMirrorsURL);
        WriteString(SDefaults,KeyRemoteRepository,FRemoteRepository);
        WriteString(SDefaults,KeyCompilerConfig,FDefaultCompilerConfig);
        WriteString(SDefaults,KeyFPMakeCompilerConfig,FFPMakeCompilerConfig);
        WriteString(SDefaults,KeyDownloader,FDownloader);
        FDirty:=False;
      end;
    Ini.UpdateFile;
  finally
    Ini.Free;
  end;
end;


procedure TGlobalOptions.LogValues;
begin
  Log(vlDebug,SLogGlobalCfgHeader);
  Log(vlDebug,SLogGlobalCfgRemoteMirrorsURL,[FRemoteMirrorsURL]);
  Log(vlDebug,SLogGlobalCfgRemoteRepository,[FRemoteRepository]);
  Log(vlDebug,SLogGlobalCfgLocalRepository,[FLocalRepository]);
  Log(vlDebug,SLogGlobalCfgBuildDir,[FBuildDir]);
  Log(vlDebug,SLogGlobalCfgArchivesDir,[FArchivesDir]);
  Log(vlDebug,SLogGlobalCfgCompilerConfigDir,[FCompilerConfigDir]);
  Log(vlDebug,SLogGlobalCfgDefaultCompilerConfig,[FDefaultCompilerConfig]);
  Log(vlDebug,SLogGlobalCfgFPMakeCompilerConfig,[FFPMakeCompilerConfig]);
  Log(vlDebug,SLogGlobalCfgDownloader,[FDownloader]);
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
  FCompiler:=ExeSearch('fpc'+ExeExt,GetEnvironmentVariable('PATH'));
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
  // Temporary hack to workaround bug in fpc.exe that doesn't support spaces
  // We retrieve the real binary
  if FCompilerVersion='2.2.0' then
    FCompiler:=GetCompilerInfo(FCompiler,'-PB');
  Log(vlDebug,SLogDetectedCompiler,[FCompiler,FCompilerVersion,MakeTargetString(FCompilerCPU,FCompilerOS)]);
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
  FGlobalInstallDir:=ExpandFileName(FGlobalInstallDir);
{$endif unix}
  Log(vlDebug,SLogDetectedFPCDIR,['global',FGlobalInstallDir]);
  // User writable install directory
  if not IsSuperUser then
    begin
      FLocalInstallDir:=GlobalOptions.LocalRepository+'lib'+PathDelim+FCompilerVersion+PathDelim;
      Log(vlDebug,SLogDetectedFPCDIR,['local',FLocalInstallDir]);
    end;
end;


procedure TCompilerOptions.LoadCompilerFromFile(const AFileName: String);
Var
  Ini : TMemIniFile;
begin
  try
    Ini:=TMemIniFile.Create(AFileName);
    With Ini do
      begin
        FConfigVersion:=ReadInteger(SDefaults,KeyConfigVersion,0);
        if (FConfigVersion<>CurrentConfigVersion) then
          begin
            Log(vlDebug,SLogUpgradingConfig,[AFileName]);
            FDirty:=true;
            if (FConfigVersion>CurrentConfigVersion) then
              Error(SErrUnsupportedConfigVersion,[AFileName]);
          end;
        FGlobalInstallDir:=FixPath(ReadString(SDefaults,KeyGlobalInstallDir,FGlobalInstallDir));
        FLocalInstallDir:=FixPath(ReadString(SDefaults,KeyLocalInstallDir,FLocalInstallDir));
        FCompiler:=ReadString(SDefaults,KeyCompiler,FCompiler);
        FCompilerOS:=StringToOS(ReadString(SDefaults,KeyCompilerOS,OSToString(CompilerOS)));
        FCompilerCPU:=StringToCPU(ReadString(SDefaults,KeyCompilerCPU,CPUtoString(CompilerCPU)));
        FCompilerVersion:=ReadString(SDefaults,KeyCompilerVersion,FCompilerVersion);
      end;
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
  try
    Ini:=TIniFile.Create(AFileName);
    With Ini do
      begin
        WriteInteger(SDefaults,KeyConfigVersion,CurrentConfigVersion);
        WriteString(SDefaults,KeyGlobalInstallDir,FGlobalInstallDir);
        WriteString(SDefaults,KeyLocalInstallDir,FLocalInstallDir);
        WriteString(SDefaults,KeyCompiler,FCompiler);
        WriteString(SDefaults,KeyCompilerOS,OSToString(CompilerOS));
        WriteString(SDefaults,KeyCompilerCPU,CPUtoString(CompilerCPU));
        WriteString(SDefaults,KeyCompilerVersion,FCompilerVersion);
        FDirty:=False;
      end;
    Ini.UpdateFile;
  finally
    Ini.Free;
  end;
end;


procedure TCompilerOptions.LogValues(const ACfgName:string);
begin
  Log(vlDebug,SLogCompilerCfgHeader,[ACfgName]);
  Log(vlDebug,SLogCompilerCfgCompiler,[FCompiler]);
  Log(vlDebug,SLogCompilerCfgTarget,[MakeTargetString(CompilerCPU,CompilerOS)]);
  Log(vlDebug,SLogCompilerCfgVersion,[FCompilerVersion]);
  Log(vlDebug,SLogCompilerCfgGlobalInstallDir,[FGlobalInstallDir]);
  Log(vlDebug,SLogCompilerCfgLocalInstallDir,[FLocalInstallDir]);
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
