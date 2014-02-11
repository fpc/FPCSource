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

// pkgglobals must be AFTER fpmkunit
uses Classes, Sysutils, Inifiles, fprepos, fpTemplate, fpmkunit, pkgglobals;

Const
  UnitConfigFileName   = 'fpunits.cfg';
  ManifestFileName     = 'manifest.xml';
  MirrorsFileName      = 'mirrors.xml';
  PackagesFileName     = 'packages.xml';
  VersionsFileName     = 'versions-%s.dat';
  CurrentConfigVersion = 4;

Type

  { TGlobalOptions }

  TGlobalOptions = Class(TPersistent)
  private
    FConfigFilename: string;
    FSaveInifileChanges : Boolean;
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
    FOptionParser: TTemplateParser;
    FShowLocation: Boolean;
    FSkipConfigurationFiles: boolean;
    FSkipFixBrokenAfterInstall: boolean;
    FCustomFPMakeOptions : string;
    function  GetOptString(Index: integer): String;
    procedure SetOptString(Index: integer; const AValue: String);
    procedure UpdateLocalRepositoryOption;
  Public
    Constructor Create;
    destructor Destroy; override;
    Procedure InitGlobalDefaults;
    Procedure LoadGlobalFromFile(const AFileName : String);
    Procedure SaveGlobalToFile(const AFileName : String);
    procedure LogValues(ALogLevel: TLogLevel);
    // Is set when the inifile has an old version number (which is also the case when a new file is generated)
    Property SaveInifileChanges : Boolean Read FSaveInifileChanges;
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
    Property CustomFPMakeOptions: String Index 11 Read GetOptString Write SetOptString;
    // Parameters
    Property CompilerConfig : String Read FCompilerConfig Write FCompilerConfig;
    Property InstallGlobal : Boolean Read FInstallGlobal Write FInstallGlobal;
    Property RecoveryMode : Boolean Read FRecoveryMode Write FRecoveryMode;
    Property AllowBroken : Boolean Read FAllowBroken Write FAllowBroken;
    Property ShowLocation : Boolean Read FShowLocation Write FShowLocation;
    Property SkipConfigurationFiles: boolean read FSkipConfigurationFiles write FSkipConfigurationFiles;
    Property SkipFixBrokenAfterInstall: boolean read FSkipFixBrokenAfterInstall write FSkipFixBrokenAfterInstall;
  end;


  { TCompilerOptions }

  TCompilerOptions = Class(TPersistent)
  private
    FConfigFilename: string;
    FSaveInifileChanges: Boolean;
    FConfigVersion : Integer;
    FCompiler,
    FCompilerVersion,
    FLocalInstallDir,
    FGlobalInstallDir,
    FLocalPrefix,
    FGlobalPrefix: String;
    FCompilerCPU: TCPU;
    FCompilerOS: TOS;
    FOptionParser: TTemplateParser;
    FOptions: TStrings;
    function GetOptions: TStrings;
    function GetOptString(Index: integer): String;
    procedure SetOptString(Index: integer; const AValue: String);
    procedure SetCompilerCPU(const AValue: TCPU);
    procedure SetCompilerOS(const AValue: TOS);
  Public
    Constructor Create;
    Destructor Destroy; override;
    Procedure InitCompilerDefaults;
    Procedure LoadCompilerFromFile(const AFileName : String);
    Procedure SaveCompilerToFile(const AFileName : String);
    procedure LogValues(ALogLevel: TLogLevel; const ACfgName:string);
    procedure UpdateLocalRepositoryOption;
    procedure CheckCompilerValues;
    Function LocalUnitDir:string;
    Function GlobalUnitDir:string;
    Function HasOptions: boolean;
    // Is set when the inifile has an old version number (which is also the case when a new file is generated)
    Property SaveInifileChanges : Boolean Read FSaveInifileChanges;
    Property ConfigVersion : Integer read FConfigVersion;
  Published
    Property Compiler : String Index 1 Read GetOptString Write SetOptString;
    Property CompilerTarget : String Index 2 Read GetOptString Write SetOptString;
    Property CompilerVersion : String Index 3 Read GetOptString Write SetOptString;
    Property GlobalInstallDir : String Index 4 Read GetOptString Write SetOptString;
    Property LocalInstallDir : String Index 5 Read GetOptString Write SetOptString;
    Property GlobalPrefix : String Index 6 Read GetOptString Write SetOptString;
    Property LocalPrefix : String Index 7 Read GetOptString Write SetOptString;
    Property Options : TStrings read GetOptions;
    Property CompilerOS : TOS Read FCompilerOS Write SetCompilerOS;
    Property CompilerCPU : TCPU Read FCompilerCPU Write SetCompilerCPU;
  end;

var
  GlobalOptions : TGlobalOptions;
  CompilerOptions : TCompilerOptions;
  FPMakeCompilerOptions : TCompilerOptions;

procedure LoadGlobalDefaults(CfgFile: string);
procedure LoadCompilerDefaults;

Implementation

uses
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
  KeyCustomFPMakeOptions   = 'FPMakeOptions';

  // Compiler dependent config
  KeyGlobalPrefix          = 'GlobalPrefix';
  KeyLocalPrefix           = 'LocalPrefix';
  KeyGlobalInstallDir      = 'GlobalInstallDir';
  KeyLocalInstallDir       = 'LocalInstallDir';
  KeyCompiler              = 'Compiler' ;
  KeyCompilerOS            = 'OS';
  KeyCompilerCPU           = 'CPU';
  KeyCompilerVersion       = 'Version';


procedure LoadGlobalDefaults(CfgFile: string);
var
  i : integer;
  GeneratedConfig,
  UseGlobalConfig : boolean;
begin
  GeneratedConfig:=false;
  UseGlobalConfig:=false;
  // First try specified config file
  if (CfgFile<>'') then
    begin
      if not FileExists(cfgfile) then
        Error(SErrNoSuchFile,[cfgfile]);
    end
  else
    begin
      // Now try if a local config-file exists
      cfgfile:=GetAppConfigFile(False,False);
      if not FileExists(cfgfile) then
        begin
          // If not, try to find a global configuration file
          cfgfile:=GetAppConfigFile(True,False);
          if FileExists(cfgfile) then
            UseGlobalConfig := true
          else
            begin
              // Create a new configuration file
              if not IsSuperUser then // Make a local, not global, configuration file
                cfgfile:=GetAppConfigFile(False,False);
              ForceDirectories(ExtractFilePath(cfgfile));
              GlobalOptions.SaveGlobalToFile(cfgfile);
              GeneratedConfig:=true;
            end;
        end;
    end;
  // Load file or create new default configuration
  if not GeneratedConfig then
    begin
      GlobalOptions.LoadGlobalFromFile(cfgfile);
      if GlobalOptions.SaveInifileChanges and (not UseGlobalConfig or IsSuperUser) then
        GlobalOptions.SaveGlobalToFile(cfgfile);
    end;
  GlobalOptions.CompilerConfig:=GlobalOptions.DefaultCompilerConfig;
  // Tracing of what we've done above, need to be done after the verbosity is set
  if GeneratedConfig then
    pkgglobals.Log(llDebug,SLogGeneratingGlobalConfig,[cfgfile])
  else
    pkgglobals.Log(llDebug,SLogLoadingGlobalConfig,[cfgfile]);
  // Log configuration
  GlobalOptions.LogValues(llDebug);
end;


procedure LoadCompilerDefaults;
var
  S : String;
begin
  // Load default compiler config
  S:=GlobalOptions.CompilerConfigDir+GlobalOptions.CompilerConfig;
  CompilerOptions.UpdateLocalRepositoryOption;
  if FileExists(S) then
    begin
      pkgglobals.Log(llDebug,SLogLoadingCompilerConfig,[S]);
      CompilerOptions.LoadCompilerFromFile(S)
    end
  else
    begin
      // Generate a default configuration if it doesn't exists
      if GlobalOptions.CompilerConfig='default' then
        begin
          pkgglobals.Log(llDebug,SLogGeneratingCompilerConfig,[S]);
          CompilerOptions.InitCompilerDefaults;
          CompilerOptions.SaveCompilerToFile(S);
          if CompilerOptions.SaveInifileChanges then
            CompilerOptions.SaveCompilerToFile(S);
        end
      else
        Error(SErrMissingCompilerConfig,[S]);
    end;
  // Log compiler configuration
  CompilerOptions.LogValues(llDebug,'');
  // Load FPMake compiler config, this is normally the same config as above
  S:=GlobalOptions.CompilerConfigDir+GlobalOptions.FPMakeCompilerConfig;
  FPMakeCompilerOptions.UpdateLocalRepositoryOption;
  if FileExists(S) then
    begin
      pkgglobals.Log(llDebug,SLogLoadingFPMakeCompilerConfig,[S]);
      FPMakeCompilerOptions.LoadCompilerFromFile(S);
      if FPMakeCompilerOptions.SaveInifileChanges then
        FPMakeCompilerOptions.SaveCompilerToFile(S);
    end
  else
    Error(SErrMissingCompilerConfig,[S]);
  // Log compiler configuration
  FPMakeCompilerOptions.LogValues(llDebug,'fpmake-building ');
end;


{*****************************************************************************
                           TGlobalOptions
*****************************************************************************}

constructor TGlobalOptions.Create;
begin
  FOptionParser := TTemplateParser.Create;
  FOptionParser.Values['AppConfigDir'] := GetAppConfigDir(false);
  FOptionParser.Values['UserDir'] := GetUserDir;
  InitGlobalDefaults;
end;

destructor TGlobalOptions.Destroy;
begin
  FOptionParser.Free;
  inherited Destroy;
end;


function TGlobalOptions.GetOptString(Index: integer): String;
begin
  Case Index of
    0 : Result:=FRemoteMirrorsURL;
    2 : Result:=FRemoteRepository;
    3 : Result:=FOptionParser.ParseString(FLocalRepository);
    4 : Result:=FOptionParser.ParseString(FBuildDir);
    5 : Result:=FOptionParser.ParseString(FArchivesDir);
    6 : Result:=FOptionParser.ParseString(FCompilerConfigDir);
    8 : Result:=FDefaultCompilerConfig;
    9 : Result:=FFPMakeCompilerConfig;
   10 : Result:=FDownloader;
   11 : Result:=FCustomFPMakeOptions;
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
    3 : begin
          FLocalRepository:=AValue;
          UpdateLocalRepositoryOption;
        end;
    4 : FBuildDir:=fpmkunit.FixPath(AValue, True);
    5 : FArchivesDir:=fpmkunit.FixPath(AValue, True);
    6 : FCompilerConfigDir:=fpmkunit.FixPath(AValue, True);
    8 : FDefaultCompilerConfig:=AValue;
    9 : FFPMakeCompilerConfig:=AValue;
    10 : FDownloader:=AValue;
    11 : FCustomFPMakeOptions:=AValue;
    else
      Error('Unknown option');
  end;
end;


procedure TGlobalOptions.UpdateLocalRepositoryOption;
begin
  FOptionParser.Values['LocalRepository'] := LocalRepository;
end;


function TGlobalOptions.LocalPackagesFile:string;
begin
  Result:=LocalRepository+PackagesFileName;
end;


function TGlobalOptions.LocalMirrorsFile:string;
begin
  Result:=LocalRepository+MirrorsFileName;
end;


function TGlobalOptions.LocalVersionsFile(const ACompilerConfig:String):string;
begin
  Result:=LocalRepository+Format(VersionsFileName,[ACompilerConfig]);
end;


Procedure TGlobalOptions.InitGlobalDefaults;
var
  i: Integer;
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
    FLocalRepository:='{UserDir}.fppkg/';
{$else}
  if IsSuperUser then
    FLocalRepository:=IncludeTrailingPathDelimiter(GetAppConfigDir(true))
  else
    FLocalRepository:='{AppConfigDir}';
{$endif}
  UpdateLocalRepositoryOption;
  // Directories
  FBuildDir:='{LocalRepository}build'+PathDelim;
  FArchivesDir:='{LocalRepository}archives'+PathDelim;
  FCompilerConfigDir:='{LocalRepository}config'+PathDelim;
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

  SetLength(FPMKUnitDeps,FPMKUnitDepDefaultCount);
  for i := 0 to FPMKUnitDepDefaultCount-1 do
    FPMKUnitDeps[i]:=FPMKUnitDepsDefaults[i];
end;


procedure TGlobalOptions.LoadGlobalFromFile(const AFileName: String);
Var
  Ini : TMemIniFile;
begin
  Ini:=TMemIniFile.Create(AFileName);
  try
    FConfigFileName:=AFileName;
    With Ini do
      begin
        FConfigVersion:=ReadInteger(SDefaults,KeyConfigVersion,0);
        if (FConfigVersion<>CurrentConfigVersion) then
          begin
            log(llDebug,SLogUpgradingConfig,[AFileName]);
            FSaveInifileChanges:=true;
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
        UpdateLocalRepositoryOption;
        FBuildDir:=fpmkunit.FixPath(ReadString(SDefaults,KeyBuildDir,FBuildDir), True);
        FArchivesDir:=fpmkunit.FixPath(ReadString(SDefaults,KeyArchivesDir,FArchivesDir), True);
        FCompilerConfigDir:=fpmkunit.FixPath(ReadString(SDefaults,KeyCompilerConfigDir,FCompilerConfigDir), True);
        FDefaultCompilerConfig:=ReadString(SDefaults,KeyCompilerConfig,FDefaultCompilerConfig);
        FFPMakeCompilerConfig:=ReadString(SDefaults,KeyFPMakeCompilerConfig,FFPMakeCompilerConfig);
        FDownloader:=ReadString(SDefaults,KeyDownloader,FDownloader);
        FCustomFPMakeOptions:=ReadString(SDefaults,KeyCustomFPMakeOptions,FCustomFPMakeOptions);
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
  Ini:=TIniFile.Create(AFileName);
  try
    With Ini do
      begin
        WriteInteger(SDefaults,KeyConfigVersion,CurrentConfigVersion);
        WriteString(SDefaults,KeyLocalRepository,FLocalRepository);
        WriteString(SDefaults,KeyBuildDir,FBuildDir);
        WriteString(SDefaults,KeyArchivesDir,FArchivesDir);
        WriteString(SDefaults,KeyCompilerConfigDir,FCompilerConfigDir);
        WriteString(SDefaults,KeyRemoteMirrorsURL,FRemoteMirrorsURL);
        WriteString(SDefaults,KeyRemoteRepository,FRemoteRepository);
        WriteString(SDefaults,KeyCompilerConfig,FDefaultCompilerConfig);
        WriteString(SDefaults,KeyFPMakeCompilerConfig,FFPMakeCompilerConfig);
        WriteString(SDefaults,KeyDownloader,FDownloader);
        FSaveInifileChanges:=False;
      end;
    Ini.UpdateFile;
  finally
    Ini.Free;
  end;
end;


procedure TGlobalOptions.LogValues(ALogLevel: TLogLevel);
begin
  log(ALogLevel,SLogGlobalCfgHeader,[FConfigFilename]);
  log(ALogLevel,SLogGlobalCfgRemoteMirrorsURL,[FRemoteMirrorsURL]);
  log(ALogLevel,SLogGlobalCfgRemoteRepository,[FRemoteRepository]);
  log(ALogLevel,SLogGlobalCfgLocalRepository,[FLocalRepository,LocalRepository]);
  log(ALogLevel,SLogGlobalCfgBuildDir,[FBuildDir,BuildDir]);
  log(ALogLevel,SLogGlobalCfgArchivesDir,[FArchivesDir,ArchivesDir]);
  log(ALogLevel,SLogGlobalCfgCompilerConfigDir,[FCompilerConfigDir,CompilerConfigDir]);
  log(ALogLevel,SLogGlobalCfgDefaultCompilerConfig,[FDefaultCompilerConfig]);
  log(ALogLevel,SLogGlobalCfgFPMakeCompilerConfig,[FPMakeCompilerConfig]);
  log(ALogLevel,SLogGlobalCfgDownloader,[FDownloader]);
end;


{*****************************************************************************
                           TCompilerOptions
*****************************************************************************}

constructor TCompilerOptions.Create;
begin
  FOptionParser := TTemplateParser.Create;
  FOptionParser.Values['AppConfigDir'] := GetAppConfigDir(false);
  FOptionParser.Values['UserDir'] := GetUserDir;
  {$ifdef unix}
  FLocalInstallDir:='{LocalPrefix}'+'lib'+PathDelim+'fpc'+PathDelim+'{CompilerVersion}'+PathDelim;
  FGlobalInstallDir:='{GlobalPrefix}'+'lib'+PathDelim+'fpc'+PathDelim+'{CompilerVersion}'+PathDelim;
  {$else unix}
  FLocalInstallDir:='{LocalPrefix}';
  FGlobalInstallDir:='{GlobalPrefix}';
  {$endif}
end;

destructor TCompilerOptions.Destroy;
begin
  FOptionParser.Free;
  if assigned(FOptions) then
    FreeAndNil(FOptions);
  inherited Destroy;
end;


function TCompilerOptions.GetOptString(Index: integer): String;
begin
  Case Index of
    1 : Result:=FCompiler;
    2 : Result:=MakeTargetString(CompilerCPU,CompilerOS);
    3 : Result:=FCompilerVersion;
    4 : Result:=FOptionParser.ParseString(FGlobalInstallDir);
    5 : Result:=FOptionParser.ParseString(FLocalInstallDir);
    6 : Result:=fpmkunit.FixPath(FOptionParser.ParseString(FGlobalPrefix), True);
    7 : Result:=fpmkunit.FixPath(FOptionParser.ParseString(FLocalPrefix), True);
    else
      Error('Unknown option');
  end;
end;

function TCompilerOptions.GetOptions: TStrings;
begin
  if not assigned(FOptions) then
    begin
      FOptions := TStringList.Create;
      FOptions.Delimiter:=' ';
    end;
  Result := FOptions;
end;


procedure TCompilerOptions.SetOptString(Index: integer; const AValue: String);
begin
  If AValue=GetOptString(Index) then
    Exit;
  Case Index of
    1 : FCompiler:=AValue;
    2 : StringToCPUOS(AValue,FCompilerCPU,FCompilerOS);
    3 : begin
          FCompilerVersion:=AValue;
          FOptionParser.Values['CompilerVersion'] := FCompilerVersion;
        end;
    4 : FGlobalInstallDir:=fpmkunit.FixPath(AValue, True);
    5 : FLocalInstallDir:=fpmkunit.FixPath(AValue, True);
    6 : begin
          FGlobalPrefix:=AValue;
          FOptionParser.Values['GlobalPrefix'] := GlobalPrefix;
        end;
    7 : begin
          FLocalPrefix:=AValue;
          FOptionParser.Values['LocalPrefix'] := LocalPrefix;
        end
    else
      Error('Unknown option');
  end;
end;


procedure TCompilerOptions.SetCompilerCPU(const AValue: TCPU);
begin
  if FCompilerCPU=AValue then
    exit;
  FCompilerCPU:=AValue;
end;


procedure TCompilerOptions.UpdateLocalRepositoryOption;
begin
  FOptionParser.Values['LocalRepository'] := GlobalOptions.LocalRepository;
end;

procedure TCompilerOptions.CheckCompilerValues;
var
  AVersion : string;
  ACpu     : TCpu;
  AOs      : TOS;
begin
  if Compiler='' then
    Exit;
  if (CompilerCPU=cpuNone) or
   (CompilerOS=osNone) or
   (CompilerVersion='') then
  begin
    GetCompilerInfo(Compiler,'-iVTPTO',AVersion,ACpu,AOs);
    if CompilerCPU=cpuNone then
      CompilerCPU := ACpu;
    if CompilerOS=osNone then
      CompilerOS:=AOs;
    if CompilerVersion='' then
      CompilerVersion:=AVersion;
  end;
end;


procedure TCompilerOptions.SetCompilerOS(const AValue: TOS);
begin
  if FCompilerOS=AValue then
    exit;
  FCompilerOS:=AValue;
end;


function TCompilerOptions.LocalUnitDir:string;
var ALocalInstallDir: string;
begin
  ALocalInstallDir:=LocalInstallDir;

  if ALocalInstallDir<>'' then
    result:=ALocalInstallDir+'units'+PathDelim+CompilerTarget+PathDelim
  else
    result:='';
end;


function TCompilerOptions.GlobalUnitDir:string;
var AGlobalInstallDir: string;
begin
  AGlobalInstallDir:=GlobalInstallDir;

  if AGlobalInstallDir<>'' then
    result:=AGlobalInstallDir+'units'+PathDelim+CompilerTarget+PathDelim
  else
    result:='';
end;


function TCompilerOptions.HasOptions: boolean;
begin
  result := assigned(FOptions);
end;


procedure TCompilerOptions.InitCompilerDefaults;
var
  ACompilerVersion: string;
  fpcdir: string;
begin
  FConfigVersion:=CurrentConfigVersion;
  if fcompiler = '' then
    FCompiler:=ExeSearch('fpc'+ExeExt,GetEnvironmentVariable('PATH'));
  if FCompiler='' then
    Raise EPackagerError.Create(SErrMissingFPC);
  // Detect compiler version/target from -i option
  GetCompilerInfo(FCompiler,'-iVTPTO',ACompilerVersion,FCompilerCPU,FCompilerOS);
  CompilerVersion := ACompilerVersion;
  // Temporary hack to workaround bug in fpc.exe that doesn't support spaces
  // We retrieve the real binary
  if FCompilerVersion='2.2.0' then
    FCompiler:=GetCompilerInfo(FCompiler,'-PB');
  log(llDebug,SLogDetectedCompiler,[FCompiler,FCompilerVersion,MakeTargetString(FCompilerCPU,FCompilerOS)]);

  // Use the same algorithm as the compiler, see options.pas
  // Except that the prefix is extracted and GlobalInstallDir is set using
  // that prefix
{$ifdef Unix}
  FGlobalPrefix:='/usr/local/';
  if not DirectoryExists(FGlobalPrefix+'lib/fpc/'+FCompilerVersion+'/') and
     DirectoryExists('/usr/lib/fpc/'+FCompilerVersion+'/') then
    FGlobalPrefix:='/usr/';
{$else unix}
  FGlobalPrefix:=ExtractFilePath(FCompiler)+'..'+PathDelim;
  if not(DirectoryExists(FGlobalPrefix+PathDelim+'units')) and
     not(DirectoryExists(FGlobalPrefix+PathDelim+'rtl')) then
    FGlobalPrefix:=FGlobalPrefix+'..'+PathDelim;
  FGlobalPrefix:=ExpandFileName(FGlobalPrefix);
{$endif unix}

  log(llDebug,SLogDetectedPrefix,['global',FGlobalPrefix]);
  // User writable install directory
  if not IsSuperUser then
    begin
      FLocalPrefix:= '{LocalRepository}';
      log(llDebug,SLogDetectedPrefix,['local',FLocalPrefix]);
    end;

  fpcdir:=fpmkunit.FixPath(GetEnvironmentVariable('FPCDIR'), True);
  if fpcdir<>'' then
    begin
    {$ifndef Unix}
    fpcdir:=ExpandFileName(fpcdir);
    {$endif unix}
    log(llDebug,SLogFPCDirEnv,[fpcdir]);
    FGlobalInstallDir:=fpcdir;
    end;
end;


procedure TCompilerOptions.LoadCompilerFromFile(const AFileName: String);
Var
  Ini : TMemIniFile;
begin
  Ini:=TMemIniFile.Create(AFileName);
  try
    FConfigFilename:=AFileName;
    With Ini do
      begin
        FConfigVersion:=ReadInteger(SDefaults,KeyConfigVersion,0);
        if (FConfigVersion<>CurrentConfigVersion) then
          begin
            log(llDebug,SLogUpgradingConfig,[AFileName]);
            FSaveInifileChanges:=true;
            if (FConfigVersion>CurrentConfigVersion) then
              Error(SErrUnsupportedConfigVersion,[AFileName]);
          end;
        GlobalPrefix:=ReadString(SDefaults,KeyGlobalPrefix,FGlobalPrefix);
        LocalPrefix:=ReadString(SDefaults,KeyLocalPrefix,FLocalPrefix);
        FGlobalInstallDir:=fpmkunit.FixPath(ReadString(SDefaults,KeyGlobalInstallDir,FGlobalInstallDir), True);
        FLocalInstallDir:=fpmkunit.FixPath(ReadString(SDefaults,KeyLocalInstallDir,FLocalInstallDir), True);
        FCompiler:=ReadString(SDefaults,KeyCompiler,FCompiler);
        FCompilerOS:=StringToOS(ReadString(SDefaults,KeyCompilerOS,OSToString(CompilerOS)));
        FCompilerCPU:=StringToCPU(ReadString(SDefaults,KeyCompilerCPU,CPUtoString(CompilerCPU)));
        CompilerVersion:=ReadString(SDefaults,KeyCompilerVersion,FCompilerVersion);
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
  Ini:=TIniFile.Create(AFileName);
  try
    With Ini do
      begin
        WriteInteger(SDefaults,KeyConfigVersion,CurrentConfigVersion);
        WriteString(SDefaults,KeyGlobalPrefix,FGlobalPrefix);
        WriteString(SDefaults,KeyLocalPrefix,FLocalPrefix);
        WriteString(SDefaults,KeyGlobalInstallDir,FGlobalInstallDir);
        WriteString(SDefaults,KeyLocalInstallDir,FLocalInstallDir);
        WriteString(SDefaults,KeyCompiler,FCompiler);
        WriteString(SDefaults,KeyCompilerOS,OSToString(CompilerOS));
        WriteString(SDefaults,KeyCompilerCPU,CPUtoString(CompilerCPU));
        WriteString(SDefaults,KeyCompilerVersion,FCompilerVersion);
        FSaveInifileChanges:=False;
      end;
    Ini.UpdateFile;
  finally
    Ini.Free;
  end;
end;


procedure TCompilerOptions.LogValues(ALogLevel: TLogLevel; const ACfgName:string);
begin
  log(ALogLevel,SLogCompilerCfgHeader,[ACfgName,FConfigFilename]);
  log(ALogLevel,SLogCompilerCfgCompiler,[FCompiler]);
  log(ALogLevel,SLogCompilerCfgTarget,[MakeTargetString(CompilerCPU,CompilerOS)]);
  log(ALogLevel,SLogCompilerCfgVersion,[FCompilerVersion]);
  log(ALogLevel,SLogCompilerCfgGlobalPrefix,[FGlobalPrefix,GlobalPrefix]);
  log(ALogLevel,SLogCompilerCfgLocalPrefix,[FLocalPrefix,LocalPrefix]);
  log(ALogLevel,SLogCompilerCfgGlobalInstallDir,[FGlobalInstallDir,GlobalInstallDir]);
  log(ALogLevel,SLogCompilerCfgLocalInstallDir,[FLocalInstallDir,LocalInstallDir]);
  log(ALogLevel,SLogCompilerCfgOptions,[Options.DelimitedText]);
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
