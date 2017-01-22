unit pkgfpmake;

{$mode objfpc}{$H+}

interface

uses
  Classes,SysUtils,DateUtils,
  pkghandler, fpmkunit;

implementation

uses
  fprepos,
  pkgoptions,
  pkgglobals,
  pkgmessages,
  pkgrepos,
  pkgFppkg,
  fpxmlrep;

type
  { TFPMakeCompiler }

  TFPMakeCompiler = Class(TPackagehandler)
  Public
    Procedure Execute;override;
  end;


  { TFPMakeRunner }

  TFPMakeRunner = Class(TPackagehandler)
  Protected
    Function RunFPMake(const Command:string):Integer;
  end;


  { TFPMakeRunnerCompile }

  TFPMakeRunnerCompile = Class(TFPMakeRunner)
  Public
    Procedure Execute;override;
  end;


  { TFPMakeRunnerBuild }

  TFPMakeRunnerBuild = Class(TFPMakeRunner)
  Public
    Procedure Execute;override;
  end;


  { TFPMakeRunnerInstall }

  TFPMakeRunnerInstall = Class(TFPMakeRunner)
  Public
    Procedure Execute;override;
  end;

  { TFPMakeRunnerUnInstall }

  TFPMakeRunnerUnInstall = Class(TFPMakeRunner)
  Public
    Procedure Execute;override;
  end;

  { TFPMakeRunnerClean }

  TFPMakeRunnerClean = Class(TFPMakeRunner)
  Public
    Procedure Execute;override;
  end;

  { TFPMakeRunnerManifest }

  TFPMakeRunnerManifest = Class(TFPMakeRunner)
  Public
    Procedure Execute;override;
  end;

  { TFPMakeRunnerArchive }

  TFPMakeRunnerArchive = Class(TFPMakeRunner)
  Public
    Procedure Execute;override;
  end;

   TMyMemoryStream=class(TMemoryStream)
   public
     constructor Create(p:pointer;mysize:integer);
   end;

{
  Generated from fpmkunit.pp, using data2inc:
  data2inc -b -s fpmkunit.pp fpmkunitsrc.inc fpmkunitsrc
}
{$i fpmkunitsrc.inc}

procedure CreateFPMKUnitSource(const AFileName:string);
var
  InStream,
  OutStream : TStream;
  pend      : pchar;
begin
  try
    // Don't write trailing #0
    pend:=pchar(@fpmkunitsrc)+sizeof(fpmkunitsrc)-1;
    while pend^=#0 do
      dec(pend);
    InStream:=TMyMemoryStream.Create(@fpmkunitsrc,pend-pchar(@fpmkunitsrc));
    OutStream:=TFileStream.Create(AFileName,fmCreate);
    OutStream.CopyFrom(InStream,InStream.Size);
  finally
    InStream.Destroy;
    OutStream.Destroy;
  end;
end;


{*****************************************************************************
                               TMyMemoryStream
*****************************************************************************}

    constructor TMyMemoryStream.Create(p:pointer;mysize:integer);
      begin
        inherited Create;
        SetPointer(p,mysize);
      end;


{ TFPMakeCompiler }

Procedure TFPMakeCompiler.Execute;
var
  OOptions : string;

  function CheckUnitDir(const APackageName:string;Out AUnitDir:string):boolean;
  var
    P: TFPPackage;
  begin
    Result:=false;
    P := GFPpkg.FPMakeRepoFindPackage(APackageName, pkgpkInstalled);
    if Assigned(P) then
      begin
        AUnitDir := P.PackagesStructure.GetUnitDirectory(P);
        Result := DirectoryExistsLog(AUnitDir);
      end;
  end;

  procedure AddOption(const s:string);
  begin
    if OOptions<>'' then
      OOptions:=OOptions+' ';
    OOptions:=OOptions+maybequoted(s);
  end;

Var
  i : Integer;
  TempBuildDir,
  DepDir,
  FPMakeBin,
  FPMakeSrc : string;
  NeedFPMKUnitSource,
  HaveFpmake : boolean;
  P : TFPPackage;
begin
  P:=GFPpkg.PackageByName(PackageName, pkgpkAvailable);
  NeedFPMKUnitSource:=false;
  OOptions:='';
  SetCurrentDir(PackageManager.PackageBuildPath(P));
  // Generate random name for build path
  TempBuildDir:='build_fpmake_'+HexStr(DateTimeToUnix(Now),8)+HexStr(GetProcessId,4);
  // Check for fpmake source
  FPMakeBin:='fpmake'+ExeExt;
  FPMakeSrc:='fpmake.pp';
  HaveFpmake:=FileExists(FPMakeSrc);
  If Not HaveFPMake then
    begin
      HaveFPMake:=FileExists('fpmake.pas');
      If HaveFPMake then
        FPMakeSrc:='fpmake.pas';
    end;
  // Need to compile fpmake executable?
  if not FileExists(FPMakeBin) or
     (FileAge(FPMakeBin)<FileAge(FPMakeSrc)) then
    begin
      if Not HaveFPMake then
        Error(SErrMissingFPMake);
      AddOption('-n');
      AddOption('-dCOMPILED_BY_FPPKG');
      for i:=0 to high(FPMKUnitDeps) do
        begin
          if FPMKUnitDeps[i].available then
            begin
              if CheckUnitDir(FPMKUnitDeps[i].package,DepDir) then
                AddOption('-Fu'+DepDir)
              else
                Error(SErrMissingInstallPackage,[FPMKUnitDeps[i].package]);
              if FPMKUnitDeps[i].def<>'' then
                AddOption('-d'+FPMKUnitDeps[i].def);
              if FPMKUnitDeps[i].PluginUnit<>'' then
                AddOption('-Fa'+FPMKUnitDeps[i].PluginUnit);
            end
          else
            begin
              // If fpmkunit is not installed, we use the internal fpmkunit source
              if FPMKUnitDeps[i].package='fpmkunit' then
                begin
                  NeedFPMKUnitSource:=true;
                  AddOption('-Fu'+TempBuildDir);
                end;
              if FPMKUnitDeps[i].undef<>'' then
                AddOption('-d'+FPMKUnitDeps[i].undef);
            end;
        end;
      // Add RTL unit dir
      if not CheckUnitDir('rtl',DepDir) then
        Error(SErrMissingInstallPackage,['rtl']);
      AddOption('-Fu'+DepDir);
      // Units in a directory for easy cleaning
      DeleteDir(TempBuildDir);
      ForceDirectories(TempBuildDir);
      AddOption('-FU'+TempBuildDir);
      // Compile options
      //   -- default is to optimize, smartlink and strip to reduce
      //      the executable size (there can be 100's of fpmake's on a system)
      if llInfo in LogLevels then
        AddOption('-vi');
      AddOption('-O2');
      AddOption('-XXs');
      // Create fpmkunit.pp if needed
      if NeedFPMKUnitSource then
        begin
          Log(llWarning,SLogUseInternalFpmkunit);
          CreateFPMKUnitSource(TempBuildDir+PathDelim+'fpmkunit.pp');
        end;
      // Call compiler
      If ExecuteProcess(GFPpkg.FPMakeCompilerOptions.Compiler,OOptions+' '+FPmakeSrc)<>0 then
        begin
          if not GFPpkg.Options.CommandLineSection.RecoveryMode then
            Error(SErrCompileFailureFPMakeTryRecovery)
          else
            Error(SErrCompileFailureFPMake);
        end;
      // Cleanup units
      DeleteDir(TempBuildDir);
    end
  else
    Log(llCommands,SLogNotCompilingFPMake);
end;


{ TFPMakeRunner }

Function TFPMakeRunner.RunFPMake(const Command:string) : Integer;
Var
  P : TFPPackage;
  FPMakeBin,
  OOptions : string;
  InstallRepo: TFPRepository;
  GlobalUnitDir,
  LocalUnitDir: string;
  BaseInstDir: string;
  i: Integer;

  procedure AddOption(const s:string);
  begin
    if OOptions<>'' then
      OOptions:=OOptions+' ';
    OOptions:=OOptions+maybequoted(s);
  end;

  procedure CondAddOption(const Name,Value:string);
  begin
    if Value<>'' then
      AddOption(Name+'='+Value);
  end;

  procedure ObtainSupportedTargetsFromManifest(p:TFPPackage);
  var
    X : TFPXMLRepositoryHandler;
    ManifestPackages : TFPPackages;
    i: integer;
  begin
    p.OSes:=[];
    p.CPUs:=[];
    ManifestPackages:=TFPPackages.Create(TFPPackage);
    X:=TFPXMLRepositoryHandler.Create;
    try
      X.LoadFromXml(ManifestPackages,ManifestFileName);
      for i := 0 to ManifestPackages.Count-1 do
        begin
          p.OSes:=p.OSes+ManifestPackages[i].OSes;
          p.CPUs:=p.CPUs+ManifestPackages[i].CPUs;
        end;
    finally
      X.Free;
      ManifestPackages.Free;
    end;
  end;

begin
  OOptions:='';
  // Does the current package support this CPU-OS?
  if PackageName<>'' then
    begin
      P:=PackageManager.PackageByName(PackageName, pkgpkAvailable);
      if (PackageName=CurrentDirPackageName) and (FileExists(ManifestFileName)) then
        ObtainSupportedTargetsFromManifest(p);
    end
  else
    P:=nil;
  if assigned(P) then
    begin
      if (command<>'archive') and (command<>'manifest') and
         (not(PackageManager.CompilerOptions.CompilerOS in P.OSes) or
          not(PackageManager.CompilerOptions.CompilerCPU in P.CPUs)) then
        Error(SErrPackageDoesNotSupportTarget,[P.Name,MakeTargetString(PackageManager.CompilerOptions.CompilerCPU,GFPpkg.CompilerOptions.CompilerOS)]);
    end;
  { Maybe compile fpmake executable? }
  ExecuteAction(PackageName,'compilefpmake');
  { Create options }
  if llDebug in LogLevels then
    AddOption('--debug')
  else if llInfo in LogLevels then
    AddOption('--verbose');
  if (P.FPMakeOptionsString<>'') then
    begin
      // When the package is being reinstalled because of broken dependencies, use the same fpmake-options
      // as were used to compile the package in the first place.
      OOptions:=P.FPMakeOptionsString;
    end
  else
    begin
      if PackageManager.CompilerOptions.HasOptions then
        AddOption('--options='+GFPpkg.CompilerOptions.Options.DelimitedText);

      if PackageManager.Options.GlobalSection.CustomFPMakeOptions<>'' then
        begin
        AddOption('--ignoreinvalidoption');
        AddOption(PackageManager.Options.GlobalSection.CustomFPMakeOptions);
        end;
    end;

  AddOption('--nofpccfg');
  AddOption('--compiler='+GFPpkg.CompilerOptions.Compiler);
  AddOption('--cpu='+CPUToString(GFPpkg.CompilerOptions.CompilerCPU));
  AddOption('--os='+OSToString(GFPpkg.CompilerOptions.CompilerOS));

  // While scanning a source-repository it could be necessary to create manifest
  // files. At this moment the InstallRepo could not be initialized yet. And the
  // manifest command does not use the --prefix and --baseinstalldir parameters.
  if (command<>'manifest') then
    begin
      InstallRepo := PackageManager.RepositoryByName(PackageManager.Options.CommandLineSection.InstallRepository);

      if not Assigned(InstallRepo.DefaultPackagesStructure) then
        begin
          Error(SErrIllConfRepository,[InstallRepo.RepositoryName]);
          Exit;
        end;
      CondAddOption('--prefix',InstallRepo.DefaultPackagesStructure.GetPrefix);
      CondAddOption('--baseinstalldir',InstallRepo.DefaultPackagesStructure.GetBaseInstallDir);
    end;

  for i := PackageManager.RepositoryList.Count-1 downto 0 do
    begin
      if PackageManager.RepositoryList[i] is TFPRepository then
        begin
          InstallRepo := TFPRepository(PackageManager.RepositoryList[i]);
          if (InstallRepo.RepositoryType = fprtInstalled) and Assigned(InstallRepo.DefaultPackagesStructure) then
            begin
              BaseInstDir := InstallRepo.DefaultPackagesStructure.GetBaseInstallDir;
              CondAddOption('--searchpath', BaseInstDir);
              if LocalUnitDir='' then
                LocalUnitDir := BaseInstDir
              else if GlobalUnitDir='' then
                GlobalUnitDir := BaseInstDir;
            end;
        end;
    end;
  if GlobalUnitDir<>'' then
    CondAddOption('--globalunitdir', GlobalUnitDir);
  if LocalUnitDir<>'' then
    CondAddOption('--localunitdir', LocalUnitDir);

  { Run FPMake }
  FPMakeBin:='fpmake'+ExeExt;
  SetCurrentDir(PackageManager.PackageBuildPath(P));
  Result:=ExecuteProcess(FPMakeBin,Command+' '+OOptions);
  if Result<>0 then
    Error(SErrExecutionFPMake,[Command]);
end;


procedure TFPMakeRunnerCompile.Execute;
begin
  RunFPMake('compile');
end;


procedure TFPMakeRunnerBuild.Execute;
begin
  RunFPMake('build');
end;


procedure TFPMakeRunnerInstall.Execute;
begin
  RunFPMake('install');
end;


procedure TFPMakeRunnerUnInstall.Execute;
begin
  RunFPMake('uninstall');
end;


procedure TFPMakeRunnerClean.Execute;
begin
  RunFPMake('clean');
end;


procedure TFPMakeRunnerManifest.Execute;
begin
  RunFPMake('manifest');
end;


procedure TFPMakeRunnerArchive.Execute;
var
  StoredLocalPrefix: string;
  StoredGlobalPrefix: string;
begin
  // In most (all?) cases we do not want a prefix in the archive.
  StoredGlobalPrefix := GFPpkg.CompilerOptions.GlobalPrefix;
  StoredLocalPrefix := GFPpkg.CompilerOptions.LocalPrefix;
  GFPpkg.CompilerOptions.GlobalPrefix := '';
  GFPpkg.CompilerOptions.LocalPrefix := '';
  try
    RunFPMake('archive');
  finally
    GFPpkg.CompilerOptions.GlobalPrefix := StoredGlobalPrefix;
    GFPpkg.CompilerOptions.LocalPrefix := StoredLocalPrefix;
  end;
end;


initialization
  RegisterPkgHandler('compilefpmake',TFPMakeCompiler);
  RegisterPkgHandler('fpmakecompile',TFPMakeRunnerCompile);
  RegisterPkgHandler('fpmakebuild',TFPMakeRunnerBuild);
  RegisterPkgHandler('fpmakeinstall',TFPMakeRunnerInstall);
  RegisterPkgHandler('fpmakeuninstall',TFPMakeRunnerUnInstall);
  RegisterPkgHandler('fpmakeclean',TFPMakeRunnerClean);
  RegisterPkgHandler('fpmakemanifest',TFPMakeRunnerManifest);
  RegisterPkgHandler('fpmakearchive',TFPMakeRunnerArchive);
end.
