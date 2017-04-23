unit FullFPCInstallationTests;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  fpcunit,
  testdecorator,
  testregistry,
  CustApp,
  process,
  fpmkunit,
  pkgFppkg,
  fprepos;

type

  { TFullFPCInstallationTests }

  TFullFPCInstallationTests = class(TTestCase)
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestListPackages;
    procedure IntTestListPackages;
    procedure IntTestMergeGlobalOptions;
    procedure TestPackageA;
    procedure TestLooseFPMFile;
    procedure TestMissingSource;
    procedure TestBuildWithInstalledDependency;
    procedure TestFakePackageDir;
    procedure TestSourceDependency;
    procedure TestTransmitOptions;
    procedure TestPackageVariantPackage;
    procedure TestFPMakeCommandLikePackageVariants;
  end;

  { TFullFPCInstallationSetup }

  TFullFPCInstallationSetup = class(TTestSetup)
  private
    class var
      FFPCSourcePath: string;
      FTestPath: string;
      FPackagesPath: string;
      FStartCompiler: string;
      FTargetCPU: string;
      FTargetOS: string;
      FCompilerVersion: string;
  protected
    procedure OneTimeSetup; override;
    procedure OneTimeTearDown; override;
  public
    class function GetCurrentTestPath: string;
    class function GetTemplatePath: string;
    class function GetTestPath: string;
    class function GetBasePackagesPath: string;
    class function GetSpecificPackagesPath: string;
    class function GetCurrentTestBasePackagesPath: string;
    class function GetTestBinPath: string;
    class function GetTargetString: string;
    class function GetCompilerVersion: string;
    class function SyncPackageIntoCurrentTest(APackageName: string; SpecificPackageDir: string = ''): string;
  end;

implementation

function RunTestCommandIndir(const Curdir:string; const Exename:string; const Commands:array of string; TaskDescription: string; ExpectedExitStatus: Integer = 0):string;
var
  CommandOutput: string;
  i: integer;
  CommandLine: string;
  ExitStatus: Integer;
begin
  if RunCommandInDir(Curdir, Exename, Commands, CommandOutput, ExitStatus, [poStderrToOutPut]) <> 0 then
    raise Exception.CreateFmt('Failed to run ''%s''', [exename]);
  if ExitStatus<>ExpectedExitStatus then
    begin
    for i := 0 to length(Commands) -1 do
      begin
      CommandLine := CommandLine + ' ' + Commands[i];
      end;
    raise Exception.CreateFmt('Failed to %s.' +sLineBreak+ 'Current directory: ' +Curdir+ sLineBreak + 'command line: ' + Exename + CommandLine + sLineBreak + ' Output: ' + sLineBreak + CommandOutput, [TaskDescription]);
    end;
  result := CommandOutput;
end;

function RunFppkgIndir(const Curdir:string; Commands: array of string; TaskDescription: string; ExpectedExitStatus: Integer = 0):string;
var
  i: Integer;
  StrArr: array of string;
begin
  i := length(Commands);
  SetLength(StrArr, i + 2);
  StrArr[i] := '-C';
  StrArr[i+1] := ConcatPaths([TFullFPCInstallationSetup.GetCurrentTestPath,'etc','fppkg.cfg']);
  for i := 0 to length(Commands) -1 do
    StrArr[i] := Commands[i];
  Result := RunTestCommandIndir(Curdir, TFullFPCInstallationSetup.GetTestBinPath+'fppkg', StrArr, TaskDescription, ExpectedExitStatus);
end;

function RunFPMakeIndir(const Curdir:string; Commands: array of string; TaskDescription: string; ExpectedExitStatus: Integer = 0):string;
var
  i: Integer;
  StrArr: array of string;
  CompilerStr, FpcSearchpath, PackageSearchPath: string;
begin
  // Compile the package in the ProcVersion=VersionB variant
  CompilerStr := ConcatPaths([TFullFPCInstallationSetup.GetCurrentTestPath, 'bin', 'fpc']);
  FpcSearchpath := ConcatPaths([TFullFPCInstallationSetup.GetCurrentTestPath, 'lib', 'fpc', TFullFPCInstallationSetup.GetCompilerVersion]);
  PackageSearchpath := TFullFPCInstallationSetup.GetCurrentTestBasePackagesPath;

  i := length(Commands);
  SetLength(StrArr, i + 6);
  StrArr[i] := '--nofpccfg';
  StrArr[i+1] := '--compiler='+CompilerStr;
  StrArr[i+2] := '--searchpath='+FpcSearchpath;
  StrArr[i+3] := '--searchpath='+PackageSearchpath;
  StrArr[i+4] := '--prefix='+TFullFPCInstallationSetup.GetCurrentTestPath + 'user';
  StrArr[i+5] := '--baseinstalldir=' + ConcatPaths([TFullFPCInstallationSetup.GetCurrentTestPath, 'user', 'lib', 'fpc', TFullFPCInstallationSetup.GetCompilerVersion]);

  for i := 0 to length(Commands) -1 do
    StrArr[i] := Commands[i];
  Result := RunTestCommandIndir(Curdir, ConcatPaths([Curdir, 'fpmake']), StrArr, TaskDescription, ExpectedExitStatus);
end;


function DeleteDirectory(const DirectoryName: string; OnlyChildren: boolean): boolean;
const
  //Don't follow symlinks on *nix, just delete them
  DeleteMask = faAnyFile {$ifdef unix} or faSymLink{%H-} {$endif unix};
var
  FileInfo: TSearchRec;
  CurSrcDir: String;
  CurFilename: String;
begin
  Result:=false;
  CurSrcDir:=IncludeTrailingPathDelimiter(DirectoryName);
  if FindFirst(CurSrcDir+AllFilesMask,DeleteMask,FileInfo)=0 then begin
    repeat
      // check if special file
      if (FileInfo.Name='.') or (FileInfo.Name='..') or (FileInfo.Name='') then
        continue;
      CurFilename:=CurSrcDir+FileInfo.Name;
      if ((FileInfo.Attr and faDirectory)>0)
         {$ifdef unix} and ((FileInfo.Attr and faSymLink{%H-})=0) {$endif unix} then begin
        if not DeleteDirectory(CurFilename,false) then exit;
      end else begin
        if not DeleteFile(CurFilename) then exit;
      end;
    until FindNext(FileInfo)<>0;
  end;
  FindClose(FileInfo);
  if (not OnlyChildren) and (not RemoveDir(CurSrcDir)) then exit;
  Result:=true;
end;

{ TFullFPCInstallationSetup }

procedure TFullFPCInstallationSetup.OneTimeSetup;
var
  TemplatePath: string;
  LocalBasePath: string;
  MakeParams: array of string;
begin
  FFPCSourcePath := CustomApplication.GetOptionValue('f','fpcsrcpath');
  if FFPCSourcePath<>'' then
    FFPCSourcePath := ExpandFileName(FFPCSourcePath);
  FStartCompiler := CustomApplication.GetOptionValue('s','startcompiler');
  FTestPath := CustomApplication.GetOptionValue('t','testpath');
  if FTestPath='' then
    FTestPath := IncludeTrailingPathDelimiter(ConcatPaths([ExtractFilePath(ParamStr(0)),'testroot']))
  else
    FTestPath := ExpandFileName(FTestPath);
  FPackagesPath := CustomApplication.GetOptionValue('p','packagespath');
  if FPackagesPath='' then
    FPackagesPath := IncludeTrailingPathDelimiter(ConcatPaths([ExtractFilePath(ParamStr(0)),'packages']))
  else
    FPackagesPath := ExpandFileName(FPackagesPath);

  if not CustomApplication.HasOption('T', 'skipbuildtemplate') then
    begin
    TemplatePath := GetTemplatePath;
    if DirectoryExists(GetTestPath) and not DeleteDirectory(GetTestPath, False) then
      raise Exception.CreateFmt('Failed to remove source-path ''%s''', [GetTestPath]);

    ForceDirectories(GetTemplatePath);

    SetLength(MakeParams, 2);
    MakeParams[0] := 'clean';
    MakeParams[1] := 'all';
    if FStartCompiler<>'' then
      begin
      SetLength(MakeParams, length(MakeParams)+1);
      MakeParams[High(MakeParams)] := 'PP='+FStartCompiler;
      end;

    RunTestCommandIndir(FFPCSourcePath, 'make', MakeParams, 'compile FPC');

    MakeParams[0] := 'install';
    MakeParams[1] := 'PREFIX='+GetTemplatePath;
    RunTestCommandIndir(FFPCSourcePath, 'make', MakeParams, 'install FPC');

    LocalBasePath :=  IncludeTrailingPathDelimiter(ConcatPaths([GetTemplatePath, 'user','lib','fpc']));
    FCompilerVersion := Trim(RunTestCommandIndir(GetTemplatePath, GetTemplatePath+'bin'+PathDelim+'fpc', ['-iV'], 'get compiler-version'));

    ForceDirectories(LocalBasePath+FCompilerVersion);

    SetLength(MakeParams, 8);
    MakeParams[0] := '-o';
    MakeParams[1] := GetTemplatePath+PathDelim+'fpc.cfg';
    MakeParams[2] := '-d';
    MakeParams[3] := 'basepath='+ConcatPaths([GetCurrentTestPath, 'lib','fpc','$fpcversion']);
    MakeParams[4] := '-d';
    MakeParams[5] := 'sharepath='+ConcatPaths([GetCurrentTestPath, 'share','fpc','$fpcversion']);
    MakeParams[6] := '-d';
    MakeParams[7] := 'localbasepath='+LocalBasePath+'$fpcversion';
    RunTestCommandIndir(ConcatPaths([GetTemplatePath,'bin']), 'fpcmkcfg', MakeParams, 'create fpc.cfg');

    SetLength(MakeParams, 12);
    MakeParams[1] := ConcatPaths([GetTemplatePath, 'etc', 'fppkg.cfg']);
    MakeParams[8] := '-3';
    MakeParams[9] := '-p';
    MakeParams[3] := 'GlobalPath='+ConcatPaths([GetCurrentTestPath, 'lib', 'fpc']);
    MakeParams[5] := 'GlobalPrefix='+GetCurrentTestPath;
    MakeParams[10] := '-d';
    MakeParams[11] := 'LocalRepository='+ConcatPaths([GetCurrentTestPath, 'user'])+PathDelim;
    RunTestCommandIndir(ConcatPaths([GetTemplatePath,'bin']), 'fpcmkcfg', MakeParams, 'create fppkg.cfg');


    SetLength(MakeParams, 12);
    MakeParams[1] := ConcatPaths([TemplatePath, 'user', 'config', 'default']);
    MakeParams[8] := '-4';
    MakeParams[9] := '-p';
    MakeParams[3] := 'GlobalPath='+ConcatPaths([GetCurrentTestPath, 'lib','fpc']);
    MakeParams[5] := 'fpcbin='+ConcatPaths([GetCurrentTestPath, 'bin','fpc']);
    MakeParams[10] := '-d';
    MakeParams[11] := 'LocalRepository='+ConcatPaths([GetCurrentTestPath, 'user'])+PathDelim;
    RunTestCommandIndir(ConcatPaths([TemplatePath,'bin']), 'fpcmkcfg', MakeParams, 'create default fppkg compiler file');

    ForceDirectories(ConcatPaths([TemplatePath, 'user','config','conf.d']));
    end
  else
    begin
    FCompilerVersion := Trim(RunTestCommandIndir(GetTemplatePath, GetTemplatePath+'bin'+PathDelim+'fpc', ['-iV'], 'get compiler-version'));
    end;
  FTargetOS := Trim(RunTestCommandIndir(GetTemplatePath, GetTemplatePath+'bin'+PathDelim+'fpc', ['-iTO'], 'get target-OS'));
  FTargetCPU := Trim(RunTestCommandIndir(GetTemplatePath, GetTemplatePath+'bin'+PathDelim+'fpc', ['-iTP'], 'get target-CPU'));
end;

procedure TFullFPCInstallationSetup.OneTimeTearDown;
begin

end;

class function TFullFPCInstallationSetup.GetCurrentTestPath: string;
begin
  Result := IncludeTrailingPathDelimiter(ConcatPaths([FTestPath,'currenttest']));
end;

class function TFullFPCInstallationSetup.GetTemplatePath: string;
begin
  Result := IncludeTrailingPathDelimiter(ConcatPaths([FTestPath,'templates','fullfpc']));
end;

class function TFullFPCInstallationSetup.GetTestPath: string;
begin
  Result := FTestPath;
end;

class function TFullFPCInstallationSetup.GetBasePackagesPath: string;
begin
  Result := IncludeTrailingPathDelimiter(ConcatPaths([FPackagesPath, 'base']));
end;

class function TFullFPCInstallationSetup.GetSpecificPackagesPath: string;
begin
  Result := IncludeTrailingPathDelimiter(ConcatPaths([FPackagesPath, 'specific']));
end;

class function TFullFPCInstallationSetup.GetCurrentTestBasePackagesPath: string;
begin
  Result := IncludeTrailingPathDelimiter(ConcatPaths([GetCurrentTestPath, 'packages']));
end;

class function TFullFPCInstallationSetup.GetTestBinPath: string;
begin
  Result := IncludeTrailingPathDelimiter(ConcatPaths([GetCurrentTestPath,'bin']));
end;

class function TFullFPCInstallationSetup.GetTargetString: string;
begin
  Result := FTargetCPU + '-' + FTargetOS;
end;

class function TFullFPCInstallationSetup.GetCompilerVersion: string;
begin
  Result := FCompilerVersion;
end;

class function TFullFPCInstallationSetup.SyncPackageIntoCurrentTest(APackageName: string; SpecificPackageDir: string): string;
var
  PackagePath: string;
begin
  ForceDirectories(ConcatPaths([TFullFPCInstallationSetup.GetTestPath, 'currenttest', 'packages']));
  if SpecificPackageDir='' then
    PackagePath := TFullFPCInstallationSetup.GetBasePackagesPath+APackageName+PathDelim
  else
    PackagePath := ConcatPaths([TFullFPCInstallationSetup.GetSpecificPackagesPath, SpecificPackageDir, APackageName])+PathDelim;
  RunTestCommandIndir(TFullFPCInstallationSetup.GetTestPath, 'rsync', ['-rtvu', '--delete', PackagePath, TFullFPCInstallationSetup.GetCurrentTestBasePackagesPath+APackageName], 'sync template');
end;

procedure TFullFPCInstallationTests.SetUp;
begin
  RunTestCommandIndir(TFullFPCInstallationSetup.GetTestPath, 'rsync', ['-rtvu', '--delete', 'templates/fullfpc/', 'currenttest/'], 'sync template');
end;

procedure TFullFPCInstallationTests.TearDown;
begin

end;

procedure TFullFPCInstallationTests.TestListPackages;
var
  s: String;
begin
  s := RunFppkgIndir(TFullFPCInstallationSetup.GetTestPath, ['list'], 'fppkg list');
  Check(pos('rtl',s) > 0, 'Package rtl not found in fppkg-package list');
end;

procedure TFullFPCInstallationTests.IntTestListPackages;
var
  FPpkg: TpkgFPpkg;
  RTLPackage: TFPPackage;
begin
   FPpkg := TpkgFPpkg.Create(nil);
   try
     FPpkg.InitializeGlobalOptions(ConcatPaths([TFullFPCInstallationSetup.GetCurrentTestPath,'etc','fppkg.cfg']));
     FPpkg.Options.GlobalSection.Downloader := 'FPC';
     FPpkg.InitializeCompilerOptions;

     FPpkg.CompilerOptions.InitCompilerDefaults;
     FPpkg.FpmakeCompilerOptions.InitCompilerDefaults;
     FPpkg.CompilerOptions.CheckCompilerValues;
     FPpkg.FpmakeCompilerOptions.CheckCompilerValues;
     FPpkg.LoadLocalAvailableMirrors;

     FPpkg.ScanAvailablePackages;
     FPpkg.ScanPackages;

     RTLPackage := FPpkg.RepositoryByName('fpc').FindPackage('rtl');
     CheckNotNull(RTLPackage, 'RTL package not found');
     CheckEquals('3.1.1', RTLPackage.Version.AsString, 'RTL has not the same version as the compiler');
   finally
     FPpkg.Free;
   end;
end;

procedure TFullFPCInstallationTests.IntTestMergeGlobalOptions;
var
  FPpkg: TpkgFPpkg;
  ExtraConfFile: Text;
  s: string;
begin
  // When there are multiple global-sections, it's values should be merged.
  s := ConcatPaths([TFullFPCInstallationSetup.GetCurrentTestPath, 'user', 'config', 'conf.d', 'extrasettings.conf']);
  AssignFile(ExtraConfFile, ConcatPaths([TFullFPCInstallationSetup.GetCurrentTestPath, 'user', 'config', 'conf.d', 'extrasettings.conf']));
  Rewrite(ExtraConfFile);
  WriteLn(ExtraConfFile, '[global]');
  WriteLn(ExtraConfFile, 'FPMakeOptions="-T 4"');
  CloseFile(ExtraConfFile);

  FPpkg := TpkgFPpkg.Create(nil);
  try
    FPpkg.InitializeGlobalOptions(ConcatPaths([TFullFPCInstallationSetup.GetCurrentTestPath,'etc','fppkg.cfg']));
    FPpkg.Options.GlobalSection.Downloader := 'FPC';
    FPpkg.InitializeCompilerOptions;

    FPpkg.CompilerOptions.InitCompilerDefaults;
    FPpkg.FpmakeCompilerOptions.InitCompilerDefaults;
    FPpkg.CompilerOptions.CheckCompilerValues;
    FPpkg.FpmakeCompilerOptions.CheckCompilerValues;
    FPpkg.LoadLocalAvailableMirrors;

    CheckEquals('user', FPpkg.Options.GlobalSection.InstallRepository, 'The InstallRepository does not match the value in the original configuration file');
    CheckEquals('"-T 4"', FPpkg.Options.GlobalSection.CustomFPMakeOptions, 'The custom FPMakeOptions do not match the value in the extra configuration file');
  finally
    FPpkg.Free;
  end;
end;

procedure TFullFPCInstallationTests.TestPackageA;
var
  s: String;
begin
  TFullFPCInstallationSetup.SyncPackageIntoCurrentTest('packagea');
  // Build and install package
  RunFppkgIndir(TFullFPCInstallationSetup.GetCurrentTestBasePackagesPath + 'packagea', ['build'], 'build PackageA');
  RunFppkgIndir(TFullFPCInstallationSetup.GetCurrentTestBasePackagesPath + 'packagea', ['install'], 'install PackageA');

  // Test installation
  s := RunFppkgIndir(TFullFPCInstallationSetup.GetCurrentTestPath, ['list'], 'list packages');
  Check(pos('packagea', s) > 0, 'Just installed PackageA is not in package-list');
  Check(FileExists(ConcatPaths([TFullFPCInstallationSetup.GetCurrentTestPath,'user','lib','fpc', TFullFPCInstallationSetup.GetCompilerVersion, 'units',TFullFPCInstallationSetup.GetTargetString,'packagea','PackageAUnitA.ppu'])), 'PackageAUnitA.ppu not found');
  Check(FileExists(ConcatPaths([TFullFPCInstallationSetup.GetCurrentTestPath,'user','lib','fpc', TFullFPCInstallationSetup.GetCompilerVersion, 'units',TFullFPCInstallationSetup.GetTargetString,'packagea','PackageAUnitA.o'])), 'PackageAUnitA.o not found');
  Check(FileExists(ConcatPaths([TFullFPCInstallationSetup.GetCurrentTestPath,'user','lib','fpc', TFullFPCInstallationSetup.GetCompilerVersion, 'fpmkinst',TFullFPCInstallationSetup.GetTargetString,'packagea.fpm'])), 'PackageAUnitA.fpm not found');

  // uninstall package
  RunFppkgIndir(TFullFPCInstallationSetup.GetCurrentTestBasePackagesPath + 'packagea', ['uninstall'], 'install PackageA');

  // check uninstallation
  s := RunFppkgIndir(TFullFPCInstallationSetup.GetCurrentTestPath, ['list'], 'list packages');
  Check(pos('packagea', s) = 0, 'Just de-installed PackageA is still in package-list');
  CheckFalse(DirectoryExists(ConcatPaths([TFullFPCInstallationSetup.GetCurrentTestPath,'user','lib','fpc', TFullFPCInstallationSetup.GetCompilerVersion, 'units',TFullFPCInstallationSetup.GetTargetString,'packagea'])), 'PackageAUnitA-directory found after uninstall');
  CheckFalse(FileExists(ConcatPaths([TFullFPCInstallationSetup.GetCurrentTestPath,'user','lib','fpc', TFullFPCInstallationSetup.GetCompilerVersion, 'fpmkinst',TFullFPCInstallationSetup.GetTargetString,'packagea.fpm'])), 'PackageAUnitA.fpm found after uninstall');
end;

procedure TFullFPCInstallationTests.TestLooseFPMFile;
var
  F: Text;
  s: string;
begin
  System.Assign(F, ConcatPaths([TFullFPCInstallationSetup.GetCurrentTestPath, 'lib', 'fpc', TFullFPCInstallationSetup.GetCompilerVersion, 'fpmkinst', TFullFPCInstallationSetup.GetTargetString,'empty.fpm']));
  System.Rewrite(F);
  System.Close(F);

  s := RunFppkgIndir(TFullFPCInstallationSetup.GetCurrentTestPath, ['list'], 'list packages');
  Check(pos('Failed to load package "empty"', s) > 0, 'Missing warning that the invalid package is skipped')
end;

procedure TFullFPCInstallationTests.TestMissingSource;
var
  s: String;
begin
  TFullFPCInstallationSetup.SyncPackageIntoCurrentTest('packagea');
  // Build and install package
  RunFppkgIndir(TFullFPCInstallationSetup.GetCurrentTestBasePackagesPath + 'packagea', ['build'], 'build PackageA');
  RunFppkgIndir(TFullFPCInstallationSetup.GetCurrentTestBasePackagesPath + 'packagea', ['install'], 'install PackageA');

  // Destroy the installation
  DeleteFile(ConcatPaths([TFullFPCInstallationSetup.GetCurrentTestPath,'user','lib','fpc', TFullFPCInstallationSetup.GetCompilerVersion, 'units',TFullFPCInstallationSetup.GetTargetString,'packagea','PackageAUnitA.ppu']));

  // Re-install
  RunFppkgIndir(TFullFPCInstallationSetup.GetTestPath, ['install', 'packagea'], 're-install PackageA');

  Check(FileExists(ConcatPaths([TFullFPCInstallationSetup.GetCurrentTestPath,'user','lib','fpc', TFullFPCInstallationSetup.GetCompilerVersion, 'units',TFullFPCInstallationSetup.GetTargetString,'packagea','PackageAUnitA.ppu'])), 'PackageAUnitA.ppu not found after re-install');

  // Remove the original sources
  DeleteDirectory(TFullFPCInstallationSetup.GetCurrentTestBasePackagesPath + 'packagea', False);

  s := RunFppkgIndir(TFullFPCInstallationSetup.GetTestPath, ['install', 'packagea'], 'Re-install PackageA without source', 1);
  Check(pos('Source of package packagea is not available', s) > 0, 'Missing warning that the package-source is unavailable. Fppkg-output: ' + s)
end;

procedure TFullFPCInstallationTests.TestBuildWithInstalledDependency;
var
  s: String;
begin
  TFullFPCInstallationSetup.SyncPackageIntoCurrentTest('packagea');
  TFullFPCInstallationSetup.SyncPackageIntoCurrentTest('packageb');
  // Build and install package
  RunFppkgIndir(TFullFPCInstallationSetup.GetCurrentTestBasePackagesPath + 'packagea', ['install'], 'install PackageA');
  RunFppkgIndir(TFullFPCInstallationSetup.GetCurrentTestBasePackagesPath + 'packageb', ['install'], 'install PackageB using the installed dependency PackageA');

  // Test installation
  s := RunFppkgIndir(TFullFPCInstallationSetup.GetCurrentTestPath, ['list'], 'list packages');
  Check(pos('packagea', s) > 0, 'Just installed PackageA is not in package-list');
  Check(pos('PackageB', s) > 0, 'Just installed PackageB is not in package-list');
  Check(FileExists(ConcatPaths([TFullFPCInstallationSetup.GetCurrentTestPath,'user','lib','fpc', TFullFPCInstallationSetup.GetCompilerVersion, 'units',TFullFPCInstallationSetup.GetTargetString,'packagea','PackageAUnitA.ppu'])), 'PackageAUnitA.ppu not found');
  Check(FileExists(ConcatPaths([TFullFPCInstallationSetup.GetCurrentTestPath,'user','lib','fpc', TFullFPCInstallationSetup.GetCompilerVersion, 'units',TFullFPCInstallationSetup.GetTargetString,'PackageB','PackageBUnitB.ppu'])), 'PackageBUnitB.ppu not found');
end;

procedure TFullFPCInstallationTests.TestFakePackageDir;
var
  s: String;
begin
  TFullFPCInstallationSetup.SyncPackageIntoCurrentTest('packagea');
  TFullFPCInstallationSetup.SyncPackageIntoCurrentTest('packageb');
  // Build and install package
  RunFppkgIndir(TFullFPCInstallationSetup.GetCurrentTestBasePackagesPath + 'packagea', ['install', '-i', 'fpc'], 'install PackageA');

  ForceDirectories(ConcatPaths([TFullFPCInstallationSetup.GetCurrentTestPath, 'user', 'lib', 'fpc', TFullFPCInstallationSetup.GetCompilerVersion, 'units', TFullFPCInstallationSetup.GetTargetString, 'PackageA']));

  RunFppkgIndir(TFullFPCInstallationSetup.GetCurrentTestBasePackagesPath + 'packageb', ['install'], 'install PackageB using the installed dependency PackageA');

  // Test installation
  s := RunFppkgIndir(TFullFPCInstallationSetup.GetCurrentTestPath, ['list'], 'list packages');
  Check(pos('packagea', s) > 0, 'Just installed PackageA is not in package-list');
  Check(pos('PackageB', s) > 0, 'Just installed PackageB is not in package-list');
  Check(FileExists(ConcatPaths([TFullFPCInstallationSetup.GetCurrentTestPath,'lib','fpc', TFullFPCInstallationSetup.GetCompilerVersion, 'units',TFullFPCInstallationSetup.GetTargetString,'packagea','PackageAUnitA.ppu'])), 'PackageAUnitA.ppu not found');
  Check(FileExists(ConcatPaths([TFullFPCInstallationSetup.GetCurrentTestPath,'user','lib','fpc', TFullFPCInstallationSetup.GetCompilerVersion, 'units',TFullFPCInstallationSetup.GetTargetString,'PackageB','PackageBUnitB.ppu'])), 'PackageBUnitB.ppu not found');
end;

procedure TFullFPCInstallationTests.TestSourceDependency;
var
  s: String;
begin
  // This is to test if fpmkunit works correctly when a dependency is available
  // not as an installed but as a (compiled) source-package. This happens for
  // example if you try to compile _one_ fpmake-packages in fpcsrc/packages,
  // using 'make clean all' and it needs one of the other packages in
  // fpcsrc/packages.
  TFullFPCInstallationSetup.SyncPackageIntoCurrentTest('packagea');
  TFullFPCInstallationSetup.SyncPackageIntoCurrentTest('packageb');

  RunFppkgIndir(TFullFPCInstallationSetup.GetCurrentTestBasePackagesPath + 'packagea', ['build'], 'build PackageA');

  RunFppkgIndir(TFullFPCInstallationSetup.GetCurrentTestBasePackagesPath + 'packageb', ['build'], 'create fpmake-executable', 1);
  RunFPMakeIndir(ConcatPaths([TFullFPCInstallationSetup.GetCurrentTestBasePackagesPath,'packageb']), ['build'], 'build packageb');

  // When there is no .fpm file, fpmake should complain that the package is not
  // compiled. (Another possibility is that another packages is mistakenly being
  // used, for example when the package-name does not match the directory-name)
  // This has to be enforced because without the .fpm, the dependencies are
  // not handled.
  s := ConcatPaths([TFullFPCInstallationSetup.GetCurrentTestBasePackagesPath,'packagea','packagea-'+TFullFPCInstallationSetup.GetTargetString+'.fpm']);
  DeleteFile(ConcatPaths([TFullFPCInstallationSetup.GetCurrentTestBasePackagesPath,'packagea','packagea-'+TFullFPCInstallationSetup.GetTargetString+'.fpm']));
  s := RunFPMakeIndir(ConcatPaths([TFullFPCInstallationSetup.GetCurrentTestBasePackagesPath,'packageb']), ['build', '-d'], 'build packageb without fpm', 1);
  Check(pos('the package is not compiled', s) > 0, 'Missing .fpm-file detection did not trigger');
  Check(pos('Could not find unit directory for dependency package', s) > 0, 'Incorrect error message');
end;

procedure TFullFPCInstallationTests.TestTransmitOptions;
begin
  // Test the TransmitOptions settings. PackageA contain some TransmitOptions,
  // without which the other packages won't compile.
  // PackageC depends on both PackageB's, but should only add the TransmitOptions
  // from PackageA once.
  TFullFPCInstallationSetup.SyncPackageIntoCurrentTest('packagea', 'transmitoptions');
  TFullFPCInstallationSetup.SyncPackageIntoCurrentTest('packageb1', 'transmitoptions');
  TFullFPCInstallationSetup.SyncPackageIntoCurrentTest('packageb2', 'transmitoptions');
  TFullFPCInstallationSetup.SyncPackageIntoCurrentTest('packagec', 'transmitoptions');

  RunFppkgIndir(TFullFPCInstallationSetup.GetCurrentTestBasePackagesPath + 'packagea', ['install'], 'build PackageA');
  RunFppkgIndir(TFullFPCInstallationSetup.GetCurrentTestBasePackagesPath + 'packageb1', ['install'], 'build PackageB1');
  RunFppkgIndir(TFullFPCInstallationSetup.GetCurrentTestBasePackagesPath + 'packageb2', ['install'], 'build PackageB2');
  RunFppkgIndir(TFullFPCInstallationSetup.GetCurrentTestBasePackagesPath + 'packagec', ['install'], 'build PackageC');
end;

procedure TFullFPCInstallationTests.TestPackageVariantPackage;
var
  s: String;
begin
  TFullFPCInstallationSetup.SyncPackageIntoCurrentTest('packagevarianta');
  TFullFPCInstallationSetup.SyncPackageIntoCurrentTest('packagevariantp');

  // Compile the packages with their default variant
  RunFppkgIndir(TFullFPCInstallationSetup.GetCurrentTestBasePackagesPath + 'packagevarianta', ['install'], 'install PackageVariantA');
  RunFppkgIndir(TFullFPCInstallationSetup.GetCurrentTestBasePackagesPath + 'packagevariantp', ['install'], 'install PackageVariantP');

  // Check the usage of the versiona-subdirectory
  Check(FileExists(ConcatPaths([TFullFPCInstallationSetup.GetCurrentTestPath, 'user', 'lib', 'fpc', TFullFPCInstallationSetup.GetCompilerVersion, 'units', TFullFPCInstallationSetup.GetTargetString, 'packagevarianta', 'versiona', 'packagevariantbaseunit.ppu'])), 'packagevariantbaseunit.ppu not found');

  // Check the output of the generated executable
  s := RunTestCommandIndir(TFullFPCInstallationSetup.GetCurrentTestPath, ConcatPaths([TFullFPCInstallationSetup.GetCurrentTestPath, 'user', 'bin', 'packagevariantp']), [], 'Run PackageVariantP');
  Check(pos('Hello version A', s) > 0, 'Package is not compiled with Version-A unit');

  RunFPMakeIndir(ConcatPaths([TFullFPCInstallationSetup.GetCurrentTestBasePackagesPath,'packagevarianta']), ['build', '+ProcVersion=versionb'], 'build PackageVariantA in the VersionB variant');
  RunFPMakeIndir(ConcatPaths([TFullFPCInstallationSetup.GetCurrentTestBasePackagesPath,'packagevariantp']), ['build', '+ProcVersion=versionb'], 'build PackageVariantP in the VersionB variant');

  // Check the usage of the versiona- & versionb-subdirectory
  Check(FileExists(ConcatPaths([TFullFPCInstallationSetup.GetCurrentTestBasePackagesPath, 'packagevarianta', 'units', TFullFPCInstallationSetup.GetTargetString, 'versiona', 'packagevariantbaseunit.ppu'])), 'packagevariantbaseunit.ppu versiona not found');
  Check(FileExists(ConcatPaths([TFullFPCInstallationSetup.GetCurrentTestBasePackagesPath, 'packagevarianta', 'units', TFullFPCInstallationSetup.GetTargetString, 'versionb', 'packagevariantbaseunit.ppu'])), 'packagevariantbaseunit.ppu versionb not found');

  // Check the output of the generated executable
  s := RunTestCommandIndir(TFullFPCInstallationSetup.GetCurrentTestPath, ConcatPaths([TFullFPCInstallationSetup.GetCurrentTestBasePackagesPath, 'packagevariantp', 'bin', TFullFPCInstallationSetup.GetTargetString, 'packagevariantp']), [], 'Run PackageVariantP');
  Check(pos('Bye version B', s) > 0, 'Package is not compiled with Version-B unit');
  Check(pos('Now with extra unit!', s) > 0, 'Package is not compiled with extra Version-B unit');

  // Compile PackageVariantP again, but now with VersionA
  RunFPMakeIndir(ConcatPaths([TFullFPCInstallationSetup.GetCurrentTestBasePackagesPath,'packagevariantp']), ['build', '+ProcVersion=versiona'], 'build PackageVariantP in the VersionA variant');

  // Check the output of the generated executable
  s := RunTestCommandIndir(TFullFPCInstallationSetup.GetCurrentTestPath, ConcatPaths([TFullFPCInstallationSetup.GetCurrentTestBasePackagesPath, 'packagevariantp', 'bin', TFullFPCInstallationSetup.GetTargetString, 'packagevariantp']), [], 'Run PackageVariantP');
  Check(pos('Hello version A', s) > 0, 'Package is not compiled with Version-A unit');
end;

procedure TFullFPCInstallationTests.TestFPMakeCommandLikePackageVariants;
var
  s: String;
begin
  TFullFPCInstallationSetup.SyncPackageIntoCurrentTest('packagea');
  TFullFPCInstallationSetup.SyncPackageIntoCurrentTest('packageb');

  // Build packagea so that fpmake is compiled
  RunFppkgIndir(TFullFPCInstallationSetup.GetCurrentTestBasePackagesPath + 'packagea', ['build'], 'build PackageA with fppkg');

  // Test some invalid command-line arguments
  s := RunFPMakeIndir(TFullFPCInstallationSetup.GetCurrentTestBasePackagesPath+ 'packagea', ['build', '+buildvariant'], 'Test with invalid +buildvariant command line option', 1);
  Check(pos('needs an argument', s) > 0, 'FPMake did not check for the argument');
  s := RunFPMakeIndir(TFullFPCInstallationSetup.GetCurrentTestBasePackagesPath+ 'packagea', ['build', '+buildvariant+'], 'Test with invalid +buildvariant+ command line option', 1);
  Check(pos('needs an argument', s) > 0, 'FPMake did not check for the argument +');
  s := RunFPMakeIndir(TFullFPCInstallationSetup.GetCurrentTestBasePackagesPath+ 'packagea', ['build', '+buildvariant+='], 'Test with empty +buildvariant+ value', 1);
  Check(pos('should have at least one item', s) > 0, 'FPMake did not check for an empty argument');

  // Build PackageA with BuildVariants
  RunFPMakeIndir(TFullFPCInstallationSetup.GetCurrentTestBasePackagesPath+ 'packagea', ['build', '+buildvariant+=debug,release', '--options_buildvariant_debug=-gl', '--options_buildvariant_release="-g- -CX -XX -O2"'], 'Build debug variant');
  // Build the release-variant
  RunFPMakeIndir(TFullFPCInstallationSetup.GetCurrentTestBasePackagesPath+ 'packagea', ['build', '+buildvariant+=debug,release', '--options_buildvariant_debug=-gl', '--options_buildvariant_release="-g- -CX -XX -O2"', '+buildvariant=release'], 'Build release variant');

  // Check the usage of the debug- & release-subdirectory
  Check(FileExists(ConcatPaths([TFullFPCInstallationSetup.GetCurrentTestBasePackagesPath, 'packagea', 'units', TFullFPCInstallationSetup.GetTargetString, 'debug', 'PackageAUnitA.ppu'])), 'PackageAUnitA.ppu debug-version not found');
  Check(FileExists(ConcatPaths([TFullFPCInstallationSetup.GetCurrentTestBasePackagesPath, 'packagea', 'units', TFullFPCInstallationSetup.GetTargetString, 'release', 'PackageAUnitA.ppu'])), 'PackageAUnitA.ppu release-version not found');

  // Install PackageA with BuildVariants
  RunFPMakeIndir(TFullFPCInstallationSetup.GetCurrentTestBasePackagesPath+ 'packagea', ['install', '+buildvariant+=debug,release', '--options_buildvariant_debug=-gl', '--options_buildvariant_release="-g- -CX -XX -O2"'], 'Build debug variant');
  // Install the release-variant
  RunFPMakeIndir(TFullFPCInstallationSetup.GetCurrentTestBasePackagesPath+ 'packagea', ['install', '+buildvariant+=debug,release', '--options_buildvariant_debug=-gl', '--options_buildvariant_release="-g- -CX -XX -O2"', '+buildvariant=release'], 'Build release variant');

  // Check the usage of the debug-subdirectory
  s := ConcatPaths([TFullFPCInstallationSetup.GetCurrentTestPath, 'user', 'lib', 'fpc', TFullFPCInstallationSetup.GetCompilerVersion, 'units', TFullFPCInstallationSetup.GetTargetString, 'packagea', 'debug', 'PackageAUnitA.ppu']);
  Check(FileExists(s), 'installed PackageAUnitA.ppu debug-version not found');
  // Check that the debug-version has debug-information
  s := RunTestCommandIndir(TFullFPCInstallationSetup.GetCurrentTestPath, TFullFPCInstallationSetup.GetCurrentTestPath+'bin'+PathDelim+'ppudump', s, 'Dump the ppu-information of the debug unit');
  Check(pos('has_debug_info', s) > 0, 'The debug-unit does not have debug-info');

  // Check the usage of the release-subdirectory
  s := ConcatPaths([TFullFPCInstallationSetup.GetCurrentTestPath, 'user', 'lib', 'fpc', TFullFPCInstallationSetup.GetCompilerVersion, 'units', TFullFPCInstallationSetup.GetTargetString, 'packagea', 'release', 'PackageAUnitA.ppu']);
  Check(FileExists(s), 'installed PackageAUnitA.ppu release-version not found');
  // Check that the debug-version has debug-information
  s := RunTestCommandIndir(TFullFPCInstallationSetup.GetCurrentTestPath, TFullFPCInstallationSetup.GetCurrentTestPath+'bin'+PathDelim+'ppudump', s, 'Dump the ppu-information of the release unit');
  Check(pos('has_debug_info', s) = 0, 'The release-unit has debug-info');

  // Build packageb, use the default variant (debug)
  RunFppkgIndir(TFullFPCInstallationSetup.GetCurrentTestBasePackagesPath + 'packageb', ['build'], 'build PackageB with fppkg');
end;

Initialization
  RegisterTestDecorator(TFullFPCInstallationSetup, TFullFPCInstallationTests);
end.

