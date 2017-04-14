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
    procedure TestPackageA;
    procedure TestLooseFPMFile;
    procedure TestMissingSource;
    procedure TestBuildWithInstalledDependency;
    procedure TestFakePackageDir;
    procedure TestSourceDependency;
    procedure TestTransmitOptions;
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
  CompilerStr,
  FpcSearchpath,
  PackageSearchpath: string;
begin
  // This is to test if fpmkunit works correctly when a dependency is available
  // not as an installed but as a (compiled) source-package. This happens for
  // example if you try to compile _one_ fpmake-packages in fpcsrc/packages,
  // using 'make clean all' and it needs one of the other packages in
  // fpcsrc/packages.
  TFullFPCInstallationSetup.SyncPackageIntoCurrentTest('packagea');
  TFullFPCInstallationSetup.SyncPackageIntoCurrentTest('packageb');

  RunFppkgIndir(TFullFPCInstallationSetup.GetCurrentTestBasePackagesPath + 'packagea', ['build'], 'build PackageA');

  CompilerStr := ConcatPaths([TFullFPCInstallationSetup.GetCurrentTestPath, 'bin', 'fpc']);
  FpcSearchpath := ConcatPaths([TFullFPCInstallationSetup.GetCurrentTestPath, 'lib', 'fpc', TFullFPCInstallationSetup.GetCompilerVersion]);
  PackageSearchpath := TFullFPCInstallationSetup.GetCurrentTestBasePackagesPath;

  RunFppkgIndir(TFullFPCInstallationSetup.GetCurrentTestBasePackagesPath + 'packageb', ['build'], 'create fpmake-executable', 1);
  RunTestCommandIndir(ConcatPaths([TFullFPCInstallationSetup.GetCurrentTestBasePackagesPath,'packageb']), ConcatPaths([TFullFPCInstallationSetup.GetCurrentTestBasePackagesPath,'packageb', 'fpmake']), ['build', '--nofpccfg', '--compiler='+CompilerStr, '--searchpath='+FpcSearchpath, '--searchpath='+PackageSearchpath], 'build packagea');
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

Initialization
  RegisterTestDecorator(TFullFPCInstallationSetup, TFullFPCInstallationTests);
end.

