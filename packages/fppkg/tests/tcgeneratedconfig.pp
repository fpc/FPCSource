{ Tests for the detection of the compiler installation and for the repositories of a generated fppkg.cfg.
  The compilers are shell scripts that answer -iVTPTO and -PB, so the tests run on Unix only. }
unit tcGeneratedConfig;

{$mode objfpc}{$H+}
{$WARN SYMBOL_DEPRECATED OFF}

interface

uses
  Classes, SysUtils, fpcunit, testregistry
{$ifdef unix}
  , BaseUnix, fpmkunit, pkgoptions
{$endif}
  ;

type

  { TTestGeneratedConfig }

  TTestGeneratedConfig = class(TTestCase)
{$ifdef unix}
  private
    FDir: string;
    procedure DeleteTree(const aDir: string);
    procedure WriteLines(const aFilename: string; const aLines: array of string);
    procedure MakeDir(const aDir: string);
    procedure MakeEmptyFile(const aFilename: string);
    procedure MakeSymLink(const aTarget, aLinkName: string);
    procedure MakeCompiler(const aFilename, aPBOutput: string);
    function DetectFor(const aCompiler: string): TCompilerOptions;
    function MakeDebianLayout: string;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestDebianLayout;
    procedure TestStandardLayout;
    procedure TestCompilerInInstallDir;
    procedure TestFallbackWithoutUnits;
    procedure TestDefaultRepositories;
    procedure TestDefaultRepositoriesWithoutCompiler;
    procedure TestIncludeFilesRoundTrip;
{$endif}
  end;

implementation

{$ifdef unix}

{ TTestGeneratedConfig }

procedure TTestGeneratedConfig.DeleteTree(const aDir: string);

var
  lInfo: TSearchRec;

begin
  if FindFirst(aDir+AllFilesMask,faAnyFile or faSymLink,lInfo)=0 then
    try
      repeat
        if (lInfo.Name='.') or (lInfo.Name='..') then
          continue;
        if ((lInfo.Attr and faDirectory)<>0) and ((lInfo.Attr and faSymLink)=0) then
          DeleteTree(aDir+lInfo.Name+PathDelim)
        else
          DeleteFile(aDir+lInfo.Name);
      until FindNext(lInfo)<>0;
    finally
      FindClose(lInfo);
    end;
  RemoveDir(aDir);
end;


procedure TTestGeneratedConfig.WriteLines(const aFilename: string; const aLines: array of string);

var
  lList: TStringList;
  lLine: string;

begin
  lList:=TStringList.Create;
  try
    for lLine in aLines do
      lList.Add(lLine);
    lList.SaveToFile(aFilename);
  finally
    lList.Free;
  end;
end;


procedure TTestGeneratedConfig.MakeDir(const aDir: string);

begin
  AssertTrue('Create directory '+aDir,ForceDirectories(aDir));
end;


procedure TTestGeneratedConfig.MakeEmptyFile(const aFilename: string);

begin
  WriteLines(aFilename,[]);
end;


procedure TTestGeneratedConfig.MakeSymLink(const aTarget, aLinkName: string);

begin
  AssertEquals('Create symbolic link '+aLinkName,0,fpSymLink(PChar(aTarget),PChar(aLinkName)));
end;


procedure TTestGeneratedConfig.MakeCompiler(const aFilename, aPBOutput: string);

begin
  WriteLines(aFilename,[
    '#!/bin/sh',
    'case "$1" in',
    '  -iVTPTO) echo "3.2.2 x86_64 linux" ;;',
    '  -PB) echo "'+aPBOutput+'" ;;',
    'esac']);
  AssertEquals('Make '+aFilename+' executable',0,fpChmod(aFilename,&755));
end;


function TTestGeneratedConfig.DetectFor(const aCompiler: string): TCompilerOptions;

begin
  Result:=TCompilerOptions.Create;
  Result.Compiler:=aCompiler;
  Result.InitCompilerDefaults;
end;


function TTestGeneratedConfig.MakeDebianLayout: string;

var
  lInstallDir: string;

begin
  lInstallDir:=FDir+'usr/lib/x86_64-linux-gnu/fpc/3.2.2/';
  MakeDir(lInstallDir+'units/x86_64-linux/rtl');
  MakeDir(FDir+'usr/bin');
  MakeEmptyFile(lInstallDir+'ppcx64');
  MakeSymLink('../lib/x86_64-linux-gnu/fpc/3.2.2/ppcx64',FDir+'usr/bin/ppcx64');
  Result:=FDir+'usr/bin/fpc';
  MakeCompiler(Result,FDir+'usr/bin/ppcx64');
end;


procedure TTestGeneratedConfig.SetUp;

begin
  if GetEnvironmentVariable('FPCDIR')<>'' then
    Ignore('FPCDIR is set and overrides the detected install directory');
  inherited SetUp;
  // fppkg creates the fpmkunit defaults at startup, TCompilerOptions relies on them
  if Defaults=nil then
    Defaults:=TFPCDefaults.Create;
  // the lib component checks that the prefix is cut at the last /lib/
  FDir:=IncludeTrailingPathDelimiter(GetTempDir(false))+'tgenconfig'+IntToStr(GetProcessID)+'/lib/root/';
  MakeDir(FDir);
end;


procedure TTestGeneratedConfig.TearDown;

begin
  if FDir<>'' then
    DeleteTree(ExtractFilePath(ExcludeTrailingPathDelimiter(ExtractFilePath(ExcludeTrailingPathDelimiter(FDir)))));
  inherited TearDown;
end;


procedure TTestGeneratedConfig.TestDebianLayout;

var
  lOptions: TCompilerOptions;

begin
  lOptions:=DetectFor(MakeDebianLayout);
  try
    AssertEquals('Compiler version','3.2.2',lOptions.CompilerVersion);
    AssertEquals('Global prefix',FDir+'usr/',lOptions.GlobalPrefix);
    AssertEquals('Global install directory',FDir+'usr/lib/x86_64-linux-gnu/fpc/3.2.2/',lOptions.GlobalInstallDir);
  finally
    lOptions.Free;
  end;
end;


procedure TTestGeneratedConfig.TestStandardLayout;

var
  lOptions: TCompilerOptions;

begin
  MakeDir(FDir+'local/lib/fpc/3.2.2/units/x86_64-linux/rtl');
  MakeEmptyFile(FDir+'local/lib/fpc/3.2.2/ppcx64');
  MakeDir(FDir+'local/bin');
  MakeCompiler(FDir+'local/bin/fpc',FDir+'local/lib/fpc/3.2.2/ppcx64');
  lOptions:=DetectFor(FDir+'local/bin/fpc');
  try
    AssertEquals('Global prefix',FDir+'local/',lOptions.GlobalPrefix);
    AssertEquals('Global install directory',FDir+'local/lib/fpc/3.2.2/',lOptions.GlobalInstallDir);
  finally
    lOptions.Free;
  end;
end;


procedure TTestGeneratedConfig.TestCompilerInInstallDir;

var
  lOptions: TCompilerOptions;

begin
  MakeDir(FDir+'opt/fpc/3.2.2/units/x86_64-linux/rtl');
  MakeCompiler(FDir+'opt/fpc/3.2.2/ppcx64','Error: Illegal parameter: -PB');
  lOptions:=DetectFor(FDir+'opt/fpc/3.2.2/ppcx64');
  try
    AssertEquals('Global install directory from the compiler path',FDir+'opt/fpc/3.2.2/',lOptions.GlobalInstallDir);
  finally
    lOptions.Free;
  end;
end;


procedure TTestGeneratedConfig.TestFallbackWithoutUnits;

var
  lOptions: TCompilerOptions;

begin
  MakeDir(FDir+'bare/bin');
  MakeCompiler(FDir+'bare/bin/fpc',FDir+'bare/bin/ppcx64');
  lOptions:=DetectFor(FDir+'bare/bin/fpc');
  try
    AssertTrue('Fallback prefix /usr/local/ or /usr/, got '+lOptions.GlobalPrefix,
      (lOptions.GlobalPrefix='/usr/local/') or (lOptions.GlobalPrefix='/usr/'));
  finally
    lOptions.Free;
  end;
end;


procedure TTestGeneratedConfig.TestDefaultRepositories;

var
  lCompilerOptions: TCompilerOptions;
  lOptions: TFppkgOptions;
  lText: TStringList;

begin
  lCompilerOptions:=DetectFor(MakeDebianLayout);
  lOptions:=TFppkgOptions.Create;
  lText:=TStringList.Create;
  try
    lOptions.AddDefaultRepositories(lCompilerOptions);
    lOptions.SaveToFile(FDir+'fppkg.cfg');
    lText.LoadFromFile(FDir+'fppkg.cfg');
    AssertTrue('InstallRepository',lText.IndexOf('InstallRepository=user')>=0);
    AssertTrue('fpc repository',lText.IndexOf('Name=fpc')>=0);
    AssertTrue('fpc repository path keeps {CompilerVersion}: '+lText.Text,
      lText.IndexOf('Path='+FDir+'usr/lib/x86_64-linux-gnu/fpc/{CompilerVersion}/')>=0);
    AssertTrue('fpc repository prefix',lText.IndexOf('Prefix='+FDir+'usr/')>=0);
    AssertTrue('user repository',lText.IndexOf('Name=user')>=0);
    AssertTrue('user repository path',lText.IndexOf('Path={LocalRepository}lib/fpc/{CompilerVersion}/')>=0);
    AssertTrue('user repository prefix',lText.IndexOf('Prefix={LocalRepository}')>=0);
    AssertTrue('IncludeFiles section',lText.IndexOf('[IncludeFiles]')>=0);
    AssertEquals('IncludeFiles mask follows the section','FileMask={LocalRepository}config/conf.d/*.conf',
      lText[lText.IndexOf('[IncludeFiles]')+1]);
  finally
    lText.Free;
    lOptions.Free;
    lCompilerOptions.Free;
  end;
end;


procedure TTestGeneratedConfig.TestDefaultRepositoriesWithoutCompiler;

var
  lOptions: TFppkgOptions;
  lText: TStringList;

begin
  lOptions:=TFppkgOptions.Create;
  lText:=TStringList.Create;
  try
    lOptions.AddDefaultRepositories(nil);
    lOptions.SaveToFile(FDir+'fppkg.cfg');
    lText.LoadFromFile(FDir+'fppkg.cfg');
    AssertTrue('No fpc repository',lText.IndexOf('Name=fpc')<0);
    AssertTrue('user repository',lText.IndexOf('Name=user')>=0);
    AssertTrue('InstallRepository',lText.IndexOf('InstallRepository=user')>=0);
    AssertTrue('IncludeFiles mask',lText.IndexOf('FileMask={LocalRepository}config/conf.d/*.conf')>=0);
  finally
    lText.Free;
    lOptions.Free;
  end;
end;


procedure TTestGeneratedConfig.TestIncludeFilesRoundTrip;

var
  lOptions: TFppkgOptions;
  lText: TStringList;

begin
  MakeDir(FDir+'conf.d');
  WriteLines(FDir+'in.cfg',[
    '[Global]',
    'ConfigVersion=5',
    '[IncludeFiles]',
    'FileMask='+FDir+'conf.d/*.conf',
    'File='+FDir+'extra.conf']);
  lOptions:=TFppkgOptions.Create;
  lText:=TStringList.Create;
  try
    lOptions.LoadFromFile(FDir+'in.cfg');
    lOptions.SaveToFile(FDir+'out.cfg');
    lText.LoadFromFile(FDir+'out.cfg');
    AssertTrue('FileMask kept: '+lText.Text,lText.IndexOf('FileMask='+FDir+'conf.d/*.conf')>=0);
    AssertTrue('File kept: '+lText.Text,lText.IndexOf('File='+FDir+'extra.conf')>=0);
  finally
    lText.Free;
    lOptions.Free;
  end;
end;

{$endif unix}

initialization
  RegisterTest(TTestGeneratedConfig);
end.
