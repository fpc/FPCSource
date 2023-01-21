unit tciotuils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, system.ioutils;

type

  { TTestTPath }

  TTestIO = class(TTestCase)
  Private
    FCWD : String;
    FBaseDir : String;
  protected
    Procedure CreateTestDirs;
    Procedure CleanDirs(aDir : String);
    procedure SetUp; override;
    procedure TearDown; override;
    Property CWD : String Read FCWD;
    Property BaseDir : String Read FBaseDir;
  end;

  TTestTPath = Class(TTestIO)
  Published
    Procedure TestIsValidPathChar;
    Procedure TestIsValidFileNameChar;
    Procedure TestHasValidPathChars;
    Procedure TestHasValidFileNameChars;
    Procedure TestGetExtendedPrefix;
    Procedure TestIsDriveRooted;
    Procedure TestIsExtendedPrefixed;
    Procedure TestIsRelativePath;
    Procedure TestIsUNCPath;
    Procedure TestIsUNCRooted;
    Procedure TestGetGUIDFileName;
    Procedure TestDriveExists;
    Procedure TestMatchesPattern;
    Procedure TestChangeExtension;
    Procedure TestCombine;
    Procedure TestCombineMulti;
    Procedure TestGetDirectoryName;
    Procedure TestGetExtension;
    Procedure TestGetFileName;
    Procedure TestGetFileNameWithoutExtension;
    Procedure TestGetFullPath;
    Procedure TestGetInvalidFileNameChars;
    Procedure TestGetInvalidPathChars;
    Procedure TestGetPathRoot;
    Procedure TestGetRandomFileName;
    Procedure TestGetTempFileName;
    Procedure TestGetTempPath;
    Procedure TestGetHomePath;
    Procedure TestGetDocumentsPath;
    Procedure TestGetSharedDocumentsPath;
    Procedure TestGetLibraryPath;
    Procedure TestGetCachePath;
    Procedure TestGetPublicPath;
    Procedure TestGetPicturesPath;
    Procedure TestGetSharedPicturesPath;
    Procedure TestGetCameraPath;
    Procedure TestGetSharedCameraPath;
    Procedure TestGetMusicPath;
    Procedure TestGetSharedMusicPath;
    Procedure TestGetMoviesPath;
    Procedure TestGetSharedMoviesPath;
    Procedure TestGetAlarmsPath;
    Procedure TestGetSharedAlarmsPath;
    Procedure TestGetDownloadsPath;
    Procedure TestGetSharedDownloadsPath;
    Procedure TestGetRingtonesPath;
    Procedure TestGetSharedRingtonesPath;
    Procedure TestGetTemplatesPath;
    Procedure TestGetSetAttributes;
    Procedure TestHasExtension;
    Procedure TestIsPathRooted;
    Procedure TestExtensionSeparatorChar;
    Procedure TestAltDirectorySeparatorChar;
    Procedure TestDirectorySeparatorChar;
    Procedure TestPathSeparator;
    Procedure TestVolumeSeparatorChar;
  end;

implementation


procedure TTestTPath.TestIsValidPathChar;

Var
  C : AnsiChar;

begin
  For C:=#0 to #31 do
    AssertFalse('Char #'+intToStr(Ord(C)),TPath.IsValidPathChar(C));
 {$IFNDEF UNIX}
 AssertFalse('Char "',TPath.IsValidPathChar('"'));
 AssertFalse('Char <',TPath.IsValidPathChar('<'));
 AssertFalse('Char >',TPath.IsValidPathChar('>'));
 AssertFalse('Char |',TPath.IsValidPathChar('|'));
 {$ENDIF}
end;

procedure TTestTPath.TestIsValidFileNameChar;
Var
  C : AnsiChar;

begin
  For C:=#0 to #31 do
    AssertFalse('Char #'+intToStr(Ord(C)),TPath.IsValidPathChar(C));
 {$IFNDEF UNIX}
 {$IFDEF WINDOWS}
 AssertFalse('Char /',TPath.IsValidFileNameChar('/'));
 AssertFalse('Char *',TPath.IsValidFileNameChar('*'));
 AssertFalse('Char :',TPath.IsValidFileNameChar(':'));
 AssertFalse('Char >',TPath.IsValidFileNameChar('>'));
 AssertFalse('Char <',TPath.IsValidFileNameChar('<'));
 AssertFalse('Char ?',TPath.IsValidFileNameChar('?'));
 AssertFalse('Char \',TPath.IsValidFileNameChar('\'));
 AssertFalse('Char |',TPath.IsValidFileNameChar('|'));
 {$ENDIF}
 {$ELSE}
 AssertFalse('Char /',TPath.IsValidFileNameChar('/'));
 AssertFalse('Char ~',TPath.IsValidFileNameChar('~'));
 {$ENDIF}

end;

procedure TTestTPath.TestHasValidPathChars;

  procedure testok(s: string);

  begin
     AssertTrue(S+' is OK',TPath.HasValidPathChars(S,False))
  end;

  procedure testnok(s: string);

  begin
     AssertFalse(S+' is NOK',TPath.HasValidPathChars(S,False))
  end;

var
  C : Char;

begin
   TestOK('abcde12');
   TestOK('abcde/12');
   TestNOK(#10'abcde12');
   For C:=#0 to #31 do
     TestNOK(C+'abcde12');
end;

procedure TTestTPath.TestHasValidFileNameChars;
  procedure testok(s: string);

  begin
     AssertTrue(S+' is OK',TPath.HasValidFIleNameChars(S,False))
  end;

  procedure testnok(s: string);

  begin
     AssertFalse(S+' is NOK',TPath.HasValidFileNameChars(S,False))
  end;

var
  C : Char;

begin
   TestOK('abcde12');
   TestNOK('abcde/12');
   TestNOK(#10'abcde12');
   For C:=#0 to #31 do
     TestNOK(C+'abcde12');

end;

procedure TTestTPath.TestGetExtendedPrefix;

  Procedure TestIt(aExpected : TPathPrefixType; aPath : String);

  Var
    S : String;

  begin
    DoDirSeparators(aPath);
    Str(aExpected,S);
    {$IFNDEF WINDOWS}
    aExpected:=pptNoPrefix;
    {$ENDIF}
    AssertTrue(aPath+' -> '+S,aExpected=TPath.GetExtendedPrefix(aPath));
  end;

begin
  TestIt(pptNoPrefix,'/a/b/c.txt');
  TestIt(pptExtended,'//?/a/b/c.txt');
  TestIt(pptExtendedUNC,'//?/UNC/a/b/c.txt');
end;

procedure TTestTPath.TestIsDriveRooted;
  Procedure TestIt(aExpected : Boolean; aPath : String);

  Var
    S : String;

  begin
    DoDirSeparators(aPath);
    Str(aExpected,S);
    {$IFNDEF WINDOWS}
    aExpected:=False;
    {$ENDIF}
    AssertTrue(aPath+' -> '+S,aExpected=TPath.IsDriveRooted(aPath));
  end;

begin
  TestIt(True,'c:/me/you.txt');
  TestIt(True,'A:/me/you.txt');
  TestIt(False,'1:/me/you.txt');
  TestIt(False,'/me/you.txt');

end;

procedure TTestTPath.TestIsExtendedPrefixed;

  Procedure TestIt(aExpected : Boolean; aPath : String);

  Var
    S : String;

  begin
    DoDirSeparators(aPath);
    Str(aExpected,S);
    {$IFNDEF WINDOWS}
    aExpected:=False;
    {$ENDIF}
    AssertTrue(aPath+' -> '+S,aExpected=TPath.IsExtendedPrefixed(aPath));
  end;

begin
  TestIt(False,'/a/b/c.txt');
  TestIt(True,'//?/a/b/c.txt');
  TestIt(True,'//?/UNC/a/b/c.txt');

end;

procedure TTestTPath.TestIsRelativePath;

  Procedure TestIt(aExpected : Boolean; aPath : String);

   Var
     S : String;

   begin
     DoDirSeparators(aPath);
     Str(aExpected,S);
     AssertTrue(aPath+' -> '+S,aExpected=TPath.IsRelativePath(aPath));
   end;

 begin
   TestIt(False,'/a/b/c.txt');
   TestIt(True,'a/b/c.txt');
   TestIt(True,'../a/b/c.txt');
   TestIt(True,'./a/b/c.txt');

end;

procedure TTestTPath.TestIsUNCPath;

  Procedure TestIt(aExpected : Boolean; aPath : String);

  Var
    S : String;

  begin
    DoDirSeparators(aPath);
    Str(aExpected,S);
    {$IFNDEF WINDOWS}
    aExpected:=False;
    {$ENDIF}
    AssertTrue(aPath+' -> '+S,aExpected=TPath.IsUNCPath(aPath));
  end;

begin
  TestIt(False,'/a/b/c.txt');
  TestIt(False,'a/b/c.txt');
  TestIt(True,'//a/b/c.txt');
  TestIt(True,'//?/a/b/c.txt');
  TestIt(True,'//?/UNC/a/b/c.txt');


end;

procedure TTestTPath.TestIsUNCRooted;
  Procedure TestIt(aExpected : Boolean; aPath : String);

  Var
    S : String;

  begin
    DoDirSeparators(aPath);
    Str(aExpected,S);
    {$IFNDEF WINDOWS}
    aExpected:=False;
    {$ENDIF}
    AssertTrue(aPath+' -> '+S,aExpected=TPath.IsUNCRooted(aPath));
  end;

begin
  TestIt(False,'//a/b/c.txt');
  TestIt(False,'/a/b/c.txt');
  TestIt(False,'//a/');
  TestIt(True,'//?/a/b/c.txt');
  TestIt(True,'//?/UNC/a/b/c.txt');
end;

procedure TTestTPath.TestGetGUIDFileName;

var
  G : TGUID;
   S : String;
begin
  S:=TPath.GetGUIDFileName(True);
  S:='{'+S+'}';
  AssertTrue('Correct GUID1',TryStringToGUID(S,G));
  S:=TPath.GetGUIDFileName(False);
  //['{AC1AF8B9-C050-4D5E-86FD-199A72E93313}']
  System.Insert('-',S,21);
  Insert('-',S,17);
  Insert('-',S,13);
  Insert('-',S,9);
  S:='{'+S+'}';
  AssertTrue('Correct GUID2',TryStringToGUID(S,G));
end;

procedure TTestTPath.TestDriveExists;

Var
   C : String;
begin
  {$IFDEF WINDOWS}
  C:=GetEnvironmentVariable('SYSTEMDRIVE');
  AssertTrue('Systemdrive',TPath.DriveExists(C));
  C:=GetEnvironmentVariable('SYSTEMROOT');
  AssertTrue('Systemroot',TPath.DriveExists(C));
  {$ELSE}
  C:='/tmp';
  AssertFalse('Unix',TPath.DriveExists(C));
  {$ENDIF}
end;

procedure TTestTPath.TestMatchesPattern;

  Procedure TestIt(aResult : Boolean; const aFileName,aPattern : string);
  begin
    AssertEquals(aFIleName+' matches '+aPattern,aResult,TPath.MatchesPattern(aFileName,aPattern,True));
  end;

begin
  TestIt(True,'a.txt','?.txt');
  TestIt(False,'ab','?');
  TestIt(True,'ab','*');
  TestIt(True,'abc','*c*');

end;

procedure TTestTPath.TestChangeExtension;
begin
  AssertEquals('1','a.txt',TPath.ChangeExtension('a.doc','.txt'));
  AssertEquals('2','a',TPath.ChangeExtension('a.doc',''));
end;

procedure TTestTPath.TestCombine;

  Procedure TestIt(aResult,aPath,aFile : String);

  begin
    DoDirSeparators(aResult);
    DoDirSeparators(aPath);
    DoDirSeparators(aFile);
    AssertEquals(aPath+'+'+aFile,aResult,TPath.Combine(aPath,aFile));
  end;

begin
  TestIt('a.txt','','a.txt');
  TestIt('/path','/path','');
  TestIt('/path/','/path/','');
  TestIt('/path/a.doc','/path','a.doc');
  TestIt('/path/a.doc','/path/','a.doc');
end;

procedure TTestTPath.TestCombineMulti;

  procedure DoTest(const Paths: array of String; Validate: Boolean; Expected: string; ExceptionExpected: Boolean=False);

    function ArgsToStr: string;
    var
      i: Integer;
    begin
      Result := '';
      for i := Low(Paths) to High(Paths) do
        Result := Result+''''+Paths[i] + ''',';
      if (Result <> '') then SetLength(Result, Length(Result)-1);
      Result := '['+Result+']';
    end;

  var
    Res,FailMsg: String;
    P : Array of string;
    I : Integer;

  begin
    FailMsg:='';
    try
      SetLength(P,Length(Paths));
      for I:=0 to Length(Paths)-1 do
        begin
        P[i]:=Paths[i];
        DoDirSeparators(P[i]);
        end;
      DoDirSeparators(Expected);
      Res := TPath.Combine(P,Validate);
      AssertEquals(ArgsToStr,Expected,Res)
    except
      on E: Exception do
        if not ExceptionExpected then
          FailMsg:=Format('%s : an unexpected exception %s occurred: %s',[ArgsToStr,E.ClassName,E.Message])
    end;
    if FailMsg<>'' then
      Fail(FailMsg);
  end;

  var
    S: String;

begin
  //EInOutError
  DoTest([''],True,'');
  DoTest(['',''],True,'');
  DoTest(['','',''],True,'');
  DoTest(['a','b','c'],True,'a\b\c');
  DoTest(['a','b','\c'],True,'\c');
  DoTest(['a','\b','c'],True,'\b\c');
  DoTest(['\a','\b','c'],True,'\b\c');
  DoTest(['\a','\b','\c'],True,'\c');
  DoTest(['\a','b','\c:'],True,'\c:');
  DoTest(['a','<>','\b','c','\d'],True,'',True);
  {$IFDEF WINDOWS}
  DoTest(['c:','a','b'],True,'c:a\b',False);
  {$ENDIF}
  DoTest(['\1','..\2','..\3','..4'],True,'\1\..\2\..\3\..4');
  DoTest(['\1','','','4','','6',''],True,'\1\4\6');
  DoTest(['','','','<>|'],True,'<>|',True);
  DoTest([''],False,'');
  DoTest(['',''],False,'');
  DoTest(['','',''],False,'');
  DoTest(['a','b','c'],False,'a\b\c');
  DoTest(['a','b','\c'],False,'\c');
  DoTest(['a','\b','c'],False,'\b\c');
  DoTest(['\a','\b','c'],False,'\b\c');
  DoTest(['\a','\b','\c'],False,'\c');
  DoTest(['\a','b','\c:'],False,'\c:');
  DoTest(['a','<>','\b','c','\d'],False,'\d',False);
end;

procedure TTestTPath.TestGetDirectoryName;

  Procedure TestIt(aResult,aFile : String);

  begin
    DoDirSeparators(aResult);
    DoDirSeparators(aFile);
    AssertEquals(aFile,aResult,TPath.GetDirectoryName(aFile));
  end;

begin
  TestIt('/a','/a/b.txt');
  TestIt('','b.txt');
  TestIt('.','./b.txt');

end;

procedure TTestTPath.TestGetExtension;

  Procedure TestIt(aResult,aFile : String);

  begin
    DoDirSeparators(aResult);
    DoDirSeparators(aFile);
    AssertEquals(aFile,aResult,TPath.GetExtension(aFile));
  end;

begin
  TestIt('.txt','/a/b.txt');
  TestIt('.txt','b.txt');
  TestIt('','.txt');

end;

procedure TTestTPath.TestGetFileName;
  Procedure TestIt(aResult,aFile : String);

  begin
    DoDirSeparators(aResult);
    DoDirSeparators(aFile);
    AssertEquals(aFile,aResult,TPath.GetFileName(aFile));
  end;

begin
  TestIt('b.txt','/a/b.txt');
  TestIt('b.txt','b.txt');
  TestIt('.txt','.txt');

end;

procedure TTestTPath.TestGetFileNameWithoutExtension;


  Procedure TestIt(aResult,aFile : String);

  begin
    DoDirSeparators(aResult);
    DoDirSeparators(aFile);
    AssertEquals(aFile,aResult,TPath.GetFileNameWithoutExtension(aFile));
  end;

begin
  TestIt('b','/a/b.txt');
  TestIt('b','b.txt');
  TestIt('.txt','.txt');

end;

procedure TTestTPath.TestGetFullPath;

  Procedure TestIt(aResult,aFile : String);

  begin
    DoDirSeparators(aResult);
    DoDirSeparators(aFile);
    AssertEquals(aFile,aResult,TPath.GetFullPath(aFile));
  end;

begin
  TestIt('/a/b.txt','/a/b.txt');
  TestIt(CWD+'b.txt','b.txt');
  TestIt(CWD+'a/b.txt','a/b.txt');

end;

Function IndexOf(C : Char; A : TCharArray) : Integer; overload;

Var
   Len : Integer;
begin
  Result:=0;
  Len:=Length(A);
  While (Result<Len) and (A[Result]<>C) do
   Inc(Result);
  if Result>=Len then
    Result:=-1;
end;


procedure TTestTPath.TestGetInvalidFileNameChars;


Const
  {$IFDEF UNIX}
    CExtraInvalid = '/~';
  {$ELSE}
    CExtraInvalid = '"*/:<>?\|';
  {$ENDIF}

Var
  CA : TCharArray;
  C : Char;
  I,P : Integer;

begin
  CA:=TPath.GetInvalidFileNameChars;
  For C:=#0 to #31 do
    begin
    P:=IndexOf(C,CA);
    AssertTrue('1 Have #'+IntToStr(Ord(C)),P<>-1);
    System.Delete(CA,P,1);
    end;
  For C in CExtraInvalid do
    begin
    P:=IndexOf(C,CA);
    AssertTrue('2 Have #'+IntToStr(Ord(C)),P<>-1);
    System.Delete(CA,P,1);
    end;

  AssertEquals('All characters accounted for',0,Length(CA));
end;

procedure TTestTPath.TestGetInvalidPathChars;
Const
  {$IFDEF UNIX}
    CExtraInvalid = '"<>|';
  {$ELSE}
    CExtraInvalid = '';
  {$ENDIF}

Var
  CA : TCharArray;
  C : Char;
  I,P : Integer;

begin
  CA:=TPath.GetInvalidPathChars;
  For C:=#0 to #31 do
    begin
    P:=IndexOf(C,CA);
    AssertTrue('1 Have #'+IntToStr(Ord(C)),P<>-1);
    System.Delete(CA,P,1);
    end;
  For C in CExtraInvalid do
    begin
    P:=IndexOf(C,CA);
    AssertTrue('2 Have #'+IntToStr(Ord(C)),P<>-1);
    System.Delete(CA,P,1);
    end;
  AssertEquals('All characters accounted for',0,Length(CA));
end;

procedure TTestTPath.TestGetPathRoot;

  Procedure TestIt(aResult,aFile : string);

  begin
    DoDirSeparators(aResult);
    DoDirSeparators(aFile);
    AssertEquals(aFile+' -> '+aResult,aResult,TPath.GetPathRoot(aFIle));
  end;

begin
  TestIt('/','/a/b/c.txt');
  {$IFDEF WINDOWS}
  TestIt('a:/','a:/b/c.txt');
  TestIt('//a/','//a/b/c.txt');
  {$ENDIF}
end;

procedure TTestTPath.TestGetRandomFileName;

Const
  allowed = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789.';

Var
  FN : String;
  C : Char;

begin
  FN:=TPath.GetRandomFileName;
  AssertEquals('Have correct length',12,Length(FN));
  AssertTrue('Have dot',(Pos('.',FN)<>0) and (Pos('.',FN)<Length(FN)));
  For C in FN do
    if (Pos(Upcase(C),Allowed)=0) then
      Fail('Invalid char in string: '+c);
end;

procedure TTestTPath.TestGetTempFileName;

Var
  aDir,FN : String;

begin
  FN:=GetTempFileName;
  AssertEquals('Path',TPath.GetTempPath,ExtractFilePath(FN));
  FN:=ExtractFileName(FN);
  AssertTrue('Not empty name',0<Length(FN));
  AssertTrue('Not empty extension',0<Length(ExtractFileExt(FN)));
  aDir:=BaseDir;
  DoDirSeparators(aDir);
  FN:=GetTempFileName(aDir,'ttt');
  AssertEquals('2 Path',adir,ExtractFilePath(FN));
  FN:=ExtractFileName(FN);
  AssertTrue('2 Not empty name',0<Length(FN));
  AssertEquals('Prefix',1,Pos('ttt',FN));
  AssertTrue('2 Not empty extension',0<Length(ExtractFileExt(FN)));
end;

procedure TTestTPath.TestGetTempPath;

Var
  FN : String;

begin
  FN:=TPath.GetTempPath;
  AssertTrue('Dir exists',DirectoryExists(FN));
end;

procedure TTestTPath.TestGetHomePath;
Var
  FN : String;

begin
  FN:=TPath.GetHomePath;
  AssertTrue('Dir exists',DirectoryExists(FN));
  AssertEquals('UserDir exists',GetUserDir,FN);

end;

{$IFDEF UNIX}
//Names : array[TSpecialDir] of string
//      = ('DESKTOP', 'DOCUMENTS', 'DOWNLOAD',  'MUSIC', 'PICTURES', 'PUBLICSHARE', 'TEMPLATES', 'VIDEOS');


function UnixSpecialDir(const AType: String): string;

var
  cfg,varname: string;
  L: TStringList;
begin
  Result := '';
  // XDG variable name
  varName:=Format('XDG_%s_DIR',[UpperCase(AType)]);
  Cfg:=GetEnvironmentVariable('XDG_CONFIG_HOME');
  if (Cfg='') then
    Cfg:=GetUserDir+'.config/user-dirs.dirs'
  else
    CFG:=CFG+'user-dirs.dirs';
  if not FileExists(Cfg) then
    Exit;
  L:=TStringList.Create;
  try
    L.LoadFromFile(Cfg);
    Result:=AnsiDequotedStr(L.Values[VarName],'"');
  finally
    FreeAndNil(L);
  end;
  Result:=StringReplace(Result,'$HOME', ExcludeTrailingPathDelimiter(GetUserDir), [rfIgnoreCase]);
end;
{$ENDIF}

procedure TTestTPath.TestGetDocumentsPath;

Var
  FN : String;

begin
  FN:=TPath.GetDocumentsPath;
{$IFDEF UNIX}
  AssertEquals(UnixSpecialDir('Documents'),FN);
{$ELSE}
{$IFDEF WINDOWS}
  AssertEquals(GetSpecialDir(CSIDL_PERSONAL),FN);
{$ELSE}
  AssertEquals(GetUserDir,FN);
{$ENDIF}
{$ENDIF}
end;

procedure TTestTPath.TestGetSharedDocumentsPath;

Var
  FN : String;

begin
  FN:=TPath.GetSharedDocumentsPath;
{$IFDEF UNIX}
  AssertEquals(UnixSpecialDir('PublicShare'),FN);
{$ELSE}
{$IFDEF WINDOWS}
  AssertEquals(GetSpecialDir(CSIDL_COMMON_DOCUMENTS),FN);
{$ELSE}
  AssertEquals(GetUserDir,FN);
{$ENDIF}
{$ENDIF}

end;

procedure TTestTPath.TestGetLibraryPath;

Var
  FN : String;

begin
  FN:=TPath.GetLibraryPath;
{$IFDEF UNIX}
  AssertEquals(GetCurrentDir,FN);
{$ELSE}
{$IFDEF WINDOWS}
  AssertEquals(ExtractFilePath(Paramstr(0)),FN);
{$ELSE}
  AssertEquals(GetUserDir,FN);
{$ENDIF}
{$ENDIF}

end;

procedure TTestTPath.TestGetCachePath;
Var
  FN : String;

begin
  FN:=TPath.GetCachePath;
{$IFDEF UNIX}
  AssertEquals(GetUserDir+'.cache',FN);
{$ELSE}
{$IFDEF WINDOWS}
  AssertEquals(GetSpecialDir(CSIDL_LOCAL_APPDATA),FN);
{$ELSE}
  AssertEquals(GetTempDir,FN);
{$ENDIF}
{$ENDIF}

end;

procedure TTestTPath.TestGetPublicPath;
Var
  FN : String;

begin
  FN:=TPath.GetPublicPath;
{$IFDEF UNIX}
  AssertEquals(UnixSpecialDir('PublicShare'),FN);
{$ELSE}
{$IFDEF WINDOWS}
  AssertEquals(GetSpecialDir(CSIDL_COMMON_APPDATA),FN);
{$ELSE}
  AssertEquals(GetUserDir,FN);
{$ENDIF}
{$ENDIF}

end;

procedure TTestTPath.TestGetPicturesPath;
Var
  FN : String;

begin
  FN:=TPath.GetPicturesPath;
{$IFDEF UNIX}
  AssertEquals(UnixSpecialDir('Pictures'),FN);
{$ELSE}
{$IFDEF WINDOWS}
  AssertEquals(GetSpecialDir(CSIDL_MYPICTURES),FN);
{$ELSE}
  AssertEquals(GetUserDir,FN);
{$ENDIF}
{$ENDIF}

end;

procedure TTestTPath.TestGetSharedPicturesPath;
Var
  FN : String;

begin
  FN:=TPath.GetSharedPicturesPath;
{$IFDEF UNIX}
  AssertEquals(UnixSpecialDir('PublicShare'),FN);
{$ELSE}
{$IFDEF WINDOWS}
  AssertEquals(GetSpecialDir(CSIDL_COMMON_PICTURES),FN);
{$ELSE}
  AssertEquals(GetUserDir,FN);
{$ENDIF}
{$ENDIF}

end;

procedure TTestTPath.TestGetCameraPath;
Var
  FN : String;

begin
  FN:=TPath.GetPicturesPath;
{$IFDEF UNIX}
  AssertEquals(UnixSpecialDir('Pictures'),FN);
{$ELSE}
{$IFDEF WINDOWS}
  AssertEquals(GetSpecialDir(CSIDL_MYPICTURES),FN);
{$ELSE}
  AssertEquals(GetUserDir,FN);
{$ENDIF}
{$ENDIF}
end;

procedure TTestTPath.TestGetSharedCameraPath;
Var
  FN : String;

begin
  FN:=TPath.GetSharedCameraPath;
{$IFDEF UNIX}
  AssertEquals(UnixSpecialDir('PublicShare'),FN);
{$ELSE}
{$IFDEF WINDOWS}
  AssertEquals(GetSpecialDir(CSIDL_COMMON_PICTURES),FN);
{$ELSE}
  AssertEquals(GetUserDir,FN);
{$ENDIF}
{$ENDIF}

end;

procedure TTestTPath.TestGetMusicPath;

Var
  FN : String;

begin
  FN:=TPath.GetMusicPath;
{$IFDEF UNIX}
  AssertEquals(UnixSpecialDir('Music'),FN);
{$ELSE}
{$IFDEF WINDOWS}
  AssertEquals(GetSpecialDir(CSIDL_MYMUSIC),FN);
{$ELSE}
  AssertEquals(GetUserDir,FN);
{$ENDIF}
{$ENDIF}

end;

procedure TTestTPath.TestGetSharedMusicPath;
Var
  FN : String;

begin
  FN:=TPath.GetSharedMusicPath;
{$IFDEF UNIX}
  AssertEquals(UnixSpecialDir('PublicShare'),FN);
{$ELSE}
{$IFDEF WINDOWS}
  AssertEquals(GetSpecialDir(CSIDL_COMMON_MUSIC),FN);
{$ELSE}
  AssertEquals(GetUserDir,FN);
{$ENDIF}
{$ENDIF}
end;

procedure TTestTPath.TestGetMoviesPath;
Var
  FN : String;

begin
  FN:=TPath.GetMoviesPath;
{$IFDEF UNIX}
  AssertEquals(UnixSpecialDir('Videos'),FN);
{$ELSE}
{$IFDEF WINDOWS}
  AssertEquals(GetSpecialDir(CSIDL_MYVIDEO),FN);
{$ELSE}
  AssertEquals(GetUserDir,FN);
{$ENDIF}
{$ENDIF}


end;

procedure TTestTPath.TestGetSharedMoviesPath;
Var
  FN : String;

begin
  FN:=TPath.GetSharedMoviesPath;
{$IFDEF UNIX}
  AssertEquals(UnixSpecialDir('PublicShare'),FN);
{$ELSE}
{$IFDEF WINDOWS}
  AssertEquals(GetSpecialDir(CSIDL_COMMON_VIDEO),FN);
{$ELSE}
  AssertEquals(GetUserDir,FN);
{$ENDIF}
{$ENDIF}

end;

procedure TTestTPath.TestGetAlarmsPath;
Var
  FN : String;

begin
  FN:=TPath.GetAlarmsPath;
{$IFDEF UNIX}
  AssertEquals(UnixSpecialDir('Music'),FN);
{$ELSE}
{$IFDEF WINDOWS}
  AssertEquals(GetSpecialDir(CSIDL_MYMUSIC),FN);
{$ELSE}
  AssertEquals(GetUserDir,FN);
{$ENDIF}
{$ENDIF}
end;

procedure TTestTPath.TestGetSharedAlarmsPath;
Var
  FN : String;

begin
  FN:=TPath.GetSharedAlarmsPath;
{$IFDEF UNIX}
  AssertEquals(UnixSpecialDir('PublicShare'),FN);
{$ELSE}
{$IFDEF WINDOWS}
  AssertEquals(GetSpecialDir(CSIDL_COMMON_MUSIC),FN);
{$ELSE}
  AssertEquals(GetUserDir,FN);
{$ENDIF}
{$ENDIF}

end;

procedure TTestTPath.TestGetDownloadsPath;
Var
  FN : String;

begin
  FN:=TPath.GetDownloadsPath;
{$IFDEF UNIX}
  AssertEquals(UnixSpecialDir('Download'),FN);
{$ELSE}
{$IFDEF WINDOWS}
  AssertEquals(GetSpecialDir(CSIDL_MYMUSIC),FN);
{$ELSE}
  AssertEquals(GetUserDir,FN);
{$ENDIF}
{$ENDIF}

end;

procedure TTestTPath.TestGetSharedDownloadsPath;
Var
  FN : String;

begin
  FN:=TPath.GetSharedDownloadsPath;
{$IFDEF UNIX}
  AssertEquals(UnixSpecialDir('PublicShare'),FN);
{$ELSE}
{$IFDEF WINDOWS}
  AssertEquals(GetSpecialDir(CSIDL_COMMON_APPDATA),FN);
{$ELSE}
  AssertEquals(GetUserDir,FN);
{$ENDIF}
{$ENDIF}
end;

procedure TTestTPath.TestGetRingtonesPath;
Var
  FN : String;

begin
  FN:=TPath.GetAlarmsPath;
{$IFDEF UNIX}
  AssertEquals(UnixSpecialDir('Music'),FN);
{$ELSE}
{$IFDEF WINDOWS}
  AssertEquals(GetSpecialDir(CSIDL_MYMUSIC),FN);
{$ELSE}
  AssertEquals(GetUserDir,FN);
{$ENDIF}
{$ENDIF}
end;

procedure TTestTPath.TestGetSharedRingtonesPath;
Var
  FN : String;

begin
  FN:=TPath.GetSharedRingtonesPath;
{$IFDEF UNIX}
  AssertEquals(UnixSpecialDir('PublicShare'),FN);
{$ELSE}
{$IFDEF WINDOWS}
  AssertEquals(GetSpecialDir(CSIDL_COMMON_MUSIC),FN);
{$ELSE}
  AssertEquals(GetUserDir,FN);
{$ENDIF}
{$ENDIF}
end;

procedure TTestTPath.TestGetTemplatesPath;
Var
  FN : String;

begin
  FN:=TPath.GetTemplatesPath;
{$IFDEF UNIX}
  AssertEquals(UnixSpecialDir('Templates'),FN);
{$ELSE}
{$IFDEF WINDOWS}
  AssertEquals(GetSpecialDir(CSIDL_PERSONAL),FN);
{$ELSE}
  AssertEquals(GetUserDir,FN);
{$ENDIF}
{$ENDIF}

end;

procedure TTestTPath.TestGetSetAttributes;

Const
{$IFDEF UNIX}
  FAS = [faOwnerRead, faOwnerWrite, faOwnerExecute,
         faGroupRead, faGroupWrite, faGroupExecute,
         faOthersRead, faOthersWrite, faOthersExecute,
         faUserIDExecution, faGroupIDExecution, faStickyBit];
{$ENDIF}



Var
  FN : String;
  H : THandle;
  FAG : TFileAttributes;

begin
  FN:=BaseDir+'attrfile.txt';
  if FileExists(FN) then
    DeleteFile(FN);
  h:=FileCreate(Fn);
  try
    {$IFDEF UNIX}
    TPath.SetAttributes(FN,FAS);
    FAG:=TPath.GetAttributes(FN);
    AssertTrue('All attributes set',FAS*FAG=FAG);
    {$ENDIF}
  finally
    FileCLose(h);
    DeleteFile(FN);
  end;
end;


procedure TTestTPath.TestHasExtension;

  Procedure TestIt(aResult : Boolean; aFileName : string);

  begin
    DoDirSeparators(aFileName);
    AssertEquals(aFileName+' has extension',aResult,TPatH.HasExtension(aFileName));
  end;

begin
  TestIt(False,'abc');
  TestIt(True,'abc.def');
  TestIt(True,'/123/abc.def');
  TestIt(True,'123/abc.def');
  TestIt(True,'123.345/abc.def');
  TestIt(False,'123.345/abcdef');
  {$ifdef unix}
  TestIt(False,'.abcdef');
  TestIt(False,'/123/.abcdef');
  {$endif}
end;

procedure TTestTPath.TestIsPathRooted;

  Procedure TestIt(aResult : Boolean; aFileName : string);

  begin
    DoDirSeparators(aFileName);
    AssertEquals(aFileName+' is rooted',aResult,TPatH.IsPathRooted(aFileName));
  end;

begin
  TestIt(False,'abc.def');
  TestIt(False,'ad/abc.def');
  TestIt(False,'../abc.def');
  TestIt(False,'/abc.def');
  TestIt(False,'/a/abc.def');
{$IFDEF WINDOWS}
  TestIt(True,'a:/abc.def');
  TestIt(True,'//share/o/abc.def');
{$ENDIF}
end;

procedure TTestTPath.TestExtensionSeparatorChar;
begin
  AssertEquals('.',TPath.ExtensionSeparatorChar);
end;

procedure TTestTPath.TestAltDirectorySeparatorChar;
begin
  {$ifdef Windows}
    AssertEquals('/',TPath.AltDirectorySeparatorChar);
  {$elseif defined(unix)}
    AssertEquals('\',TPath.AltDirectorySeparatorChar);
  {$else}
    AssertEquals('\',TPath.AltDirectorySeparatorChar);
  {$endif}
end;

procedure TTestTPath.TestDirectorySeparatorChar;
begin
  {$ifdef Windows}
    AssertEquals('\',TPath.DirectorySeparatorChar);
  {$else}
    AssertEquals('/',TPath.DirectorySeparatorChar);
  {$endif}
end;

procedure TTestTPath.TestPathSeparator;

begin
{$ifdef Windows}
  AssertEquals(';',TPath.PathSeparator);
{$else}
  AssertEquals(':',TPath.PathSeparator);
{$endif}
end;

procedure TTestTPath.TestVolumeSeparatorChar;
begin
{$ifdef Windows}
  AssertEquals(':',TPath.VolumeSeparatorChar);
{$else}
  AssertEquals(#0,TPath.VolumeSeparatorChar);
{$endif}
end;

procedure TTestIO.CreateTestDirs;

  procedure DoCreateDir(const aDir : string);

  begin
    if not ForceDirectories(FBaseDir+aDir) then
      Fail('Could not create directory %s',[FBaseDir+aDir]);
  end;

begin
  DoCreateDir('testpath');
  DoCreateDir('testpath/dir1');
  DoCreateDir('testpath/dir2');
  DoCreateDir('testpath/dir3');
end;

procedure TTestIO.CleanDirs(aDir: String);

Var
  Info : TSearchRec;
  lDir,lFull : String;

begin
  lDir:=IncludeTrailingPathDelimiter(aDir);
  If FIndFirst(lDir+AllFilesMask,sysutils.faDirectory,Info)=0 then
    try
      Repeat
        lFull:=lDir+Info.Name;
        if Info.IsDirectory then
          begin
          if not Info.IsCurrentOrParentDir then
            CleanDirs(lFull);
          if not RemoveDir(lFull) then
            Fail('Failed to remove directory %s',[lFull])
          end
        else if not DeleteFIle(lFull) then
          Fail('Failed to remove file %s',[lFull])
      until FIndNext(Info)>0;
    finally
      FindClose(Info);
    end;
end;

procedure TTestIO.SetUp;
begin
  FCWD:=IncludeTrailingPathDelimiter(GetCurrentDir);
  FBaseDir:=GetTempDir(False)+'testio/';
end;

procedure TTestIO.TearDown;
begin
  CleanDirs(FBaseDir);
  ChDir(FCWD);
end;

initialization

  RegisterTest(TTestTPath);
end.

