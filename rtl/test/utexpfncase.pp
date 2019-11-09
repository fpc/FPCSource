unit utexpfncase;

interface
{$MODE OBJFPC}
{$H+}
{$DEFINE FPCTEST}

uses SysUtils;

implementation

uses punit, utrtl;

const
  TestFilesNumber = 3;
{$IFDEF UNIX}
  MinPathLength = 1;
{$ELSE UNIX}
  MinPathLength = 3;
{$ENDIF UNIX}

type
  TTestFiles = array [1..TestFilesNumber] of shortstring;


const
  TestFiles: TTestFiles = ('testFile1.tst', 'testFile2.tst', 'TestFile2.tst');

Procedure TestExpFNC (const FN1, ExpReturn: string; ExpMatch: TFilenameCaseMatch);

var
  FN2: string;
  Match: TFilenameCaseMatch;
  N1,N2 : String;
  
begin
  Str(expmatch,N1);
  FN2 := ExpandFileNameCase (FN1, Match);
  if (Match <> ExpMatch)  or ((ExpReturn <> '') and (FN2 <> ExpReturn) and
     ((Match <> mkAmbiguous) or not (FileNameCaseSensitive) or
      (UpperCase (FN2) <> UpperCase (ExpReturn)))) then
    begin
    Str(Match,N2);
    FailExit('Error: Input = '+ FN1+ ', Output = '+ FN2+ ' (expected '+ExpReturn+'), MatchFound = '+N2+' (expected '+ N1+ ')');
    end;
end;


Procedure DoTestExpandFilename(TempDir : String);

var
  I: byte;
  TestDir: string;

begin
  for I := 1 to TestFilesNumber do
    FileClose (FileCreate (TestFiles [I]));

  TestExpFNC ('*File1.tst', ExpandFileName ('testFile1.tst'), mkExactMatch);
  if FileNameCaseSensitive then
   TestExpFNC ('TestFile1.tst', ExpandFileName ('testFile1.tst'), mkSingleMatch)
  else
   TestExpFNC ('TestFile1.tst', ExpandFileName ('testFile1.tst'), mkExactMatch);
  TestExpFNC ('testFile1.tst', ExpandFileName ('testFile1.tst'), mkExactMatch);
  TestExpFNC ('testFile2.tst', ExpandFileName ('testFile2.tst'), mkExactMatch);
  if FileNameCaseSensitive then
   TestExpFNC ('TestFile2.tst', ExpandFileName ('TestFile2.tst'), mkExactMatch)
  else
   TestExpFNC ('TestFile2.tst', ExpandFileName ('testFile2.tst'), mkExactMatch);
  if FileNameCaseSensitive then
   TestExpFNC ('testfile2.tst', ExpandFileName ('testFile2.tst'), mkAmbiguous)
  else
   TestExpFNC ('testfile2.tst', ExpandFileName ('testFile2.tst'), mkExactMatch);
(* Return value depends on ordering of files in the particular filesystem used thus not checked *)
  TestExpFNC ('*File2.tst', '', mkExactMatch);
  if FileNameCaseSensitive then
   TestExpFNC ('*File*.tst', '', mkExactMatch)
  else
   TestExpFNC ('*File*.tst', '', mkExactMatch);
  TestExpFNC ('..' + DirectorySeparator + '*' + DirectorySeparator + '*File*.tst',
     ExpandFileName ('..' + DirectorySeparator + '*' + DirectorySeparator + '*File*.tst'),
                                                                                    mkNone);
  I := Length (TempDir);
  TestDir := TempDir;
  while (I > 1) and not (TempDir [I] in ['a'..'z','A'..'Z']) do
   Dec (I);
  if I > 0 then
   begin
    if TestDir [I] in ['a'..'z'] then
     TestDir [I] := char (Ord (TestDir [I]) and not $20)
    else
     TestDir [I] := char (Ord (TestDir [I]) or $20);
   end
  else
    WriteLn ('Warning: Cannot perform all required tests; please set TEMP!');
  if FileNameCaseSensitive then
   TestExpFNC (TestDir + DirectorySeparator + 'testFile1.tst',
               ExpandFileName (TempDir + DirectorySeparator + 'testFile1.tst'), mkSingleMatch)
  else
   TestExpFNC (TestDir + DirectorySeparator + 'testFile1.tst',
               ExpandFileName (TestDir + DirectorySeparator + 'testFile1.tst'), mkExactMatch);
  if FileNameCaseSensitive then
   TestExpFNC (TestDir + DirectorySeparator + 'testfile1.tst',
               ExpandFileName (TempDir + DirectorySeparator + 'testFile1.tst'), mkSingleMatch)
  else
   TestExpFNC (TestDir + DirectorySeparator + 'testfile1.tst',
               ExpandFileName (TestDir + DirectorySeparator + 'testFile1.tst'), mkExactMatch);
  for I := 1 to TestFilesNumber do
   if not (DeleteFile (TestFiles [I])) then
    begin
    if FileNameCaseSensitive or (I <> 3) then
      WriteLn ('Warning: Deletion of ', TestFiles [I], ' (file #', I, ') failed - possibly due to case insensitive file system!');
    end;
end;

Function TestExpandFilename : String;   

var
  TempDir : string;
  CurDir: string;

begin
  Result:='';
  TempDir := ExpandFilename (GetTempDir);
  if (Length (TempDir) > MinPathLength) and
                  (TempDir [Length (TempDir)] in AllowDirectorySeparators) then
  TempDir := LeftStr (TempDir, Length (TempDir) - 1);

  CurDir := GetCurrentDir;
  Try
    SetCurrentDir (TempDir);
    DoTestExpandFilename(TempDir);
  finally
    SetCurrentDir(CurDir);
  end;    
end;

begin
  SysUtilsTest('TestExpandFileNameCase',@TestExpandFilename);
end.
