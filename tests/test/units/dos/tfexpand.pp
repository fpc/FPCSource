program TFExpand;

(* Test for possible bugs in Dos.FExpand *)

{ $DEFINE DEBUG}
(* Defining DEBUG causes all the source and target strings *)
(* to be written to the console to make debugging easier.  *)

uses
{$ifdef FPC}
  {$ifdef win32}
    win32err,
  {$endif win32}
{$endif FPC}
 Dos;

{$IFDEF OS2}
function _DosError (Error: longint): longint; cdecl;
                                                 external 'DOSCALLS' index 212;
{$ENDIF OS2}

{$IFDEF LINUX}
 {$IFNDEF UNIX}
  {$DEFINE UNIX}
 {$ENDIF UNIX}
{$ENDIF LINUX}

{$IFDEF AMIGA}
 {$DEFINE VOLUMES}
{$ENDIF AMIGA}

{$IFDEF NETWARE}
 {$DEFINE VOLUMES}
{$ENDIF NETWARE}

const
{$IFNDEF FPC}
 FileNameCaseSensitive = false;
 DirSep = '\';
 CDrive = 'C:';
{$ELSE}
   DirSep = System.DirectorySeparator;
  {$IFDEF UNIX}
   CDrive = '';
  {$ELSE}
   CDrive = 'C:';
  {$ENDIF}
{$ENDIF}
 HasErrors: boolean = false;

var
 TestDir, OrigDir, OrigTstDir, CurDir, CDir, S: DirStr;
 TestDrive: string [2];
 I: byte;
 IOR: longint;

function Translate (S: PathStr): PathStr;
var
 I: byte;
begin
{$IFDEF UNIX}
 if (Length (S) > 1) and (S [2] = ':') then Delete (S, 1, 2);
{$ELSE}
 for I := 1 to Length (S) do if S [I] = '/' then S [I] := DirSep;
 if (Length (S) > 0) and (S [1] in ['a'..'z']) then S [1] := UpCase (S [1]);
{$ENDIF}
 if not (FileNameCaseSensitive) then
                           for I := 1 to Length (S) do S [I] := UpCase (S [I]);
 Translate := S;
end;

procedure Check (Src, Rslt: PathStr);
var
 Rslt2: PathStr;
begin
{$IFDEF DEBUG}
 WriteLn (Src, '=>', Rslt);
{$ENDIF}
 Rslt := Translate (Rslt);
 Rslt2 := FExpand (Src);
 if Rslt <> Rslt2 then
 begin
  WriteLn ('Error: FExpand (', Src, ') should be "', Rslt, '", not "',
                                                                   Rslt2, '"');
  HasErrors := true;
 end;
end;

begin
{$IFDEF OS2}
(* Avoid OS/2 error messages. *)
 _DosError (0);
{$ENDIF OS2}
 if ParamCount <> 1 then
 begin
  WriteLn ('Warning: Parameter missing!');
  WriteLn ('Full path to a directory with write access' +
{$IFNDEF UNIX}
                               #13#10'(preferably not on a C: drive)' +
{$ENDIF}
                                                                 ' expected.');
  WriteLn ('Trying to use the current directory instead ' +
{$IFDEF UNIX}
                                                         '(not quite ideal).');
{$ELSE UNIX}
                                                    '(problems might arise).');
{$ENDIF UNIX}
  GetDir (0, TestDir);
 end else TestDir := ParamStr (1);
 if TestDir [Length (TestDir)] <> DirSep then TestDir := TestDir + DirSep;
 GetDir (0, OrigDir);
{$IFDEF UNIX}
 TestDrive := '';
{$ELSE UNIX}
 TestDrive := Copy (TestDir, 1, 2);
 GetDir ((Ord (TestDir [1]) and not ($20)) - 64, OrigTstDir);
{$ENDIF UNIX}
{$I-}
 MkDir (TestDir + 'TESTDIR1');
 if IOResult <> 0 then ;
 MkDir (TestDir + 'TESTDIR1' + DirSep + 'TESTDIR2');
 if IOResult <> 0 then ;
{$I+}
 ChDir (TestDir + 'TESTDIR1' + DirSep + 'TESTDIR2');
 GetDir (0, CurDir);
{$IFDEF UNIX}
 CDir := CurDir;
{$ELSE UNIX}
 GetDir (3, CDir);
{$ENDIF UNIX}
 Check (' ', CurDir + DirSep + ' ');
 Check ('', CurDir + DirSep);
 Check ('.', CurDir);
 Check ('C:', CDir);
 Check ('C:.', CDir);
 if CDir [Length (CDir)] = DirSep then Check ('c:anything', CDir + 'anything')
                         else Check ('c:anything', CDir + DirSep + 'anything');
 Check ('C:' + DirSep, CDrive + DirSep);
 Check ('C:' + DirSep + '.', CDrive + DirSep);
 Check ('C:' + DirSep + '..', CDrive + DirSep);
 Check ('C:' + DirSep + 'DOS', CDrive + DirSep + 'DOS');
 Check ('C:' + DirSep + '..' + DirSep + 'DOS', CDrive + DirSep + 'DOS');
 Check ('C:' + DirSep + 'DOS.', CDrive + DirSep + 'DOS.');
 Check ('C:' + DirSep + 'DOS' + DirSep, CDrive + DirSep + 'DOS' + DirSep);
 Check ('C:' + DirSep + 'DOS' + DirSep + '.', CDrive + DirSep + 'DOS');
 Check ('C:' + DirSep + 'DOS' + DirSep + '..', CDrive + DirSep);
 Check ('C:' + DirSep + 'DOS' + DirSep + '..' + DirSep, CDrive + DirSep);
 Check ('C:' + DirSep + 'DOS' + DirSep + 'TEST' + DirSep + '..', CDrive +
                                                               DirSep + 'DOS');
 Check ('C:' + DirSep + 'DOS' + DirSep + 'TEST' + DirSep + '..' + DirSep,
                                             CDrive + DirSep + 'DOS' + DirSep);
 Check (DirSep, TestDrive + DirSep);
 Check (DirSep + '.', TestDrive + DirSep);
 Check (DirSep + '..', TestDrive + DirSep);
 Check (DirSep + 'DOS', TestDrive + DirSep + 'DOS');
 Check ('d', CurDir + DirSep + 'd');
 Check (' d', CurDir + DirSep + ' d');
 Check ('dd', CurDir + DirSep + 'dd');
 Check ('dd' + DirSep + 'dd', CurDir + DirSep + 'dd' + DirSep + 'dd');
 Check ('ddd', CurDir + DirSep + 'ddd');
 Check ('dddd' + DirSep + 'eeee.ffff', CurDir + DirSep + 'dddd' + DirSep
                                                                + 'eeee.ffff');
 Check ('.special', CurDir + DirSep + '.special');
 Check ('..special', CurDir + DirSep + '..special');
 Check ('special..', CurDir + DirSep + 'special..');
 Check ('special.' + DirSep, CurDir + DirSep + 'special.' + DirSep);
 Check (DirSep + '.special', TestDrive + DirSep + '.special');
 Check ('..', TestDir + 'TESTDIR1');
 Check ('.' + DirSep + '..', TestDir + 'TESTDIR1');
 Check ('..' + DirSep + '.', TestDir + 'TESTDIR1');
 Check ('...', CurDir + DirSep + '...');
{$IFDEF UNIX}
 S := GetEnv ('HOME');
 { On m68k netbsd at least, HOME contains a final slash
   remove it PM }
 if S[length(S)]=DirSep then
   S:=Copy(S,1,Length(S)-1);
 Check ('~', S);
 Check ('~' + DirSep + '.', S);
 if (Length (S) > 0) and (S [Length (S)] <> DirSep) then S := S + DirSep;
 Check ('~NobodyWithThisNameShouldEverExist.test/nothing', CurDir + DirSep +
                            '~NobodyWithThisNameShouldEverExist.test/nothing');
 Check ('/tmp/~NoSuchUserAgain', '/tmp/~NoSuchUserAgain');
 Check ('~' + DirSep, S);
 Check ('~' + DirSep + '.' + DirSep, S);
 Check ('~' + DirSep + 'directory' + DirSep + 'another',
                                         S + 'directory' + DirSep + 'another');
{$ELSE UNIX}
 Check (TestDrive + '..', TestDir + 'TESTDIR1');
 Check (TestDrive + '..' + DirSep, TestDir + 'TESTDIR1' + DirSep);
 Check (TestDrive + '.' + DirSep + '.', CurDir);
 Check (TestDrive + '.' + DirSep + '..', TestDir + 'TESTDIR1');
{$I-}
{$ifndef unix}
 { avoid a and b drives for
   no unix systems to reduce the
   probablility of getting an alert message box }
 I := 3;
{$else unix}
 I := 1;
{$endif unix}
 repeat
  S := '';
  GetDir (I, S);
  IOR := IOResult;
  if IOR = 0 then Inc (I);
 until (I > 26) or (IOR <> 0);
 if I <= 26 then
 begin
  S := Chr (I + 64) + ':ddd';
  Check (S, Chr (I + 64) + ':' + DirSep + 'ddd');
 end else
   WriteLn ('Sorry, cannot test FExpand behaviour for incorrect drives here.');
{$I+}
{$IFDEF FPC}
 Check ('d\d/d', CurDir + DirSep + 'd' + DirSep + 'd' + DirSep + 'd');
 Check ('\\server\share\directory', '\\server\share\directory');
 Check ('\\server\share\directory1\directory2\..',
                                                  '\\server\share\directory1');
 Check ('\\', '\\');
 Check ('\\.', '\\.\');
 Check ('\\.\', '\\.\');
 Check ('\\.\.', '\\.\.');
 Check ('\\.\..', '\\.\..');
 Check ('\\.\...', '\\.\...');
 Check ('\\.\TEST', '\\.\TEST');
 Check ('\\..\', '\\..\');
 Check ('\\..\TEST', '\\..\TEST');
 Check ('\\..\TEST\.', '\\..\TEST');
 Check ('\\..\TEST1\TEST2\..', '\\..\TEST1');
 Check ('\\..\TEST\..', '\\..\TEST');
 Check ('\\..\TEST\..\..', '\\..\TEST');
{$ENDIF FPC}
 ChDir (OrigTstDir);
{$ENDIF UNIX}
{$IFDEF VOLUMES}
 Check ('VolName:' + DirSep + 'DIR1', 'VolName:' + DirSep + 'DIR1');
 Check ('VolName:' + DirSep + 'DIR1' + DirSep + '..', 'VolName:' + DirSep);
 Check ('VolName:' + DirSep + 'DIR1' + DirSep + '..' + DirSep + '..',
                                                          'VolName:' + DirSep);
 Check ('VolName:' + DirSep + '.', 'VolName:' + DirSep);
 Check ('VolName:' + DirSep + '..', 'VolName:' + DirSep);
 Check ('VolName:' + DirSep + '..' + DirSep, 'VolName:' + DirSep);
 Check ('SrvName\VolName:' + DirSep + 'TEST', 'SrvName' + DirSep + 'VolName:' +
                                                              DirSep + 'TEST');
 Check ('SrvName/VolName:' + DirSep + 'TEST', 'SrvName' + DirSep + 'VolName:' +
                                                              DirSep + 'TEST');
{$ENDIF VOLUMES}
 ChDir (OrigDir);
 RmDir (TestDir + 'TESTDIR1' + DirSep + 'TESTDIR2');
 RmDir (TestDir + 'TESTDIR1');
 if HasErrors then
 begin
  WriteLn ('FExpand doesn''t work correctly.');
  Halt (1);
 end;
end.
