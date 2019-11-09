{ %skiptarget=wince }

{
    This file is part of the Free Pascal test suite.
    Copyright (c) 1999-2004 by the Free Pascal development team.

    Test for possible bugs in Dos.FExpand

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$mode objfpc}

unit utdfexp;

interface

uses punit, utrtl;

{$DEFINE DEBUG}
(* Defining DEBUG causes all the source and target strings *)
(* to be written to the console to make debugging easier.  *)
{ $DEFINE DIRECT}
(* Defining DIRECT causes direct embedding of fexpand.inc instead     *)
(* of using FExpand implementation in (previously compiled) unit Dos. *)

implementation

uses
 Dos;

{$IFDEF DIRECT}
(* For testing purposes on non-native platforms *)
 {$DEFINE VOLUMES}
 {$DEFINE NODOTS}
 { $DEFINE AMIGA}
 { $DEFINE UNIX}
 {$DEFINE MACOS}

 { $DEFINE FPC_FEXPAND_DRIVES}
 { $DEFINE FPC_FEXPAND_UNC}
 {$DEFINE FPC_FEXPAND_VOLUMES}
 {$DEFINE FPC_FEXPAND_NO_DEFAULT_PATHS}
 {$DEFINE FPC_FEXPAND_DRIVESEP_IS_ROOT}
 { $DEFINE FPC_FEXPAND_DIRSEP_IS_UPDIR}
 {$DEFINE FPC_FEXPAND_NO_DOTS_UPDIR}
 { $DEFINE FPC_FEXPAND_NO_CURDIR}
 { $DEFINE FPC_FEXPAND_TILDE}
 { $DEFINE FPC_FEXPAND_MULTIPLE_UPDIR}
 {$DEFINE FPC_FEXPAND_DIRSEP_IS_CURDIR}
 { $DEFINE FPC_FEXPAND_GETENV_PCHAR}
{$ENDIF DIRECT}

{$IFDEF LINUX}
 {$IFNDEF UNIX}
  {$DEFINE UNIX}
 {$ENDIF UNIX}
{$ENDIF LINUX}

{$IFDEF AMIGA}
 {$IFNDEF HASAMIGA}
  {$DEFINE HASAMIGA}
 {$ENDIF HASAMIGA}
{$ENDIF AMIGA}

{$IFDEF HASAMIGA}
 {$DEFINE VOLUMES}
 {$DEFINE NODRIVEC}
 {$DEFINE NODOTS}
{$ENDIF HASAMIGA}

{$IFDEF NETWARE}
 {$DEFINE VOLUMES}
 {$DEFINE NODRIVEC}
{$ENDIF NETWARE}

{$IFDEF UNIX}
 {$DEFINE NODRIVEC}
{$ENDIF UNIX}

{$IFDEF MACOS}
 {$DEFINE VOLUMES}
 {$DEFINE NODRIVEC}
 {$DEFINE NODOTS}
{$ENDIF MACOS}

const
{$IFNDEF NODRIVEC}
 CC = 'C:';
{$ENDIF NODRIVEC}
{$IFNDEF FPC}
 FileNameCasePreserving = false;
 DirectorySeparator = '\';
 DirectorySeparator2 = '\';
 DirSep = '\';
 CDrive = 'C:';
 DriveSep = ':';
{$ELSE FPC}
(* Used for ChDir/MkDir *)
 DirectorySeparator2 = System.DirectorySeparator;
 {$IFDEF DIRECT}
  {$IFDEF MACOS}
 DirectorySeparator = ':';
 LFNSupport = true;
 FileNameCasePreserving = true;
  {$ELSE MACOS}
   {$IFDEF UNIX}
 DirectorySeparator = '/';
 DriveSeparator = '/';
 FileNameCasePreserving = true;
   {$ELSE UNIX}
    {$IFDEF HASAMIGA}
 DirectorySeparator = '/';
 FileNameCasePreserving = true;
    {$ELSE HASAMIGA}
 DirectorySeparator = '\';
 FileNameCasePreserving = false;
    {$ENDIF HASAMIGA}
   {$ENDIF UNIX}
  {$ENDIF MACOS}
 {$ENDIF DIRECT}
 DirSep = DirectorySeparator;
 {$IFDEF MACOS}
 DriveSep = '';
 {$ELSE MACOS}
 DriveSep = DriveSeparator;
 {$ENDIF MACOS}
 {$IFDEF UNIX}
 CDrive = '';
 {$ELSE UNIX}
  {$IFDEF MACOS}
 CDrive = 'C';
  {$ELSE MACOS}
   {$IFDEF HASAMIGA}
 CDrive = 'C';
   {$ELSE HASAMIGA}
 CDrive = 'C:';
   {$ENDIF HASAMIGA}
  {$ENDIF MACOS}
 {$ENDIF UNIX}
{$ENDIF FPC}
 TestFileName = 'testfile.tst';
 TestDir1Name = 'TESTDIR1';
 TestDir2Name = 'TESTDIR2';

{$IFDEF DIRECT}
procedure XToDirect (var S: string);
var
 I: byte;
begin
 if DirectorySeparator2 <> DirectorySeparator then
  for I := 1 to Length (S) do
   if S [I] = DirectorySeparator2 then
    S [I] := DirectorySeparator;
{$IFNDEF FPC_FEXPAND_DRIVES}
 if DriveSeparator = DirectorySeparator then
  I := Pos (DirectorySeparator + DirectorySeparator, S)
 else
  I := Pos (DriveSeparator, S);
 if I <> 0 then
  Delete (S, 1, I);
{$ENDIF FPC_FEXPAND_DRIVES}
end;

procedure GetDir (Drive: byte; var Directory: string);
begin
 System.GetDir (Drive, Directory);
 XToDirect (Directory);
end;

 {$I fexpand.inc}
{$ENDIF DIRECT}

var
{$IFNDEF NODRIVEC}
 CDir,
{$endif}
 TestDir, TestDir0, OrigDir, CurDir, S: DirStr;
 TestDrive: string [2];
 F: file;

function Translate (S: PathStr): PathStr;

var
  I: byte;

begin
{$IFDEF UNIX}
 if (Length (S) > 1) and (S [2] = ':') then Delete (S, 1, 2);
{$ELSE UNIX}
 for I := 1 to Length (S) do if S [I] = '/' then S [I] := DirSep;
 if (Length (S) > 1) and (S [1] in ['a'..'z']) and (S[2]=DriveSep) then
   S [1] := UpCase (S [1]);
{$ENDIF UNIX}
 if not (FileNameCasePreserving) then
                           for I := 1 to Length (S) do S [I] := UpCase (S [I]);
 Translate := S;
end;

Function Check (ID : Integer; Src, Rslt: PathStr) : Boolean;

var
 Rslt2: PathStr;
 S : string;
begin
{$IFDEF DEBUG}
  if ShowDebugOutput then
    WriteLn (ID,' : ',Src, '=>', Rslt);
{$ENDIF DEBUG}
  Rslt := Translate (Rslt);
  Rslt2 := FExpand (Src);
{$IFDEF DIRECT}
  {$IFNDEF FPC_FEXPAND_DRIVES}
    I := Pos (System.DriveSeparator, Rslt2);
    if I <> 0 then
      Delete (Rslt2, 1, I);
  {$ENDIF FPC_FEXPAND_DRIVES}
{$ENDIF DIRECT}
{$IFNDEF UNIX}
  if (Length (Rslt2) > 1) and (Rslt2 [1] in ['a'..'z']) and (Rslt2[2]=DriveSep) then
    Rslt2 [1] := UpCase (Rslt2 [1]);
{$ENDIF NDEF UNIX}
  Str(ID,S);
  Check:=AssertEquals(S+': FExpand ('+Src+ ')', Rslt,Rslt2);
end;

Function DoTest : TTestString;

begin
  Result:='';
  {$IFDEF DIRECT}
   {$IFNDEF FPC_FEXPAND_DRIVES}
   I := Pos (System.DriveSeparator, CurDir);
   if I <> 0 then
    Delete (CurDir, 1, I);
   {$ENDIF FPC_FEXPAND_DRIVES}
  {$ENDIF DIRECT}
  {$IFNDEF NODRIVEC}
   GetDir (3, CDir);
  {$ENDIF NODRIVEC}
   if not Check (1,' ', CurDir + DirSep + ' ') then exit;
  {$IFDEF HASAMIGA}
  if not Check (2, '', CurDir) then exit;
  {$ELSE HASAMIGA}
  if not Check (3,'', CurDir + DirSep) then exit;
  {$ENDIF HASAMIGA}
  {$IFDEF MACOS}
   if not Check (4,':', CurDir + DirSep) then exit;
  {$ELSE MACOS}
   if not Check (5,'.', CurDir) then exit;
  {$ENDIF MACOS}

  {$IFNDEF NODRIVEC}
  if CDir [Length (CDir)] = DirSep then
    begin
    if not Check (6,'c:anything', CDir + 'anything') then
      exit,
    end
  else
    if not Check (7,'c:anything', CDir + DirSep + 'anything') then exit;
    if not Check (8,CC + DirSep, CDrive + DirSep) then exit;
   {$IFDEF NODOTS}
   if not Check (9,'C:.', 'C:.') then exit;
   if not Check (10,CC + DirSep + '.', CDrive + DirSep + '.') then exit;
   if not Check (CC + DirSep + '..', CDrive + DirSep + '..') then exit;
   {$ELSE NODOTS}
   if not Check (11,'C:.', CDir) then exit;
   if not Check (12,CC + DirSep + '.', CDrive + DirSep) then exit;
   if not Check (13,CC + DirSep + '..', CDrive + DirSep) then exit;
   {$ENDIF NODOTS}
   if not  Check (14,CC + DirSep + 'DOS', CDrive + DirSep + 'DOS') then exit;
   {$IFNDEF NODOTS}
   if not Check (15,CC + DirSep + '..' + DirSep + 'DOS', CDrive + DirSep + 'DOS') then exit;
   {$ENDIF NODOTS}
   if not Check (16,CC + DirSep + 'DOS.', CDrive + DirSep + 'DOS.') then exit;
   {$IFDEF HASAMIGA} (* This has no effect - AMIGA has NODRIVEC defined... *)
   if not Check (17,CC + DirSep + 'DOS' + DirSep, CDrive + DirSep) then exit;
   {$ELSE HASAMIGA}
   if not Check (18,CC + DirSep + 'DOS' + DirSep, CDrive + DirSep + 'DOS' + DirSep) then exit;
   {$ENDIF HASAMIGA}
   {$IFNDEF NODOTS}
   if not Check (19,CC + DirSep + 'DOS' + DirSep + '.', CDrive + DirSep + 'DOS') then exit;
   if not Check (20,CC + DirSep + 'DOS' + DirSep + '..', CDrive + DirSep) then exit;
   if not Check (21,CC + DirSep + 'DOS' + DirSep + '..' + DirSep, CDrive + DirSep) then exit;
   if not Check (22,CC + DirSep + 'DOS' + DirSep + 'TEST' + DirSep + '..', CDrive +
                                                                 DirSep + 'DOS') then exit;
   if not Check (23,ID,'C:' + DirSep + 'DOS' + DirSep + 'TEST' + DirSep + '..' + DirSep,
                                               CDrive + DirSep + 'DOS' + DirSep) then exit;
   {$ENDIF NODOTS}
  {$ENDIF NODRIVEC}

  {$IFNDEF MACOS}
   {$IFDEF HASAMIGA}
   if not Check (24,DirSep, TestDir + TestDir1Name) then exit;
   if not Check (25,DirSep + DirSep + TestFileName, TestDir + TestFileName) then exit;
   if not Check (26,DirSep + 'DOS', TestDir + TestDir1Name + DirSep + 'DOS') then exit;
   {$ELSE HASAMIGA}
   if not Check (27,DirSep, TestDrive + DirSep) then exit;
   if not Check (28,DirSep + '.', TestDrive + DirSep) then exit;
   if not Check (29,DirSep + '..', TestDrive + DirSep)then exit;
   if not Check (30,DirSep + 'DOS', TestDrive + DirSep + 'DOS') then exit;
   {$ENDIF HASAMIGA}
  {$ENDIF MACOS}
   if not Check (31,'d', CurDir + DirSep + 'd')then exit;
  {$IFDEF MACOS}
   if not Check (32,DirSep + 'd', CurDir + DirSep + 'd') then exit;
  {$ELSE MACOS}
   {$IFNDEF NODOTS}
   if not Check (33,'.' + DirSep + 'd', CurDir + DirSep + 'd') then exit;
   {$ENDIF NODOTS}
  {$ENDIF MACOS}
   if not Check (34,'d' + DirSep + TestFileName, CurDir + DirSep + 'd' + DirSep + TestFileName) then exit;
   if not Check (35,' d', CurDir + DirSep + ' d') then exit;
   if not Check (36,'dd', CurDir + DirSep + 'dd') then exit;
  {$IFDEF MACOS}
   if not Check (37,DirSep + 'dd' + DirSep + 'dd', CurDir + DirSep + 'dd' + DirSep + 'dd') then exit;
   if not Check (38,'dd' + DirSep + 'dd', 'dd' + DirSep + 'dd') then exit;
  {$ELSE MACOS}
   if not Check (39,'dd' + DirSep + 'dd', CurDir + DirSep + 'dd' + DirSep + 'dd') then exit;
  {$ENDIF MACOS}
   if not Check (40,'ddd', CurDir + DirSep + 'ddd') then exit;
  {$IFDEF MACOS}
   if not Check (41,'dddd' + DirSep + 'eeee.ffff', 'dddd' + DirSep + 'eeee.ffff') then exit;
  {$ELSE MACOS}
   if not Check (42,'dddd' + DirSep + 'eeee.ffff', CurDir + DirSep + 'dddd' + DirSep
                                                                  + 'eeee.ffff') then exit;
  {$ENDIF MACOS}
   if not Check (43,'.special', CurDir + DirSep + '.special') then exit;
   if not Check (44,'..special', CurDir + DirSep + '..special') then exit;
   if not Check (45,'special..', CurDir + DirSep + 'special..') then exit;
  {$IFDEF HASAMIGA}
   if not Check (46,'special.' + DirSep, CurDir + DirSep + 'special.' + DirSep) then exit;
  {$ELSE HASAMIGA}
   {$IFDEF MACOS}
   if not Check (47,'special.' + DirSep, 'special.' + DirSep) then exit;
   {$ELSE MACOS}
   if not Check (48,'special.' + DirSep, CurDir + DirSep + 'special.' + DirSep) then exit;
   {$ENDIF MACOS}
  {$ENDIF HASAMIGA}
  {$IFDEF MACOS}
   if not Check (49,DirSep + DirSep, TestDir + TestDir1Name + DirSep) then exit;
   if not Check (50,DirSep + DirSep + TestFileName, TestDir + TestDir1Name + DirSep
                                                                 + TestFileName) then exit;
  {$ELSE MACOS}
   if not Check (51,DirSep + '.special', TestDrive + DirSep + '.special') then exit;
   {$IFNDEF NODOTS}
   if not Check (52,'..', TestDir + TestDir1Name) then exit;
   if not Check (53,'.' + DirSep + '..', TestDir + TestDir1Name) then exit;
   if not Check (54,'..' + DirSep + '.', TestDir + TestDir1Name) then exit;
   {$ENDIF NODOTS}
  {$ENDIF MACOS}
  {$IFDEF NETWARE}
   if not Check (55,'...', TestDir) then exit;
  {$ELSE NETWARE}
   if not Check (56,'...', CurDir + DirSep + '...') then exit;
  {$ENDIF NETWARE}
   if not Check (57,TestFileName, CurDir + DirSep + TestFileName) then exit;
  {$IFDEF UNIX}
   S := GetEnv ('HOME');
   { On m68k netbsd at least, HOME contains a final slash
     remove it PM }
   if (Length (S) > 1) and (S [Length (S)] = DirSep) then
     S:=Copy(S,1,Length(S)-1);
   if Length (S) = 0 then
    begin
     if not Check (58,'~', CurDir) then exit;
     if not Check (59,'~' + DirSep + '.', DirSep) then exit;
    end
   else
    begin
     if not Check (60,'~', S) then exit;
     if not Check (61,'~' + DirSep + '.', S) then exit;
    end;
   if (Length (S) > 0) and (S [Length (S)] <> DirSep) then
    S := S + DirSep;
   if not Check (62,'~NobodyWithThisNameShouldEverExist.test/nothing', CurDir + DirSep +
                              '~NobodyWithThisNameShouldEverExist.test/nothing') then exit;
   if not Check (63,'/tmp/~NoSuchUserAgain', '/tmp/~NoSuchUserAgain') then exit;
   if Length (S) = 0 then
    begin
     if not Check (64,'~' + DirSep, DirSep) then exit;
     if not Check (65,'~' + DirSep + '.' + DirSep, DirSep) then exit;
     if not Check (66,'~' + DirSep + 'directory' + DirSep + 'another',
                                      DirSep + 'directory' + DirSep + 'another') then exit;
    end
   else
    begin
     if not Check (67,'~' + DirSep, S) then exit;
     if not Check (68,'~' + DirSep + '.' + DirSep, S) then exit;
     if not Check (69,'~' + DirSep + 'directory' + DirSep + 'another',
                                           S + 'directory' + DirSep + 'another') then exit;
    end;
  {$ELSE UNIX}
   {$IFNDEF NODRIVEC}
   if not Check (70,TestDrive + '..', TestDir + TestDir1Name) then exit;
   if not Check (71,TestDrive + '..' + DirSep, TestDir + TestDir1Name + DirSep) then exit;
   if not Check (72,TestDrive + '.' + DirSep + '.', CurDir) then exit;
   if not Check (73,TestDrive + '.' + DirSep + '..', TestDir + TestDir1Name) then exit;
  {$I-}
  (*
  { $ ifndef unix }
  {   avoid a and b drives for
     no unix systems to reduce the
     probablility of getting an alert message box }
   { This should not be needed - unit popuperr should solve this?! TH }
   I := 3;
  {$else unix} *)
   I := 1;
  { $ endif unix}
   repeat
    S := '';
    GetDir (I, S);
    IOR := IOResult;
    if IOR = 0 then Inc (I);
   until (I > 26) or (IOR <> 0);
   if I <= 26 then
   begin
    S := Chr (I + 64) + ':ddd';
    if not Check (74,S, Chr (I + 64) + ':' + DirSep + 'ddd') then exit;
   end else
     if ShowDebugOutput then
       WriteLn ('Sorry, cannot test FExpand behaviour for incorrect drives here.');
  {$I+}
    {$IFDEF FPC}
   if not Check (75,'d\d/d', CurDir + DirSep + 'd' + DirSep + 'd' + DirSep + 'd') then exit;
   if not Check (76,'\\server\share\directory', '\\server\share\directory') then exit;
   if not Check (77,'\\server\share\directory1\directory2\..',
                                                    '\\server\share\directory1') then exit;
   if not Check (78,'\\', '\\') then exit;
   if not Check (79,'\\.', '\\.\') then exit;
   if not Check (80,'\\.\', '\\.\') then exit;
   if not Check (81,'\\.\.', '\\.\.') then exit;
   if not Check (82,'\\.\..', '\\.\..') then exit;
   if not Check (83,'\\.\...', '\\.\...') then exit;
   if not Check (84,'\\.\TEST', '\\.\TEST') then exit;
   if not Check (85,'\\..\', '\\..\') then exit;
   if not Check (86,'\\..\TEST', '\\..\TEST') then exit;
   if not Check (87,'\\..\TEST\.', '\\..\TEST') then exit;
   if not Check (88,'\\..\TEST1\TEST2\..', '\\..\TEST1') then exit;
   if not Check (89,'\\..\TEST\..', '\\..\TEST') then exit;
   if not Check (90,'\\..\TEST\..\..', '\\..\TEST') then exit;
    {$ENDIF FPC}
   {$ENDIF NODRIVEC}
  {$ENDIF UNIX}
  {$IFDEF VOLUMES}
   {$IFDEF HASAMIGA}
   if not Check (91,'VolName' + DriveSep + 'DIR1', 'VolName' + DriveSep + 'DIR1') then exit;
   {$ELSE HASAMIGA}
   if not Check (92,'VolName' + DriveSep + DirSep + 'DIR1', 'VolName' + DriveSep + DirSep + 'DIR1') then exit;
   {$ENDIF HASAMIGA}
   {$IFNDEF NODOTS}
   if not Check (93,'VolName' + DriveSep + DirSep + 'DIR1' + DirSep + '..', 'VolName' + DriveSep + DirSep) then exit;
   if not Check (94,'VolName' + DriveSep + DirSep + 'DIR1' + DirSep + '..' + DirSep + '..',
                                                            'VolName' + DriveSep + DirSep) then exit;
   if not Check (95,'VolName' + DriveSep + DirSep + '.', 'VolName:' + DirSep) then exit;
   if not Check (96,'VolName' + DriveSep + DirSep + '..', 'VolName:' + DirSep) then exit;
   if not Check (97,'VolName' + DriveSep + DirSep + '..' + DirSep, 'VolName' + DriveSep + DirSep) then exit;
   {$ENDIF NODOTS}
   {$IFDEF NETWARE}
   if not Check (98,'SrvName\VolName' + DriveSep + DirSep + 'TEST', 'SrvName' + DirSep + 'VolName' +
                                                           DriveSep + DirSep + 'TEST') then exit;
   if not Check (99,'SrvName/VolName' + DriveSep + DirSep + 'TEST', 'SrvName' + DirSep + 'VolName' +
                                                           DriveSep + DirSep + 'TEST') then exit;
   {$ENDIF NETWARE}
   {$IFDEF HASAMIGA}
    {$IFDEF NODOTS}
   if not Check (100,'.', CurDir + DirSep + '.') then exit;
    {$ELSE NODOTS}
   if not Check (101,'.', CurDir) then exit;
    {$ENDIF NODOTS}
   {$ENDIF HASAMIGA}
  {$ENDIF VOLUMES}
end;

Function TestDosFExpand : TTestString;

begin
  Result:='';
  TestDir:=SysGetSetting('fexpanddir');
  if (TestDir='') then
    begin
    if ShowDebugOutput then
      begin
      WriteLn ('Warning: Parameter missing!');
      WriteLN('Full path to a directory with write access' +
{$IFNDEF UNIX}
 {$IFNDEF VOLUMES}
        #13#10'(preferably not on a C: drive)' +
 {$ENDIF VOLUMES}
{$ENDIF UNIX}
        ' expected.');
      WriteLn ('Trying to use the current directory instead ' +
{$IFDEF UNIX}
        '(not quite ideal).');
{$ELSE UNIX}
        '(problems might arise).');
{$ENDIF UNIX}
      end;
    // Get current dir
{$IFDEF DIRECT}System.{$ENDIF DIRECT}GetDir(0,TestDir);
    end;
  if TestDir[Length(TestDir)]<>DirectorySeparator2 then
    TestDir := TestDir + DirectorySeparator2;
{$IFDEF DIRECT}System.{$ENDIF DIRECT}GetDir (0,OrigDir);
{$IFDEF NODRIVEC}
  TestDrive := '';
{$ELSE NODRIVEC}
  TestDrive := Copy (TestDir, 1, 2);
  GetDir ((Ord (TestDir [1]) and not ($20)) - 64, OrigTstDir);
{$ENDIF NODRIVEC}
{$I-}
  MkDir (TestDir + TestDir1Name);
  if IOResult <> 0 then ;
  MkDir (TestDir + TestDir1Name + DirectorySeparator2 + TestDir2Name);
  if IOResult <> 0 then ;
{$I+}
  ChDir (TestDir + TestDir1Name + DirectorySeparator2 + TestDir2Name);
{$I-}
  TestDir0 := TestDir;
{$IFDEF DIRECT}
  XToDirect (TestDir);
  {$IFNDEF FPC_FEXPAND_DRIVES}
  I := Pos (System.DriveSeparator, TestDir);
  if I <> 0 then
    Delete (TestDir, 1, I);
  {$ENDIF FPC_FEXPAND_DRIVES}
{$ENDIF DIRECT}
  Assign (F, TestFileName);
  Rewrite (F);
  Close (F);
  if IOResult <> 0 then ;
{$IFNDEF DIRECT}
  Assign (F, FExpand (TestFileName));
{$ENDIF DIRECT}
{$I+}
  GetDir (0, CurDir);
  // Do the actual tests.
  // The test exits at the first error, so we put it in a subroutine to be able to clean up.
  Result:=DoTest;
  // Clean up
  Erase (F);
{$IFNDEF NODRIVEC}
  ChDir (OrigTstDir);
{$ENDIF NODRIVEC}
  ChDir (OrigDir);
  RmDir (TestDir0 + TestDir1Name + DirectorySeparator2 + TestDir2Name);
  RmDir (TestDir0 + TestDir1Name);
end;

begin
  AddTest('DosFExpand',@TestDosFExpand,EnsureSuite('Dos'));
end.
