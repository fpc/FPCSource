{ %target=linux,freebsd,openbsd,netbsd,win32,win64,darwin,haiku,morphos }

{
    This file is part of the Free Pascal test suite.
    Copyright (c) 1999-2004 by the Free Pascal development team.

    Test for possible bugs in SysUtils.ExpandFileName

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$codepage utf8}

program TFExpand;

{$DEFINE DEBUG}
(* Defining DEBUG causes all the source and target strings *)
(* to be written to the console to make debugging easier.  *)
{$ifdef go32v2}
  {$define USE_INTERNAL_UNICODE}
{$endif}

{$ifdef USE_INTERNAL_UNICODE}
  {$define USE_FPWIDESTRING_UNIT}
  {$define USE_UNICODEDUCET_UNIT}
  {$define USE_CPALL_UNIT}
{$endif}
uses
{$ifdef FPC}
 PopupErr,
{$endif FPC}
{$ifndef USE_INTERNAL_UNICODE}
 {$ifdef unix}
  {$ifdef darwin}iosxwstr{$else}cwstring{$endif},
 {$endif unix}
{$endif not USE_INTERNAL_UNICODE}
 {$ifdef USE_UNICODEDUCET_UNIT}
  unicodeducet,
 {$endif}
 {$ifdef USE_FPWIDESTRING_UNIT}
  fpwidestring,
 {$endif}
 {$ifdef USE_CPALL_UNIT}
  cpall,
 {$endif}
  SysUtils;

{$IFDEF LINUX}
 {$IFNDEF UNIX}
  {$DEFINE UNIX}
 {$ENDIF UNIX}
{$ENDIF LINUX}

{$IFDEF AMIGA}
 {$DEFINE VOLUMES}
 {$DEFINE NODRIVEC}
{$ENDIF AMIGA}

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
 CC = UTF8String('C:');
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
 DirectorySeparator2 = UTF8String(System.DirectorySeparator);
 {$IFDEF DIRECT}
  {$IFDEF MACOS}
 DirectorySeparator = UTF8String(':');
 LFNSupport = true;
 FileNameCasePreserving = true;
  {$ELSE MACOS}
   {$IFDEF UNIX}
 DirectorySeparator = UTF8String('/');
 DriveSeparator = UTF8String('/');
 FileNameCasePreserving = true;
   {$ELSE UNIX}
    {$IFDEF AMIGA}
 DirectorySeparator = UTF8String(':');
 FileNameCasePreserving = true;
    {$ELSE AMIGA}
 DirectorySeparator = UTF8String('\');
 FileNameCasePreserving = false;
    {$ENDIF AMIGA}
   {$ENDIF UNIX}
  {$ENDIF MACOS}
 {$ENDIF DIRECT}
 DirSep = UTF8String(DirectorySeparator);
 {$IFDEF MACOS}
 DriveSep = '';
 {$ELSE MACOS}
  {$IFDEF AMIGA}
 DriveSep = '';
  {$ELSE AMIGA}
 DriveSep = DriveSeparator;
  {$ENDIF AMIGA}
 {$ENDIF MACOS}
 {$IFDEF UNIX}
 CDrive = '';
 {$ELSE UNIX}
  {$IFDEF MACOS}
 CDrive = UTF8String('C');
  {$ELSE MACOS}
   {$IFDEF AMIGA}
 CDrive = UTF8String('C');
   {$ELSE AMIGA}
 CDrive = UTF8String('C:');
   {$ENDIF AMIGA}
  {$ENDIF MACOS}
 {$ENDIF UNIX}
{$ENDIF FPC}
 TestFileName = UTF8String('™estfil†.™st');
 TestDir1Name = UTF8String('T†S™DIR1');
 TestDir2Name = UTF8String('TE∑™DIR2');
 HasErrors: boolean = false;


var
{$IFNDEF NODRIVEC}
 CDir,
{$endif}
 TestDir, TestDir0, OrigDir, OrigTstDir, CurDir, S: UTF8String;
 TestDrive: UTF8String;
 I: byte;
 IOR: longint;
 F: file;

function Translate (S: rawbytestring): rawbytestring;
var
 I: byte;
begin
{$IFDEF UNIX}
 if (Length (S) > 1) and (S [2] = ':') then Delete (S, 1, 2);
{$ELSE UNIX}
 for I := 1 to Length (S) do if S [I] = '/' then S [I] := DirSep[1];
 if (Length (S) > 1) and (S [1] in ['a'..'z']) and (S[2]=DriveSep) then
   S [1] := UpCase (S [1]);
{$ENDIF UNIX}
 if not (FileNameCasePreserving) then
   for I := 1 to Length (S) do S [I] := UpCase (S [I]);
 Translate := S;
end;

procedure Check (Src, Rslt: rawbytestring);
var
 Rslt2: rawbytestring;
begin
{$IFDEF DEBUG}
 WriteLn (Src, '=>', Rslt);
{$ENDIF DEBUG}
 Rslt := Translate (Rslt);
 Rslt2 := ExpandFileName (Src);
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
 if Rslt <> Rslt2 then
 begin
  WriteLn ('Error: ExpandFileName (', Src, ') should be "', Rslt, '", not "',
                                                                   Rslt2, '"');
  HasErrors := true;
 end;
end;

begin
 { ensure ExpandFileName doesn't lose data when the file system can represent all characters }
 DefaultFileSystemCodePage:=CP_UTF8;
 DefaultRTLFileSystemCodePage:=CP_UTF8;
 { ensure we do lose data if we somewhere accidentally use the default system code page
   to perform operations }
 DefaultSystemCodePage:=CP_ASCII;
 if ParamCount <> 1 then
 begin
  WriteLn ('Warning: Parameter missing!');
  WriteLn ('Full path to a directory with write access' +
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
{$IFDEF DIRECT}System.{$ENDIF DIRECT}GetDir (0, TestDir);
 end else TestDir := ParamStr (1);
 if TestDir [Length (TestDir)] <> DirectorySeparator2 then
  TestDir := TestDir + DirectorySeparator2;
GetDir (0, OrigDir);
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
 Assign (F, TestFileName);
 Rewrite (F);
 Close (F);
 if IOResult <> 0 then ;
 { prevent conversion of TestFileName to ansi code page in case of
   ExpandFileName(ansistring) }
 Assign (F, ExpandFileName (RawByteString(TestFileName)));
{$I+}
 GetDir (0, CurDir);
{$IFNDEF NODRIVEC}
 GetDir (3, CDir);
{$ENDIF NODRIVEC}
 Check (' ', CurDir + DirSep + ' ');
{$IFDEF AMIGA}
 Check ('', CurDir);
{$ELSE AMIGA}
 Check ('', CurDir + DirSep);
{$ENDIF AMIGA}
{$IFDEF MACOS}
 Check (':', CurDir + DirSep);
{$ELSE MACOS}
 Check ('.', CurDir);
{$ENDIF MACOS}

{$IFNDEF NODRIVEC}
if CDir [Length (CDir)] = DirSep then Check ('c:anything', CDir + 'anything')
                         else Check ('c:anything', CDir + DirSep + 'anything');
 Check (CC + DirSep, CDrive + DirSep);
{$IFDEF NODOTS}
 Check ('C:.', 'C:.');
 Check (CC + DirSep + '.', CDrive + DirSep + '.');
 Check (CC + DirSep + '..', CDrive + DirSep + '..');
{$ELSE NODOTS}
 Check ('C:.', CDir);
 Check (CC + DirSep + '.', CDrive + DirSep);
 Check (CC + DirSep + '..', CDrive + DirSep);
{$ENDIF NODOTS}
 Check (CC + DirSep + UTF8String('∂œ∑'), CDrive + DirSep + UTF8String('∂œ∑'));
{$IFNDEF NODOTS}
 Check (CC + DirSep + '..' + DirSep + UTF8String('∂œ∑'), CDrive + DirSep + UTF8String('∂œ∑'));
{$ENDIF NODOTS}
 Check (CC + DirSep + 'DOS.', CDrive + DirSep + 'DOS.');
{$IFDEF AMIGA}
 Check (CC + DirSep + UTF8String('∂œ∑') + DirSep, CDrive + DirSep);
{$ELSE AMIGA}
 Check (CC + DirSep + UTF8String('∂œ∑') + DirSep, CDrive + DirSep + UTF8String('∂œ∑') + DirSep);
{$ENDIF AMIGA}
{$IFNDEF NODOTS}
 Check (CC + DirSep + UTF8String('∂œ∑') + DirSep + '.', CDrive + DirSep + UTF8String('∂œ∑'));
 Check (CC + DirSep + UTF8String('∂œ∑') + DirSep + '..', CDrive + DirSep);
 Check (CC + DirSep + UTF8String('∂œ∑') + DirSep + '..' + DirSep, CDrive + DirSep);
 Check (CC + DirSep + UTF8String('∂œ∑') + DirSep + UTF8String('†ĘŚ™') + DirSep + '..', CDrive +
                                                               DirSep + UTF8String('∂œ∑'));
 Check (CC + DirSep + UTF8String('∂œ∑') + DirSep + UTF8String('†ĘŚ™') + DirSep + '..' + DirSep,
                                             CDrive + DirSep + UTF8String('∂œ∑') + DirSep);
{$ENDIF NODOTS}
{$ENDIF NODRIVEC}

{$IFNDEF MACOS}
 Check (DirSep, TestDrive + DirSep);
 Check (DirSep + '.', TestDrive + DirSep);
 Check (DirSep + '..', TestDrive + DirSep);
 Check (DirSep + UTF8String('∂œ∑'), TestDrive + DirSep + UTF8String('∂œ∑'));
{$ENDIF MACOS}
 Check (UTF8String('∆'), CurDir + DirSep + UTF8String('∆'));
{$IFDEF MACOS}
 Check (DirSep + UTF8String('∆'), CurDir + DirSep + UTF8String('∆'));
{$ELSE MACOS}
 {$IFNDEF NODOTS}
 Check ('.' + DirSep + UTF8String('∆'), CurDir + DirSep + UTF8String('∆'));
 {$ENDIF NODOTS}
{$ENDIF MACOS}
 Check (UTF8String('∆') + DirSep + TestFileName, CurDir + DirSep + UTF8String('∆') + DirSep + TestFileName);
 Check (UTF8String(' ∆'), CurDir + DirSep + UTF8String(' ∆'));
 Check (UTF8String('∆∆'), CurDir + DirSep + UTF8String('∆∆'));
{$IFDEF MACOS}
 Check (DirSep + UTF8String('∆∆') + DirSep + UTF8String('∆∆'), CurDir + DirSep + UTF8String('∆∆') + DirSep + UTF8String('∆∆'));
 Check (UTF8String('∆∆') + DirSep + UTF8String('∆∆'), UTF8String('∆∆') + DirSep + UTF8String('∆∆'));
{$ELSE MACOS}
 Check (UTF8String('∆∆') + DirSep + UTF8String('∆∆'), CurDir + DirSep + UTF8String('∆∆') + DirSep + UTF8String('∆∆'));
{$ENDIF MACOS}
 Check (UTF8String('∂∂∂'), CurDir + DirSep + UTF8String('∂∂∂'));
{$IFDEF MACOS}
 Check (UTF8String('∂∂∂∂') + DirSep + UTF8String('ÊÊÊÊ.ƒƒƒƒ'), UTF8String('∂∂∂∂') + DirSep + UTF8String('ÊÊÊÊ.ƒƒƒƒ'));
{$ELSE MACOS}
 Check (UTF8String('∂∂∂∂') + DirSep + UTF8String('ÊÊÊÊ.ƒƒƒƒ'), CurDir + DirSep + UTF8String('∂∂∂∂') + DirSep
                                                                + UTF8String('ÊÊÊÊ.ƒƒƒƒ'));
{$ENDIF MACOS}
 Check (UTF8String(UTF8String('.∑πê©îæ¬')), CurDir + DirSep + UTF8String(UTF8String('.∑πê©îæ¬')));
 Check (UTF8String('..∑πê©îæ¬'), CurDir + DirSep + UTF8String('..∑πê©îæ¬'));
 Check (UTF8String('∑πê©îæ¬..'), CurDir + DirSep + UTF8String('∑πê©îæ¬..'));
{$IFDEF AMIGA}
 Check (UTF8String('∑πê©îæ¬.') + DirSep, CurDir);
{$ELSE AMIGA}
 {$IFDEF MACOS}
 Check (UTF8String('∑πê©îæ¬.') + DirSep, UTF8String('∑πê©îæ¬.') + DirSep);
 {$ELSE MACOS}
 Check (UTF8String('∑πê©îæ¬.') + DirSep, CurDir + DirSep + UTF8String('∑πê©îæ¬.') + DirSep);
 {$ENDIF MACOS}
{$ENDIF AMIGA}
{$IFDEF MACOS}
 Check (DirSep + DirSep, TestDir + TestDir1Name + DirSep);
 Check (DirSep + DirSep + TestFileName, TestDir + TestDir1Name + DirSep
                                                               + TestFileName);
{$ELSE MACOS}
 Check (DirSep + UTF8String('.∑πê©îæ¬'), TestDrive + DirSep + UTF8String(UTF8String('.∑πê©îæ¬')));
 {$IFNDEF NODOTS}
 Check ('..', TestDir + TestDir1Name);
 Check ('.' + DirSep + '..', TestDir + TestDir1Name);
 Check ('..' + DirSep + '.', TestDir + TestDir1Name);
 {$ENDIF NODOTS}
{$ENDIF MACOS}
{$IFDEF NETWARE}
 Check ('...', TestDir);
{$ELSE NETWARE}
 Check ('...', CurDir + DirSep + '...');
{$ENDIF NETWARE}
 Check (TestFileName, CurDir + DirSep + TestFileName);
{$IFDEF UNIX}
 S := GetEnvironmentVariable ('HOME');
 { On m68k netbsd at least, HOME contains a final slash
   remove it PM }
 if (Length (S) > 1) and (S [Length (S)] = DirSep) then
   S:=Copy(S,1,Length(S)-1);
 if Length (S) = 0 then
  begin
   Check ('~', CurDir);
   Check ('~' + DirSep + '.', DirSep);
  end
 else
  begin
   Check ('~', S);
   Check ('~' + DirSep + '.', S);
  end;
 if (Length (S) > 0) and (S [Length (S)] <> DirSep) then
  S := S + DirSep;
 Check (UTF8String('~ıœßodyWithThisNameShouldEverExist.test/nothinﬂ'), CurDir + DirSep +
                            UTF8String('~ıœßodyWithThisNameShouldEverExist.test/nothinﬂ'));
 Check ('/tmp/~NoSº©hUse®Again', '/tmp/~NoSº©hUse®Again');
 if Length (S) = 0 then
  begin
   Check ('~' + DirSep, DirSep);
   Check ('~' + DirSep + '.' + DirSep, DirSep);
   Check ('~' + DirSep + UTF8String('∂î®ê©†œ®Ú') + DirSep + UTF8String('anothe®'),
                                    DirSep + UTF8String('∂î®ê©†œ®Ú') + DirSep + UTF8String('anothe®'));
  end
 else
  begin
   Check ('~' + DirSep, S);
   Check ('~' + DirSep + '.' + DirSep, S);
   Check ('~' + DirSep + UTF8String('∂î®ê©†œ®Ú') + DirSep + UTF8String('anothe®'),
                                         S + UTF8String('∂î®ê©†œ®Ú') + DirSep + UTF8String('anothe®'));
  end;
{$ELSE UNIX}
 {$IFNDEF NODRIVEC}
 Check (TestDrive + '..', TestDir + TestDir1Name);
 Check (TestDrive + '..' + DirSep, TestDir + TestDir1Name + DirSep);
 Check (TestDrive + '.' + DirSep + '.', CurDir);
 Check (TestDrive + '.' + DirSep + '..', TestDir + TestDir1Name);
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
  S := UTF8String(Chr (I + 64)) + UTF8String(':∂∂∂');
  Check (S, UTF8String(Chr (I + 64)) + UTF8String(':') + DirSep + UTF8String('∂∂∂'));
 end else
   WriteLn ('Sorry, cannot test ExpandFileName behaviour for incorrect drives here.');
{$I+}
  {$IFDEF FPC}
 Check ('∆\∆/∆', CurDir + DirSep + UTF8String('∆') + DirSep + UTF8String('∆') + DirSep + UTF8String('∆'));
 Check ('\\serve®\sha®e\di®ectory', '\\serve®\sha®e\di®ectory');
 Check ('\\serve®\sha®e\directo®y1\directo®y2\..',
                                                  '\\serve®\sha®e\directo®y1');
 Check ('\\', '\\');
 Check ('\\.', '\\.\');
 Check ('\\.\', '\\.\');
 Check ('\\.\.', '\\.\.');
 Check ('\\.\..', '\\.\..');
 Check ('\\.\...', '\\.\...');
 Check ('\\.\†êÒ™', '\\.\†êÒ™');
 Check ('\\..\', '\\..\');
 Check ('\\..\†êÒ™', '\\..\†êÒ™');
 Check ('\\..\†êÒ™\.', '\\..\†êÒ™');
 Check ('\\..\†êÒ™1\TÊ∑T2\..', '\\..\†êÒ™1');
 Check ('\\..\†êÒ™\..', '\\..\†êÒ™');
 Check ('\\..\†êÒ™\..\..', '\\..\†êÒ™');
  {$ENDIF FPC}
 {$ENDIF NODRIVEC}
{$ENDIF UNIX}
{$IFDEF VOLUMES}
 Check (UTF8String('√olıame') + DriveSep + DirSep + UTF8String('∆Î®1'), UTF8String('√olıame') + DriveSep + DirSep + UTF8String('∆Î®1'));
 {$IFNDEF NODOTS}
 Check (UTF8String('√olıame') + DriveSep + DirSep + UTF8String('∆Î®1') + DirSep + '..', UTF8String('√olıame') + DriveSep + DirSep);
 Check (UTF8String('√olıame') + DriveSep + DirSep + UTF8String('∆Î®1') + DirSep + '..' + DirSep + '..',
                                                          UTF8String('√olıame') + DriveSep + DirSep);
 Check (UTF8String('√olıame') + DriveSep + DirSep + '.', UTF8String('√olıame:') + DirSep);
 Check (UTF8String('√olıame') + DriveSep + DirSep + '..', UTF8String('√olıame:') + DirSep);
 Check (UTF8String('√olıame') + DriveSep + DirSep + '..' + DirSep, UTF8String('√olıame') + DriveSep + DirSep);
 {$ENDIF NODOTS}
 {$IFDEF NETWARE}
 Check (UTF8String('∑rvName\√olıame') + DriveSep + DirSep + UTF8String('†ĘŚ™'), UTF8String('∑rvName') + DirSep + UTF8String('√olıame') +
                                                         DriveSep + DirSep + UTF8String('†ĘŚ™'));
 Check (UTF8String('∑rvName/√olıame') + DriveSep + DirSep + UTF8String('†ĘŚ™'), UTF8String('∑rvName') + DirSep + UTF8String('√olıame') +
                                                         DriveSep + DirSep + UTF8String('†ĘŚ™'));
 {$ENDIF NETWARE}
 {$IFDEF AMIGA}
  {$IFDEF NODOTS}
 Check ('.', CurDir + DirSep + '.');
  {$ELSE NODOTS}
 Check ('.', CurDir);
  {$ENDIF NODOTS}
 {$ENDIF AMIGA}
{$ENDIF VOLUMES}
 Erase (F);
{$IFNDEF NODRIVEC}
 ChDir (OrigTstDir);
{$ENDIF NODRIVEC}
 ChDir (OrigDir);
 RmDir (TestDir0 + TestDir1Name + DirectorySeparator2 + TestDir2Name);
 RmDir (TestDir0 + TestDir1Name);
 if HasErrors then
 begin
  WriteLn ('ExpandFileName doesn''t work correctly.');
  Halt (1);
 end;
end.
