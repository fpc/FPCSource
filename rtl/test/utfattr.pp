{******************************************}
{  Used to check the DOS unit              }
{------------------------------------------}
{  SetFAttr / GetFAttr testing             }
{******************************************}
{$mode objfpc}
unit utfattr;

interface

uses punit, utrtl;

implementation

uses dos, utdos;
{$IFDEF MSDOS}
        {$DEFINE EXTATTR}
{$ENDIF}
{$IFDEF DPMI}
        {$DEFINE EXTATTR}
{$ENDIF}
{$IFDEF GO32V1}
        {$DEFINE EXTATTR}
{$ENDIF}
{$IFDEF GO32V2}
        {$DEFINE EXTATTR}
{$ENDIF}
{$IFDEF OS2}
        {$DEFINE EXTATTR}
{$ENDIF}
{$IFDEF WIN32}
        {$DEFINE EXTATTR}
{$ENDIF}
{$IFDEF ATARI}
        {$DEFINE EXTATTR}
{$ENDIF}
{$IFDEF WINCE}
        {$DEFINE EXTATTR}
{$ENDIF}


CONST
{ what is the root path }
{$ifdef UNIX}
  RootPath = '/';
{$else UNIX}
  {$ifdef WINCE}
    RootPath = '\';
  {$else WINCE}
    RootPath = 'C:\';
  {$endif WINCE}
{$ENDIF}
 Week:Array[0..6] of String =
 ('Sunday','Monday','Tuesday','Wednesday','Thursday','Friday','Saturday');

 TestFName = 'TESTDOS.DAT';  { CASE SENSITIVE DON'T TOUCH! }
 TestFName1 = 'TESTFILE';    { CASE SENSITIVE DON'T TOUCH! }
 TestDir = 'MYDIR';          { CASE SENSITIVE DON'T TOUCH! }
{$IFDEF TP}
  DirectorySeparator = '\';
{$ENDIF}


Function TestFAttr1 : TTestString;

Var
 F: File;
 Attr: Word;
 m,s: string;

Begin
  Result:='';
  Attr:=0;
  S:='';
  M:='Opening an invalid file...';
  if ShowDebugOutput then
    WriteLn(M);
  Assign(f,'');
  GetFAttr(f,Attr);
  if not CheckDosError(M,3) then exit;
  Assign(f,TestFName);
  M:='Trying to open a valid file...';
  if ShowDebugOutput then
    WriteLn(M+'Success!');
  GetFAttr(f,Attr);
  if not CheckDosError(M,0) then exit;
{$ifndef wince}
  M:='Trying to open the current directory file...';
  if ShowDebugOutput then
    Write(M);
  Assign(f,'.');
  GetFAttr(f,Attr);
  if Not AssertTrue(M+'Not directory',(attr and Directory)<>0) then
    Exit;
  if ShowDebugOutput then
    Writeln('Success');
  if not CheckDosError(M,0) then exit;
  M:='Trying to open the parent directory file...';
  if ShowDebugOutput then
    Write(M);
  Assign(f,'..');
  GetFAttr(f,Attr);
  if not AssertTrue(M+'Not directory',(attr and Directory)<> 0) then
    exit;
  if ShowDebugOutput then
     WriteLn('Success!');
  if not CheckDosError(M,0) then exit;
{$endif wince}
{ This is completely platform dependent
  M:='Trying to open the parent directory file when in root...';
  if ShowDebugOutput then
    Write(M);
  Getdir(0,s);
  ChDir(RootPath);
  Assign(f,'..');
  GetFAttr(f,Attr);
  ChDir(s);
  if not CheckDosError(M,3) then exit;
  if ShowDebugOutput then
    WriteLn('Success!');
}
{$ifdef go32v2}
  { Should normally fail, because of end directory separator. This is
    allowed under unixes so the test is go32v2 only }
  M:='Trying to open a directory file...Success!';
  if ShowDebugOutput then
    WriteLn(M);
  GetDir(0,s);
  Assign(f,s+DirectorySeparator);
  GetFAttr(f, Attr);
  if not CheckDosError(M,3) then exit;
{$endif}
  M:='Trying to open a directory file...';
  if ShowDebugOutput then
    Write(M);
{$ifdef wince}
  s:='\windows';
{$else}
  GetDir(0,s);
{$endif wince}
  Assign(f,s);
  GetFAttr(f, Attr);
  if not AssertTrue(M+'Not directory',(attr and Directory)<> 0) then
   exit;
  if ShowDebugOutput then
    WriteLn('Success!');
  CheckDosError(M,0);
end;

Function TestFAttr : TTestString;
Var
 F: File;
 Attr: Word;
 s: string;
Begin
  Result:='';
  Attr:=0;
  S:='';
  Assign(f, TestFname);
  {----------------------------------------------------------------}
  { This routine causes problems, because it all depends on the    }
  { operating system. It is assumed here that HIDDEN is available  }
  { to all operating systems.                                      }
  {----------------------------------------------------------------}
  s:='Setting read-only attribute on '+TestFName+'...';
  SetFAttr(f,ReadOnly);
  if not CheckDosError(S,0) then exit;
{$IFDEF EXTATTR}
  GetFAttr(f,Attr);
  if not CheckDosError(S,0) then exit;
  if not AssertTrue(S+'Read-only attribute set.',Attr and ReadOnly<> 0) then exit;
  if ShowDebugOutput then
    WriteLn(s+'Success.')
  { file should no longer be read only }
  s:='Removing read-only attribute...';
  SetFAttr(f,Archive);
  if not CheckDosError(S,0) then exit;
  GetFAttr(f,Attr);
  if not CheckDosError(S,0) then exit;
  if not AssertTrue(S+'Read-only attribute still set.',Attr and ReadOnly=0) then exit;
  if ShowDebugOutput then
    WriteLn(s+'Success.');
{$ENDIF}
  s:='Setting hidden attribute on '+TestFName+'...';
  SetFAttr(f,Hidden);
  CheckDosError(S,0);
{$IFDEF EXTATTR}
  GetFAttr(f,Attr);
  CheckDosError(0);
  if not AssertTrue(S+'Hidden attribute set.',Attr and Hidden<> 0) then exit;
  if ShowDebugOutput then
    WriteLn(s+'Success.');
  { file should no longer be read only }
  s:='Removing hidden attribute...';
  SetFAttr(f,Archive);
  CheckDosError(S,0);
  GetFAttr(f,Attr);
  CheckDosError(S,0);
  if not AssertTrue(S+'Hidden attribute still set.',Attr and Hidden=0) then exit;
  if ShowDebugOutput then
    WriteLn(s+'Success.');
{$ENDIF}

{$IFDEF EXTATTR}

 s:='Setting system attribute on '+TestFName+'...';
 SetFAttr(f,SysFile);
 CheckDosError(S,0);
 GetFAttr(f,Attr);
 CheckDosError(S,0);
 if not AssertTrue(S+'System attribute set.',Attr and SysFile<> 0) then exit;
 if ShowDebugOutput then
   WriteLn(s+'Success.')
 { file should no longer be read only }
 s:='Removing Sysfile attribute...';
 SetFAttr(f,0);
 CheckDosError(0);
 GetFAttr(f,Attr);
 CheckDosError(0);
 if not AssertTrue(S+'System attribute set.',Attr and SysFile= 0) then exit;
 if ShowDebugOutput then
   WriteLn(s+'Success.');
{$ENDIF}
{
 s:='Setting Directory attribute on '+TestFName+'...';
 SetFAttr(f,Directory);
 CheckDosError(S,5);
 GetFAttr(f,Attr);
 CheckDosError(S,0);
 if Not AssertTrue(s+'Directory Attribute set.',(Attr and Directory)=0) then exit;
 if ShowDebugOutput then
   WriteLn(s+'Success.');
}
 {**********************************************************************}
 {********************** TURBO PASCAL BUG ******************************}
 { The File is not a volume name, and DosError = 0, which is incorrect  }
 { it shoulf not be so in FPC.                                          }
 {**********************************************************************}
 {********************** TURBO PASCAL BUG ******************************}
 s:='Setting Volume attribute on '+TestFName+'...';
 SetFAttr(f,VolumeID);
{$ifndef tp}
 CheckDosError(S,5);
{$else}
 CheckDosError(S,0);
{$endif}
 GetFAttr(f,Attr);
 CheckDosError(S,0);
 if not AssertTrue(s+'Volume Attribute set.',Attr and VolumeID=0) then
 if ShowDebugOutput then
   WriteLn(s+'Success.');
end;


Function DoneFattr : TTestString;

var
  f: file;

begin
  Result:='';
  RmDir(TestDir);
  Assign(f,TestFname);
  Erase(f);
  Assign(f,TestFname1);
  Erase(f);
end;

Function InitFattr : TTestString;

var
  f: file;

Begin
  Result:='';
{$IFDEF MACOS}
  pathTranslation:= true;
{$ENDIF}
  if ShowDebugoutput then
    WriteLn('File should never be executed in root path!');
  Assign(f,TestFName);
  Rewrite(f,1);
  BlockWrite(f,Week,sizeof(Week));
  Close(f);
  Assign(f,TestFName1);
  Rewrite(f,1);
  Close(F);
  MkDir(TestDir);
end;

Procedure RegisterFattrTests;

Var
  P : PSuite;

begin
  P:=AddSuite('Fattr',@InitFattr,@DonefAttr,EnsureSuite('Dos'));
  AddTest('testfattr1',@testfattr1,P);
  AddTest('testfattr',@testfattr,P);
end;

initialization
  RegisterFattrTests;

end.
