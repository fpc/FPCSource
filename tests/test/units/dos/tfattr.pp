{******************************************}
{  Used to check the DOS unit              }
{------------------------------------------}
{  SetFAttr / GetFAttr testing             }
{******************************************}
Program tfattr;

uses dos;

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
 TestExt   = 'DAT';
{$IFDEF TP}
  DirectorySeparator = '\';
{$ENDIF}
  has_errors : boolean = false;


{ verifies that the DOSError variable is equal to }
{ the value requested.                            }
Procedure CheckDosError(err: Integer);
 var
  x : integer;
  s :string;
 Begin
  x := DosError;
  case x of
  0 : s := '(0): No Error.';
  2 : s := '(2): File not found.';
  3 : s := '(3): Path not found.';
  5 : s := '(5): Access Denied.';
  6 : s := '(6): Invalid File Handle.';
  8 : s := '(8): Not enough memory.';
  10 : s := '(10) : Invalid Environment.';
  11 : s := '(11) : Invalid format.';
  18 : s := '(18) : No more files.';
  else
   begin
    Str (X, S);
    s := '(' + s + ') - INVALID DOSERROR';
   end
  end;
  if err <> x then
    Begin
      WriteLn('FAILURE. (Value of DOSError should be ',err,' '+s+')');
      has_errors:=true;
    end;
 end;

procedure fail;
Begin
  WriteLn('Failed!');
  has_errors:=true;
End;

Procedure TestFAttr1;
Var
 F: File;
 Attr: Word;
 s: string;
Begin
 WriteLn('Opening an invalid file...Success!');
 Assign(f,'');
 GetFAttr(f,Attr);
 CheckDosError(3);
 Assign(f,TestFName);
 WriteLn('Trying to open a valid file...Success!');
 GetFAttr(f,Attr);
 CheckDosError(0);
{$ifndef wince}
 Write('Trying to open the current directory file...');
 Assign(f,'.');
 GetFAttr(f,Attr);
 if (attr and Directory) = 0 then
   fail
 else
   WriteLn('Success!');
 CheckDosError(0);
 Write('Trying to open the parent directory file...');
 Assign(f,'..');
 GetFAttr(f,Attr);
 if (attr and Directory) = 0 then
   fail
 else
   WriteLn('Success!');
 CheckDosError(0);
{$endif wince}
{ This is completely platform dependent
 Write('Trying to open the parent directory file when in root...');
 Getdir(0,s);
 ChDir(RootPath);
 Assign(f,'..');
 GetFAttr(f,Attr);
 ChDir(s);
 CheckDosError(3);
 WriteLn('Success!');
}
{$ifdef go32v2}
 { Should normally fail, because of end directory separator. This is
   allowed under unixes so the test is go32v2 only }
 WriteLn('Trying to open a directory file...Success!');
 GetDir(0,s);
 Assign(f,s+DirectorySeparator);
 GetFAttr(f, Attr);
 CheckDosError(3);
{$endif}

 Write('Trying to open a directory file...');
{$ifdef wince}
 s:='\windows';
{$else}
 GetDir(0,s);
{$endif wince}
 Assign(f,s);
 GetFAttr(f, Attr);
 if (attr and Directory) = 0 then
   fail
 else
   WriteLn('Success!');
 CheckDosError(0);
end;

Procedure TestFAttr;
Var
 F: File;
 Attr: Word;
 s: string;
Begin
 Assign(f, TestFname);
 {----------------------------------------------------------------}
 { This routine causes problems, because it all depends on the    }
 { operating system. It is assumed here that HIDDEN is available  }
 { to all operating systems.                                      }
 {----------------------------------------------------------------}
 s:='Setting read-only attribute on '+TestFName+'...';
 SetFAttr(f,ReadOnly);
 CheckDosError(0);
{$IFDEF EXTATTR}
 GetFAttr(f,Attr);
 CheckDosError(0);
 if Attr and ReadOnly<> 0 then
   WriteLn(s+'Success.')
 else
  Begin
    WriteLn(s+'FAILURE. Read-only attribute not set.');
    has_errors:=true;
  end;
 { file should no longer be read only }
 s:='Removing read-only attribute...';
 SetFAttr(f,Archive);
 CheckDosError(0);
 GetFAttr(f,Attr);
 CheckDosError(0);
 if Attr and ReadOnly<> 0 then
  Begin
    WriteLn(s+'FAILURE. Read-only attribute still set.');
    has_errors:=true;
  end
 else
   WriteLn(s+'Success.');
{$ENDIF}

 s:='Setting hidden attribute on '+TestFName+'...';
 SetFAttr(f,Hidden);
 CheckDosError(0);
{$IFDEF EXTATTR}
 GetFAttr(f,Attr);
 CheckDosError(0);
 if Attr and Hidden<> 0 then
   WriteLn(s+'Success.')
 else
  Begin
    WriteLn(s+'FAILURE. Hidden attribute not set.');
    has_errors:=true;
  end;

 { file should no longer be read only }
 s:='Removing hidden attribute...';
 SetFAttr(f,Archive);
 CheckDosError(0);
 GetFAttr(f,Attr);
 CheckDosError(0);
 if Attr and Hidden<> 0 then
  Begin
    WriteLn(s+'FAILURE. Hidden attribute still set.');
    has_errors:=true;
  end
 else
   WriteLn(s+'Success.');
{$ENDIF}

{$IFDEF EXTATTR}

 s:='Setting system attribute on '+TestFName+'...';
 SetFAttr(f,SysFile);
 CheckDosError(0);
 GetFAttr(f,Attr);
 CheckDosError(0);
 if Attr and SysFile<> 0 then
   WriteLn(s+'Success.')
 else
  Begin
    WriteLn(s+'FAILURE. SysFile attribute not set.');
    has_errors:=true;
  end;
 { file should no longer be read only }
 s:='Removing Sysfile attribute...';
 SetFAttr(f,0);
 CheckDosError(0);
 GetFAttr(f,Attr);
 CheckDosError(0);
 if Attr and Sysfile<> 0 then
  Begin
    WriteLn(s+'FAILURE. SysFile attribute still set.');
    has_errors:=true;
  end
 else
   WriteLn(s+'Success.');
{$ENDIF}
{
 s:='Setting Directory attribute on '+TestFName+'...';
 SetFAttr(f,Directory);
 CheckDosError(5);
 GetFAttr(f,Attr);
 CheckDosError(0);
 if Attr and Directory<> 0 then
  Begin
    WriteLn(s+'FAILURE. Directory Attribute set.');
    has_errors:=true;
  end
 else
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
 CheckDosError(5);
{$else}
 CheckDosError(0);
{$endif}
 GetFAttr(f,Attr);
 CheckDosError(0);
 if Attr and VolumeID<> 0 then
  Begin
    WriteLn(s+'FAILURE. Volume Attribute set.');
    has_errors:=true;
  end
 else
   WriteLn(s+'Success.');
end;




var
 f: file;
 oldexit : pointer;

  procedure MyExit;far;
   begin
     ExitProc := OldExit;
     RmDir(TestDir);
     Assign(f, TestFname);
     Erase(f);
     Assign(f, TestFname1);
     Erase(f);
   end;


Begin
{$IFDEF MACOS}
  pathTranslation:= true;
{$ENDIF}
  WriteLn('File should never be executed in root path!');
  OldExit := ExitProc;
  ExitProc := @MyExit;
  Assign(f,TestFName);
  Rewrite(f,1);
  BlockWrite(f,Week,sizeof(Week));
  Close(f);
  Assign(f,TestFName1);
  Rewrite(f,1);
  Close(F);
  MkDir(TestDir);
  testfattr1;
  testfattr;
  if has_errors then
    halt(1);
end.
