{
    $Id$
    This file is part of the Free Pascal run time library.

    POSIX Interface to the system unit

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This is the core of the system unit *nix systems (now FreeBSD
     and Unix).

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

const
     { Default creation mode for directories and files }

     { read/write permission for everyone }
     MODE_OPEN = S_IWUSR OR S_IRUSR OR
                 S_IWGRP OR S_IRGRP OR
                 S_IWOTH OR S_IROTH;
     { read/write search permission for everyone }
     MODE_MKDIR = MODE_OPEN OR
                 S_IXUSR OR S_IXGRP OR S_IXOTH;


{*****************************************************************************
                       Misc. System Dependent Functions
*****************************************************************************}

procedure System_exit;
begin
   Fpexit(cint(ExitCode));
End;


Function ParamCount: Longint;
Begin
  Paramcount:=argc-1
End;


function BackPos(c:char; const s: shortstring): integer;
var
 i: integer;
Begin
  for i:=length(s) downto 0 do
    if s[i] = c then break;
  if i=0 then
    BackPos := 0
  else
    BackPos := i;
end;


 { variable where full path and filename and executable is stored }
 { is setup by the startup of the system unit.                    }
var
 execpathstr : shortstring;

function paramstr(l: longint) : string;
 var
  s: string;
  s1: string;
 begin
   { stricly conforming POSIX applications  }
   { have the executing filename as argv[0] }
   if l=0 then
     begin
       paramstr := execpathstr;
     end
   else
     paramstr:=strpas(argv[l]);
 end;

Procedure Randomize;
Begin
  randseed:=longint(Fptime(nil));
End;


{*****************************************************************************
                              Heap Management
*****************************************************************************}

var
  _HEAP : longint;external name 'HEAP';
  _HEAPSIZE : longint;external name 'HEAPSIZE';

{$ifndef SYSTEM_HAS_GETHEAPSTART}
function getheapstart:pointer;
begin
  getheapstart := @_HEAP;
end;
{$endif}


{$ifndef SYSTEM_HAS_GETHEAPSIZE}
function getheapsize:longint;
begin
  getheapsize := _HEAPSIZE;
end;
{$endif}


{*****************************************************************************
                          Low Level File Routines
*****************************************************************************}

{
  The lowlevel file functions should take care of setting the InOutRes to the
  correct value if an error has occured, else leave it untouched
}

Function PosixToRunError  (PosixErrno : longint) : longint;
{
  Convert ErrNo error to the correct Inoutres value
}

begin
  if PosixErrNo=0 then { Else it will go through all the cases }
   exit(0);
  case PosixErrNo of
   ESysENFILE,
   ESysEMFILE : Inoutres:=4;
   ESysENOENT : Inoutres:=2;
    ESysEBADF : Inoutres:=6;
   ESysENOMEM,
   ESysEFAULT : Inoutres:=217;
   ESysEINVAL : Inoutres:=218;
    ESysEPIPE,
    ESysEINTR,
      ESysEIO,
   ESysEAGAIN,
   ESysENOSPC : Inoutres:=101;
 ESysENAMETOOLONG : Inoutres := 3;
    ESysEROFS,
   ESysEEXIST,
   ESysENOTEMPTY,
   ESysEACCES : Inoutres:=5;
   ESysEISDIR : InOutRes:=5;
  else
    begin
       InOutRes := Integer(PosixErrno);
    end;
  end;
 PosixToRunError:=InOutRes;
end;

Function Errno2InoutRes : longint;

begin
  Errno2InoutRes:=PosixToRunError(Errno);
  InoutRes:=Errno2InoutRes;
end;

Procedure Do_Close(Handle:Longint);
Begin
  Fpclose(cint(Handle));
End;

Procedure Do_Erase(p:pchar);
var
 fileinfo : stat;
Begin
  { verify if the filename is actually a directory }
  { if so return error and do nothing, as defined  }
  { by POSIX                                       }
  if Fpstat(p,fileinfo)<0 then
   begin
     Errno2Inoutres;
     exit;
   end;
  if FpISDIR(fileinfo.st_mode) then
   begin
     InOutRes := 2;
     exit;
   end;
  Fpunlink(p);
  Errno2Inoutres;
End;

{ truncate at a given position }
procedure do_truncate (handle,fpos:longint);
begin
  { should be simulated in cases where it is not }
  { available.                                   }
  If Fpftruncate(handle,fpos)<0 Then
   Errno2Inoutres
  Else
   InOutRes:=0;
end;



Procedure Do_Rename(p1,p2:pchar);
Begin
  If Fprename(p1,p2)<0 Then
   Errno2Inoutres
  Else
   InOutRes:=0;
End;


Function Do_Write(Handle,Addr,Len:Longint):longint;
Begin
  repeat
    Do_Write:=Fpwrite(Handle,pchar(addr),len);
  until ErrNo<>ESysEINTR;
  If Do_Write<0 Then
   Begin
    Errno2InOutRes;
    Do_Write:=0;
   End
  else
   InOutRes:=0;
End;


Function Do_Read(Handle,Addr,Len:Longint):Longint;
Begin
  repeat
    Do_Read:=Fpread(Handle,pchar(addr),len);
  until ErrNo<>ESysEINTR;
  If Do_Read<0 Then
   Begin
    Errno2InOutRes;
    Do_Read:=0;
   End
  else
   InOutRes:=0;
End;

function Do_FilePos(Handle: Longint):longint;
Begin
  do_FilePos:=Fplseek(Handle, 0, SEEK_CUR);
  If Do_FilePos<0 Then
    Errno2InOutRes
  else
   InOutRes:=0;
End;

Procedure Do_Seek(Handle,Pos:Longint);
Begin
  If Fplseek(Handle, pos, SEEK_SET)<0 Then
   Errno2Inoutres
  Else
   InOutRes:=0;
End;

Function Do_SeekEnd(Handle:Longint): Longint;
begin
  Do_SeekEnd:=Fplseek(Handle,0,SEEK_END);
  If Do_SeekEnd<0 Then
   Errno2Inoutres
  Else
   InOutRes:=0;
end;

Function Do_FileSize(Handle:Longint): Longint;
var
  Info : Stat;
  Ret  : Longint;
Begin
  Ret:=Fpfstat(handle,info);
  If Ret=0 Then
   Do_FileSize:=Info.st_size
  else
   Do_FileSize:=0;
  If Ret<0 Then
   Errno2InOutRes
  Else
   InOutRes:=0;
End;

Procedure Do_Open(var f;p:pchar;flags:longint);
{
  FileRec and textrec have both Handle and mode as the first items so
  they could use the same routine for opening/creating.
  when (flags and $100)   the file will be append
  when (flags and $1000)  the file will be truncate/rewritten
  when (flags and $10000) there is no check for close (needed for textfiles)
}
var
  oflags : cint;
Begin
{ close first if opened }
  if ((flags and $10000)=0) then
   begin
     case FileRec(f).mode of
      fminput,fmoutput,fminout : Do_Close(FileRec(f).Handle);
      fmclosed : ;
     else
      begin
        inoutres:=102; {not assigned}
        exit;
      end;
     end;
   end;
{ reset file Handle }
  FileRec(f).Handle:=UnusedHandle;
{ We do the conversion of filemodes here, concentrated on 1 place }
  case (flags and 3) of
   0 : begin
         oflags :=O_RDONLY;
         FileRec(f).mode:=fminput;
       end;
   1 : begin
         oflags :=O_WRONLY;
         FileRec(f).mode:=fmoutput;
       end;
   2 : begin
         oflags :=O_RDWR;
         FileRec(f).mode:=fminout;
       end;
  end;
  if (flags and $1000)=$1000 then
   oflags:=oflags or (O_CREAT or O_TRUNC)
  else
   if (flags and $100)=$100 then
    oflags:=oflags or (O_APPEND);
{ empty name is special }
  if p[0]=#0 then
   begin
     case FileRec(f).mode of
       fminput :
         FileRec(f).Handle:=StdInputHandle;
       fminout, { this is set by rewrite }
       fmoutput :
         FileRec(f).Handle:=StdOutputHandle;
       fmappend :
         begin
           FileRec(f).Handle:=StdOutputHandle;
           FileRec(f).mode:=fmoutput; {fool fmappend}
         end;
     end;
     exit;
   end;
{ real open call }
  FileRec(f).Handle:=Fpopen(p,oflags,MODE_OPEN);
  if (ErrNo=ESysEROFS) and ((OFlags and O_RDWR)<>0) then
   begin
     Oflags:=Oflags and not(O_RDWR);
     FileRec(f).Handle:=Fpopen(p,oflags,MODE_OPEN);
   end;
  If Filerec(f).Handle<0 Then
   Errno2Inoutres
  else
   InOutRes:=0;
End;



{*****************************************************************************
                           Directory Handling
*****************************************************************************}

Procedure MkDir(Const s: String);[IOCheck];
Var
  Buffer: Array[0..255] of Char;
Begin
  If (s='') or (InOutRes <> 0) then
   exit;
  Move(s[1], Buffer, Length(s));
  Buffer[Length(s)] := #0;
  If Fpmkdir(@buffer, MODE_MKDIR)<0 Then
   Errno2Inoutres
  Else
   InOutRes:=0;
End;


Procedure RmDir(Const s: String);[IOCheck];
Var
  Buffer: Array[0..255] of Char;
Begin
  if (s = '.') then
    InOutRes := 16;
  If (s='') or (InOutRes <> 0) then
   exit;
  Move(s[1], Buffer, Length(s));
  Buffer[Length(s)] := #0;
  If Fprmdir(@buffer)<0 Then
   Errno2Inoutres
  Else
   InOutRes:=0;
End;


Procedure ChDir(Const s: String);[IOCheck];
Var
  Buffer: Array[0..255] of Char;
Begin
  If (s='') or (InOutRes <> 0) then
   exit;
  Move(s[1], Buffer, Length(s));
  Buffer[Length(s)] := #0;
  If Fpchdir(@buffer)<0 Then
   Errno2Inoutres
  Else
   InOutRes:=0;
  { file not exists is path not found under tp7 }
  if InOutRes=2 then
   InOutRes:=3;
End;

{ // $define usegetcwd}

procedure getdir(drivenr : byte;var dir : shortstring);
var
{$ifndef usegetcwd}
  cwdinfo      : stat;
  rootinfo     : stat;
  thedir,dummy : string[255];
  dirstream    : pdir;
  d            : pdirent;
  name         : string[255];
  thisdir      : stat;
{$endif}
  tmp          : string[255];

begin
{$ifdef usegetcwd}
 Fpgetcwd(@tmp[1],255);
 dir:=tmp;
{$else}
  dir:='';
  thedir:='';
  dummy:='';

  { get root directory information }
  tmp := '/'+#0;
  if Fpstat(@tmp[1],rootinfo)<0 then
      Begin
      Errno2Inoutres;
      Exit
     End
    Else
     InOutRes:=0;
  repeat
    tmp := dummy+'.'+#0;
    { get current directory information }
    if Fpstat(@tmp[1],cwdinfo)<0 then
      Begin
        Errno2Inoutres;
        Exit
      End
    Else
      InOutRes:=0;
    tmp:=dummy+'..'+#0;
    { open directory stream }
    { try to find the current inode number of the cwd }
    dirstream:=Fpopendir(@tmp[1]);
    if dirstream=nil then
       exit;
    repeat
      name:='';
      d:=Fpreaddir(dirstream);
      { no more entries to read ... }
      if not assigned(d) then
        begin
          break;
        end;
      tmp:=dummy+'../'+strpas(d^.d_name) + #0;
      if Fpstat(@tmp[1],thisdir)<0 then
        begin
          Errno2Inoutres;
          Exit
        End
      Else
        InOutRes:=0;
      { found the entry for this directory name }
      if (cwdinfo.st_dev=thisdir.st_dev) and (cwdinfo.st_ino=thisdir.st_ino) then
        begin
          { are the filenames of type '.' or '..' ? }
          { then do not set the name.               }
          if (not ((d^.d_name[0]='.') and ((d^.d_name[1]=#0) or
                  ((d^.d_name[1]='.') and (d^.d_name[2]=#0))))) then
            begin
              name:='/'+strpas(d^.d_name);
            end;
        end
    until (name<>'');
    If Fpclosedir(dirstream)<0 THen
      Begin
        Errno2Inoutres;
        Exit
      End
    Else
      InOutRes:=0;
    thedir:=name+thedir;
    dummy:=dummy+'../';
    if ((cwdinfo.st_dev=rootinfo.st_dev) and (cwdinfo.st_ino=rootinfo.st_ino)) then
      begin
        if thedir='' then
          dir:='/'
        else
          dir:=thedir;
        exit;
      end;
  until false;
 {$endif}
end;


{*****************************************************************************
                         SystemUnit Initialization
*****************************************************************************}


procedure SignalToRunerror(signo: cint); cdecl;
var
  res : word;
begin
    res:=0;
    if signo = SIGFPE then
     begin
        res := 200;
     end
    else
    if (signo = SIGILL) or (signo = SIGBUS) or (signo = SIGSEGV) then
      begin
        res := 216;
      end;
  { give runtime error at the position where the signal was raised }
  if res<>0 then
   begin
     HandleError(res);
   end;
end;


var
  act: SigActionRec;

Procedure InstallSignals;
var
  oldact: SigActionRec;
begin
  { Initialize the sigaction structure }
  { all flags and information set to zero }
  FillChar(act, sizeof(SigActionRec),0);
  { initialize handler                    }
  act.sa_handler := @SignalToRunError;
  FpSigAction(SIGFPE,@act,@oldact);
  FpSigAction(SIGSEGV,@act,@oldact);
  FpSigAction(SIGBUS,@act,@oldact);
  FpSigAction(SIGILL,@act,@oldact);
end;


procedure SetupCmdLine;
var
  bufsize,
  len,j,
  size,i : longint;
  found  : boolean;
  buf    : pchar;

  procedure AddBuf;
  begin
    reallocmem(cmdline,size+bufsize);
    move(buf^,cmdline[size],bufsize);
    inc(size,bufsize);
    bufsize:=0;
  end;

begin
  GetMem(buf,ARG_MAX);
  size:=0;
  bufsize:=0;
  i:=0;
  while (i<argc) do
   begin
     len:=strlen(argv[i]);
     if len>ARG_MAX-2 then
      len:=ARG_MAX-2;
     found:=false;
     for j:=1 to len do
      if argv[i][j]=' ' then
       begin
         found:=true;
         break;
       end;
     if bufsize+len>=ARG_MAX-2 then
      AddBuf;
     if found then
      begin
        buf[bufsize]:='"';
        inc(bufsize);
      end;
     move(argv[i]^,buf[bufsize],len);
     inc(bufsize,len);
     if found then
      begin
        buf[bufsize]:='"';
        inc(bufsize);
      end;
     if i<argc then
      buf[bufsize]:=' '
     else
      buf[bufsize]:=#0;
     inc(bufsize);
     inc(i);
   end;
  AddBuf;
  FreeMem(buf,ARG_MAX);
end;

(*
Begin
{ Set up signals handlers }
   InstallSignals;
{ Setup heap }
  InitHeap;
  InitExceptions;
{ Arguments }
  SetupCmdLine;
{ Setup stdin, stdout and stderr }
  OpenStdIO(Input,fmInput,StdInputHandle);
  OpenStdIO(Output,fmOutput,StdOutputHandle);
  OpenStdIO(StdOut,fmOutput,StdOutputHandle);
  OpenStdIO(StdErr,fmOutput,StdErrorHandle);
{ Reset IO Error }
  InOutRes:=0;
End.
*)
{
 $Log$
 Revision 1.2  2002-12-18 20:43:27  peter
   * removed stackcheck, the generic stackcheck is used
   * fixed return value for error conversion when no error was passed

 Revision 1.1  2002/12/18 16:43:26  marco
  * new unix rtl, linux part.....

 Revision 1.7  2002/11/14 12:18:03  marco
  * fixed Fptime call to (NIL)

 Revision 1.6  2002/10/27 17:21:29  marco
  * Only "difficult" functions + execvp + termios + rewinddir left to do

 Revision 1.5  2002/10/26 18:27:52  marco
  * First series POSIX calls commits. Including getcwd.

 Revision 1.4  2002/09/07 16:01:26  peter
   * old logs removed and tabs fixed

 Revision 1.3  2002/08/20 12:50:22  marco
  * New errno handling. Should be libc compatible.

 Revision 1.2  2002/08/10 13:42:36  marco
  * Fixes Posix dir copied to devel branch

 Revision 1.1.2.18  2002/03/10 11:45:02  carl
 * InOutRes := 16 with rmdir()
 * InOutRes := 5 more checking

 Revision 1.1.2.17  2002/03/03 15:11:51  carl
 * erase() bugfix (erasing a directory is done via rmdir() only!)

 Revision 1.1.2.16  2002/02/15 18:13:35  carl
 * bugfix for paramstr(0)

}
