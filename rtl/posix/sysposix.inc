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
                         Stack check code
*****************************************************************************}

{
{$IFOPT S+}
{$DEFINE STACKCHECK}
{$ENDIF}
{$S-}
procedure int_stackcheck(stack_size:longint);[public,alias:'FPC_STACKCHECK'];
var
 c: cardinal;
begin
 c := cardinal(Sptr) - cardinal(stack_size) - STACK_MARGIN;
 if (c <= cardinal(StackBottom)) then
     HandleError(202);
end;
{$IFDEF STACKCHECK}
{$S+}
{$ENDIF}
{$UNDEF STACKCHECK}
}

{*****************************************************************************
                       Misc. System Dependent Functions
*****************************************************************************}

procedure System_exit;
begin
   sys_exit(cint(ExitCode));
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
var
 t: time_t;
Begin
  randseed:=longint(sys_time(t));
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
   exit;
  case PosixErrNo of
   Sys_ENFILE,
   Sys_EMFILE : Inoutres:=4;
   Sys_ENOENT : Inoutres:=2;
    Sys_EBADF : Inoutres:=6;
   Sys_ENOMEM,
   Sys_EFAULT : Inoutres:=217;
   Sys_EINVAL : Inoutres:=218;
    Sys_EPIPE,
    Sys_EINTR,
      Sys_EIO,
   Sys_EAGAIN,
   Sys_ENOSPC : Inoutres:=101;
 Sys_ENAMETOOLONG : Inoutres := 3;
    Sys_EROFS,
   Sys_EEXIST,
   Sys_ENOTEMPTY,
   Sys_EACCES : Inoutres:=5;
   Sys_EISDIR : InOutRes:=5;
  else
    begin
       InOutRes := Integer(PosixErrno);
    end;
  end;
end;

Function Errno2InoutRes : longint;

begin
  Errno2InoutRes:=PosixToRunError(Errno);
  InoutRes:=Errno2InoutRes;
end;

Procedure Do_Close(Handle:Longint);
Begin
  sys_close(cint(Handle));
End;

Procedure Do_Erase(p:pchar);
var
 fileinfo : stat;
Begin
  { verify if the filename is actually a directory }
  { if so return error and do nothing, as defined  }
  { by POSIX					   }
  if sys_stat(p,fileinfo)<0 then
   begin
     Errno2Inoutres;
     exit;
   end;
  if S_ISDIR(fileinfo.st_mode) then
   begin
     InOutRes := 2;
     exit;
   end;
  sys_unlink(p);
  Errno2Inoutres;
End;

{ truncate at a given position }
procedure do_truncate (handle,fpos:longint);
begin
  { should be simulated in cases where it is not }
  { available.                                   }
  If sys_ftruncate(handle,fpos)<0 Then
   Errno2Inoutres
  Else
   InOutRes:=0;
end;



Procedure Do_Rename(p1,p2:pchar);
Begin
  If sys_rename(p1,p2)<0 Then
   Errno2Inoutres
  Else
   InOutRes:=0;
End;


Function Do_Write(Handle,Addr,Len:Longint):longint;
Begin
  repeat
    Do_Write:=sys_write(Handle,pchar(addr),len);
  until ErrNo<>Sys_EINTR;
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
    Do_Read:=sys_read(Handle,pchar(addr),len);
  until ErrNo<>Sys_EINTR;
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
  do_FilePos:=sys_lseek(Handle, 0, SEEK_CUR);
  If Do_FilePos<0 Then
    Errno2InOutRes
  else
   InOutRes:=0;
End;

Procedure Do_Seek(Handle,Pos:Longint);
Begin
  If sys_lseek(Handle, pos, SEEK_SET)<0 Then
   Errno2Inoutres 
  Else
   InOutRes:=0;
End;

Function Do_SeekEnd(Handle:Longint): Longint;
begin
  Do_SeekEnd:=sys_lseek(Handle,0,SEEK_END);
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
  Ret:=sys_fstat(handle,info);
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
  FileRec(f).Handle:=sys_open(p,oflags,MODE_OPEN);
  if (ErrNo=Sys_EROFS) and ((OFlags and O_RDWR)<>0) then
   begin
     Oflags:=Oflags and not(O_RDWR);
     FileRec(f).Handle:=sys_open(p,oflags,MODE_OPEN);
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
  If sys_mkdir(@buffer, MODE_MKDIR)<0 Then
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
  If sys_rmdir(@buffer)<0 Then
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
  If sys_chdir(@buffer)<0 Then
   Errno2Inoutres
  Else
   InOutRes:=0;
  { file not exists is path not found under tp7 }
  if InOutRes=2 then
   InOutRes:=3;
End;



procedure getdir(drivenr : byte;var dir : shortstring);
var
  cwdinfo      : stat;
  rootinfo     : stat;
  thedir,dummy : string[255];
  dirstream    : pdir;
  d            : pdirent;
  name         : string[255];
  tmp          : string[255];
  thisdir      : stat;
begin
  dir:='';
  thedir:='';
  dummy:='';

  { get root directory information }
  tmp := '/'+#0;
  if sys_stat(@tmp[1],rootinfo)<0 then
      Begin
      Errno2Inoutres;
      Exit
     End
    Else 
     InOutRes:=0;
  repeat
    tmp := dummy+'.'+#0;
    { get current directory information }
    if sys_stat(@tmp[1],cwdinfo)<0 then
      Begin
        Errno2Inoutres;
        Exit
      End
    Else 
      InOutRes:=0;
    tmp:=dummy+'..'+#0;
    { open directory stream }
    { try to find the current inode number of the cwd }
    dirstream:=sys_opendir(@tmp[1]);
    if dirstream=nil then
       exit;
    repeat
      name:='';
      d:=sys_readdir(dirstream);
      { no more entries to read ... }
      if not assigned(d) then
        begin
          break;
        end;
      tmp:=dummy+'../'+strpas(d^.d_name) + #0;
      if sys_stat(@tmp[1],thisdir)<0 then
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
    If sys_closedir(dirstream)<0 THen
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
  sys_SigAction(SIGFPE,act,oldact);
  sys_SigAction(SIGSEGV,act,oldact);
  sys_SigAction(SIGBUS,act,oldact);
  sys_SigAction(SIGILL,act,oldact);
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
 Revision 1.3  2002-08-20 12:50:22  marco
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

 Revision 1.1.2.15  2001/12/03 03:15:15  carl
 * update readdir prototype

 Revision 1.1.2.14  2001/09/27 02:24:43  carl
 * correct problem with getting paramstr(0) when in root

 Revision 1.1.2.13  2001/08/15 01:05:22  carl
 + add do_truncate()

 Revision 1.1.2.12  2001/08/13 09:38:12  carl
 * changed prototype of sys_readdir
 * bugfix of problems of changing signs with errno!

 Revision 1.1.2.11  2001/08/13 05:55:43  carl
 - removed some debug code

 Revision 1.1.2.10  2001/08/08 02:01:03  carl
 * bugfix of getdir() with root

 Revision 1.1.2.9  2001/08/03 02:00:26  carl
 + hack :(... for heap management.
 + correct I/O bug (to test) should be also updated in linux

 Revision 1.1.2.8  2001/07/21 19:20:52  carl
 + getdir() implemented
 + MAX_ARGS define now used

 Revision 1.1.2.7  2001/07/14 04:18:39  carl
 + started debugging getdir()

 Revision 1.1.2.6  2001/07/08 04:46:43  carl
 * correct parameter calls to sigaction

 Revision 1.1.2.5  2001/07/08 00:38:04  carl
 + updates

 Revision 1.1.2.3  2001/07/06 11:59:58  carl
 * renamed some defines
 * correct includes

 Revision 1.1.2.2  2001/07/06 11:42:28  carl
 * modified for more compliance

 Revision 1.1.2.1  2001/07/06 11:22:18  carl
 + add files for POSIX

}
