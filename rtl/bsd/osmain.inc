{
    $Id$
    This file is part of the Free Pascal run time library.

    Main OS dependant body of the system unit, loosely modelled
    after POSIX.  *BSD version (Linux version is near identical)

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

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
//   if l=0 then
//     begin
//       paramstr := execpathstr;
//     end
//   else
     paramstr:=strpas(argv[l]);
 end;

Procedure Randomize;
Begin
  randseed:=longint(Fptime(nil));
End;


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
  Errno2InoutRes:=PosixToRunError(getErrno);
  InoutRes:=Errno2InoutRes;
end;

Procedure Do_Close(Handle:thandle);
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
  if FpS_ISDIR(fileinfo.st_mode) then
   begin
     InOutRes := 2;
     exit;
   end;
  if Fpunlink(p)<0 then
   Errno2Inoutres
  Else
   InOutRes:=0;
End;

{ truncate at a given position }
procedure do_truncate (handle:thandle;fpos:longint);
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


Function Do_Write(Handle:thandle;Addr:Pointer;Len:Longint):longint;

var j : cint;
Begin
  repeat
    Do_Write:=Fpwrite(Handle,addr,len);
    j:=geterrno;
  until (do_write<>-1) or ((j<>ESysEINTR) and (j<>ESysEAgain));
  If Do_Write<0 Then
   Begin
    Errno2InOutRes;
    Do_Write:=0;
   End
  else
   InOutRes:=0;
End;


Function Do_Read(Handle:thandle;Addr:Pointer;Len:Longint):Longint;

var j:cint;

Begin
  repeat
    Do_Read:=Fpread(Handle,addr,len);
    j:=geterrno;
  until (do_read<>-1) or ((j<>ESysEINTR) and (j<>ESysEAgain));
  If Do_Read<0 Then
   Begin
    Errno2InOutRes;
    Do_Read:=0;
   End
  else
   InOutRes:=0;
End;

function Do_FilePos(Handle: thandle):longint;
Begin
  do_FilePos:=Fplseek(Handle, 0, SEEK_CUR);
  If Do_FilePos<0 Then
    Errno2InOutRes
  else
   InOutRes:=0;
End;

Procedure Do_Seek(Handle:thandle;Pos:Longint);
Begin
  If Fplseek(Handle, pos, SEEK_SET)<0 Then
   Errno2Inoutres
  Else
   InOutRes:=0;
End;

Function Do_SeekEnd(Handle:thandle): Longint;
begin
  Do_SeekEnd:=Fplseek(Handle,0,SEEK_END);
  If Do_SeekEnd<0 Then
   Errno2Inoutres
  Else
   InOutRes:=0;
end;

Function Do_FileSize(Handle:thandle): Longint;
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
  if (FileRec(f).Handle<0) and
    (getErrNo=ESysEROFS) and ((OFlags and O_RDWR)<>0) then
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
 Fpgetcwd(@tmp[1],4096);
 dir:=tmp;
{$else}
  dir:='';
  thedir:='';
  dummy:='';

  { get root directory information }
  tmp := '/'+#0;
  if Fpstat(@tmp[1],rootinfo)<0 then
    Exit;
  repeat
    tmp := dummy+'.'+#0;
    { get current directory information }
    if Fpstat(@tmp[1],cwdinfo)<0 then
      Exit;
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
        break;
      tmp:=dummy+'../'+strpas(d^.d_name) + #0;
      if (Fpstat(@tmp[1],thisdir)=0) then
       begin
         { found the entry for this directory name }
         if (cwdinfo.st_dev=thisdir.st_dev) and (cwdinfo.st_ino=thisdir.st_ino) then
          begin
            { are the filenames of type '.' or '..' ? }
            { then do not set the name.               }
            if (not ((d^.d_name[0]='.') and ((d^.d_name[1]=#0) or
                    ((d^.d_name[1]='.') and (d^.d_name[2]=#0))))) then
              name:='/'+strpas(d^.d_name);
          end;
       end;
    until (name<>'');
    if Fpclosedir(dirstream)<0 then
      Exit;
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

function  reenable_signal(sig : longint) : boolean;
var
  e,oe : TSigSet;
  i,j : byte;
begin
  fillchar(e,sizeof(e),#0);
  fillchar(oe,sizeof(oe),#0);
  { set is 1 based PM }
  dec(sig);
  i:=sig mod 32;
  j:=sig div 32;
  e[j]:=1 shl i;
  fpsigprocmask(SIG_UNBLOCK,@e,@oe);
  reenable_signal:=geterrno=0;
end;

{$i sighnd.inc}

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
  act.sa_handler :=@SignalToRunError;
  act.sa_flags:=SA_SIGINFO;
  FpSigAction(SIGFPE,act,oldact);
  FpSigAction(SIGSEGV,act,oldact);
  FpSigAction(SIGBUS,act,oldact);
  FpSigAction(SIGILL,act,oldact);
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

{
   $Log$
   Revision 1.16  2004-10-25 15:38:59  peter
     * compiler defined HEAP and HEAPSIZE removed

   Revision 1.15  2004/07/17 15:20:55  jonas
     * don't use O_CREATE when opening a file for appending (fixes tw1744)

   Revision 1.14  2004/05/16 18:51:20  peter
     * use thandle in do_*

   Revision 1.13  2004/04/22 21:10:56  peter
     * do_read/do_write addr argument changed to pointer

   Revision 1.12  2004/01/06 15:42:05  marco
    * o_creat added when o_append

   Revision 1.11  2004/01/03 14:56:10  marco
    * typo fix

   Revision 1.10  2004/01/03 12:35:39  marco
    * sighnd to separate file, like linux. Some comments removed

   Revision 1.9  2003/12/30 12:26:21  marco
    * FPC_USE_LIBC

   Revision 1.8  2003/12/21 20:31:50  peter
     * fix getdir when directory contains files that give EACCESS

   Revision 1.7  2003/12/14 14:47:02  marco
    * fix for repeating 'x' bug

   Revision 1.6  2003/11/18 10:12:25  marco
    * Small fixes for EAGAIN. bunxfunc only has comments added.

   Revision 1.5  2003/10/27 17:12:45  marco
    * fixes for signal handling.

   Revision 1.4  2003/10/26 17:01:04  marco
    * moved sigprocmask to system

   Revision 1.3  2003/09/27 13:04:58  peter
     * fpISxxx renamed

   Revision 1.2  2003/05/29 20:54:09  marco
    * progname fix.

   Revision 1.1  2003/01/05 19:01:28  marco
    * FreeBSD compiles now with baseunix mods.


}
