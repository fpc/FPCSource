{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{ no stack check in system }
{$S-}
unit system;

interface

{$ifdef SYSTEMDEBUG}
  {$define SYSTEMEXCEPTIONDEBUG}
{$endif SYSTEMDEBUG}

{$ifdef i386}
  {$define Set_i386_Exception_handler}
{$endif i386}


{ include system-independent routine headers }

{$I systemh.inc}

{ include heap support headers }
{Why the hell do i have to define that ???
 otherwise FPC_FREEMEM expects 2 parameters but the compiler only
 puhes the address}
{  DEFINE NEWMM}
{  I heaph.inc}

{Platform specific information}
const
 LineEnding = #13#10;
 LFNSupport = false; { ??? - that's how it was declared in dos.pp! }
 DirectorySeparator = '\';
 DriveSeparator = ':';
 PathSeparator = ';';
{ FileNameCaseSensitive is defined separately below!!! }

type
   { the fields of this record are os dependent  }
   { and they shouldn't be used in a program     }
   { only the type TCriticalSection is important }
   TRTLCriticalSection = packed record
      SemaHandle : LONGINT;
      SemaIsOpen : BOOLEAN;
   end;

{ include threading stuff }
{$i threadh.inc}

{ include heap support headers }
{$I heaph.inc}

CONST
  { Default filehandles }
   UnusedHandle    : longint = -1;
   StdInputHandle  : longint = 0;
   StdOutputHandle : longint = 0;
   StdErrorHandle  : longint = 0;

   FileNameCaseSensitive : boolean = false;

   sLineBreak : STRING = LineEnding;
   DefaultTextLineBreakStyle : TTextLineBreakStyle = tlbsCRLF;

VAR
   ArgC   : INTEGER;
   ArgV   : ppchar;

CONST   
   envp   : ppchar = nil;   {dummy to make heaptrc happy}


implementation

{ ?? why does this not work ?? DEFINE FPC_SYSTEM_HAS_MOVE}
{procedure move (const source; var dest; count : longint);
begin
  _memcpy (@dest, @source, count);
end;}

{ include system independent routines }

{$I system.inc}

{ some declarations for Netware API calls }
{$I nwsys.inc}
{$I errno.inc}

procedure setup_arguments;
begin
end;

procedure setup_environment;
begin
end;



procedure PASCALMAIN;external name 'PASCALMAIN';
procedure fpc_do_exit;external name 'FPC_DO_EXIT';


{*****************************************************************************
                         Startup
*****************************************************************************}


PROCEDURE _nlm_main (_ArgC : LONGINT; _ArgV : ppchar); CDECL; [public,alias: '_nlm_main'];
BEGIN
  ArgC := _ArgC;
  ArgV := _ArgV;
  PASCALMAIN;
END;


{$ifdef MT}
PROCEDURE CloseAllRemainingSemaphores; FORWARD;
{$endif}

{*****************************************************************************
                         System Dependent Exit code
*****************************************************************************}
Procedure system_exit;
begin
  {ConsolePrintf ('system_exit called'#13#10,0);}
  {$ifdef MT}
  CloseAllRemainingSemaphores;
  {$endif}
  _exit (ExitCode);
end;

{*****************************************************************************
                         Stack check code
*****************************************************************************}
procedure int_stackcheck(stack_size:longint);[public,alias:'FPC_STACKCHECK'];
{
  called when trying to get local stack if the compiler directive $S
  is set this function must preserve esi !!!! because esi is set by
  the calling proc for methods it must preserve all registers !!

  With a 2048 byte safe area used to write to StdIo without crossing
  the stack boundary
}
begin
  IF _stackavail > stack_size + 2048 THEN EXIT;
  HandleError (202);
end;

{*****************************************************************************
                              ParamStr/Randomize
*****************************************************************************}

{ number of args }
function paramcount : longint;
begin
  paramcount := argc - 1;
end;

{ argument number l }
function paramstr(l : longint) : string;
begin
  if (l>=0) and (l+1<=argc) then
   paramstr:=strpas(argv[l])
  else
   paramstr:='';
end;

{ set randseed to a new pseudo random value }
procedure randomize;
begin
  randseed := _time (NIL);
end;

{*****************************************************************************
                              Heap Management
*****************************************************************************}

{ first address of heap }
function getheapstart:pointer;
assembler;
asm
        leal    HEAP,%eax
end ['EAX'];

{ current length of heap }
function getheapsize:longint;
assembler;
asm
        movl    HEAPSIZE,%eax
end ['EAX'];

{ function to allocate size bytes more for the program }
{ must return the first address of new data space or -1 if fail }
FUNCTION Sbrk(size : longint):longint;
VAR P : POINTER;
BEGIN
  P := _malloc (size);
  IF P = NIL THEN
    Sbrk := -1
  ELSE
    Sbrk := LONGINT (P);
END;


{ include standard heap management }
{$I heap.inc}


{****************************************************************************
                        Low level File Routines
       All these functions can set InOutRes on errors
 ****************************************************************************}


PROCEDURE NW2PASErr (Err : LONGINT);
BEGIN
  if Err = 0 then { Else it will go through all the cases }
   exit;
  case Err of
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
 Sys_ENAMETOOLONG,
    Sys_ELOOP,
  Sys_ENOTDIR : Inoutres:=3;
    Sys_EROFS,
   Sys_EEXIST,
   Sys_EACCES : Inoutres:=5;
  Sys_EBUSY   : Inoutres:=162;
  end;
END;

FUNCTION errno : LONGINT;
BEGIN
  errno := __get_errno_ptr^;
END;

PROCEDURE Errno2Inoutres;
BEGIN
  NW2PASErr (errno);
END;

PROCEDURE SetFileError (VAR Err : LONGINT);
BEGIN
  IF Err >= 0 THEN
    InOutRes := 0
  ELSE
  BEGIN
    Err := errno;
    NW2PASErr (Err);
    Err := 0;
  END;
END;

{ close a file from the handle value }
procedure do_close(handle : longint);
VAR res : LONGINT;
begin
  res := _close (handle);
  IF res <> 0 THEN
    SetFileError (res)
  ELSE
    InOutRes := 0;
end;

procedure do_erase(p : pchar);
VAR res : LONGINT;
begin
  res := _unlink (p);
  IF Res < 0 THEN
    SetFileError (res)
  ELSE
    InOutRes := 0;
end;

procedure do_rename(p1,p2 : pchar);
VAR res : LONGINT;
begin
  res := _rename (p1,p2);
  IF Res < 0 THEN
    SetFileError (res)
  ELSE
    InOutRes := 0
end;

function do_write(h,addr,len : longint) : longint;
VAR res : LONGINT;
begin
  res := _write (h,POINTER(addr),len);
  IF res > 0 THEN
    InOutRes := 0
  ELSE
    SetFileError (res);
  do_write := res;
end;

function do_read(h,addr,len : longint) : longint;
VAR res : LONGINT;
begin
  res := _read (h,POINTER(addr),len);
  IF res > 0 THEN
    InOutRes := 0
  ELSE
    SetFileError (res);
  do_read := res;
end;


function do_filepos(handle : longint) : longint;
VAR res : LONGINT;
begin
  InOutRes:=1;
  res := _tell (handle);
  IF res < 0 THEN
    SetFileError (res)
  ELSE
    InOutRes := 0;
  do_filepos := res;
end;

CONST SEEK_SET = 0;	// Seek from beginning of file.
      SEEK_CUR = 1;	// Seek from current position.
      SEEK_END = 2;	// Seek from end of file.


procedure do_seek(handle,pos : longint);
VAR res : LONGINT;
begin
  res := _lseek (handle,pos, SEEK_SET);
  IF res >= 0 THEN
    InOutRes := 0
  ELSE
    SetFileError (res);
end;

function do_seekend(handle:longint):longint;
VAR res : LONGINT;
begin
  res := _lseek (handle,0, SEEK_END);
  IF res >= 0 THEN
    InOutRes := 0
  ELSE
    SetFileError (res);
  do_seekend := res;
end;


function do_filesize(handle : longint) : longint;
VAR res     : LONGINT;
begin
  res := _filelength (handle);
  IF res < 0 THEN
  BEGIN
    SetFileError (Res);
    do_filesize := -1;
  END ELSE
  BEGIN
    InOutRes := 0;
    do_filesize := res;
  END;
end;

{ truncate at a given position }
procedure do_truncate (handle,pos:longint);
VAR res : LONGINT;
begin
  res := _chsize (handle,pos);
  IF res <> 0 THEN
    SetFileError (res)
  ELSE
    InOutRes := 0;
end;

// mostly stolen from syslinux
procedure do_open(var f;p:pchar;flags:longint);
{
  filerec and textrec have both handle and mode as the first items so
  they could use the same routine for opening/creating.
  when (flags and $10)   the file will be append
  when (flags and $100)  the file will be truncate/rewritten
  when (flags and $1000) there is no check for close (needed for textfiles)
}
var
  oflags : longint;
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
         oflags := O_RDONLY;
         filerec(f).mode := fminput;
       end;
   1 : begin
         oflags := O_WRONLY;
         filerec(f).mode := fmoutput;
       end;
   2 : begin
         oflags := O_RDWR;
         filerec(f).mode := fminout;
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
  FileRec(f).Handle := _open(p,oflags,438);
  //WriteLn ('_open (',p,') liefert ',ErrNo, 'Handle: ',FileRec(f).Handle);
  // errno does not seem to be set on succsess ??
  IF FileRec(f).Handle < 0 THEN
    if (ErrNo=Sys_EROFS) and ((OFlags and O_RDWR)<>0) then
    begin  // i.e. for cd-rom
      Oflags:=Oflags and not(O_RDWR);
      FileRec(f).Handle := _open(p,oflags,438);
    end;
  IF FileRec(f).Handle < 0 THEN
    Errno2Inoutres
  ELSE
    InOutRes := 0;
End;

function do_isdevice(handle:longint):boolean;
begin
  do_isdevice := (_isatty (handle) > 0);
end;


{*****************************************************************************
                           UnTyped File Handling
*****************************************************************************}

{$i file.inc}

{*****************************************************************************
                           Typed File Handling
*****************************************************************************}

{$i typefile.inc}

{*****************************************************************************
                           Text File Handling
*****************************************************************************}

{ should we consider #26 as the  end of a file ? }
{?? $DEFINE EOF_CTRLZ}

{$i text.inc}

{*****************************************************************************
                           Directory Handling
*****************************************************************************}
procedure mkdir(const s : string);[IOCheck];
VAR S2 : STRING;
    Res: LONGINT;
BEGIN
  S2 := S;
  IF Length (S2) = 255 THEN DEC (BYTE(S2[0]));
  S2 := S2 + #0;
  Res := _mkdir (@S2[1]);
  IF Res = 0 THEN
    InOutRes:=0
  ELSE
    SetFileError (Res);
END;

procedure rmdir(const s : string);[IOCheck];
VAR S2 : STRING;
    Res: LONGINT;
BEGIN
  S2 := S;
  IF Length (S2) = 255 THEN DEC (BYTE(S2[0]));
  S2 := S2 + #0;
  Res := _rmdir (@S2[1]);
  IF Res = 0 THEN
    InOutRes:=0
  ELSE
    SetFileError (Res);
end;

procedure chdir(const s : string);[IOCheck];
VAR S2 : STRING;
    Res: LONGINT;
begin
  S2 := S;
  IF Length (S2) = 255 THEN DEC (BYTE(S2[0]));
  S2 := S2 + #0;
  Res := _chdir (@S2[1]);
  IF Res = 0 THEN
    InOutRes:=0
  ELSE
    SetFileError (Res);
end;

procedure getdir(drivenr : byte;var dir : shortstring);
VAR P  : ARRAY [0..255] OF CHAR;
    Len: LONGINT;
begin
  P[0] := #0;
  _getcwd (@P, SIZEOF (P));
  Len := _strlen (P);
  IF Len > 0 THEN
  BEGIN
    Move (P, dir[1], Len);
    BYTE(dir[0]) := Len;
  END ELSE
    InOutRes := 1;
end;


{*****************************************************************************
                             Thread Handling
*****************************************************************************}

const
  fpucw : word = $1332;

procedure InitFPU;assembler;

  asm
     fninit
     fldcw   fpucw
  end;


{ include threading stuff, this is os dependend part }
{$I thread.inc}



{*****************************************************************************
                         SystemUnit Initialization
*****************************************************************************}

Begin
{$ifdef MT}
  { the exceptions use threadvars so do this _before_ initexceptions }
  AllocateThreadVars;
{$endif MT}

{ Setup heap }
  InitHeap;
  InitExceptions;

{ Setup stdin, stdout and stderr }
  StdInputHandle := _fileno (LONGINT (_GetStdIn^));    // GetStd** returns **FILE !
  StdOutputHandle:= _fileno (LONGINT (_GetStdOut^));
  StdErrorHandle := _fileno (LONGINT (_GetStdErr^));

  OpenStdIO(Input,fmInput,StdInputHandle);
  OpenStdIO(Output,fmOutput,StdOutputHandle);
  OpenStdIO(StdOut,fmOutput,StdOutputHandle);
  OpenStdIO(StdErr,fmOutput,StdErrorHandle);
{ Setup environment and arguments }
  Setup_Environment;
  Setup_Arguments;
{ Reset IO Error }
  InOutRes:=0;
  {Delphi Compatible}
  IsLibrary := FALSE;
  IsConsole := TRUE;
End.
{
  $Log$
  Revision 1.7  2002-03-17 17:57:33  armin
  + threads and winsock2 implemented

  Revision 1.5  2001/06/18 14:26:16  jonas
    * move platform independent constant declarations after inclusion of
      systemh.inc

  Revision 1.4  2001/06/13 22:20:11  hajny
    + platform specific information

  Revision 1.3  2001/04/16 18:39:50  florian
    * updates from Armin commited

  Revision 1.2  2001/04/11 14:17:00  florian
    * added logs, fixed email address of Armin, it is
      diehl@nordrhein.de

  Revision 1.1  2001/04/11 14:14:12  florian
    * initial commit, thanks to Armin Diehl (diehl@nordrhein.de)

  Revision 1.2  2000/07/13 11:33:56  michael
  + removed logs

}
