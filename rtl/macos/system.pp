{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 2002 by Olle Raab

    FreePascal system unit for MacOS.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit System;

interface

{ include system-independent routine headers }
{$I systemh.inc}

{Platform specific information}
const
 LineEnding = #13;
 LFNSupport = true;
 DirectorySeparator = ':';
 DriveSeparator = ':';
 PathSeparator = ',';  // Is used in MPW
 FileNameCaseSensitive = false;

{ include heap support headers }
{$I heaph.inc}

const
{ Default filehandles }
  UnusedHandle    : Longint = -1;
  StdInputHandle  : Longint = 0;
  StdOutputHandle : Longint = 1;
  StdErrorHandle  : Longint = 2;

  sLineBreak = LineEnding;
  DefaultTextLineBreakStyle : TTextLineBreakStyle = tlbsCR;

var
  argc : longint;
  argv : ppchar;
  envp : ppchar;

implementation

{$define MACOS_USE_STDCLIB}


{ include system independent routines }
{$I system.inc}

{*********************** MacOS API *************}

{TODO: Perhaps the System unit should check the MacOS version to
ensure it is a supported version. }

{Below is some MacOS API routines needed for internal use.
Note, because the System unit is the most low level, it should not
depend on any other units, and in particcular not the MacOS unit.

Note: Types like Mac_XXX corresponds to the type XXX defined
in MacOS Universal Headers. The prefix is to avoid name clashes
with FPC types.}

type
  SignedByte = shortint;
  SignedBytePtr = ^SignedByte;
  OSErr = Integer;
  OSType = Longint;
  Mac_Ptr = pointer;
  Mac_Handle = ^Mac_Ptr;
  Str31 = string[31];
  Str32 = string[32];
  Str63 = string[63];
  Str255 = string[255];
  FSSpec = record
      vRefNum: Integer;
      parID: Longint;
      name: Str63;
   end;
  FSSpecPtr = ^FSSpec;
  AliasHandle = Mac_Handle;
  ScriptCode = Integer;

const
  noErr = 0;
  fnfErr = -43;   //File not found error
  fsFromStart = 1;
  fsFromLEOF = 2;

function Sbrk(logicalSize: Longint): Mac_Ptr ;
external 'InterfaceLib' name 'NewPtr';

procedure DisposeHandle(hdl: Mac_Handle);
external 'InterfaceLib';

function Mac_FreeMem: Longint;
external 'InterfaceLib' name 'FreeMem';

procedure Debugger;
external 'InterfaceLib';

procedure DebugStr(s: Str255);
external 'InterfaceLib';

procedure ExitToShell;
external 'InterfaceLib';

procedure SysBeep(dur: Integer);
external 'SysBeep';

function TickCount: Longint;
external 'InterfaceLib';

{$ifndef MACOS_USE_STDCLIB}

function FSpOpenDF(spec: FSSpec; permission: SignedByte;
  var refNum: Integer): OSErr;
external 'InterfaceLib';

function FSpCreate(spec: FSSpec; creator, fileType: OSType;
  scriptTag: ScriptCode): OSErr;
external 'InterfaceLib';

function FSClose(refNum: Integer): OSErr;
external 'InterfaceLib';

function FSRead(refNum: Integer; var count: Longint; buffPtr: Mac_Ptr): OSErr;
external 'InterfaceLib';

function FSWrite(refNum: Integer; var count: Longint; buffPtr: Mac_Ptr): OSErr;
external 'InterfaceLib';

function GetFPos(refNum: Integer; var filePos: Longint): OSErr;
external 'InterfaceLib';

function SetFPos(refNum: Integer; posMode: Integer; posOff: Longint): OSErr;
external 'InterfaceLib';

function GetEOF(refNum: Integer; var logEOF: Longint): OSErr;
external 'InterfaceLib';

function SetEOF(refNum: Integer; logEOF: Longint): OSErr;
external 'InterfaceLib';

function NewAliasMinimalFromFullPath(fullPathLength: Integer;
  fullPath: Mac_Ptr; zoneName: Str32; serverName: Str31;
  var alias: AliasHandle):OSErr;
external 'InterfaceLib';

function ResolveAlias(fromFile: FSSpecPtr; alias: AliasHandle;
  var target: FSSpec; var wasChanged: Boolean):OSErr;
external 'InterfaceLib';

{$else}

{**************** API to StdCLib in MacOS *************}
{The reason StdCLib is used is that it can easily be connected
to either SIOW or, in case of MPWTOOL, to MPW }

{The prefix C_ or c_ is used where names conflicts with pascal
keywords and names. Suffix Ptr is added for pointer to a type.}

type
  size_t = Longint;
  off_t = Longint;
  C_int = Longint;
  C_short = Integer;
  C_long = Longint;
  C_unsigned_int = Cardinal;

var
  errno: C_int; external name 'errno';
  MacOSErr: C_short; external name 'MacOSErr';

const
  _IOFBF = $00;
  _IOLBF = $40;
  _IONBF = $04;


  O_RDONLY = $00;     // Open for reading only.
  O_WRONLY = $01;     // Open for writing only.
  O_RDWR   = $02;     // Open for reading & writing.
  O_APPEND = $08;     // Write to the end of the file.
  O_RSRC   = $10;     // Open the resource fork.
  O_ALIAS  = $20;     // Open alias file.
  O_CREAT  = $100;    // Open or create a file.
  O_TRUNC  = $200;    // Open and truncate to zero length.
  O_EXCL   = $400;    // Create file only; fail if exists.
  O_BINARY = $800;    // Open as a binary stream.
  O_NRESOLVE = $4000; // Don't resolve any aliases.


  SEEK_SET = 0;
  SEEK_CUR = 1;
  SEEK_END = 2;

  FIOINTERACTIVE = $00006602; // If device is interactive
  FIOBUFSIZE     = $00006603; // Return optimal buffer size
  FIOFNAME       = $00006604;   // Return filename
  FIOREFNUM          = $00006605; // Return fs refnum
  FIOSETEOF          = $00006606; // Set file length

  TIOFLUSH = $00007408;       // discard unread input.  arg is ignored

function C_open(path: PChar; oflag: C_int): C_int;
  external 'StdCLib' name 'open';

function C_close(filedes: C_int): C_int;
  external 'StdCLib' name 'close';

function C_write(filedes: C_int; buf: pointer; nbyte: size_t): size_t;
  external 'StdCLib' name 'write';

{??? fread returns only when n items has been read. Does not specifically
return after newlines, so cannot be used for reading input from the console.}

function C_read(filedes: C_int; buf: pointer; nbyte: size_t): size_t;
  external 'StdCLib' name 'read';

function lseek(filedes: C_int; offset: off_t; whence: C_int): off_t;
  external 'StdCLib' name 'lseek';

function ioctl(filedes: C_int; cmd: C_unsigned_int; arg: pointer): C_int;
  external 'StdCLib' name 'ioctl';

function remove(filename: PChar): C_int;
  external 'StdCLib';

function c_rename(old, c_new: PChar): C_int;
  external 'StdCLib' name 'rename';

procedure c_exit(status: C_int);
  external 'StdCLib' name 'exit';

var
  {Is set to nonzero for MPWTool, zero otherwise.}
  StandAlone: C_int; external name 'StandAlone';

CONST

Sys_EPERM       = 1;    { No permission match }
Sys_ENOENT      = 2;    { No such file or directory }
Sys_ENORSRC     = 3;    { Resource not found *}
Sys_EINTR       = 4;    { System service interrupted *}
Sys_EIO         = 5;    { I/O error }
Sys_ENXIO       = 6;    { No such device or address }
Sys_E2BIG       = 7;    { Insufficient space for return argument * }
Sys_ENOEXEC     = 8;    { File not executable * }
Sys_EBADF       = 9;    { Bad file number }
Sys_ECHILD      = 10;   { No child processes }
Sys_EAGAIN      = 11;   { Resource temporarily unavailable * }
Sys_ENOMEM      = 12;   { Not enough space * }
Sys_EACCES      = 13;   { Permission denied }
Sys_EFAULT      = 14;   { Illegal filename * }
Sys_ENOTBLK     = 15;   { Block device required }
Sys_EBUSY       = 16;   { Device or resource busy }
Sys_EEXIST      = 17;   { File exists }
Sys_EXDEV       = 18;   { Cross-device link }
Sys_ENODEV      = 19;   { No such device }
Sys_ENOTDIR     = 20;   { Not a directory }
Sys_EISDIR      = 21;   { Is a directory }
Sys_EINVAL      = 22;   { Invalid parameter * }
Sys_ENFILE      = 23;   { File table overflow }
Sys_EMFILE      = 24;   { Too many open files }
Sys_ENOTTY      = 25;   { Not a typewriter }
Sys_ETXTBSY     = 26;   { Text file busy }
Sys_EFBIG       = 27;   { File too large }
Sys_ENOSPC      = 28;   { No space left on device }
Sys_ESPIPE      = 29;   { Illegal seek }
Sys_EROFS       = 30;   { Read-only file system }
Sys_EMLINK      = 31;   { Too many links }
Sys_EPIPE       = 32;   { Broken pipe }
Sys_EDOM        = 33;   { Math argument out of domain of func }
Sys_ERANGE      = 34;   { Math result not representable }

{ Note * is slightly different, compared to rtl/sunos/errno.inc}

{$endif}


{******************************************************}


   Procedure Errno2InOutRes;
{
  Convert ErrNo error to the correct Inoutres value
}

   Begin
  if errno = 0 then { Else it will go through all the cases }
   exit;
  //If errno<0 then Errno:=-errno;
     case Errno of
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
  Sys_ENOTDIR : Inoutres:=3;
    Sys_EROFS,
   Sys_EEXIST,
   Sys_EISDIR,
   Sys_EACCES : Inoutres:=5;
  Sys_ETXTBSY : Inoutres:=162;
  else
    InOutRes := Integer(errno);
  end;
  errno:=0;
end;

{*****************************************************************************
                              ParamStr/Randomize
*****************************************************************************}

{ number of args }
function paramcount : longint;
begin
  {paramcount := argc - 1;}
  paramcount:=0;
end;

{ argument number l }
function paramstr(l : longint) : string;
begin
  {if (l>=0) and (l+1<=argc) then
    paramstr:=strpas(argv[l])
  else}
    paramstr:='';
end;

{ set randseed to a new pseudo random value }
procedure randomize;
begin
  randseed:= Cardinal(TickCount);
end;

{*****************************************************************************
                              Heap Management
*****************************************************************************}

var
  { Pointer to a block allocated with the MacOS Memory Manager, which
    is used as the initial FPC heap. }
  theHeap: Mac_Ptr;
  intern_heapsize : longint;external name 'HEAPSIZE';

{ first address of heap }
function getheapstart:pointer;
begin
   getheapstart:= theHeap;
end;

{ current length of heap }
function getheapsize:longint;
begin
  getheapsize:= intern_heapsize ;
end;

{ include standard heap management }
{$I heap.inc}

{*****************************************************************************
                          Low Level File Routines
 ****************************************************************************}

function do_isdevice(handle:longint):boolean;
begin
  do_isdevice:=false;
end;

{ close a file from the handle value }
procedure do_close(h : longint);
begin
  {$ifdef MACOS_USE_STDCLIB}
  C_close(h);
  Errno2InOutRes;
  {$else}
  InOutRes:=1;
  if FSClose(h) = noErr then
    InOutRes:=0;
  {$endif}
end;

procedure do_erase(p : pchar);
begin
  {$ifdef MACOS_USE_STDCLIB}
  remove(p);
  Errno2InoutRes;
  {$else}
  InOutRes:=1;
  {$endif}
end;

procedure do_rename(p1,p2 : pchar);
begin
  {$ifdef MACOS_USE_STDCLIB}
  c_rename(p1,p2);
  Errno2InoutRes;
  {$else}
  InOutRes:=1;
  {$endif}
end;

function do_write(h,addr,len : longint) : longint;
begin
  {$ifdef MACOS_USE_STDCLIB}
  do_write:= C_write(h, pointer(addr), len);
  Errno2InoutRes;
  {$else}
  InOutRes:=1;
  if FSWrite(h, len, Mac_Ptr(addr)) = noErr then
    InOutRes:=0;
  do_write:= len;
  {$endif}
end;

function do_read(h,addr,len : longint) : longint;

var
  i: Longint;

begin
  {$ifdef MACOS_USE_STDCLIB}
  len:= C_read(h, pointer(addr), len);
  Errno2InoutRes;

  // TEMP BUGFIX Exchange CR to LF.
  for i:= 0 to len-1 do
    if SignedBytePtr(ord(addr) + i)^ = 13 then
      SignedBytePtr(ord(addr) + i)^ := 10;

  do_read:= len;

  {$else}
  InOutRes:=1;
  if FSread(h, len, Mac_Ptr(addr)) = noErr then
    InOutRes:=0;
  do_read:= len;
  {$endif}
end;

function do_filepos(handle : longint) : longint;

var
  pos: Longint;

begin
  {$ifdef MACOS_USE_STDCLIB}
  {This returns the filepos without moving it.}
  do_filepos := lseek(handle, 0, SEEK_CUR);
  Errno2InoutRes;
  {$else}
  InOutRes:=1;
  if GetFPos(handle, pos) = noErr then
    InOutRes:=0;
  do_filepos:= pos;
  {$endif}
end;

procedure do_seek(handle,pos : longint);
begin
  {$ifdef MACOS_USE_STDCLIB}
  lseek(handle, pos, SEEK_SET);
  Errno2InoutRes;
  {$else}
  InOutRes:=1;
  if SetFPos(handle, fsFromStart, pos) = noErr then
    InOutRes:=0;
  {$endif}
end;

function do_seekend(handle:longint):longint;
begin
  {$ifdef MACOS_USE_STDCLIB}
  lseek(handle, 0, SEEK_END);
  Errno2InoutRes;
  {$else}
  InOutRes:=1;
  if SetFPos(handle, fsFromLEOF, 0) = noErr then
    InOutRes:=0;
  {$endif}
end;

function do_filesize(handle : longint) : longint;

var
  aktfilepos: Longint;

begin
  {$ifdef MACOS_USE_STDCLIB}
  aktfilepos:= lseek(handle, 0, SEEK_CUR);
  if errno = 0 then
    begin
      do_filesize := lseek(handle, 0, SEEK_END);
      Errno2InOutRes; {Report the error from this operation.}
      lseek(handle, aktfilepos, SEEK_SET);   {Always try to move back,
         even in presence of error.}
    end
  else
    Errno2InOutRes;
  {$else}
  InOutRes:=1;
  if GetEOF(handle, pos) = noErr then
    InOutRes:=0;
  do_filesize:= pos;
  {$endif}
end;

{ truncate at a given position }
procedure do_truncate (handle,pos:longint);
begin
  {$ifdef MACOS_USE_STDCLIB}
  ioctl(handle, FIOSETEOF, pointer(pos));
  Errno2InoutRes;
  {$else}
  InOutRes:=1;
  do_seek(handle,pos);  //TODO: Is this needed (Does the user anticipate the filemarker is at the end?)
  if SetEOF(handle, pos) = noErr then
    InOutRes:=0;
  {$endif}
end;

{$ifndef MACOS_USE_STDCLIB}
function FSpLocationFromFullPath(fullPathLength: Integer;
  fullPath: Mac_Ptr; var spec: FSSpec ):OSErr;

var
  alias: AliasHandle;
  res: OSErr;
  wasChanged: Boolean;
  nullString: Str32;

begin
  nullString:= '';
  res:= NewAliasMinimalFromFullPath(fullPathLength,
             fullPath, nullString, nullString, alias);
  if res = noErr then
    begin
      res:= ResolveAlias(nil, alias, spec, wasChanged);
      DisposeHandle(Mac_Handle(alias));
end;
  FSpLocationFromFullPath:= res;
end;
{$endif}

procedure do_open(var f;p:pchar;flags:longint);
{
  filerec and textrec have both handle and mode as the first items so
  they could use the same routine for opening/creating.
  when (flags and $100)   the file will be append
  when (flags and $1000)  the file will be truncate/rewritten
  when (flags and $10000) there is no check for close (needed for textfiles)
}

var
  spec: FSSpec;
  creator, fileType: OSType;
  scriptTag: ScriptCode;
  refNum: Integer;
  res: OSErr;

  fh: Longint;

  oflags : longint;

Const
  fsCurPerm = 0;
  smSystemScript = -1;

begin
  // AllowSlash(p);

{ close first if opened }
  if ((flags and $10000)=0) then
   begin
     case filerec(f).mode of
       fminput,fmoutput,fminout : Do_Close(filerec(f).handle);
       fmclosed : ;
     else
      begin
        {not assigned}
        inoutres:=102;
        exit;
      end;
     end;
   end;

{ reset file handle }
  filerec(f).handle:=UnusedHandle;

  {$ifdef MACOS_USE_STDCLIB}

{ We do the conversion of filemodes here, concentrated on 1 place }
  case (flags and 3) of
   0 : begin
         oflags :=O_RDONLY;
         filerec(f).mode:=fminput;
       end;
   1 : begin
         oflags :=O_WRONLY;
         filerec(f).mode:=fmoutput;
       end;
   2 : begin
         oflags :=O_RDWR;
         filerec(f).mode:=fminout;
       end;
  end;

  if (flags and $1000)=$1000 then
    oflags:=oflags or (O_CREAT or O_TRUNC)
  else if (flags and $100)=$100 then
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

  fh:= C_open(p, oflags);

  //TODO Perhaps handle readonly filesystems, as in sysunix.inc
  Errno2InOutRes;
  if fh <> -1 then
    filerec(f).handle:= fh
  else
    filerec(f).handle:= UnusedHandle;

  {$else}

  InOutRes:=1;
  //creator:= $522A6368;        {'MPS ' -- MPW}
  //creator:= $74747874;        {'ttxt'}
  creator:= $522A6368;  {'R*ch' -- BBEdit}
  fileType:= $54455854; {'TEXT'}

  { reset file handle }
  filerec(f).handle:=UnusedHandle;

  res:= FSpLocationFromFullPath(StrLen(p), p, spec);
  if (res = noErr) or (res = fnfErr) then
   begin
      if FSpCreate(spec, creator, fileType, smSystemScript) = noErr then
        ;

      if FSpOpenDF(spec, fsCurPerm, refNum) = noErr then
        begin
          filerec(f).handle:= refNum;
          InOutRes:=0;
   end;
    end;

  if (filerec(f).handle=UnusedHandle) then
    begin
      //errno:=GetLastError;
      //Errno2InoutRes;
    end;
  {$endif}
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
procedure mkdir(const s:string);[IOCheck];
begin
  InOutRes:=1;
end;

procedure rmdir(const s:string);[IOCheck];
begin
  InOutRes:=1;
end;

procedure chdir(const s:string);[IOCheck];
begin
  InOutRes:=1;
end;

procedure GetDir (DriveNr: byte; var Dir: ShortString);

begin
  InOutRes := 1;
end;

{*****************************************************************************
                         SystemUnit Initialization
*****************************************************************************}

procedure setup_arguments;
         begin
         end;

procedure setup_environment;
         begin
         end;

{*****************************************************************************
                         System Dependent Exit code
*****************************************************************************}

Procedure system_exit;
begin
  {$ifndef MACOS_USE_STDCLIB}
  if StandAlone <> 0 then
    ExitToShell;
  {$else}
  c_exit(exitcode); //exitcode is only utilized by an MPW tool
  {$endif}
end;

procedure SysInitStdIO;
begin
  { Setup stdin, stdout and stderr }
  {$ifdef MACOS_USE_STDCLIB}
     OpenStdIO(Input,fmInput,StdInputHandle);
     OpenStdIO(Output,fmOutput,StdOutputHandle);
     OpenStdIO(StdOut,fmOutput,StdOutputHandle);
     OpenStdIO(StdErr,fmOutput,StdErrorHandle);
  {$endif }
end;

begin
  if false then //To save it from the dead code stripper
    begin
      //Included only to make them available for debugging in asm.
      Debugger;
      DebugStr('');
    end;
  { To be set if this is a GUI or console application }
  IsConsole := TRUE;
  { To be set if this is a library and not a program  }
  IsLibrary := FALSE;

  StackLength := InitialStkLen;
  StackBottom := SPtr - StackLength;

  { Setup heap }
  if Mac_FreeMem - intern_heapsize < 30000 then
    Halt(3);
  theHeap:= Sbrk(intern_heapsize);
  if theHeap = nil then
    Halt(3);  //According to MPW
  InitHeap;
  SysInitStdIO;

  { Setup environment and arguments }
  Setup_Environment;
  setup_arguments;
  { Reset IO Error }
  InOutRes:=0;
  errno:=0;
{$ifdef HASVARIANT}
  initvariantmanager;
{$endif HASVARIANT}
end.


{
  $Log$
  Revision 1.7  2003-09-27 11:52:35  peter
    * sbrk returns pointer

  Revision 1.6  2003/09/12 12:45:15  olle
    + filehandling complete
    + heaphandling complete
    + support for random
    * filehandling now uses filedecriptors in StdCLib
    * other minor changes
    - removed DEFINE MAC_SYS_RUNNABLE

  Revision 1.5  2003/01/13 17:18:55  olle
    + added support for rudimentary file handling

  Revision 1.4  2002/11/28 10:58:02  olle
    + added support for rudimentary heap

  Revision 1.3  2002/10/23 15:29:09  olle
    + added switch MAC_SYS_RUNABLE
    + added include of system.h etc
    + added standard globals
    + added dummy hook procedures

  Revision 1.2  2002/10/10 19:44:05  florian
    * changes from Olle to compile/link a simple program

  Revision 1.1  2002/10/02 21:34:31  florian
    * first dummy implementation
}
