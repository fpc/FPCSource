{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 2002-2003 by Olle Raab

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
type
{$ifdef CPU64}
  THandle = Int64;
{$else CPU64}
  THandle = Longint;
{$endif CPU64}

const
 LineEnding = #13;
 LFNSupport = true;
 DirectorySeparator = ':';
 DriveSeparator = ':';
 PathSeparator = ',';  // Is used in MPW and OzTeX
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

{
 MacOS paths
 ===========
 MacOS directory separator is a colon ":" which is the only character not
 allowed in filenames. 
 A path containing no colon or which begins with a colon is a partial path.
 E g ":kalle:petter" ":kalle" "kalle"
 All other paths are full (absolute) paths. E g "HD:kalle:" "HD:"
 When generating paths, one is safe is one ensures that all partial paths
 begins with a colon, and all full paths ends with a colon.
 In full paths the first name (e g HD above) is the name of a mounted volume.
 These names are not unique, because, for instance, two diskettes with the
 same names could be inserted. This means that paths on MacOS is not
 waterproof. In case of equal names the first volume found will do.
 Two colons "::" are the relative path to the parent. Three is to the
 grandparent etc.
}

implementation

{
About the implementation
========================
A MacOS application is assembled and linked by MPW (Macintosh
Programmers Workshop), which nowadays is free to use. For info
and download of MPW and MacOS api, see www.apple.com

It can be linked to either a standalone application (using SIOW) or
to an MPW tool, this is entirely controlled by the linking step.

It requires system 7 and CFM, which is always the case for PowerPC.

If a m68k version would be implemented, it would save a lot
of efforts if it also uses CFM. This System.pp should, with
minor modifications, probably work with m68k.

Initial working directory is the directory of the application,
or for an MPWTool, the MPW directory.

Note about working directory. There is a facility in MacOS which
manages a working directory for an application, initially set to
the applictaions directory, or for an MPWTool, the tool's directory.
However, this requires the application to have a unique application
signature (creator code), to distinguish its working directory
from working directories of other applications. Due to the fact
that casual applications are anonymous in this sense (without an
application signature), this facility will not work. Hence we
will manage a working directory by our self.


Deviations
==========

In current implementation, working directory is stored as
directory id. This means there is a possibility the user moves the
working directory or a parent to it, while the application uses it.
Then the path to the wd suddenly changes. This is AFAIK not in
accordance with other OS's. Although this is a minor caveat,
it is mentioned here. To overcome this the wd could be stored
as a path instead, but this imposes translations from fullpath
to directory id each time the filesystem is accessed.

The initial working directory for an MPWTool, as considered by
FPC, is different from the MacOS working directory facility,
see above.


Possible improvements:
=====================
TODO: Add check so that working directory cannot be removed. Alt ensure
the nothing crashes if wd is removed.

TODO: rmdir and erase does not differentiate between files and directories
thus removing both of them.

TODO: Check of the MacOS version (and prescence of CFM) to
ensure it is a supported version. only needed for m68k.
}

{This implementation uses StdCLib, which is included in the MPW.}
{$define MACOS_USE_STDCLIB}


{******** include system independent routines **********}
{$I system.inc}


{*********************** MacOS API *********************}
{Below is some MacOS API routines included for internal use.
Note, because the System unit is the most low level, it should not 
depend on any other units, and thus the macos api must be accessed 
as an include file and not a unit.}

{$I macostp.inc}

{$ifdef MACOS_USE_STDCLIB}

{************** API to StdCLib in MacOS ***************}
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
  FIOFNAME       = $00006604; // Return filename
  FIOREFNUM      = $00006605; // Return fs refnum
  FIOSETEOF      = $00006606; // Set file length

  TIOFLUSH = $00007408;       // discard unread input.  arg is ignored

function c_open(path: PChar; oflag: C_int): C_int; cdecl;
  external 'StdCLib' name 'open';

function c_close(filedes: C_int): C_int; cdecl;
  external 'StdCLib' name 'close';

function c_write(filedes: C_int; buf: pointer; nbyte: size_t): size_t; cdecl;
  external 'StdCLib' name 'write';

function c_read(filedes: C_int; buf: pointer; nbyte: size_t): size_t; cdecl;
  external 'StdCLib' name 'read';

function lseek(filedes: C_int; offset: off_t; whence: C_int): off_t; cdecl;
  external 'StdCLib' name 'lseek';

function ioctl(filedes: C_int; cmd: C_unsigned_int; arg: pointer): C_int; cdecl;
  external 'StdCLib' name 'ioctl';

function remove(filename: PChar): C_int; cdecl;
  external 'StdCLib';

function c_rename(old, c_new: PChar): C_int; cdecl;
  external 'StdCLib' name 'rename';

procedure c_exit(status: C_int); cdecl;
  external 'StdCLib' name 'exit';

  {cdecl is actually only needed for m68k}

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

var
  {working directory}
  curDirectorySpec: FSSpec;

function GetAppFileLocation (var spec: FSSpec): Boolean;
//Requires >= System 7

  var
   PSN: ProcessSerialNumber;
   info: ProcessInfoRec;
   appFileRefNum: Integer;
   appName: Str255;
   dummy: Mac_Handle;

begin
  begin
    PSN.highLongOfPSN := 0;
    PSN.lowLongOfPSN := kCurrentProcess;
    info.processInfoLength := SizeOf(info);
    info.processName := nil;
    info.processAppSpec := @spec;
    if GetProcessInformation(PSN, info) = noErr then
      begin
        spec.name := '';
        GetAppFileLocation := true;
      end
    else
      GetAppFileLocation := false;
  end
end;

{Gives the path for a given file or directory. If parent is true, 
 a path to the directory, where the file or directory is located,
 is returned. Functioning even with System 6}
function FSpGetFullPath (spec: FSSpec; var fullPathHandle: Mac_Handle;
  parent: Boolean): OSErr;

  var
    res: OSErr;
    pb: CInfoPBRec;

begin
  fullPathHandle:= NewHandle(0); { Allocate a zero-length handle }
  if fullPathHandle = nil then
    begin
      FSpGetFullPath:= MemError;
      Exit;
    end;

  if spec.parID = fsRtParID then { The object is a volume }
    begin
      if not parent then
        begin
          { Add a colon to make it a full pathname }
          spec.name := Concat(spec.name, ':');

          { We're done }
          Munger(fullPathHandle, 0, nil, 0, @spec.name[1], Length(spec.name));
          res := MemError;
        end
      else
        res := noErr;
    end
  else
    begin
      { The object isn't a volume }

      { Add the object name }
      if not parent then
        Munger(fullPathHandle, 0, nil, 0, @spec.name[1], Length(spec.name));

      { Get the ancestor directory names }
      pb.ioNamePtr := @spec.name;
      pb.ioVRefNum := spec.vRefNum;
      pb.ioDrParID := spec.parID;
      repeat { loop until we have an error or find the root directory }
        begin
          pb.ioFDirIndex := -1;
          pb.ioDrDirID := pb.ioDrParID;
          res := PBGetCatInfoSync(@pb);
          if res = noErr then
            begin
              { Append colon to directory name }
              spec.name := Concat(spec.name, ':');

              { Add directory name to fullPathHandle }
              Munger(fullPathHandle, 0, nil, 0, @spec.name[1], Length(spec.name));
              res := MemError;
            end
        end
      until not ((res = noErr) and (pb.ioDrDirID <> fsRtDirID));
    end;

  if res <> noErr then
    begin
      DisposeHandle(fullPathHandle);
      fullPathHandle:= nil;
    end;

  FSpGetFullPath := res;
end;

Procedure Errno2InOutRes;
{
  Convert ErrNo error to the correct InOutRes value.
  It seems that some of the errno is, in macos,
  used for other purposes than its original definition.
}

begin
  if errno = 0 then { Else it will go through all the cases }
    exit;
  case Errno of
   Sys_ENFILE,
   Sys_EMFILE : Inoutres:=4;
   Sys_ENOENT : Inoutres:=2;
    Sys_EBADF : Inoutres:=6;
   Sys_ENOMEM,
   Sys_EFAULT : Inoutres:=217; //TODO Exchange to something better
   Sys_EINVAL : Inoutres:=218; //TODO RTE 218 doesn't exist
   Sys_EAGAIN,
   Sys_ENOSPC : Inoutres:=101;
  Sys_ENOTDIR : Inoutres:=3;
    Sys_EPERM,
    Sys_EROFS,
   Sys_EEXIST,
   Sys_EISDIR,
    Sys_EINTR,  //Happens when attempt to rename a file fails
    Sys_EBUSY,  //Happens when attempt to remove a locked file
   Sys_EACCES,
  Sys_ETXTBSY,  //Happens when attempt to open an already open file
   Sys_EMLINK : Inoutres:=5; //Happens when attempt to remove open file
    Sys_ENXIO : InOutRes:=152;
   Sys_ESPIPE : InOutRes:=156; //Illegal seek
  else
    InOutRes := Integer(errno);//TODO Exchange to something better
  end;
  errno:=0;
end;

Procedure OSErr2InOutRes(err: OSErr);
{ Convert MacOS specific error codes to the correct InOutRes value}

begin
  if err = noErr then { Else it will go through all the cases }
    exit;

  case err of
    dirFulErr, { Directory full }
    dskFulErr  { disk full }
      :Inoutres:=101;
    nsvErr     { no such volume }
      :Inoutres:=3;
    ioErr,     { I/O error (bummers) }
    bdNamErr   { there may be no bad names in the final system! }
      :Inoutres:=1; //TODO Exchange to something better
    fnOpnErr   { File not open }
      :Inoutres:=103;
    eofErr,    { End of file }
    posErr     { tried to position to before start of file (r/w) }
      :Inoutres:=100;
    mFulErr    { memory full (open) or file won't fit (load) }
      :Inoutres:=1; //TODO Exchange to something better
    tmfoErr    { too many files open}
      :Inoutres:=4;
    fnfErr     { File not found }
      :Inoutres:=2;
    wPrErr     { diskette is write protected. }
      :Inoutres:=150;
    fLckdErr   { file is locked }
      :Inoutres:=5;
    vLckdErr   { volume is locked }
      :Inoutres:=150;
    fBsyErr    { File is busy (delete) }
      :Inoutres:=5;
    dupFNErr   { duplicate filename (rename) }
      :Inoutres:=5;
    opWrErr    { file already open with with write permission }
      :Inoutres:=5;
    rfNumErr,  { refnum error }
    gfpErr     { get file position error }
      :Inoutres:=1; //TODO Exchange to something better
    volOffLinErr   { volume not on line error (was Ejected) }
      :Inoutres:=152;
    permErr    { permissions error (on file open) }
      :Inoutres:=5;
    volOnLinErr{ drive volume already on-line at MountVol }
      :Inoutres:=0; //TODO Exchange to something other      
    nsDrvErr       { no such drive (tried to mount a bad drive num) }
      :Inoutres:=1; //TODO Perhaps exchange to something better
    noMacDskErr,   { not a mac diskette (sig bytes are wrong) }
    extFSErr       { volume in question belongs to an external fs }
      :Inoutres:=157; //TODO Perhaps exchange to something better
    fsRnErr,   { file system internal error:during rename the old 
                 entry was deleted but could not be restored. }
    badMDBErr  { bad master directory block }
      :Inoutres:=1; //TODO Exchange to something better
    wrPermErr  { write permissions error }
      :Inoutres:=5;
    dirNFErr   { Directory not found }
      :Inoutres:=3;
    tmwdoErr   { No free WDCB available }
      :Inoutres:=1; //TODO Exchange to something better
    badMovErr  { Move into offspring error }
      :Inoutres:=5;
    wrgVolTypErr   { Wrong volume type error [operation not 
                     supported for MFS] }
      :Inoutres:=1; //TODO Exchange to something better
    volGoneErr { Server volume has been disconnected. }
      :Inoutres:=152;

    diffVolErr         { files on different volumes }
      :Inoutres:=17;
    catChangedErr      { the catalog has been modified }
                       { OR comment: when searching with PBCatSearch }
      :Inoutres:=0; //TODO Exchange to something other      
    afpAccessDenied,   {  Insufficient access privileges for operation  }
    afpDenyConflict    {  Specified open/deny modes conflict with current open modes  }
      :Inoutres:=5;
    afpNoMoreLocks     {  Maximum lock limit reached  }
      :Inoutres:=5;
    afpRangeNotLocked, {  Tried to unlock range that was not locked by user  }
    afpRangeOverlap    {  Some or all of range already locked by same user  }
      :Inoutres:=1; //TODO Exchange to something better
    afpObjectTypeErr   {  File/Directory specified where Directory/File expected  }
      :Inoutres:=3;
    afpCatalogChanged  { OR comment: when searching with PBCatSearch }
      :Inoutres:=0; //TODO Exchange to something other      
    afpSameObjectErr  
      :Inoutres:=5; //TODO Exchange to something better

    memFullErr { Not enough room in heap zone }
      :Inoutres:=203;
  else
    InOutRes := 1; //TODO Exchange to something better
  end;
end;

function PathArgToFSSpec(s: string; var spec: FSSpec): Boolean;
var
  err: OSErr;
begin 
  err:= FSMakeFSSpec(curDirectorySpec.vRefNum,
      curDirectorySpec.parID, s, spec);

  if err in [ noErr, fnfErr] then
    PathArgToFSSpec:= true
  else
    begin
      OSErr2InOutRes(err);
      PathArgToFSSpec:= false;
    end;
end;

function PathArgToFullPath(s: string; var fullpath: AnsiString): Boolean;
var
  err: OSErr;
  spec: FSSpec;
  pathHandle: Mac_Handle;
begin
  PathArgToFullPath:= false;
  if PathArgToFSSpec(s, spec) then
    begin
      err:= FSpGetFullPath(spec, pathHandle, false);
      if err = noErr then
        begin 
          SetString(fullpath, pathHandle^, GetHandleSize(pathHandle));
          DisposeHandle(pathHandle);
          PathArgToFullPath:= true;
        end
      else
        OSErr2InOutRes(err);
    end;
end;

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

{*****************************************************************************
                              ParamStr/Randomize
*****************************************************************************}

{ number of args }
function paramcount : longint;
begin
  paramcount := argc - 1;
  //paramcount:=0;
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

{ function to allocate size bytes more for the program }
{ must return the first address of new data space or nil if failed }
function Sbrk(logicalSize: Longint): Mac_Ptr ;
external 'InterfaceLib' name 'NewPtr'; //Directly mapped to NewPtr


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
var
  err: OSErr;
{No error handling, according to the other targets, which seems reasonable,
because close might be used to clean up after an error.}
begin
  {$ifdef MACOS_USE_STDCLIB}
  c_close(h);
  // Errno2InOutRes;
  {$else}
  err:= FSClose(h);
  // OSErr2InOutRes(err);
  {$endif}
end;

procedure do_erase(p : pchar);
{this implementation cannot distinguish between directories and files}
var
  s: AnsiString;
begin
  {$ifdef MACOS_USE_STDCLIB}
  if not PathArgToFullPath(p, s) then
    exit;
  remove(PChar(s));
  Errno2InoutRes;
  {$else}
  InOutRes:=1;
  {$endif}
end;

procedure do_rename(p1,p2 : pchar);
var
  s1,s2: AnsiString;
begin
  {$ifdef MACOS_USE_STDCLIB}
  if not PathArgToFullPath(p1, s1) then
    exit;
  if not PathArgToFullPath(p2, s2) then
    exit;
  c_rename(PChar(s1),PChar(s2));
  Errno2InoutRes;
  {$else}
  InOutRes:=1;
  {$endif}
end;

function do_write(h,addr,len : longint) : longint;
begin
  {$ifdef MACOS_USE_STDCLIB}
  do_write:= c_write(h, pointer(addr), len);
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
  len:= c_read(h, pointer(addr), len);
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

procedure do_open(var f;p:pchar;flags:longint);
{
  filerec and textrec have both handle and mode as the first items so
  they could use the same routine for opening/creating.
  when (flags and $100)   the file will be append
  when (flags and $1000)  the file will be truncate/rewritten
  when (flags and $10000) there is no check for close (needed for textfiles)
}

var
  creator, fileType: OSType;
  scriptTag: ScriptCode;
  refNum: Integer;
  res: OSErr;

  fh: Longint;

  oflags : longint;
  s: AnsiString;

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
   end
  else
    begin
      if not PathArgToFullPath(p, s) then
        exit;
      p:= PChar(s);
    end;

  //TODO Perhaps handle readonly filesystems, as in sysunix.inc

  fh:= c_open(p, oflags);
  Errno2InOutRes;

  if fh <> -1 then
    filerec(f).handle:= fh
  else
    filerec(f).handle:= UnusedHandle;

  {$else}

  InOutRes:=1;
  //creator:= $522A6368; {'MPS ' -- MPW}
  //creator:= $74747874; {'ttxt'}
  creator:= $522A6368; {'R*ch' -- BBEdit}
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

{ #26 is not end of a file in MacOS ! }

{$i text.inc}

{*****************************************************************************
                           Directory Handling
*****************************************************************************}

procedure mkdir(const s:string);[IOCheck];
var
  spec: FSSpec;
  createdDirID: Longint;
  err: OSErr;
begin
  If (s='') or (InOutRes <> 0) then
    exit;
 
  if PathArgToFSSpec(s, spec) then
    begin
      err:= FSpDirCreate(spec, smSystemScript, createdDirID);
      OSErr2InOutRes(err);
    end;
end;

procedure rmdir(const s:string);[IOCheck];
{this implementation cannot distinguish between directories and files}
var
  spec: FSSpec;
  err: OSErr;
begin
  If (s='') or (InOutRes <> 0) then
    exit;

  if PathArgToFSSpec(s, spec) then
    begin
      err:= FSpDelete(spec);
      OSErr2InOutRes(err);
    end;
end;

procedure chdir(const s:string);[IOCheck];
var
  spec, newDirSpec: FSSpec;
  err: OSErr;
begin
  if (s='') or (InOutRes <> 0) then
    exit;

  if PathArgToFSSpec(s, spec) then
    begin
      { The fictive file x is appended to the directory name to make 
        FSMakeFSSpec return a FSSpec to a file in the directory.
        Then by clearing the name, the FSSpec then
        points to the directory. It doesn't matter whether x exists or not.}
      err:= FSMakeFSSpec (spec.vRefNum, spec.parID, ':'+spec.name+':x', newDirSpec);
      if err in [ noErr, fnfErr] then
        begin
          curDirectorySpec:= newDirSpec;
          curDirectorySpec.name:='';
          InOutRes:= 0;
        end
      else
        begin
          //E g if the directory doesn't exist.
          OSErr2InOutRes(err);
        end;
    end;
end;

procedure getDir (DriveNr: byte; var Dir: ShortString);
var
  pathHandle: Mac_Handle;
  pathHandleSize: Longint;
begin
  if FSpGetFullPath(curDirectorySpec, pathHandle, false) <> noErr then
    Halt(3);  //exit code 3 according to MPW

  pathHandleSize:= GetHandleSize(pathHandle);
  SetString(dir, pathHandle^, pathHandleSize);
  DisposeHandle(pathHandle);

  if pathHandleSize <= 255 then //because dir is ShortString
    InOutRes := 0
  else
    InOutRes := 1; //TODO Exchange to something better 
end;

{*****************************************************************************
                         SystemUnit Initialization
*****************************************************************************}

procedure pascalmain; external name 'PASCALMAIN';

{Main entry point in C style, needed to capture program parameters.
 For this to work, the system unit must be before the main program
 in the linking order.}
procedure main(argcparam: Longint; argvparam: ppchar; envpparam: ppchar); cdecl; [public];

begin
  argc:= argcparam;
  argv:= argvparam;
  envp:= envpparam;
  pascalmain;  {run the pascal main program}
end;

procedure setup_arguments;
         begin
           //Nothing needs to be done here.	
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

var
  pathHandle: Mac_Handle;

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

  { Setup working directory }
  if not GetAppFileLocation(curDirectorySpec) then
    Halt(3);  //exit code 3 according to MPW

  { Setup heap }
  if Mac_FreeMem - intern_heapsize < 30000 then
    Halt(3);  //exit code 3 according to MPW
  theHeap:= Sbrk(intern_heapsize);
  if theHeap = nil then
    Halt(3);  //exit code 3 according to MPW

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
  Revision 1.11  2004-01-04 21:06:43  jonas
    * make the C-main public

  Revision 1.10  2003/10/29 22:34:52  olle
    + handles program parameters for MPW
    + program start stub
    * improved working directory handling
    * minor changes
    + some documentation

  Revision 1.9  2003/10/17 23:44:30  olle
    + working direcory emulated
    + implemented directory handling procs
    + all proc which take a path param, now resolve it relative wd

  Revision 1.8  2003/10/16 15:43:13  peter
    * THandle is platform dependent

  Revision 1.7  2003/09/27 11:52:35  peter
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
