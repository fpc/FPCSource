{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 2002-2004 by Olle Raab

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
 PathSeparator = ',';  {Is used in MPW and OzTeX}
 FileNameCaseSensitive = false;

 maxExitCode = 65535;

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

{*********************************}
{**  MacOS specific functions    **}
{*********************************}

{To be called at regular intervals, for lenghty tasks.
 Yield might give time for other tasks to run under the cooperative
 multitasked macos. For an MPW Tool, it also spinns the cursor.}

procedure Yield;

{To set mac file type and creator codes, to be used for files created
 by the FPC runtime library. They must be exactly 4 chars long.}

procedure SetDefaultMacOSFiletype(ftype: ShortString);
procedure SetDefaultMacOSCreator(creator: ShortString);

{*********************************}
{**  Available features on macos **}
{*********************************}


  var
    macosHasGestalt: Boolean;
    macosHasWaitNextEvent: Boolean;
    macosHasColorQD: Boolean;
    macosHasFPU: Boolean;
    macosSystemVersion: Integer;
    macosHasSysDebugger: Boolean = false;
    macosHasCFM: Boolean;

    macosHasAppleEvents: Boolean;
    macosHasAliasMgr: Boolean;


    macosHasFSSpec: Boolean;
    macosHasFindFolder: Boolean;


    macosHasScriptMgr: Boolean;
    macosNrOfScriptsInstalled: Integer;

    macosHasAppearance: Boolean;
    macosHasAppearance101: Boolean;
    macosHasAppearance11: Boolean;

    macosBootVolumeVRefNum: Integer;
    macosBootVolumeName: String[31];

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

It can be linked to either a graphical user interface application,
a standalone text only application (using SIOW) or
to an MPW tool, this is entirely controlled by the linking step.

It requires system 7 and CFM, which is always the case for PowerPC.

If a m68k version would be implemented, it would save a lot
of efforts if it also uses CFM. This System.pp should, with
minor modifications, probably work with m68k.

Initial working directory is the directory of the application,
or for an MPWTool, the working directory as set by the
Directory command in MPW.

Note about working directory. There is a facility in MacOS which
manages a working directory for an application, initially set to
the applications directory, or for an MPWTool, the tool's directory.
However, this requires the application to have a unique application
signature (creator code), to distinguish its working directory
from working directories of other applications. Due to the fact
that casual applications are anonymous in this sense (without an
application signature), this facility will not work. Also, this
working directory facility is not present in Carbon. Hence we
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
to directory ID each time the filesystem is accessed.

The initial working directory for an MPWTool, as considered by
FPC, is different from the MacOS working directory facility,
see above.


Possible improvements:
=====================

Perhaps handle readonly filesystems, as in sysunix.inc

}

{******** include system independent routines **********}
{$I system.inc}


{*********************** MacOS API *********************}

{This implementation uses StdCLib: }
{$define MACOS_USE_STDCLIB}

{Some MacOS API routines and StdCLib included for internal use:}
{$I macostp.inc}

{Note, because the System unit is the most low level, it should not
depend on any other units, and thus the macos api must be accessed
as an include file and not a unit.}

{The reason StdCLib is used is that it can easily be connected
to either SIOW or, in case of MPWTOOL, to MPW }

{If the Apples Universal Interfaces are used, the qd variable is required
to be allocated somewhere, so we do it here for the convenience to the user.}

var
  qd: QDGlobals; cvar;


{$ifdef MACOS_USE_STDCLIB}

{************** API to StdCLib in MacOS ***************}
{The reason StdCLib is used is that it can easily be connected
to either SIOW or, in case of MPWTOOL, to MPW }

{$endif}


{*********************** Macutils *********************}

{And also include the same utilities as in the macutils.pp unit.}

var
  {emulated working directory}
  workingDirectorySpec: FSSpec; cvar;
  {Also declared in macutils.pp as external. Declared here to be available
   to macutils.inc and below in this file.}

{$I macutils.inc}

{******************************************************}

function GetAppFileLocation (var spec: FSSpec): Boolean;
{Requires >= System 7}

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
   Sys_EMLINK : Inoutres:=5; //Happens when attempt to remove open file
    Sys_ENXIO : InOutRes:=152;
   Sys_ESPIPE : InOutRes:=156; //Illegal seek
  else
    InOutRes := Integer(errno);//TODO Exchange to something better
  end;
  errno:=0;
end;

Procedure OSErr2InOutRes(err: OSErr);
begin
  InOutRes:= MacOSErr2RTEerr(err);
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
                              MacOS specific functions
*****************************************************************************}
var
  defaultCreator: OSType =  $4D505320; {'MPS '   MPW Shell}
  //defaultCreator: OSType =  $74747874; {'ttxt'   Simple Text}
  defaultFileType: OSType = $54455854; {'TEXT'}

procedure Yield;

begin
  if StandAlone = 0 then
    SpinCursor(1);
end;

procedure SetDefaultMacOSFiletype(ftype: ShortString);

begin
  if Length(ftype) = 4 then
    defaultFileType:= PLongWord(@ftype[1])^;
end;

procedure SetDefaultMacOSCreator(creator: ShortString);

begin
  if Length(creator) = 4 then
    defaultCreator:= PLongWord(@creator[1])^;
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
      OS Memory allocation / deallocation
 ****************************************************************************}

{ function to allocate size bytes more for the program }
{ must return the first address of new data space or nil if failed }
function SysOSAlloc(size: ptrint): pointer;
begin
  result := NewPtr(size);
end;

{$define HAS_SYSOSFREE}

procedure SysOSFree(p: pointer; size: ptrint);
begin
  DisposePtr(p);
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

var
  spec: FSSpec;
  err: OSErr;
  res: Integer;

begin
  res:= PathArgToFSSpec(p, spec);
  if (res = 0) then
    begin
      if not IsDirectory(spec) then
        begin
          err:= FSpDelete(spec);
          OSErr2InOutRes(err);
        end
      else
        InOutRes:= 2;
    end
  else
    InOutRes:=res;
end;

procedure do_rename(p1,p2 : pchar);
var
  s1,s2: AnsiString;
begin
  {$ifdef MACOS_USE_STDCLIB}
  InOutRes:= PathArgToFullPath(p1, s1);
  if InOutRes <> 0 then
    exit;
  InOutRes:= PathArgToFullPath(p2, s2);
  if InOutRes <> 0 then
    exit;
  c_rename(PChar(s1),PChar(s2));
  Errno2InoutRes;
  {$else}
  InOutRes:=1;
  {$endif}
end;

function do_write(h:longint;addr:pointer;len : longint) : longint;
begin
  {$ifdef MACOS_USE_STDCLIB}
  do_write:= c_write(h, addr, len);
  Errno2InoutRes;
  {$else}
  InOutRes:=1;
  if FSWrite(h, len, Mac_Ptr(addr)) = noErr then
    InOutRes:=0;
  do_write:= len;
  {$endif}
end;

function do_read(h:longint;addr:pointer;len : longint) : longint;

var
  i: Longint;

begin
  {$ifdef MACOS_USE_STDCLIB}
  len:= c_read(h, addr, len);
  Errno2InoutRes;

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
  do_seekend:= lseek(handle, 0, SEEK_END);
  Errno2InoutRes;
  {$else}
  InOutRes:=1;
  if SetFPos(handle, fsFromLEOF, 0) = noErr then
    InOutRes:=0;
  {TODO Resulting file position is to be returned.}
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
  scriptTag: ScriptCode;
  refNum: Integer;

  err: OSErr;
  res: Integer;
  spec: FSSpec;

  fh: Longint;

  oflags : longint;
  fullPath: AnsiString;

  finderInfo: FInfo;

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
      InOutRes:= PathArgToFSSpec(p, spec);
      if (InOutRes = 0) or (InOutRes = 2) then
        begin
          err:= FSpGetFullPath(spec, fullPath, false);
          InOutRes:= MacOSErr2RTEerr(err);
        end;

      if InOutRes <> 0 then
        exit;

      p:= PChar(fullPath);
    end;


  fh:= c_open(p, oflags);
  if (fh = -1) and (errno = Sys_EROFS) and ((oflags and O_RDWR)<>0) then
    begin
      oflags:=oflags and not(O_RDWR);
      fh:= c_open(p, oflags);
    end;
  Errno2InOutRes;
  if fh <> -1 then
    begin
      if FileRec(f).mode in [fmoutput, fminout, fmappend] then
        begin
          {Change of filetype and creator is always done when a file is opened
          for some kind of writing. This ensures overwritten Darwin files will 
          get apropriate filetype. It must be done after file is opened,
          in the case the file did not previously exist.}

          FSpGetFInfo(spec, finderInfo);
          finderInfo.fdType:= defaultFileType;
          finderInfo.fdCreator:= defaultCreator;
          FSpSetFInfo(spec, finderInfo);
        end;
      filerec(f).handle:= fh;
    end
  else
    filerec(f).handle:= UnusedHandle;

  {$else}

  InOutRes:=1;

  { reset file handle }
  filerec(f).handle:=UnusedHandle;

  res:= FSpLocationFromFullPath(StrLen(p), p, spec);
  if (res = noErr) or (res = fnfErr) then
    begin
      if FSpCreate(spec, defaultCreator, defaultFileType, smSystemScript) = noErr then
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
  res: Integer;
begin
  If (s='') or (InOutRes <> 0) then
    exit;

  res:= PathArgToFSSpec(s, spec);
  if (res = 0) or (res = 2) then
    begin
      err:= FSpDirCreate(spec, smSystemScript, createdDirID);
      OSErr2InOutRes(err);
    end
  else
    InOutRes:=res;
end;

procedure rmdir(const s:string);[IOCheck];

var
  spec: FSSpec;
  err: OSErr;
  res: Integer;

begin
  If (s='') or (InOutRes <> 0) then
    exit;

  res:= PathArgToFSSpec(s, spec);

  if (res = 0) then
    begin
      if IsDirectory(spec) then
        begin
          err:= FSpDelete(spec);
          OSErr2InOutRes(err);
        end
      else
        InOutRes:= 20;
    end
  else
    InOutRes:=res;
end;

procedure chdir(const s:string);[IOCheck];
var
  spec, newDirSpec: FSSpec;
  err: OSErr;
  res: Integer;
begin
  if (s='') or (InOutRes <> 0) then
    exit;

  res:= PathArgToFSSpec(s, spec);
  if (res = 0) or (res = 2) then
    begin
      { The fictive file x is appended to the directory name to make
        FSMakeFSSpec return a FSSpec to a file in the directory.
        Then by clearing the name, the FSSpec then
        points to the directory. It doesn't matter whether x exists or not.}
      err:= FSMakeFSSpec (spec.vRefNum, spec.parID, ':'+spec.name+':x', newDirSpec);
      if (err = noErr) or (err = fnfErr) then
        begin
          workingDirectorySpec:= newDirSpec;
          workingDirectorySpec.name:='';
          InOutRes:= 0;
        end
      else
        begin
          {E g if the directory doesn't exist.}
          OSErr2InOutRes(err);
        end;
    end
  else
    InOutRes:=res;
end;

procedure getDir (DriveNr: byte; var Dir: ShortString);

var
  fullPath: AnsiString;
  pathHandleSize: Longint;

begin
  if FSpGetFullPath(workingDirectorySpec, fullPath, false) <> noErr then
    Halt(3);  {exit code 3 according to MPW}

  if Length(fullPath) <= 255 then {because dir is ShortString}
    InOutRes := 0
  else
    InOutRes := 1; //TODO Exchange to something better

  dir:= fullPath;
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
           {Nothing needs to be done here.}
         end;

procedure setup_environment;
         begin
         end;


{ FindSysFolder returns the (real) vRefNum, and the DirID of the current
system folder. It uses the Folder Manager if present, otherwise it falls
back to SysEnvirons. It returns zero on success, otherwise a standard
system error. }

function FindSysFolder(var foundVRefNum: Integer; var foundDirID: Longint): OSErr;

var
  gesResponse: Longint;
  envRec: SysEnvRec;
  myWDPB: WDPBRec;
  volName: String[34];
  err: OSErr;

begin
  foundVRefNum := 0;
  foundDirID := 0;
  if  macosHasGestalt
      and (Gestalt (FourCharCodeToLongword(gestaltFindFolderAttr), gesResponse) = noErr)
      and BitIsSet (gesResponse, gestaltFindFolderPresent) then
    begin { Does Folder Manager exist? }
       err := FindFolder (kOnSystemDisk, FourCharCodeToLongword(kSystemFolderType),
        kDontCreateFolder, foundVRefNum, foundDirID);
    end
  else
    begin
      { Gestalt can't give us the answer, so we resort to SysEnvirons }
      err := SysEnvirons (curSysEnvVers, envRec);
      if (err = noErr) then
        begin
          myWDPB.ioVRefNum := envRec.sysVRefNum;
          volName := '';
          myWDPB.ioNamePtr := @volName;
          myWDPB.ioWDIndex := 0;
          myWDPB.ioWDProcID := 0;
          err := PBGetWDInfoSync (@myWDPB);
          if (err = noErr) then
            begin
              foundVRefNum := myWDPB.ioWDVRefNum;
              foundDirID := myWDPB.ioWDDirID;
            end;
          end;
        end;
  FindSysFolder:= err;
end;

procedure InvestigateSystem;

  {$IFDEF CPUM68K}
  const
    _GestaltDispatch = $A0AD;
    _WaitNextEvent = $A860;
    _ScriptUtil = $A8B5;

    qdOffscreenTrap = $AB1D;
  {$ENDIF}

  var
    err: Integer;
    response: Longint;
    {$IFDEF CPUM68K}
    environs: SysEnvRec;
    {$ENDIF}

  {Vi rŠknar med att man kšr pŒ minst system 6.0.5.  DŒ finns bŒde Gestalt och GDevice med.}
  {Enligt Change Histrory Šr MacOS 6.0.5 mera konsistent mellan maskinmodellerna Šn fšregŒende system}

begin
  {$IFDEF CPUM68K}
  macosHasGestalt := TrapAvailable(_GestaltDispatch);
  {$ELSE}
  macosHasGestalt := true;  {There is always Gestalt on PowerPC}
  {$ENDIF}

  if not macosHasGestalt then    (* If we don't have Gestalt, then we can't have any System 7 features  *)
    begin
      {$IFDEF CPUM68K}
      {      Detta kan endast gŠlla pŒ en 68K maskin.}
      macosHasScriptMgr := TrapAvailable(_ScriptUtil);

      macosNrOfScriptsInstalled := 1; (* assume only Roman script, to start with  *)

      err := SysEnvirons(1, environs);
      if err = noErr then
        begin
          if environs.machineType < 0 then       { gammalt ROM}
            macosHasWaitNextEvent := FALSE
          else
            macosHasWaitNextEvent := TrapAvailable(_WaitNextEvent);
          macosHasColorQD := environs.hasColorQD;
          macosHasFPU := environs.hasFPU;
          macosSystemVersion := environs.systemVersion;
        end
      else
        begin
          macosHasWaitNextEvent := FALSE;
          macosHasColorQD := FALSE;
          macosHasFPU := FALSE;
          macosSystemVersion := 0;
        end;

      macosHasSysDebugger := (LongintPtr(MacJmp)^ <> 0);

      macosHasCFM := false;
      macosHasAppleEvents := false;
      macosHasAliasMgr := false;

      macosHasFSSpec := false;
      macosHasFindFolder := false;

      macosHasAppearance := false;
      macosHasAppearance101 := false;
      macosHasAppearance11 := false;
      {$IFDEF THINK_PASCAL}
      if (macosHasScriptMgr) then
        macosNrOfScriptsInstalled := GetEnvirons(smEnabled);
      {$ELSE}
      if (macosHasScriptMgr) then
        macosNrOfScriptsInstalled := GetScriptManagerVariable(smEnabled);  {Gamla rutinnamnet var GetEnvirons.}
      {$ENDIF}
      {$ENDIF}
    end
  else
    begin
      macosHasScriptMgr := Gestalt(FourCharCodeToLongword(gestaltScriptMgrVersion), response) = noErr;  {Fšr att ta reda pŒ om script mgr finns.}
      macosNrOfScriptsInstalled := 1; (* assume only Roman script, to start with  *)
      macosHasWaitNextEvent := true;

      if Gestalt(FourCharCodeToLongword(gestaltSystemVersion), response) = noErr then
        macosSystemVersion := response
      else
        macosSystemVersion := 0;  {Borde inte kunna hŠnda.}

      if Gestalt(FourCharCodeToLongword(gestaltOSAttr), response) = noErr then
        macosHasSysDebugger := BitIsSet(response, gestaltSysDebuggerSupport)
      else
        macosHasSysDebugger := false;

      if Gestalt(FourCharCodeToLongword(gestaltQuickdrawVersion), response) = noErr then
        macosHasColorQD := (response >= $0100)
      else
        macosHasColorQD := false;

      if Gestalt(FourCharCodeToLongword(gestaltFPUType), response) = noErr then
        macosHasFPU := (response <> gestaltNoFPU)
      else
        macosHasFPU := false;

      if Gestalt(FourCharCodeToLongword(gestaltCFMAttr), response) = noErr then
        macosHasCFM := BitIsSet(response, gestaltCFMPresent)
      else
        macosHasCFM := false;

      macosHasAppleEvents := Gestalt(FourCharCodeToLongword(gestaltAppleEventsAttr), response) = noErr;
      macosHasAliasMgr := Gestalt(FourCharCodeToLongword(gestaltAliasMgrAttr), response) = noErr;

      if Gestalt(FourCharCodeToLongword(gestaltFSAttr), response) = noErr then
        macosHasFSSpec := BitIsSet(response, gestaltHasFSSpecCalls)
      else
        macosHasFSSpec := false;
      macosHasFindFolder := Gestalt(FourCharCodeToLongword(gestaltFindFolderAttr), response) = noErr;

      if macosHasScriptMgr then
        begin
          err := Gestalt(FourCharCodeToLongword(gestaltScriptCount), response);
          if (err = noErr) then
            macosNrOfScriptsInstalled := Integer(response);
        end;

      if (Gestalt(FourCharCodeToLongword(gestaltAppearanceAttr), response) = noErr) then
        begin
          macosHasAppearance := BitIsSet(response, gestaltAppearanceExists);
          if Gestalt(FourCharCodeToLongword(gestaltAppearanceVersion), response) = noErr then
            begin
              macosHasAppearance101 := (response >= $101);
              macosHasAppearance11 := (response >= $110);
            end
        end
      else
        begin
          macosHasAppearance := false;
          macosHasAppearance101 := false;
          macosHasAppearance11 := false;
        end;
    end;
end;

{*****************************************************************************
                         System Dependent Exit code
*****************************************************************************}

Procedure system_exit;
var
  s: ShortString;
begin
  if StandAlone <> 0 then
    if exitcode <> 0 then
        begin
          Str(exitcode,s);
          if IsConsole then
            Writeln( '### Program exited with exit code ' + s)
          else if macosHasSysDebugger then
            DebugStr('A possible error occured, exit code: ' + s + '. Type "g" and return to continue.')
          else
            {Be quiet}
        end;

  {$ifndef MACOS_USE_STDCLIB}
  if StandAlone <> 0 then
    ExitToShell;
  {$else}
  c_exit(exitcode); {exitcode is only utilized by an MPW tool}
  {$endif}
end;

procedure SysInitStdIO;
begin
  { Setup stdin, stdout and stderr }
  {$ifdef MACOS_USE_STDCLIB}
     OpenStdIO(Input,fmInput,StdInputHandle);
     OpenStdIO(Output,fmOutput,StdOutputHandle);
     OpenStdIO(ErrOutput,fmOutput,StdErrorHandle);
     OpenStdIO(StdOut,fmOutput,StdOutputHandle);
     OpenStdIO(StdErr,fmOutput,StdErrorHandle);
  {$endif }
end;

function GetProcessID: SizeUInt;
begin
 GetProcessID := 1;
{$WARNING To be implemented - using GetProcessInformation???}
end;


var
  resHdl: Mac_Handle;
  isFolder, hadAlias, leafIsAlias: Boolean;
  dirStr: string[2];
  err: OSErr;
  dummySysFolderDirID: Longint;

begin
  InvestigateSystem; {Must be first}

  {Check requred features for system.pp to work.}
  if not macosHasFSSpec then
    Halt(3);  //exit code 3 according to MPW

  if FindSysFolder(macosBootVolumeVRefNum, dummySysFolderDirID) <> noErr then
    Halt(3);  //exit code 3 according to MPW

  if GetVolumeName(macosBootVolumeVRefNum, macosBootVolumeName) <> noErr then
    Halt(3);  //exit code 3 according to MPW

  { To be set if this is a GUI or console application }
  if StandAlone = 0 then
    IsConsole := true {Its an MPW tool}
  else
    begin
      resHdl:= Get1Resource(FourCharCodeToLongword('siow'),0);
      IsConsole := (resHdl <> nil); {A SIOW app is also a console}
      ReleaseResource(resHdl);
    end;

  { To be set if this is a library and not a program  }
  IsLibrary := FALSE;

  StackLength := InitialStkLen;
  StackBottom := SPtr - StackLength;

  { Setup working directory }
  if StandAlone <> 0 then
    begin
      if not GetAppFileLocation(workingDirectorySpec) then
        Halt(3);  //exit code 3 according to MPW
    end
  else
    begin
      { The fictive file x is used to make
        FSMakeFSSpec return a FSSpec to a file in the directory.
        Then by clearing the name, the FSSpec then
        points to the directory. It doesn't matter whether x exists or not.}
      dirStr:= ':x';
      err:= ResolveFolderAliases(0, 0, @dirStr, true,
           workingDirectorySpec, isFolder, hadAlias, leafIsAlias);
      workingDirectorySpec.name:='';
      if (err <> noErr) and (err <> fnfErr) then
        Halt(3);  //exit code 3 according to MPW
    end;

  { Setup heap }
  if StandAlone <> 0 then
    MaxApplZone;

  InitHeap;
  SysInitExceptions;
  SysInitStdIO;

  { Setup environment and arguments }
  Setup_Environment;
  setup_arguments;
  { Reset IO Error }
  InOutRes:=0;
  errno:=0;
(* This should be changed to a real value during *)
(* thread driver initialization if appropriate.  *)
  ThreadID := 1;
{$ifdef HASVARIANT}
  initvariantmanager;
{$endif HASVARIANT}
{$ifdef HASWIDESTRING}
  initwidestringmanager;
{$endif HASWIDESTRING}

  if StandAlone = 0 then
    begin
      InitGraf(@qd.thePort);
      SetFScaleDisable(true);
      InitCursorCtl(nil);
    end;
end.


{
  $Log$
  Revision 1.28  2005-02-01 20:22:49  florian
    * improved widestring infrastructure manager

  Revision 1.27  2005/01/24 18:51:23  olle
    * filetype/filecreator changed after the file is opened, in case the file did not previously exist

  Revision 1.26  2004/12/05 14:36:37  hajny
    + GetProcessID added

  Revision 1.25  2004/11/04 09:32:31  peter
  ErrOutput added

  Revision 1.24  2004/10/25 15:38:59  peter
    * compiler defined HEAP and HEAPSIZE removed

  Revision 1.23  2004/10/19 19:56:59  olle
    * Interface to StdLibC moved from system to macostp

  Revision 1.22  2004/09/30 19:58:42  olle
    + Added SetDefaultMacOS[Filetype|Creator]
    * Files written to by fpc rtl now always will get decent filetype/creator
    * Adapted to use FSpGetFullPath

  Revision 1.21  2004/09/12 19:51:02  olle
    + InitGraf called for MPW tool, which make strange bug disappear.
    * bugfix initial wd for MPW tool
    + Added SysInitExceptions

  Revision 1.20  2004/09/03 19:26:08  olle
    + added maxExitCode to all System.pp
    * constrained error code to be below maxExitCode in RunError et. al.

  Revision 1.19  2004/08/20 10:18:15  olle
    + added Yield routine

  Revision 1.18  2004/07/14 23:34:07  olle
    + added qd, the "QuickDraw globals"

  Revision 1.17  2004/06/21 19:23:34  olle
    + Variables describing misc OS features added
    + Detection of GUI app
    * Working directory for APPTYPE TOOL correct now
    + Exit code <> 0 written to, console for console apps, to system debugger (if installed) for GUI apps.
    * Misc fixes

  Revision 1.16  2004/06/17 16:16:13  peter
    * New heapmanager that releases memory back to the OS, donated
      by Micha Nelissen

  Revision 1.15  2004/05/11 18:05:41  olle
    + added call to MaxApplZone to have the whole MacOS heap available

  Revision 1.14  2004/04/29 11:27:36  olle
    * do_read/do_write addr arg changed to pointer
    * misc internal changes

  Revision 1.13  2004/02/04 15:17:16  olle
    * internal changes

  Revision 1.12  2004/01/20 23:11:20  hajny
    * ExecuteProcess fixes, ProcessID and ThreadID added

  Revision 1.11  2004/01/04 21:06:43  jonas
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
