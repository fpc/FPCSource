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

{If MAC_SYS_RUNABLE is defined, this file can be included in a
 runnable program, but it then lacks lot of features. If not defined
 it tries to be faithful to a real system.pp, but it may not be
 able to assemble and link. The switch is only temporary, and only for
 use when system.pp is developed.}

{$Y-}

{$ifdef MAC_SYS_RUNABLE}

type
   integer = -32768 .. 32767;
   byte =0..255;
   shortint=-128..127;
   word=0..65535;
   longint=+(-$7FFFFFFF-1)..$7FFFFFFF;
   pchar=^char;

{$else}

{At the moment we do not support threadvars}
{$undef HASTHREADVAR}

{$I systemh.inc}

{$I heaph.inc}


{Platform specific information}
const
 LineEnding = #13;
 LFNSupport = true;
 DirectorySeparator = ':';
 DriveSeparator = ':';
 PathSeparator = ';';
 FileNameCaseSensitive = false;

const
  UnusedHandle    = 0;
  StdInputHandle  = 0;
  StdOutputHandle = 0;
  StdErrorHandle  = 0;

  sLineBreak : string[1] = LineEnding;
  DefaultTextLineBreakStyle : TTextLineBreakStyle = tlbsCR;

var
  argc : longint;
  argv : ppchar;
  envp : ppchar;

{$endif}

implementation

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
  OSErr = Integer;
  OSType = Longint;
  Mac_Ptr = pointer;
  Mac_Handle = ^Mac_Ptr;
  Str31 = string[31];
  Str32 = string[32];
  Str63 = string[63];
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

function NewPtr(logicalSize: Longint): Mac_Ptr ;
external 'InterfaceLib';

procedure DisposeHandle(hdl: Mac_Handle);
external 'InterfaceLib';

procedure Debugger;
external 'InterfaceLib';

procedure ExitToShell;
external 'InterfaceLib';

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

{$ifdef MAC_SYS_RUNABLE}

procedure do_exit;[public,alias:'FPC_DO_EXIT'];

begin
end;

procedure fpc_initializeunits;[public,alias:'FPC_INITIALIZEUNITS'];

begin
end;

{$else}

{$I system.inc}

{*********************** ??????? *************}

procedure SysInitStdIO;
begin
end;

{*****************************************************************************}

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
  ExitToShell;
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
  {regs.realeax:=$2c00;
  sysrealintr($21,regs);
  hl:=regs.realedx and $ffff;
  randseed:=hl*$10000+ (regs.realecx and $ffff);}
  randseed:=0;
end;

{*****************************************************************************
                              Heap Management
*****************************************************************************}
const
  theHeapSize = 300000;	//TODO: Use heapsize set by user.

var
  { Pointer to a block allocated with the MacOS Memory Manager, which 
    is used as the FPC heap }
  theHeap: Mac_Ptr;

{ first address of heap }
function getheapstart:pointer;
begin
   getheapstart:= theHeap;
end;

{ current length of heap }
function getheapsize:longint;
begin
   getheapsize:= theHeapSize ;
end;

{ function to allocate size bytes more for the program }
{ must return the first address of new data space or -1 if fail }
function Sbrk(size : longint):longint;
begin
  Sbrk:=-1;	//TODO: Allow heap increase.
end;

{$I heap.inc}

{****************************************************************************
                        Low level File Routines
       All these functions can set InOutRes on errors
 ****************************************************************************}

{ close a file from the handle value }
procedure do_close(handle : longint);
begin
  InOutRes:=1;
  if handle = UnusedHandle then exit;
  if FSClose(handle) = noErr then
    InOutRes:=0;	//TODO: Is this right ?	
end;

procedure do_erase(p : pchar);
begin
  InOutRes:=1;
end;

procedure do_rename(p1,p2 : pchar);
begin
  InOutRes:=1;
end;

function do_write(h,addr,len : longint) : longint;
begin
  InOutRes:=1;	
  if h = UnusedHandle then exit;
  if FSWrite(h, len, Mac_Ptr(addr)) = noErr then
    InOutRes:=0;	//TODO: Is this right ?	
  do_write:= len;
end;

function do_read(h,addr,len : longint) : longint;
begin
  InOutRes:=1;
  if h = UnusedHandle then exit;
  if FSread(h, len, Mac_Ptr(addr)) = noErr then
    InOutRes:=0;	//TODO: Is this right ?	
  do_read:= len;
end;

function do_filepos(handle : longint) : longint;
var
  pos: Longint;
begin
  InOutRes:=1;
  if handle = UnusedHandle then exit;
  if GetFPos(handle, pos) = noErr then
    InOutRes:=0;	//TODO: Is this right ?	
  do_filepos:= pos;
end;

procedure do_seek(handle,pos : longint);
begin
  InOutRes:=1;
  if handle = UnusedHandle then exit;
  if SetFPos(handle, fsFromStart, pos) = noErr then
    InOutRes:=0;	//TODO: Is this right ?	
end;

function do_seekend(handle:longint):longint;
begin
  InOutRes:=1;
  if handle = UnusedHandle then exit;
  if SetFPos(handle, fsFromLEOF, 0) = noErr then
    InOutRes:=0;	//TODO: Is this right ?	
end;

function do_filesize(handle : longint) : longint;
var
  pos: Longint;
begin
  InOutRes:=1;
  if handle = UnusedHandle then exit;
  if GetEOF(handle, pos) = noErr then
    InOutRes:=0;	//TODO: Is this right ?
  do_filesize:= pos;
end;

{ truncate at a given position }
procedure do_truncate (handle,pos:longint);
begin
  InOutRes:=1;
  do_seek(handle,pos);	//TODO: Is this needed (Does the user anticipate the filemarker is at the end?) 
  if SetEOF(handle, pos) = noErr then
    InOutRes:=0;	//TODO: Is this right ?
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

procedure do_open(var f;p:pchar;flags:longint);
{
  filerec and textrec have both handle and mode as the first items so
  they could use the same routine for opening/creating.
  when (flags and $10)   the file will be append
  when (flags and $100)  the file will be truncate/rewritten
  when (flags and $1000) there is no check for close (needed for textfiles)
}

var
  spec: FSSpec;
  creator, fileType: OSType;
  scriptTag: ScriptCode;
  refNum: Integer;
  res: OSErr;

const
  fsCurPerm = 0;
  smSystemScript = -1;

begin
  InOutRes:=1;
  //creator:= $522A6368;	{'MPS ' -- MPW}
  //creator:= $74747874;	{'ttxt'}
  creator:= $522A6368;	{'R*ch' -- BBEdit}
  fileType:= $54455854;	{'TEXT'}

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
end;

function do_isdevice(handle:longint):boolean;
begin
  do_isdevice:=false;
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
begin
  InOutRes:=1;
end;

procedure rmdir(const s : string);[IOCheck];
begin
  InOutRes:=1;
end;

procedure chdir(const s : string);[IOCheck];
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

Begin
  if false then //To save it from the dead code stripper
    Debugger; //Included only to make it available for debugging 
 
{ To be set if this is a GUI or console application }
  IsConsole := TRUE;
  { To be set if this is a library and not a program  }
  IsLibrary := FALSE;
  StackBottom := SPtr - StackLength;
  ExitCode := 0;
{ Setup heap }
  theHeap:= NewPtr(theHeapSize);
  InitHeap;
{ Setup stdin, stdout and stderr }
(*  OpenStdIO(Input,fmInput,StdInputHandle);
  OpenStdIO(Output,fmOutput,StdOutputHandle);
  OpenStdIO(StdOut,fmOutput,StdOutputHandle);
  OpenStdIO(StdErr,fmOutput,StdErrorHandle);*)
{ Setup environment and arguments }
  Setup_Environment;
  Setup_Arguments;
{ Reset IO Error }
  InOutRes:=0;

{$endif}

End.


{
  $Log$
  Revision 1.5  2003-01-13 17:18:55  olle
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