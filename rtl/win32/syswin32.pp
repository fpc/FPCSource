{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1993-98 by Florian Klaempfl and Pavel Ozerski
    member of the Free Pascal development team.

    FPC Pascal system unit for the Win32 API.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$S-}
unit syswin32;

{$I os.inc}

{.$DEFINE WINHEAP}   { Use windows heap manager, if not set use FPC heap }


interface

{ include system-independent routine headers }

{$I systemh.inc}

{$ifndef WinHeap}
  { include heap support headers }
  {$I heaph.inc}
{$endif}

const
{ Default filehandles }
   UnusedHandle    : longint = -1;
   StdInputHandle  : longint = 0;
   StdOutputHandle : longint = 0;
   StdErrorHandle  : longint = 0;

type
  TStartupInfo=packed record
    cb : longint;
    lpReserved : Pointer;
    lpDesktop : Pointer;
    lpTitle : Pointer;
    dwX : longint;
    dwY : longint;
    dwXSize : longint;
    dwYSize : longint;
    dwXCountChars : longint;
    dwYCountChars : longint;
    dwFillAttribute : longint;
    dwFlags : longint;
    wShowWindow : Word;
    cbReserved2 : Word;
    lpReserved2 : Pointer;
    hStdInput : longint;
    hStdOutput : longint;
    hStdError : longint;
  end;

var
{ C compatible arguments }
  argc  : longint;
  argv  : ppchar;
{ Win32 Info }
  startupinfo : tstartupinfo;
  hprevinst,
  hinstance,
  cmdshow     : longint;

{$ifdef WinHeap}
var
  heaperror  : pointer;

function HeapSize:longint;
{$endif}

implementation

{ include system independent routines }
{$I system.inc}

{ some declarations for Win32 API calls }
{$I win32.inc}


CONST
  { These constants are used for conversion of error codes }
  { from win32 i/o errors to tp i/o errors                 }
  { errors 1 to 18 are the same as in Turbo Pascal         }
  { DO NOT MODIFY UNLESS YOU KNOW EXACTLY WHAT YOU ARE DOING! }

{  The media is write protected.                   }
    ERROR_WRITE_PROTECT       =      19;
{  The system cannot find the device specified.    }
    ERROR_BAD_UNIT            =      20;
{  The device is not ready.                        }
    ERROR_NOT_READY           =      21;
{  The device does not recognize the command.      }
    ERROR_BAD_COMMAND         =      22;
{  Data error (cyclic redundancy check)            }
    ERROR_CRC                 =      23;
{  The program issued a command but the            }
{  command length is incorrect.                    }
    ERROR_BAD_LENGTH           =     24;
{  The drive cannot locate a specific              }
{  area or track on the disk.                      }
    ERROR_SEEK                 =     25;
{  The specified disk or diskette cannot be accessed. }
    ERROR_NOT_DOS_DISK         =     26;
{  The drive cannot find the sector requested.     }
    ERROR_SECTOR_NOT_FOUND      =    27;
{  The printer is out of paper.                    }
    ERROR_OUT_OF_PAPER          =    28;
{  The system cannot write to the specified device. }
    ERROR_WRITE_FAULT           =    29;
{  The system cannot read from the specified device. }
    ERROR_READ_FAULT            =    30;
{  A device attached to the system is not functioning.}
    ERROR_GEN_FAILURE           =    31;
{  The process cannot access the file because         }
{  it is being used by another process.               }
    ERROR_SHARING_VIOLATION      =   32;

var
    errno : longint;

   { misc. functions }
   function GetLastError : DWORD;
     external 'kernel32' name 'GetLastError';
   function MessageBox(w1:longint;l1,l2:pointer;w2:longint):longint;
     external 'user32' name 'MessageBoxA';

   { time and date functions }
   function GetTickCount : longint;
     external 'kernel32' name 'GetTickCount';

   { process functions }
   procedure ExitProcess(uExitCode : UINT);
     external 'kernel32' name 'ExitProcess';


   Procedure Errno2InOutRes;
   Begin
     { DO NOT MODIFY UNLESS YOU KNOW EXACTLY WHAT YOU ARE DOING }
     if (errno >= ERROR_WRITE_PROTECT) and (errno <= ERROR_GEN_FAILURE) THEN
       BEGIN
          { This is the offset to the Win32 to add to directly map  }
          { to the DOS/TP compatible error codes when in this range }
          InOutRes := word(errno)+131;
       END
     else
     { This case is special }
     if errno=ERROR_SHARING_VIOLATION THEN
       BEGIN
         InOutRes :=5;
       END
     else
     { other error codes can directly be mapped }
         InOutRes := Word(errno);
     errno:=0;
   end;


{$ifdef dummy}
procedure int_stackcheck(stack_size:longint);[public,alias: 'STACKCHECK'];
{
  called when trying to get local stack if the compiler directive $S
  is set this function must preserve esi !!!! because esi is set by
  the calling proc for methods it must preserve all registers !!

  With a 2048 byte safe area used to write to StdIo without crossing
  the stack boundary

}
begin
  asm
        pushl   %eax
        pushl   %ebx
        movl    stack_size,%ebx
        addl    $2048,%ebx
        movl    %esp,%eax
        subl    %ebx,%eax
        movl    stacklimit,%ebx
        cmpl    %eax,%ebx
        jae     __short_on_stack
        popl    %ebx
        popl    %eax
        leave
        ret     $4
__short_on_stack:
        { can be usefull for error recovery !! }
        popl    %ebx
        popl    %eax
  end['EAX','EBX'];
  HandleError(202);
end;
{$endif dummy}


procedure halt(errnum : byte);
begin
  do_exit;
  ExitProcess(errnum);
end;


function paramcount : longint;
begin
  paramcount := argc - 1;
end;


function paramstr(l : longint) : string;
begin
  if (l>=0) and (l+1<=argc) then
   paramstr:=strpas(argv[l])
  else
   paramstr:='';
end;


procedure randomize;
begin
  randseed:=GetTickCount;
end;


{*****************************************************************************
                              Heap Management
*****************************************************************************}

{$ifdef WinHeap}

  {$i winheap.inc}

{$else}

   { memory functions }
   function GlobalAlloc(mode,size:longint):longint;
     external 'kernel32' name 'GlobalAlloc';
   function GlobalReAlloc(mode,size:longint):longint;
     external 'kernel32' name 'GlobalReAlloc';
   function GlobalHandle(p:pointer):longint;
     external 'kernel32' name 'GlobalHandle';
   function GlobalLock(handle:longint):pointer;
     external 'kernel32' name 'GlobalLock';
   function GlobalUnlock(h:longint):longint;
     external 'kernel32' name 'GlobalUnlock';
   function GlobalFree(h:longint):longint;
     external 'kernel32' name 'GlobalFree';
   function GlobalSize(h:longint):longint;
     external 'kernel32' name 'GlobalSize';
   procedure GlobalMemoryStatus(p:pointer);
     external 'kernel32' name 'GlobalMemoryStatus';
   function LocalAlloc(uFlags : UINT;uBytes :UINT) : HLOCAL;
     external 'kernel32' name 'LocalAlloc';
   function LocalFree(hMem:HLOCAL):HLOCAL;
     external 'kernel32' name 'LocalFree';

function Sbrk(size : longint):longint;
var
  h,l : longint;
begin
  h:=GlobalAlloc(258,size);
  l:=longint(GlobalLock(h));
  Writeln('new heap part at $',hexstr(l,8), ' size = ',GlobalSize(h));
  sbrk:=l;
end;

{ include standard heap management }
{$I heap.inc}

{$endif WinHeap}


{*****************************************************************************
                          Low Level File Routines
*****************************************************************************}

   function WriteFile(fh:longint;buf:pointer;len:longint;var loaded:longint;
     overlap:pointer):longint;
     external 'kernel32' name 'WriteFile';
   function ReadFile(fh:longint;buf:pointer;len:longint;var loaded:longint;
     overlap:pointer):longint;
     external 'kernel32' name 'ReadFile';
   function CloseHandle(h : longint) : longint;
     external 'kernel32' name 'CloseHandle';
   function DeleteFile(p : pchar) : longint;
     external 'kernel32' name 'DeleteFileA';
   function MoveFile(old,_new : pchar) : longint;
     external 'kernel32' name 'MoveFileA';
   function SetFilePointer(l1,l2 : longint;l3 : pointer;l4 : longint) : longint;
     external 'kernel32' name 'SetFilePointer';
   function GetFileSize(h:longint;p:pointer) : longint;
     external 'kernel32' name 'GetFileSize';
   function CreateFile(name : pointer;access,sharing : longint;
     security : pointer;how,attr,template : longint) : longint;
     external 'kernel32' name 'CreateFileA';
   function SetEndOfFile(h : longint) : boolean;
     external 'kernel32' name 'SetEndOfFile';
   function GetFileType(Handle:DWORD):DWord;
     external 'kernel32' name 'GetFileType';


procedure AllowSlash(p:pchar);
var
   i : longint;
begin
{ allow slash as backslash }
   for i:=0 to strlen(p) do
     if p[i]='/' then p[i]:='\';
end;


procedure do_close(h : longint);
begin
   closehandle(h);
end;


procedure do_erase(p : pchar);
begin
   AllowSlash(p);
   if DeleteFile(p)=0 then
    Begin
      errno:=GetLastError;
      Errno2InoutRes;
    end;
end;


procedure do_rename(p1,p2 : pchar);
begin
  AllowSlash(p1);
  AllowSlash(p2);
  if MoveFile(p1,p2)=0 then
   Begin
      errno:=GetLastError;
      Errno2InoutRes;
   end;
end;


function do_write(h,addr,len : longint) : longint;
var
   size:longint;
begin
   if writefile(h,pointer(addr),len,size,nil)=0 then
    Begin
      errno:=GetLastError;
      Errno2InoutRes;
    end;
   do_write:=size;
end;


function do_read(h,addr,len : longint) : longint;
var
  result:longint;
begin
  if readfile(h,pointer(addr),len,result,nil)=0 then
    Begin
      errno:=GetLastError;
      Errno2InoutRes;
    end;
  do_read:=result;
end;


function do_filepos(handle : longint) : longint;
var
  l:longint;
begin
  l:=SetFilePointer(handle,0,nil,FILE_CURRENT);
  if l=-1 then
   begin
    l:=0;
    errno:=GetLastError;
    Errno2InoutRes;
   end;
  do_filepos:=l;
end;


procedure do_seek(handle,pos : longint);
begin
  if SetFilePointer(handle,pos,nil,FILE_BEGIN)=-1 then
   Begin
    errno:=GetLastError;
    Errno2InoutRes;
   end;
end;


function do_seekend(handle:longint):longint;
begin
  do_seekend:=SetFilePointer(handle,0,nil,FILE_END);
  if do_seekend=-1 then
    begin
      errno:=GetLastError;
      Errno2InoutRes;
    end;
end;


function do_filesize(handle : longint) : longint;
var
  aktfilepos : longint;
begin
  aktfilepos:=do_filepos(handle);
  do_filesize:=do_seekend(handle);
  do_seek(handle,aktfilepos);
end;


procedure do_truncate (handle,pos:longint);
begin
   do_seek(handle,pos);
   if not(SetEndOfFile(handle)) then
    begin
      errno:=GetLastError;
      Errno2InoutRes;
    end;
end;


procedure do_open(var f;p : pchar;flags:longint);

{
  filerec and textrec have both handle and mode as the first items so
  they could use the same routine for opening/creating.
  when (flags and $10)   the file will be append
  when (flags and $100)  the file will be truncate/rewritten
  when (flags and $1000) there is no check for close (needed for textfiles)
}

var
  oflags,cd : longint;
begin
  AllowSlash(p);
{ close first if opened }
  if ((flags and $1000)=0) then
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
{ convert filemode to filerec modes }
  case (flags and 3) of
   0 : begin
         filerec(f).mode:=fminput;
         oflags:=GENERIC_READ;
       end;
   1 : begin
         filerec(f).mode:=fmoutput;
         oflags:=GENERIC_WRITE;
       end;
   2 : begin
         filerec(f).mode:=fminout;
         oflags:=GENERIC_WRITE or GENERIC_READ;
       end;
  end;
{ standard is opening and existing file }
  cd:=OPEN_EXISTING;
{ create it ? }
  if (flags and $100)<>0 then
   cd:=CREATE_ALWAYS
{ or append ? }
  else
   if (flags and $10)<>0 then
    cd:=OPEN_ALWAYS;
{ empty name is special }
  if p[0]=#0 then
   begin
     case filerec(f).mode of
       fminput : filerec(f).handle:=StdInputHandle;
      fmappend,
      fmoutput : begin
                   filerec(f).handle:=StdOutputHandle;
                   filerec(f).mode:=fmoutput; {fool fmappend}
                 end;
     end;
     exit;
   end;
  filerec(f).handle:=CreateFile(p,oflags,0,nil,cd,FILE_ATTRIBUTE_NORMAL,0);
{ append mode }
  if (flags and $10)<>0 then
   begin
     do_seekend(filerec(f).handle);
     filerec(f).mode:=fmoutput; {fool fmappend}
   end;
{ get errors }
  if filerec(f).handle=0 then
    begin
      errno:=GetLastError;
      Errno2InoutRes;
    end;
end;


function do_isdevice(handle:longint):boolean;
begin
  do_isdevice:=(getfiletype(handle)=2);
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

{$DEFINE EOF_CTRLZ}

{$i text.inc}

{*****************************************************************************
                           Directory Handling
*****************************************************************************}

   function CreateDirectory(name : pointer;sec : pointer) : longint;
     external 'kernel32' name 'CreateDirectoryA';
   function RemoveDirectory(name:pointer):longint;
     external 'kernel32' name 'RemoveDirectoryA';
   function SetCurrentDirectory(name : pointer) : longint;
     external 'kernel32' name 'SetCurrentDirectoryA';
   function GetCurrentDirectory(bufsize : longint;name : pchar) : longint;
     external 'kernel32' name 'GetCurrentDirectoryA';

type
 TDirFnType=function(name:pointer):word;

procedure dirfn(afunc : TDirFnType;const s:string);
var
  buffer : array[0..255] of char;
begin
  move(s[1],buffer,length(s));
  buffer[length(s)]:=#0;
  AllowSlash(pchar(@buffer));
  if aFunc(@buffer)=0 then
    begin
      errno:=GetLastError;
      Errno2InoutRes;
    end;
end;

function CreateDirectoryTrunc(name:pointer):word;
 begin
  CreateDirectoryTrunc:=CreateDirectory(name,nil);
 end;

procedure mkdir(const s:string);[IOCHECK];
 begin
  If InOutRes <> 0 then exit;
  dirfn(TDirFnType(@CreateDirectoryTrunc),s);
 end;

procedure rmdir(const s:string);[IOCHECK];
 begin
  If InOutRes <> 0 then exit;
  dirfn(TDirFnType(@RemoveDirectory),s);
 end;

procedure chdir(const s:string);[IOCHECK];
 begin
  If InOutRes <> 0 then exit;
  dirfn(TDirFnType(@SetCurrentDirectory),s);
 end;

procedure getdir(drivenr:byte;var dir:string);
 const
  Drive:array[0..3]of char=(#0,':',#0,#0);
 var
  defaultdrive:boolean;
  DirBuf,SaveBuf:array[0..259] of Char;
 begin
  defaultdrive:=drivenr=0;
  if not defaultdrive then
   begin
    byte(Drive[0]):=Drivenr+64;
    GetCurrentDirectory(SizeOf(SaveBuf),SaveBuf);
    SetCurrentDirectory(@Drive);
   end;
  GetCurrentDirectory(SizeOf(DirBuf),DirBuf);
  if not defaultdrive then
   SetCurrentDirectory(@SaveBuf);
  dir:=strpas(DirBuf);
 end;

{*****************************************************************************
                         SystemUnit Initialization
*****************************************************************************}

   { Startup }
   procedure GetStartupInfo(p : pointer);
     external 'kernel32' name 'GetStartupInfoA';
   function GetStdHandle(nStdHandle:DWORD):THANDLE;
     external 'kernel32' name 'GetStdHandle';

   { command line/enviroment functions }
   function GetCommandLine : pchar;
     external 'kernel32' name 'GetCommandLineA';

   { module functions }
   function GetModuleFileName(l1:longint;p:pointer;l2:longint):longint;
     external 'kernel32' name 'GetModuleFileNameA';
   function GetModuleHandle(p : pointer) : longint;
     external 'kernel32' name 'GetModuleHandleA';

var
  ModuleName : array[0..255] of char;
function GetCommandFile:pchar;
begin
  GetModuleFileName(0,@ModuleName,255);
  GetCommandFile:=@ModuleName;
end;


procedure setup_arguments;
var
  arglen,
  count   : longint;
  argstart,
  cmdline : pchar;
  quote   : set of char;
  argsbuf : array[0..127] of pchar;
begin
{ create commandline, it starts with the executed filename which is argv[0] }
  cmdline:=GetCommandLine;
  count:=0;
  repeat
  { skip leading spaces }
    while cmdline^ in [' ',#9,#13] do
     inc(longint(cmdline));
    case cmdline^ of
      #0 : break;
     '"' : begin
             quote:=['"'];
             inc(longint(cmdline));
           end;
    '''' : begin
             quote:=[''''];
             inc(longint(cmdline));
           end;
    else
     quote:=[' ',#9,#13];
    end;
  { scan until the end of the argument }
    argstart:=cmdline;
    while (cmdline^<>#0) and not(cmdline^ in quote) do
     inc(longint(cmdline));
  { reserve some memory }
    arglen:=cmdline-argstart;
    getmem(argsbuf[count],arglen+1);
    move(argstart^,argsbuf[count]^,arglen);
    argsbuf[count][arglen]:=#0;
  { skip quote }
    if cmdline^ in quote then
     inc(longint(cmdline));
    inc(count);
  until false;
{ create argc }
  argc:=count;
{ create an nil entry }
  argsbuf[count]:=nil;
  inc(count);
{ create the argv }
  getmem(argv,count shl 2);
  move(argsbuf,argv^,count shl 2);
end;


{$ASMMODE DIRECT}
procedure Entry;[public,alias: '_mainCRTStartup'];
begin
   { call to the pascal main }
   asm
     call PASCALMAIN
   end;
   { that's all folks }
   ExitProcess(0);
end;

{$ifdef dummy}
Function SetUpStack : longint;
{ This routine does the following :                            }
{  returns the value of the initial SP - __stklen              }
begin
  asm
    pushl %ebx
    pushl %eax
    movl  __stklen,%ebx
    movl  %esp,%eax
    subl  %ebx,%eax
    movl  %eax,__RESULT
    popl  %eax
    popl  %ebx
  end;
end;
{$endif}
{$ASMMODE ATT}




begin
{ get some helpful informations }
  GetStartupInfo(@startupinfo);
{ some misc Win32 stuff }
  hprevinst:=0;
  hinstance:=getmodulehandle(GetCommandFile);
  cmdshow:=startupinfo.wshowwindow;
{ to test stack depth }
  loweststack:=maxlongint;
{ real test stack depth        }
{   stacklimit := setupstack;  }
{ Setup heap }
{$ifndef WinHeap}
  InitHeap;
{$endif WinHeap}
{ Setup stdin, stdout and stderr }
  StdInputHandle:=longint(GetStdHandle(STD_INPUT_HANDLE));
  StdOutputHandle:=longint(GetStdHandle(STD_OUTPUT_HANDLE));
  StdErrorHandle:=longint(GetStdHandle(STD_ERROR_HANDLE));
  OpenStdIO(Input,fmInput,StdInputHandle);
  OpenStdIO(Output,fmOutput,StdOutputHandle);
  OpenStdIO(StdErr,fmOutput,StdErrorHandle);
{ Arguments }
  setup_arguments;
{ Reset IO Error }
  InOutRes:=0;
{ Reset internal error variable }
  errno := 0;
end.

{
  $Log$
  Revision 1.16  1998-08-24 14:45:22  pierre
    * sbrk was wrong
      heap growing now works for win32

  Revision 1.15  1998/08/21 10:10:16  peter
    * winheap turned off by default

  Revision 1.14  1998/07/30 13:27:19  michael
  + Added support for errorproc. Changed runerror to HandleError

  Revision 1.13  1998/07/13 21:19:15  florian
    * some problems with ansi string support fixed

  Revision 1.12  1998/07/07 12:37:28  carl
    * correct mapping of error codes for TP compatibility
    + implemented stack checking in ifdef dummy

  Revision 1.11  1998/07/02 12:33:18  carl
    * IOCheck/InOutRes check for mkdir,rmdir and chdir like in TP

  Revision 1.10  1998/07/01 15:30:02  peter
    * better readln/writeln

  Revision 1.9  1998/06/10 10:39:17  peter
    * working w32 rtl

  Revision 1.8  1998/06/08 23:07:47  peter
    * dos interface is now 100% compatible
    * fixed call PASCALMAIN which must be direct asm

  Revision 1.7  1998/05/06 12:36:51  michael
  + Removed log from before restored version.

  Revision 1.6  1998/04/27 18:29:09  florian
    + do_open implemented, the file-I/O should be now complete

  Revision 1.5  1998/04/27 13:58:21  florian
    + paramstr/paramcount implemented

  Revision 1.4  1998/04/26 22:37:22  florian
    * some small extensions

  Revision 1.3  1998/04/26 21:49:57  florian
    + more stuff added (??dir procedures etc.)
}
