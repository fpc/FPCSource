{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Florian Klaempfl and Pavel Ozerski
    member of the Free Pascal development team.

    FPC Pascal system unit for the Win32 API.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit syswin32;
interface

{$ifdef i386}
  {$define Set_i386_Exception_handler}
{$endif i386}

{ include system-independent routine headers }
{$I systemh.inc}

{ include heap support headers }
{$I heaph.inc}

const
{ Default filehandles }
   UnusedHandle    : longint = -1;
   StdInputHandle  : longint = 0;
   StdOutputHandle : longint = 0;
   StdErrorHandle  : longint = 0;

   FileNameCaseSensitive : boolean = true;

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
  HInstance,
  MainInstance,
  cmdshow     : longint;
  IsLibrary,IsMultiThreaded,IsConsole : boolean;
  DLLreason,DLLparam:longint;
  Win32StackTop : Dword;
{ Thread count for DLL }
const
  Thread_count : longint = 0;
type
  TDLL_Process_Entry_Hook = function (dllparam : longint) : longbool;
  TDLL_Entry_Hook = procedure (dllparam : longint);

const
  Dll_Process_Attach_Hook : TDLL_Process_Entry_Hook = nil;
  Dll_Process_Detach_Hook : TDLL_Entry_Hook = nil;
  Dll_Thread_Attach_Hook : TDLL_Entry_Hook = nil;
  Dll_Thread_Detach_Hook : TDLL_Entry_Hook = nil;

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

{$ASMMODE ATT}


   { misc. functions }
   function GetLastError : DWORD;
     external 'kernel32' name 'GetLastError';

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
        jae     .L__short_on_stack
        popl    %ebx
        popl    %eax
        leave
        ret     $4
.L__short_on_stack:
        { can be usefull for error recovery !! }
        popl    %ebx
        popl    %eax
  end['EAX','EBX'];
  HandleError(202);
end;
{$endif dummy}


function paramcount : longint;
begin
  paramcount := argc - 1;
end;

   { module functions }
   function GetModuleFileName(l1:longint;p:pointer;l2:longint):longint;
     external 'kernel32' name 'GetModuleFileNameA';
   function GetModuleHandle(p : pointer) : longint;
     external 'kernel32' name 'GetModuleHandleA';
   function GetCommandFile:pchar;forward;

function paramstr(l : longint) : string;
begin
  if (l>=0) and (l<argc) then
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

   { memory functions }
   function GlobalAlloc(mode,size:longint):longint;
     external 'kernel32' name 'GlobalAlloc';
   function GlobalLock(handle:longint):pointer;
     external 'kernel32' name 'GlobalLock';
{$ifdef SYSTEMDEBUG}
   function GlobalSize(h:longint):longint;
     external 'kernel32' name 'GlobalSize';
{$endif}

var
  heap : longint;external name 'HEAP';
  intern_heapsize : longint;external name 'HEAPSIZE';

function getheapstart:pointer;assembler;
asm
        leal    HEAP,%eax
end ['EAX'];


function getheapsize:longint;assembler;
asm
        movl    intern_HEAPSIZE,%eax
end ['EAX'];


function Sbrk(size : longint):longint;
var
  h,l : longint;
begin
  h:=GlobalAlloc(258,size);
  l:=longint(GlobalLock(h));
  if l=0 then
    l:=-1;
{$ifdef DUMPGROW}
  Writeln('new heap part at $',hexstr(l,8), ' size = ',GlobalSize(h));
{$endif}
  sbrk:=l;
end;

{ include standard heap management }
{$I heap.inc}


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
   function SetEndOfFile(h : longint) : longbool;
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

function do_isdevice(handle:longint):boolean;
begin
  do_isdevice:=(getfiletype(handle)=2);
end;


procedure do_close(h : longint);
begin
  if do_isdevice(h) then
   exit;
  CloseHandle(h);
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
  _result:longint;
begin
  if readfile(h,pointer(addr),len,_result,nil)=0 then
    Begin
      errno:=GetLastError;
      Errno2InoutRes;
    end;
  do_read:=_result;
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
  when (flags and $100)   the file will be append
  when (flags and $1000)  the file will be truncate/rewritten
  when (flags and $10000) there is no check for close (needed for textfiles)
}
Const
  file_Share_Read  = $00000001;
  file_Share_Write = $00000002;
  fmShareCompat    = $00000000;
  fmShareExclusive = $10;
  fmShareDenyWrite = $20;
  fmShareDenyRead  = $30;
  fmShareDenyNone  = $40;
Var
  shflags,
  oflags,cd : longint;
begin
  AllowSlash(p);
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
{ convert filesharing }
  shflags:=0;
  if ((filemode and fmshareExclusive) = fmshareExclusive) then
    { no sharing }
  else
    if (filemode = fmShareCompat) or ((filemode and fmshareDenyWrite) = fmshareDenyWrite) then
      shflags := file_Share_Read
  else
    if ((filemode and fmshareDenyRead) = fmshareDenyRead) then
      shflags := file_Share_Write
  else
    if ((filemode and fmshareDenyNone) = fmshareDenyNone) then
      shflags := file_Share_Read + file_Share_Write;
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
  if (flags and $1000)<>0 then
   cd:=CREATE_ALWAYS
{ or append ? }
  else
   if (flags and $100)<>0 then
    cd:=OPEN_ALWAYS;
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
  filerec(f).handle:=CreateFile(p,oflags,shflags,nil,cd,FILE_ATTRIBUTE_NORMAL,0);
{ append mode }
  if (flags and $100)<>0 then
   begin
     do_seekend(filerec(f).handle);
     filerec(f).mode:=fmoutput; {fool fmappend}
   end;
{ get errors }
  { handle -1 is returned sometimes !! (PM) }
  if (filerec(f).handle=0) or (filerec(f).handle=-1) then
    begin
      errno:=GetLastError;
      Errno2InoutRes;
    end;
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

procedure getdir(drivenr:byte;var dir:shortstring);
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
  if not FileNameCaseSensitive then
   dir:=upcase(dir);
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
  pc      : pchar;
  quote   : set of char;
  argsbuf : array[0..127] of pchar;
begin
  { create commandline, it starts with the executed filename which is argv[0] }
  { Win32 passes the command NOT via the args, but via getmodulefilename}
  count:=0;
  pc:=getcommandfile;
  Arglen:=0;
  repeat
    Inc(Arglen);
  until (pc[Arglen]=#0);
  getmem(argsbuf[count],arglen+1);
  move(pc^,argsbuf[count]^,arglen);
  { Now skip the first one }
  pc:=GetCommandLine;
  repeat
    { skip leading spaces }
    while pc^ in [' ',#9,#13] do
     inc(pc);
    case pc^ of
      #0 : break;
     '"' : begin
             quote:=['"'];
             inc(pc);
           end;
    '''' : begin
             quote:=[''''];
             inc(pc);
           end;
    else
     quote:=[' ',#9,#13];
    end;
  { scan until the end of the argument }
    argstart:=pc;
    while (pc^<>#0) and not(pc^ in quote) do
     inc(pc);
    { Don't copy the first one, it is already there.}
    If Count<>0 then
     begin
       { reserve some memory }
       arglen:=pc-argstart;
       getmem(argsbuf[count],arglen+1);
       move(argstart^,argsbuf[count]^,arglen);
       argsbuf[count][arglen]:=#0;
     end;
    { skip quote }
    if pc^ in quote then
     inc(pc);
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
{ Setup cmdline variable }
  cmdline:=GetCommandLine;
end;


{*****************************************************************************
                         System Dependent Exit code
*****************************************************************************}

  procedure install_exception_handlers;forward;
  procedure remove_exception_handlers;forward;
  procedure PascalMain;external name 'PASCALMAIN';
  procedure fpc_do_exit;external name 'FPC_DO_EXIT';
  Procedure ExitDLL(Exitcode : longint); forward;

Procedure system_exit;
begin
  { don't call ExitProcess inside
    the DLL exit code !!
    This crashes Win95 at least PM }
  if IsLibrary then
    ExitDLL(ExitCode);
  if not IsConsole then
   begin
     Close(stderr);
     Close(stdout);
     { what about Input and Output ?? PM }
   end;
  remove_exception_handlers;
  ExitProcess(ExitCode);
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


var
  { value of the stack segment
    to check if the call stack can be written on exceptions }
  _SS : longint;

procedure Exe_entry;[public, alias : '_FPC_EXE_Entry'];
  begin
     IsLibrary:=false;
     { install the handlers for exe only ?
       or should we install them for DLL also ? (PM) }
     install_exception_handlers;
     { This strange construction is needed to solve the _SS problem
       with a smartlinked syswin32 (PFV) }
     asm
        pushl %ebp
        xorl %ebp,%ebp
        movl %esp,%eax
        movl %eax,Win32StackTop
        movw %ss,%bp
        movl %ebp,_SS
        xorl %ebp,%ebp
        call PASCALMAIN
        popl %ebp
     end;
     { if we pass here there was no error ! }
     system_exit;
  end;

Const
  { DllEntryPoint  }
     DLL_PROCESS_ATTACH = 1;
     DLL_THREAD_ATTACH = 2;
     DLL_PROCESS_DETACH = 0;
     DLL_THREAD_DETACH = 3;
Var
     DLLBuf : Jmp_buf;
Const
     DLLExitOK : boolean = true;

function Dll_entry : longbool;[public, alias : '_FPC_DLL_Entry'];
var
  res : longbool;

  begin
     IsLibrary:=true;
     Dll_entry:=false;
     case DLLreason of
       DLL_PROCESS_ATTACH :
         begin
           If SetJmp(DLLBuf) = 0 then
             begin
               if assigned(Dll_Process_Attach_Hook) then
                 begin
                   res:=Dll_Process_Attach_Hook(DllParam);
                   if not res then
                     exit(false);
                 end;
               PASCALMAIN;
               Dll_entry:=true;
             end
           else
             Dll_entry:=DLLExitOK;
         end;
       DLL_THREAD_ATTACH :
         begin
           inc(Thread_count);
           if assigned(Dll_Thread_Attach_Hook) then
             Dll_Thread_Attach_Hook(DllParam);
           Dll_entry:=true; { return value is ignored }
         end;
       DLL_THREAD_DETACH :
         begin
           dec(Thread_count);
           if assigned(Dll_Thread_Detach_Hook) then
             Dll_Thread_Detach_Hook(DllParam);
           Dll_entry:=true; { return value is ignored }
         end;
       DLL_PROCESS_DETACH :
         begin
           Dll_entry:=true; { return value is ignored }
           If SetJmp(DLLBuf) = 0 then
             begin
               FPC_DO_EXIT;
             end;
           if assigned(Dll_Process_Detach_Hook) then
             Dll_Process_Detach_Hook(DllParam);
         end;
     end;
  end;

Procedure ExitDLL(Exitcode : longint);
begin
    DLLExitOK:=ExitCode=0;
    LongJmp(DLLBuf,1);
end;

{$ifdef Set_i386_Exception_handler}

const
     EXCEPTION_MAXIMUM_PARAMETERS = 15;
     EXCEPTION_ACCESS_VIOLATION = $c0000005;
     EXCEPTION_BREAKPOINT = $80000003;
     EXCEPTION_DATATYPE_MISALIGNMENT = $80000002;
     EXCEPTION_SINGLE_STEP = $80000004;
     EXCEPTION_ARRAY_BOUNDS_EXCEEDED = $c000008c;
     EXCEPTION_FLT_DENORMAL_OPERAND = $c000008d;
     EXCEPTION_FLT_DIVIDE_BY_ZERO = $c000008e;
     EXCEPTION_FLT_INEXACT_RESULT = $c000008f;
     EXCEPTION_FLT_INVALID_OPERATION = $c0000090;
     EXCEPTION_FLT_OVERFLOW = $c0000091;
     EXCEPTION_FLT_STACK_CHECK = $c0000092;
     EXCEPTION_FLT_UNDERFLOW = $c0000093;
     EXCEPTION_INT_DIVIDE_BY_ZERO = $c0000094;
     EXCEPTION_INT_OVERFLOW = $c0000095;
     EXCEPTION_INVALID_HANDLE = $c0000008;
     EXCEPTION_PRIV_INSTRUCTION = $c0000096;
     EXCEPTION_NONCONTINUABLE_EXCEPTION = $c0000025;
     EXCEPTION_NONCONTINUABLE = $1;
     EXCEPTION_STACK_OVERFLOW = $c00000fd;
     EXCEPTION_INVALID_DISPOSITION = $c0000026;
     ExceptionContinueExecution = 0;
     ExceptionContinueSearch = 1;
  type

     FLOATING_SAVE_AREA = record
          ControlWord : DWORD;
          StatusWord : DWORD;
          TagWord : DWORD;
          ErrorOffset : DWORD;
          ErrorSelector : DWORD;
          DataOffset : DWORD;
          DataSelector : DWORD;
          RegisterArea : array[0..79] of BYTE;
          Cr0NpxState : DWORD;
       end;
     _FLOATING_SAVE_AREA = FLOATING_SAVE_AREA;
     TFLOATINGSAVEAREA = FLOATING_SAVE_AREA;
     PFLOATINGSAVEAREA = ^FLOATING_SAVE_AREA;

     CONTEXT = record
          ContextFlags : DWORD;
          Dr0 : DWORD;
          Dr1 : DWORD;
          Dr2 : DWORD;
          Dr3 : DWORD;
          Dr6 : DWORD;
          Dr7 : DWORD;
          FloatSave : FLOATING_SAVE_AREA;
          SegGs : DWORD;
          SegFs : DWORD;
          SegEs : DWORD;
          SegDs : DWORD;
          Edi : DWORD;
          Esi : DWORD;
          Ebx : DWORD;
          Edx : DWORD;
          Ecx : DWORD;
          Eax : DWORD;
          Ebp : DWORD;
          Eip : DWORD;
          SegCs : DWORD;
          EFlags : DWORD;
          Esp : DWORD;
          SegSs : DWORD;
       end;
     LPCONTEXT = ^CONTEXT;
     _CONTEXT = CONTEXT;
     TCONTEXT = CONTEXT;
     PCONTEXT = ^CONTEXT;


type pexception_record = ^exception_record;
     EXCEPTION_RECORD  = record
       ExceptionCode   : longint;
       ExceptionFlags  : longint;
       ExceptionRecord : pexception_record;
       ExceptionAddress : pointer;
       NumberParameters : longint;
       ExceptionInformation : array[0..EXCEPTION_MAXIMUM_PARAMETERS-1] of pointer;
     end;

     PEXCEPTION_POINTERS = ^EXCEPTION_POINTERS;
     EXCEPTION_POINTERS = record
       ExceptionRecord   : PEXCEPTION_RECORD ;
       ContextRecord     : PCONTEXT ;
     end;

     { type of functions that should be used for exception handling }
     LPTOP_LEVEL_EXCEPTION_FILTER = function(excep :PEXCEPTION_POINTERS) : longint;

     function SetUnhandledExceptionFilter(lpTopLevelExceptionFilter : LPTOP_LEVEL_EXCEPTION_FILTER)
       : LPTOP_LEVEL_EXCEPTION_FILTER;
       external 'kernel32' name 'SetUnhandledExceptionFilter';

  function syswin32_i386_exception_handler(excep :PEXCEPTION_POINTERS) : longint;
    var frame : longint;
    begin
       { default : unhandled !}
       if excep^.ContextRecord^.SegSs=_SS then
         frame:=excep^.ContextRecord^.Ebp
       else
         frame:=0;
       syswin32_i386_exception_handler:=ExceptionContinueSearch;
       case excep^.ExceptionRecord^.ExceptionCode of
         EXCEPTION_ACCESS_VIOLATION :
           HandleErrorFrame(216,frame);
         { EXCEPTION_BREAKPOINT = $80000003;
         EXCEPTION_DATATYPE_MISALIGNMENT = $80000002;
         EXCEPTION_SINGLE_STEP = $80000004; }
         EXCEPTION_ARRAY_BOUNDS_EXCEEDED :
           HandleErrorFrame(201,frame);
         { EXCEPTION_FLT_DENORMAL_OPERAND = $c000008d; }
         EXCEPTION_FLT_DIVIDE_BY_ZERO :
           HandleErrorFrame(200,frame);
         {EXCEPTION_FLT_INEXACT_RESULT = $c000008f;
         EXCEPTION_FLT_INVALID_OPERATION = $c0000090;}
         EXCEPTION_FLT_OVERFLOW :
           HandleErrorFrame(205,frame);
         EXCEPTION_FLT_STACK_CHECK :
           HandleErrorFrame(207,frame);
         { EXCEPTION_FLT_UNDERFLOW :
           HandleErrorFrame(206,frame); should be accepted as zero !! }
         EXCEPTION_INT_DIVIDE_BY_ZERO :
           HandleErrorFrame(200,frame);
         EXCEPTION_INT_OVERFLOW :
           HandleErrorFrame(215,frame);
         {EXCEPTION_INVALID_HANDLE = $c0000008;
         EXCEPTION_PRIV_INSTRUCTION = $c0000096;
         EXCEPTION_NONCONTINUABLE_EXCEPTION = $c0000025;
         EXCEPTION_NONCONTINUABLE = $1;}
         EXCEPTION_STACK_OVERFLOW :
           HandleErrorFrame(202,frame);
         {EXCEPTION_INVALID_DISPOSITION = $c0000026;}
         end;
    end;


  procedure install_exception_handlers;
    begin
      SetUnhandledExceptionFilter(@syswin32_i386_exception_handler);
    end;

  procedure remove_exception_handlers;
    begin
      SetUnhandledExceptionFilter(nil);
    end;

{$else not i386 (Processor specific !!)}
  procedure install_exception_handlers;
    begin
    end;

  procedure remove_exception_handlers;
    begin
    end;

{$endif Set_i386_Exception_handler}


{****************************************************************************
                    Error Message writing using messageboxes
****************************************************************************}

function MessageBox(w1:longint;l1,l2:pointer;w2:longint):longint;
   external 'user32' name 'MessageBoxA';

const
  ErrorBufferLength = 1024;
var
  ErrorBuf : array[0..ErrorBufferLength] of char;
  ErrorLen : longint;

Function ErrorWrite(Var F: TextRec): Integer;
{
  An error message should always end with #13#10#13#10
}
var
  p : pchar;
  i : longint;
Begin
  if F.BufPos>0 then
   begin
     if F.BufPos+ErrorLen>ErrorBufferLength then
       i:=ErrorBufferLength-ErrorLen
     else
       i:=F.BufPos;
     Move(F.BufPtr^,ErrorBuf[ErrorLen],i);
     inc(ErrorLen,i);
     ErrorBuf[ErrorLen]:=#0;
   end;
  if ErrorLen>3 then
   begin
     p:=@ErrorBuf[ErrorLen];
     for i:=1 to 4 do
      begin
        dec(p);
        if not(p^ in [#10,#13]) then
         break;
      end;
   end;
   if ErrorLen=ErrorBufferLength then
     i:=4;
   if (i=4) then
    begin
      MessageBox(0,@ErrorBuf,pchar('Error'),0);
      ErrorLen:=0;
    end;
  F.BufPos:=0;
  ErrorWrite:=0;
End;


Function ErrorClose(Var F: TextRec): Integer;
begin
  if ErrorLen>0 then
   begin
     MessageBox(0,@ErrorBuf,pchar('Error'),0);
     ErrorLen:=0;
   end;
  ErrorLen:=0;
  ErrorClose:=0;
end;


Function ErrorOpen(Var F: TextRec): Integer;
Begin
  TextRec(F).InOutFunc:=@ErrorWrite;
  TextRec(F).FlushFunc:=@ErrorWrite;
  TextRec(F).CloseFunc:=@ErrorClose;
  ErrorOpen:=0;
End;


procedure AssignError(Var T: Text);
begin
  Assign(T,'');
  TextRec(T).OpenFunc:=@ErrorOpen;
  Rewrite(T);
end;



const
   Exe_entry_code : pointer = @Exe_entry;
   Dll_entry_code : pointer = @Dll_entry;

begin
{ get some helpful informations }
  GetStartupInfo(@startupinfo);
{ some misc Win32 stuff }
  hprevinst:=0;
  if not IsLibrary then
    HInstance:=getmodulehandle(GetCommandFile);
  MainInstance:=HInstance;
  { No idea how to know this issue !! }
  IsMultithreaded:=false;
  cmdshow:=startupinfo.wshowwindow;
{ to test stack depth }
  loweststack:=maxlongint;
{ real test stack depth        }
{   stacklimit := setupstack;  }
{ Setup heap }
  InitHeap;
  InitExceptions;
{ Setup stdin, stdout and stderr, for GUI apps redirect stderr,stdout to be
  displayed in and messagebox }
  StdInputHandle:=longint(GetStdHandle(STD_INPUT_HANDLE));
  StdOutputHandle:=longint(GetStdHandle(STD_OUTPUT_HANDLE));
  StdErrorHandle:=longint(GetStdHandle(STD_ERROR_HANDLE));
  if not IsConsole then
   begin
     AssignError(stderr);
     AssignError(stdout);
     Assign(Output,'');
     Assign(Input,'');
   end
  else
   begin
     OpenStdIO(Input,fmInput,StdInputHandle);
     OpenStdIO(Output,fmOutput,StdOutputHandle);
     OpenStdIO(StdOut,fmOutput,StdOutputHandle);
     OpenStdIO(StdErr,fmOutput,StdErrorHandle);
   end;
{ Arguments }
  setup_arguments;
{ Reset IO Error }
  InOutRes:=0;
{ Reset internal error variable }
  errno:=0;
end.

{
  $Log$
  Revision 1.61  2000-03-10 09:21:11  pierre
    * ExitDLL fixed : uses now SetJmp LongJmp
    * System_exit unloads the exception hanlder before leaving

  Revision 1.60  2000/02/09 16:59:34  peter
    * truncated log

  Revision 1.59  2000/02/09 12:24:39  peter
    * halt moved to system.inc

  Revision 1.58  2000/01/20 23:38:02  peter
    * support fm_inout as stdoutput for assign(f,'');rewrite(f,1); becuase
      rewrite opens always with filemode 2

  Revision 1.57  2000/01/18 09:03:04  pierre
    * DLL crash fixed : ExitProcess can not be called in DLL system_exit
      Problem : Halt or RunError code inside DLL will return to caller !!
    * Changed the "if h<4 then" into "if do_isdevice(h) then " in do_close
      to avoid closing of standard files

  Revision 1.56  2000/01/16 23:05:03  peter
    * fixed typo

  Revision 1.55  2000/01/16 22:25:38  peter
    * check handle for file closing

  Revision 1.54  2000/01/07 16:41:52  daniel
    * copyright 2000

  Revision 1.53  2000/01/07 16:32:34  daniel
    * copyright 2000 added

  Revision 1.52  2000/01/06 23:40:36  peter
    * fixed exitprocess call, it's now in system_exit and uses exitcode

  Revision 1.51  1999/12/01 22:57:31  peter
    * cmdline support

  Revision 1.50  1999/11/20 00:16:44  pierre
   + DLL Hooks for the four callings added

  Revision 1.49  1999/11/18 22:19:57  pierre
   * bug fix for web bug703 and 704

  Revision 1.48  1999/11/09 22:34:00  pierre
    * Check ErrorBuf at exit
    + Win32StackTop

  Revision 1.47  1999/10/26 12:25:51  peter
    * report stderr,stdout to message box for errors
    * close input,output when GUI app is made

  Revision 1.46  1999/10/22 14:47:19  peter
    * allocate an extra byte for argv[0]

  Revision 1.45  1999/10/03 19:39:05  peter
    * fixed argv[0] length

  Revision 1.44  1999/09/10 15:40:35  peter
    * fixed do_open flags to be > $100, becuase filemode can be upto 255

}