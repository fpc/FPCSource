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

interface

{ include system-independent routine headers }

{$I systemh.inc}

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
  startupinfo : tstartupinfo;
  hprevinst,
  hinstance,
  cmdshow     : longint;
  heaperror   : pointer;

implementation

{ include system independent routines }

{$I system.inc}

{ some declarations for Win32 API calls }
{$I win32.inc}

type
  plongint = ^longint;

   { misc. functions }
   function GetLastError : DWORD;
     external 'kernel32' name 'GetLastError';
   function MessageBox(w1:longint;l1,l2:pointer;w2:longint):longint;
     external 'user32' name 'MessageBoxA';

   { command line/enviroment functions }
   function GetCommandLine : LPTSTR;
     external 'kernel32' name 'GetCommandLineA';
   { time and date functions }
   function GetTickCount : longint;
     external 'kernel32' name 'GetTickCount';
   { process functions }
   procedure ExitProcess(uExitCode : UINT);
     external 'kernel32' name 'ExitProcess';


{$ifdef dummy}
procedure int_stackcheck(stack_size:longint);[public,alias: 'STACKCHECK'];
{
  called when trying to get local stack if the compiler directive $S
  is set this function must preserve esi !!!! because esi is set by
  the calling proc for methods it must preserve all registers !!
}
begin
  asm
        pushl   %eax
        pushl   %ebx
        movl    stack_size,%ebx
        movl    %esp,%eax
        subl    %ebx,%eax
{$ifdef SYSTEMDEBUG}
        movl    U_SYSTEM_LOWESTSTACK,%ebx
        cmpl    %eax,%ebx
        jb      _is_not_lowest
        movl    %eax,U_SYSTEM_LOWESTSTACK
_is_not_lowest:
{$endif SYSTEMDEBUG}
        movl    __stkbottom,%ebx
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
  RunError(202);
end;
{$endif dummy}


procedure halt(errnum : byte);
begin
  do_exit;
  flush(stderr);
  ExitProcess(errnum);
end;


function paramcount : longint;
var
  count   : longint;
  cmdline : pchar;
  quote   : set of char;
begin
  cmdline:=GetCommandLine;
  count:=0;
  while true do
   begin
     { skip leading spaces }
     while cmdline^ in [' ',#9] do
       cmdline:=cmdline+1;
     if cmdline^='"' then
       begin
          quote:=['"'];
          cmdline:=cmdline+1;
       end
     else
       quote:=[' ',#9];
     if cmdline^=#0 then
       break;
     inc(count);
     while (cmdline^<>#0) and not(cmdline^ in quote) do
       cmdline:=cmdline+1;
     { skip quote }
     if cmdline^ in quote then
       cmdline:=cmdline+1;
   end;
  paramcount:=count-1;
end;


function paramstr(l : longint) : string;
var
  s       : string;
  count   : longint;
  cmdline : pchar;
  quote   : set of char;
begin
  s:='';
  if (l>=0) and (l<=paramcount) then
    begin
       cmdline:=GetCommandLine;
       count:=0;
       while true do
         begin
            { skip leading spaces }
            while cmdline^ in [' ',#9] do
              cmdline:=cmdline+1;
            if cmdline^='"' then
              begin
                 quote:=['"'];
                 cmdline:=cmdline+1;
              end
            else
              quote:=[' ',#9];
            if cmdline^=#0 then
              break;
            if count=l then
              begin
                 while (cmdline^<>#0) and not(cmdline^ in quote) do
                   begin
                      s:=s+cmdline^;
                      cmdline:=cmdline+1;
                   end;
                 break;
              end
            else
              begin
                 while (cmdline^<>#0) and not(cmdline^ in quote) do
                   cmdline:=cmdline+1;
              end;
            { skip quote }
            if cmdline^ in quote then
              cmdline:=cmdline+1;
            inc(count);
         end;

    end;
  paramstr:=s;
end;


procedure randomize;
begin
  randseed:=GetTickCount;
end;


{*****************************************************************************
                              Heap Management
*****************************************************************************}

{ Include Windows Heap manager }
{$I winheap.inc}

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
      inoutres:=GetLastError;
end;


procedure do_rename(p1,p2 : pchar);
begin
  AllowSlash(p1);
  AllowSlash(p2);
  if MoveFile(p1,p2)=0 then
   inoutres:=GetLastError;
end;


function do_write(h,addr,len : longint) : longint;
var
   size:longint;
begin
   if writefile(h,pointer(addr),len,size,nil)=0 then
     inoutres:=GetLastError;
   do_write:=size;
end;


function do_read(h,addr,len : longint) : longint;
var
  result:longint;
begin
  if readfile(h,pointer(addr),len,result,nil)=0 then
   inoutres:=GetLastError;
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
    inoutres:=GetLastError;
   end;
  do_filepos:=l;
end;


procedure do_seek(handle,pos : longint);
begin
  if SetFilePointer(handle,pos,nil,FILE_BEGIN)=-1 then
   inoutres:=GetLastError;
end;


function do_seekend(handle:longint):longint;
begin
  do_seekend:=SetFilePointer(handle,0,nil,FILE_END);
  if do_seekend=-1 then
    begin
       inoutres:=GetLastError;
       do_seekend:=0;
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
     inoutres:=GetLastError;
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
   inoutres:=GetLastError;
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
   inoutres:=GetLastError;
end;

function CreateDirectoryTrunc(name:pointer):word;
 begin
  CreateDirectoryTrunc:=CreateDirectory(name,nil);
 end;

procedure mkdir(const s:string);[IOCHECK];
 begin
  dirfn(TDirFnType(@CreateDirectoryTrunc),s);
 end;

procedure rmdir(const s:string);[IOCHECK];
 begin
  dirfn(TDirFnType(@RemoveDirectory),s);
 end;

procedure chdir(const s:string);[IOCHECK];
 begin
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

   { module functions }
   function GetModuleFileName(l1:longint;p:pointer;l2:longint):longint;
     external 'kernel32' name 'GetModuleFileNameA';
   function GetModuleHandle(p : pointer) : longint;
     external 'kernel32' name 'GetModuleHandleA';


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

{$ASMMODE ATT}


procedure OpenStdIO(var f:text;mode:word;hdl:longint);
begin
  Assign(f,'');
  TextRec(f).Handle:=hdl;
  TextRec(f).Mode:=mode;
  TextRec(f).InOutFunc:=@FileInOutFunc;
  TextRec(f).FlushFunc:=@FileInOutFunc;
  TextRec(f).Closefunc:=@fileclosefunc;
end;


var
  s : string;
begin
{ get some helpful informations }
  GetStartupInfo(@startupinfo);
{ Initialize ExitProc }
  ExitProc:=Nil;
{ to test stack depth }
  loweststack:=maxlongint;
{ Setup heap }
{!!!  InitHeap; }
{ Setup stdin, stdout and stderr }
  StdInputHandle:=longint(GetStdHandle(STD_INPUT_HANDLE));
  StdOutputHandle:=longint(GetStdHandle(STD_OUTPUT_HANDLE));
  StdErrorHandle:=longint(GetStdHandle(STD_ERROR_HANDLE));
  OpenStdIO(Input,fmInput,StdInputHandle);
  OpenStdIO(Output,fmOutput,StdOutputHandle);
  OpenStdIO(StdErr,fmOutput,StdErrorHandle);
{ Reset IO Error }
  InOutRes:=0;
{ some misc Win32 stuff }
  hprevinst:=0;
  getmodulefilename(0,@s,256);
  hinstance:=getmodulehandle(@s);
  cmdshow:=startupinfo.wshowwindow;
end.

{
  $Log$
  Revision 1.9  1998-06-10 10:39:17  peter
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
