{
    $Id$
    This file is part of the Free Pascal run time library.
    FPC Pascal system unit for the Win32 API.
    Copyright (c) 1993,98 by Florian Klaempfl and Pavel Ozerski
    member of the Free Pascal development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit syswin32;

{$I os.inc}

  interface

    {$I systemh.inc}

    var
       hprevinst,hinstance,cmdshow : longint;
       heaperror : pointer;

    { $I heaph.inc}

const
  UnusedHandle    : longint = -1;
  StdInputHandle  : longint = 0;
  StdOutputHandle : longint = 0;
  StdErrorHandle  : longint = 0;

  implementation

    { some declarations for Win32 API calls }
    {$I Win32.inc}
    {$I system.inc}

    type
       plongint = ^longint;

{$ifdef dummy}
{$S-}
    procedure st1(stack_size : longint);[public,alias: 'STACKCHECK'];

      begin
         { called when trying to get local stack }
         { if the compiler directive $S is set   }
         { this function must preserve esi !!!!  }
         { because esi is set by the calling     }
         { proc for methods                      }
         { it must preserve all registers !!     }

         asm
            pushl %eax
            pushl %ebx
            movl stack_size,%ebx
            movl %esp,%eax
            subl %ebx,%eax
{$ifdef SYSTEMDEBUG}
            movl U_SYSTEM_LOWESTSTACK,%ebx
            cmpl %eax,%ebx
            jb   _is_not_lowest
            movl %eax,U_SYSTEM_LOWESTSTACK
            _is_not_lowest:
{$endif SYSTEMDEBUG}
            movl __stkbottom,%ebx
            cmpl %eax,%ebx
            jae  __short_on_stack
            popl %ebx
            popl %eax
            leave
            ret  $4
            __short_on_stack:
            { can be usefull for error recovery !! }
            popl %ebx
            popl %eax
         end['EAX','EBX'];
         RunError(202);
         { this needs a local variable }
         { so the function called itself !! }
         { Writeln('low in stack ');
         RunError(202);             }
      end;
{$endif dummy}
    var
       argc : longint;
       args : pointer;
       arg_buffer : pointer;

    procedure halt(errnum : byte);

      begin
         do_exit;
         flush(stderr);
         LocalFree(arg_buffer);
         ExitProcess(errnum);
      end;

    function paramcount : longint;

      begin
         paramcount:=argc-1;
      end;

    function paramstr(l : longint) : string;

      var
         p : ^pchar;

      begin
         if (l>=0) and (l<=paramcount) then
           begin
              p:=args;
              paramstr:=strpas(p[l]);
           end
         else paramstr:='';
      end;

    procedure randomize;

      var
         hl : longint;

      begin
         asm
            movb $0x2c,%ah
            int $0x21
            movw %cx,-4(%ebp)
            movw %dx,-2(%ebp)
         end;
         randseed:=hl;
      end;

{ use standard heap management }
{ sbrk function of go32v1 }
  function Sbrk(size : longint) : longint;

    begin
       asm
         movl size,%ebx
         movl $0x4a01,%eax
         int  $0x21
         movl %eax,__RESULT
       end;
    end;

{$i winheap.inc}
{ $I heap.inc}

{****************************************************************************
                          Low Level File Routines
 ****************************************************************************}

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
  l:=SetFilePointer(handle,0,nil,1);
  if l=-1 then
   begin
    l:=0;
    inoutres:=GetLastError;
   end;
  do_filepos:=l;
 end;

procedure do_seek(handle,pos : longint);
begin
  if SetFilePointer(handle,pos,nil,0)=-1 then
   inoutres:=GetLastError;
end;

function do_seekend(handle:longint):longint;
begin
   {!!!!!!!!!!!!}
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
  {!!!!!!!!!!!!}
end;

procedure do_open(var f;p:pchar;flags:longint);
{
  filerec and textrec have both handle and mode as the first items so
  they could use the same routine for opening/creating.
  when (flags and $10)   the file will be append
  when (flags and $100)  the file will be truncate/rewritten
  when (flags and $1000) there is no check for close (needed for textfiles)
}
begin
  AllowSlash(p);
  {!!!!!!!!!!!!}
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

procedure DosDir(func:byte;const s:string);
var
  buffer : array[0..255] of char;
begin
  move(s[1],buffer,length(s));
  buffer[length(s)]:=#0;
  AllowSlash(pchar(@buffer));
  {!!!!!!!!!!!!}
end;


procedure mkdir(const s : string);
begin
  {!!!!!!!!!!!!}
end;


procedure rmdir(const s : string);
begin
  {!!!!!!!!!!!!}
end;


procedure chdir(const s : string);
begin
  DosDir($3b,s);
end;

{ thanks to Michael Van Canneyt <michael@tfdec1.fys.kuleuven.ac.be>, }
{ who writes this code                                               }
{ her is a problem if the getdir is called with a pathstr var in dos.pp }
procedure getdir(drivenr : byte;var dir : string);
var
  temp : array[0..255] of char;
  sof  : pchar;
  i    : byte;
begin
  sof:=pchar(@dir[4]);
  { dir[1..3] will contain '[drivenr]:\', but is not }
  { supplied by DOS, so we let dos string start at   }
  { dir[4]                                           }
  { Get dir from drivenr : 0=default, 1=A etc... }
  asm
    movb drivenr,%dl
    movl sof,%esi
    mov  $0x47,%ah
    int  $0x21
  end;
{ Now Dir should be filled with directory in ASCIIZ, }
{ starting from dir[4]                               }
  dir[0]:=#3;
  dir[2]:=':';
  dir[3]:='\';
  i:=4;
{ conversation to Pascal string }
  while (dir[i]<>#0) do
   begin
   { convert path name to DOS }
     if dir[i]='/' then
      dir[i]:='\';
     dir[0]:=chr(i);
     inc(i);
   end;
{ upcase the string (FPKPascal function) }
  dir:=upcase(dir);
  if drivenr<>0 then   { Drive was supplied. We know it }
   dir[1]:=chr(65+drivenr-1)
  else
   begin
   { We need to get the current drive from DOS function 19H  }
   { because the drive was the default, which can be unknown }
     asm
       movb $0x19,%ah
       int $0x21
       addb $65,%al
       movb %al,i
     end;
     dir[1]:=chr(i);
   end;
end;


{*****************************************************************************
                         SystemUnit Initialization
*****************************************************************************}

procedure Entry;[public,alias: '_mainCRTStartup'];
{
  the following procedure is written with the help of an article of
  the german computer magazine c't (3/97 p. 372)
}
var
   cmdline : pchar;
begin
   cmdline:=GetCommandLine;
   argc:=0;
   while true do
    begin
      break;
    end;
   arg_buffer:=LocalAlloc(LMEM_FIXED,8);
   { call to the pascal main }
   asm
     call PASCALMAIN
   end;
   { that's all folks }
   LocalFree(arg_buffer);
   ExitProcess(0);
end;


procedure OpenStdIO(var f:text;mode:word;hdl:longint);
begin
  Assign(f,'');
  TextRec(f).Handle:=hdl;
  TextRec(f).Mode:=mode;
  TextRec(f).InOutFunc:=@FileInOutFunc;
  TextRec(f).FlushFunc:=@FileInOutFunc;
  TextRec(f).Closefunc:=@fileclosefunc;
end;

{$PACKRECORDS 1}
var
 s : string;
 StartupInfo : record
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

{$PACKRECORDS NORMAL}

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
  Revision 1.2  1998-03-27 00:50:22  peter
    * small fixes so it compiles

  Revision 1.1.1.1  1998/03/25 11:18:47  root
  * Restored version

  Revision 1.13  1998/03/05 22:37:36  florian
    * some stuff added

  Revision 1.12  1998/01/26 12:02:28  michael
  + Added log at the end

  Working file: rtl/win32/syswin32.pp
  description:
  ----------------------------
  revision 1.11
  date: 1998/01/25 21:53:37;  author: peter;  state: Exp;  lines: +415 -408
    + Universal Handles support for StdIn/StdOut/StdErr
    * Updated layout of sysamiga.pas
  ----------------------------
  revision 1.10
  date: 1998/01/16 22:22:59;  author: michael;  state: Exp;  lines: +408 -544
  * Synchronised with other system files (Peter Vreman)
  ----------------------------
  revision 1.9
  date: 1998/01/07 00:04:55;  author: michael;  state: Exp;  lines: +84 -124
  + Final adjustments  for a uniform file handling interface.
     (From Peter Vreman)
  ----------------------------
  revision 1.8
  date: 1998/01/05 16:51:26;  author: michael;  state: Exp;  lines: +18 -52
  + Moved init of heap to heap.inc: INITheap() (From Peter Vreman)
  ----------------------------
  revision 1.7
  date: 1997/12/19 11:47:08;  author: florian;  state: Exp;  lines: +2 -2
  *** empty log message ***
  ----------------------------
  revision 1.6
  date: 1997/12/01 12:42:52;  author: michael;  state: Exp;  lines: +12 -5
  + added copyright reference in header.
  ----------------------------
  revision 1.5
  date: 1997/11/27 23:04:10;  author: florian;  state: Exp;  lines: +1 -13
  Old log entries to log-file added
  ----------------------------
  revision 1.4
  date: 1997/11/27 23:01:09;  author: florian;  state: Exp;  lines: +8 -3
  This was a test
  ----------------------------
  revision 1.3
  date: 1997/11/27 22:49:06;  author: florian;  state: Exp;  lines: +12 -9
  - CPU.PP added
  - some bugs in DOS fixed (espsecially for go32v1)
  - the win32 system unit is now compilable
  ----------------------------
  revision 1.2
  date: 1997/11/27 17:40:12;  author: florian;  state: Exp;  lines: +8 -1
  Added log and id to syswin32.pp for test purposes
  ----------------------------
  revision 1.1
  date: 1997/11/27 10:15:33;  author: florian;  state: Exp;
  Win32 files added (they are untested)
  =============================================================================
}
