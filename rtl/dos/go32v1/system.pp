{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1993,97 by Florian Klaempfl,
    member of the Free Pascal development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{ system unit for go32v1 }
{$define DOS}
unit system;

{$I os.inc}

  interface

    { die betriebssystemunabhangigen Deklarationen einfuegen: }

    {$I systemh.inc}

    {$I heaph.inc}

const
  UnusedHandle=$ffff;
  StdInputHandle=0;
  StdOutputHandle=1;
  StdErrorHandle=2;

type
{$PACKRECORDS 1}
       t_stub_info   = record
       magic         : array[0..15] of char;
       size          : longint;
       minstack      : longint;
       memory_handle : longint;
       initial_size  : longint;
       minkeep       : word;
       ds_selector   : word;
       ds_segment    : word;
       psp_selector  : word;
       cs_selector   : word;
       env_size      : word;
       basename      : array[0..7] of char;
       argv0         : array [0..15] of char;
       dpmi_server   : array [0..15] of char;
       end;
       p_stub_info   = ^t_stub_info;

       t_go32_info_block = record
       size_of_this_structure_in_bytes : longint; {offset 0}
       linear_address_of_primary_screen : longint; {offset 4}
       linear_address_of_secondary_screen : longint; {offset 8}
       linear_address_of_transfer_buffer : longint; {offset 12}
       size_of_transfer_buffer : longint; {offset 16}
       pid : longint; {offset 20}
       master_interrupt_controller_base : byte; {offset 24}
       slave_interrupt_controller_base : byte; {offset 25}
       selector_for_linear_memory : word; {offset 26}
       linear_address_of_stub_info_structure : longint; {offset 28}
       linear_address_of_original_psp : longint; {offset 32}
       run_mode : word; {offset 36}
       run_mode_info : word; {offset 38}
       end;
{$PACKRECORDS NORMAL}

var
  stub_info       : p_stub_info;
  go32_info_block : t_go32_info_block;
  environ         : ppchar;

  implementation

    { include system independent routines }

    {$I system.inc}

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

    procedure halt(errnum : byte);

      begin
         do_exit;
         flush(stderr);
         asm
            movl $0x4c00,%eax
            movb 8(%ebp),%al
            int $0x21
         end;
      end;

    function paramcount : longint;

      begin
         asm
            movl _argc,%eax
            decl %eax
            leave
            ret
         end ['EAX'];
      end;

    function paramstr(l : longint) : string;

      function args : pointer;

        begin
           asm
              movl _args,%eax
              leave
              ret
           end ['EAX'];
        end;

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

{$I heap.inc}


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
   asm
      movl 8(%ebp),%ebx
      movb $0x3e,%ah
      pushl %ebp
      intl $0x21
      popl %ebp
   end;
end;


procedure do_erase(p : pchar);
begin
  AllowSlash(p);
  asm
     movl 8(%ebp),%edx
     movb $0x41,%ah
     pushl %ebp
     int $0x21
     popl %ebp
     jnc .LERASE1
     movw %ax,U_SYSTEM_INOUTRES;
  .LERASE1:
  end;
end;


procedure do_rename(p1,p2 : pchar);
begin
  AllowSlash(p1);
  AllowSlash(p2);
  asm
     movl 8(%ebp),%edx
     movl 12(%ebp),%edi
     movb $0x56,%ah
     pushl %ebp
     int $0x21
     popl %ebp
     jnc .LRENAME1
     movw %ax,U_SYSTEM_INOUTRES;
  .LRENAME1:
  end;
end;


function do_write(h,addr,len : longint) : longint;
begin
  asm
     movl 16(%ebp),%ecx
     movl 12(%ebp),%edx
     movl 8(%ebp),%ebx
     movb $0x40,%ah
     int $0x21
     jnc .LDOSWRITE1
     movw %ax,U_SYSTEM_INOUTRES;
  .LDOSWRITE1:
     movl %eax,-4(%ebp)
  end;
end;


function do_read(h,addr,len : longint) : longint;
begin
  asm
     movl 16(%ebp),%ecx
     movl 12(%ebp),%edx
     movl 8(%ebp),%ebx
     movb $0x3f,%ah
     int $0x21
     jnc .LDOSREAD1
     movw %ax,U_SYSTEM_INOUTRES;
     xorl %eax,%eax
  .LDOSREAD1:
     leave
     ret $12
  end;
end;


function do_filepos(handle : longint) : longint;
begin
  asm
     movb $0x42,%ah
     movb $0x1,%al
     movl 8(%ebp),%ebx
     xorl %ecx,%ecx
     xorl %edx,%edx
     pushl %ebp
     int $0x21
     popl %ebp
     jnc .LDOSFILEPOS1
     movw %ax,U_SYSTEM_INOUTRES;
     xorl %eax,%eax
     jmp .LDOSFILEPOS2
  .LDOSFILEPOS1:
     shll $16,%edx
     movzwl %ax,%eax
     orl %edx,%eax
  .LDOSFILEPOS2:
     leave
     ret $4
  end;
end;


procedure do_seek(handle,pos : longint);
begin
  asm
     movl $0x4200,%eax
     movl 8(%ebp),%ebx
     movl 12(%ebp),%edx
     movl %edx,%ecx
     shrl $16,%ecx
     pushl %ebp
     int $0x21
     popl %ebp
     jnc .LDOSSEEK1
     movw %ax,U_SYSTEM_INOUTRES;
  .LDOSSEEK1:
     leave
     ret $8
  end;
end;


function do_seekend(handle : longint) : longint;
begin
  asm
     movl $0x4202,%eax
     movl 8(%ebp),%ebx
     xorl %ecx,%ecx
     xorl %edx,%edx
     pushl %ebp
     int $0x21
     popl %ebp
     jnc .Lset_at_end1
     movw %ax,U_SYSTEM_INOUTRES;
     xorl %eax,%eax
     jmp .Lset_at_end2
  .Lset_at_end1:
     shll $16,%edx
     movzwl %ax,%eax
     orl %edx,%eax
  .Lset_at_end2:
     leave
     ret $4
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


procedure do_truncate(handle,pos : longint);
begin
   asm
      movl $0x4200,%eax
      movl 8(%ebp),%ebx
      movl 12(%ebp),%edx
      movl %edx,%ecx
      shrl $16,%ecx
      pushl %ebp
      int $0x21
      popl %ebp
      jc .LTruncate1
      movl 8(%ebp),%ebx
      movl 12(%ebp),%edx
      movl %ebp,%edx
      xorl %ecx,%ecx
      movb $0x40,%ah
      int $0x21
      jnc .LTruncate2
   .LTruncate1:
      movw %ax,U_SYSTEM_INOUTRES;
   .LTruncate2:
      leave
      ret $8
   end;
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
   oflags : longint;
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
        inoutres:=102; {not assigned}
        exit;
      end;
     end;
   end;
{ reset file handle }
  filerec(f).handle:=UnusedHandle;
  oflags:=$8404;
{ convert filemode to filerec modes }
  case (flags and 3) of
   0 : begin
         filerec(f).mode:=fminput;
         oflags:=$8001;
       end;
   1 : filerec(f).mode:=fmoutput;
   2 : filerec(f).mode:=fminout;
  end;
  if (flags and $100)<>0 then
   begin
     filerec(f).mode:=fmoutput;
     oflags:=$8302;
   end
  else
   if (flags and $10)<>0 then
    begin
      filerec(f).mode:=fmoutput;
      oflags:=$8404;
    end;
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
   asm
      movl $0xff02,%ax
      movl -4(%ebp),%ecx
      movl 12(%ebp),%ebx
      int $0x21
      jnc .LOPEN1
      movw %ax,U_SYSTEM_INOUTRES;
      movw $0xffff,%ax
   .LOPEN1:
      movl 8(%ebp),%edx
      movw %ax,(%edx)
   end;
  if (flags and $10)<>0 then
   do_seekend(filerec(f).handle);
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
  asm
    leal buffer,%edx
    movb 8(%ebp),%ah
    int  $0x21
    jnc  .LDOS_DIRS1
    movw %ax,U_SYSTEM_INOUTRES;
  .LDOS_DIRS1:
  end;
end;


procedure mkdir(const s : string);
begin
  DosDir($39,s);
end;


procedure rmdir(const s : string);
begin
  DosDir($3a,s);
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

procedure OpenStdIO(var f:text;mode:word;hdl:longint);
begin
  Assign(f,'');
  TextRec(f).Handle:=hdl;
  TextRec(f).Mode:=mode;
  TextRec(f).InOutFunc:=@FileInOutFunc;
  TextRec(f).FlushFunc:=@FileInOutFunc;
  TextRec(f).Closefunc:=@fileclosefunc;
end;


Begin
{ Initialize ExitProc }
  ExitProc:=Nil;
{ to test stack depth }
  loweststack:=maxlongint;
{ Setup heap }
  InitHeap;
{ Setup stdin, stdout and stderr }
  OpenStdIO(Input,fmInput,StdInputHandle);
  OpenStdIO(Output,fmOutput,StdOutputHandle);
  OpenStdIO(StdErr,fmOutput,StdErrorHandle);
{ Reset IO Error }
  InOutRes:=0;
End.

{
  $Log$
  Revision 1.2  1998-03-26 12:21:02  peter
    * makefile works again
    * environ is now defined in system.pp (like go32v2)

  Revision 1.1.1.1  1998/03/25 11:18:41  root
  * Restored version

  Revision 1.9  1998/02/14 01:41:35  peter
    * fixed unusedhandle bug which was -1

  Revision 1.8  1998/01/26 11:57:03  michael
  + Added log at the end



  Working file: rtl/dos/go32v1/system.pp
  description:
  ----------------------------
  revision 1.7
  date: 1998/01/25 21:53:22;  author: peter;  state: Exp;  lines: +12 -8
    + Universal Handles support for StdIn/StdOut/StdErr
    * Updated layout of sysamiga.pas
  ----------------------------
  revision 1.6
  date: 1998/01/16 23:10:50;  author: florian;  state: Exp;  lines: +2 -2
    + some tobject stuff
  ----------------------------
  revision 1.5
  date: 1998/01/11 02:47:31;  author: michael;  state: Exp;  lines: +384 -507
  * Changed files to use the new filestructure in /inc directory.
    (By Peter Vreman)
  ----------------------------
  revision 1.4
  date: 1998/01/07 00:05:04;  author: michael;  state: Exp;  lines: +189 -184
  + Final adjustments  for a uniform file handling interface.
     (From Peter Vreman)
  ----------------------------
  revision 1.3
  date: 1998/01/05 16:51:04;  author: michael;  state: Exp;  lines: +18 -46
  + Moved init of heap to heap.inc: INITheap() (From Peter Vreman)
  ----------------------------
  revision 1.2
  date: 1997/12/01 12:24:06;  author: michael;  state: Exp;  lines: +12 -3
  + added copyright reference in header.
  ----------------------------
  revision 1.1
  date: 1997/11/27 08:33:53;  author: michael;  state: Exp;
  Initial revision
  ----------------------------
  revision 1.1.1.1
  date: 1997/11/27 08:33:53;  author: michael;  state: Exp;  lines: +0 -0
  FPC RTL CVS start
  =============================================================================
}
