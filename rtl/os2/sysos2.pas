{****************************************************************************

                     Free Pascal -- OS/2 runtime library

                  Copyright (c) 1999-2000 by Florian Klaempfl
                   Copyright (c) 1999-2000 by Daniel Mantione

 Free Pascal is distributed under the GNU Public License v2. So is this unit.
 The GNU Public License requires you to distribute the source code of this
 unit with any product that uses it. We grant you an exception to this, and
 that is, when you compile a program with the Free Pascal Compiler, you do not
 need to ship source code with that program, AS LONG AS YOU ARE USING
 UNMODIFIED CODE! If you modify this code, you MUST change the next line:

 <This an official, unmodified Free Pascal source code file.>

 Send us your modified files, we can work together if you want!

 Free Pascal is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 Library GNU General Public License for more details.

 You should have received a copy of the Library GNU General Public License
 along with Free Pascal; see the file COPYING.LIB.  If not, write to
 the Free Software Foundation, 59 Temple Place - Suite 330,
 Boston, MA 02111-1307, USA.

****************************************************************************}

unit sysos2;

{Changelog:

    People:

        DM - Daniel Mantione

    Date:           Description of change:              Changed by:

     -              First released version 0.1.         DM

Coding style:

    My coding style is a bit unusual for Pascal. Nevertheless I friendly ask
    you to try to make your changes not look all to different. In general,
    set your IDE to use tab characters, optimal fill on and a tabsize of 4.}

interface

{Link the startup code.}
{$l prt1.oo2}

{$I SYSTEMH.INC}
{$I heaph.inc}

type    Tos=(osDOS,osOS2,osDPMI);

var     os_mode:Tos;
        first_meg:pointer;

type    Psysthreadib=^Tsysthreadib;
        Pthreadinfoblock=^Tthreadinfoblock;
        Pprocessinfoblock=^Tprocessinfoblock;

        Tbytearray=array[0..$ffff] of byte;
        Pbytearray=^Tbytearray;

        Tsysthreadib=record
            tid,
            priority,
            version:longint;
            MCcount,
            MCforceflag:word;
        end;

        Tthreadinfoblock=record
            pexchain,
            stack,
            stacklimit:pointer;
            tib2:Psysthreadib;
            version,
            ordinal:longint;
        end;

        Tprocessinfoblock=record
            pid,
            parentpid,
            hmte:longint;
            cmd,
            env:Pbytearray;
            flstatus,
            ttype:longint;
        end;

const   UnusedHandle=$ffff;
        StdInputHandle=0;
        StdOutputHandle=1;
        StdErrorHandle=2;

        FileNameCaseSensitive : boolean = false;

var
{ C-compatible arguments and environment }
  argc  : longint;external name '_argc';
  argv  : ppchar;external name '_argv';
  envp  : ppchar;external name '_environ';


implementation

{$I SYSTEM.INC}

procedure DosGetInfoBlocks (var Atib: PThreadInfoBlock;
                            var Apib: PProcessInfoBlock); cdecl;
                            external 'DOSCALLS' index 312;

function DosSetRelMaxFH (var ReqCount, CurMaxFH: longint): longint; cdecl;
external 'DOSCALLS' index 382;

{This is the correct way to call external assembler procedures.}
procedure syscall; external name '___SYSCALL';

{***************************************************************************

                Runtime error checking related routines.

***************************************************************************}

{$S-}
procedure st1(stack_size:longint);[public,alias: 'STACKCHECK'];

begin
    { called when trying to get local stack }
    { if the compiler directive $S is set   }
    {$ASMMODE DIRECT}
    asm
        movl stack_size,%ebx
        movl %esp,%eax
        subl %ebx,%eax
{$ifdef SYSTEMDEBUG}
        movl U_SYSOS2_LOWESTSTACK,%ebx
        cmpl %eax,%ebx
        jb   Lis_not_lowest
        movl %eax,U_SYSOS2_LOWESTSTACK
    Lis_not_lowest:
{$endif SYSTEMDEBUG}
        cmpb $2,U_SYSOS2_OS_MODE
        jne Lrunning_in_dos
        movl U_SYSOS2_STACKBOTTOM,%ebx
        jmp Lrunning_in_os2
    Lrunning_in_dos:
        movl __heap_brk,%ebx
    Lrunning_in_os2:
        cmpl %eax,%ebx
        jae  Lshort_on_stack
        leave
        ret  $4
    Lshort_on_stack:
    end ['EAX','EBX'];
    {$ASMMODE ATT}
    { this needs a local variable }
    { so the function called itself !! }
    { Writeln('low in stack ');}
    HandleError(202);
end;
{no stack check in system }

{****************************************************************************

                    Miscellaneous related routines.

****************************************************************************}

procedure system_exit; assembler;
asm
    movb $0x4c,%ah
    movb exitcode,%al
    call syscall
end;


{$asmmode direct}
function paramcount:longint;assembler;

asm
    movl _argc,%eax
    decl %eax
end ['EAX'];

function paramstr(l:longint):string;

    function args:pointer;assembler;

    asm
        movl _argv,%eax
    end ['EAX'];

var p:^Pchar;

begin
     if (l>=0) and (l<=paramcount) then
        begin
            p:=args;
            paramstr:=strpas(p[l]);
        end
     else paramstr:='';
end;

{$asmmode att}

procedure randomize;

var hl:longint;

begin
    asm
        movb $0x2c,%ah
        call syscall
        movw %cx,-4(%ebp)
        movw %dx,-2(%ebp)
    end;
    randseed:=hl;
end;

{****************************************************************************

                    Heap management releated routines.

****************************************************************************}


{ this function allows to extend the heap by calling
syscall $7f00 resizes the brk area}

function sbrk(size:longint):longint; assembler;
asm
    movl size,%edx
    movw $0x7f00,%ax
    call syscall
end;

{$ASMMODE direct}
function getheapstart:pointer;assembler;

asm
    movl __heap_base,%eax
end ['EAX'];

function getheapsize:longint;assembler;
asm
    movl    HEAPSIZE,%eax
end ['EAX'];
{$ASMMODE ATT}

{$i heap.inc}

{****************************************************************************

                          Low Level File Routines

****************************************************************************}

procedure allowslash(p:Pchar);

{Allow slash as backslash.}

var i:longint;

begin
    for i:=0 to strlen(p) do
        if p[i]='/' then p[i]:='\';
end;

procedure do_close(h:longint);

begin
{ Only three standard handles under real OS/2 }
  if (h > 4) or
     (os_MODE = osOS2) and (h > 2) then
   begin
     asm
        movb $0x3e,%ah
        mov h,%ebx
        call syscall
     end;
   end;
end;

procedure do_erase(p:Pchar);

begin
    allowslash(p);
    asm
        movl P,%edx
        movb $0x41,%ah
        call syscall
        jnc .LERASE1
        movw %ax,inoutres;
    .LERASE1:
    end;
end;

procedure do_rename(p1,p2:Pchar);

begin
    allowslash(p1);
    allowslash(p2);
    asm
        movl P1, %edx
        movl P2, %edi
        movb $0x56,%ah
        call syscall
        jnc .LRENAME1
        movw %ax,inoutres;
    .LRENAME1:
    end;
end;

function do_read(h,addr,len:longint):longint; assembler;
asm
    movl len,%ecx
    movl addr,%edx
    movl h,%ebx
    movb $0x3f,%ah
    call syscall
    jnc .LDOSREAD1
    movw %ax,inoutres;
    xorl %eax,%eax
.LDOSREAD1:
end;

function do_write(h,addr,len:longint) : longint; assembler;
asm
    movl len,%ecx
    movl addr,%edx
    movl h,%ebx
    movb $0x40,%ah
    call syscall
    jnc .LDOSWRITE1
    movw %ax,inoutres;
.LDOSWRITE1:
end;

function do_filepos(handle:longint): longint; assembler;
asm
    movw $0x4201,%ax
    movl handle,%ebx
    xorl %edx,%edx
    call syscall
    jnc .LDOSFILEPOS
    movw %ax,inoutres;
    xorl %eax,%eax
.LDOSFILEPOS:
end;

procedure do_seek(handle,pos:longint); assembler;
asm
    movw $0x4200,%ax
    movl handle,%ebx
    movl pos,%edx
    call syscall
    jnc .LDOSSEEK1
    movw %ax,inoutres;
.LDOSSEEK1:
end;

function do_seekend(handle:longint):longint; assembler;
asm
    movw $0x4202,%ax
    movl handle,%ebx
    xorl %edx,%edx
    call syscall
    jnc .Lset_at_end1
    movw %ax,inoutres;
    xorl %eax,%eax
.Lset_at_end1:
end;

function do_filesize(handle:longint):longint;

var aktfilepos:longint;

begin
    aktfilepos:=do_filepos(handle);
    do_filesize:=do_seekend(handle);
    do_seek(handle,aktfilepos);
end;

procedure do_truncate(handle,pos:longint); assembler;
asm
(* DOS function 40h isn't safe for this according to EMX documentation
        movl $0x4200,%eax
        movl 8(%ebp),%ebx
        movl 12(%ebp),%edx
        call syscall
        jc .LTruncate1
        movl 8(%ebp),%ebx
        movl 12(%ebp),%edx
        movl %ebp,%edx
        xorl %ecx,%ecx
        movb $0x40,%ah
        call syscall
*)
    movl $0x7F25,%eax
    movl Handle,%ebx
    movl Pos,%edx
    call syscall
    inc %eax
    movl %ecx, %eax
    jnz .LTruncate1
(* File position is undefined after truncation, move to the end. *)
    movl $0x4202,%eax
    movl Handle,%ebx
    movl $0,%edx
    call syscall
    jnc .LTruncate2
.LTruncate1:
    movw %ax,inoutres;
.LTruncate2:
end;

const
    FileHandleCount: longint = 20;

function Increase_File_Handle_Count: boolean;
var Err: word;
    L1, L2: longint;
begin
    if os_mode = osOS2 then
        begin
            L1 := 10;
            if DosSetRelMaxFH (L1, L2) <> 0 then
                Increase_File_Handle_Count := false
            else
                if L2 > FileHandleCount then
                    begin
                        FileHandleCount := L2;
                        Increase_File_Handle_Count := true;
                    end
                else
                    Increase_File_Handle_Count := false;
        end
    else
        begin
            Inc (FileHandleCount, 10);
            Err := 0;
            asm
                movl $0x6700, %eax
                movl FileHandleCount, %ebx
                call syscall
                jnc .LIncFHandles
                movw %ax, Err
.LIncFHandles:
            end;
            if Err <> 0 then
                begin
                    Increase_File_Handle_Count := false;
                    Dec (FileHandleCount, 10);
                end
            else
                Increase_File_Handle_Count := true;
        end;
end;

procedure do_open(var f;p:pchar;flags:longint);

{
  filerec and textrec have both handle and mode as the first items so
  they could use the same routine for opening/creating.
  when (flags and $100)   the file will be append
  when (flags and $1000)  the file will be truncate/rewritten
  when (flags and $10000) there is no check for close (needed for textfiles)
}

var Action: longint;

begin
    allowslash(p);
    { close first if opened }
    if ((flags and $10000)=0) then
        begin
            case filerec(f).mode of
                fminput,fmoutput,fminout : Do_Close(filerec(f).handle);
                fmclosed:;
            else
                begin
                    inoutres:=102; {not assigned}
                    exit;
                end;
            end;
       end;
    { reset file handle }
    filerec(f).handle := UnusedHandle;
    Action := 0;
    { convert filemode to filerec modes }
    case (flags and 3) of
        0 : filerec(f).mode:=fminput;
        1 : filerec(f).mode:=fmoutput;
        2 : filerec(f).mode:=fminout;
    end;
    if (flags and $1000)<>0 then
        Action := $50000; (* Create / replace *)
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
    Action := Action or (Flags and $FF);
(* DenyAll if sharing not specified. *)
    if Flags and 112 = 0 then
        Action := Action or 16;
    asm
        movl $0x7f2b, %eax
        movl Action, %ecx
        movl p, %edx
        call syscall
        cmpl $0xffffffff, %eax
        jnz .LOPEN1
        movw %cx, InOutRes
        movw UnusedHandle, %ax
.LOPEN1:
        movl f,%edx
        movw %ax,(%edx)
    end;
    if (InOutRes = 4) and Increase_File_Handle_Count then
(* Trying again after increasing amount of file handles *)
        asm
            movl $0x7f2b, %eax
            movl Action, %ecx
            movl p, %edx
            call syscall
            cmpl $0xffffffff, %eax
            jnz .LOPEN2
            movw %cx, InOutRes
            movw UnusedHandle, %ax
.LOPEN2:
            movl f,%edx
            movw %ax,(%edx)
        end;
      { for systems that have more handles }
    if FileRec (F).Handle > FileHandleCount then
        FileHandleCount := FileRec (F).Handle;
    if (flags and $100)<>0 then
        begin
            do_seekend(filerec(f).handle);
            FileRec (F).Mode := fmOutput; {fool fmappend}
        end;
end;

{$ASMMODE INTEL}
function do_isdevice (Handle: longint): boolean; assembler;
(*
var HT, Attr: longint;
begin
    if os_mode = osOS2 then
        begin
            if DosQueryHType (Handle, HT, Attr) <> 0 then HT := 1;
        end
    else
*)
asm
    mov ebx, Handle
    mov eax, 4400h
    call syscall
    mov eax, 1
    jc @IsDevEnd
    test edx, 80h
    jnz @IsDevEnd
    dec eax
@IsDevEnd:
end;
{$ASMMODE ATT}


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

{****************************************************************************

                          Directory related routines.

****************************************************************************}

{*****************************************************************************
                           Directory Handling
*****************************************************************************}

procedure dosdir(func:byte;const s:string);

var buffer:array[0..255] of char;

begin
    move(s[1],buffer,length(s));
    buffer[length(s)]:=#0;
    allowslash(Pchar(@buffer));
    asm
        leal buffer,%edx
        movb func,%ah
        call syscall
        jnc  .LDOS_DIRS1
        movw %ax,inoutres;
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

procedure getdir(drivenr : byte;var dir : shortstring);

{Written by Michael Van Canneyt.}

var temp:array[0..255] of char;
    sof:Pchar;
    i:byte;

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
        call syscall
    end;
    { Now Dir should be filled with directory in ASCIIZ, }
    { starting from dir[4]                               }
    dir[0]:=#3;
    dir[2]:=':';
    dir[3]:='\';
    i:=4;
    {Conversion to Pascal string }
    while (dir[i]<>#0) do
        begin
            { convert path name to DOS }
            if dir[i]='/' then
            dir[i]:='\';
            dir[0]:=char(i);
            inc(i);
        end;
    { upcase the string (FPC function) }
    if not (FileNameCaseSensitive) then dir:=upcase(dir);
    if drivenr<>0 then   { Drive was supplied. We know it }
        dir[1]:=char(65+drivenr-1)
    else
        begin
            { We need to get the current drive from DOS function 19H  }
            { because the drive was the default, which can be unknown }
            asm
                movb $0x19,%ah
                call syscall
                addb $65,%al
                movb %al,i
            end;
            dir[1]:=char(i);
        end;
end;



{****************************************************************************

                        System unit initialization.

****************************************************************************}

function GetFileHandleCount: longint;
var L1, L2: longint;
begin
    L1 := 0; (* Don't change the amount, just check. *)
    if DosSetRelMaxFH (L1, L2) <> 0 then GetFileHandleCount := 50
                                                 else GetFileHandleCount := L2;
end;

var pib:Pprocessinfoblock;
    tib:Pthreadinfoblock;

begin
    {Determine the operating system we are running on.}
    asm
        movl $0,os_mode
        movw $0x7f0a,%ax
        call syscall
        testw $512,%bx         {Bit 9 is OS/2 flag.}
        setnzb os_mode
        testw $4096,%bx
        jz .LnoRSX
        movl $2,os_mode
    .LnoRSX:
    end;

    {$ASMMODE DIRECT}
    {Enable the brk area by initializing it with the initial heap size.}
    asm
        movw $0x7f01,%ax
        movl HEAPSIZE,%edx
        addl __heap_base,%edx
        call ___SYSCALL
        cmpl $-1,%eax
        jnz Lheapok
        pushl $204
        {call RUNERROR$$WORD}
    Lheapok:
    end;
    {$ASMMODE ATT}

    {Now request, if we are running under DOS,
     read-access to the first meg. of memory.}
    if os_mode in [osDOS,osDPMI] then
        asm
            movw $0x7f13,%ax
            xorl %ebx,%ebx
            movl $0xfff,%ecx
            xorl %edx,%edx
            call syscall
            movl %eax,first_meg
        end
    else
        begin
            first_meg := nil;
    (* Initialize the amount of file handles *)
            FileHandleCount := GetFileHandleCount;
        end;
    {At 0.9.2, case for enumeration does not work.}
    case os_mode of
        osDOS:
            stackbottom:=0;     {In DOS mode, heap_brk is also the
                                 stack bottom.}
        osOS2:
            begin
                dosgetinfoblocks(tib,pib);
                stackbottom:=longint(tib^.stack);
            end;
        osDPMI:
            stackbottom:=0;     {Not sure how to get it, but seems to be
                                 always zero.}
    end;
    exitproc:=nil;

    {Initialize the heap.}
    initheap;

    { ... and exceptions }
    InitExceptions;

    { to test stack depth }
    loweststack:=maxlongint;

    OpenStdIO(Input,fmInput,StdInputHandle);
    OpenStdIO(Output,fmOutput,StdOutputHandle);
    OpenStdIO(StdOut,fmOutput,StdOutputHandle);
    OpenStdIO(StdErr,fmOutput,StdErrorHandle);

    { no I/O-Error }
    inoutres:=0;
end.
{
  $Log$
  Revision 1.1  2000-07-13 06:31:07  michael
  + Initial import

  Revision 1.34  2000/07/09 17:09:47  hajny
    * little mistyping

  Revision 1.33  2000/07/09 17:05:24  hajny
    * default sharing mode changed to deny all (compatibility)

  Revision 1.32  2000/06/11 09:47:57  hajny
    * error handling and sharing corrected

  Revision 1.31  2000/06/05 18:53:30  hajny
    * FileHandleCount handling for OS/2 added

  Revision 1.30  2000/06/04 14:14:01  hajny
    * do_truncate corrected, do_open might work under W9x now

  Revision 1.29  2000/05/28 18:17:39  hajny
    do_isdevice corrected

  Revision 1.28  2000/05/21 15:58:50  hajny
    + FileNameCaseSensitive added

  Revision 1.27  2000/04/07 17:47:34  hajny
    * got rid of os.inc

  Revision 1.26  2000/02/09 16:59:34  peter
    * truncated log

  Revision 1.25  2000/02/09 12:39:11  peter
    * halt moved to system.inc

  Revision 1.24  2000/01/20 23:38:02  peter
    * support fm_inout as stdoutput for assign(f,'');rewrite(f,1); becuase
      rewrite opens always with filemode 2

  Revision 1.23  2000/01/16 23:10:15  peter
    * handle check fixed

  Revision 1.22  2000/01/16 22:25:38  peter
    * check handle for file closing

  Revision 1.21  2000/01/09 20:45:58  hajny
    * FPK changed to FPC

  Revision 1.20  2000/01/07 16:41:50  daniel
    * copyright 2000

  Revision 1.19  2000/01/07 16:32:33  daniel
    * copyright 2000 added

  Revision 1.18  2000/01/02 17:45:25  hajny
    * cdecl added for doscalls routines

  Revision 1.17  1999/09/10 15:40:35  peter
    * fixed do_open flags to be > $100, becuase filemode can be upto 255

}
