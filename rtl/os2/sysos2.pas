{****************************************************************************

                     FPK-Pascal -- OS/2 runtime library

                  Copyright (c) 1993,95 by Florian Kl„mpfl
                   Copyright (c) 1997 by Dani‰l Mantione

 FPK-Pascal is distributed under the GNU Public License v2. So is this unit.
 The GNU Public License requires you to distribute the source code of this
 unit with any product that uses it. We grant you an exception to this, and
 that is, when you compile a program with the FPK Pascal compiler, you do not
 need to ship source code with that program, AS LONG AS YOU ARE USING
 UNMODIFIED CODE! If you modify this code, you MUST change the next line:

 <This an official, unmodified FPK Pascal source code file.>

 Send us your modified files, we can work together if you want!

 FPK-Pascal is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 Library GNU General Public License for more details.

 You should have received a copy of the Library GNU General Public License
 along with FPK-Pascal; see the file COPYING.LIB.  If not, write to
 the Free Software Foundation, 59 Temple Place - Suite 330,
 Boston, MA 02111-1307, USA.

****************************************************************************}

unit sysos2;

{Changelog:

    People:

        DM - Dani‰l Mantione

    Date:           Description of change:              Changed by:

     -              First released version 0.1.         DM

Coding style:

    My coding style is a bit unusual for Pascal. Nevertheless I friendly ask
    you to try to make your changes not look all to different. In general,
    set your IDE to use tab characters, optimal fill on and a tabsize of 4.}

{$I os.inc}

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

implementation

{ die betriebssystemunabhangigen Implementationen einfuegen: }

{$I SYSTEM.INC}

procedure dosgetinfoblocks(var Atib:Pthreadinfoblock;
                           var Apib:Pprocessinfoblock);
                           external 'DOSCALLS' index 312;

{***************************************************************************

                Runtime error checking related routines.

***************************************************************************}

{$S-}
procedure st1(stack_size:longint);[public,alias: 'STACKCHECK'];

begin
    { called when trying to get local stack }
    { if the compiler directive $S is set   }
    asm
        movl stack_size,%ebx
        movl %esp,%eax
        subl %ebx,%eax
{$ifdef SYSTEMDEBUG}
        movl U_SYSOS2_LOWESTSTACK,%ebx
        cmpl %eax,%ebx
        jb   _is_not_lowest
        movl %eax,U_SYSOS2_LOWESTSTACK
    _is_not_lowest:
{$endif SYSTEMDEBUG}
        cmpb $2,U_SYSOS2_OS_MODE
        jne _running_in_dos
        movl U_SYSOS2_STACKBOTTOM,%ebx
        jmp _running_in_os2
    _running_in_dos:
        movl __heap_brk,%ebx
    _running_in_os2:
        cmpl %eax,%ebx
        jae  __short_on_stack
        leave
        ret  $4
    __short_on_stack:
    end ['EAX','EBX'];
    { this needs a local variable }
    { so the function called itself !! }
    { Writeln('low in stack ');}
    RunError(202);
end;
{no stack check in system }

{****************************************************************************

                    Miscelleanious related routines.

****************************************************************************}

procedure halt(errnum:byte);

begin
    asm
        movb $0x4c,%ah
        movb errnum,%al
        call ___syscall
    end;
end;

function paramcount:longint;

begin
     asm
        movl _argc,%eax
        decl %eax
        leave
        ret
     end ['EAX'];
end;

function paramstr(l:longint):string;

    function args:pointer;

    begin
        asm
            movl _argv,%eax
            leave
            ret
        end ['EAX'];
    end;

var p:^Pchar;

begin
     if (l>=0) and (l<=paramcount) then
        begin
            p:=args;
            paramstr:=strpas(p[l]);
        end
     else paramstr:='';
end;

procedure randomize;

var hl:longint;

begin
    asm
        movb $0x2c,%ah
        call ___syscall
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

function sbrk(size:longint):longint;

begin
    asm
        movl size,%edx
        movl $0x7f00,%ax
        int  $0x21
        movl %eax,__RESULT
    end;
end;

function getheapstart:pointer;

begin
    asm
        movl __heap_base,%eax
        leave
        ret
    end ['EAX'];
end;

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
     asm
        movb $0x3e,%ah
        mov h,%ebx
        call ___syscall
     end;
end;

procedure do_erase(p:Pchar);

begin
    allowslash(p);
    asm
        movl 8(%ebp),%edx
        movb $0x41,%ah
        call ___syscall
        jnc LERASE1
        movw %ax,U_SYSOS2_INOUTRES;
    LERASE1:
    end;
end;

procedure do_rename(p1,p2:Pchar);

begin
    allowslash(p1);
    allowslash(p2);
    asm
        movl 8(%ebp),%edx
        movl 12(%ebp),%edi
        movb $0x56,%ah
        call ___syscall
        jnc LRENAME1
        movw %ax,U_SYSOS2_INOUTRES;
    LRENAME1:
    end;
end;

function do_read(h,addr,len:longint):longint;

begin
    asm
        movl 16(%ebp),%ecx
        movl 12(%ebp),%edx
        movl 8(%ebp),%ebx
        movb $0x3f,%ah
        call ___syscall
        jnc LDOSREAD1
        movw %ax,U_SYSOS2_INOUTRES;
        xorl %eax,%eax
    LDOSREAD1:
        leave
        ret $12
    end;
end;

function do_write(h,addr,len:longint) : longint;

begin
    asm
        movl 16(%ebp),%ecx
        movl 12(%ebp),%edx
        movl 8(%ebp),%ebx
        movb $0x40,%ah
        call ___syscall
        jnc LDOSWRITE1
        movw %ax,U_SYSOS2_INOUTRES;
    LDOSWRITE1:
       movl %eax,-4(%ebp)
    end;
end;

function do_filepos(handle:longint):longint;

begin
    asm
        movb $0x42,%ah
        movb $0x1,%al
        movl 8(%ebp),%ebx
        xorl %edx,%edx
        call ___syscall
        jnc LDOSFILEPOS
        movw %ax,U_SYSOS2_INOUTRES;
        xorl %eax,%eax
    LDOSFILEPOS:
        leave
        ret $4
     end;
end;

procedure do_seek(handle,pos:longint);

begin
    asm
        movl $0x4200,%eax
        movl 8(%ebp),%ebx
        movl 12(%ebp),%edx
        movl %edx,%ecx
        shrl $16,%ecx
        call ___syscall
        jnc .LDOSSEEK1
        movw %ax,U_SYSOS2_INOUTRES;
    .LDOSSEEK1:
        leave
        ret $8
    end;
end;

function do_seekend(handle:longint):longint;

begin
    asm
        movl $0x4202,%eax
        movl 8(%ebp),%ebx
        xorl %ecx,%ecx
        xorl %edx,%edx
        call ___syscall
        jnc .Lset_at_end1
        movw %ax,U_SYSOS2_INOUTRES;
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

function do_filesize(handle:longint):longint;

var aktfilepos:longint;

begin
    aktfilepos:=do_filepos(handle);
    do_filesize:=do_seekend(handle);
    do_seek(handle,aktfilepos);
end;

procedure do_truncate(handle,pos:longint);

begin
    asm
        movl $0x4200,%eax
        movl 8(%ebp),%ebx
        movl 12(%ebp),%edx
        movl %edx,%ecx
        shrl $16,%ecx
        call ___syscall
        jc .LTruncate1
        movl 8(%ebp),%ebx
        movl 12(%ebp),%edx
        movl %ebp,%edx
        xorl %ecx,%ecx
        movb $0x40,%ah
        call ___syscall
        jnc .LTruncate2
        .LTruncate1:
        movw %ax,U_SYSOS2_INOUTRES;
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

var oflags:longint;

begin
    allowslash(p);
    { close first if opened }
    if ((flags and $1000)=0) then
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
    filerec(f).handle:=high(word);
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
                fminput:filerec(f).handle:=StdInputHandle;
                fmappend,fmoutput : begin
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
        call ___syscall
        jnc .LOPEN1
        movw %ax,U_SYSOS2_INOUTRES;
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
        movb 8(%ebp),%ah
        call ___syscall
        jnc  .LDOS_DIRS1
        movw %ax,U_SYSOS2_INOUTRES;
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

procedure getdir(drivenr : byte;var dir : string);

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
        call ___syscall
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
    { upcase the string (FPKPascal function) }
    dir:=upcase(dir);
    if drivenr<>0 then   { Drive was supplied. We know it }
        dir[1]:=char(65+drivenr-1)
    else
        begin
            { We need to get the current drive from DOS function 19H  }
            { because the drive was the default, which can be unknown }
            asm
                movb $0x19,%ah
                call ___syscall
                addb $65,%al
                movb %al,i
            end;
            dir[1]:=char(i);
        end;
end;



{****************************************************************************

                        System unit initialization.

****************************************************************************}

procedure OpenStdIO(var f:text;mode:word;hdl:longint);

begin
    Assign(f,'');
    TextRec(f).Handle:=hdl;
    TextRec(f).Mode:=mode;
    TextRec(f).InOutFunc:=@FileInOutFunc;
    TextRec(f).FlushFunc:=@FileInOutFunc;
    TextRec(f).Closefunc:=@fileclosefunc;
end;

var pib:Pprocessinfoblock;
    tib:Pthreadinfoblock;

begin
    {Determine the operating system we are running on.}
    asm
        movw $0x7f0a,%ax
        call ___syscall
        test $512,%bx          {Bit 9 is OS/2 flag.}
        setnzb U_SYSOS2_OS_MODE
        test $4096,%bx
        jz _noRSX
        movb $2,U_SYSOS2_OS_MODE
    _noRSX:
    end;

    {Enable the brk area by initializing it with the initial heap size.}
    asm
        mov $0x7f01,%ax
        movl HEAPSIZE,%edx
        call ___syscall
    end;

    {Now request, if we are running under DOS,
     read-access to the first meg. of memory.}
    if os_mode in [osDOS,osDPMI] then
        asm
            mov $0x7f13,%ax
            xor %ebx,%ebx
            mov $0xfff,%ecx
            xor %edx,%edx
            call ___syscall
            mov %eax,U_SYSOS2_FIRST_MEG
        end
    else
        first_meg:=nil;
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

    { to test stack depth }
    loweststack:=maxlongint;

    OpenStdIO(Input,fmInput,StdInputHandle);
    OpenStdIO(Output,fmOutput,StdOutputHandle);
    OpenStdIO(StdErr,fmOutput,StdErrorHandle);

    { kein Ein- Ausgabefehler }
    inoutres:=0;
end.
