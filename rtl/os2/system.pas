{
 $Id$
 ****************************************************************************

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

unit {$ifdef VER1_0}sysos2{$else}System{$endif};

{Changelog:

    People:

        DM - Daniel Mantione

    Date:           Description of change:              Changed by:

     -              First released version 0.1.         DM

Coding style:

    My coding style is a bit unusual for Pascal. Nevertheless I friendly ask
    you to try to make your changes not look all to different. In general,
    set your IDE to use a tabsize of 4.}

interface

{Link the startup code.}
{$l prt1.oo2}

{$I SYSTEMH.INC}

type
    { FK: The fields of this record are OS dependent and they shouldn't  }
    { be used in a program; only the type TCriticalSection is important. }
    (* TH: To make things easier, I copied the record definition *)
    (* from the Win32 version and just added longint variants,   *)
    (* because it seemed well suited for OS/2 too.               *)
    TCriticalSection = packed record
        DebugInfo: pointer;
        LockCount: longint;
        RecursionCount: longint;
        case boolean of
        false:
        (OwningThread: DWord;
        LockSemaphore: DWord;
        Reserved: DWord);
        true:
        (OwningThread2: longint;
        LockSemaphore2: longint;
        Reserved2: longint);
    end;

{ include threading stuff }
{$i threadh.inc}

{$I heaph.inc}

type    Tos=(osDOS,osOS2,osDPMI);

var     os_mode:Tos;
        first_meg:pointer;

type    PSysThreadIB=^TSysThreadIB;
        PThreadInfoBlock=^Tthreadinfoblock;
        PPThreadInfoBlock=^PThreadInfoBlock;
        PProcessInfoBlock=^TProcessInfoBlock;
        PPProcessInfoBlock=^PProcessInfoBlock;

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

var
    heap_base: pointer; external name '__heap_base';
    heap_brk: pointer; external name '__heap_brk';

procedure DosGetInfoBlocks (PATIB: PPThreadInfoBlock;
                            PAPIB: PPProcessInfoBlock); cdecl;
                            external 'DOSCALLS' index 312;

function DosSetRelMaxFH (var ReqCount, CurMaxFH: longint): longint; cdecl;
external 'DOSCALLS' index 382;

function DosSetCurrentDir (Name:PChar): longint; cdecl;
external 'DOSCALLS' index 255;

function DosSetDefaultDisk (DiskNum:longint): longint; cdecl;
external 'DOSCALLS' index 220;

{This is the correct way to call external assembler procedures.}
procedure syscall; external name '___SYSCALL';

{***************************************************************************

                Runtime error checking related routines.

***************************************************************************}

{$S-}
procedure st1(stack_size:longint); assembler; [public,alias: 'FPC_STACKCHECK'];

asm
    movl stack_size,%ebx
    movl %esp,%eax
    subl %ebx,%eax
{$ifdef SYSTEMDEBUG}
    movl loweststack,%ebx
    cmpl %eax,%ebx
    jb   .Lis_not_lowest
    movl %eax,loweststack
.Lis_not_lowest:
{$endif SYSTEMDEBUG}
    cmpb osOS2,os_mode
    jne .Lrunning_in_dos
    movl stackbottom,%ebx
    jmp .Lrunning_in_os2
.Lrunning_in_dos:
    movl heap_brk,%ebx
.Lrunning_in_os2:
    cmpl %eax,%ebx
    jae  .Lshort_on_stack
.Lshort_on_stack:
    pushl $202
    call HandleError
end ['EAX','EBX'];
{no stack check in system }

{****************************************************************************

                    Miscellaneous related routines.

****************************************************************************}

{$asmmode intel}
procedure system_exit; assembler;
asm
    mov  ah, 04ch
    mov  al, byte ptr exitcode
    call syscall
end ['EAX'];

{$ASMMODE ATT}

function paramcount:longint;assembler;

asm
    movl argc,%eax
    decl %eax
end ['EAX'];

function paramstr(l:longint):string;

    function args:pointer;assembler;

    asm
        movl argv,%eax
    end ['EAX'];

var p:^Pchar;

begin
    if L = 0 then
        begin
            GetMem (P, 260);
{$ASMMODE INTEL}
            asm
                mov edx, P
                mov ecx, 260
                mov eax, 7F33h
                call syscall
            end;
            ParamStr := StrPas (PChar (P));
            FreeMem (P, 260);
        end
    else
        if (l>0) and (l<=paramcount) then
            begin
                p:=args;
                paramstr:=strpas(p[l]);
            end
        else paramstr:='';
end;

{
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
}

procedure randomize; assembler;
asm
    mov ah, 2Ch
    call syscall
    mov word ptr [randseed], cx
    mov word ptr [randseed + 2], dx
end;

{$ASMMODE ATT}

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

function getheapstart:pointer;assembler;

asm
    movl heap_base,%eax
end ['EAX'];

function getheapsize:longint;assembler;
asm
    movl HeapSize,%eax
end ['EAX'];

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
        movl h,%ebx
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
        movl handle,%ebx
        movl pos,%edx
        call syscall
        jc .LTruncate1
        movl handle,%ebx
        movl pos,%edx
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
        movw %ax,inoutres
    .LDOS_DIRS1:
    end;
end;


procedure MkDir (const S: string);

begin
    If (s='') or (InOutRes <> 0) then
     exit;
        DosDir ($39, S);
end;


procedure rmdir(const s : string);

begin
    If (s='') or (InOutRes <> 0) then
     exit;
        DosDir ($3A, S);
end;

{$ASMMODE INTEL}

procedure ChDir (const S: string);

var RC: longint;
    Buffer: array [0..255] of char;

begin
    If (s='') or (InOutRes <> 0) then
     exit;
(* According to EMX documentation, EMX has only one current directory
   for all processes, so we'll use native calls under OS/2. *)
            if os_Mode = osOS2 then
                begin
                    if (Length (S) >= 2) and (S [2] = ':') then
                        begin
                            RC := DosSetDefaultDisk ((Ord (S [1]) and
                                                             not ($20)) - $40);
                            if RC <> 0 then
                                InOutRes := RC
                            else
                                if Length (S) > 2 then
                                    begin
                                        Move (S [1], Buffer, Length (S));
                                        Buffer [Length (S)] := #0;
                                        AllowSlash (PChar (@Buffer));
                                        RC := DosSetCurrentDir (@Buffer);
                                        if RC <> 0 then
                                            InOutRes := RC;
                                    end;
                        end
                    else
                        begin
                            Move (S [1], Buffer, Length (S));
                            Buffer [Length (S)] := #0;
                            AllowSlash (PChar (@Buffer));
                            RC := DosSetCurrentDir (@Buffer);
                            if RC <> 0 then
                                InOutRes := RC;
                        end;
                end
            else
                if (Length (S) >= 2) and (S [2] = ':') then
                    begin
                        asm
                            mov esi, S
                            mov al, [esi + 1]
                            and al, not (20h)
                            sub al, 41h
                            mov edx, eax
                            mov ah, 0Eh
                            call syscall
                            mov ah, 19h
                            call syscall
                            cmp al, dl
                            jz @LCHDIR
                            mov InOutRes, 15
@LCHDIR:
                        end;
                        if (Length (S) > 2) and (InOutRes <> 0) then
                            DosDir ($3B, S);
                    end
                else
                    DosDir ($3B, S);
end;

{$ASMMODE ATT}

function GetDirIO (DriveNr: byte; var Dir: ShortString): word;
                                               [public, alias: 'FPC_GETDIRIO'];

{Written by Michael Van Canneyt.}

var sof:Pchar;
    i:byte;
    IOR: word;

begin
    Dir [4] := #0;
    { Used in case the specified drive isn't available }
    sof:=pchar(@dir[4]);
    { dir[1..3] will contain '[drivenr]:\', but is not }
    { supplied by DOS, so we let dos string start at   }
    { dir[4]                                           }
    { Get dir from drivenr : 0=default, 1=A etc... }
    IOR := 0;
    asm
        movb drivenr,%dl
        movl sof,%esi
        mov  $0x47,%ah
        call syscall
        jnc .LGetDir
        movw %ax, IOR
.LGetDir:
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
    if drivenr<>0 then   { Drive was supplied. We know it }
        dir[1]:=chr(64+drivenr)
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
    if not (FileNameCaseSensitive) then dir:=upcase(dir);
    GetDirIO := IOR;
end;

procedure GetDir (DriveNr: byte; var Dir: ShortString);

begin
    InOutRes := GetDirIO (DriveNr, Dir);
end;


{****************************************************************************

                             Thread Handling

*****************************************************************************}

const
    fpucw: word = $1332;

procedure InitFPU; assembler;

asm
    fninit
    fldcw fpucw
end;

{ include threading stuff, this is os independend part }
{$I thread.inc}

{*****************************************************************************

                        System unit initialization.

****************************************************************************}

function GetFileHandleCount: longint;
var L1, L2: longint;
begin
    L1 := 0; (* Don't change the amount, just check. *)
    if DosSetRelMaxFH (L1, L2) <> 0 then GetFileHandleCount := 50
                                                 else GetFileHandleCount := L2;
end;

var tib:Pthreadinfoblock;

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
{    end;}

    {Enable the brk area by initializing it with the initial heap size.}
{    asm}
        movw $0x7f01,%ax
        movl HeapSize,%edx
        addl heap_base,%edx
        call syscall
        cmpl $-1,%eax
        jnz .Lheapok
        pushl $204
        call HandleError
    .Lheapok:
    end;

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
                dosgetinfoblocks(@tib,nil);
                stackbottom:=longint(tib^.stack);
            end;
        osDPMI:
            stackbottom:=0;     {Not sure how to get it, but seems to be
                                 always zero.}
    end;
    exitproc:=nil;

{$ifdef MT}
    if os_mode = osOS2 then
        begin
            { allocate one ThreadVar entry from the OS, we use this entry }
            { for a pointer to our threadvars                             }
            if DosAllocThreadLocalMemory (1, DataIndex) <> 0 then RunError (8);
            { the exceptions use threadvars so do this _before_ initexceptions }
            AllocateThreadVars;
        end;
{$endif MT}

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
  Revision 1.9  2001-03-10 09:57:51  hajny
    * FExpand without IOResult change, remaining direct asm removed

  Revision 1.8  2001/02/20 21:31:12  peter
    * chdir,mkdir,rmdir with empty string fixed

  Revision 1.7  2001/02/04 01:57:52  hajny
    * direct asm removing

  Revision 1.6  2001/02/01 21:30:01  hajny
    * MT support completion

  Revision 1.5  2001/01/23 20:38:59  hajny
    + beginning of the OS/2 version

  Revision 1.4  2000/11/13 21:23:38  hajny
    * ParamStr (0) fixed

  Revision 1.3  2000/11/11 23:12:39  hajny
    * stackcheck alias corrected

  Revision 1.2  2000/10/15 20:43:10  hajny
    * ChDir correction, unit name changed

  Revision 1.1  2000/10/15 08:19:49  peter
    * system unit rename for 1.1 branch

  Revision 1.3  2000/09/29 21:49:41  jonas
    * removed warnings

  Revision 1.2  2000/07/14 10:33:11  michael
  + Conditionals fixed

  Revision 1.1  2000/07/13 06:31:07  michael
  + Initial import

}
