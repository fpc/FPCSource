{
 $Id$
 ****************************************************************************

    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2002 by Free Pascal development team

    Free Pascal - EMX runtime library

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

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
{$ifdef VER1_0}
 {$l prt1.oo2}
{$else}
 {$l prt1.o}
{$endif}

{$I systemh.inc}

{$I heaph.inc}

{Platform specific information}
const
 LineEnding = #13#10;
{ LFNSupport is defined separately below!!! }
 DirectorySeparator = '\';
 DriveSeparator = ':';
 PathSeparator = ';';
{ FileNameCaseSensitive is defined separately below!!! }

type    Tos=(osDOS,osOS2,osDPMI);

var     os_mode:Tos;
        first_meg:pointer;

type    TByteArray = array [0..$ffff] of byte;
        PByteArray = ^TByteArray;

        TSysThreadIB = record
            TID,
            Priority,
            Version: cardinal;
            MCCount,
            MCForceFlag: word;
        end;
        PSysThreadIB = ^TSysThreadIB;

        TThreadInfoBlock = record
            PExChain,
            Stack,
            StackLimit: pointer;
            TIB2: PSysThreadIB;
            Version,
            Ordinal: cardinal;
        end;
        PThreadInfoBlock = ^TThreadInfoBlock;
        PPThreadInfoBlock = ^PThreadInfoBlock;

        TProcessInfoBlock = record
            PID,
            ParentPid,
            Handle: cardinal;
            Cmd,
            Env: PByteArray;
            Status,
            ProcType: cardinal;
        end;
        PProcessInfoBlock = ^TProcessInfoBlock;
        PPProcessInfoBlock = ^PProcessInfoBlock;

const   UnusedHandle=$ffff;
        StdInputHandle=0;
        StdOutputHandle=1;
        StdErrorHandle=2;

        LFNSupport: boolean = true;
        FileNameCaseSensitive: boolean = false;

        sLineBreak = LineEnding;
        DefaultTextLineBreakStyle : TTextLineBreakStyle = tlbsCRLF;

var
{ C-compatible arguments and environment }
  argc  : longint;external name '_argc';
  argv  : ppchar;external name '_argv';
  envp  : ppchar;external name '_environ';
  EnvC: cardinal; external name '_envc';

(* Pointer to the block of environment variables - used e.g. in unit Dos. *)
  Environment: PChar;

var
(* Type / run mode of the current process: *)
(* 0 .. full screen OS/2 session           *)
(* 1 .. DOS session                        *)
(* 2 .. VIO windowable OS/2 session        *)
(* 3 .. Presentation Manager OS/2 session  *)
(* 4 .. detached (background) OS/2 process *)
  ApplicationType: cardinal;

implementation

{$I system.inc}

var
    heap_base: pointer; external name '__heap_base';
    heap_brk: pointer; external name '__heap_brk';
    heap_end: pointer; external name '__heap_end';

(* Maximum heap size - only used if heap is allocated as continuous block. *)
{$IFDEF CONTHEAP}
    BrkLimit: cardinal;
{$ENDIF CONTHEAP}

procedure DosGetInfoBlocks (PATIB: PPThreadInfoBlock;
                            PAPIB: PPProcessInfoBlock); cdecl;
                            external 'DOSCALLS' index 312;

function DosLoadModule (ObjName: PChar; ObjLen: cardinal; DLLName: PChar;
                                         var Handle: cardinal): longint; cdecl;
external 'DOSCALLS' index 318;

function DosQueryProcAddr (Handle, Ordinal: cardinal; ProcName: PChar;
                                         var Address: pointer): longint; cdecl;
external 'DOSCALLS' index 321;

function DosSetRelMaxFH (var ReqCount, CurMaxFH: longint): longint; cdecl;
external 'DOSCALLS' index 382;

function DosSetCurrentDir (Name:PChar): longint; cdecl;
external 'DOSCALLS' index 255;

function DosSetDefaultDisk (DiskNum:longint): longint; cdecl;
external 'DOSCALLS' index 220;

{ This is not real prototype, but is close enough }
{ for us (the 2nd parameter is actually a pointer }
{ to a structure).                                }
function DosCreateDir( Name : pchar; p : pointer): longint; cdecl;
external 'DOSCALLS' index 270;

function DosDeleteDir( Name : pchar) : longint; cdecl;
external 'DOSCALLS' index 226;

{This is the correct way to call external assembler procedures.}
procedure syscall; external name '___SYSCALL';

{
procedure syscall; external 'EMX' index 2;

procedure emx_init; external 'EMX' index 1;
}



   { converts an OS/2 error code to a TP compatible error }
   { code. Same thing exists under most other supported   }
   { systems.                                             }
   { Only call for OS/2 DLL imported routines             }
   Procedure Errno2InOutRes;
   Begin
     { errors 1..18 are the same as in DOS }
     case InOutRes of
      { simple offset to convert these error codes }
      { exactly like the error codes in Win32      }
      19..31 : InOutRes := InOutRes + 131;
      { gets a bit more complicated ... }
      32..33 : InOutRes := 5;
      38 : InOutRes := 100;
      39 : InOutRes := 101;
      112 : InOutRes := 101;
      110 : InOutRes := 5;
      114 : InOutRes := 6;
      290 : InOutRes := 290;
     end;
     { all other cases ... we keep the same error code }
   end;


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

    function args:pointer;assembler;

    asm
        movl argv,%eax
    end ['EAX'];


function paramstr(l:longint):string;

var p:^Pchar;

begin
    { There seems to be a problem with EMX for DOS when trying to }
    { access paramstr(0), and to avoid problems between DOS and   }
    { OS/2 they have been separated.                              }
    if os_Mode = OsOs2 then
    begin
    if L = 0 then
        begin
            GetMem (P, 260);
            p[0] := #0;  { in case of error, initialize to empty string }
{$ASMMODE INTEL}
            asm
                mov edx, P
                mov ecx, 260
                mov eax, 7F33h
                call syscall    { error handle already with empty string }
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
    end
   else
    begin
      p:=args;
      paramstr:=strpas(p[l]);
    end;
end;


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

function sbrk(size:longint):longint;
{$IFDEF DUMPGROW}
var
  L: longint;
begin
  WriteLn ('Trying to grow heap by ', Size, ' to ', HeapSize + Size);
{$IFDEF CONTHEAP}
  WriteLn ('BrkLimit is ', BrkLimit);
{$ENDIF CONTHEAP}
  asm
    movl size,%edx
    movw $0x7f00,%ax
    call syscall     { result directly in EAX }
    mov  %eax,L
  end;
  WriteLn ('New heap at ', L);
  Sbrk := L;
end;
{$ELSE DUMPGROW}
                                     assembler;
asm
    movl size,%edx
    movw $0x7f00,%ax
    call syscall     { result directly in EAX }
end;
{$ENDIF DUMPGROW}

function getheapstart:pointer;assembler;

asm
    movl heap_base,%eax
end ['EAX'];

function getheapsize:longint;assembler;
asm
    movl heap_brk,%eax
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
     ((os_MODE = osOS2) and (h > 2)) then
   begin
     asm
        movb $0x3e,%ah
        movl h,%ebx
        call syscall
        jnc  .Lnoerror           { error code?            }
        movw  %ax, InOutRes       { yes, then set InOutRes }
     .Lnoerror:
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
    xorl %eax,%eax
    cmpl $0,len    { 0 bytes to write is undefined behavior }
    jz   .LDOSWRITE1
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
(* DOS function 40h isn't safe for this according to EMX documentation *)
    movl $0x7F25,%eax
    movl Handle,%ebx
    movl Pos,%edx
    call syscall
    incl %eax
    movl %ecx, %eax
    jnz .LTruncate1      { compare the value of EAX to verify error }
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
(* DenyNone if sharing not specified. *)
    if Flags and 112 = 0 then
        Action := Action or 64;
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
        movl f,%edx         { Warning : This assumes Handle is first }
        movw %ax,(%edx)     { field of FileRec                       }
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
    test edx, 80h           { verify if it is a file  }
    jnz @IsDevEnd
    dec eax                 { nope, so result is zero }
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


procedure MkDir (const S: string);[IOCHECK];

var buffer:array[0..255] of char;
    Rc : word;

begin
  If (s='') or (InOutRes <> 0) then
   exit;
 if os_mode = osOs2 then
    begin
      move(s[1],buffer,length(s));
      buffer[length(s)]:=#0;
      allowslash(Pchar(@buffer));
      Rc := DosCreateDir(buffer,nil);
      if Rc <> 0 then
       begin
         InOutRes := Rc;
         Errno2Inoutres;
       end;
    end
  else
   begin
     { Under EMX 0.9d DOS this routine call may sometimes fail   }
     { The syscall documentation indicates clearly that this     }
     { routine was NOT tested.                                   }
        DosDir ($39, S);
end;
end;


procedure rmdir(const s : string);[IOCHECK];
var buffer:array[0..255] of char;
    Rc : word;
begin
  if (s = '.' ) then
    InOutRes := 16;
  If (s='') or (InOutRes <> 0) then
   exit;
  if os_mode = osOs2 then
    begin
      move(s[1],buffer,length(s));
      buffer[length(s)]:=#0;
      allowslash(Pchar(@buffer));
      Rc := DosDeleteDir(buffer);
      if Rc <> 0 then
       begin
         InOutRes := Rc;
         Errno2Inoutres;
       end;
    end
  else
   begin
     { Under EMX 0.9d DOS this routine call may sometimes fail   }
     { The syscall documentation indicates clearly that this     }
     { routine was NOT tested.                                   }
        DosDir ($3A, S);
end;
end;

{$ASMMODE INTEL}

procedure ChDir (const S: string);[IOCheck];

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
                                         begin
                                            InOutRes := RC;
                                            Errno2InOutRes;
                                         end;
                                    end;
                        end
                    else
                        begin
                            Move (S [1], Buffer, Length (S));
                            Buffer [Length (S)] := #0;
                            AllowSlash (PChar (@Buffer));
                            RC := DosSetCurrentDir (@Buffer);
                            if RC <> 0 then
                             begin
                                  InOutRes:= RC;
                                  Errno2InOutRes;
                             end;
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
                            { Under EMX 0.9d DOS this routine may sometime }
                            { fail or crash the system.                    }
                            DosDir ($3B, S);
                    end
                else
                    { Under EMX 0.9d DOS this routine may sometime }
                    { fail or crash the system.                    }
                    DosDir ($3B, S);
end;

{$ASMMODE ATT}

procedure GetDir (DriveNr: byte; var Dir: ShortString);

{Written by Michael Van Canneyt.}

var sof:Pchar;
    i:byte;

begin
    Dir [4] := #0;
    { Used in case the specified drive isn't available }
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
        jnc .LGetDir
        movw %ax, InOutRes
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
end;


{*****************************************************************************

                        System unit initialization.

****************************************************************************}

{****************************************************************************
                    Error Message writing using messageboxes
****************************************************************************}

type
  TWinMessageBox = function (Parent, Owner: cardinal;
         BoxText, BoxTitle: PChar; Identity, Style: cardinal): cardinal; cdecl;
  TWinInitialize = function (Options: cardinal): cardinal; cdecl;
  TWinCreateMsgQueue = function (Handle: cardinal; cmsg: longint): cardinal;
                                                                         cdecl;

const
  ErrorBufferLength = 1024;
  mb_OK = $0000;
  mb_Error = $0040;
  mb_Moveable = $4000;
  MBStyle = mb_OK or mb_Error or mb_Moveable;
  WinInitialize: TWinInitialize = nil;
  WinCreateMsgQueue: TWinCreateMsgQueue = nil;
  WinMessageBox: TWinMessageBox = nil;
  EnvSize: cardinal = 0;

var
  ErrorBuf: array [0..ErrorBufferLength] of char;
  ErrorLen: longint;
  PMWinHandle: cardinal;

function ErrorWrite (var F: TextRec): integer;
{
  An error message should always end with #13#10#13#10
}
var
  P: PChar;
  I: longint;
begin
  if F.BufPos > 0 then
   begin
     if F.BufPos + ErrorLen > ErrorBufferLength then
       I := ErrorBufferLength - ErrorLen
     else
       I := F.BufPos;
     Move (F.BufPtr^, ErrorBuf [ErrorLen], I);
     Inc (ErrorLen, I);
     ErrorBuf [ErrorLen] := #0;
   end;
  if ErrorLen > 3 then
   begin
     P := @ErrorBuf [ErrorLen];
     for I := 1 to 4 do
      begin
        Dec (P);
        if not (P^ in [#10, #13]) then
          break;
      end;
   end;
   if ErrorLen = ErrorBufferLength then
     I := 4;
   if (I = 4) then
    begin
      WinMessageBox (0, 0, @ErrorBuf, PChar ('Error'), 0, MBStyle);
      ErrorLen := 0;
    end;
  F.BufPos := 0;
  ErrorWrite := 0;
end;

function ErrorClose (var F: TextRec): integer;
begin
  if ErrorLen > 0 then
   begin
     WinMessageBox (0, 0, @ErrorBuf, PChar ('Error'), 0, MBStyle);
     ErrorLen := 0;
   end;
  ErrorLen := 0;
  ErrorClose := 0;
end;

function ErrorOpen (var F: TextRec): integer;
begin
  TextRec(F).InOutFunc := @ErrorWrite;
  TextRec(F).FlushFunc := @ErrorWrite;
  TextRec(F).CloseFunc := @ErrorClose;
  ErrorOpen := 0;
end;


procedure AssignError (var T: Text);
begin
  Assign (T, '');
  TextRec (T).OpenFunc := @ErrorOpen;
  Rewrite (T);
end;


procedure DosEnvInit;
var
 Q: PPChar;
 I: cardinal;
begin
(* It's a hack, in fact - DOS stores the environment the same way as OS/2 does,
   but I don't know how to find Program Segment Prefix and thus the environment
   address under EMX, so I'm recreating this structure using EnvP pointer. *)
{$ASMMODE INTEL}
 asm
  cld
  mov ecx, EnvC
  mov esi, EnvP
  xor eax, eax
  xor edx, edx
@L1:
  xchg eax, edx
  push ecx
  mov ecx, -1
  mov edi, [esi]
  repne
  scasb
  neg ecx
  dec ecx
  xchg eax, edx
  add eax, ecx
  pop ecx
  dec ecx
  jecxz @Stop
  inc esi
  inc esi
  inc esi
  inc esi
  jmp @L1
@Stop:
  inc eax
  mov EnvSize, eax
 end;
 Environment := GetMem (EnvSize);
 asm
  cld
  mov ecx, EnvC
  mov edx, EnvP
  mov edi, Environment
@L2:
  mov esi, [edx]
@Copying:
  lodsb
  stosb
  or al, al
  jnz @Copying
  dec ecx
  jecxz @Stop2
  inc edx
  inc edx
  inc edx
  inc edx
  jmp @L2
@Stop2:
  stosb
 end;
end;


procedure SysInitStdIO;
begin
  { Setup stdin, stdout and stderr, for GUI apps redirect stderr,stdout to be
    displayed in a messagebox }
(*
  StdInputHandle := longint(GetStdHandle(cardinal(STD_INPUT_HANDLE)));
  StdOutputHandle := longint(GetStdHandle(cardinal(STD_OUTPUT_HANDLE)));
  StdErrorHandle := longint(GetStdHandle(cardinal(STD_ERROR_HANDLE)));

  if not IsConsole then
    begin
      if (DosLoadModule (nil, 0, 'PMWIN', PMWinHandle) = 0) and
       (DosQueryProcAddr (PMWinHandle, 789, nil, pointer (WinMessageBox)) = 0)
                                                                           and
       (DosQueryProcAddr (PMWinHandle, 763, nil, pointer (WinInitialize)) = 0)
                                                                           and
       (DosQueryProcAddr (PMWinHandle, 716, nil, pointer (WinCreateMsgQueue))
                                                                           = 0)
        then
          begin
            WinInitialize (0);
            WinCreateMsgQueue (0, 0);
          end
        else
          HandleError (2);
     AssignError (StdErr);
     AssignError (StdOut);
     Assign (Output, '');
     Assign (Input, '');
   end
  else
   begin
*)
     OpenStdIO (Input, fmInput, StdInputHandle);
     OpenStdIO (Output, fmOutput, StdOutputHandle);
     OpenStdIO (StdOut, fmOutput, StdOutputHandle);
     OpenStdIO (StdErr, fmOutput, StdErrorHandle);
(*
   end;
*)
end;


function GetFileHandleCount: longint;
var L1, L2: longint;
begin
    L1 := 0; (* Don't change the amount, just check. *)
    if DosSetRelMaxFH (L1, L2) <> 0 then GetFileHandleCount := 50
                                                 else GetFileHandleCount := L2;
end;

var TIB: PThreadInfoBlock;
    PIB: PProcessInfoBlock;

begin
    IsLibrary := FALSE;
    {Determine the operating system we are running on.}
{$ASMMODE INTEL}
    asm
        mov os_mode, 0
        mov eax, 7F0Ah
        call syscall
        test bx, 512         {Bit 9 is OS/2 flag.}
        setne byte ptr os_mode
        test bx, 4096
        jz @noRSX
        mov os_mode, 2
    @noRSX:

    {Enable the brk area by initializing it with the initial heap size.}

        mov eax, 7F01h
        mov edx, heap_brk
        add edx, heap_base
        call syscall
        cmp eax, -1
        jnz @heapok
        push dword 204
        call HandleError
    @heapok:
{$IFDEF CONTHEAP}
{ Find out brk limit }
        mov eax, 7F02h
        mov ecx, 3
        call syscall
        jcxz @heaplimitknown
        mov eax, 0
    @heaplimitknown:
        mov BrkLimit, eax
{$ELSE CONTHEAP}
{ Change sbrk behaviour to allocate arbitrary (non-contiguous) memory blocks }
        mov eax, 7F0Fh
        mov ecx, 0Ch
        mov edx, 8
        call syscall
{$ENDIF CONTHEAP}
    end;

    { in OS/2 this will always be nil, but in DOS mode }
    { this can be changed.                             }
    first_meg := nil;
    {Now request, if we are running under DOS,
     read-access to the first meg. of memory.}
    if os_mode in [osDOS,osDPMI] then
        asm
            mov eax, 7F13h
            xor ebx, ebx
            mov ecx, 0FFFh
            xor edx, edx
            call syscall
            jc @endmem
            mov first_meg, eax
         @endmem:
        end
    else
        begin
    (* Initialize the amount of file handles *)
            FileHandleCount := GetFileHandleCount;
        end;
    {At 0.9.2, case for enumeration does not work.}
    case os_mode of
        osDOS:
            begin
                stackbottom:=cardinal(heap_brk);    {In DOS mode, heap_brk is
                                                     also the stack bottom.}
                ApplicationType := 1;   (* Running under DOS. *)
                IsConsole := true;
                DosEnvInit;
            end;
        osOS2:
            begin
                DosGetInfoBlocks (@TIB, @PIB);
                StackBottom := cardinal (TIB^.Stack);
                Environment := pointer (PIB^.Env);
                ApplicationType := PIB^.ProcType;
                IsConsole := ApplicationType <> 3;
            end;
        osDPMI:
            begin
                stackbottom:=0;     {Not sure how to get it, but seems to be
                                     always zero.}
                ApplicationType := 1;   (* Running under DOS. *)
                IsConsole := true;
                DosEnvInit;
            end;
    end;
    exitproc:=nil;

    {Initialize the heap.}
    initheap;

    { ... and exceptions }
    SysInitExceptions;

    { ... and I/O }
    SysInitStdIO;

    { no I/O-Error }
    inoutres:=0;

{$ifdef HASVARIANT}
    initvariantmanager;
{$endif HASVARIANT}

{$IFDEF DUMPGROW}
 {$IFDEF CONTHEAP}
    WriteLn ('Initial brk size is ', GetHeapSize);
    WriteLn ('Brk limit is ', BrkLimit);
 {$ENDIF CONTHEAP}
{$ENDIF DUMPGROW}
end.
{
  $Log$
  Revision 1.5  2003-06-26 17:12:29  yuri
  * pmbidi added
  * some cosmetic changes

  Revision 1.4  2003/03/23 23:11:17  hajny
    + emx target added

  Revision 1.3  2002/12/15 22:46:29  hajny
    * First_Meg fixed + Environment initialization under Dos

  Revision 1.2  2002/11/17 22:32:05  hajny
    * type corrections (longing x cardinal)

  Revision 1.1  2002/11/17 16:22:54  hajny
    + RTL for emx target

}
