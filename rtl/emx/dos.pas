{****************************************************************************


                         Free Pascal Runtime-Library
                              DOS unit for EMX
                   Copyright (c) 1997,1999-2000 by Daniel Mantione,
                   member of the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 ****************************************************************************}

unit dos;

{$ASMMODE ATT}

{***************************************************************************}

interface

{***************************************************************************}

{$PACKRECORDS 1}

uses    Strings, DosCalls;

Type
  {Search record which is used by findfirst and findnext:}
  searchrec=record
    case boolean of
    false: (handle:THandle;     {Used in os_OS2 mode}
            FStat:PFileFindBuf3;
            fill2:array[1..21-SizeOf(THandle)-SizeOf(pointer)] of byte;
            attr2:byte;
            time2:longint;
            size2:longint;
            name2:string);      {Filenames can be long in OS/2!}
    true:  (fill:array[1..21] of byte;
            attr:byte;
            time:longint;
            size:longint;
            name:string);       {Filenames can be long in OS/2!}
  end;

{$i dosh.inc}

        {Flags for the exec procedure:

        Starting the program:
        efwait:        Wait until program terminates.
        efno_wait:     Don't wait until the program terminates. Does not work
                       in dos, as DOS cannot multitask.
        efoverlay:     Terminate this program, then execute the requested
                       program. WARNING: Exit-procedures are not called!
        efdebug:       Debug program. Details are unknown.
        efsession:     Do not execute as child of this program. Use a seperate
                       session instead.
        efdetach:      Detached. Function unknown. Info wanted!
        efpm:          Run as presentation manager program.

 Not found info about execwinflags

        Determining the window state of the program:
        efdefault:     Run the pm program in it's default situation.
        efminimize:    Run the pm program minimized.
        efmaximize:    Run the pm program maximized.
        effullscreen:  Run the non-pm program fullscreen.
        efwindowed:    Run the non-pm program in a window.

}
const
    efWait    = 0;  (* Spawn child, wait until terminated *)
    efNo_Wait = 1;  (* Not implemented according to EMX documentation! *)
    efOverlay = 2;  (* Exec child, kill current process *)
    efDebug   = 3;  (* Debug child - use with ptrace syscall *)
    efSession = 4;  (* Run in a separate session *)
    efDetach  = 5;  (* Run detached *)
    efPM      = 6;  (* Run as a PM program *)

    efDefault    = 0;
    efMinimize   = $100;
    efMaximize   = $200;
    efFullScreen = $300;
    efWindowed   = $400;
    efBackground = $1000;
    efNoClose    = $2000;
    efNoSession  = $4000;
    efMoreFlags  = $8000; (* Needed if any flags > $FFFF are supplied *)
    efQuote      = $10000;
    efTilde      = $20000;
    efDebugDesc  = $40000;


{OS/2 specific functions}

function GetEnvPChar (EnvVar: string): PChar;


threadvar
(* For compatibility with VP/2, used for runflags in Exec procedure. *)
    ExecFlags: cardinal;



implementation

{$DEFINE HAS_INTR}
{$DEFINE HAS_SETVERIFY}
{$DEFINE HAS_GETVERIFY}

{$DEFINE FPC_FEXPAND_UNC} (* UNC paths are supported *)
{$DEFINE FPC_FEXPAND_DRIVES} (* Full paths begin with drive specification *)

const
    LFNSupport = true;

{$I dos.inc}


threadvar
  LastSR: SearchRec;

var
  EnvC: longint; external name '_envc';
  EnvP: ppchar; external name '_environ';

type
  TBA = array [1..SizeOf (SearchRec)] of byte;
  PBA = ^TBA;

const
  FindResvdMask = $00003737; {Allowed bits in attribute
                              specification for DosFindFirst call.}


{Import syscall to call it nicely from assembler procedures.}

procedure syscall;external name '___SYSCALL';


function fsearch(path:pathstr;dirlist:string):pathstr;

var i,p1:longint;
    newdir:pathstr;

{$ASMMODE INTEL}
function CheckFile (FN: ShortString):boolean; assembler;
asm
{$IFDEF REGCALL}
    mov edx, eax
{$ELSE REGCALL}
    mov edx, FN      { get pointer to string }
{$ENDIF REGCALL}
    inc edx          { avoid length byte     }
    mov ax, 4300h
    call syscall
    mov ax, 0
    jc @LCFstop
    test cx, 18h
    jnz @LCFstop
    inc ax
@LCFstop:
end ['eax', 'ecx', 'edx'];
{$ASMMODE ATT}

begin
{ check if the file specified exists }
    if CheckFile (Path + #0) then
        FSearch := Path
    else
        begin
            {No wildcards allowed in these things:}
            if (pos('?',path)<>0) or (pos('*',path)<>0) then
                fsearch:=''
            else
                begin
                    { allow slash as backslash }
                    DoDirSeparators(dirlist);
                    repeat
                        p1:=pos(';',dirlist);
                        if p1<>0 then
                            begin
                                newdir:=copy(dirlist,1,p1-1);
                                delete(dirlist,1,p1);
                            end
                        else
                            begin
                                newdir:=dirlist;
                                dirlist:='';
                            end;
                        if (newdir<>'') and
                         not (newdir[length(newdir)] in AllowDirectorySeparators+AllowDriveSeparators) then
                            newdir:=newdir+DirectorySeparator;
                        if CheckFile (NewDir + Path + #0) then
                            NewDir := NewDir + Path
                        else
                            NewDir := '';
                    until (DirList = '') or (NewDir <> '');
                    FSearch := NewDir;
                end;
        end;
end;


procedure GetFTime (var F; var Time: longint); assembler;
asm
    pushl %ebx
    {Load handle}
{$IFDEF REGCALL}
    movl %eax,%ebx
    pushl %edx
{$ELSE REGCALL}
    movl F,%ebx
{$ENDIF REGCALL}
    movl (%ebx),%ebx
    {Get date}
    movw $0x5700,%ax
    call syscall
    shll $16,%edx
    movw %cx,%dx
{$IFDEF REGCALL}
    popl %ebx
{$ELSE REGCALL}
    movl Time,%ebx
{$ENDIF REGCALL}
    movl %edx,(%ebx)
    movw %ax,DosError
    popl %ebx
end {['eax', 'ecx', 'edx']};


procedure SetFTime (var F; Time: longint);

var FStat: TFileStatus3;
    RC: cardinal;

begin
    if os_mode = osOS2 then
        begin
            RC := DosQueryFileInfo (FileRec (F).Handle, ilStandard, @FStat,
                                                               SizeOf (FStat));
            if RC = 0 then
                begin
                    FStat.DateLastAccess := Hi (Time);
                    FStat.DateLastWrite := Hi (Time);
                    FStat.TimeLastAccess := Lo (Time);
                    FStat.TimeLastWrite := Lo (Time);
                    RC := DosSetFileInfo (FileRec (F).Handle, ilStandard,
                                                       @FStat, SizeOf (FStat));
                end;
            DosError := integer (RC);
        end
    else
        asm
            pushl %ebx
            {Load handle}
            movl f,%ebx
            movl (%ebx),%ebx
            movl time,%ecx
            shldl $16,%ecx,%edx
            {Set date}
            movw $0x5701,%ax
            call syscall
            movw %ax,doserror
            popl %ebx
        end ['eax', 'ecx', 'edx'];
end;


procedure Intr (IntNo: byte; var Regs: Registers);

{Not recommended for EMX. Only works in DOS mode, not in OS/2 mode.}

begin
  if os_mode = osos2 then exit;
  asm
    jmp .Lstart
{    .data}
.Lint86:
    .byte        0xcd
.Lint86_vec:
    .byte        0x03
    jmp          .Lint86_retjmp

{    .text}
.Lstart:
    movb    intno,%al
    movb    %al,.Lint86_vec

{
    movl    10(%ebp),%eax
    incl    %eax
    incl    %eax
}
    movl    regs,%eax
    {Do not use first int}
    movl    4(%eax),%ebx
    movl    8(%eax),%ecx
    movl    12(%eax),%edx
    movl    16(%eax),%ebp
    movl    20(%eax),%esi
    movl    24(%eax),%edi
    movl    (%eax),%eax

    jmp     .Lint86
.Lint86_retjmp:
    pushf
    pushl   %ebp
    pushl   %eax
    movl    %esp,%ebp
    {Calc EBP new}
    addl    $12,%ebp
{
    movl    10(%ebp),%eax
    incl    %eax
    incl    %eax
}
    {Do not use first int}
    movl    regs,%eax

    popl    (%eax)
    movl    %ebx,4(%eax)
    movl    %ecx,8(%eax)
    movl    %edx,12(%eax)
    {Restore EBP}
    popl    %edx
    movl    %edx,16(%eax)
    movl    %esi,20(%eax)
    movl    %edi,24(%eax)
    {Ignore ES and DS}
    popl    %ebx            {Flags.}
    movl    %ebx,32(%eax)
    {FS and GS too}
  end ['eax','ebx','ecx','edx','esi','edi'];
end;


procedure exec(const path:pathstr;const comline:comstr);

{Execute a program.}

type    bytearray=array[0..8191] of byte;
        Pbytearray=^bytearray;

        execstruc=packed record
            argofs : pointer;    { pointer to arguments (offset)   }
            envofs : pointer;    { pointer to environment (offset) }
            nameofs: pointer;    { pointer to file name (offset)   }
            argseg : word;       { pointer to arguments (selector) }
            envseg : word;       { pointer to environment (selector}
            nameseg: word;       { pointer to file name (selector) }
            numarg : word;       { number of arguments             }
            sizearg : word;      { size of arguments               }
            numenv :  word;      { number of env strings           }
            sizeenv:word;        { size of environment             }
            mode:word;           { mode word                       }
        end;

var args:Pbytearray;
    env:Pbytearray;
    Path2:PByteArray;
    i,argsize:word;
    es:execstruc;
    esadr:pointer;
    d:dirstr;
    n:namestr;
    e:extstr;
    p : ppchar;
    j : integer;
const
    ArgsSize = 2048; (* Amount of memory reserved for arguments in bytes. *)

begin
    getmem(args,ArgsSize);
    GetMem(env, envc*sizeof(pchar)+16384);
    GetMem (Path2, 260);
    {Now setup the arguments. The first argument should be the program
     name without directory and extension.}
    fsplit(path,d,n,e);
    es.numarg:=1;
    args^[0]:=$80;
    argsize:=1;
    for i:=1 to length(n) do
        begin
            args^[argsize]:=byte(n[i]);
            inc(argsize);
        end;
    args^[argsize]:=0;
    inc(argsize);
    {Now do the real arguments.}
    i:=1;
    while i<=length(comline) do
        begin
            if comline[i]<>' ' then
                begin
                    {Commandline argument found. Copy it.}
                    inc(es.numarg);
                    args^[argsize]:=$80;
                    inc(argsize);
                    while (i<=length(comline)) and (comline[i]<>' ') do
                        begin
                            args^[argsize]:=byte(comline[i]);
                            inc(argsize);
                            inc(i);
                        end;
                    args^[argsize]:=0;
                    inc(argsize);
                end;
            inc(i);
        end;
    args^[argsize]:=0;
    inc(argsize);

    {Commandline ready, now build the environment.

     Oh boy, I always had the opinion that executing a program under Dos
     was a hard job!}

    asm
        movl env,%edi       {Setup destination pointer.}
        movl envc,%ecx      {Load number of arguments in edx.}
        movl envp,%esi      {Load env. strings.}
        xorl %edx,%edx      {Count environment size.}
.Lexa1:
        lodsl               {Load a Pchar.}
        xchgl %eax,%ebx
.Lexa2:
        movb (%ebx),%al     {Load a byte.}
        incl %ebx           {Point to next byte.}
        stosb               {Store it.}
        incl %edx           {Increase counter.}
        cmpb $0,%al         {Ready ?.}
        jne .Lexa2
        loop .Lexa1           {Next argument.}
        stosb               {Store an extra 0 to finish. (AL is now 0).}
        incl %edx
        movw %dx,ES.SizeEnv    {Store environment size.}
    end ['eax','ebx','ecx','edx','esi','edi'];

    {Environment ready, now set-up exec structure.}
    es.argofs:=args;
    es.envofs:=env;
    es.numenv:=envc;
    Move (Path [1], Path2^, Length (Path));
    Path2^ [Length (Path)] := 0;
    es.nameofs := Path2;
    asm
        movw %ss,es.argseg
        movw %ss,es.envseg
        movw %ss,es.nameseg
    end;
    es.sizearg:=argsize;
    es.mode := word (ExecFlags);

    {Now exec the program.}
    asm
        leal es,%edx
        movw $0x7f06,%ax
        call syscall
        movl $0,%edi
        jnc .Lexprg1
        xchgl %eax,%edi
        xorl %eax,%eax
    .Lexprg1:
        movw %di,doserror
        movl %eax, LastDosExitCode
    end ['eax', 'ebx', 'ecx', 'edx', 'esi', 'edi'];

    FreeMem (Path2, 260);
    FreeMem(env, envc*sizeof(pchar)+16384);
    freemem(args,ArgsSize);
    {Phew! That's it. This was the most sophisticated procedure to call
     a system function I ever wrote!}
end;


function dosversion:word;assembler;

{Returns DOS version in DOS and OS/2 version in OS/2}
asm
    movb $0x30,%ah
    call syscall
end ['eax'];


procedure GetDate (var Year, Month, MDay, WDay: word);

begin
    asm
        movb $0x2a, %ah
        call syscall
        xorb %ah, %ah
        movl WDay, %edi
        stosw
        movl MDay, %edi
        movb %dl, %al
        stosw
        movl Month, %edi
        movb %dh, %al
        stosw
        movl Year, %edi
        xchgw %ecx, %eax
        stosw
    end ['eax', 'ecx', 'edx'];
end;


{$asmmode intel}
procedure SetDate (Year, Month, Day: word);
var DT: TDateTime;
begin
    if os_mode = osOS2 then
        begin
            DosGetDateTime (DT);
            DT.Year := Year;
            DT.Month := byte (Month);
            DT.Day := byte (Day);
            DosSetDateTime (DT);
        end
    else
        asm
            mov  cx, Year
            mov  dh, byte ptr Month
            mov  dl, byte ptr Day
            mov  ah, 2Bh
            call syscall
        end ['eax', 'ecx', 'edx'];
end;
{$asmmode att}


procedure GetTime (var Hour, Minute, Second, Sec100: word);
{$IFDEF REGCALL}
begin
{$ELSE REGCALL}
                                                            assembler;
{$ENDIF REGCALL}
asm
    movb $0x2c, %ah
    call syscall
    xorb %ah, %ah
    movl Sec100, %edi
    movb %dl, %al
    stosw
    movl Second, %edi
    movb %dh,%al
    stosw
    movl Minute, %edi
    movb %cl,%al
    stosw
    movl Hour, %edi
    movb %ch,%al
    stosw
{$IFDEF REGCALL}
  end ['eax', 'ecx', 'edx'];
end;
{$ELSE REGCALL}
end {['eax', 'ecx', 'edx']};
{$ENDIF REGCALL}


{$asmmode intel}
procedure SetTime (Hour, Minute, Second, Sec100: word);
var DT: TDateTime;
begin
    if os_mode = osOS2 then
begin
  DosGetDateTime (DT);
  DT.Hour := byte (Hour);
  DT.Minute := byte (Minute);
  DT.Second := byte (Second);
  DT.Sec100 := byte (Sec100);
  DosSetDateTime (DT);
        end
    else
        asm
            mov  ch, byte ptr Hour
            mov  cl, byte ptr Minute
            mov  dh, byte ptr Second
            mov  dl, byte ptr Sec100
            mov  ah, 2Dh
            call syscall
        end ['eax', 'ecx', 'edx'];
end;

{$asmmode att}


procedure getverify(var verify:boolean);

begin
  {! Do not use in OS/2.}
  if os_mode in [osDOS,osDPMI] then
      asm
         movb $0x54,%ah
         call syscall
         movl verify,%edi
         stosb
      end ['eax', 'edi']
  else
  verify := true;
end;

procedure setverify(verify:boolean);

begin
  {! Do not use in OS/2!}
  if os_mode in [osDOS,osDPMI] then
    asm
        movb verify,%al
        movb $0x2e,%ah
        call syscall
    end ['eax'];
 end;


function DiskFree (Drive: byte): int64;

var FI: TFSinfo;
    RC: cardinal;

begin
    if (os_mode = osDOS) or (os_mode = osDPMI) then
    {Function 36 is not supported in OS/2.}
        asm
            pushl %ebx
            movb Drive,%dl
            movb $0x36,%ah
            call syscall
            cmpw $-1,%ax
            je .LDISKFREE1
            mulw %cx
            mulw %bx
            shll $16,%edx
            movw %ax,%dx
            movl $0,%eax
            xchgl %edx,%eax
            jmp .LDISKFREE2
         .LDISKFREE1:
            cltd
         .LDISKFREE2:
            popl %ebx
            leave
            ret
        end ['eax', 'ecx', 'edx']
    else
        {In OS/2, we use the filesystem information.}
        begin
  RC := DosQueryFSInfo (Drive, 1, FI, SizeOf (FI));
  if RC = 0 then
      DiskFree := int64 (FI.Free_Clusters) *
         int64 (FI.Sectors_Per_Cluster) * int64 (FI.Bytes_Per_Sector)
  else
      DiskFree := -1;
end;
end;

function DiskSize (Drive: byte): int64;

var FI: TFSinfo;
    RC: cardinal;

begin
    if (os_mode = osDOS) or (os_mode = osDPMI) then
        {Function 36 is not supported in OS/2.}
        asm
            pushl %ebx
            movb Drive,%dl
            movb $0x36,%ah
            call syscall
            movw %dx,%bx
            cmpw $-1,%ax
            je .LDISKSIZE1
            mulw %cx
            mulw %bx
            shll $16,%edx
            movw %ax,%dx
            movl $0,%eax
            xchgl %edx,%eax
            jmp .LDISKSIZE2
        .LDISKSIZE1:
            cltd
        .LDISKSIZE2:
            popl %ebx
            leave
            ret
        end ['eax', 'ecx', 'edx']
    else
        {In OS/2, we use the filesystem information.}
begin
  RC := DosQueryFSinfo (Drive, 1, FI, SizeOf (FI));
  if RC = 0 then
      DiskSize := int64 (FI.Total_Clusters) *
         int64 (FI.Sectors_Per_Cluster) * int64 (FI.Bytes_Per_Sector)
  else
      DiskSize := -1;
end;
end;


procedure SearchRec2DosSearchRec (var F: SearchRec);

const   NameSize = 255;

var L, I: longint;

begin
    if os_mode <> osOS2 then
    begin
        I := 1;
        while (I <= SizeOf (LastSR))
                           and (PBA (@F)^ [I] = PBA (@LastSR)^ [I]) do Inc (I);
{ Raise "Invalid file handle" RTE if nested FindFirst calls were used. }
        if I <= SizeOf (LastSR) then RunError (6);
        l:=length(f.name);
        for i:=1 to namesize do
            f.name[i-1]:=f.name[i];
        f.name[l]:=#0;
    end;
end;

procedure DosSearchRec2SearchRec (var F: SearchRec);

const NameSize=255;

var L, I: longint;

type    TRec = record
    T, D: word;
  end;

begin
    if os_mode = osOS2 then with F do
    begin
        Name := FStat^.Name;
        Size := FStat^.FileSize;
        Attr := byte(FStat^.AttrFile and $FF);
        TRec (Time).T := FStat^.TimeLastWrite;
        TRec (Time).D := FStat^.DateLastWrite;
    end else
    begin
        for i:=0 to namesize do
            if f.name[i]=#0 then
                begin
                    l:=i;
                    break;
                end;
        for i:=namesize-1 downto 0 do
            f.name[i+1]:=f.name[i];
        f.name[0]:=char(l);
        Move (F, LastSR, SizeOf (LastSR));
    end;
end;


    procedure _findfirst(path:pchar;attr:word;var f:searchrec);

    begin
        asm
            pushl %esi
            movl path,%edx
            movw attr,%cx
            {No need to set DTA in EMX. Just give a pointer in ESI.}
            movl f,%esi
            movb $0x4e,%ah
            call syscall
            jnc .LFF
            movw %ax,doserror
        .LFF:
            popl %esi
        end ['eax', 'ecx', 'edx'];
    end;


procedure FindFirst (const Path: PathStr; Attr: word; var F: SearchRec);


var path0: array[0..255] of char;
    Count: cardinal;

begin
  {No error.}
  DosError := 0;
    if os_mode = osOS2 then
    begin
      New (F.FStat);
      F.Handle := THandle ($FFFFFFFF);
      Count := 1;
      DosError := integer (DosFindFirst (Path, F.Handle,
                     Attr and FindResvdMask, F.FStat, SizeOf (F.FStat^),
                                                         Count, ilStandard));
      if (DosError = 0) and (Count = 0) then DosError := 18;
    end else
    begin
        strPcopy(path0,path);
        _findfirst(path0,attr,f);
    end;
    DosSearchRec2SearchRec (F);
end;

    procedure _findnext(var f : searchrec);

    begin
        asm
            pushl %esi
            movl f,%esi
            movb $0x4f,%ah
            call syscall
            jnc .LFN
            movw %ax,doserror
        .LFN:
            popl %esi
        end ['eax'];
    end;


procedure FindNext (var F: SearchRec);
var Count: cardinal;

begin
    {No error}
    DosError := 0;
    SearchRec2DosSearchRec (F);
    if os_mode = osOS2 then
    begin
        Count := 1;
        DosError := integer (DosFindNext (F.Handle, F.FStat, SizeOf (F.FStat^),
                                                                       Count));
        if (DosError = 0) and (Count = 0) then DosError := 18;
    end else _findnext (F);
    DosSearchRec2SearchRec (F);
end;


procedure FindClose (var F: SearchRec);
begin
    if os_mode = osOS2 then
    begin
  if F.Handle <> THandle ($FFFFFFFF) then DosError := DosFindClose (F.Handle);
  Dispose (F.FStat);
end;
end;


function envcount:longint;assembler;
asm
    movl envc,%eax
end ['EAX'];


function envstr(index : longint) : string;

var hp:Pchar;

begin
    if (index<=0) or (index>envcount) then
        begin
            envstr:='';
            exit;
        end;
    hp:=EnvP[index-1];
    envstr:=strpas(hp);
end;


function GetEnvPChar (EnvVar: string): PChar;
(* The assembler version is more than three times as fast as Pascal. *)
var
 P: PChar;
begin
 EnvVar := UpCase (EnvVar);
{$ASMMODE INTEL}
 asm
  cld
  mov edi, Environment
  lea esi, EnvVar
  xor eax, eax
  lodsb
@NewVar:
  cmp byte ptr [edi], 0
  jz @Stop
  push eax        { eax contains length of searched variable name }
  push esi        { esi points to the beginning of the variable name }
  mov ecx, -1     { our character ('=' - see below) _must_ be found }
  mov edx, edi    { pointer to beginning of variable name saved in edx }
  mov al, '='     { searching until '=' (end of variable name) }
  repne
  scasb           { scan until '=' not found }
  neg ecx         { what was the name length? }
  dec ecx         { corrected }
  dec ecx         { exclude the '=' character }
  pop esi         { restore pointer to beginning of variable name }
  pop eax         { restore length of searched variable name }
  push eax        { and save both of them again for later use }
  push esi
  cmp ecx, eax    { compare length of searched variable name with name }
  jnz @NotEqual   { ... of currently found variable, jump if different }
  xchg edx, edi   { pointer to current variable name restored in edi }
  repe
  cmpsb           { compare till the end of variable name }
  xchg edx, edi   { pointer to beginning of variable contents in edi }
  jz @Equal       { finish if they're equal }
@NotEqual:
  xor eax, eax    { look for 00h }
  mov ecx, -1     { it _must_ be found }
  repne
  scasb           { scan until found }
  pop esi         { restore pointer to beginning of variable name }
  pop eax         { restore length of searched variable name }
  jmp @NewVar     { ... or continue with new variable otherwise }
@Stop:
  xor eax, eax
  mov P, eax      { Not found - return nil }
  jmp @End
@Equal:
  pop esi         { restore the stack position }
  pop eax
  mov P, edi      { place pointer to variable contents in P }
@End:
 end ['eax','ecx','edx','esi','edi'];
 GetEnvPChar := P;
end;
{$ASMMODE ATT}


function GetEnv (EnvVar: string): string;
begin
 GetEnv := StrPas (GetEnvPChar (EnvVar));
end;


procedure getfattr(var f;var attr : word);
 { Under EMX, this routine requires     }
 { the expanded path specification      }
 { otherwise it will not function       }
 { properly (CEC)                       }
var
 path:  pathstr;
 buffer:array[0..255] of char;
begin
  DosError := 0;
{$ifdef FPC_ANSI_TEXTFILEREC}
  path:=filerec(f).Name;
{$else}
  path:=ToSingleByteFileSystemEncodedFileName(filerec(f).Name);
{$endif}
  { Takes care of slash and backslash support }
  path:=FExpand(path);
  move(path[1],buffer,length(path));
  buffer[length(path)]:=#0;
 asm
    pushl %ebx
    movw $0x4300,%ax
    leal buffer,%edx
    call syscall
    jnc  .Lnoerror         { is there an error ? }
    movw %ax,doserror
  .Lnoerror:
    movl attr,%ebx
    movw %cx,(%ebx)
    popl %ebx
 end ['eax', 'ecx', 'edx'];
end;


procedure setfattr(var f;attr : word);
 { Under EMX, this routine requires     }
 { the expanded path specification      }
 { otherwise it will not function       }
 { properly (CEC)                       }
var
 path:  pathstr;
 buffer:array[0..255] of char;
begin
  DosError := 0;
{$ifdef FPC_ANSI_TEXTFILEREC}
  path:=filerec(f).Name;
{$else}
  path:=ToSingleByteFileSystemEncodedFileName(filerec(f).Name);
{$endif}
  { Takes care of slash and backslash support }
  path:=FExpand(path);
  move(path[1],buffer,length(path));
  buffer[length(path)]:=#0;
   asm
     movw $0x4301,%ax
     leal buffer,%edx
     movw attr,%cx
     call syscall
     jnc  .Lnoerror
     movw %ax,doserror
   .Lnoerror:
  end ['eax', 'ecx', 'edx'];
end;



procedure InitEnvironment;
var
 cnt : integer;
 ptr : pchar;
 base : pchar;
 i: integer;
 PIB: PProcessInfoBlock;
 TIB: PThreadInfoBlock;
begin
  { We need to setup the environment     }
  { only in the case of OS/2             }
  { otherwise everything is in the stack }
  if os_Mode in [OsDOS,osDPMI] then
    exit;
  cnt := 0;
  { count number of environment pointers }
  DosGetInfoBlocks (PPThreadInfoBlock (@TIB), PPProcessInfoBlock (@PIB));
  ptr := pchar(PIB^.env);
  { stringz,stringz...,#0 }
  i := 0;
  repeat
    repeat
     (inc(i));
    until (ptr[i] = #0);
    inc(i);
    { here, it may be a double null, end of environment }
    if ptr[i] <> #0 then
       inc(cnt);
  until (ptr[i] = #0);
  { save environment count }
  envc := cnt;
  { got count of environment strings }
  GetMem(envp, cnt*sizeof(pchar)+16384);
  cnt := 0;
  ptr := pchar(PIB^.env);
  i:=0;
  repeat
    envp[cnt] := ptr;
    Inc(cnt);
    { go to next string ... }
    repeat
      inc(ptr);
    until (ptr^ = #0);
    inc(ptr);
  until ptr^ = #0;
  envp[cnt] := #0;
end;


procedure DoneEnvironment;
begin
  { it is allocated on the stack for DOS/DPMI }
  if os_mode = osOs2 then
     FreeMem(envp, envc*sizeof(pchar)+16384);
end;

var
  oldexit : pointer;


{******************************************************************************
                             --- Not Supported ---
******************************************************************************}



begin
 oldexit:=exitproc;
 exitproc:=@doneenvironment;
 InitEnvironment;
 LastDosExitCode := 0;
 ExecFlags := 0;
end.
