{****************************************************************************

    $Id$

                         Free Pascal Runtime-Library
                              DOS unit for OS/2
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

uses    strings;

const   {Bit masks for CPU flags.}
        fcarry      = $0001;
        fparity     = $0004;
        fauxiliary  = $0010;
        fzero       = $0040;
        fsign       = $0080;
        foverflow   = $0800;

        {Bit masks for file attributes.}
        readonly    = $01;
        hidden      = $02;
        sysfile     = $04;
        volumeid    = $08;
        directory   = $10;
        archive     = $20;
        anyfile     = $3F;

        fmclosed    = $D7B0;
        fminput     = $D7B1;
        fmoutput    = $D7B2;
        fminout     = $D7B3;

type    {Some string types:}
        comstr=string;              {Filenames can be long in OS/2.}
        pathstr=string;             {String for pathnames.}
        dirstr=string;              {String for a directory}
        namestr=string;             {String for a filename.}
        extstr=string[40];          {String for an extension. Can be 253
                                     characters long, in theory, but let's
                                     say fourty will be enough.}

        {Search record which is used by findfirst and findnext:}
        searchrec=record
            case boolean of
             false: (handle:longint;     {Used in os_OS2 mode}
                     fill2:array[1..21-SizeOf(longint)] of byte;
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

{$i filerec.inc}
{$i textrec.inc}

        {Data structure for the registers needed by msdos and intr:}
       registers=record
            case i:integer of
                0:(ax,f1,bx,f2,cx,f3,dx,f4,bp,f5,si,f51,di,f6,ds,f7,es,
                   f8,flags,fs,gs:word);
                1:(al,ah,f9,f10,bl,bh,f11,f12,cl,ch,f13,f14,dl,dh:byte);
                2:(eax,ebx,ecx,edx,ebp,esi,edi:longint);
            end;

        {Record for date and time:}
        datetime=record
            year,month,day,hour,min,sec:word;
        end;

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

        Determining the window state of the program:
        efdefault:     Run the pm program in it's default situation.
        efminimize:    Run the pm program minimized.
        efmaximize:    Run the pm program maximized.
        effullscreen:  Run the non-pm program fullscreen.
        efwindowed:    Run the non-pm program in a window.

        Other options are not implemented defined because lack of
        knowledge about what they do.}

        type    execrunflags=(efwait,efno_wait,efoverlay,efdebug,efsession,
                              efdetach,efpm);
                execwinflags=(efdefault,efminimize,efmaximize,effullscreen,
                              efwindowed);

const
(* For compatibility with VP/2, used for runflags in Exec procedure. *)
    ExecFlags: cardinal = ord (efwait);

var doserror:integer;
    dosexitcode:word;

procedure getdate(var year,month,day,dayofweek:word);
procedure gettime(var hour,minute,second,sec100:word);
function dosversion:word;
procedure setdate(year,month,day:word);
procedure settime(hour,minute,second,sec100:word);
procedure getcbreak(var breakvalue:boolean);
procedure setcbreak(breakvalue:boolean);
procedure getverify(var verify:boolean);
procedure setverify(verify : boolean);

function DiskFree (Drive: byte) : int64;
function DiskSize (Drive: byte) : int64;

procedure findfirst(const path:pathstr;attr:word;var f:searchRec);
procedure findnext(var f:searchRec);
procedure findclose(var f:searchRec);

{Is a dummy:}
procedure swapvectors;

{Not supported:
procedure getintvec(intno:byte;var vector:pointer);
procedure setintvec(intno:byte;vector:pointer);
procedure keep(exitcode:word);
}
procedure msdos(var regs:registers);
procedure intr(intno : byte;var regs:registers);

procedure getfattr(var f;var attr:word);
procedure setfattr(var f;attr:word);

function fsearch(path:pathstr;dirlist:string):pathstr;
procedure getftime(var f;var time:longint);
procedure setftime(var f;time:longint);
procedure packtime (var d:datetime; var time:longint);
procedure unpacktime (time:longint; var d:datetime);
function fexpand(const path:pathstr):pathstr;
procedure fsplit(path:pathstr;var dir:dirstr;var name:namestr;
                 var ext:extstr);
procedure exec(const path:pathstr;const comline:comstr);
function exec(path:pathstr;runflags:execrunflags;winflags:execwinflags;
              const comline:comstr):longint;
function envcount:longint;
function envstr(index:longint) : string;
function getenv(const envvar:string): string;

implementation

uses    DosCalls;

var     LastSR: SearchRec;

type    TBA = array [1..SizeOf (SearchRec)] of byte;
        PBA = ^TBA;

{Import syscall to call it nicely from assembler procedures.}

procedure syscall;external name '___SYSCALL';


function fsearch(path:pathstr;dirlist:string):pathstr;

var i,p1:longint;
    newdir:pathstr;

{$ASMMODE INTEL}
function CheckFile (FN: ShortString):boolean; assembler;
asm
    mov ax, 4300h
    mov edx, FN
    inc edx
    call syscall
    mov ax, 0
    jc @LCFstop
    test cx, 18h
    jnz @LCFstop
    inc ax
@LCFstop:
end;
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
                    for i:=1 to length(dirlist) do
                       if dirlist[i]='/' then dirlist[i]:='\';
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
                         not (newdir[length(newdir)] in ['\',':']) then
                            newdir:=newdir+'\';
                        if CheckFile (NewDir + Path + #0) then
                            NewDir := NewDir + Path
                        else
                            NewDir := '';
                    until (DirList = '') or (NewDir <> '');
                    FSearch := NewDir;
                end;
        end;
end;

procedure getftime(var f;var time:longint);

begin
    asm
        {Load handle}
        movl f,%ebx
        movw (%ebx),%bx
        {Get date}
        movw $0x5700,%ax
        call syscall
        shll $16,%edx
        movw %cx,%dx
        movl time,%ebx
        movl %edx,(%ebx)
        xorb %ah,%ah
        movw %ax,doserror
    end;
end;

procedure SetFTime (var F; Time: longint);

var FStat: PFileStatus0;
    RC: longint;

begin
    if os_mode = osOS2 then
        begin
            New (FStat);
            RC := DosQueryFileInfo (TextRec (F).Handle, ilStandard, FStat,
                                                              SizeOf (FStat^));
            if RC = 0 then
                begin
                    FStat^.DateLastAccess := Hi (Time);
                    FStat^.DateLastWrite := Hi (Time);
                    FStat^.TimeLastAccess := Lo (Time);
                    FStat^.TimeLastWrite := Lo (Time);
                    RC := DosSetFileInfo (TextRec (F).Handle, ilStandard,
                                                       FStat, SizeOf (FStat^));
                end;
            Dispose (FStat);
        end
    else
        asm
            {Load handle}
            movl f,%ebx
            movw (%ebx),%bx
            movl time,%ecx
            shldl $16,%ecx,%edx
            {Set date}
            movw $0x5701,%ax
            call syscall
            xorb %ah,%ah
            movw %ax,doserror
        end;
end;

procedure msdos(var regs:registers);

{Not recommended for EMX. Only works in DOS mode, not in OS/2 mode.}

begin
    intr($21,regs);
end;

{$ASMMODE DIRECT}

procedure intr(intno:byte;var regs:registers);

{Not recommended for EMX. Only works in DOS mode, not in OS/2 mode.}

begin
    asm
        .data
    int86:
        .byte        0xcd
    int86_vec:
        .byte        0x03
        jmp        int86_retjmp

        .text
        movl        8(%ebp),%eax
        movb        %al,int86_vec

        movl        10(%ebp),%eax
        {Do not use first int}
        incl        %eax
        incl        %eax

        movl        4(%eax),%ebx
        movl        8(%eax),%ecx
        movl        12(%eax),%edx
        movl        16(%eax),%ebp
        movl        20(%eax),%esi
        movl        24(%eax),%edi
        movl        (%eax),%eax

        jmp        int86
    int86_retjmp:
        pushf
        pushl   %ebp
        pushl       %eax
        movl        %esp,%ebp
        {Calc EBP new}
        addl        $12,%ebp
        movl        10(%ebp),%eax
        {Do not use first int}
        incl        %eax
        incl        %eax

        popl        (%eax)
        movl        %ebx,4(%eax)
        movl        %ecx,8(%eax)
        movl        %edx,12(%eax)
        {Restore EBP}
        popl    %edx
        movl    %edx,16(%eax)
        movl        %esi,20(%eax)
        movl        %edi,24(%eax)
        {Ignore ES and DS}
        popl        %ebx            {Flags.}
        movl        %ebx,32(%eax)
        {FS and GS too}
    end;
end;

{$ASMMODE ATT}

procedure exec(const path:pathstr;const comline:comstr);

{Execute a program.}

begin
    dosexitcode:=exec(path,execrunflags(ExecFlags),efdefault,comline);
end;

function exec(path:pathstr;runflags:execrunflags;winflags:execwinflags;
              const comline:comstr):longint;

{Execute a program. More suitable for OS/2 than the exec above.}

{512 bytes should be enough to contain the command-line.}

type    bytearray=array[0..8191] of byte;
        Pbytearray=^bytearray;

        execstruc=record
            argofs,envofs,nameofs:pointer;
            argseg,envseg,nameseg:word;
            numarg,sizearg,
            numenv,sizeenv:word;
            mode1,mode2:byte;
        end;

var args:Pbytearray;
    env:Pbytearray;
    i,j:word;
    es:execstruc;
    esadr:pointer;
    d:dirstr;
    n:namestr;
    e:extstr;

begin
    getmem(args,512);
    getmem(env,8192);
    j:=1;

    {Now setup the arguments. The first argument should be the program
     name without directory and extension.}
    fsplit(path,d,n,e);
    es.numarg:=1;
    args^[0]:=$80;
    for i:=1 to length(n) do
        begin
            args^[j]:=byte(n[i]);
            inc(j);
        end;
    args^[j]:=0;
    inc(j);
    {Now do the real arguments.}
    i:=1;
    while i<=length(comline) do
        begin
            if comline[i]<>' ' then
                begin
                    {Commandline argument found. Copy it.}
                    inc(es.numarg);
                    args^[j]:=$80;
                    inc(j);
                    while (i<=length(comline)) and (comline[i]<>' ') do
                        begin
                            args^[j]:=byte(comline[i]);
                            inc(j);
                            inc(i);
                        end;
                    args^[j]:=0;
                    inc(j);
                end;
            inc(i);
        end;
    args^[j]:=0;
    inc(j);

    {Commandline ready, now build the environment.

     Oh boy, I always had the opinion that executing a program under Dos
     was a hard job!}

    {$ASMMODE DIRECT}

    asm
        movl env,%edi       {Setup destination pointer.}
        movl _envc,%ecx     {Load number of arguments in edx.}
        movl _environ,%esi  {Load env. strings.}
        xorl %edx,%edx      {Count environment size.}
    exa1:
        lodsl               {Load a Pchar.}
        xchgl %eax,%ebx
    exa2:
        movb (%ebx),%al     {Load a byte.}
        incl %ebx           {Point to next byte.}
        stosb               {Store it.}
        incl %edx           {Increase counter.}
        cmpb $0,%al         {Ready ?.}
        jne exa2
        loop exa1           {Next argument.}
        stosb               {Store an extra 0 to finish. (AL is now 0).}
        incl %edx
        movl %edx,(24)es    {Store environment size.}
    end;

    {$ASMMODE ATT}

    {Environment ready, now set-up exec structure.}
    es.argofs:=args;
    es.envofs:=env;
    asm
        leal path,%esi
        lodsb
        movzbl %al,%eax
        addl %eax,%esi
        movb $0,(%esi)
    end;
    es.nameofs:=pointer(longint(@path)+1);
    asm
        movw %ss,es.argseg
        movw %ss,es.envseg
        movw %ss,es.nameseg
    end;
    es.sizearg:=j;
    es.numenv:=0;
    {Typecasting of sets in FPC is a bit hard.}
    es.mode1:=byte(runflags);
    es.mode2:=byte(winflags);

    {Now exec the program.}
    asm
        leal es,%edx
        mov $0x7f06,%ax
        call syscall
        xorl %edi,%edi
        jnc .Lexprg1
        xchgl %eax,%edi
        xorl %eax,%eax
        decl %eax
    .Lexprg1:
        movw %di,doserror
        movl %eax,__RESULT
    end;

    freemem(args,512);
    freemem(env,8192);
    {Phew! That's it. This was the most sophisticated procedure to call
     a system function I ever wrote!}
end;

function dosversion:word;assembler;

{Returns DOS version in DOS and OS/2 version in OS/2}
asm
    movb $0x30,%ah
    call syscall
end;

procedure GetDate (var Year, Month, Day, DayOfWeek: word);

begin
    asm
        movb $0x2a, %ah
        call syscall
        xorb %ah, %ah
        movl DayOfWeek, %edi
        stosw
        movl Day, %edi
        movb %dl, %al
        stosw
        movl Month, %edi
        movb %dh, %al
        stosw
        movl Year, %edi
        xchgw %ecx, %eax
        stosw
    end;
end;

{$asmmode intel}

procedure SetDate (Year, Month, Day: word);
var DT: TDateTime;
begin
    if os_mode = osOS2 then
        begin
            DosGetDateTime (DT);
            DT.Year := Year;
            DT.Month := Month;
            DT.Day := Day;
            DosSetDateTime (DT);
        end
    else
        asm
            mov  cx, Year
            mov  dh, byte ptr Month
            mov  dl, byte ptr Day
            mov  ah, $2b
            call syscall
        end;
end;

{$asmmode att}

procedure GetTime (var Hour, Minute, Second, Sec100: word); assembler;
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
end;

{$asmmode intel}
procedure SetTime (Hour, Minute, Second, Sec100: word);
var DT: TDateTime;
begin
    if os_mode = osOS2 then
        begin
            DosGetDateTime (DT);
            DT.Hour := Hour;
            DT.Minute := Minute;
            DT.Second := Second;
            DT.Sec100 := Sec100;
            DosSetDateTime (DT);
        end
    else
        asm
            mov  ch, byte ptr Hour
            mov  cl, byte ptr Minute
            mov  dh, byte ptr Second
            mov  dl, byte ptr Sec100
            mov  ah, $2d
            call syscall
        end;
end;

{$asmmode att}

procedure getcbreak(var breakvalue:boolean);

begin
     {! Do not use in OS/2. Also not recommended in DOS. Use
        signal handling instead.}
    asm
        movw $0x3300,%ax
        call syscall
        movl 8(%ebp),%eax
        movb %dl,(%eax)
    end;
end;

procedure setcbreak(breakvalue:boolean);

begin
    {! Do not use in OS/2. Also not recommended in DOS. Use
       signal handling instead.}
    asm
        movb 8(%ebp),%dl
        movw $0x3301,%ax
        call syscall
    end;
end;

procedure getverify(var verify:boolean);

begin
    {! Do not use in OS/2.}
    asm
        movb $0x54,%ah
        call syscall
        movl 8(%ebp),%edi
        stosb
    end;
end;

procedure setverify(verify:boolean);

begin
    {! Do not use in OS/2.}
    asm
        movb 8(%ebp),%al
        movb $0x2e,%ah
        call syscall
    end;
end;


function DiskFree (Drive: byte): int64;

var FI: TFSinfo;
    RC: longint;

begin
    if (os_mode = osDOS) or (os_mode = osDPMI) then
    {Function 36 is not supported in OS/2.}
        asm
            movb 8(%ebp),%dl
            movb $0x36,%ah
            call syscall
            cmpw $-1,%ax
            je .LDISKFREE1
            mulw %cx
            mulw %bx
            shll $16,%edx
            movw %ax,%dx
            xchgl %edx,%eax
            leave
            ret
         .LDISKFREE1:
            cltd
            leave
            ret
        end
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
    RC: longint;

begin
    if (os_mode = osDOS) or (os_mode = osDPMI) then
        {Function 36 is not supported in OS/2.}
        asm
            movb 8(%ebp),%dl
            movb $0x36,%ah
            call syscall
            movw %dx,%bx
            cmpw $-1,%ax
            je .LDISKSIZE1
            mulw %cx
            mulw %bx
            shll $16,%edx
            movw %ax,%dx
            xchgl %edx,%eax
            leave
            ret
        .LDISKSIZE1:
            cltd
            leave
            ret
        end
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

procedure DosSearchRec2SearchRec (var F: SearchRec; FStat: PFileFindBuf3);

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
        Attr := FStat^.AttrFile;
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

procedure FindFirst (const Path: PathStr; Attr: word; var F: SearchRec);

    procedure _findfirst(path:pchar;attr:word;var f:searchrec);

    begin
        asm
            movl 12(%esp),%edx
            movw 16(%esp),%cx
            {No need to set DTA in EMX. Just give a pointer in ESI.}
            movl 18(%ebp),%esi
            movb $0x4e,%ah
            call syscall
            jnc .LFF
            movw %ax,doserror
        .LFF:
        end;
    end;

const
    FStat: PFileFindBuf3 = nil;

var path0: array[0..255] of char;
    Count: longint;

begin
    {No error.}
    DosError := 0;
    if os_mode = osOS2 then
    begin
        New (FStat);
        F.Handle := $FFFFFFFF;
        Count := 1;
        DosError := DosFindFirst (Path, F.Handle, Attr, FStat,
                                           SizeOf (FStat^), Count, ilStandard);
        if (DosError = 0) and (Count = 0) then DosError := 18;
    end else
    begin
        strPcopy(path0,path);
        _findfirst(path0,attr,f);
    end;
    DosSearchRec2SearchRec (F, FStat);
    if os_mode = osOS2 then Dispose (FStat);
end;

procedure FindNext (var F: SearchRec);
var FStat: PFileFindBuf3;
    Count: longint;

    procedure _findnext(var f : searchrec);

    begin
        asm
            movl 12(%ebp),%esi
            movb $0x4f,%ah
            call syscall
            jnc .LFN
            movw %ax,doserror
        .LFN:
        end;
    end;

begin
    {No error}
    DosError := 0;
    SearchRec2DosSearchRec (F);
    if os_mode = osOS2 then
    begin
        New (FStat);
        Count := 1;
        DosError := DosFindNext (F.Handle, FStat, SizeOf (FStat), Count);
        if (DosError = 0) and (Count = 0) then DosError := 18;
    end else _findnext (F);
    DosSearchRec2SearchRec (F, FStat);
    if os_mode = osOS2 then Dispose (FStat);
end;

procedure FindClose (var F: SearchRec);
begin
    if os_mode = osOS2 then
    begin
        DosError := DosFindClose (F.Handle);
    end;
end;

procedure swapvectors;

{For TP compatibility, this exists.}

begin
end;

type    PPchar=^Pchar;

{$ASMMODE DIRECT}

function envs:PPchar;assembler;

asm
    movl _environ,%eax
end ['EAX'];

function envcount:longint;assembler;

var hp : ppchar;

asm
    movl _envc,%eax
end ['EAX'];

{$ASMMODE ATT}

function envstr(index : longint) : string;

var hp:PPchar;

begin
    if (index<=0) or (index>envcount) then
        begin
            envstr:='';
            exit;
        end;
    hp:=PPchar(cardinal(envs)+4*(index-1));
    envstr:=strpas(hp^);
end;

function getenv(const envvar : string) : string;

var hs,_envvar : string;
    eqpos,i : longint;

begin
    _envvar:=upcase(envvar);
    getenv:='';
    for i:=1 to envcount do
        begin
            hs:=envstr(i);
            eqpos:=pos('=',hs);
            if copy(hs,1,eqpos-1)=_envvar then
                begin
                    getenv:=copy(hs,eqpos+1,length(hs)-eqpos);
                    exit;
                end;
        end;
end;

procedure fsplit(path:pathstr;var dir:dirstr;var name:namestr;
                 var ext:extstr);

var p1,i : longint;

begin
    {Get drive name}
    p1:=pos(':',path);
    if p1>0 then
        begin
            dir:=path[1]+':';
            delete(path,1,p1);
        end
    else
        dir:='';
    { split the path and the name, there are no more path informtions }
    { if path contains no backslashes                                 }
    while true do
        begin
            p1:=pos('\',path);
            if p1=0 then
                p1:=pos('/',path);
            if p1=0 then
                break;
            dir:=dir+copy(path,1,p1);
              delete(path,1,p1);
        end;
    {Try to find an extension.}
    ext:='';
    for i:=length(path) downto 1 do
        if path[i]='.' then
            begin
                ext:=copy(path,i,high(extstr));
                delete(path,i,length(path)-i+1);
                break;
            end;
    name:=path;
end;

function fexpand(const path:pathstr):pathstr;

    function get_current_drive:byte;assembler;

    asm
        movb $0x19,%ah
        call syscall
    end;

var s,pa:string;
    i,j:longint;

begin
    getdir(0,s);
    i:=ioresult;
    if FileNameCaseSensitive then
        pa := path
    else
        pa:=upcase(path);
    {Allow slash as backslash}
    for i:=1 to length(pa) do
        if pa[i]='/' then
            pa[i]:='\';
    if (length(pa)>1) and (pa[1] in ['A'..'Z','a'..'z']) and (pa[2]=':') then
        begin
            { Always uppercase driveletter }
            if (pa[1] in ['a'..'z']) then
                pa[1]:=Chr(Ord(Pa[1])-32);
            {We must get the right directory}
            getdir(byte(pa[1])-byte('A')+1,s);
            i:=ioresult;
            if pa[0] = #2 then
                pa := s
            else
                if (byte(pa[0])>2) and (pa[3]<>'\') then
                    if pa[1]=s[1] then
                        begin
                            { remove ending slash if it already exists }
                            if s[length(s)]='\' then
                                dec(s[0]);
                            pa:=s+'\'+copy (pa,3,length(pa))
                        end
                    else
                        pa:=pa[1]+':\'+copy (pa,3,length(pa))
        end
    else
        if pa[1]='\' then
            begin
                { Do not touch Network drive names }
                if not ((Length(pa)>1) and (pa[2]='\')) then
                    pa:=s[1]+':'+pa
            end
        else if s[0]=#3 then
            pa:=s+pa
        else
            pa:=s+'\'+pa;
    {First remove all references to '\.\'}
    i:=pos('\.\',pa);
    while i<>0 do
        begin
            delete(pa,i,2);
            i:=pos('\.\',pa);
        end;
    {Now remove also all references to '\..\' + of course previous dirs..}
    repeat
        i:=pos('\..\',pa);
        if i<>0 then
            begin
                j:=i-1;
                while (j>1) and (pa[j]<>'\') do
                    dec(j);
                if pa[j+1] = ':' then
                    j := 3;
                delete (pa,j,i-j+3);
            end;
    until i=0;

    fexpand:=pa;
end;

procedure packtime(var d:datetime;var time:longint);

var zs:longint;

begin
    time:=-1980;
    time:=time+d.year and 127;
    time:=time shl 4;
    time:=time+d.month;
    time:=time shl 5;
    time:=time+d.day;
    time:=time shl 16;
    zs:=d.hour;
    zs:=zs shl 6;
    zs:=zs+d.min;
    zs:=zs shl 5;
    zs:=zs+d.sec div 2;
    time:=time+(zs and $ffff);
end;

procedure unpacktime (time:longint;var d:datetime);

begin
    d.sec:=(time and 31) * 2;
    time:=time shr 5;
    d.min:=time and 63;
    time:=time shr 6;
    d.hour:=time and 31;
    time:=time shr 5;
    d.day:=time and 31;
    time:=time shr 5;
    d.month:=time and 15;
    time:=time shr 4;
    d.year:=time+1980;
end;

procedure getfattr(var f;var attr : word);assembler;

asm
    movw $0x4300,%ax
    movl f,%edx
    {addl $filerec.name,%edx Doesn't work!!}
    addl $60,%edx
    call syscall
    movl attr,%ebx
    movw %cx,(%ebx)
    xorb %ah,%ah
    movw %ax,doserror
end;

procedure setfattr(var f;attr : word);assembler;

asm
    movw $0x4301,%ax
    movl f,%edx
    {addl $filerec.name,%edx Doesn't work!!}
    addl $60,%edx
    movw attr,%cx
    call syscall
    xorb %ah,%ah
    movw %ax,doserror
end;

end.
{
  $Log$
  Revision 1.4  2000-10-28 16:58:34  hajny
    * many FExpand fixes

  Revision 1.3  2000/09/29 21:49:41  jonas
    * removed warnings

  Revision 1.2  2000/07/14 10:33:10  michael
  + Conditionals fixed

  Revision 1.1  2000/07/13 06:31:04  michael
  + Initial import

}
