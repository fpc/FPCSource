{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1993,97 by the Free Pascal development team.

    Dos unit for BP7 compatible RTL

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit dos;

{$I os.inc}

interface
Uses Go32;

Const
  {Bitmasks for CPU Flags}
  fcarry     = $0001;
  fparity    = $0004;
  fauxiliary = $0010;
  fzero      = $0040;
  fsign      = $0080;
  foverflow  = $0800;

  {Bitmasks for file attribute}
  readonly  = $01;
  hidden    = $02;
  sysfile   = $04;
  volumeid  = $08;
  directory = $10;
  archive   = $20;
  anyfile   = $3F;

  {File Status}
  fmclosed = $D7B0;
  fminput  = $D7B1;
  fmoutput = $D7B2;
  fminout  = $D7B3;


Type
{$IFDEF GO32V2}
{ Needed for Win95 LFN Support }
  ComStr  = String[255];
  PathStr = String[255];
  DirStr  = String[255];
  NameStr = String[255];
  ExtStr  = String[255];
{$ELSE}
  comstr  = string[127];        { command line string }
  pathstr = string[79];         { string for a file path }
  dirstr  = string[67];         { string for a directory }
  namestr = string[8];          { string for a file name }
  extstr  = string[4];          { string for an extension }
{$ENDIF}

{
  filerec.inc contains the definition of the filerec.
  textrec.inc contains the definition of the textrec.
  It is in a separate file to make it available in other units without
  having to use the DOS unit for it.
}
{$i filerec.inc}
{$i textrec.inc}

  DateTime = packed record
    Year,
    Month,
    Day,
    Hour,
    Min,
    Sec   : word;
  End;

{$ifdef GO32V2}

  searchrec = packed record
     fill : array[1..21] of byte;
     attr : byte;
     time : longint;
     { reserved : word; not in DJGPP V2 }
     size : longint;
     name : string[12]; { the same size as declared by (DJ GNU C) }
  end;

  Registers = Go32.Registers;

{$ELSE}

  searchrec = packed record
     fill     : array[1..21] of byte;
     attr     : byte;
     time     : longint;
     reserved : word; { requires the DOS extender (DJ GNU-C) }
     size     : longint;
     name     : string[15]; { the same size as declared by (DJ GNU C) }
  end;

  registers = packed record
    case i : integer of
     0 : (ax,f1,bx,f2,cx,f3,dx,f4,bp,f5,si,f51,di,f6,ds,f7,es,f8,flags,fs,gs : word);
     1 : (al,ah,f9,f10,bl,bh,f11,f12,cl,ch,f13,f14,dl,dh : byte);
     2 : (eax,  ebx,  ecx,  edx,  ebp,  esi,  edi : longint);
    end;
{$endif GO32V1}

Var
  DosError : integer;

{Interrupt}
Procedure Intr(intno: byte; var regs: registers);
Procedure MSDos(var regs: registers);

{Info/Date/Time}
Function  DosVersion: Word;
Procedure GetDate(var year, month, mday, wday: word);
Procedure GetTime(var hour, minute, second, sec100: word);
procedure SetDate(year,month,day: word);
Procedure SetTime(hour,minute,second,sec100: word);
Procedure UnpackTime(p: longint; var t: datetime);
Procedure PackTime(var t: datetime; var p: longint);

{Exec}
Procedure Exec(const path: pathstr; const comline: comstr);
Function  DosExitCode: word;

{Disk}
Function  DiskFree(drive: byte) : longint;
Function  DiskSize(drive: byte) : longint;
Procedure FindFirst(const path: pathstr; attr: word; var f: searchRec);
Procedure FindNext(var f: searchRec);
Procedure FindClose(Var f: SearchRec);

{File}
Procedure GetFAttr(var f; var attr: word);
Procedure GetFTime(var f; var time: longint);
Function  FSearch(path: pathstr; dirlist: string): pathstr;
Function  FExpand(const path: pathstr): pathstr;
Procedure FSplit(path: pathstr; var dir: dirstr; var name: namestr; var ext: extstr);

{Environment}
Function  EnvCount: longint;
Function  EnvStr(index: integer): string;
Function  GetEnv(envvar: string): string;

{Misc}
Procedure SetFAttr(var f; attr: word);
Procedure SetFTime(var f; time: longint);
Procedure GetCBreak(var breakvalue: boolean);
Procedure SetCBreak(breakvalue: boolean);
Procedure GetVerify(var verify: boolean);
Procedure SetVerify(verify: boolean);

{Do Nothing Functions}
Procedure SwapVectors;
Procedure GetIntVec(intno: byte; var vector: pointer);
Procedure SetIntVec(intno: byte; vector: pointer);
Procedure Keep(exitcode: word);

implementation

uses
  strings;

{$ASMMODE ATT}

{******************************************************************************
                           --- Dos Interrupt ---
******************************************************************************}

var
  dosregs : registers;

    procedure LoadDosError;
      begin
        if (dosregs.flags and carryflag) <> 0 then
         doserror:=dosregs.ax
        else
         doserror:=0;
      end;

{$ifdef GO32V2}

    procedure intr(intno : byte;var regs : registers);
      begin
         realintr(intno,regs);
      end;

{$else GO32V2}
{$ASMMODE DIRECT}
    procedure intr(intno : byte;var regs : registers);

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
            // do not use first int
            addl        $2,%eax

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
            pushl       %ebp
            pushl       %eax
            movl        %esp,%ebp
            // calc EBP new
            addl        $12,%ebp
            movl        10(%ebp),%eax
            // do not use first int
            addl        $2,%eax

            popl        (%eax)
            movl        %ebx,4(%eax)
            movl        %ecx,8(%eax)
            movl        %edx,12(%eax)
            // restore EBP
            popl        %edx
            movl        %edx,16(%eax)
            movl        %esi,20(%eax)
            movl        %edi,24(%eax)
            // ignore ES and DS
            popl        %ebx        /* flags */
            movl        %ebx,32(%eax)
            // FS and GS too
         end;
      end;
{$ASMMODE ATT}
{$endif GO32V2}

procedure msdos(var regs : registers);
begin
  intr($21,regs);
end;


{******************************************************************************
                        --- Info / Date / Time ---
******************************************************************************}

function dosversion : word;
begin
  dosregs.ax:=$3000;
  msdos(dosregs);
  dosversion:=dosregs.ax;
end;


procedure getdate(var year,month,mday,wday : word);
begin
  dosregs.ax:=$2a00;
  msdos(dosregs);
  wday:=dosregs.al;
  year:=dosregs.cx;
  month:=dosregs.dh;
  mday:=dosregs.dl;
end;


procedure setdate(year,month,day : word);
begin
   dosregs.cx:=year;
   dosregs.dh:=month;
   dosregs.dl:=day;
   dosregs.ah:=$2b;
   msdos(dosregs);
   LoadDosError;
end;


procedure gettime(var hour,minute,second,sec100 : word);
begin
  dosregs.ah:=$2c;
  msdos(dosregs);
  hour:=dosregs.ch;
  minute:=dosregs.cl;
  second:=dosregs.dh;
  sec100:=dosregs.dl;
end;


procedure settime(hour,minute,second,sec100 : word);
begin
  dosregs.ch:=hour;
  dosregs.cl:=minute;
  dosregs.dh:=second;
  dosregs.dl:=sec100;
  dosregs.ah:=$2d;
  msdos(dosregs);
  LoadDosError;
end;


Procedure packtime(var t : datetime;var p : longint);
Begin
  p:=(t.sec shr 1)+(t.min shl 5)+(t.hour shl 11)+(t.day shl 16)+(t.month shl 21)+((t.year-1980) shl 25);
End;


Procedure unpacktime(p : longint;var t : datetime);
Begin
  with t do
   begin
     sec:=(p and 31) shl 1;
     min:=(p shr 5) and 63;
     hour:=(p shr 11) and 31;
     day:=(p shr 16) and 31;
     month:=(p shr 21) and 15;
     year:=(p shr 25)+1980;
   end;
End;


{******************************************************************************
                               --- Exec ---
******************************************************************************}

var
  lastdosexitcode : word;

{$ifdef GO32V2}

{
        Table 0931
        Format of EXEC parameter block for AL=00h,01h,04h:
        Offset  Size    Description
         00h    WORD    segment of environment to copy for child process (copy caller's
                          environment if 0000h)
         this does not seem to work (PM)
         02h    DWORD   pointer to command tail to be copied into child's PSP
         06h    DWORD   pointer to first FCB to be copied into child's PSP
         0Ah    DWORD   pointer to second FCB to be copied into child's PSP
         0Eh    DWORD   (AL=01h) will hold subprogram's initial SS:SP on return
         12h    DWORD   (AL=01h) will hold entry point (CS:IP) on return
        INT 21 4B--
}

procedure exec(const path : pathstr;const comline : comstr);
type
  realptr = packed record
    ofs,seg : word;
  end;
  texecblock = packed record
    envseg    : word;
    comtail   : realptr;
    firstFCB  : realptr;
    secondFCB : realptr;
    iniStack  : realptr;
    iniCSIP   : realptr;
  end;
var
  current_dos_buffer_pos,
  arg_ofs,
  i,la_env,
  la_p,la_c,la_e,
  fcb1_la,fcb2_la : longint;
  execblock       : texecblock;
  c,p             : string;

  function paste_to_dos(src : string) : boolean;
  var
    c : array[0..255] of char;
  begin
     paste_to_dos:=false;
     if current_dos_buffer_pos+length(src)+1>transfer_buffer+tb_size then
      RunError(217);
     move(src[1],c[0],length(src));
     c[length(src)]:=#0;
     seg_move(get_ds,longint(@c),dosmemselector,current_dos_buffer_pos,length(src)+1);
     current_dos_buffer_pos:=current_dos_buffer_pos+length(src)+1;
     paste_to_dos:=true;
  end;

begin
{ create command line }
  move(comline[0],c[1],length(comline)+1);
  c[length(comline)+2]:=#13;
  c[0]:=char(length(comline)+2);
{ create path }
  p:=path;
  for i:=1 to length(p) do
   if p[i]='/' then
    p[i]:='\';
{ create buffer }
  la_env:=transfer_buffer;
  while (la_env and 15)<>0 do
   inc(la_env);
  current_dos_buffer_pos:=la_env;
{ copy environment }
  for i:=1 to envcount do
   paste_to_dos(envstr(i));
  paste_to_dos(''); { adds a double zero at the end }
{ allow slash as backslash }
  la_p:=current_dos_buffer_pos;
  paste_to_dos(p);
  la_c:=current_dos_buffer_pos;
  paste_to_dos(c);
  la_e:=current_dos_buffer_pos;
  fcb1_la:=la_e;
  la_e:=la_e+16;
  fcb2_la:=la_e;
  la_e:=la_e+16;
{ allocate FCB see dosexec code }
  arg_ofs:=1;
  while (c[arg_ofs] in [' ',#9]) do
   inc(arg_ofs);
  dosregs.ax:=$2901;
  dosregs.ds:=(la_c+arg_ofs) shr 4;
  dosregs.esi:=(la_c+arg_ofs) and 15;
  dosregs.es:=fcb1_la shr 4;
  dosregs.edi:=fcb1_la and 15;
  msdos(dosregs);
{ allocate second FCB see dosexec code }
  repeat
    inc(arg_ofs);
  until (c[arg_ofs] in [' ',#9,#13]);
  if c[arg_ofs]<>#13 then
   begin
     repeat
       inc(arg_ofs);
     until not (c[arg_ofs] in [' ',#9]);
   end;
  dosregs.ax:=$2901;
  dosregs.ds:=(la_c+arg_ofs) shr 4;
  dosregs.si:=(la_c+arg_ofs) and 15;
  dosregs.es:=fcb2_la shr 4;
  dosregs.di:=fcb2_la and 15;
  msdos(dosregs);
  with execblock do
   begin
     envseg:=la_env shr 4;
     comtail.seg:=la_c shr 4;
     comtail.ofs:=la_c and 15;
     firstFCB.seg:=fcb1_la shr 4;
     firstFCB.ofs:=fcb1_la and 15;
     secondFCB.seg:=fcb2_la shr 4;
     secondFCB.ofs:=fcb2_la and 15;
   end;
  seg_move(get_ds,longint(@execblock),dosmemselector,la_e,sizeof(texecblock));
  dosregs.edx:=la_p and 15;
  dosregs.ds:=la_p shr 4;
  dosregs.ebx:=la_e and 15;
  dosregs.es:=la_e shr 4;
  dosregs.ax:=$4b00;
  msdos(dosregs);
  LoadDosError;
  if DosError=0 then
   begin
     dosregs.ax:=$4d00;
     msdos(dosregs);
     LastDosExitCode:=DosRegs.al
   end
  else
   LastDosExitCode:=0;
end;

{$else GO32V2}

procedure exec(const path : pathstr;const comline : comstr);
var
  i : longint;
  b : array[0..255] of char;
begin
  doserror:=0;
  for i:=1to length(path) do
   if path[i]='/' then
    b[i-1]:='\'
   else
    b[i-1]:=path[i];
  b[i]:=' ';
  inc(i);
  move(comline[1],b[i],length(comline));
  inc(i,length(comline));
  b[i]:=#0;
  asm
        leal    b,%ebx
        movw    $0xff07,%ax
        int     $0x21
        movw    %ax,LastDosExitCode
  end;
end;

{$endif}


function dosexitcode : word;
begin
  dosexitcode:=lastdosexitcode;
end;


procedure getcbreak(var breakvalue : boolean);
begin
  dosregs.ax:=$3300;
  msdos(dosregs);
  breakvalue:=dosregs.dl<>0;
end;


procedure setcbreak(breakvalue : boolean);
begin
  dosregs.ax:=$3301;
  dosregs.dl:=ord(breakvalue);
  msdos(dosregs);
end;


procedure getverify(var verify : boolean);
begin
  dosregs.ah:=$54;
  msdos(dosregs);
  verify:=dosregs.al<>0;
end;


procedure setverify(verify : boolean);
begin
  dosregs.ah:=$2e;
  dosregs.al:=ord(verify);
  msdos(dosregs);
end;


{******************************************************************************
                               --- Disk ---
******************************************************************************}

function diskfree(drive : byte) : longint;
begin
  dosregs.dl:=drive;
  dosregs.ah:=$36;
  msdos(dosregs);
  if dosregs.ax<>$FFFF then
   diskfree:=dosregs.ax*dosregs.bx*dosregs.cx
  else
   diskfree:=-1;
end;


function disksize(drive : byte) : longint;
begin
  dosregs.dl:=drive;
  dosregs.ah:=$36;
  msdos(dosregs);
  if dosregs.ax<>$FFFF then
   disksize:=dosregs.ax*dosregs.cx*dosregs.dx
  else
   disksize:=-1;
end;


{******************************************************************************
                         --- Findfirst FindNext ---
******************************************************************************}

    procedure searchrec2dossearchrec(var f : searchrec);
      var
         l,i : longint;
      begin
         l:=length(f.name);
         for i:=1 to 12 do
           f.name[i-1]:=f.name[i];
         f.name[l]:=#0;
      end;

    procedure dossearchrec2searchrec(var f : searchrec);
      var
         l,i : longint;
      begin
         l:=12;
         for i:=0 to 12 do
           if f.name[i]=#0 then
             begin
                l:=i;
                break;
             end;
         for i:=11 downto 0 do
           f.name[i+1]:=f.name[i];
         f.name[0]:=chr(l);
      end;

    procedure findfirst(const path : pathstr;attr : word;var f : searchRec);

{$ifdef GO32V2}

      procedure _findfirst(path : pchar;attr : word;var f : searchrec);

        var
           i : longint;
        begin
           { allow slash as backslash }
           for i:=0 to strlen(path) do
             if path[i]='/' then path[i]:='\';
           copytodos(f,sizeof(searchrec));
           dosregs.edx:=transfer_buffer mod 16;
           dosregs.ds:=transfer_buffer div 16;
           dosregs.ah:=$1a;
           msdos(dosregs);
           dosregs.ecx:=attr;
           dosregs.edx:=(transfer_buffer mod 16) + Sizeof(searchrec)+1;
           dosmemput(transfer_buffer div 16,
             (transfer_buffer mod 16) +Sizeof(searchrec)+1,path^,strlen(path)+1);
           dosregs.ds:=transfer_buffer div 16;
           dosregs.ah:=$4e;
           msdos(dosregs);
           copyfromdos(f,sizeof(searchrec));
           LoadDosError;
        end;

{$else GO32V2}

      procedure _findfirst(path : pchar;attr : word;var f : searchrec);

        var
           i : longint;
        begin
           { allow slash as backslash }
           for i:=0 to strlen(path) do
             if path[i]='/' then path[i]:='\';
           asm
              movl f,%edx
              movb $0x1a,%ah
              int $0x21
              movl path,%edx
              movzwl attr,%ecx
              movb $0x4e,%ah
              int $0x21
              jnc .LFF
              movw %ax,DosError
           .LFF:
           end;
        end;

{$endif GO32V2}

      var
         path0 : array[0..80] of char;

      begin
         { no error }
         doserror:=0;
         strpcopy(path0,path);
         _findfirst(path0,attr,f);
         dossearchrec2searchrec(f);
      end;

    procedure findnext(var f : searchRec);

{$ifdef GO32V2}

      procedure _findnext(var f : searchrec);

        begin
           copytodos(f,sizeof(searchrec));
           dosregs.edx:=transfer_buffer mod 16;
           dosregs.ds:=transfer_buffer div 16;
           dosregs.ah:=$1a;
           msdos(dosregs);
           dosregs.ah:=$4f;
           msdos(dosregs);
           copyfromdos(f,sizeof(searchrec));
           LoadDosError;
        end;

{$else GO32V2}

      procedure _findnext(var f : searchrec);

        begin
           asm
              movl 12(%ebp),%edx
              movb $0x1a,%ah
              int $0x21
              movb $0x4f,%ah
              int $0x21
              jnc .LFN
              movw %ax,DosError
           .LFN:
           end;
        end;

{$endif GO32V2}

      begin
         { no error }
         doserror:=0;
         searchrec2dossearchrec(f);
         _findnext(f);
         dossearchrec2searchrec(f);
      end;

    procedure swapvectors;
{$ifdef go32v2}
{ uses four global symbols from v2prt0.as to be able to know the current
  exception state without using dpmiexcp unit }
{$ASMMODE DIRECT}
    begin
         asm
            movl _exception_exit,%eax
            orl  %eax,%eax
            je   .Lno_excep
            movl _v2prt0_exceptions_on,%eax
            orl  %eax,%eax
            je   .Lexceptions_off
            movl _swap_out,%eax
            call *%eax
            jmp  .Lno_excep
         .Lexceptions_off:
            movl _swap_in,%eax
            call *%eax
         .Lno_excep:
         end;
      end;
{$ASMMODE ATT}
{$else not go32v2}
      begin
      end;
{$endif go32v2}


    Procedure FindClose(Var f: SearchRec);
      begin
      end;

{******************************************************************************
                               --- File ---
******************************************************************************}

    procedure fsplit(path : pathstr;var dir : dirstr;var name : namestr;var ext : extstr);
      var
         p1,i : longint;
      begin
         { allow slash as backslash }
         for i:=1 to length(path) do
          if path[i]='/' then path[i]:='\';
         { get drive name }
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
                break;
              dir:=dir+copy(path,1,p1);
              delete(path,1,p1);
           end;
         { try to find out a extension }
         p1:=pos('.',path);
         if p1>0 then
           begin
              ext:=copy(path,p1,4);
              delete(path,p1,length(path)-p1+1);
           end
         else
           ext:='';
         name:=path;
      end;

    function fexpand(const path : pathstr) : pathstr;
       var
         s,pa : string[79];
         i,j  : longint;
       begin
          getdir(0,s);
          pa:=upcase(path);
          { allow slash as backslash }
          for i:=1 to length(pa) do
           if pa[i]='/' then
            pa[i]:='\';
 
          if (length(pa)>1) and (pa[1] in ['A'..'Z']) and (pa[2]=':') then
            begin
               { we must get the right directory }
               getdir(ord(pa[1])-ord('A')+1,s);
               if (ord(pa[0])>2) and (pa[3]<>'\') then
                 if pa[1]=s[1] then
                   pa:=s+'\'+copy (pa,3,length(pa))
                 else
                   pa:=pa[1]+':\'+copy (pa,3,length(pa))
            end
          else
            if pa[1]='\' then
              pa:=s[1]+':'+pa
            else if s[0]=#3 then
              pa:=s+pa
            else
              pa:=s+'\'+pa;
 
        { Turbo Pascal gives current dir on drive if only drive given as parameter! }
        if length(pa) = 2 then
         begin
           getdir(byte(pa[1])-64,s);
           pa := s;
         end;
 
        {First remove all references to '\.\'}
          while pos ('\.\',pa)<>0 do
           delete (pa,pos('\.\',pa),2);
        {Now remove also all references to '\..\' + of course previous dirs..}
          repeat
            i:=pos('\..\',pa);
            if i<>0 then
             begin
               j:=i-1;
               while (j>1) and (pa[j]<>'\') do
                dec (j);
               if pa[j+1] = ':' then j := 3;
               delete (pa,j,i-j+3);
             end;
          until i=0;
 
          { Turbo Pascal gets rid of a \.. at the end of the path }
          { Now remove also any reference to '\..'  at end of line 
            + of course previous dir.. }
          i:=pos('\..',pa);
          if i<>0 then
           begin
             if i = length(pa) - 2 then
              begin
                j:=i-1;
                while (j>1) and (pa[j]<>'\') do
                 dec (j);
                delete (pa,j,i-j+3);
              end;
              pa := pa + '\';
            end;
          { Remove End . and \}
          if (length(pa)>0) and (pa[length(pa)]='.') then
           dec(byte(pa[0]));
          { if only the drive + a '\' is left then the '\' should be left to prevtn the program 
            accessing the current directory on the drive rather than the root!}
          { if the last char of path = '\' then leave it in as this is what TP does! }
          if ((length(pa)>3) and (pa[length(pa)]='\')) and (path[length(path)] <> '\') then
           dec(byte(pa[0]));
          { if only a drive is given in path then there should be a '\' at the
            end of the string given back }
          if length(path) = 2 then pa := pa + '\';
          fexpand:=pa;
       end;

    Function FSearch(path: pathstr; dirlist: string): pathstr;
      var
         i,p1   : longint;
         s      : searchrec;
         newdir : pathstr;
      begin
      { No wildcards allowed in these things }
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
                if (newdir<>'') and (not (newdir[length(newdir)] in ['\',':'])) then
                 newdir:=newdir+'\';
                findfirst(newdir+path,anyfile,s);
                if doserror=0 then
                 newdir:=newdir+path
                else
                 newdir:='';
              until (dirlist='') or (newdir<>'');
              fsearch:=newdir;
           end;
      end;

    procedure getftime(var f;var time : longint);
      begin
         dosregs.bx:=textrec(f).handle;
         dosregs.ax:=$5700;
         msdos(dosregs);
         time:=(dosregs.dx shl 16)+dosregs.cx;
         doserror:=dosregs.al;
      end;

   procedure setftime(var f;time : longint);
      begin
         dosregs.bx:=textrec(f).handle;
         dosregs.cx:=time and $ffff;
         dosregs.dx:=time shr 16;
         dosregs.ax:=$5701;
         msdos(dosregs);
         doserror:=dosregs.al;
      end;


procedure getfattr(var f;var attr : word);
{$ifndef GO32V2}
var
  n : array[0..255] of char;
{$endif}
begin
{$ifdef GO32V2}
  copytodos(filerec(f).name,strlen(filerec(f).name)+1);
  dosregs.edx:=transfer_buffer and 15;
  dosregs.ds:=transfer_buffer shr 4;
{$else}
  strpcopy(n,filerec(f).name);
  dosregs.edx:=longint(@n);
{$endif}
  dosregs.ax:=$4300;
  msdos(dosregs);
  LoadDosError;
  Attr:=dosregs.cx;
end;


procedure setfattr(var f;attr : word);
{$ifndef GO32V2}
var
  n : array[0..255] of char;
{$endif}
begin
{$ifdef GO32V2}
  copytodos(filerec(f).name,strlen(filerec(f).name)+1);
  dosregs.edx:=transfer_buffer mod 16;
  dosregs.ds:=transfer_buffer div 16;
{$else}
  strpcopy(n,filerec(f).name);
  dosregs.edx:=longint(@n);
{$endif}
  dosregs.ax:=$4301;
  dosregs.cx:=attr;
  msdos(dosregs);
  LoadDosError;
end;


{******************************************************************************
                             --- Environment ---
******************************************************************************}

function envcount : longint;
var
  hp : ppchar;
begin
  hp:=envp;
  envcount:=0;
  while assigned(hp^) do
   begin
     inc(envcount);
     hp:=hp+4;
   end;
end;


function envstr(index : integer) : string;
begin
  if (index<=0) or (index>envcount) then
   begin
     envstr:='';
     exit;
   end;
  envstr:=strpas(ppchar(envp+4*(index-1))^);
end;


Function  GetEnv(envvar: string): string;
var
  hp      : ppchar;
  hs    : string;
  eqpos : longint;
begin
  envvar:=upcase(envvar);
  hp:=envp;
  getenv:='';
  while assigned(hp^) do
   begin
     hs:=strpas(hp^);
     eqpos:=pos('=',hs);
     if copy(hs,1,eqpos-1)=envvar then
      begin
        getenv:=copy(hs,eqpos+1,255);
        exit;
      end;
     hp:=hp+4;
   end;
end;


{******************************************************************************
                             --- Not Supported ---
******************************************************************************}

Procedure keep(exitcode : word);
Begin
End;

Procedure getintvec(intno : byte;var vector : pointer);
Begin
End;

Procedure setintvec(intno : byte;vector : pointer);
Begin
End;


end.
{
  $Log$
  Revision 1.7  1998-08-16 09:12:13  michael
  Corrected fexpand behaviour.

  Revision 1.6  1998/08/05 21:01:50  michael
  applied bugfix from maillist to fsearch

  Revision 1.5  1998/05/31 14:18:13  peter
    * force att or direct assembling
    * cleanup of some files

  Revision 1.4  1998/05/22 00:39:22  peter
    * go32v1, go32v2 recompiles with the new objects
    * remake3 works again with go32v2
    - removed some "optimizes" from daniel which were wrong

  Revision 1.3  1998/05/21 19:30:47  peter
    * objects compiles for linux
    + assign(pchar), assign(char), rename(pchar), rename(char)
    * fixed read_text_as_array
    + read_text_as_pchar which was not yet in the rtl

}

