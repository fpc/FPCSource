{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit system;

interface

{ two debug conditionnals can be used
  - SYSTEMDEBUG
    -for STACK checks
    -for non closed files at exit (or at any time with GDB)
  - SYSTEM_DEBUG_STARTUP
    specifically for
    - proxy command line (DJGPP feature)
    - list of args
    - list of env variables  (PM) }

{$ifndef NO_EXCEPTIONS_IN_SYSTEM}
{$define EXCEPTIONS_IN_SYSTEM}
{$endif NO_EXCEPTIONS_IN_SYSTEM}

{ include system-independent routine headers }

{$I systemh.inc}

{ include heap support headers }

{$I heaph.inc}


{Platform specific information}
const
 LineEnding = #13#10;
{ LFNSupport is a variable here, defined below!!! }
 DirectorySeparator = '\';
 DriveSeparator = ':';
 PathSeparator = ';';
{ FileNameCaseSensitive is defined separately below!!! }


const
{ Default filehandles }
  UnusedHandle    = -1;
  StdInputHandle  = 0;
  StdOutputHandle = 1;
  StdErrorHandle  = 2;

  FileNameCaseSensitive : boolean = false;

  sLineBreak = LineEnding;
  DefaultTextLineBreakStyle : TTextLineBreakStyle = tlbsCRLF;

{ Default memory segments (Tp7 compatibility) }
  seg0040 = $0040;
  segA000 = $A000;
  segB000 = $B000;
  segB800 = $B800;

var
{ Mem[] support }
  mem  : array[0..$7fffffff] of byte absolute $0:$0;
  memw : array[0..$7fffffff div sizeof(word)] of word absolute $0:$0;
  meml : array[0..$7fffffff div sizeof(longint)] of longint absolute $0:$0;
{ C-compatible arguments and environment }
  argc  : longint;
  argv  : ppchar;
  envp  : ppchar;
  dos_argv0 : pchar;

{$ifndef RTLLITE}
{ System info }
  LFNSupport : boolean;
{$ELSE RTLLITE}
const
  LFNSupport = false;
{$endif RTLLITE}

type
{ Dos Extender info }
  p_stub_info = ^t_stub_info;
  t_stub_info = packed record
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

  p_go32_info_block = ^t_go32_info_block;
  t_go32_info_block = packed record
       size_of_this_structure_in_bytes    : longint; {offset 0}
       linear_address_of_primary_screen   : longint; {offset 4}
       linear_address_of_secondary_screen : longint; {offset 8}
       linear_address_of_transfer_buffer  : longint; {offset 12}
       size_of_transfer_buffer            : longint; {offset 16}
       pid                                : longint; {offset 20}
       master_interrupt_controller_base   : byte; {offset 24}
       slave_interrupt_controller_base    : byte; {offset 25}
       selector_for_linear_memory         : word; {offset 26}
       linear_address_of_stub_info_structure : longint; {offset 28}
       linear_address_of_original_psp     : longint; {offset 32}
       run_mode                           : word; {offset 36}
       run_mode_info                      : word; {offset 38}
  end;

var
  stub_info       : p_stub_info;
  go32_info_block : t_go32_info_block;
{$ifdef SYSTEMDEBUG}
const
   accept_sbrk : boolean = true;
{$endif}

{
  necessary for objects.pas, should be removed (at least from the interface
  to the implementation)
}
  type
    trealregs=record
      realedi,realesi,realebp,realres,
      realebx,realedx,realecx,realeax : longint;
      realflags,
      reales,realds,realfs,realgs,
      realip,realcs,realsp,realss  : word;
    end;
  function  do_write(h,addr,len : longint) : longint;
  function  do_read(h,addr,len : longint) : longint;
  procedure syscopyfromdos(addr : longint; len : longint);
  procedure syscopytodos(addr : longint; len : longint);
  procedure sysrealintr(intnr : word;var regs : trealregs);
  function  tb : longint;

implementation

{ include system independent routines }

{$I system.inc}

const
  carryflag = 1;

type
  tseginfo=packed record
    offset  : pointer;
    segment : word;
  end;

var
  old_int00 : tseginfo;cvar;
  old_int75 : tseginfo;cvar;

{$asmmode ATT}

{*****************************************************************************
                              Go32 Helpers
*****************************************************************************}

function far_strlen(selector : word;linear_address : longint) : longint;assembler;
asm
        movl linear_address,%edx
        movl %edx,%ecx
        movw selector,%gs
.Larg19:
        movb %gs:(%edx),%al
        testb %al,%al
        je .Larg20
        incl %edx
        jmp .Larg19
.Larg20:
        movl %edx,%eax
        subl %ecx,%eax
end;


function tb : longint;
begin
  tb:=go32_info_block.linear_address_of_transfer_buffer;
end;


function tb_segment : longint;
begin
  tb_segment:=go32_info_block.linear_address_of_transfer_buffer shr 4;
end;


function tb_offset : longint;
begin
  tb_offset:=go32_info_block.linear_address_of_transfer_buffer and $f;
end;


function tb_size : longint;
begin
  tb_size:=go32_info_block.size_of_transfer_buffer;
end;


function dos_selector : word;
begin
  dos_selector:=go32_info_block.selector_for_linear_memory;
end;


function get_ds : word;assembler;
asm
        movw    %ds,%ax
end;


function get_cs : word;assembler;
asm
        movw    %cs,%ax
end;


procedure sysseg_move(sseg : word;source : longint;dseg : word;dest : longint;count : longint);
begin
   if count=0 then
     exit;
   if (sseg<>dseg) or ((sseg=dseg) and (source>dest)) then
     asm
        pushw %es
        pushw %ds
        cld
        movl count,%ecx
        movl source,%esi
        movl dest,%edi
        movw dseg,%ax
        movw %ax,%es
        movw sseg,%ax
        movw %ax,%ds
        movl %ecx,%eax
        shrl $2,%ecx
        rep
        movsl
        movl %eax,%ecx
        andl $3,%ecx
        rep
        movsb
        popw %ds
        popw %es
     end ['ESI','EDI','ECX','EAX']
   else if (source<dest) then
     { copy backward for overlapping }
     asm
        pushw %es
        pushw %ds
        std
        movl count,%ecx
        movl source,%esi
        movl dest,%edi
        movw dseg,%ax
        movw %ax,%es
        movw sseg,%ax
        movw %ax,%ds
        addl %ecx,%esi
        addl %ecx,%edi
        movl %ecx,%eax
        andl $3,%ecx
        orl %ecx,%ecx
        jz .LSEG_MOVE1

        { calculate esi and edi}
        decl %esi
        decl %edi
        rep
        movsb
        incl %esi
        incl %edi
     .LSEG_MOVE1:
        subl $4,%esi
        subl $4,%edi
        movl %eax,%ecx
        shrl $2,%ecx
        rep
        movsl
        cld
        popw %ds
        popw %es
     end ['ESI','EDI','ECX'];
end;



var
  _args : ppchar;external name '_args';


procedure setup_arguments;
type
  arrayword = array [0..255] of word;
var
  psp      : word;
  proxy_s  : string[50];
  proxy_argc,proxy_seg,proxy_ofs,lin : longint;
  rm_argv  : ^arrayword;
  argv0len : longint;
  useproxy : boolean;
  hp       : ppchar;
  doscmd   : string[129];  { Dos commandline copied from PSP, max is 128 chars +1 for terminating zero }
  arglen,
  count   : longint;
  argstart,
  pc,arg  : pchar;
  quote   : char;
  argvlen : longint;

  function atohex(s : pchar) : longint;
  var
    rv : longint;
    v  : byte;
  begin
    rv:=0;
    while (s^<>#0) do
     begin
       v:=byte(s^)-byte('0');
       if (v > 9) then
         dec(v,7);
       v:=v and 15; { in case it's lower case }
       rv:=(rv shl 4) or v;
       inc(longint(s));
     end;
    atohex:=rv;
  end;

  procedure allocarg(idx,len:longint);
  begin
    if idx>=argvlen then
     begin
       argvlen:=(idx+8) and (not 7);
       sysreallocmem(argv,argvlen*sizeof(pointer));
     end;
    { use realloc to reuse already existing memory }
    if len<>0 then
     sysreallocmem(argv[idx],len+1);
  end;

begin
  count:=0;
  argc:=1;
  argv:=nil;
  argvlen:=0;
  { load commandline from psp }
  psp:=stub_info^.psp_selector;
  sysseg_move(psp, 128, get_ds, longint(@doscmd), 128);
  doscmd[length(doscmd)+1]:=#0;
{$IfDef SYSTEM_DEBUG_STARTUP}
  Writeln(stderr,'Dos command line is #',doscmd,'# size = ',length(doscmd));
{$EndIf }
  { create argv[0] }
  argv0len:=strlen(dos_argv0);
  allocarg(count,argv0len);
  move(dos_argv0^,argv[count]^,argv0len);
  inc(count);
  { setup cmdline variable }
  cmdline:=Getmem(argv0len+length(doscmd)+2);
  move(dos_argv0^,cmdline^,argv0len);
  cmdline[argv0len]:=' ';
  inc(argv0len);
  move(doscmd[1],cmdline[argv0len],length(doscmd));
  cmdline[argv0len+length(doscmd)+1]:=#0;
  { parse dos commandline }
  pc:=@doscmd[1];
  while pc^<>#0 do
   begin
     { skip leading spaces }
     while pc^ in [#1..#32] do
      inc(pc);
     if pc^=#0 then
      break;
     { calc argument length }
     quote:=' ';
     argstart:=pc;
     arglen:=0;
     while (pc^<>#0) do
      begin
        case pc^ of
          #1..#32 :
            begin
              if quote<>' ' then
               inc(arglen)
              else
               break;
            end;
          '"' :
            begin
              if quote<>'''' then
               begin
                 if pchar(pc+1)^<>'"' then
                  begin
                    if quote='"' then
                     quote:=' '
                    else
                     quote:='"';
                  end
                 else
                  inc(pc);
               end
              else
               inc(arglen);
            end;
          '''' :
            begin
              if quote<>'"' then
               begin
                 if pchar(pc+1)^<>'''' then
                  begin
                    if quote=''''  then
                     quote:=' '
                    else
                     quote:='''';
                  end
                 else
                  inc(pc);
               end
              else
               inc(arglen);
            end;
          else
            inc(arglen);
        end;
        inc(pc);
      end;
     { copy argument }
     allocarg(count,arglen);
     quote:=' ';
     pc:=argstart;
     arg:=argv[count];
     while (pc^<>#0) do
      begin
        case pc^ of
          #1..#32 :
            begin
              if quote<>' ' then
               begin
                 arg^:=pc^;
                 inc(arg);
               end
              else
               break;
            end;
          '"' :
            begin
              if quote<>'''' then
               begin
                 if pchar(pc+1)^<>'"' then
                  begin
                    if quote='"' then
                     quote:=' '
                    else
                     quote:='"';
                  end
                 else
                  inc(pc);
               end
              else
               begin
                 arg^:=pc^;
                 inc(arg);
               end;
            end;
          '''' :
            begin
              if quote<>'"' then
               begin
                 if pchar(pc+1)^<>'''' then
                  begin
                    if quote=''''  then
                     quote:=' '
                    else
                     quote:='''';
                  end
                 else
                  inc(pc);
               end
              else
               begin
                 arg^:=pc^;
                 inc(arg);
               end;
            end;
          else
            begin
              arg^:=pc^;
              inc(arg);
            end;
        end;
        inc(pc);
      end;
     arg^:=#0;
 {$IfDef SYSTEM_DEBUG_STARTUP}
     Writeln(stderr,'dos arg ',count,' #',arglen,'#',argv[count],'#');
 {$EndIf SYSTEM_DEBUG_STARTUP}
     inc(count);
   end;
  argc:=count;
  { check for !proxy for long commandlines passed using environment }
  hp:=envp;
  useproxy:=false;
  while assigned(hp^) do
   begin
     if (hp^[0]=' ') then
      begin
        proxy_s:=strpas(hp^);
        if Copy(proxy_s,1,7)=' !proxy' then
         begin
           proxy_s[13]:=#0;
           proxy_s[18]:=#0;
           proxy_s[23]:=#0;
           argv[2]:=@proxy_s[9];
           argv[3]:=@proxy_s[14];
           argv[4]:=@proxy_s[19];
           useproxy:=true;
           break;
         end;
      end;
     inc(hp);
   end;
  { check for !proxy for long commandlines passed using commandline }
  if (not useproxy) and
     (argc > 1) and (far_strlen(get_ds,longint(argv[1])) = 6)  then
   begin
     move(argv[1]^,proxy_s[1],6);
     proxy_s[0] := #6;
     if (proxy_s = '!proxy') then
      useproxy:=true;
   end;
  { use proxy when found }
  if useproxy then
   begin
     proxy_argc:=atohex(argv[2]);
     proxy_seg:=atohex(argv[3]);
     proxy_ofs:=atohex(argv[4]);
{$IfDef SYSTEM_DEBUG_STARTUP}
     Writeln(stderr,'proxy command line found');
     writeln(stderr,'argc: ',proxy_argc,' seg: ',proxy_seg,' ofs: ',proxy_ofs);
{$EndIf SYSTEM_DEBUG_STARTUP}
     rm_argv:=SysGetmem(proxy_argc*sizeof(word));
     sysseg_move(dos_selector,proxy_seg*16+proxy_ofs, get_ds,longint(rm_argv),proxy_argc*sizeof(word));
     for count:=0 to proxy_argc - 1 do
      begin
        lin:=proxy_seg*16+rm_argv^[count];
        arglen:=far_strlen(dos_selector,lin);
        allocarg(count,arglen);
        sysseg_move(dos_selector,lin,get_ds,longint(argv[count]),arglen+1);
{$IfDef SYSTEM_DEBUG_STARTUP}
        Writeln(stderr,'arg ',count,' #',rm_argv^[count],'#',arglen,'#',argv[count],'#');
{$EndIf SYSTEM_DEBUG_STARTUP}
    end;
     SysFreemem(rm_argv);
     argc:=proxy_argc;
   end;
  { create an nil entry }
  allocarg(argc,0);
  { free unused memory }
  sysreallocmem(argv,(argc+1)*sizeof(pointer));
  _args:=argv;
end;


function strcopy(dest,source : pchar) : pchar;
begin
  asm
        cld
        movl 12(%ebp),%edi
        movl $0xffffffff,%ecx
        xorb %al,%al
        repne
        scasb
        not %ecx
        movl 8(%ebp),%edi
        movl 12(%ebp),%esi
        movl %ecx,%eax
        shrl $2,%ecx
        rep
        movsl
        movl %eax,%ecx
        andl $3,%ecx
        rep
        movsb
        movl 8(%ebp),%eax
        leave
        ret $8
  end;
end;


var
  __stubinfo : p_stub_info;external name '__stubinfo';
  ___dos_argv0 : pchar;external name '___dos_argv0';

procedure setup_environment;
var env_selector : word;
    env_count : longint;
    dos_env,cp : pchar;
begin
   stub_info:=__stubinfo;
   dos_env := sysgetmem(stub_info^.env_size);
   env_count:=0;
   sysseg_move(stub_info^.psp_selector,$2c, get_ds, longint(@env_selector), 2);
   sysseg_move(env_selector, 0, get_ds, longint(dos_env), stub_info^.env_size);
  cp:=dos_env;
  while cp ^ <> #0 do
    begin
    inc(env_count);
    while (cp^ <> #0) do inc(longint(cp)); { skip to NUL }
    inc(longint(cp)); { skip to next character }
    end;
  envp := sysgetmem((env_count+1) * sizeof(pchar));
  if (envp = nil) then exit;
  cp:=dos_env;
  env_count:=0;
  while cp^ <> #0 do
   begin
     envp[env_count] := sysgetmem(strlen(cp)+1);
     strcopy(envp[env_count], cp);
{$IfDef SYSTEM_DEBUG_STARTUP}
     Writeln(stderr,'env ',env_count,' = "',envp[env_count],'"');
{$EndIf SYSTEM_DEBUG_STARTUP}
     inc(env_count);
     while (cp^ <> #0) do
      inc(longint(cp)); { skip to NUL }
     inc(longint(cp)); { skip to next character }
   end;
  envp[env_count]:=nil;
  longint(cp):=longint(cp)+3;
  dos_argv0 := sysgetmem(strlen(cp)+1);
  if (dos_argv0 = nil) then halt;
  strcopy(dos_argv0, cp);
  { update ___dos_argv0 also }
  ___dos_argv0:=dos_argv0
end;


procedure syscopytodos(addr : longint; len : longint);
begin
   if len > tb_size then
     HandleError(217);
   sysseg_move(get_ds,addr,dos_selector,tb,len);
end;


procedure syscopyfromdos(addr : longint; len : longint);
begin
   if len > tb_size then
     HandleError(217);
   sysseg_move(dos_selector,tb,get_ds,addr,len);
end;


procedure sysrealintr(intnr : word;var regs : trealregs);
begin
   regs.realsp:=0;
   regs.realss:=0;
   asm
      movw  intnr,%bx
      xorl  %ecx,%ecx
      movl  regs,%edi
      movw  $0x300,%ax
      int   $0x31
   end;
end;


procedure set_pm_interrupt(vector : byte;const intaddr : tseginfo);
begin
  asm
        movl intaddr,%eax
        movl (%eax),%edx
        movw 4(%eax),%cx
        movl $0x205,%eax
        movb vector,%bl
        int $0x31
  end;
end;


procedure get_pm_interrupt(vector : byte;var intaddr : tseginfo);
begin
  asm
        movb    vector,%bl
        movl    $0x204,%eax
        int     $0x31
        movl    intaddr,%eax
        movl    %edx,(%eax)
        movw    %cx,4(%eax)
  end;
end;


procedure getinoutres(def : word);
var
  regs : trealregs;
begin
  regs.realeax:=$5900;
  regs.realebx:=$0;
  sysrealintr($21,regs);
  InOutRes:=lo(regs.realeax);
  case InOutRes of
   19 : InOutRes:=150;
   21 : InOutRes:=152;
   32 : InOutRes:=5;
  end;
  if InOutRes=0 then
    InOutRes:=Def;
end;


   { Keep Track of open files }
   const
      max_files = 50;
   var
      openfiles : array [0..max_files-1] of boolean;
{$ifdef SYSTEMDEBUG}
      opennames : array [0..max_files-1] of pchar;
   const
      free_closed_names : boolean = true;
{$endif SYSTEMDEBUG}

{*****************************************************************************
                         System Dependent Exit code
*****************************************************************************}

procedure ___exit(exitcode:longint);cdecl;external name '___exit';

procedure do_close(handle : longint);forward;

Procedure system_exit;
var
  h : byte;
begin
  for h:=0 to max_files-1 do
    if openfiles[h] then
      begin
{$ifdef SYSTEMDEBUG}
         writeln(stderr,'file ',opennames[h],' not closed at exit');
{$endif SYSTEMDEBUG}
         if h>=5 then
           do_close(h);
      end;
  { halt is not allways called !! }
  { not on normal exit !! PM }
  set_pm_interrupt($00,old_int00);
{$ifndef EXCEPTIONS_IN_SYSTEM}
  set_pm_interrupt($75,old_int75);
{$endif EXCEPTIONS_IN_SYSTEM}
  ___exit(exitcode);
end;


procedure new_int00;
begin
  HandleError(200);
end;


{$ifndef EXCEPTIONS_IN_SYSTEM}
procedure new_int75;
begin
  asm
        xorl    %eax,%eax
        outb    %al,$0x0f0
        movb    $0x20,%al
        outb    %al,$0x0a0
        outb    %al,$0x020
  end;
  HandleError(200);
end;
{$endif EXCEPTIONS_IN_SYSTEM}


var
  __stkbottom : longint;external name '__stkbottom';



{*****************************************************************************
                              ParamStr/Randomize
*****************************************************************************}

function paramcount : longint;
begin
  paramcount := argc - 1;
end;


function paramstr(l : longint) : string;
begin
  if (l>=0) and (l+1<=argc) then
   paramstr:=strpas(argv[l])
  else
   paramstr:='';
end;


procedure randomize;
var
  hl   : longint;
  regs : trealregs;
begin
  regs.realeax:=$2c00;
  sysrealintr($21,regs);
  hl:=lo(regs.realedx);
  randseed:=hl*$10000+ lo(regs.realecx);
end;

{*****************************************************************************
                              Heap Management
*****************************************************************************}

var
  int_heap : longint;external name 'HEAP';
  int_heapsize : longint;external name 'HEAPSIZE';

function getheapstart:pointer;
begin
  getheapstart:=@int_heap;
end;


function getheapsize:longint;
begin
  getheapsize:=int_heapsize;
end;


function ___sbrk(size:longint):longint;cdecl;external name '___sbrk';

function Sbrk(size : longint):longint;assembler;
asm
{$ifdef SYSTEMDEBUG}
        cmpb    $1,accept_sbrk
        je      .Lsbrk
        movl    $-1,%eax
        jmp     .Lsbrk_fail
      .Lsbrk:
{$endif}
        movl    size,%eax
        pushl   %eax
        call    ___sbrk
        addl    $4,%esp
{$ifdef SYSTEMDEBUG}
      .Lsbrk_fail:
{$endif}
end;


{ include standard heap management }
{$I heap.inc}


{****************************************************************************
                        Low level File Routines
 ****************************************************************************}

procedure AllowSlash(p:pchar);
var
  i : longint;
begin
{ allow slash as backslash }
  for i:=0 to strlen(p) do
   if p[i]='/' then p[i]:='\';
end;

procedure do_close(handle : longint);
var
  regs : trealregs;
begin
  if Handle<=4 then
   exit;
  regs.realebx:=handle;
  if handle<max_files then
    begin
       openfiles[handle]:=false;
{$ifdef SYSTEMDEBUG}
       if assigned(opennames[handle]) and free_closed_names then
         begin
            sysfreememsize(opennames[handle],strlen(opennames[handle])+1);
            opennames[handle]:=nil;
         end;
{$endif SYSTEMDEBUG}
    end;
  regs.realeax:=$3e00;
  sysrealintr($21,regs);
  if (regs.realflags and carryflag) <> 0 then
   GetInOutRes(lo(regs.realeax));
end;


procedure do_erase(p : pchar);
var
  regs : trealregs;
begin
  AllowSlash(p);
  syscopytodos(longint(p),strlen(p)+1);
  regs.realedx:=tb_offset;
  regs.realds:=tb_segment;
  if LFNSupport then
   regs.realeax:=$7141
  else
   regs.realeax:=$4100;
  regs.realesi:=0;
  regs.realecx:=0;
  sysrealintr($21,regs);
  if (regs.realflags and carryflag) <> 0 then
   GetInOutRes(lo(regs.realeax));
end;


procedure do_rename(p1,p2 : pchar);
var
  regs : trealregs;
begin
  AllowSlash(p1);
  AllowSlash(p2);
  if strlen(p1)+strlen(p2)+3>tb_size then
   HandleError(217);
  sysseg_move(get_ds,longint(p2),dos_selector,tb,strlen(p2)+1);
  sysseg_move(get_ds,longint(p1),dos_selector,tb+strlen(p2)+2,strlen(p1)+1);
  regs.realedi:=tb_offset;
  regs.realedx:=tb_offset + strlen(p2)+2;
  regs.realds:=tb_segment;
  regs.reales:=tb_segment;
  if LFNSupport then
   regs.realeax:=$7156
  else
   regs.realeax:=$5600;
  regs.realecx:=$ff;            { attribute problem here ! }
  sysrealintr($21,regs);
  if (regs.realflags and carryflag) <> 0 then
   GetInOutRes(lo(regs.realeax));
end;


function do_write(h,addr,len : longint) : longint;
var
  regs      : trealregs;
  size,
  writesize : longint;
begin
  writesize:=0;
  while len > 0 do
   begin
     if len>tb_size then
      size:=tb_size
     else
      size:=len;
     syscopytodos(addr+writesize,size);
     regs.realecx:=size;
     regs.realedx:=tb_offset;
     regs.realds:=tb_segment;
     regs.realebx:=h;
     regs.realeax:=$4000;
     sysrealintr($21,regs);
     if (regs.realflags and carryflag) <> 0 then
      begin
        GetInOutRes(lo(regs.realeax));
        exit(writesize);
      end;
     inc(writesize,lo(regs.realeax));
     dec(len,lo(regs.realeax));
     { stop when not the specified size is written }
     if lo(regs.realeax)<size then
      break;
   end;
  Do_Write:=WriteSize;
end;


function do_read(h,addr,len : longint) : longint;
var
  regs     : trealregs;
  size,
  readsize : longint;
begin
  readsize:=0;
  while len > 0 do
   begin
     if len>tb_size then
      size:=tb_size
     else
      size:=len;
     regs.realecx:=size;
     regs.realedx:=tb_offset;
     regs.realds:=tb_segment;
     regs.realebx:=h;
     regs.realeax:=$3f00;
     sysrealintr($21,regs);
     if (regs.realflags and carryflag) <> 0 then
      begin
        GetInOutRes(lo(regs.realeax));
        do_read:=0;
        exit;
      end;
     syscopyfromdos(addr+readsize,lo(regs.realeax));
     inc(readsize,lo(regs.realeax));
     dec(len,lo(regs.realeax));
     { stop when not the specified size is read }
     if lo(regs.realeax)<size then
      break;
   end;
  do_read:=readsize;
end;


function do_filepos(handle : longint) : longint;
var
  regs : trealregs;
begin
  regs.realebx:=handle;
  regs.realecx:=0;
  regs.realedx:=0;
  regs.realeax:=$4201;
  sysrealintr($21,regs);
  if (regs.realflags and carryflag) <> 0 then
   Begin
     GetInOutRes(lo(regs.realeax));
     do_filepos:=0;
   end
  else
   do_filepos:=lo(regs.realedx) shl 16+lo(regs.realeax);
end;


procedure do_seek(handle,pos : longint);
var
  regs : trealregs;
begin
  regs.realebx:=handle;
  regs.realecx:=pos shr 16;
  regs.realedx:=pos and $ffff;
  regs.realeax:=$4200;
  sysrealintr($21,regs);
  if (regs.realflags and carryflag) <> 0 then
   GetInOutRes(lo(regs.realeax));
end;



function do_seekend(handle:longint):longint;
var
  regs : trealregs;
begin
  regs.realebx:=handle;
  regs.realecx:=0;
  regs.realedx:=0;
  regs.realeax:=$4202;
  sysrealintr($21,regs);
  if (regs.realflags and carryflag) <> 0 then
   Begin
     GetInOutRes(lo(regs.realeax));
     do_seekend:=0;
   end
  else
   do_seekend:=lo(regs.realedx) shl 16+lo(regs.realeax);
end;


function do_filesize(handle : longint) : longint;
var
  aktfilepos : longint;
begin
  aktfilepos:=do_filepos(handle);
  do_filesize:=do_seekend(handle);
  do_seek(handle,aktfilepos);
end;


{ truncate at a given position }
procedure do_truncate (handle,pos:longint);
var
  regs : trealregs;
begin
  do_seek(handle,pos);
  regs.realecx:=0;
  regs.realedx:=tb_offset;
  regs.realds:=tb_segment;
  regs.realebx:=handle;
  regs.realeax:=$4000;
  sysrealintr($21,regs);
  if (regs.realflags and carryflag) <> 0 then
   GetInOutRes(lo(regs.realeax));
end;

const
  FileHandleCount : longint = 20;

function Increase_file_handle_count : boolean;
var
  regs : trealregs;
begin
  Inc(FileHandleCount,10);
  regs.realebx:=FileHandleCount;
  regs.realeax:=$6700;
  sysrealintr($21,regs);
  if (regs.realflags and carryflag) <> 0 then
   begin
    Increase_file_handle_count:=false;
    Dec (FileHandleCount, 10);
   end
  else
    Increase_file_handle_count:=true;
end;

procedure do_open(var f;p:pchar;flags:longint);
{
  filerec and textrec have both handle and mode as the first items so
  they could use the same routine for opening/creating.
  when (flags and $100)   the file will be append
  when (flags and $1000)  the file will be truncate/rewritten
  when (flags and $10000) there is no check for close (needed for textfiles)
}
var
  regs   : trealregs;
  action : longint;
begin
  AllowSlash(p);
{ close first if opened }
  if ((flags and $10000)=0) then
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
  action:=$1;
{ convert filemode to filerec modes }
  case (flags and 3) of
   0 : filerec(f).mode:=fminput;
   1 : filerec(f).mode:=fmoutput;
   2 : filerec(f).mode:=fminout;
  end;
  if (flags and $1000)<>0 then
   action:=$12; {create file function}
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
{ real dos call }
  syscopytodos(longint(p),strlen(p)+1);
  if LFNSupport then
   regs.realeax:=$716c
  else
   regs.realeax:=$6c00;
  regs.realedx:=action;
  regs.realds:=tb_segment;
  regs.realesi:=tb_offset;
  regs.realebx:=$2000+(flags and $ff);
  regs.realecx:=$20;
  sysrealintr($21,regs);
  if (regs.realflags and carryflag) <> 0 then
    if lo(regs.realeax)=4 then
      if Increase_file_handle_count then
        begin
          { Try again }
            if LFNSupport then
             regs.realeax:=$716c
            else
             regs.realeax:=$6c00;
          regs.realedx:=action;
          regs.realds:=tb_segment;
          regs.realesi:=tb_offset;
          regs.realebx:=$2000+(flags and $ff);
          regs.realecx:=$20;
          sysrealintr($21,regs);
        end;
  if (regs.realflags and carryflag) <> 0 then
    begin
      GetInOutRes(lo(regs.realeax));
      exit;
    end
  else
    begin
      filerec(f).handle:=lo(regs.realeax);
      { for systems that have more then 20 by default ! }
      if lo(regs.realeax)>FileHandleCount then
        FileHandleCount:=lo(regs.realeax);
    end;
  if lo(regs.realeax)<max_files then
    begin
{$ifdef SYSTEMDEBUG}
       if openfiles[lo(regs.realeax)] and
          assigned(opennames[lo(regs.realeax)]) then
         begin
            Writeln(stderr,'file ',opennames[lo(regs.realeax)],'(',lo(regs.realeax),') not closed but handle reused!');
            sysfreememsize(opennames[lo(regs.realeax)],strlen(opennames[lo(regs.realeax)])+1);
         end;
{$endif SYSTEMDEBUG}
       openfiles[lo(regs.realeax)]:=true;
{$ifdef SYSTEMDEBUG}
       opennames[lo(regs.realeax)] := sysgetmem(strlen(p)+1);
       move(p^,opennames[lo(regs.realeax)]^,strlen(p)+1);
{$endif SYSTEMDEBUG}
    end;
{ append mode }
  if (flags and $100)<>0 then
   begin
     do_seekend(filerec(f).handle);
     filerec(f).mode:=fmoutput; {fool fmappend}
   end;
end;


function do_isdevice(handle:longint):boolean;
var
  regs : trealregs;
begin
  regs.realebx:=handle;
  regs.realeax:=$4400;
  sysrealintr($21,regs);
  do_isdevice:=(regs.realedx and $80)<>0;
  if (regs.realflags and carryflag) <> 0 then
   GetInOutRes(lo(regs.realeax));
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
                           Generic Handling
*****************************************************************************}

{$ifdef TEST_GENERIC}
{$i generic.inc}
{$endif TEST_GENERIC}

{*****************************************************************************
                           Directory Handling
*****************************************************************************}

procedure DosDir(func:byte;const s:string);
var
  buffer : array[0..255] of char;
  regs   : trealregs;
begin
  move(s[1],buffer,length(s));
  buffer[length(s)]:=#0;
  AllowSlash(pchar(@buffer));
  { True DOS does not like backslashes at end
    Win95 DOS accepts this !!
    but "\" and "c:\" should still be kept and accepted hopefully PM }
  if (length(s)>0) and (buffer[length(s)-1]='\') and
     Not ((length(s)=1) or ((length(s)=3) and (s[2]=':'))) then
    buffer[length(s)-1]:=#0;
  syscopytodos(longint(@buffer),length(s)+1);
  regs.realedx:=tb_offset;
  regs.realds:=tb_segment;
  if LFNSupport then
   regs.realeax:=$7100+func
  else
   regs.realeax:=func shl 8;
  sysrealintr($21,regs);
  if (regs.realflags and carryflag) <> 0 then
   GetInOutRes(lo(regs.realeax));
end;


procedure mkdir(const s : string);[IOCheck];
begin
  If (s='') or (InOutRes <> 0) then
   exit;
  DosDir($39,s);
end;


procedure rmdir(const s : string);[IOCheck];
begin
  if (s = '.' ) then
    InOutRes := 16;
  If (s='') or (InOutRes <> 0) then
   exit;
  DosDir($3a,s);
end;


procedure chdir(const s : string);[IOCheck];
var
  regs : trealregs;
begin
  If (s='') or (InOutRes <> 0) then
   exit;
{ First handle Drive changes }
  if (length(s)>=2) and (s[2]=':') then
   begin
     regs.realedx:=(ord(s[1]) and (not 32))-ord('A');
     regs.realeax:=$0e00;
     sysrealintr($21,regs);
     regs.realeax:=$1900;
     sysrealintr($21,regs);
     if byte(regs.realeax)<>byte(regs.realedx) then
      begin
        Inoutres:=15;
        exit;
      end;
     { DosDir($3b,'c:') give Path not found error on
       pure DOS PM }
     if length(s)=2 then
       exit;
   end;
{ do the normal dos chdir }
  DosDir($3b,s);
end;


procedure GetDir (DriveNr: byte; var Dir: ShortString);
var
  temp : array[0..255] of char;
  i    : longint;
  regs : trealregs;
begin
  regs.realedx:=drivenr;
  regs.realesi:=tb_offset;
  regs.realds:=tb_segment;
  if LFNSupport then
   regs.realeax:=$7147
  else
   regs.realeax:=$4700;
  sysrealintr($21,regs);
  if (regs.realflags and carryflag) <> 0 then
   Begin
     GetInOutRes (lo(regs.realeax));
     Dir := char (DriveNr + 64) + ':\';
     exit;
   end
  else
   syscopyfromdos(longint(@temp),251);
{ conversion to Pascal string including slash conversion }
  i:=0;
  while (temp[i]<>#0) do
   begin
     if temp[i]='/' then
      temp[i]:='\';
     dir[i+4]:=temp[i];
     inc(i);
   end;
  dir[2]:=':';
  dir[3]:='\';
  dir[0]:=char(i+3);
{ upcase the string }
  if not FileNameCaseSensitive then
   dir:=upcase(dir);
  if drivenr<>0 then   { Drive was supplied. We know it }
   dir[1]:=char(65+drivenr-1)
  else
   begin
   { We need to get the current drive from DOS function 19H  }
   { because the drive was the default, which can be unknown }
     regs.realeax:=$1900;
     sysrealintr($21,regs);
     i:= (regs.realeax and $ff) + ord('A');
     dir[1]:=chr(i);
   end;
end;


{*****************************************************************************
                         SystemUnit Initialization
*****************************************************************************}

function CheckLFN:boolean;
var
  regs     : TRealRegs;
  RootName : pchar;
begin
{ Check LFN API on drive c:\ }
  RootName:='C:\';
  syscopytodos(longint(RootName),strlen(RootName)+1);
{ Call 'Get Volume Information' ($71A0) }
  regs.realeax:=$71a0;
  regs.reales:=tb_segment;
  regs.realedi:=tb_offset;
  regs.realecx:=32;
  regs.realds:=tb_segment;
  regs.realedx:=tb_offset;
  regs.realflags:=carryflag;
  sysrealintr($21,regs);
{ If carryflag=0 and LFN API bit in ebx is set then use Long file names }
  CheckLFN:=(regs.realflags and carryflag=0) and (regs.realebx and $4000=$4000);
end;

{$ifdef  EXCEPTIONS_IN_SYSTEM}
{$define IN_SYSTEM}
{$i dpmiexcp.pp}
{$endif  EXCEPTIONS_IN_SYSTEM}

procedure SysInitStdIO;
begin
  OpenStdIO(Input,fmInput,StdInputHandle);
  OpenStdIO(Output,fmOutput,StdOutputHandle);
  OpenStdIO(StdOut,fmOutput,StdOutputHandle);
  OpenStdIO(StdErr,fmOutput,StdErrorHandle);
end;


var
  temp_int : tseginfo;
Begin
  StackLength := InitialStkLen;
  StackBottom := __stkbottom;
  { To be set if this is a GUI or console application }
  IsConsole := TRUE;
  { To be set if this is a library and not a program  }
  IsLibrary := FALSE;
{ save old int 0 and 75 }
  get_pm_interrupt($00,old_int00);
  get_pm_interrupt($75,old_int75);
  temp_int.segment:=get_cs;
  temp_int.offset:=@new_int00;
  set_pm_interrupt($00,temp_int);
{$ifndef EXCEPTIONS_IN_SYSTEM}
  temp_int.offset:=@new_int75;
  set_pm_interrupt($75,temp_int);
{$endif EXCEPTIONS_IN_SYSTEM}
{ Setup heap }
  InitHeap;
  SysInitExceptions;
{ Setup stdin, stdout and stderr }
  SysInitStdIO;
{ Setup environment and arguments }
  Setup_Environment;
  Setup_Arguments;
{ Use LFNSupport LFN }
  LFNSupport:=CheckLFN;
  if LFNSupport then
   FileNameCaseSensitive:=true;
{ Reset IO Error }
  InOutRes:=0;
{$ifdef  EXCEPTIONS_IN_SYSTEM}
  InitDPMIExcp;
  InstallDefaultHandlers;
{$endif  EXCEPTIONS_IN_SYSTEM}
{$ifdef HASVARIANT}
  initvariantmanager;
{$endif HASVARIANT}
End.
{
  $Log$
  Revision 1.23  2002-10-14 19:39:16  peter
    * threads unit added for thread support

  Revision 1.22  2002/10/13 09:28:44  florian
    + call to initvariantmanager inserted

  Revision 1.21  2002/09/07 21:32:08  carl
    - removed unused defines

  Revision 1.20  2002/09/07 16:01:19  peter
    * old logs removed and tabs fixed

  Revision 1.19  2002/07/01 16:29:05  peter
    * sLineBreak changed to normal constant like Kylix

  Revision 1.18  2002/05/05 10:23:54  peter
    * fixed memw and meml array sizes

  Revision 1.17  2002/04/21 15:52:58  carl
  + initialize some global variables

  Revision 1.16  2002/04/12 17:34:05  carl
  + generic stack checking

  Revision 1.15  2002/03/11 19:10:33  peter
    * Regenerated with updated fpcmake

}
