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

{ include system-independent routine headers }

{$I systemh.inc}

{ include heap support headers }

{$I heaph.inc}

const
{ Default filehandles }
  UnusedHandle    = -1;
  StdInputHandle  = 0;
  StdOutputHandle = 1;
  StdErrorHandle  = 2;

  FileNameCaseSensitive : boolean = false;

{ Default memory segments (Tp7 compatibility) }
  seg0040 = $0040;
  segA000 = $A000;
  segB000 = $B000;
  segB800 = $B800;

var
{ Mem[] support }
  mem  : array[0..$7fffffff] of byte absolute $0:$0;
  memw : array[0..$7fffffff] of word absolute $0:$0;
  meml : array[0..$7fffffff] of longint absolute $0:$0;
{ C-compatible arguments and environment }
  argc  : longint;
  argv  : ppchar;
  envp  : ppchar;
  dos_argv0 : pchar;

{$ifndef RTLLITE}
{ System info }
  LFNSupport : boolean;
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
  doscmd    : string[128];  { Dos commandline copied from PSP, max is 128 chars }
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

type
  arrayword = array [0..255] of word;
var
  psp : word;
  i,j : longint;
  quote : char;
  proxy_s : string[50];
  al,proxy_argc,proxy_seg,proxy_ofs,lin : longint;
  largs : array[0..127] of pchar;
  rm_argv : ^arrayword;
  argv0len : longint;
  useproxy : boolean;
  hp : ppchar;
begin
  fillchar(largs,sizeof(largs),0);
  psp:=stub_info^.psp_selector;
  largs[0]:=dos_argv0;
  argc := 1;
  sysseg_move(psp, 128, get_ds, longint(@doscmd), 128);
{$IfDef SYSTEM_DEBUG_STARTUP}
  Writeln(stderr,'Dos command line is #',doscmd,'# size = ',length(doscmd));
{$EndIf }

{ setup cmdline variable }
  argv0len:=strlen(dos_argv0);
  cmdline:=sysgetmem(argv0len+length(doscmd)+1);
  move(dos_argv0^,cmdline^,argv0len);
  move(doscmd[1],cmdline[argv0len],length(doscmd));
  cmdline[argv0len+length(doscmd)]:=#0;

  j := 1;
  quote := #0;
  for i:=1 to length(doscmd) do
   Begin
     if doscmd[i] = quote then
      begin
        quote := #0;
        if (i>1) and ((doscmd[i-1]='''') or (doscmd[i-1]='"')) then
          begin
          j := i+1;
          doscmd[i] := #0;
          continue;
          end;
        doscmd[i] := #0;
        largs[argc]:=@doscmd[j];
        inc(argc);
        j := i+1;
      end
     else
      if (quote = #0) and ((doscmd[i] = '''') or (doscmd[i]='"')) then
       begin
         quote := doscmd[i];
         j := i + 1;
       end else
     if (quote = #0) and ((doscmd[i] = ' ')
       or (doscmd[i] = #9) or (doscmd[i] = #10) or
       (doscmd[i] = #12) or (doscmd[i] = #9)) then
       begin
       doscmd[i]:=#0;
       if j<i then
         begin
         largs[argc]:=@doscmd[j];
         inc(argc);
         j := i+1;
         end else inc(j);
       end else
     if (i = length(doscmd)) then
       begin
       doscmd[i+1]:=#0;
       largs[argc]:=@doscmd[j];
       inc(argc);
       end;
  end;

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
           largs[2]:=@proxy_s[9];
           largs[3]:=@proxy_s[14];
           largs[4]:=@proxy_s[19];
           useproxy:=true;
           break;
         end;
      end;
     inc(hp);
   end;

  if (not useproxy) and
     (argc > 1) and (far_strlen(get_ds,longint(largs[1])) = 6)  then
   begin
     move(largs[1]^,proxy_s[1],6);
     proxy_s[0] := #6;
     if (proxy_s = '!proxy') then
      useproxy:=true;
   end;

  if useproxy then
   begin
     proxy_argc := atohex(largs[2]);
     proxy_seg  := atohex(largs[3]);
     proxy_ofs := atohex(largs[4]);
{$IfDef SYSTEM_DEBUG_STARTUP}
     Writeln(stderr,'proxy command line found');
     writeln(stderr,'argc: ',proxy_argc,' seg: ',proxy_seg,' ofs: ',proxy_ofs);
{$EndIf SYSTEM_DEBUG_STARTUP}
     if proxy_argc>128 then
      proxy_argc:=128;
     rm_argv := sysgetmem(proxy_argc*sizeof(word));
     sysseg_move(dos_selector,proxy_seg*16+proxy_ofs, get_ds,longint(rm_argv),proxy_argc*sizeof(word));
     for i:=0 to proxy_argc - 1 do
      begin
        lin := proxy_seg*16 + rm_argv^[i];
        al :=far_strlen(dos_selector, lin);
        largs[i] := sysgetmem(al+1);
        sysseg_move(dos_selector, lin, get_ds,longint(largs[i]), al+1);
{$IfDef SYSTEM_DEBUG_STARTUP}
        Writeln(stderr,'arg ',i,' #',rm_argv^[i],'#',al,'#',largs[i],'#');
{$EndIf SYSTEM_DEBUG_STARTUP}
      end;
     sysfreemem(rm_argv);
     argc := proxy_argc;
   end;
  argv := sysgetmem(argc shl 2);
  for i := 0 to argc-1  do
   argv[i]:=largs[i];
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


procedure getinoutres;
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
  end;
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

procedure ___exit(exitcode:byte);cdecl;external name '___exit';

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
  set_pm_interrupt($75,old_int75);
  ___exit(exitcode);
end;


procedure halt(errnum : byte);
begin
  exitcode:=errnum;
  do_exit;
  { do_exit should call system_exit but this does not hurt }
  System_exit;
end;

procedure new_int00;
begin
  HandleError(200);
end;

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


var
  __stkbottom : longint;external name '__stkbottom';

procedure int_stackcheck(stack_size:longint);[public,alias:'FPC_STACKCHECK'];
{
  called when trying to get local stack if the compiler directive $S
  is set this function must preserve esi !!!! because esi is set by
  the calling proc for methods it must preserve all registers !!

  With a 2048 byte safe area used to write to StdIo without crossing
  the stack boundary
}
begin
  asm
        pushl   %eax
        pushl   %ebx
        movl    stack_size,%ebx
        addl    $2048,%ebx
        movl    %esp,%eax
        subl    %ebx,%eax
{$ifdef SYSTEMDEBUG}
        movl    loweststack,%ebx
        cmpl    %eax,%ebx
        jb      .L_is_not_lowest
        movl    %eax,loweststack
.L_is_not_lowest:
{$endif SYSTEMDEBUG}
        movl    __stkbottom,%ebx
        cmpl    %eax,%ebx
        jae     .L__short_on_stack
        popl    %ebx
        popl    %eax
        leave
        ret     $4
.L__short_on_stack:
        { can be usefull for error recovery !! }
        popl    %ebx
        popl    %eax
  end['EAX','EBX'];
  HandleError(202);
end;


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
  hl:=regs.realedx and $ffff;
  randseed:=hl*$10000+ (regs.realecx and $ffff);
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
        movl    size,%eax
        pushl   %eax
        call    ___sbrk
        addl    $4,%esp
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
   GetInOutRes;
end;


procedure do_erase(p : pchar);
var
  regs : trealregs;
begin
  AllowSlash(p);
  syscopytodos(longint(p),strlen(p)+1);
  regs.realedx:=tb_offset;
  regs.realds:=tb_segment;
{$ifndef RTLLITE}
  if LFNSupport then
   regs.realeax:=$7141
  else
{$endif RTLLITE}
   regs.realeax:=$4100;
  regs.realesi:=0;
  regs.realecx:=0;
  sysrealintr($21,regs);
  if (regs.realflags and carryflag) <> 0 then
   GetInOutRes;
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
{$ifndef RTLLITE}
  if LFNSupport then
   regs.realeax:=$7156
  else
{$endif RTLLITE}
   regs.realeax:=$5600;
  regs.realecx:=$ff;            { attribute problem here ! }
  sysrealintr($21,regs);
  if (regs.realflags and carryflag) <> 0 then
   GetInOutRes;
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
        GetInOutRes;
        exit(writesize);
      end;
     inc(writesize,regs.realeax);
     dec(len,regs.realeax);
     { stop when not the specified size is written }
     if regs.realeax<size then
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
        GetInOutRes;
        do_read:=0;
        exit;
      end;
     syscopyfromdos(addr+readsize,regs.realeax);
     inc(readsize,regs.realeax);
     dec(len,regs.realeax);
     { stop when not the specified size is read }
     if regs.realeax<size then
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
     GetInOutRes;
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
   GetInOutRes;
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
     GetInOutRes;
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
   GetInOutRes;
end;

{$ifndef RTLLITE}
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
    Increase_file_handle_count:=false
  else
    Increase_file_handle_count:=true;
end;
{$endif not RTLLITE}

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
{$ifndef RTLLITE}
  if LFNSupport then
   regs.realeax:=$716c
  else
{$endif RTLLITE}
   regs.realeax:=$6c00;
  regs.realedx:=action;
  regs.realds:=tb_segment;
  regs.realesi:=tb_offset;
  regs.realebx:=$2000+(flags and $ff);
  regs.realecx:=$20;
  sysrealintr($21,regs);
{$ifndef RTLLITE}
  if (regs.realflags and carryflag) <> 0 then
    if (regs.realeax and $ffff)=4 then
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
{$endif RTLLITE}
  if (regs.realflags and carryflag) <> 0 then
    begin
      GetInOutRes;
      exit;
    end
  else
    begin
      filerec(f).handle:=regs.realeax;
{$ifndef RTLLITE}
      { for systems that have more then 20 by default ! }
      if regs.realeax>FileHandleCount then
        FileHandleCount:=regs.realeax;
{$endif RTLLITE}
    end;
  if regs.realeax<max_files then
    begin
{$ifdef SYSTEMDEBUG}
       if openfiles[regs.realeax] and
          assigned(opennames[regs.realeax]) then
         begin
            Writeln(stderr,'file ',opennames[regs.realeax],'(',regs.realeax,') not closed but handle reused!');
            sysfreememsize(opennames[regs.realeax],strlen(opennames[regs.realeax])+1);
         end;
{$endif SYSTEMDEBUG}
       openfiles[regs.realeax]:=true;
{$ifdef SYSTEMDEBUG}
       opennames[regs.realeax] := sysgetmem(strlen(p)+1);
       move(p^,opennames[regs.realeax]^,strlen(p)+1);
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
   GetInOutRes;
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
  syscopytodos(longint(@buffer),length(s)+1);
  regs.realedx:=tb_offset;
  regs.realds:=tb_segment;
{$ifndef RTLLITE}
  if LFNSupport then
   regs.realeax:=$7100+func
  else
{$endif RTLLITE}
   regs.realeax:=func shl 8;
  sysrealintr($21,regs);
  if (regs.realflags and carryflag) <> 0 then
   GetInOutRes;
end;


procedure mkdir(const s : string);[IOCheck];
begin
  If InOutRes <> 0 then
   exit;
  DosDir($39,s);
end;


procedure rmdir(const s : string);[IOCheck];
begin
  If InOutRes <> 0 then
   exit;
  DosDir($3a,s);
end;


procedure chdir(const s : string);[IOCheck];
var
  regs : trealregs;
begin
  If InOutRes <> 0 then
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


procedure getdir(drivenr : byte;var dir : shortstring);
var
  temp : array[0..255] of char;
  i    : longint;
  regs : trealregs;
begin
  regs.realedx:=drivenr;
  regs.realesi:=tb_offset;
  regs.realds:=tb_segment;
{$ifndef RTLLITE}
  if LFNSupport then
   regs.realeax:=$7147
  else
{$endif RTLLITE}
   regs.realeax:=$4700;
  sysrealintr($21,regs);
  if (regs.realflags and carryflag) <> 0 then
   Begin
     GetInOutRes;
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

{$ifndef RTLLITE}
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
{$endif RTLLITE}

{$ifdef MT}
{$I thread.inc}
{$endif MT}

var
  temp_int : tseginfo;
Begin
{ save old int 0 and 75 }
  get_pm_interrupt($00,old_int00);
  get_pm_interrupt($75,old_int75);
  temp_int.segment:=get_cs;
  temp_int.offset:=@new_int00;
  set_pm_interrupt($00,temp_int);
{  temp_int.offset:=@new_int75;
  set_pm_interrupt($75,temp_int); }
{ to test stack depth }
  loweststack:=maxlongint;
{ Setup heap }
  InitHeap;
{$ifdef MT}
  { before this, you can't use thread vars !!!! }
  { threadvarblocksize is calculate before the initialization }
  { of the system unit                                        }
  mainprogramthreadblock :=  sysgetmem(threadvarblocksize);
{$endif MT}
  InitExceptions;
{ Setup stdin, stdout and stderr }
  OpenStdIO(Input,fmInput,StdInputHandle);
  OpenStdIO(Output,fmOutput,StdOutputHandle);
  OpenStdIO(StdOut,fmOutput,StdOutputHandle);
  OpenStdIO(StdErr,fmOutput,StdErrorHandle);
{ Setup environment and arguments }
  Setup_Environment;
  Setup_Arguments;
{ Use LFNSupport LFN }
  LFNSupport:=CheckLFN;
  if LFNSupport then
   FileNameCaseSensitive:=true;
{ Reset IO Error }
  InOutRes:=0;
End.
{
  $Log$
  Revision 1.31  2000-01-24 11:57:18  daniel
    * !proxy support in environment added (Peter)

  Revision 1.30  2000/01/20 23:38:02  peter
    * support fm_inout as stdoutput for assign(f,'');rewrite(f,1); becuase
      rewrite opens always with filemode 2

  Revision 1.29  2000/01/16 22:25:38  peter
    * check handle for file closing

  Revision 1.28  2000/01/07 16:41:32  daniel
    * copyright 2000

  Revision 1.27  2000/01/07 16:32:23  daniel
    * copyright 2000 added

  Revision 1.26  1999/12/20 22:22:41  pierre
   * better closing of left open files

  Revision 1.25  1999/12/17 23:11:48  pierre
   * fix for bug754 : increase now dynamically max open handles

  Revision 1.24  1999/12/01 22:57:30  peter
    * cmdline support

  Revision 1.23  1999/11/25 16:24:56  pierre
   * avoid a problem with ChDir('c:') on pure DOS

  Revision 1.22  1999/11/06 14:38:24  peter
    * truncated log

  Revision 1.21  1999/10/31 09:34:48  jonas
    * updated for new syntax of sysgetmem

  Revision 1.20  1999/10/28 09:53:19  peter
    * create can also open file in fminout

  Revision 1.19  1999/09/20 12:40:20  pierre
   * adapted to new heaph

  Revision 1.18  1999/09/10 17:14:09  peter
    * better errorcode returning using int21h,5900

  Revision 1.17  1999/09/10 15:40:33  peter
    * fixed do_open flags to be > $100, becuase filemode can be upto 255

  Revision 1.16  1999/09/08 16:09:18  peter
    * do_isdevice not called if already error

  Revision 1.15  1999/08/19 14:03:16  pierre
   * use sysgetmem for startup and debug allocations

  Revision 1.14  1999/07/19 07:57:49  michael
  + Small fix from Michael Baikov in setup_params

  Revision 1.13  1999/05/19 16:54:21  pierre
   * closes all handles >+ 5

  Revision 1.12  1999/05/17 21:52:33  florian
    * most of the Object Pascal stuff moved to the system unit

  Revision 1.11  1999/05/04 23:28:40  pierre
    SYSTEM_DEBUG_STARTUP used to output args and env at start

  Revision 1.10  1999/04/28 11:42:45  peter
    + FileNameCaseSensetive boolean

  Revision 1.9  1999/04/28 06:01:25  florian
    * define MT for multithreading introduced

  Revision 1.8  1999/04/08 12:23:02  peter
    * removed os.inc

  Revision 1.7  1999/03/10 22:15:28  florian
    + system.cmdline variable for go32v2 and win32 added

  Revision 1.6  1999/03/01 15:40:52  peter
    * use external names
    * removed all direct assembler modes

  Revision 1.5  1999/01/18 10:05:50  pierre
   + system_exit procedure added

  Revision 1.4  1998/12/30 22:17:59  peter
    * fixed mem decls to use $0:$0

  Revision 1.3  1998/12/28 15:50:45  peter
    + stdout, which is needed when you write something in the system unit
      to the screen. Like the runtime error

  Revision 1.2  1998/12/21 14:22:02  pierre
   * old_int?? transformed to cvar to be readable by dpmiexcp

}
