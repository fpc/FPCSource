{
    Copyright (c) 1998-2002 by Florian Klaempfl

    This unit contains information about the target systems supported
    (these are not processor specific)

    This program is free software; you can redistribute it and/or modify
    iu under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge- MA 02139, USA.

 ****************************************************************************
}
unit systems;

{$i fpcdefs.inc}

interface


   type
       tendian = (endian_little,endian_big);

     (*
       IMPORTANT NOTE:
       The value of this enumeration is stored in PPU files.
       Therefore adding new CPU targets should not change the
       values of the pre-existing targets. (CEC)
       FURTHERMORE : Make sure that this branch values, are
       consistant with the main branch version always.
     *)
       tsystemcpu=
       (
             cpu_no,                       { 0 }
             cpu_i386,                     { 1 }
             cpu_m68k,                     { 2 }
             cpu_alpha,                    { 3 }
             cpu_powerpc,                  { 4 }
             cpu_sparc,                    { 5 }
             cpu_vm,                       { 6 }
             cpu_iA64,                     { 7 }
             cpu_x86_64,                   { 8 }
             cpu_mips,                     { 9 }
             cpu_arm,                      { 10 }
             cpu_powerpc64                 { 11 }
       );

       tasmmode= (asmmode_none
            { standard assembler (cpu dependant) with full parsing }
            ,asmmode_standard
            ,asmmode_i386_att
            ,asmmode_i386_intel
            ,asmmode_ppc_gas
            ,asmmode_ppc_motorola
            ,asmmode_arm_gas
            ,asmmode_sparc_gas
            ,asmmode_x86_64_gas
            ,asmmode_m68k_mot
            ,asmmode_x86_64_intel
       );

     (* IMPORTANT NOTE:
       the integer value of this enum is stored in PPU
       files to recognize the target, so if you add new targets
       allways add them at end PM
       FURTHERMORE : Make sure that this branch values are
       consistant with the main branch version always. (CEC)
       *)
     type
       tsystem =
       (
             system_none,               { 0 }
             obsolete_system_i386_GO32V1,{ 1 }
             system_i386_GO32V2,        { 2 }
             system_i386_linux,         { 3 }
             system_i386_OS2,           { 4 }
             system_i386_Win32,         { 5 }
             system_i386_freebsd,       { 6 }
             system_m68k_Amiga,         { 7 }
             system_m68k_Atari,         { 8 }
             system_m68k_Mac,           { 9 }
             system_m68k_linux,         { 10 }
             system_m68k_PalmOS,        { 11 }
             system_alpha_linux,        { 12 }
             system_powerpc_linux,      { 13 }
             system_powerpc_macos,      { 14 }
             system_i386_solaris,       { 15 }
             system_i386_beos,          { 16 }
             system_i386_netbsd,        { 17 }
             system_m68k_netbsd,        { 18 }
             system_i386_Netware,       { 19 }
             system_i386_qnx,           { 20 }
             system_i386_wdosx,         { 21 }
             system_sparc_solaris,      { 22 }
             system_sparc_linux,        { 23 }
             system_i386_openbsd,       { 24 }
             system_m68k_openbsd,       { 25 }
             system_x86_64_linux,       { 26 }
             system_powerpc_darwin,     { 27 }
             system_i386_EMX,           { 28 }
             system_powerpc_netbsd,     { 29 }
             system_powerpc_openbsd,    { 30 }
             system_arm_linux,          { 31 }
             system_i386_watcom,        { 32 }
             system_powerpc_MorphOS,    { 33 }
             system_x86_64_freebsd,     { 34 }
             system_i386_netwlibc,      { 35 }
             system_powerpc_Amiga,      { 36 }
             system_x86_64_win64,       { 37 }
             system_arm_wince,          { 38 }
             system_ia64_win64,         { 39 }
             system_i386_wince,         { 40 }
             system_x86_6432_linux,     { 41 }
             system_arm_gba,            { 42 }
             system_powerpc64_linux,    { 43 }
             system_i386_darwin,        { 44 }
             system_arm_palmos,         { 45 }
             system_powerpc64_darwin,   { 46 }
             system_arm_nds,            { 47 }
             system_i386_embedded,      { 48 }
             system_m68k_embedded,      { 49 }
             system_alpha_embedded,     { 50 }
             system_powerpc_embedded,   { 51 }
             system_sparc_embedded,     { 52 }
             system_vm_embedded,        { 53 }
             system_iA64_embedded,      { 54 }
             system_x86_64_embedded,    { 55 }
             system_mips_embedded,      { 56 }
             system_arm_embedded,       { 57 }
             system_powerpc64_embedded  { 58 }
       );

       tasm = (as_none
             ,as_gas                   { standard gnu assembler }
             ,as_i386_as_aout
             ,as_i386_nasmcoff
             ,as_i386_nasmwin32
             ,as_i386_nasmwdosx
             ,as_i386_nasmelf
             ,as_i386_nasmobj
             ,as_i386_nasmbeos
             ,as_i386_tasm
             ,as_i386_masm
             ,as_i386_wasm
             ,as_i386_coff
             ,as_i386_pecoff
             ,as_i386_elf32
             ,as_i386_pecoffwdosx
             ,as_m68k_mit
             ,as_powerpc_mpw
             ,as_darwin
             ,as_x86_64_masm
             ,as_x86_64_pecoff
             ,as_i386_pecoffwince
             ,as_arm_pecoffwince
             ,as_x86_64_elf64
             ,as_sparc_elf32
             ,as_ggas                  { gnu assembler called "gas" instead of "as" }
       );

       tar = (ar_none
            ,ar_gnu_ar
            ,ar_mpw_ar
            ,ar_gnu_ar_scripted
            ,ar_gnu_gar
       );

       tres = (res_none
            ,res_gnu_windres,res_emxbind
            ,res_m68k_palmos,res_m68k_mpw
            ,res_powerpc_mpw,res_elf
            ,res_gnu_wince_windres
       );

       tdbg = (dbg_none
            ,dbg_stabs,dbg_dwarf2,dbg_dwarf3
       );

       tscripttype = (script_none
            ,script_dos,script_unix,script_amiga,
            script_mpw
       );

       tabi = (abi_default
            ,abi_powerpc_sysv,abi_powerpc_aix
       );

{*****************************************************************************
                               Structures
*****************************************************************************}

     type
       { Abstract linker class which is implemented in link module }
       TAbstractLinker = class
       end;

       TAbstractLinkerClass = class of TAbstractLinker;


       { Abstract assembler class which is implemented in assemble module }
       TAbstractAssembler = class
       end;

       TAbstractAssemblerClass = class of TAbstractAssembler;


       palignmentinfo = ^talignmentinfo;
       talignmentinfo = record
         procalign,
         loopalign,
         jumpalign,
         constalignmin,
         constalignmax,
         varalignmin,
         varalignmax,
         localalignmin,
         localalignmax,
         recordalignmin,
         recordalignmax,
         maxCrecordalign : longint;
       end;

       tasmflags = (af_none,
         af_outputbinary,af_allowdirect,
         af_needar,af_smartlink_sections,
         af_labelprefix_only_inside_procedure,
         af_supports_dwarf
       );

       pasminfo = ^tasminfo;
       tasminfo = record
          id          : tasm;
          idtxt       : string[12];
          asmbin      : string[8];
          asmcmd      : string[50];
          supported_target : tsystem;
          flags        : set of tasmflags;
          labelprefix : string[3];
          comment     : string[3];
       end;

       parinfo = ^tarinfo;
       tarinfo = record
          id          : tar;
          arcmd       : string[50];
          arfinishcmd : string[10];
       end;

       presinfo = ^tresinfo;
       tresinfo = record
          id      : tres;
          resbin  : string[8];
          rescmd  : string[50];
       end;

       pdbginfo = ^tdbginfo;
       tdbginfo = record
          id      : tdbg;
          idtxt   : string[12];
       end;

       tsystemflags = (tf_none,
            tf_under_development,
            tf_need_export,
            tf_needs_isconsole,
            tf_code_small,
            tf_static_reg_based,
            tf_needs_symbol_size,
            tf_smartlink_sections,
            tf_needs_dwarf_cfi,
            tf_use_8_3,
            tf_pic_uses_got,
            tf_library_needs_pic,
            tf_needs_symbol_type,
            tf_section_threadvars,
            tf_files_case_sensitive,
            tf_files_case_aware,
            tf_p_ext_support,
            tf_has_dllscanner,
            tf_use_function_relative_addresses,
            tf_winlikewidestring
       );

       psysteminfo = ^tsysteminfo;
       { using packed causes bus errors on processors which require alignment }
       tsysteminfo = record
          system       : tsystem;
          name         : string[34];
          shortname    : string[9];
          flags        : set of tsystemflags;
          cpu          : tsystemcpu;
          unit_env     : string[16];
          extradefines : string[40];
          exeext,
          defext,
          scriptext,
          smartext,
          unitext,
          unitlibext,
          asmext,
          objext,
          resext,
          resobjext    : string[4];
          sharedlibext : string[10];
          staticlibext,
          staticlibprefix : string[4];
          sharedlibprefix : string[4];
          sharedClibext : string[10];
          staticClibext,
          staticClibprefix : string[4];
          sharedClibprefix : string[4];
          Cprefix      : string[2];
          newline      : string[2];
          dirsep       : char;
          assem        : tasm;
          assemextern  : tasm; { external assembler, used by -a }
          link         : tabstractlinkerclass;
          linkextern   : tabstractlinkerclass;  { external linker, used by -s }
          ar           : tar;
          res          : tres;
          dbg          : tdbg;
          script       : tscripttype;
          endian       : tendian;
          alignment    : talignmentinfo;
          {
            Offset from the argument pointer register to the first
            argument's address. On some machines it may depend on
            the data type of the function.
            (see also FIRST_PARM_OFFSET in GCC source)
          }
          first_parm_offset : longint;
          stacksize    : longint;
          abi          : tabi;
       end;

    const
       { alias for supported_target field in tasminfo }
       system_any = system_none;

       system_wince = [system_arm_wince,system_i386_wince];
       system_linux = [system_i386_linux,system_x86_64_linux,system_powerpc_linux,
                       system_arm_linux,system_sparc_linux,system_alpha_linux,system_m68k_linux,
                       system_x86_6432_linux];
       { all real windows systems, no cripple ones like wince, wdosx et. al. }
       system_windows = [system_i386_win32,system_x86_64_win64,system_ia64_win64];
       { all windows systems }
       system_all_windows = [system_i386_win32,system_x86_64_win64,system_ia64_win64,
                             system_arm_wince,system_i386_wince];

       { all systems supporting exports from programs or units }
       system_unit_program_exports = [system_i386_win32,
                                         system_i386_wdosx,
                                         system_i386_Netware,
                                         system_i386_netwlibc,
                                         system_arm_wince,
                                         system_x86_64_win64,
                                         system_ia64_win64]+system_linux;

       system_internal_sysinit = [system_i386_linux,system_i386_win32];

       system_embedded = [system_i386_embedded,system_m68k_embedded,system_alpha_embedded,
             system_powerpc_embedded,system_sparc_embedded,system_vm_embedded,
             system_iA64_embedded,system_x86_64_embedded,system_mips_embedded,
             system_arm_embedded,system_powerpc64_embedded];


       cpu2str : array[TSystemCpu] of string =
            ('','i386','m68k','alpha','powerpc','sparc','vm','ia64','x86_64',
             'mips','arm', 'powerpc64');

    var
       targetinfos   : array[tsystem] of psysteminfo;
       arinfos       : array[tar] of parinfo;
       resinfos      : array[tres] of presinfo;
       asminfos      : array[tasm] of pasminfo;
       dbginfos      : array[tdbg] of pdbginfo;

       source_info : tsysteminfo;
       target_cpu  : tsystemcpu;
       target_info : tsysteminfo;
       target_asm  : tasminfo;
       target_ar   : tarinfo;
       target_res  : tresinfo;
       target_dbg  : tdbginfo;
       target_cpu_string,
       target_os_string   : string[12]; { for rtl/<X>/,fcl/<X>/, etc. }
       target_full_string : string[24];

    function set_target(t:tsystem):boolean;
    function set_target_asm(t:tasm):boolean;
    function set_target_ar(t:tar):boolean;
    function set_target_res(t:tres):boolean;
    function set_target_dbg(t:tdbg):boolean;

    function find_system_by_string(const s : string) : tsystem;
    function find_asm_by_string(const s : string) : tasm;
    function find_dbg_by_string(const s : string) : tdbg;

    procedure set_source_info(const ti : tsysteminfo);

    procedure UpdateAlignment(var d:talignmentinfo;const s:talignmentinfo);

    procedure RegisterTarget(const r:tsysteminfo);
    procedure RegisterRes(const r:tresinfo);
    procedure RegisterAr(const r:tarinfo);
    { Register the external linker. This routine is called to setup the
      class to use for the linker. It returns the tsysteminfo structure
      updated with the correct linker class for external linking.
    }
    procedure RegisterExternalLinker(var system_info: tsysteminfo; c:TAbstractLinkerClass);
    { Register the internal linker. This routine is called to setup the
      class to use for the linker. It returns the tsysteminfo structure
      updated with the correct linker class for internal linking.

      If internal linking is not supported, this class can be set
      to nil.
    }
    procedure RegisterInternalLinker(var system_info : tsysteminfo; c:TAbstractLinkerClass);

    procedure InitSystems;

    {$ifdef FreeBSD}
        function GetOSRelDate:Longint;
    {$endif}

implementation

    uses
      cutils{$ifdef FreeBSD},SysCtl,BaseUnix{$endif};

{****************************************************************************
           OS runtime version detection utility routine
****************************************************************************}

{$ifdef FreeBSD}
function GetOSRelDate:Longint;

var
        mib  : array[0..1] of cint;
        rval : cint;
        len  : size_t;
        i    : longint;
        v    : longint;
        oerrno : cint;
        S    : AnsiString;

Begin
        s:='ab';
        SetLength(S,50);
        mib[0] := CTL_KERN;
        mib[1] := KERN_OSRELDATE;
        len    := 4;
        oerrno:= fpgeterrno;
        if (FPsysctl(pChar(@mib), 2, pchar(@v), @len, NIL, 0) = -1) Then
           Begin
                if (fpgeterrno = ESysENOMEM) Then
                        fpseterrno(oerrno);
                GetOSRelDate:=0;
           End
        else
         GetOSRelDate:=v;
End;
{$endif}


{****************************************************************************
                              Target setting
****************************************************************************}

function set_target(t:tsystem):boolean;
begin
  set_target:=false;
  if assigned(targetinfos[t]) then
   begin
     target_info:=targetinfos[t]^;
     set_target_asm(target_info.assem);
     set_target_ar(target_info.ar);
     set_target_res(target_info.res);
     set_target_dbg(target_info.dbg);
     target_cpu:=target_info.cpu;
     target_os_string:=lower(target_info.shortname);
     target_cpu_string:=cpu2str[target_cpu];
     target_full_string:=target_cpu_string+'-'+target_os_string;
     set_target:=true;
     exit;
   end;
end;


function set_target_asm(t:tasm):boolean;
begin
  set_target_asm:=false;
  if assigned(asminfos[t]) and
    ((asminfos[t]^.supported_target=target_info.system) or
     (asminfos[t]^.supported_target=system_any)) then
   begin
     target_asm:=asminfos[t]^;
     set_target_asm:=true;
     exit;
   end;
end;


function set_target_ar(t:tar):boolean;
begin
  result:=false;
  if assigned(arinfos[t]) then
   begin
     target_ar:=arinfos[t]^;
     result:=true;
     exit;
   end;
end;


function set_target_res(t:tres):boolean;
begin
  result:=false;
  if assigned(resinfos[t]) then
   begin
     target_res:=resinfos[t]^;
     result:=true;
     exit;
   end;
end;


function set_target_dbg(t:tdbg):boolean;
begin
  result:=false;
  if assigned(dbginfos[t]) then
   begin
     target_dbg:=dbginfos[t]^;
     result:=true;
     exit;
   end;
end;


function find_system_by_string(const s : string) : tsystem;
var
  hs : string;
  t  : tsystem;
begin
  result:=system_none;
  hs:=upper(s);
  for t:=low(tsystem) to high(tsystem) do
   if assigned(targetinfos[t]) and
      (upper(targetinfos[t]^.shortname)=hs) then
    begin
      result:=t;
      exit;
    end;
end;


function find_asm_by_string(const s : string) : tasm;
var
  hs : string;
  t  : tasm;
begin
  result:=as_none;
  hs:=upper(s);
  for t:=low(tasm) to high(tasm) do
   if assigned(asminfos[t]) and
      (asminfos[t]^.idtxt=hs) then
    begin
      result:=t;
      exit;
    end;
end;


function find_dbg_by_string(const s : string) : tdbg;
var
  hs : string;
  t  : tdbg;
begin
  result:=dbg_none;
  hs:=upper(s);
  for t:=low(tdbg) to high(tdbg) do
   if assigned(dbginfos[t]) and
      (dbginfos[t]^.idtxt=hs) then
    begin
      result:=t;
      exit;
    end;
end;


procedure UpdateAlignment(var d:talignmentinfo;const s:talignmentinfo);
begin
  with d do
   begin
     { general update rules:
       minimum: if higher then update
       maximum: if lower then update or if undefined then update }
     if s.procalign>procalign then
      procalign:=s.procalign;
     if s.loopalign>loopalign then
      loopalign:=s.loopalign;
     if s.jumpalign>jumpalign then
      jumpalign:=s.jumpalign;
     if s.constalignmin>constalignmin then
      constalignmin:=s.constalignmin;
     if (constalignmax=0) or
        ((s.constalignmax>0) and (s.constalignmax<constalignmax)) then
      constalignmax:=s.constalignmax;
     if s.varalignmin>varalignmin then
      varalignmin:=s.varalignmin;
     if (varalignmax=0) or
        ((s.varalignmax>0) and (s.varalignmax<varalignmax)) then
      varalignmax:=s.varalignmax;
     if s.localalignmin>localalignmin then
      localalignmin:=s.localalignmin;
     if (localalignmax=0) or
        ((s.localalignmax>0) and (s.localalignmax<localalignmax)) then
      localalignmax:=s.localalignmax;
     if s.recordalignmin>recordalignmin then
      recordalignmin:=s.recordalignmin;
     if (recordalignmax=0) or
        ((s.recordalignmax>0) and (s.recordalignmax<recordalignmax)) then
      recordalignmax:=s.recordalignmax;
     if (maxCrecordalign=0) or
        ((s.maxCrecordalign>0) and (s.maxCrecordalign<maxCrecordalign)) then
      maxCrecordalign:=s.maxCrecordalign;
   end;
end;


{****************************************************************************
                              Target registration
****************************************************************************}

procedure RegisterTarget(const r:tsysteminfo);
var
  t : tsystem;
begin
  t:=r.system;
  if assigned(targetinfos[t]) then
   writeln('Warning: Target is already registered!')
  else
   Getmem(targetinfos[t],sizeof(tsysteminfo));
  targetinfos[t]^:=r;
end;


procedure RegisterRes(const r:tresinfo);
var
  t : tres;
begin
  t:=r.id;
  if assigned(resinfos[t]) then
    writeln('Warning: resourcecompiler is already registered!')
  else
    Getmem(resinfos[t],sizeof(tresinfo));
  resinfos[t]^:=r;
end;


procedure RegisterAr(const r:tarinfo);
var
  t : tar;
begin
  t:=r.id;
  if assigned(arinfos[t]) then
    writeln('Warning: ar is already registered!')
  else
    Getmem(arinfos[t],sizeof(tarinfo));
  arinfos[t]^:=r;
end;

procedure RegisterExternalLinker(var system_info: tsysteminfo; c:TAbstractLinkerClass);
begin
  system_info.linkextern := c;
end;

procedure RegisterInternalLinker(var system_info : tsysteminfo; c:TAbstractLinkerClass);
begin
  system_info.link := c;
end;



procedure DeregisterInfos;
var
  assem   : tasm;
  target  : tsystem;
  ar      : tar;
  res     : tres;
  dbg     : tdbg;
begin
  for target:=low(tsystem) to high(tsystem) do
   if assigned(targetinfos[target]) then
    begin
      freemem(targetinfos[target],sizeof(tsysteminfo));
      targetinfos[target]:=nil;
    end;
  for assem:=low(tasm) to high(tasm) do
   if assigned(asminfos[assem]) then
    begin
      freemem(asminfos[assem],sizeof(tasminfo));
      asminfos[assem]:=nil;
    end;
  for ar:=low(tar) to high(tar) do
   if assigned(arinfos[ar]) then
    begin
      freemem(arinfos[ar],sizeof(tarinfo));
      arinfos[ar]:=nil;
    end;
  for res:=low(tres) to high(tres) do
   if assigned(resinfos[res]) then
    begin
      freemem(resinfos[res],sizeof(tresinfo));
      resinfos[res]:=nil;
    end;
  for dbg:=low(tdbg) to high(tdbg) do
   if assigned(dbginfos[dbg]) then
    begin
      freemem(dbginfos[dbg],sizeof(tdbginfo));
      dbginfos[dbg]:=nil;
    end;
end;


{****************************************************************************
                      Initialization of default target
****************************************************************************}

procedure default_target(t:tsystem);
begin
  set_target(t);
  if source_info.name='' then
    source_info:=target_info;
end;


procedure set_source_info(const ti : tsysteminfo);
begin
{ can't use message() here (PFV) }
  if source_info.name<>'' then
    Writeln('Warning: Source OS Redefined!');
  source_info:=ti;
end;


procedure InitSystems;
begin
{ Now default target, this is dependent on the target cpu define,
  when the define is the same as the source cpu then we use the source
  os, else we pick a default }
{$ifdef i386}
  {$ifdef cpu86}
    default_target(source_info.system);
  {$else cpu86}
   {$ifdef linux}
    default_target(system_i386_linux);
   {$endif}
   {$ifdef freebsd}
    default_target(system_i386_freebsd);
   {$endif}
   {$ifdef darwin}
    default_target(system_i386_darwin);
   {$endif}
  {$endif cpu86}
{$endif i386}

{$ifdef x86_64}
  {$ifdef cpux86_64}
    default_target(source_info.system);
    {$define source_system_set}
  {$else cpux86_64}
   {$ifdef MSWindows}
    default_target(system_x86_64_win64);
    {$define source_system_set}
   {$endif}
   {$ifdef linux}
    default_target(system_x86_64_linux);
    {$define source_system_set}
   {$endif}
   {$ifdef freebsd}
    default_target(system_x86_64_freebsd);
    {$define source_system_set}
   {$endif}
   { default is linux }
   {$ifndef source_system_set}
    default_target(system_x86_64_linux);
   {$endif source_system_set}
  {$endif cpux86_64}
{$endif x86_64}

{$ifdef m68k}
  {$ifdef cpu68}
    default_target(source_info.target);
  {$else cpu68}
    default_target(system_m68k_linux);
  {$endif cpu68}
{$endif m68k}
{$ifdef alpha}
  {$ifdef cpualpha}
    default_target(source_info.system);
  {$else cpualpha}
    default_target(system_alpha_linux);
  {$endif cpualpha}
{$endif alpha}
{$ifdef powerpc}
  {$ifdef cpupowerpc}
    default_target(source_info.system);
  {$else cpupowerpc}
   {$ifdef linux}
    default_target(system_powerpc_linux);
   {$endif}
   {$ifdef darwin}
    default_target(system_powerpc_darwin);
   {$endif}
  {$endif cpupowerpc}
{$endif powerpc}
{$ifdef POWERPC64}
  {$ifdef cpupowerpc64}
    default_target(source_info.system);
  {$else cpupowerpc64}
    default_target(system_powerpc64_linux);
  {$endif cpupowerpc64}
{$endif POWERPC64}
{$ifdef sparc}
  {$ifdef cpusparc}
    default_target(source_info.system);
  {$else cpusparc}
    default_target(system_sparc_linux);
  {$endif cpusparc}
{$endif sparc}
{$ifdef arm}
  {$ifdef cpuarm}
    default_target(source_info.system);
  {$else cpuarm}
    default_target(system_arm_linux);
  {$endif cpuarm}
{$endif arm}
end;


initialization
   source_info.name:='';
finalization
  DeregisterInfos;
end.
