{
    $Id$
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
             cpu_arm                       { 10 }
       );


       TSection=(sec_none,
         sec_code,sec_data,sec_bss,
         sec_idata2,sec_idata4,sec_idata5,sec_idata6,sec_idata7,sec_edata,
         sec_stab,sec_stabstr,sec_common
       );

       tasmmode= (asmmode_none
            { direct output with minimal parsing }
            ,asmmode_direct
            { standard assembler (cpu dependant) with full parsing }
            ,asmmode_standard
            ,asmmode_i386_att
            ,asmmode_i386_intel
            ,asmmode_ppc_gas
            ,asmmode_ppc_motorola
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
             system_i386_sunos,         { 15 }
             system_i386_beos,          { 16 }
             system_i386_netbsd,        { 17 }
             system_m68k_netbsd,        { 18 }
             system_i386_Netware,       { 19 }
             system_i386_qnx,           { 20 }
             system_i386_wdosx,         { 21 }
             system_sparc_sunos,        { 22 }
             system_sparc_linux,        { 23 }
             target_i386_openbsd,       { 24 }
             target_m68k_openbsd,       { 25 }
             system_x86_64_linux,       { 26 }
             system_powerpc_darwin,     { 27 }
             system_i386_EMX,           { 28 }
	     system_powerpc_netbsd,     { 29 }
    	     system_powerpc_openbsd,    { 30 }
             system_arm_linux           { 31 }
       );

       tasm = (as_none
             ,as_gas                   { standard gnu assembler }
             ,as_i386_as_aout
             ,as_i386_asw
             ,as_i386_nasmcoff
             ,as_i386_nasmwin32
             ,as_i386_nasmwdosx
             ,as_i386_nasmelf
             ,as_i386_nasmobj
             ,as_i386_tasm
             ,as_i386_masm
             ,as_i386_coff
             ,as_i386_pecoff
             ,as_i386_elf32
             ,as_i386_pecoffwdosx
             ,as_m68k_mit
             ,as_powerpc_mpw
       );

       tar = (ar_none
            ,ar_gnu_ar,ar_gnu_arw,ar_mpw_ar
       );

       tres = (res_none
            ,res_gnu_windres,res_emxbind
            ,res_m68k_palmos,res_m68k_mpw
            ,res_powerpc_mpw
       );

       tscripttype = (script_none
            ,script_dos,script_unix,script_amiga
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


       TAbstractLinkerClass = class of TABstractLinker;


       { Abstract assembler class which is implemented in assemble module }
       TAbstractAssembler = class
       end;

       TAbstractAssemblerClass = class of TAbstractAssembler;



       palignmentinfo = ^talignmentinfo;
       talignmentinfo = packed record
         procalign,
         loopalign,
         jumpalign,
         constalignmin,
         constalignmax,
         varalignmin,
         varalignmax,
         localalignmin,
         localalignmax,
         paraalign,
         recordalignmin,
         recordalignmax,
         maxCrecordalign : longint;
       end;

       pasminfo = ^tasminfo;
       tasminfo = packed record
          id          : tasm;
          idtxt       : string[9];
          asmbin      : string[8];
          asmcmd      : string[50];
          supported_target : tsystem;
          outputbinary,
          allowdirect,
          needar,
          labelprefix_only_inside_procedure : boolean;
          labelprefix : string[3];
          comment     : string[2];
          secnames    : array[TSection] of string[20];
       end;

       parinfo = ^tarinfo;
       tarinfo = packed record
          id      : tar;
          arcmd   : string[50];
       end;

       presinfo = ^tresinfo;
       tresinfo = packed record
          id      : tres;
          resbin  : string[8];
          rescmd  : string[50];
       end;

       tsystemflags = (tf_none,
            tf_under_development,
            tf_need_export,tf_needs_isconsole
            ,tf_code_small,tf_static_reg_based
       );

       psysteminfo = ^tsysteminfo;
       tsysteminfo = packed record
          system       : tsystem;
          name         : string[30];
          shortname    : string[9];
          flags        : set of tsystemflags;
          cpu          : tsystemcpu;
          unit_env     : string[12];
          extradefines : string[40];
          sourceext,
          pasext,
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
          files_case_relevent : boolean;
          assem        : tasm;
          assemextern  : tasm; { external assembler, used by -a }
          link         : tabstractlinkerclass;
          linkextern   : tabstractlinkerclass;  { external linker, used by -s }
          ar           : tar;
          res          : tres;
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
          heapsize,
          stacksize       : longint;
          DllScanSupported : boolean;
          use_function_relative_addresses : boolean;
          abi : tabi;
       end;

       pasmmodeinfo = ^tasmmodeinfo;
       tasmmodeinfo = packed record
          id    : tasmmode;
          idtxt : string[8];
       end;

    const
       { alias for supported_target field in tasminfo }
       system_any = system_none;

    var
       targetinfos   : array[tsystem] of psysteminfo;
       asminfos      : array[tasm] of pasminfo;
       arinfos       : array[tar] of parinfo;
       resinfos      : array[tres] of presinfo;
       asmmodeinfos  : array[tasmmode] of pasmmodeinfo;

       source_info : tsysteminfo;
       target_cpu  : tsystemcpu;
       target_info : tsysteminfo;
       target_asm  : tasminfo;
       target_ar   : tarinfo;
       target_res  : tresinfo;
       target_path : string[12]; { for rtl/<X>/,fcl/<X>/, etc. }

    function set_target(t:tsystem):boolean;
    function set_target_asm(t:tasm):boolean;
    function set_target_ar(t:tar):boolean;
    function set_target_res(t:tres):boolean;

    function set_target_by_string(const s : string) : boolean;
    function set_target_asm_by_string(const s : string) : boolean;
    function set_asmmode_by_string(const s:string;var t:tasmmode):boolean;

    procedure set_source_info(const ti : tsysteminfo);

    procedure UpdateAlignment(var d:talignmentinfo;const s:talignmentinfo);

    procedure RegisterTarget(const r:tsysteminfo);
    procedure RegisterAsmMode(const r:tasmmodeinfo);
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


implementation

    uses
      cutils;


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
     target_path:=lower(target_info.shortname);
     target_cpu:=target_info.cpu;
     set_target:=true;
     exit;
   end;
end;


function set_target_asm(t:tasm):boolean;
begin
  set_target_asm:=false;
  if assigned(asminfos[t]) then
   begin
     target_asm:=asminfos[t]^;
     set_target_asm:=true;
     exit;
   end;
end;


function set_target_ar(t:tar):boolean;
begin
  set_target_ar:=false;
  if assigned(arinfos[t]) then
   begin
     target_ar:=arinfos[t]^;
     set_target_ar:=true;
     exit;
   end;
end;




function set_target_res(t:tres):boolean;
begin
  set_target_res:=false;
  if assigned(resinfos[t]) then
   begin
     target_res:=resinfos[t]^;
     set_target_res:=true;
     exit;
   end;
end;


function set_target_by_string(const s : string) : boolean;
var
  hs : string;
  t  : tsystem;
begin
  set_target_by_string:=false;
  { this should be case insensitive !! PM }
  hs:=upper(s);
  for t:=low(tsystem) to high(tsystem) do
   if assigned(targetinfos[t]) and
      (upper(targetinfos[t]^.shortname)=hs) then
    begin
      set_target_by_string:=set_target(t);
      exit;
    end;
end;


function set_target_asm_by_string(const s : string) : boolean;
var
  hs : string;
  t  : tasm;
begin
  set_target_asm_by_string:=false;
  { this should be case insensitive !! PM }
  hs:=upper(s);
  for t:=low(tasm) to high(tasm) do
   if assigned(asminfos[t]) and
      (asminfos[t]^.idtxt=hs) then
    begin
      set_target_asm_by_string:=set_target_asm(t);
      exit;
    end;
end;


function set_asmmode_by_string(const s:string;var t:tasmmode):boolean;
var
  hs : string;
  ht : tasmmode;
begin
  set_asmmode_by_string:=false;
  { this should be case insensitive !! PM }
  hs:=upper(s);
  for ht:=low(tasmmode) to high(tasmmode) do
   if assigned(asmmodeinfos[ht]) and
      (asmmodeinfos[ht]^.idtxt=hs) then
    begin
      t:=asmmodeinfos[ht]^.id;
      set_asmmode_by_string:=true;
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
     if s.paraalign>paraalign then
      paraalign:=s.paraalign;
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


procedure RegisterAsmmode(const r:tasmmodeinfo);
var
  t : tasmmode;
begin
  t:=r.id;
  if assigned(asmmodeinfos[t]) then
    writeln('Warning: Asmmode is already registered!')
  else
    Getmem(asmmodeinfos[t],sizeof(tasmmodeinfo));
  asmmodeinfos[t]^:=r;
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
  asmmode : tasmmode;
  res     : tres;
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
  for asmmode:=low(tasmmode) to high(tasmmode) do
   if assigned(asmmodeinfos[asmmode]) then
    begin
      freemem(asmmodeinfos[asmmode],sizeof(tasmmodeinfo));
      asmmodeinfos[asmmode]:=nil;
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
    default_target(system_i386_linux);
  {$endif cpu86}
{$endif i386}
{$ifdef x86_64}
  {$ifdef cpu86_64}
    default_target(source_info.system);
  {$else cpu86_64}
    default_target(system_x86_64_linux);
  {$endif cpu86_64}
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
    default_target(system_powerpc_linux);
  {$endif cpupowerpc}
{$endif powerpc}
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
{
  $Log$
  Revision 1.67  2003-08-08 15:49:24  olle
    * merged macos entry/exit code generation into the general one.

  Revision 1.66  2003/07/21 11:52:57  florian
    * very basic stuff for the arm

  Revision 1.65  2003/05/31 16:17:27  marco
   * cpuppc -> cpupowerpc. Target compiler was always linux for ppc

  Revision 1.64  2003/05/28 23:18:31  florian
    * started to fix and clean up the sparc port

  Revision 1.63  2003/05/25 23:15:04  marco
   * NetBSD target support. OpenBSD reserved in the enum, for future use.

  Revision 1.62  2003/05/20 23:54:00  florian
    + basic darwin support added

  Revision 1.61  2003/05/18 15:15:59  florian
    + added abi field to tsysteminfo

  Revision 1.60  2003/03/23 23:21:42  hajny
    + emx target added

  Revision 1.59  2003/01/12 15:42:23  peter
    * m68k pathexist update from 1.0.x
    * palmos res update from 1.0.x

  Revision 1.58  2003/01/05 13:36:53  florian
    * x86-64 compiles
    + very basic support for float128 type (x86-64 only)

  Revision 1.57  2002/10/05 12:43:29  carl
    * fixes for Delphi 6 compilation
     (warning : Some features do not work under Delphi)

  Revision 1.56  2002/10/03 21:18:29  carl
   * correct tsystem enumeration

  Revision 1.55  2002/09/07 18:05:51  florian
    * first part of PowerPC fixes

  Revision 1.54  2002/08/20 21:40:44  florian
    + target macos for ppc added
    + frame work for mpw assembler output

  Revision 1.53  2002/08/18 09:13:02  florian
    * small fixes to the alpha stuff

  Revision 1.52  2002/08/13 21:40:57  florian
    * more fixes for ppc calling conventions

  Revision 1.51  2002/08/12 15:08:40  carl
    + stab register indexes for powerpc (moved from gdb to cpubase)
    + tprocessor enumeration moved to cpuinfo
    + linker in target_info is now a class
    * many many updates for m68k (will soon start to compile)
    - removed some ifdef or correct them for correct cpu

  Revision 1.50  2002/08/10 14:46:31  carl
    + moved target_cpu_string to cpuinfo
    * renamed asmmode enum.
    * assembler reader has now less ifdef's
    * move from nppcmem.pas -> ncgmem.pas vec. node.

  Revision 1.49  2002/07/28 20:45:22  florian
    + added direct assembler reader for PowerPC

  Revision 1.48  2002/07/26 21:15:42  florian
    * rewrote the system handling

  Revision 1.47  2002/07/04 20:43:02  florian
    * first x86-64 patches

  Revision 1.46  2002/07/01 18:46:29  peter
    * internal linker
    * reorganized aasm layer

  Revision 1.45  2002/05/18 13:34:21  peter
    * readded missing revisions

  Revision 1.44  2002/05/16 19:46:45  carl
  + defines.inc -> fpcdefs.inc to avoid conflicts if compiling by hand
  + try to fix temp allocation (still in ifdef)
  + generic constructor calls
  + start of tassembler / tmodulebase class cleanup

  Revision 1.42  2002/05/06 19:52:04  carl
  + added more patches from Mazen for SPARC port

  Revision 1.41  2002/04/24 16:08:30  carl

  * fix compilation problem

  Revision 1.40  2002/04/20 21:32:26  carl
  + generic FPC_CHECKPOINTER
  + first parameter offset in stack now portable
  * rename some constants
  + move some cpu stuff to other units
  - remove unused constents
  * fix stacksize for some targets
  * fix generic size problems which depend now on EXTEND_SIZE constant

  Revision 1.39  2002/04/15 19:08:22  carl
  + target_info.size_of_pointer -> pointer_size
  + some cleanup of unused types/variables

  Revision 1.38  2002/04/14 16:56:30  carl
  - remove duplicate comment
}
