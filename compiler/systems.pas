{
    $Id$
    Copyright (C) 1998-2000 by Florian Klaempfl

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

{$i defines.inc}

interface

   type
       tendian = (endian_little,endian_big);

     {
       IMPORTANT NOTE:
       The value of this enumeration is stored in PPU files.
       Therefore adding new CPU targets should not change the
       values of the pre-existing targets. (CEC)
       FURTHERMORE : Make sure that this branch values, are
       consistant with the main branch version always.
     }
       ttargetcpu=
       (
             no_cpu,                   { 0 }
             i386,                     { 1 }
             m68k,                     { 2 }
             alpha,                    { 3 }
             powerpc                   { 4 }
       );

       tprocessors = (no_processor
            ,Class386,ClassP5,ClassP6
            ,MC68000,MC68100,MC68020
       );

       tsection=(sec_none,
         sec_code,sec_data,sec_bss,
         sec_idata2,sec_idata4,sec_idata5,sec_idata6,sec_idata7,sec_edata,
         sec_stab,sec_stabstr
       );

       tasmmode= (asmmode_none
            ,asmmode_i386_direct,asmmode_i386_att,asmmode_i386_intel
            ,asmmode_m68k_mot
            ,asmmode_alpha_direct
            ,asmmode_powerpc_direct
       );

       { IMPORTANT NOTE:
         the integer value of this enum is stored in PPU
         files to recognize the target, so if you add new targets
         allways add them at end PM
         FURTHERMORE : Make sure that this branch values, are
         consistant with the main branch version always.
       }
     { IMPORTANT NOTE:
       the integer value of this enum is stored in PPU
       files to recognize the target, so if you add new targets
       allways add them at end PM
       FURTHERMORE : Make sure that this branch values, are
       consistant with the main branch version always. (CEC)
       }
     type
       ttarget =
       (
             target_none,               { 0 }
             target_i386_GO32V1,        { 1 }
             target_i386_GO32V2,        { 2 }
             target_i386_linux,         { 3 }
             target_i386_OS2,           { 4 }
             target_i386_Win32,         { 5 }
             target_i386_freebsd,       { 6 }
             target_m68k_Amiga,         { 7 }
             target_m68k_Atari,         { 8 }
             target_m68k_Mac,           { 9 }
             target_m68k_linux,         { 10 }
             target_m68k_PalmOS,        { 11 }
             target_alpha_linux,        { 12 }
             target_powerpc_linux,      { 13 }
             target_powerpc_macos,      { 14 }
             target_i386_sunos,         { 15 }
             target_i386_beos,          { 16 }
             target_i386_netbsd,        { 17 }
             target_m68k_netbsd,        { 18 }
             target_i386_Netware,       { 19 }
             target_i386_qnx            { 20 }
       );

       tasm = (as_none
            ,as_i386_as,as_i386_as_aout,as_i386_asw,
              as_i386_nasmcoff,as_i386_nasmwin32,
              as_i386_nasmelf,as_i386_nasmobj,
              as_i386_tasm,as_i386_masm,
              as_i386_dbg,as_i386_coff,as_i386_pecoff,as_i386_elf32
            ,as_m68k_as,as_m68k_gas,as_m68k_mit,as_m68k_mot,
              as_m68k_mpw,as_m68k_palm
            ,as_alpha_as
            ,as_powerpc_as,as_powerpc_mpw
       );

       tld = (ld_none,
            ld_i386_GO32V1,ld_i386_GO32V2,ld_i386_linux,
              ld_i386_OS2,ld_i386_Win32,ld_i386_freebsd,
              ld_i386_Netware,ld_i386_sunos,ld_i386_beos,
            ld_m68k_Amiga,ld_m68k_Atari,ld_m68k_Mac,
              ld_m68k_linux,ld_m68k_PalmOS,ld_m68k_freebsd,
            ld_alpha_linux,
            ld_powerpc_linux,ld_powerpc_macos
       );

       tar = (ar_none
            ,ar_gnu_ar,ar_gnu_arw
       );

       tres = (res_none
            ,res_gnu_windres,res_emxbind
       );

       tscripttype = (script_none
            ,script_dos,script_unix,script_amiga
       );


{*****************************************************************************
                               Structures
*****************************************************************************}

     type
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
          supported_target : ttarget;
          outputbinary,
          allowdirect,
          externals,
          needar,
          labelprefix_only_inside_procedure : boolean;
          labelprefix : string[3];
          comment     : string[2];
          secnames    : array[tsection] of string[20];
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

       ttargetflags = (tf_none,
            tf_under_development,tf_supports_stack_checking,
            tf_need_export,tf_needs_isconsole
{$ifdef m68k}
            ,tf_code_small,tf_static_a5_based
{$endif m68k}
       );

       ptargetinfo = ^ttargetinfo;
       ttargetinfo = packed record
          target       : ttarget;
          name         : string[30];
          shortname    : string[9];
          flags        : set of ttargetflags;
          cpu          : ttargetcpu;
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
          link         : tld;
          linkextern   : tld;  { external linker, used by -s }
          ar           : tar;
          res          : tres;
          script       : tscripttype;
          endian       : tendian;
          alignment    : talignmentinfo;
          size_of_pointer : byte;
          size_of_longint : byte;
          heapsize,
          maxheapsize,
          stacksize       : longint;
          DllScanSupported : boolean;
          use_bound_instruction : boolean;
          use_function_relative_addresses : boolean;
       end;

       pasmmodeinfo = ^tasmmodeinfo;
       tasmmodeinfo = packed record
          id    : tasmmode;
          idtxt : string[8];
       end;

    const
       { alias for supported_target field in tasminfo }
       target_any = target_none;

    var
       targetinfos   : array[ttarget] of ptargetinfo;
       asminfos      : array[tasm] of pasminfo;
       arinfos       : array[tar] of parinfo;
       resinfos      : array[tres] of presinfo;
       asmmodeinfos  : array[tasmmode] of pasmmodeinfo;

       source_info : ttargetinfo;
       target_cpu  : ttargetcpu;
       target_info : ttargetinfo;
       target_asm  : tasminfo;
       target_ar   : tarinfo;
       target_res  : tresinfo;
       target_path : string[12]; { for rtl/<X>/,fcl/<X>/, etc. }

    function set_target(t:ttarget):boolean;
    function set_target_asm(t:tasm):boolean;
    function set_target_ar(t:tar):boolean;
    function set_target_res(t:tres):boolean;

    function set_target_by_string(const s : string) : boolean;
    function set_target_asm_by_string(const s : string) : boolean;
    function set_asmmode_by_string(const s:string;var t:tasmmode):boolean;

    procedure UpdateAlignment(var d:talignmentinfo;const s:talignmentinfo);

    procedure RegisterTarget(const r:ttargetinfo);
    procedure RegisterAsmMode(const r:tasmmodeinfo);
    procedure RegisterRes(const r:tresinfo);
    procedure RegisterAr(const r:tarinfo);

    procedure InitSystems;


implementation

    uses
      cutils;

{****************************************************************************
                              Target setting
****************************************************************************}

function set_target(t:ttarget):boolean;
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
  t  : ttarget;
begin
  set_target_by_string:=false;
  { this should be case insensitive !! PM }
  hs:=upper(s);
  for t:=low(ttarget) to high(ttarget) do
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
     if (constalignmax=0) or (s.constalignmax<constalignmax) then
      constalignmax:=s.constalignmax;
     if s.varalignmin>varalignmin then
      varalignmin:=s.varalignmin;
     if (varalignmax=0) or (s.varalignmax<varalignmax) then
      varalignmax:=s.varalignmax;
     if s.localalignmin>localalignmin then
      localalignmin:=s.localalignmin;
     if (localalignmax=0) or (s.localalignmax<localalignmax) then
      localalignmax:=s.localalignmax;
     if s.paraalign>paraalign then
      paraalign:=s.paraalign;
     if s.recordalignmin>recordalignmin then
      recordalignmin:=s.recordalignmin;
     if (recordalignmax=0) or (s.recordalignmax<recordalignmax) then
      recordalignmax:=s.recordalignmax;
     if (maxCrecordalign=0) or (s.maxCrecordalign<maxCrecordalign) then
      maxCrecordalign:=s.maxCrecordalign;
   end;
end;


{****************************************************************************
                              Target registration
****************************************************************************}

procedure RegisterTarget(const r:ttargetinfo);
var
  t : ttarget;
begin
  t:=r.target;
  if assigned(targetinfos[t]) then
   writeln('Warning: Target is already registered!')
  else
   Getmem(targetinfos[t],sizeof(ttargetinfo));
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


procedure DeregisterInfos;
var
  assem   : tasm;
  target  : ttarget;
  ar      : tar;
  asmmode : tasmmode;
  res     : tres;
begin
  for target:=low(ttarget) to high(ttarget) do
   if assigned(targetinfos[target]) then
    begin
      freemem(targetinfos[target],sizeof(ttargetinfo));
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

procedure default_target(t:ttarget);
begin
  set_target(t);
  if source_info.name='' then
    source_info:=target_info;
end;


procedure set_source(t:ttarget);
begin
{ can't use message() here (PFV) }
  if source_info.name<>'' then
    Writeln('Warning: Source OS Redefined!');
  if assigned(targetinfos[t]) then
   source_info:=targetinfos[t]^
  else
   Writeln('Warning: Source OS Not Supported!');
end;


procedure InitSystems;
begin
{ first get source OS }
  source_info.name:='';
{ please note then we use cpu86 and cpu68 here on purpose !! }
{$ifdef cpu86}
  {$ifdef GO32V1}
    set_source(target_i386_GO32V1);
  {$else}
    {$ifdef GO32V2}
      set_source(target_i386_GO32V2);
    {$else}
      {$ifdef OS2}
        set_source(target_i386_OS2);
        if (OS_Mode = osDOS) or (OS_Mode = osDPMI) then
          source_info.scriptext := '.bat';
        { OS/2 via EMX can be run under DOS as well }
      {$else}
        {$ifdef WIN32}
          set_source(target_i386_WIN32);
        {$else}
           {$ifdef FreeBSD}
              set_source(target_i386_FreeBSD);
           {$else}
              {$ifdef netbsd}
                set_source(target_i386_NetBSD);
              {$else}
                {$ifdef sunos}
                  set_source(target_i386_sunos);
                {$else}
                  {$ifdef beos}
                    set_source(target_i386_beos);
                  {$else}
                    { Must be the last as some freebsd also
                      defined linux }
                    {$ifdef linux}
                      set_source(target_i386_linux);
                    {$else}
                      {$error Error setting source OS}
                    {$endif linux}
                  {$endif beos}
               {$endif sunos}
            {$endif netbsd}
          {$endif freebsd}
        {$endif win32}
      {$endif os2}
    {$endif go32v2}
  {$endif go32v1}
{$endif cpu86}
{$ifdef cpu68}
  {$ifdef AMIGA}
    set_source(target_m68k_Amiga);
  {$else}
    {$ifdef ATARI}
      set_source(target_m68k_Atari);
    {$else}
      {$ifdef MACOS}
        set_source(target_m68k_MAC);
      {$else}
        {$ifdef linux}
           set_source(target_m68k_linux);
        {$endif linux}
      {$endif macos}
    {$endif atari}
  {$endif amiga}
{$endif cpu68}

{ Now default target, this is dependent on the i386 or m68k define,
  when the define is the same as the current cpu then we use the source
  os, else we pick a default }
{$ifdef i386}
  {$ifdef cpu86}
    default_target(source_info.target);
  {$else cpu86}
    default_target(target_i386_linux);
  {$endif cpu86}
{$endif i386}
{$ifdef m68k}
  {$ifdef cpu68}
    default_target(source_info.target);
  {$else cpu68}
    default_target(target_m68k_linux);
  {$endif cpu68}
{$endif m68k}
{$ifdef alpha}
  {$ifdef cpualpha}
    default_target(source_info.target);
  {$else cpualpha}
    default_target(target_alpha_linux);
  {$endif cpualpha}
{$endif alpha}
{$ifdef powerpc}
  {$ifdef cpuppc}
    default_target(source_info.target);
  {$else cpuppc}
    default_target(target_powerpc_linux);
  {$endif cpuppc}
{$endif powerpc}
end;


initialization
finalization
  DeregisterInfos;
end.
{
  $Log$
  Revision 1.33  2002-01-06 20:34:34  hajny
    * source_os changed to source_info in OS/2 define

  Revision 1.32  2001/12/15 05:43:20  carl
  + QNX target

  Revision 1.31  2001/11/15 20:48:43  hajny
    * Target_Mode corrected back to OS_Mode

  Revision 1.30  2001/09/30 21:27:59  peter
    * much cleaner default source and target setting

  Revision 1.29  2001/09/24 10:57:22  jonas
    * fixed typo in Carl's patch

  Revision 1.28  2001/09/22 00:03:53  carl
  + added warning for targets - use same target values as fixes branch

  Revision 1.27  2001/09/18 11:30:48  michael
  * Fixes win32 linking problems with import libraries
  * LINKLIB Libraries are now looked for using C file extensions
  * get_exepath fix

  Revision 1.26  2001/09/17 21:29:13  peter
    * merged netbsd, fpu-overflow from fixes branch

  Revision 1.25  2001/08/30 20:57:10  peter
    * asbsd merged

  Revision 1.24  2001/08/19 11:22:24  peter
    * palmos support from v10 merged

  Revision 1.23  2001/08/12 17:57:07  peter
    * under development flag for targets

  Revision 1.22  2001/08/07 18:47:13  peter
    * merged netbsd start
    * profile for win32

  Revision 1.21  2001/07/01 20:16:18  peter
    * alignmentinfo record added
    * -Oa argument supports more alignment settings that can be specified
      per type: PROC,LOOP,VARMIN,VARMAX,CONSTMIN,CONSTMAX,RECORDMIN
      RECORDMAX,LOCALMIN,LOCALMAX. It is possible to set the mimimum
      required alignment and the maximum usefull alignment. The final
      alignment will be choosen per variable size dependent on these
      settings

  Revision 1.20  2001/06/19 14:43:31  marco
   * Fixed ifdef linux bug

  Revision 1.19  2001/06/03 20:21:08  peter
    * Kylix fixes, mostly case names of units

  Revision 1.18  2001/06/03 15:15:31  peter
    * dllprt0 stub for linux shared libs
    * pass -init and -fini for linux shared libs
    * libprefix splitted into staticlibprefix and sharedlibprefix

  Revision 1.17  2001/06/02 19:21:45  peter
    * extradefines field added to target_info, so that targets don't
      need to put code in options.pas for it

  Revision 1.16  2001/04/18 22:02:00  peter
    * registration of targets and assemblers

  Revision 1.15  2001/03/06 18:28:02  peter
    * patch from Pavel with a new and much faster DLL Scanner for
      automatic importing so $linklib works for DLLs. Thanks Pavel!

  Revision 1.14  2001/02/26 19:44:55  peter
    * merged generic m68k updates from fixes branch

  Revision 1.13  2001/02/20 21:36:40  peter
    * tasm/masm fixes merged

  Revision 1.12  2001/01/06 20:15:43  peter
    * merged libp library prefix

  Revision 1.11  2000/10/15 09:08:58  peter
    * use System for the systemunit instead of target dependent

  Revision 1.10  2000/09/24 21:12:41  hajny
    * OS/2 stack alignment corrected + default stack increased

  Revision 1.9  2000/09/24 15:06:30  peter
    * use defines.inc

  Revision 1.8  2000/09/20 10:49:39  marco
   * Set writer to elf. (Only a prob for smart with -OG3p3r)

  Revision 1.7  2000/09/16 12:22:52  peter
    * freebsd support merged

  Revision 1.6  2000/09/11 17:00:23  florian
    + first implementation of Netware Module support, thanks to
      Armin Diehl (diehl@nordrhein.de) for providing the patches

  Revision 1.5  2000/08/12 19:14:59  peter
    * ELF writer works now also with -g
    * ELF writer is default again for linux

  Revision 1.4  2000/07/14 21:29:38  michael
  * Back to external assembler till peter fixes gdb

  Revision 1.3  2000/07/13 12:08:28  michael
  + patched to 1.1.0 with former 1.09patch from peter

  Revision 1.2  2000/07/13 11:32:50  michael
  + removed logs

}
