{
    $Id$
    Copyright (C) 1995,97 by Florian Klaempfl

    This unit contains informations about the target systems supported
    (these are not processor specific)

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

 ****************************************************************************
}
unit systems;

  interface

    type
       ttarget = (target_GO32V1,target_GO32V2,target_LINUX,target_OS2,
                  target_WIN32,target_Amiga,target_Atari,target_Mac68k);

       tos = (os_GO32V1, os_GO32V2, os_Linux, os_OS2,
              os_WIN32, os_Amiga, os_Atari, os_Mac68k);

       tasm = (as_as
       {$ifdef i386}
              ,as_nasmcoff, as_nasmelf, as_nasmobj
       {$endif}
       {$ifdef m68k}
              ,as_as68k
       {$endif}
       );

       tlink = (link_ld
       {$ifdef i386}
              ,link_ldgo32v1, link_ldgo32v2, link_ldw, link_ldos2);
       {$endif i386}
       {$ifdef m68k}
              );
       {$endif}

       tendian = (endian_little,en_big_endian);

       tosinfo = record
          name      : string[30];
          sharedlibext,
          staticlibext,
          sourceext,
          pasext,
          exeext,
          scriptext : string[4];
          Cprefix   : string[2];
          newline   : string[2];
          endian    : tendian;
          use_function_relative_addresses : boolean;
       end;

       tasminfo = record
          id          : tasm;
          idtxt       : string[8];
          asmbin      : string[8];
          asmcmd      : string[50];
          labelprefix : string[2];
          comment     : string[2];
       end;

       tlinkinfo = record
          linkbin   : string[8];
          linkcmd   : string[50];
          stripopt  : string[2];
          groupstart,
          groupend,
          inputstart,
          inputend  : string[8];
          libprefix : string[2];
       end;

       ttargetinfo = record
          target      : ttarget;
          short_name  : string[8];
          unit_env    : string[12];
          system_unit : string[8];
          smartext,
          unitext,
          unitlibext,
          asmext,
          objext      : string[4];
          os          : tos;
          link        : tlink;
          assem       : tasm;
       end;

    var
       target_info : ttargetinfo;
       target_os   : tosinfo;
       target_asm  : tasminfo;
       target_link : tlinkinfo;
       source_os   : tosinfo;

    function set_string_target(const s : string) : boolean;

  implementation

    const
       os_infos : array[tos] of tosinfo = (
          (
            name         : 'GO32 V1 DOS extender';
            sharedlibext : '.DLL';
            staticlibext : '.A';
            sourceext    : '.PP';
            pasext       : '.PAS';
            exeext       : '.EXE';
            scriptext    : '.BAT';
            Cprefix      : '_';
            newline      : #13#10;
            endian       : endian_little;
            use_function_relative_addresses : true
          ),
          (
            name         : 'GO32 V2 DOS extender';
            sharedlibext : '.DLL';
            staticlibext : '.A';
            sourceext    : '.PP';
            pasext       : '.PAS';
            exeext       : '.EXE';
            scriptext    : '.BAT';
            Cprefix      : '_';
            newline      : #13#10;
            endian       : endian_little;
            use_function_relative_addresses : true
          ),
          (
            name         : 'Linux';
            sharedlibext : '.so';
            staticlibext : '.a';
            sourceext    : '.pp';
            pasext       : '.pas';
            exeext       : '';
            scriptext    : '.sh';
            Cprefix      : '';
            newline      : #10;
            endian       : endian_little;
            use_function_relative_addresses : true
          ),
          (
            name         : 'OS/2 (32bit)';
            sharedlibext : '.ao2';
            staticlibext : '.a';
            sourceext    : '.pp';
            pasext       : '.pas';
            exeext       : '.exe';
            scriptext    : '.cmd';
            Cprefix      : '_';
            newline      : #13#10;
            endian       : endian_little;
            use_function_relative_addresses : false
          ),
          (
            name         : 'Win32';
            sharedlibext : '.dll';
            staticlibext : '.a';
            sourceext    : '.pp';
            pasext       : '.pas';
            exeext       : '.exe';
            scriptext    : '.bat';
            Cprefix      : '_';
            newline      : #13#10;
            endian       : endian_little;
            use_function_relative_addresses : true
          ),
          (
            name         : 'Commodore Amiga';
            sharedlibext : '.library';
            staticlibext : '.a';
            sourceext    : '.pp';
            pasext       : '.pas';
            exeext       : '';
            scriptext    : '';
            Cprefix      : '';
            newline      : #10;
            endian       : en_big_endian;
            use_function_relative_addresses : false
          ),
          (
            name         : 'Atari ST/STE';
            sharedlibext : '.dll';
            staticlibext : '.a';
            sourceext    : '.pp';
            pasext       : '.pas';
            exeext       : '.tpp';
            scriptext    : '';
            Cprefix      : '';
            newline      : #10;
            endian       : en_big_endian;
            use_function_relative_addresses : false
          ),
          (
            name         : 'Macintosh m68k';
            sharedlibext : '.dll';
            staticlibext : '.a';
            sourceext    : '.pp';
            pasext       : '.pas';
            exeext       : '.tpp';
            scriptext    : '';
            Cprefix      : '';
            newline      : #10;
            endian       : en_big_endian;
            use_function_relative_addresses : false
          )
          );

       as_infos : array[tasm] of tasminfo = (
          (
            id     : as_as;
            idtxt  : 'O';
            asmbin : 'as';
            asmcmd : '-D -o $OBJ $ASM';
            labelprefix : '.L';
            comment : '# '
          )
{$ifdef i386}
          ,(
            id     : as_nasmcoff;
{$ifdef linux}
            idtxt  : 'NASM';
{$else}
            idtxt  : 'NASMCOFF';
{$endif}
            asmbin : 'nasm';
            asmcmd : '-f coff -o $OBJ $ASM';
            labelprefix : 'L';
            comment : '; '
          )
          ,(
            id     : as_nasmelf;
{$ifdef linux}
            idtxt  : 'NASM';
{$else}
            idtxt  : 'NASMELF';
{$endif}
            asmbin : 'nasm';
            asmcmd : '-f elf -o $OBJ $ASM';
            labelprefix : 'L';
            comment : '; '
          )
          ,(
            id     : as_nasmobj;
            idtxt  : 'OBJ';
            asmbin : 'nasm';
            asmcmd : '-f obj -o $OBJ $ASM';
            labelprefix : 'L';
            comment : '; '
          )
{$endif}
{$ifdef m68k}
          ,(
            id     : as_as68k;
            idtxt  : 'O';
            asmbin : 'as68k'; { Gas for the Amiga}
            asmcmd : '-D --register-prefix-optional -o $OBJ $ASM';
            labelprefix : '__L';
            comment : '| '
          )
{$endif}
          );

       link_infos : array[tlink] of tlinkinfo = (
          (
            linkbin : 'ld';
            linkcmd : '$OPT -o $EXE $RES';
            stripopt   : '-s';
            groupstart : 'GROUP(';
            groupend   : ')';
            inputstart : 'INPUT(';
            inputend   : ')';
            libprefix  : '-l'
          )
{$ifdef i386}
          ,(
            linkbin : 'ld';
            linkcmd : '-oformat coff-go32 $OPT -o $EXE @$RES';
            stripopt   : '-s';
            groupstart : '-(';
            groupend   : '-)';
            inputstart : '';
            inputend   : '';
            libprefix  : '-l'
          )
          ,(
            linkbin : 'ld';
            linkcmd : '-oformat coff-go32-exe $OPT -o $EXE @$RES';
            stripopt   : '-s';
            groupstart : '-(';
            groupend   : '-)';
            inputstart : '';
            inputend   : '';
            libprefix  : '-l'
          )
          ,(
            linkbin : 'ldw';
            linkcmd : '$OPT -o $EXE $RES';
            stripopt   : '-s';
            groupstart : 'GROUP(';
            groupend   : ')';
            inputstart : 'INPUT(';
            inputend   : ')';
            libprefix  : '-l'
          )
          ,(
            linkbin : 'ld';
            linkcmd : '-o $EXE @$RES';
            stripopt   : '-s';
            groupstart : '-(';
            groupend   : '-)';
            inputstart : '';
            inputend   : '';
            libprefix  : ''
          )
{$endif i386}
          );

       target_infos : array[ttarget] of ttargetinfo = (
          (
            target      : target_GO32V1;
            short_name  : 'GO32V1';
            unit_env    : 'GO32V1UNITS';
            system_unit : 'SYSTEM';
            smartext    : '.SL';
            unitext     : '.PP1';
            unitlibext  : '.PPL';
            asmext      : '.S1';
            objext      : '.O1';
            os          : os_GO32V1;
            link        : link_ldgo32v1;
            assem       : as_as
          ),
          (
            target      : target_GO32V2;
            short_name  : 'GO32V2';
            unit_env    : 'GO32V2UNITS';
            system_unit : 'SYSTEM';
{$ifndef UseAnsiString}
            smartext    : '.SL';
            unitext     : '.PPU';
            unitlibext  : '.PPL';
            asmext      : '.S';
            objext      : '.O';
{$else UseAnsiString}
            smartext    : '.SL';
            unitext     : '.PAU';
            unitlibext  : '.PPL';
            asmext      : '.SA';
            objext      : '.OA';
{$endif UseAnsiString}
            os          : os_GO32V2;
            link        : link_ldgo32v2;
            assem       : as_as
          ),
          (
            target      : target_LINUX;
            short_name  : 'LINUX';
            unit_env    : 'LINUXUNITS';
            system_unit : 'syslinux';
            smartext    : '.sl';
            unitext     : '.ppu';
            unitlibext  : '.ppl';
            asmext      : '.s';
            objext      : '.o';
            os          : os_Linux;
            link        : link_ld;
            assem       : as_as
          ),
          (
            target      : target_OS2;
            short_name  : 'OS2';
            unit_env    : 'OS2UNITS';
            system_unit : 'SYSOS2';
            smartext    : '.sl';
            unitext     : '.ppo';
            unitlibext  : '.ppl';
            asmext      : '.so2';
            objext      : '.oo2';
            os          : os_OS2;
            link        : link_ldos2;
            assem       : as_as
          ),
          (
            target      : target_WIN32;
            short_name  : 'WIN32';
            unit_env    : 'WIN32UNITS';
            system_unit : 'SYSWIN32';
            smartext    : '.sl';
            unitext     : '.ppw';
            unitlibext  : '.ppl';
            asmext      : '.s';
            objext      : '.o';
            os          : os_Win32;
            link        : link_ldw;
            assem       : as_as
          ),
          (
            target      : target_Amiga;
            short_name  : 'AMIGA';
            unit_env    : '';
            system_unit : 'sysamiga';
            smartext    : '.sl';
            unitext     : '.ppa';
            unitlibext  : '.ppl';
            asmext      : '.asm';
            objext      : '.o';
            os          : os_Amiga;
            link        : link_ld;
            assem       : as_as
          ),
          (
            target      : target_Atari;
            short_name  : 'ATARI';
            unit_env    : '';
            system_unit : 'SYSATARI';
            smartext    : '.sl';
            unitext     : '.ppt';
            unitlibext  : '.ppl';
            asmext      : '.s';
            objext      : '.o';
            os          : os_Atari;
            link        : link_ld;
            assem       : as_as
          ),
          (
            target      : target_Mac68k;
            short_name  : 'MACOS';
            unit_env    : '';
            system_unit : 'sysmac';
            smartext    : '.sl';
            unitext     : '.ppt';
            unitlibext  : '.ppl';
            asmext      : '.s';
            objext      : '.o';
            os          : os_Mac68k;
            link        : link_ld;
            assem       : as_as
          )
          );


procedure set_target(t : ttarget);
begin
  target_info:=target_infos[t];
  target_os:=os_infos[target_info.os];
  target_asm:=as_infos[target_info.assem];
  target_link:=link_infos[target_info.link];
end;



function set_string_target(const s : string) : boolean;
var
  t : ttarget;
begin
  set_string_target:=false;
  for t:=target_GO32V1 to target_mac68k do
   if target_infos[t].short_name=s then
    begin
      set_string_target:=true;
      set_target(t);
    end;
end;


procedure default_os(t:ttarget);
begin
  set_target(t);
  source_os:=os_infos[target_info.os];
end;


begin
{$ifdef tp}
  default_os(target_GO32V2);
{$else}
  {$ifdef DOS}
    default_os(target_GO32V1);
  {$endif}
  {$ifdef GO32V1}
    default_os(target_GO32V1);
  {$endif}
  {$ifdef GO32V2}
    default_os(target_GO32V2);
  {$endif}
  {$ifdef OS2}
    default_os(target_OS2);
  {$endif}
  {$ifdef LINUX}
    default_os(target_LINUX);
  {$endif}
  {$ifdef WIN32}
    default_os(target_WIN32);
  {$endif}
  {$ifdef AMIGA}
    default_os(target_AMIGA);
  {$endif}
  {$ifdef ATARI}
    default_os(target_ATARI);
  {$endif}
  {$ifdef MACOS}
    default_os(target_MAC68k);
  {$endif}
{$endif}
end.
{
  $Log$
  Revision 1.10  1998-05-11 13:07:58  peter
    + $ifdef NEWPPU for the new ppuformat
    + $define GDB not longer required
    * removed all warnings and stripped some log comments
    * no findfirst/findnext anymore to remove smartlink *.o files

  Revision 1.9  1998/05/06 08:38:49  pierre
    * better position info with UseTokenInfo
      UseTokenInfo greatly simplified
    + added check for changed tree after first time firstpass
      (if we could remove all the cases were it happen
      we could skip all firstpass if firstpasscount > 1)
      Only with ExtDebug

  Revision 1.8  1998/05/04 20:19:54  peter
    * small fix for go32v2

  Revision 1.7  1998/05/04 17:54:29  peter
    + smartlinking works (only case jumptable left todo)
    * redesign of systems.pas to support assemblers and linkers
    + Unitname is now also in the PPU-file, increased version to 14

  Revision 1.6  1998/05/01 07:43:57  florian
    + basics for rtti implemented
    + switch $m (generate rtti for published sections)

  Revision 1.5  1998/04/29 10:34:06  pierre
    + added some code for ansistring (not complete nor working yet)
    * corrected operator overloading
    * corrected nasm output
    + started inline procedures
    + added starstarn : use ** for exponentiation (^ gave problems)
    + started UseTokenInfo cond to get accurate positions

  Revision 1.4  1998/04/27 15:45:20  peter
    + -Xl for smartlink
    + target_info.arext = .a

  Revision 1.3  1998/04/16 10:50:45  daniel
  * Fixed some things that were broken for OS/2.
}
