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
       { target operanting system }
       ttarget = (target_GO32V1,target_OS2,target_LINUX,
                  target_WIN32,target_GO32V2,
                  target_Amiga,target_Atari,target_Mac68k);

       tendian = (endian_little,en_big_endian);


       ttargetinfo = record
          target : ttarget;
          target_name : string[30];
          short_name : string[8];
          unit_env : string[20];
          system_unit : string[8];
          exeext,
          objext,
          dllext,
          unitext,
          libext,
          asmext,
          sourceext,
          pasext  : string[4];
          newline : string[3];
          labelprefix : string[2];
          Cprefix : string[2];
          use_function_relative_addresses : boolean;
          endian : tendian;
       end;

       tsourceinfo = record
          source:ttarget;
          source_name:string[30];
          exeext,
          scriptext : string[4];
          endian : tendian;
       end;

    var
       source_info : tsourceinfo;
       target_info : ttargetinfo;

    function set_string_target(const s : string) : boolean;

  implementation

    const
       target_infos : array[ttarget] of ttargetinfo = (
          (
            target : target_GO32V1;
            target_name : 'GO32 V1 DOS extender';
            short_name : 'GO32V1';
            unit_env : 'GO32V1UNITS';
            system_unit : 'SYSTEM';
            exeext : '';
            objext : '.O1';
            dllext : '.DLL';
            unitext : '.PP1';
            libext : '.PPL';
            asmext : '.S1';
            sourceext : '.PP';
            pasext : '.PAS';
            newline : #13#10;
            labelprefix : '.L';
            Cprefix : '_';
            use_function_relative_addresses : true;
            endian : endian_little
          ),
          (
            target : target_OS2;
            target_name : 'OS/2 (32 bit)';
            short_name : 'OS2';
            unit_env : 'OS2UNITS';
            system_unit : 'SYSOS2';
            exeext : '.exe';
            objext : '.oo2';
            dllext : '.dll';
            unitext : '.ppo';
            libext : '.ppl';
            asmext : '.so2';
            sourceext : '.pas';
            pasext : '.pp';
            newline : #13#10;
            labelprefix : 'L';
            Cprefix : ''; {???}
            use_function_relative_addresses : true;
            endian : endian_little
          ),
          (
            target : target_LINUX;
            target_name : 'Linux';
            short_name : 'LINUX';
            unit_env : 'LINUXUNITS';
            system_unit : 'syslinux';
            exeext : '';
            objext : '.o';
            dllext : '.so';
            unitext : '.ppu';
            libext : '.ppl';
            asmext : '.s';
            sourceext : '.pp';
            pasext : '.pas';
            newline : #10;
            labelprefix : '.L';
            Cprefix : '';
            use_function_relative_addresses : true;
            endian : endian_little
          ),
          (
            target : target_WIN32;
            target_name : 'Win32';
            short_name : 'WIN32';
            unit_env : 'WIN32UNITS';
            system_unit : 'SYSWIN32';
            exeext : '.exe';
            objext : '.o';
            dllext : '.dll';
            unitext : '.ppw';
            libext : '.ppl';
            asmext : '.s';
            sourceext : '.pp';
            pasext : '.pas';
            newline : #13#10;
            labelprefix : '.L';
            Cprefix : ''; {???}
            use_function_relative_addresses : true; {????}
            endian : endian_little
          ),
          (
            target : target_GO32V2;
            target_name : 'GO32 V2.0 DOS extender';
            short_name : 'GO32V2';
            unit_env : 'GO32V2UNITS';
            system_unit : 'SYSTEM';
            exeext : '.EXE';
            objext : '.O';
            dllext : '.DLL';
            unitext : '.PPU';
            libext : '.PPL';
            asmext : '.S';
            sourceext : '.PP';
            pasext : '.PAS';
            newline : #13#10;
            labelprefix : '.L';
            Cprefix : '_';
            use_function_relative_addresses : true;
            endian : endian_little
          ),
          (
            target : target_Amiga;
            target_name : 'Commodore Amiga';
            short_name : 'AMIGA';
            unit_env : '';
            system_unit : 'sysamiga';  { case sensitive }
            exeext : '';
            objext : '.o';
            dllext : '.library';
            unitext : '.ppa';
            libext : '.ppl';
            asmext : '.asm';
            sourceext : '.pp';
            pasext : '.pas';
            newline : #10;  { ??? }
            labelprefix : '.L';
            Cprefix : '';
            use_function_relative_addresses : true;
            endian : endian_little
          ),
          (
            target : target_Atari;
            target_name : 'Atari ST/STE';
            short_name : 'ATARI';
            unit_env : '';
            system_unit : 'SYSATARI';
            exeext : '.ttp';
            objext : '.o';
            dllext : '.dll';
            unitext : '.PPT';
            libext : '.PPL';
            asmext : '.s';
            sourceext : '.pp';
            pasext : '.pas';
            newline : #13#10;
            labelprefix : '.L';
            Cprefix : '';
            use_function_relative_addresses : true;
            endian : endian_little
          ),
          (
            target : target_Mac68k;
            target_name : 'Macintosh m68k';
            short_name : 'MAC OS';
            unit_env : '';
            system_unit : 'sysmac';    { case sensitive }
            exeext : '';
            objext : '.o';
            dllext : '.dll';
            unitext : '.ppm';
            libext : '.ppl';
            asmext : '.asm';
            sourceext : '.pp';
            pasext : '.pas';
            newline : #13;   { ??? }
            labelprefix : '__L';{ only ascii A..Z,a..z or _ allowed as first }
            Cprefix : '';
            use_function_relative_addresses : true;
            endian : endian_little
          )
       );

       source_infos : array[ttarget] of tsourceinfo = (
          (
            source : target_GO32V1;
            source_name : 'GO32 V1 DOS extender';
            exeext : '.EXE';
            scriptext : '.BAT';
            endian : endian_little
          ),
          (
            source : target_OS2;
            source_name : 'OS/2 (32 bit)';
            exeext : '.EXE';
            scriptext : '.CMD';
            endian : endian_little
          ),
          (
            source : target_LINUX;
            source_name : 'Linux';
            exeext : '';
            scriptext : '.sh';
            endian : endian_little
          ),
          (
            source : target_WIN32;
            source_name : 'Win32';
            exeext : '.EXE';
            scriptext : '.BAT';
            endian : endian_little
          ),
          (
            source : target_GO32V2;
            source_name : 'GO32 V2.0 DOS extender';
            exeext : '.EXE';
            scriptext : '.BAT';
            endian : endian_little
          ),
          (
            source : target_Amiga;
            source_name : 'Commodore Amiga';
            exeext : '';
            scriptext : '';
            endian : en_big_endian
          ),
          (
            source : target_Atari;
            source_name : 'Atari ST/STE';
            exeext : '.ttp';
            scriptext : '';
            endian : en_big_endian
          ),
          (
            source : target_Mac68k;
            source_name : 'Macintosh m68k';
            exeext : '';
            scriptext : '';
            endian : en_big_endian
          )
       );

    procedure set_target(t : ttarget);

      begin
         target_info:=target_infos[t];
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
         source_info:=source_infos[t];
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
  Revision 1.1  1998-03-25 11:18:15  root
  Initial revision

  Revision 1.33  1998/03/10 23:48:37  florian
    * a couple of bug fixes to get the compiler with -OGaxz compiler, sadly
      enough, it doesn't run

  Revision 1.32  1998/03/10 16:27:46  pierre
    * better line info in stabs debug
    * symtabletype and lexlevel separated into two fields of tsymtable
    + ifdef MAKELIB for direct library output, not complete
    + ifdef CHAINPROCSYMS for overloaded seach across units, not fully
      working
    + ifdef TESTFUNCRET for setting func result in underfunction, not
      working

  Revision 1.31  1998/03/10 01:17:29  peter
    * all files have the same header
    * messages are fully implemented, EXTDEBUG uses Comment()
    + AG... files for the Assembler generation

  Revision 1.30  1998/03/05 22:43:53  florian
    * some win32 support stuff added

  Revision 1.29  1998/03/02 22:04:36  carl
    + Added mac line break

  Revision 1.28  1998/03/02 13:38:51  peter
    + importlib object
    * doesn't crash on a systemunit anymore
    * updated makefile and depend

  Revision 1.25  1998/02/28 00:20:34  florian
    * more changes to get import libs for Win32 working

  Revision 1.24  1998/02/27 22:28:01  florian
    + win_targ unit
    + support of sections
    + new asmlists: sections, exports and resource

  Revision 1.23  1998/02/27 21:24:20  florian
    * dll support changed (dll name can be also a string contants)

  Revision 1.22  1998/02/23 02:55:08  carl
    + added correct extension to AMIGA libext

  Revision 1.21  1998/02/22 23:03:39  peter
    * renamed msource->mainsource and name->unitname
    * optimized filename handling, filename is not seperate anymore with
      path+name+ext, this saves stackspace and a lot of fsplit()'s
    * recompiling of some units in libraries fixed
    * shared libraries are working again
    + $LINKLIB <lib> to support automatic linking to libraries
    + libraries are saved/read from the ppufile, also allows more libraries
      per ppufile

  Revision 1.20  1998/02/18 14:14:44  michael
  * removed entries for dos_targ and lin_targ

  Revision 1.19  1998/02/17 21:21:05  peter
    + Script unit
    + __EXIT is called again to exit a program
    - target_info.link/assembler calls
    * linking works again for dos
    * optimized a few filehandling functions
    * fixed stabs generation for procedures

  Revision 1.18  1998/02/14 01:45:35  peter
    * more fixes
    - pmode target is removed
    - search_as_ld is removed, this is done in the link.pas/assemble.pas
    + findexe() to search for an executable (linker,assembler,binder)

  Revision 1.17  1998/02/13 22:26:45  peter
    * fixed a few SigSegv's
    * INIT$$ was not written for linux!
    * assembling and linking works again for linux and dos
    + assembler object, only attasmi3 supported yet
    * restore pp.pas with AddPath etc.

  Revision 1.16  1998/02/13 10:35:50  daniel
  * Made Motorola version compilable.
  * Fixed optimizer

  Revision 1.15  1998/02/12 17:19:32  florian
    * fixed to get remake3 work, but needs additional fixes (output, I don't like
      also that aktswitches isn't a pointer)

  Revision 1.14  1998/02/12 11:50:50  daniel
  Yes! Finally! After three retries, my patch!

  Changes:

  Complete rewrite of psub.pas.
  Added support for DLL's.
  Compiler requires less memory.
  Platform units for each platform.

  Revision 1.11  1998/01/26 16:42:01  daniel
  * Reversed source_ext and pas_ext for OS/2 target. The .pas extension is
  recognized by the Workplace Shell of OS/2, the .pp is not.

  Revision 1.10  1998/01/26 13:35:33  florian
    * adapted to work with TP

  Revision 1.9  1998/01/25 18:45:50  peter
    + Search for as and ld at startup
    + source_info works the same as target_info
    + externlink allows only external linking

  Revision 1.8  1998/01/22 08:57:55  peter
    + added target_info.pasext and target_info.libext

  Revision 1.7  1998/01/09 19:44:09  carl
    * labels for mac68k target now use the MPW correct syntax

  Revision 1.6  1997/12/12 13:28:42  florian
  + version 0.99.0
  * all WASM options changed into MASM
  + -O2 for Pentium II optimizations

  Revision 1.5  1997/12/09 14:12:21  carl
  + added planned m68k systems, and fixed some problems in amiga info.

  Revision 1.4  1997/12/08 11:53:49  pierre
      reverted to old version of systems.pas,
      Daniel's version is not compilable due to the bug (corrected) of
      mil value for a procvar const !!

  Revision 1.1.1.1  1997/11/27 08:33:02  michael
  FPC Compiler CVS start

  Pre-CVS log:

    CEC    Carl-Eric Codere
    FK     Florian Klaempfl
    +      feature added
    -      removed
    *      bug fixed or changed

  History:
      15th october 1996:
         + ttargetinfo.newline added (FK)
      19th september 1997:
         * the suffix of GO32V1 units is now PP1 (FK)
       8th october 1997:
         + target amiga added for tests, unit should divided
           into sysi386 and sysm68k (FK)
}
