{
    $Id$
    Copyright (C) 1995,97 by Florian Klaempfl

    This unit contains information about the target systems supported
    (these are not processor specific)

    This progsam is free software; you can redistribute it and/or modify
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

  interface

   type
       tendian = (endian_little,endian_big);

       ttargetcpu=(no_cpu
            ,i386,m68k,alpha
       );

       tprocessors = (no_processor
            ,Class386,ClassP5,ClassP6
            ,MC68000,MC68100,MC68020
       );


     type
       tasmmode= (asmmode_none
            ,asmmode_i386_direct,asmmode_i386_att,asmmode_i386_intel
            ,asmmode_m68k_mot
       );
     const
       {$ifdef i386} i386asmmodecnt=3; {$else} i386asmmodecnt=0; {$endif}
       {$ifdef m68k} m68kasmmodecnt=1; {$else} m68kasmmodecnt=0; {$endif}
       asmmodecnt=i386asmmodecnt+m68kasmmodecnt+1;

     type
       ttarget = (target_none
            ,target_i386_GO32V1,target_i386_GO32V2,target_i386_linux,
            target_i386_OS2,target_i386_Win32
            ,target_m68k_Amiga,target_m68k_Atari,target_m68k_Mac,
            target_m68k_linux,target_m68k_PalmOS
       );
     const
       {$ifdef i386} i386targetcnt=5; {$else} i386targetcnt=0; {$endif}
       {$ifdef m68k} m68ktargetcnt=5; {$else} m68ktargetcnt=0; {$endif}
       targetcnt=i386targetcnt+m68ktargetcnt+1;

     type
       tasm = (as_none
            ,as_i386_o,as_i386_o_aout,as_i386_asw,
            as_i386_nasmcoff,as_i386_nasmelf,as_i386_nasmobj,
            as_i386_tasm,as_i386_masm
            ,as_m68k_o,as_m68k_gas,as_m68k_mit,as_m68k_mot,as_m68k_mpw
       );
     const
       {$ifdef i386} i386asmcnt=8; {$else} i386asmcnt=0; {$endif}
       {$ifdef m68k} m68kasmcnt=5; {$else} m68kasmcnt=0; {$endif}
       asmcnt=i386asmcnt+m68kasmcnt+1;

     type
       tlink = (link_none
            ,link_i386_ld,link_i386_ldgo32v1,
            link_i386_ldgo32v2,link_i386_ldw,
            link_i386_ldos2
            ,link_m68k_ld
       );
     const
       {$ifdef i386} i386linkcnt=5; {$else} i386linkcnt=0; {$endif}
       {$ifdef m68k} m68klinkcnt=1; {$else} m68klinkcnt=0; {$endif}
       linkcnt=i386linkcnt+m68klinkcnt+1;

     type
       tar = (ar_none
            ,ar_i386_ar,ar_i386_arw
            ,ar_m68k_ar
       );
     const
       {$ifdef i386} i386arcnt=2; {$else} i386arcnt=0; {$endif}
       {$ifdef m68k} m68karcnt=1; {$else} m68karcnt=0; {$endif}
       arcnt=i386arcnt+m68karcnt+1;

     type
       tos = ( os_none,
            os_i386_GO32V1,os_i386_GO32V2,os_i386_Linux,os_i386_OS2,
            os_i386_Win32,
            os_m68k_Amiga,os_m68k_Atari,os_m68k_Mac,os_m68k_Linux,
            os_m68k_PalmOS
       );
     const
       i386oscnt=5;
       m68koscnt=5;
       oscnt=i386oscnt+m68koscnt+1;

   type
       tosinfo = packed record
          id        : tos;
          name      : string[30];
          shortname : string[8];
          sharedlibext,
          staticlibext,
          sourceext,
          pasext,
          exeext,
          scriptext : string[4];
          libprefix : string[3];
          Cprefix   : string[2];
          newline   : string[2];
          endian    : tendian;
          stackalignment : {longint this is a little overkill no ?? }byte;
          size_of_pointer : byte;
          size_of_longint : byte;
          use_bound_instruction : boolean;
          use_function_relative_addresses : boolean;
       end;

       tasminfo = packed record
          id          : tasm;
          idtxt       : string[8];
          asmbin      : string[8];
          asmcmd      : string[50];
          externals   : boolean;
          labelprefix : string[2];
          comment     : string[2];
       end;

       tlinkinfo = packed record
          id            : tlink;
          linkbin       : string[8];
          linkcmd       : string[127];
          binders       : word;
          bindbin       : array[1..2]of string[8];
          bindcmd       : array[1..2]of string[127];
          stripopt      : string[2];
          libpathprefix : string[13];
          libpathsuffix : string[2];
          groupstart    : string[8];
          groupend      : string[2];
          inputstart    : string[8];
          inputend      : string[2];
          libprefix     : string[2];
       end;

       tarinfo = packed record
          id      : tar;
          arbin   : string[8];
          arcmd   : string[50];
       end;

       ttargetinfo = packed record
          target      : ttarget;
          cpu         : ttargetcpu;
          short_name  : string[8];
          unit_env    : string[12];
          system_unit : string[8];
          smartext,
          unitext,
          unitlibext,
          asmext,
          objext,
          exeext      : string[4];
          os          : tos;
          link        : tlink;
          assem       : tasm;
          ar          : tar;
          heapsize,
          maxheapsize,
          stacksize   : longint;
       end;

       tasmmodeinfo=packed record
          id    : tasmmode;
          idtxt : string[8];
       end;

    var
       target_cpu  : ttargetcpu;
       target_info : ttargetinfo;
       target_os   : tosinfo;
       target_asm  : tasminfo;
       target_link : tlinkinfo;
       target_ar   : tarinfo;
       source_os   : tosinfo;

    function set_string_target(s : string) : boolean;
    function set_string_asm(s : string) : boolean;
    function set_string_asmmode(s:string;var t:tasmmode):boolean;


implementation

    const

{****************************************************************************
                                 OS Info
****************************************************************************}
       os_infos : array[1..oscnt] of tosinfo = (
          (
            id           : os_none;
            name         : 'No operating system';
            shortname    : 'none'
          ),
          (
            id           : os_i386_go32v1;
            name         : 'GO32 V1 DOS extender';
            shortname    : 'go32v1';
            sharedlibext : '.dll';
            staticlibext : '.a';
            sourceext    : '.pp';
            pasext       : '.pas';
            exeext       : '';      { No .exe, the linker only output a.out ! }
            scriptext    : '.bat';
            libprefix    : '';
            Cprefix      : '_';
            newline      : #13#10;
            endian       : endian_little;
            stackalignment : 2;
            size_of_pointer : 4;
            size_of_longint : 4;
            use_bound_instruction : false;
            use_function_relative_addresses : true
          ),
          (
            id           : os_i386_go32v2;
            name         : 'GO32 V2 DOS extender';
            shortname    : 'go32v2';
            sharedlibext : '.dll';
            staticlibext : '.a';
            sourceext    : '.pp';
            pasext       : '.pas';
            exeext       : '.exe';
            scriptext    : '.bat';
            libprefix    : '';
            Cprefix      : '_';
            newline      : #13#10;
            endian       : endian_little;
            stackalignment : 2;
            size_of_pointer : 4;
            size_of_longint : 4;
            use_bound_instruction : true;
            use_function_relative_addresses : true
          ),
          (
            id           : os_i386_linux;
            name         : 'Linux for i386';
            shortname    : 'linux';
            sharedlibext : '.so';
            staticlibext : '.a';
            sourceext    : '.pp';
            pasext       : '.pas';
            exeext       : '';
            scriptext    : '.sh';
            libprefix    : 'lib';
            Cprefix      : '';
            newline      : #10;
            endian       : endian_little;
            stackalignment : 4;
            size_of_pointer : 4;
            size_of_longint : 4;
            use_bound_instruction : false;
            use_function_relative_addresses : true
          ),
          (
            id           : os_i386_os2;
            name         : 'OS/2 via EMX';
            shortname    : 'os2';
            sharedlibext : '.ao2';
            staticlibext : '.ao2';
            sourceext    : '.pas';
            pasext       : '.pp';
            exeext       : '.exe';
            scriptext    : '.cmd';
            libprefix    : '';
            Cprefix      : '_';
            newline      : #13#10;
            endian       : endian_little;
            stackalignment : 2;
            size_of_pointer : 4;
            size_of_longint : 4;
            use_bound_instruction : false;
            use_function_relative_addresses : false
          ),
          (
            id           : os_i386_win32;
            name         : 'Win32 for i386';
            shortname    : 'win32';
            sharedlibext : '.dll';
            staticlibext : '.aw';
            sourceext    : '.pp';
            pasext       : '.pas';
            exeext       : '.exe';
            scriptext    : '.bat';
            libprefix    : 'lib';
            Cprefix      : '_';
            newline      : #13#10;
            endian       : endian_little;
            stackalignment : 4;
            size_of_pointer : 4;
            size_of_longint : 4;
            use_bound_instruction : true;
            use_function_relative_addresses : true
          ),
          (
            id           : os_m68k_amiga;
            name         : 'Commodore Amiga';
            shortname    : 'amiga';
            sharedlibext : '.library';
            staticlibext : '.a';
            sourceext    : '.pp';
            pasext       : '.pas';
            exeext       : '';
            scriptext    : '';
            libprefix    : '';
            Cprefix      : '_';
            newline      : #10;
            endian       : endian_big;
            stackalignment : 2;
            size_of_pointer : 4;
            size_of_longint : 4;
            use_bound_instruction : false;
            use_function_relative_addresses : false
          ),
          (
            id           : os_m68k_atari;
            name         : 'Atari ST/STE';
            shortname    : 'atari';
            sharedlibext : '.dll';
            staticlibext : '.a';
            sourceext    : '.pp';
            pasext       : '.pas';
            exeext       : '.tpp';
            scriptext    : '';
            libprefix    : '';
            Cprefix      : '_';
            newline      : #10;
            endian       : endian_big;
            stackalignment : 2;
            size_of_pointer : 4;
            size_of_longint : 4;
            use_bound_instruction : false;
            use_function_relative_addresses : false
          ),
          (
            id           : os_m68k_mac;
            name         : 'Macintosh m68k';
            shortname    : 'mac';
            sharedlibext : '.dll';
            staticlibext : '.a';
            sourceext    : '.pp';
            pasext       : '.pas';
            exeext       : '.tpp';
            scriptext    : '';
            libprefix    : '';
            Cprefix      : '_';
            newline      : #13;
            endian       : endian_big;
            stackalignment : 2;
            size_of_pointer : 4;
            size_of_longint : 4;
            use_bound_instruction : false;
            use_function_relative_addresses : false
          ),
          (
            id           : os_m68k_linux;
            name         : 'Linux for m68k';
            shortname    : 'linux';
            sharedlibext : '.so';
            staticlibext : '.a';
            sourceext    : '.pp';
            pasext       : '.pas';
            exeext       : '';
            scriptext    : '.sh';
            libprefix    : 'lib';
            Cprefix      : '';
            newline      : #10;
            endian       : endian_big;
            stackalignment : 2;
            size_of_pointer : 4;
            size_of_longint : 4;
            use_bound_instruction : false;
            use_function_relative_addresses : true
          ),
          (
            id           : os_m68k_palmos;
            name         : 'PalmOS';
            shortname    : 'palmos';
            sharedlibext : '.so';
            staticlibext : '.a';
            sourceext    : '.pp';
            pasext       : '.pas';
            exeext       : '';
            scriptext    : '.sh';
            libprefix    : 'lib';
            Cprefix      : '_';
            newline      : #10;
            endian       : endian_big;
            stackalignment : 2;
            size_of_pointer : 4;
            size_of_longint : 4;
            use_bound_instruction : false;
            use_function_relative_addresses : false
          )
          );


{****************************************************************************
                             Assembler Info
****************************************************************************}

       as_infos : array[1..asmcnt] of tasminfo = (
          (
            id     : as_none;
            idtxt  : 'no'
          )
{$ifdef i386}
          ,(
            id     : as_i386_o;
            idtxt  : 'O';
            asmbin : 'as';
            asmcmd : '-o $OBJ $ASM';
            externals : false;
            labelprefix : '.L';
            comment : '# '
          )
          ,(
            id     : as_i386_o_aout;
            idtxt  : 'O_AOUT';
            asmbin : 'as';
            asmcmd : '-o $OBJ $ASM';
            externals : false;
            labelprefix : 'L';
            comment : '# '
          )
          ,(
            id     : as_i386_asw;
            idtxt  : 'ASW';
            asmbin : 'asw';
            asmcmd : '-o $OBJ $ASM';
            externals : false;
            labelprefix : '.L';
            comment : '# '
          )
          ,(
            id     : as_i386_nasmcoff;
            idtxt  : 'NASMCOFF';
            asmbin : 'nasm';
            asmcmd : '-f coff -o $OBJ $ASM';
            externals : true;
            labelprefix : 'L';
            comment : '; '
          )
          ,(
            id     : as_i386_nasmelf;
            idtxt  : 'NASMELF';
            asmbin : 'nasm';
            asmcmd : '-f elf -o $OBJ $ASM';
            externals : true;
            labelprefix : 'L';
            comment : '; '
          )
          ,(
            id     : as_i386_nasmobj;
            idtxt  : 'NASMOBJ';
            asmbin : 'nasm';
            asmcmd : '-f obj -o $OBJ $ASM';
            externals : true;
            labelprefix : 'L';
            comment : '; '
          )
          ,(
            id     : as_i386_tasm;
            idtxt  : 'TASM';
            asmbin : 'tasm';
            asmcmd : '/m2 $ASM $OBJ';
            externals : true;
            labelprefix : '.L';
            comment : '; '
          )
          ,(
            id     : as_i386_masm;
            idtxt  : 'MASM';
            asmbin : 'masm';
            asmcmd : '$ASM $OBJ';
            externals : true;
            labelprefix : '.L';
            comment : '; '
          )
{$endif i386}
{$ifdef m68k}
          ,(
            id     : as_m68k_o;
            idtxt  : 'O';
            asmbin : 'as';
            asmcmd : '-o $OBJ $ASM';
            externals : false;
            labelprefix : '.L';
            comment : '# '
          )
          ,(
            id     : as_m68k_gas;
            idtxt  : 'GAS';
            asmbin : 'as68k'; { Gas for the Amiga}
            asmcmd : '--register-prefix-optional -o $OBJ $ASM';
            externals : false;
            labelprefix : '.L';
            comment : '| '
          )
          ,(
            id     : as_m68k_mit;
            idtxt  : 'MIT';
            asmbin : '';
            asmcmd : '-o $OBJ $ASM';
            externals : false;
            labelprefix : '.L';
            comment : '| '
          )
          ,(
            id     : as_m68k_mot;
            idtxt  : 'MOT';
            asmbin : '';
            asmcmd : '-o $OBJ $ASM';
            externals : false;
            labelprefix : '__L';
            comment : '| '
          )
          ,(
            id     : as_m68k_mpw;
            idtxt  : 'MPW';
            asmbin : '';
            asmcmd : '-model far -o $OBJ $ASM';
            externals : false;
            labelprefix : '__L';
            comment : '| '
          )
{$endif m68k}
          );

{****************************************************************************
                            Linker Info
****************************************************************************}
       link_infos : array[1..linkcnt] of tlinkinfo = (
          (
            id      : link_none
          )
{$ifdef i386}
          ,(
            id      : link_i386_ld;
            linkbin : 'ld';
            linkcmd : '$OPT -o $EXE $RES';
{* Changes made by Ozerski 23.10.1998}
            binders:0;
            bindbin : ('','');
            bindcmd : ('','');
{* End changes}
            stripopt   : '-s';
            libpathprefix : 'SEARCH_DIR(';
            libpathsuffix : ')';
            groupstart : 'GROUP(';
            groupend   : ')';
            inputstart : 'INPUT(';
            inputend   : ')';
            libprefix  : '-l'
          )
          ,(
            id      : link_i386_ldgo32v1;
            linkbin : 'ld';
            linkcmd : '-oformat coff-go32 $OPT -o $EXE @$RES';
{* Changes made by Ozerski 23.10.1998}
            binders:1;
            bindbin : ('aout2exe','');
            bindcmd : ('$EXE','');
{* End changes}
            stripopt   : '-s';
            libpathprefix : '-L';
            libpathsuffix : '';
            groupstart : '-(';
            groupend   : '-)';
            inputstart : '';
            inputend   : '';
            libprefix  : '-l'
          )
          ,(
            id      : link_i386_ldgo32v2;
            linkbin : 'ld';
            linkcmd : '-oformat coff-go32-exe $OPT -o $EXE @$RES';
{* Changes made by Ozerski 23.10.1998}
            binders:0;
            bindbin : ('','');
            bindcmd : ('','');
{* End changes}
            stripopt   : '-s';
            libpathprefix : '-L';
            libpathsuffix : '';
            groupstart : '-(';
            groupend   : '-)';
            inputstart : '';
            inputend   : '';
            libprefix  : '-l'
          )
          ,(
            id      : link_i386_ldw;
            linkbin : 'ldw';
            linkcmd : '$OPT -o $EXE $RES';
{* Changes made by Ozerski 23.10.1998}
            binders:0;
            bindbin : ('dlltool','ldw');
            bindcmd : ('--as asw.exe --dllname $EXE --output-exp exp.$$$',
                       '-s $OPT -o $EXE $RES exp.$$$');
{* End changes}
            stripopt   : '-s';
            libpathprefix : 'SEARCH_DIR(';
            libpathsuffix : ')';
            groupstart : 'GROUP(';
            groupend   : ')';
            inputstart : 'INPUT(';
            inputend   : ')';
            libprefix  : '-l'
          )
          ,(
            id      : link_i386_ldos2;
            linkbin : 'ld';  { Os/2 }
            linkcmd : '-o $EXE @$RES';
{* Changes made by Ozerski 23.10.1998}
            binders:1;
            bindbin : ('emxbind','');
            bindcmd : ('-b -k$STACKKB -h$HEAPMB -o $EXE.exe $EXE -aim -s$DOSHEAPKB','');
{* End changes}
            stripopt   : '-s';
            libpathprefix : '-L';
            libpathsuffix : '';
            groupstart : ''; {Linker is too primitive...}
            groupend   : '';
            inputstart : '';
            inputend   : '';
            libprefix  : '-l'
          )
{$endif i386}
{$ifdef m68k}
          ,(
            id      : link_m68k_ld;
            linkbin : 'ld';
            linkcmd : '$OPT -o $EXE $RES';
{* Changes made by Ozerski 23.10.1998}
            binders:0;
            bindbin : ('','');
            bindcmd : ('','');
{* End changes}
            stripopt   : '-s';
            libpathprefix : 'SEARCH_DIR(';
            libpathsuffix : ')';
            groupstart : 'GROUP(';
            groupend   : ')';
            inputstart : 'INPUT(';
            inputend   : ')';
            libprefix  : '-l'
          )
{$endif m68k}
          );

{****************************************************************************
                                 Ar Info
****************************************************************************}
           ar_infos : array[1..arcnt] of tarinfo = (
          (
            id    : ar_none
          )
{$ifdef i386}
          ,(
            id    : ar_i386_ar;
            arbin : 'ar';
            arcmd : 'rs $LIB $FILES'
          ),
          (
            id    : ar_i386_arw;
            arbin : 'arw';
            arcmd : 'rs $LIB $FILES'
          )
{$endif i386}
{$ifdef m68k}
          ,(
            id    : ar_m68k_ar;
            arbin : 'ar';
            arcmd : 'rs $LIB $FILES'
          )
{$endif m68k}
          );

{****************************************************************************
                            Targets Info
****************************************************************************}
       target_infos : array[1..targetcnt] of ttargetinfo = (
          (
            target      : target_none;
            cpu         : no_cpu;
            short_name  : 'notarget'
          )
{$ifdef i386}
          ,(
            target      : target_i386_GO32V1;
            cpu         : i386;
            short_name  : 'GO32V1';
            unit_env    : 'GO32V1UNITS';
            system_unit : 'SYSTEM';
            smartext    : '.sl';
            unitext     : '.pp1';
            unitlibext  : '.ppl';
            asmext      : '.s1';
            objext      : '.o1';
            exeext      : ''; { The linker produces a.out }
            os          : os_i386_GO32V1;
            link        : link_i386_ldgo32v1;
            assem       : as_i386_o;
            ar          : ar_i386_ar;
            heapsize    : 2048*1024;
            maxheapsize : 32768*1024;
            stacksize   : 16384
          ),
          (
            target      : target_i386_GO32V2;
            cpu         : i386;
            short_name  : 'GO32V2';
            unit_env    : 'GO32V2UNITS';
            system_unit : 'SYSTEM';
            smartext    : '.sl';
            unitext     : '.ppu';
            unitlibext  : '.ppl';
            asmext      : '.s';
            objext      : '.o';
            exeext      : '.exe';
            os          : os_i386_GO32V2;
            link        : link_i386_ldgo32v2;
            assem       : as_i386_o;
            ar          : ar_i386_ar;
            heapsize    : 2048*1024;
            maxheapsize : 32768*1024;
            stacksize   : 16384
          ),
          (
            target      : target_i386_LINUX;
            cpu         : i386;
            short_name  : 'LINUX';
            unit_env    : 'LINUXUNITS';
            system_unit : 'syslinux';
            smartext    : '.sl';
            unitext     : '.ppu';
            unitlibext  : '.ppl';
            asmext      : '.s';
            objext      : '.o';
            exeext      : '';
            os          : os_i386_Linux;
            link        : link_i386_ld;
            assem       : as_i386_o;
            ar          : ar_i386_ar;
            heapsize    : 2048*1024;
            maxheapsize : 32768*1024;
            stacksize   : 8192
          ),
          (
            target      : target_i386_OS2;
            cpu         : i386;
            short_name  : 'OS2';
            unit_env    : 'OS2UNITS';
            system_unit : 'SYSOS2';
            smartext    : '.sl';
            unitext     : '.ppo';
            unitlibext  : '.ppl';
            asmext      : '.so2';
            objext      : '.oo2';
            exeext      : ''; { The linker produces a.out }
            os          : os_i386_OS2;
            link        : link_i386_ldos2;
            assem       : as_i386_o_aout;
            ar          : ar_i386_ar;
            heapsize    : 256*1024;
            maxheapsize : 32768*1024;
            stacksize   : 32768
          ),
          (
            target      : target_i386_WIN32;
            cpu         : i386;
            short_name  : 'WIN32';
            unit_env    : 'WIN32UNITS';
            system_unit : 'SYSWIN32';
            smartext    : '.slw';
            unitext     : '.ppw';
            unitlibext  : '.ppl';
            asmext      : '.sw';
            objext      : '.ow';
            exeext      : '.exe';
            os          : os_i386_Win32;
            link        : link_i386_ldw;
            assem       : as_i386_asw;
            ar          : ar_i386_arw;
            heapsize    : 2048*1024;
            maxheapsize : 32768*1024;
            stacksize   : 32768
          )
{$endif i386}
{$ifdef m68k}
          ,(
            target      : target_m68k_Amiga;
            cpu         : m68k;
            short_name  : 'AMIGA';
            unit_env    : '';
            system_unit : 'sysamiga';
            smartext    : '.sl';
            unitext     : '.ppa';
            unitlibext  : '.ppl';
            asmext      : '.asm';
            objext      : '.o';
            exeext      : '';
            os          : os_m68k_Amiga;
            link        : link_m68k_ld;
            assem       : as_m68k_o;
            ar          : ar_m68k_ar;
            heapsize    : 128*1024;
            maxheapsize : 32768*1024;
            stacksize   : 8192
          ),
          (
            target      : target_m68k_Atari;
            cpu         : m68k;
            short_name  : 'ATARI';
            unit_env    : '';
            system_unit : 'SYSATARI';
            smartext    : '.sl';
            unitext     : '.ppt';
            unitlibext  : '.ppl';
            asmext      : '.s';
            objext      : '.o';
            exeext      : '.ttp';
            os          : os_m68k_Atari;
            link        : link_m68k_ld;
            assem       : as_m68k_o;
            ar          : ar_m68k_ar;
            heapsize    : 16*1024;
            maxheapsize : 32768*1024;
            stacksize   : 8192
          ),
          (
            target      : target_m68k_Mac;
            cpu         : m68k;
            short_name  : 'MACOS';
            unit_env    : '';
            system_unit : 'sysmac';
            smartext    : '.sl';
            unitext     : '.ppt';
            unitlibext  : '.ppl';
            asmext      : '.a';
            objext      : '.o';
            exeext      : '';
            os          : os_m68k_Mac;
            link        : link_m68k_ld;
            assem       : as_m68k_mpw;
            ar          : ar_m68k_ar;
            heapsize    : 128*1024;
            maxheapsize : 32768*1024;
            stacksize   : 8192
          ),
          (
            target      : target_m68k_linux;
            cpu         : m68k;
            short_name  : 'LINUX';
            unit_env    : 'LINUXUNITS';
            system_unit : 'syslinux';
            smartext    : '.sl';
            unitext     : '.ppu';
            unitlibext  : '.ppl';
            asmext      : '.s';
            objext      : '.o';
            exeext      : '';
            os          : os_m68k_Linux;
            link        : link_m68k_ld;
            assem       : as_m68k_o;
            ar          : ar_m68k_ar;
            heapsize    : 128*1024;
            maxheapsize : 32768*1024;
            stacksize   : 8192
          ),
          (
            target      : target_m68k_PalmOS;
            cpu         : m68k;
            short_name  : 'PALMOS';
            unit_env    : 'PALMUNITS';
            system_unit : 'syspalm';
            smartext    : '.sl';
            unitext     : '.ppu';
            unitlibext  : '.ppl';
            asmext      : '.s';
            objext      : '.o';
            exeext      : '';
            os          : os_m68k_PalmOS;
            link        : link_m68k_ld;
            assem       : as_m68k_o;
            ar          : ar_m68k_ar;
            heapsize    : 128*1024;
            maxheapsize : 32768*1024;
            stacksize   : 8192
          )
{$endif m68k}
          );

{****************************************************************************
                             AsmModeInfo
****************************************************************************}
       asmmodeinfos : array[1..asmmodecnt] of tasmmodeinfo = (
          (
            id    : asmmode_none;
            idtxt : 'none'
          )
{$ifdef i386}
          ,(
            id    : asmmode_i386_direct;
            idtxt : 'DIRECT'
          ),
          (
            id    : asmmode_i386_att;
            idtxt : 'ATT'
          ),
          (
            id    : asmmode_i386_intel;
            idtxt : 'INTEL'
          )
{$endif i386}
{$ifdef m68k}
          ,(
            id    : asmmode_m68k_mot;
            idtxt : 'MOT'
          )
{$endif m68k}
          );

{****************************************************************************
                                Helpers
****************************************************************************}

function upper(const s : string) : string;
var
  i  : longint;
begin
  for i:=1 to length(s) do
   if s[i] in ['a'..'z'] then
    upper[i]:=char(byte(s[i])-32)
   else
    upper[i]:=s[i];
{$ifndef TP}
  {$ifopt H+}
    SetLength(upper,length(s));
  {$else}
    upper[0]:=s[0];
  {$endif}
{$else}
  upper[0]:=s[0];
{$endif}
end;


function set_target_os(t:tos):boolean;
var
  i : longint;
begin
  set_target_os:=false;
  { target 1 is none }
  for i:=2 to oscnt do
   if os_infos[i].id=t then
    begin
      target_os:=os_infos[i];
      set_target_os:=true;
      exit;
    end;
end;


function set_target_asm(t:tasm):boolean;
var
  i : longint;
begin
  set_target_asm:=false;
  for i:=1 to asmcnt do
   if as_infos[i].id=t then
    begin
      target_asm:=as_infos[i];
      set_target_asm:=true;
      exit;
    end;
end;


function set_target_link(t:tlink):boolean;
var
  i : longint;
begin
  set_target_link:=false;
  for i:=1 to linkcnt do
   if link_infos[i].id=t then
    begin
      target_link:=link_infos[i];
      set_target_link:=true;
      exit;
    end;
end;


function set_target_ar(t:tar):boolean;
var
  i : longint;
begin
  set_target_ar:=false;
  for i:=1 to arcnt do
   if ar_infos[i].id=t then
    begin
      target_ar:=ar_infos[i];
      set_target_ar:=true;
      exit;
    end;
end;


function set_target_info(t:ttarget):boolean;
var
  i : longint;
begin
  set_target_info:=false;
  for i:=1 to targetcnt do
   if target_infos[i].target=t then
    begin
      target_info:=target_infos[i];
      set_target_os(target_info.os);
      set_target_asm(target_info.assem);
      set_target_link(target_info.link);
      set_target_ar(target_info.ar);
      target_cpu:=target_info.cpu;
      set_target_info:=true;
      exit;
    end;
end;


{****************************************************************************
                             Load from string
****************************************************************************}

function set_string_target(s : string) : boolean;
var
  i : longint;
begin
  set_string_target:=false;
  { this should be case insensitive !! PM }
  s:=upper(s);
  for i:=1 to targetcnt do
   if target_infos[i].short_name=s then
    begin
      target_info:=target_infos[i];
      set_target_os(target_info.os);
      set_target_asm(target_info.assem);
      set_target_link(target_info.link);
      set_target_ar(target_info.ar);
      target_cpu:=target_info.cpu;
      set_string_target:=true;
      exit;
    end;
end;


function set_string_asm(s : string) : boolean;
var
  i : longint;
begin
  set_string_asm:=false;
  { this should be case insensitive !! PM }
  s:=upper(s);
  for i:=1 to asmcnt do
   if as_infos[i].idtxt=s then
    begin
      target_asm:=as_infos[i];
      set_string_asm:=true;
    end;
end;


function set_string_asmmode(s:string;var t:tasmmode):boolean;
var
  i : longint;
begin
  set_string_asmmode:=false;
  { this should be case insensitive !! PM }
  s:=upper(s);
  for i:=1 to asmmodecnt do
   if asmmodeinfos[i].idtxt=s then
    begin
      t:=asmmodeinfos[i].id;
      set_string_asmmode:=true;
    end;
end;


{****************************************************************************
                      Initialization of default target
****************************************************************************}

procedure default_os(t:ttarget);
begin
  set_target_info(t);
  if source_os.name='' then
    source_os:=target_os;
end;


procedure set_source_os(t:tos);
var
  i : longint;
begin
{ can't use message() here (PFV) }
  if source_os.name<>'' then
    Writeln('Warning: Source OS Redefined!');
  for i:=1 to oscnt do
   if os_infos[i].id=t then
    begin
      source_os:=os_infos[i];
      exit;
    end;
end;


begin
{ first get source OS }
  source_os.name:='';
{ please note then we use cpu86 and cpu68 here on purpose !! }
{$ifdef cpu86}
  {$ifdef GO32V1}
    set_source_os(os_i386_GO32V1);
  {$else}
    {$ifdef GO32V2}
      set_source_os(os_i386_GO32V2);
    {$else}
      {$ifdef OS2}
        set_source_os(os_i386_OS2);
      {$else}
        {$ifdef LINUX}
          set_source_os(os_i386_LINUX);
        {$else}
          {$ifdef WIN32}
            set_source_os(os_i386_WIN32);
          {$endif win32}
        {$endif linux}
      {$endif os2}
    {$endif go32v2}
  {$endif go32v1}
{$endif cpu86}
{$ifdef cpu68}
  {$ifdef AMIGA}
    set_source_os(os_m68k_Amiga);
  {$else}
    {$ifdef ATARI}
      set_source_os(os_m68k_Atari);
    {$else}
      {$ifdef MACOS}
        set_source_os(os_m68k_MAC);
      {$else}
        {$ifdef LINUX}
          set_source_os(os_m68k_linux);
        {$endif linux}
      {$endif macos}
    {$endif atari}
  {$endif amiga}
{$endif cpu68}

{ Now default target !! }
{$ifdef i386}
  {$ifdef GO32V1}
     default_os(target_i386_GO32V1);
  {$else}
    {$ifdef GO32V2}
      default_os(target_i386_GO32V2);
    {$else}
      {$ifdef OS2}
        default_os(target_i386_OS2);
      {$else}
        {$ifdef LINUX}
          default_os(target_i386_LINUX);
        {$else}
           {$ifdef WIN32}
             default_os(target_i386_WIN32);
           {$else}
             default_os(target_i386_GO32V2);
           {$endif win32}
        {$endif linux}
      {$endif os2}
    {$endif go32v2}
  {$endif go32v1}
{$endif i386}
{$ifdef m68k}
  {$ifdef AMIGA}
    default_os(target_m68k_Amiga);
  {$else}
    {$ifdef ATARI}
      default_os(target_m68k_Atari);
    {$else}
      {$ifdef MACOS}
        default_os(target_m68k_Mac);
      {$else}
        {$ifdef LINUX}
          default_os(target_m68k_linux);
        {$else}
          default_os(target_m68k_Amiga);
        {$endif linux}
      {$endif macos}
    {$endif atari}
  {$endif amiga}
{$endif m68k}
end.
{
  $Log$
  Revision 1.1  1998-12-22 14:27:54  peter
    * moved

  Revision 1.2  1998/12/18 17:17:05  peter
    * updated with new versions from compiler

  Revision 1.1  1998/12/10 23:54:28  peter
    * initial version of the FV IDE
    * initial version of a fake compiler

}
