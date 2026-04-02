{
    Copyright (c) 1998-2002 by Peter Vreman

    This unit implements directive parsing for the scanner

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
unit scandir;

{$i fpcdefs.inc}

  interface

    uses
      globtype,compilerbase,
      systems,scanner;

    const
      switchesstatestackmax = 20;

    type
      tsavedswitchesstate = record
        localsw: tlocalswitches;
        verbosity: longint;
        pmessage : pmessagestaterecord;
        alignment : talignmentinfo;
        setalloc,
        packenum,
        packrecords : shortint;
        asmmode : tasmmode;
        optimizerswitches : toptimizerswitches;
      end;

    type
      tswitchesstatestack = array[0..switchesstatestackmax] of tsavedswitchesstate;

    var
      switchesstatestack:tswitchesstatestack;
      switchesstatestackpos: Integer;

    type

      { TScanDir }

      TScanDir = class
      private
        FCompiler: TCompilerBase;

        procedure do_delphiswitch(sw: char);
        procedure do_localswitch(sw: tlocalswitch);
        function do_localswitchdefault(sw: tlocalswitch): char;
        procedure do_message(w: integer);
        procedure do_moduleflagswitch(flag: tmoduleflag; optional: boolean);
        procedure do_moduleswitch(sw: tmoduleswitch);
        procedure do_setverbose(flag: char);
        procedure do_version(out major, minor, revision: word; out verstr: string; allowrevision: boolean; out isset: boolean);
        function get_peflag_const(const ident: string; error: longint): longint;

        procedure dir_align;
        procedure dir_a1;
        procedure dir_a2;
        procedure dir_a4;
        procedure dir_a8;
        procedure dir_asmcpu;
        procedure dir_asmmode;
{$if defined(m68k) or defined(arm)}
        procedure dir_appid;
        procedure dir_appname;
{$endif defined(m68k) or defined(arm)}
        procedure dir_apptype;
        procedure dir_bitpacking;
        procedure dir_calling;
        procedure dir_checklowaddrloads;
        procedure dir_checkpointer;
        procedure dir_codealign;
        procedure dir_codepage;
        procedure dir_coperators;
        procedure dir_endregion;
        procedure dir_excessprecision;
        procedure dir_checkcasecoverage;
        procedure dir_checkfpuexceptions;
        procedure dir_externalsym;
        procedure dir_hppemit;
        procedure dir_hugecode;
        procedure dir_hugepointerarithmeticnormalization;
        procedure dir_hugepointercomparisonnormalization;
        procedure dir_hugepointernormalization;
        procedure dir_nodefine;
        procedure dir_objectchecks;
        procedure dir_ieeeerrors;
        procedure dir_assertions;
        procedure dir_booleval;
        procedure dir_debuginfo;
        procedure dir_definitioninfo;
        procedure dir_denypackageunit;
        procedure dir_description;
        procedure dir_referenceinfo;
        procedure dir_region;
        procedure dir_resource;
        procedure dir_rtti;
        procedure dir_safefpuexceptions;
        procedure dir_saturation;
        procedure dir_scopedenums;
        procedure dir_screenname;
        procedure dir_setpeflags;
        procedure dir_setpeoptflags;
        procedure dir_setpeosversion;
        procedure dir_setpesubsysversion;
        procedure dir_setpeuserversion;
        procedure dir_smartlink;
        procedure dir_stackframes;
        procedure dir_stop;
        procedure dir_stringchecks;
        procedure dir_syscall;
        procedure dir_targetswitch;
        procedure dir_threadname;
        procedure dir_copyright;
        procedure dir_error;
        procedure dir_extendedsyntax;
        procedure dir_forcefarcalls;
        procedure dir_fatal;
        procedure dir_floatingpointemulation;
        procedure dir_stackchecking;
        procedure dir_fputype;
        procedure dir_frameworkpath;
        procedure dir_goto;
        procedure dir_hint;
        procedure dir_hints;
        procedure dir_imagebase;
        procedure dir_implicitexceptions;
        procedure dir_importeddata;
        procedure dir_includepath;
        procedure dir_info;
        procedure dir_inline;
        procedure dir_interfaces;
        procedure dir_iochecks;
        procedure dir_libexport;
        procedure dir_librarypath;
        procedure dir_link;
        procedure dir_linkframework;
        procedure dir_linklib;
        procedure dir_localsymbols;
        procedure dir_longstrings;
        procedure dir_macro;
        procedure dir_pascalmainname;
        procedure dir_maxfpuregisters;
        procedure dir_maxstacksize;
        procedure dir_memory;
        procedure dir_message;
        procedure dir_minstacksize;
        procedure dir_mode;
        procedure dir_multilinestringlineending;
        procedure dir_textblock;
        procedure dir_multilinestringtrimleft;
        procedure dir_modeswitch;
        procedure dir_namespaces;
        procedure dir_namespace;
        procedure dir_legacyifend;
        procedure dir_mmx;
        procedure dir_note;
        procedure dir_notes;
        procedure dir_objectpath;
        procedure dir_openstrings;
        procedure dir_optimization;
        procedure dir_overflowchecks;
        procedure dir_packenum;
        procedure dir_minfpconstprec;
        procedure dir_packrecords;
        procedure dir_packset;
        procedure dir_pic;
        procedure dir_pop;
        procedure dir_pointermath;
        procedure dir_profile;
        procedure dir_push;
        procedure dir_rangechecks;
        procedure dir_typedaddress;
        procedure dir_typeinfo;
        procedure dir_unitpath;
        procedure dir_varparacopyoutcheck;
        procedure dir_varpropsetter;
        procedure dir_varstringchecks;
        procedure dir_version;
        procedure dir_wait;
        procedure dir_warn;
        procedure dir_warning;
        procedure dir_warnings;
        procedure dir_weakpackageunit;
        procedure dir_writeableconst;
        procedure dir_yd;
        procedure dir_z1;
        procedure dir_z2;
        procedure dir_z4;
        procedure dir_zerobasesstrings;

        property Compiler: TCompilerBase read FCompiler;
      public
        constructor Create(ACompiler: TCompilerBase);
        procedure InitScannerDirectives(AScanner: TScanner);
      end;

  implementation

    uses
      SysUtils,
      cutils,cfileutl,
      globals,widestr,cpuinfo,tokens,compiler,
      verbose,comphook,ppu,
      switches,
      fmodule,
      defutil,
      dirparse,link,
      syscinfo,
      symconst,symtable,symbase,symtype,symsym,symdef,
      rabase;

{*****************************************************************************
                                    Helpers
*****************************************************************************}

    procedure TScanDir.do_delphiswitch(sw:char);
      var
        state : char;
      begin
      { c contains the next char, a + or - would be fine }
        state:=current_scanner.readstate;
        if state in ['-','+'] then
          HandleSwitch(sw,state);
      end;


    procedure TScanDir.do_setverbose(flag:char);
      var
        state : char;
      begin
      { support ON/OFF }
        state:=current_scanner.ReadState;
        recordpendingverbosityswitch(flag,state);
      end;


    procedure TScanDir.do_moduleswitch(sw:tmoduleswitch);
      var
        state : char;
      begin
        state:=current_scanner.readstate;
        if (sw<>cs_modulenone) and (state in ['-','+']) then
         begin
           if state='-' then
            compiler.globals.current_settings.moduleswitches:=compiler.globals.current_settings.moduleswitches-[sw]
           else
            compiler.globals.current_settings.moduleswitches:=compiler.globals.current_settings.moduleswitches+[sw];
         end;
      end;


    procedure TScanDir.do_localswitch(sw:tlocalswitch);
      var
        state : char;
      begin
        state:=current_scanner.readstate;
        if (sw<>cs_localnone) and (state in ['-','+']) then
          recordpendinglocalswitch(sw,state);
      end;

    function TScanDir.do_localswitchdefault(sw:tlocalswitch): char;
      begin
        result:=current_scanner.readstatedefault;
        if (sw<>cs_localnone) and (result in ['-','+','*']) then
          recordpendinglocalswitch(sw,result);
      end;


    procedure TScanDir.do_moduleflagswitch(flag:tmoduleflag;optional:boolean);
      var
        state : char;
      begin
        if optional then
          state:=current_scanner.readoptionalstate('+')
        else
          state:=current_scanner.readstate;
        if state='-' then
          exclude(compiler.current_module.moduleflags,flag)
        else
          include(compiler.current_module.moduleflags,flag);
      end;


    procedure TScanDir.do_message(w:integer);
      begin
        current_scanner.skipspace;
        compiler.verbose.Message1(w,current_scanner.readlongcomment);
      end;


    procedure TScanDir.do_version(out major, minor, revision: word; out verstr: string; allowrevision: boolean; out isset: boolean);
      var
        majorl,
        minorl,
        revisionl,
        error : longint;
      begin
        { change description global var in all cases }
        { it not used but in win32, os2 and netware }
        current_scanner.skipspace;
        { we should only accept Major.Minor format for win32 and os2 }
        current_scanner.readnumber;
        major:=0;
        minor:=0;
        revision:=0;
        verstr:='';
        isset:=false;
        majorl:=0;
        minorl:=0;
        revisionl:=0;
        val(current_scanner.pattern,majorl,error);
        if (error<>0) or (majorl > high(word)) or (majorl < 0) then
          begin
            compiler.verbose.Message1(scan_w_wrong_version_ignored,current_scanner.pattern);
            exit;
          end;
        isset:=true;
        if current_scanner.c='.' then
          begin
            current_scanner.readchar;
            current_scanner.readnumber;
            val(current_scanner.pattern,minorl,error);
            if (error<>0) or (minorl > high(word)) or (minorl < 0) then
              begin
                compiler.verbose.Message1(scan_w_wrong_version_ignored,tostr(majorl)+'.'+current_scanner.pattern);
                exit;
              end;
            if (current_scanner.c='.') and
               allowrevision then
              begin
                 current_scanner.readchar;
                 current_scanner.readnumber;
                 val(current_scanner.pattern,revisionl,error);
                 if (error<>0) or (revisionl > high(word)) or (revisionl < 0) then
                   begin
                      compiler.verbose.Message1(scan_w_wrong_version_ignored,tostr(majorl)+'.'+tostr(minorl)+'.'+current_scanner.pattern);
                      exit;
                   end;
                 major:=word(majorl);
                 minor:=word(minorl);
                 revision:=word(revisionl);
                 verstr:=tostr(major)+','+tostr(minor)+','+tostr(revision);
              end
            else
              begin
                 major:=word(majorl);
                 minor:=word(minorl);
                 verstr:=tostr(major)+'.'+tostr(minor);
              end;
          end
        else
          begin
            major:=word(majorl);
            verstr:=tostr(major);
          end;
      end;


{*****************************************************************************
                              Directive Callbacks
*****************************************************************************}

    procedure TScanDir.dir_align;
      var
        hs : string;
        b : longint;
      begin
        current_scanner.skipspace;
        if not(current_scanner.c in ['0'..'9']) then
         begin
           { Support also the ON and OFF as switch }
           hs:=current_scanner.readid;
           if (hs='ON') then
            compiler.globals.current_settings.packrecords:=4
           else if (hs='OFF') then
             compiler.globals.current_settings.packrecords:=1
           else if m_mac in compiler.globals.current_settings.modeswitches then
             begin
               { Support switches used in Apples Universal Interfaces}
               if (hs='MAC68K') then
                 compiler.globals.current_settings.packrecords:=mac68k_alignment
               { "power" alignment is the default C packrecords setting on
                 Mac OS X }
               else if (hs='POWER') or (hs='POWERPC') then
                 compiler.globals.current_settings.packrecords:=C_alignment
               else if (hs='RESET') then
                 compiler.globals.current_settings.packrecords:=default_settings.packrecords
               else
                 compiler.verbose.Message1(scan_e_illegal_pack_records,hs);
             end
           else
             compiler.verbose.Message1(scan_e_illegal_pack_records,hs);
         end
        else
         begin
           b:=current_scanner.readval;
           case b of
             1,2,4,8,16,32 : compiler.globals.current_settings.packrecords:=b;
           else
            compiler.verbose.Message1(scan_e_illegal_pack_records,tostr(b));
           end;
         end;
      end;

    procedure TScanDir.dir_a1;
      begin
        compiler.globals.current_settings.packrecords:=1;
      end;

    procedure TScanDir.dir_a2;
      begin
        compiler.globals.current_settings.packrecords:=2;
      end;

    procedure TScanDir.dir_a4;
      begin
        compiler.globals.current_settings.packrecords:=4;
      end;

    procedure TScanDir.dir_a8;
      begin
        compiler.globals.current_settings.packrecords:=8;
      end;

    procedure TScanDir.dir_asmcpu;
      var
        s : string;
        cpu: tcputype;
        found: Boolean;
      begin
        current_scanner.skipspace;
        s:=current_scanner.readid;
        If compiler.globals.Inside_asm_statement then
          compiler.verbose.Message1(scan_w_no_asm_reader_switch_inside_asm,s);
        if s='ANY' then
          compiler.globals.current_settings.asmcputype:=cpu_none
        else if s='CURRENT' then
          compiler.globals.current_settings.asmcputype:=compiler.globals.current_settings.cputype
        else
          begin
            found:=false;
            for cpu:=succ(low(tcputype)) to high(tcputype) do
              if s=cputypestr[cpu] then
                begin
                  found:=true;
                  compiler.globals.current_settings.asmcputype:=cpu;
                  break;
                end;
            if not found then
              compiler.verbose.Message1(scan_e_illegal_asmcpu_specifier,s);
          end;
      end;

    procedure TScanDir.dir_asmmode;
      var
        s : string;
        asmmode: tasmmode;
      begin
        current_scanner.skipspace;
        s:=current_scanner.readid;
        If compiler.globals.Inside_asm_statement then
          compiler.verbose.Message1(scan_w_no_asm_reader_switch_inside_asm,s);
        if s='DEFAULT' then
          recordpendingasmmode(compiler.globals.init_settings.asmmode)
        else
         if not SetAsmReadMode(s,asmmode) then
           compiler.verbose.Message1(scan_e_illegal_asmmode_specifier,s)
         else
           recordpendingasmmode(asmmode);
      end;

{$if defined(m68k) or defined(arm)}
    procedure TScanDir.dir_appid;
      begin
        if compiler.target.info.system<>system_m68k_palmos then
          compiler.verbose.Message(scan_w_appid_not_support);
        { change description global var in all cases }
        { it not used but in win32 and os2 }
        current_scanner.skipspace;
        compiler.globals.palmos_applicationid:=current_scanner.readcomment;
      end;

    procedure TScanDir.dir_appname;
      begin
        if compiler.target.info.system<>system_m68k_palmos then
          compiler.verbose.Message(scan_w_appname_not_support);
        { change description global var in all cases }
        { it not used but in win32 and os2 }
        current_scanner.skipspace;
        compiler.globals.palmos_applicationname:=current_scanner.readcomment;
      end;
{$endif defined(m68k) or defined(arm)}

    procedure TScanDir.dir_apptype;
      var
         hs : string;
      begin
        if not (compiler.target.info.system in systems_all_windows + [system_i386_os2,
                                       system_i386_emx, system_powerpc_macosclassic,
                                       system_arm_nds, system_i8086_msdos,
                                       system_i8086_embedded, system_m68k_atari] +
                                       systems_nativent) then
          begin
            if m_delphi in compiler.globals.current_settings.modeswitches then
              compiler.verbose.Message(scan_n_app_type_not_support)
            else
              compiler.verbose.Message(scan_w_app_type_not_support);
          end
        else
          begin
            if not compiler.current_module.in_global then
              compiler.verbose.Message(scan_w_switch_is_global)
            else
              begin
                 current_scanner.skipspace;
                 hs:=current_scanner.readid;
                 if (hs='GUI') and not (compiler.target.info.system in [system_i8086_msdos,system_i8086_embedded]) then
                   SetApptype(app_gui)
                 else if (hs='CONSOLE') and not (compiler.target.info.system in [system_i8086_msdos,system_i8086_embedded]) then
                   SetApptype(app_cui)
                 else if (hs='NATIVE') and (compiler.target.info.system in systems_windows + systems_nativent) then
                   SetApptype(app_native)
                 else if (hs='FS') and (compiler.target.info.system in [system_i386_os2,
                                                             system_i386_emx]) then
                   SetApptype(app_fs)
                 else if (hs='TOOL') and (compiler.target.info.system in [system_powerpc_macosclassic]) then
                   SetApptype(app_tool)
                 else if (hs='ARM9') and (compiler.target.info.system in [system_arm_nds]) then
                   SetApptype(app_arm9)
                 else if (hs='ARM7') and (compiler.target.info.system in [system_arm_nds]) then
                   SetApptype(app_arm7)
                 else if (hs='COM') and (compiler.target.info.system in [system_i8086_msdos,system_i8086_embedded]) then
                   SetApptype(app_com)
                 else if (hs='EXE') and (compiler.target.info.system in [system_i8086_msdos,system_i8086_embedded]) then
                   SetApptype(app_cui)
                 else
                   compiler.verbose.Message1(scan_w_unsupported_app_type,hs);
              end;
          end;
      end;


    procedure TScanDir.dir_calling;
      var
         hs : string;
      begin
        current_scanner.skipspace;
        hs:=current_scanner.readid;
        if (hs='') then
          compiler.verbose.Message(parser_e_proc_directive_expected)
        else
          recordpendingcallingswitch(hs);
      end;


    procedure TScanDir.dir_checklowaddrloads;
      begin
        do_localswitchdefault(cs_check_low_addr_load);
      end;


    procedure TScanDir.dir_checkpointer;
      var
        switch: char;
      begin
        switch:=do_localswitchdefault(cs_checkpointer);
        if (switch='+') and
           not(compiler.target.info.system in systems_support_checkpointer) then
          compiler.verbose.Message1(scan_e_unsupported_switch,'CHECKPOINTER+');
      end;


    procedure TScanDir.dir_excessprecision;
      begin
        do_localswitch(cs_excessprecision);
      end;


    procedure TScanDir.dir_checkcasecoverage;
      begin
        do_localswitch(cs_check_all_case_coverage);
      end;


    procedure TScanDir.dir_checkfpuexceptions;
      begin
        do_localswitch(cs_check_fpu_exceptions);
      end;


    procedure TScanDir.dir_objectchecks;
      begin
        do_localswitch(cs_check_object);
      end;


    procedure TScanDir.dir_ieeeerrors;
      begin
        do_localswitch(cs_ieee_errors);
      end;


    procedure TScanDir.dir_assertions;
      begin
        do_delphiswitch('C');
      end;


    procedure TScanDir.dir_booleval;
      begin
        do_delphiswitch('B');
      end;

    procedure TScanDir.dir_debuginfo;
      begin
        do_delphiswitch('D');
      end;

    procedure TScanDir.dir_definitioninfo;
      begin
        do_delphiswitch('Y');
      end;

    procedure TScanDir.dir_denypackageunit;
      begin
        do_moduleflagswitch(mf_package_deny,true);
      end;

    procedure TScanDir.dir_description;
      begin
        if not (compiler.target.info.system in systems_all_windows+[system_i386_os2,system_i386_emx,
                 system_i386_netware,system_i386_wdosx,system_i386_netwlibc,system_i8086_win16]) then
          compiler.verbose.Message(scan_w_description_not_support);
        { change description global var in all cases }
        { it not used but in win32, os2 and netware }
        current_scanner.skipspace;
        compiler.globals.description:=current_scanner.readcomment;
        compiler.globals.DescriptionSetExplicity:=true;
      end;

    procedure TScanDir.dir_screenname; {ad}
      begin
        if not (compiler.target.info.system in [system_i386_netware,system_i386_netwlibc]) then
          {compiler.verbose.Message(scan_w_description_not_support);}
          compiler.verbose.comment (V_Warning,'Screenname only supported for target netware');
        current_scanner.skipspace;
        compiler.globals.nwscreenname:=current_scanner.readcomment;
      end;

      procedure TScanDir.dir_threadname; {ad}
      begin
        if not (compiler.target.info.system in [system_i386_netware,system_i386_netwlibc]) then
          {compiler.verbose.Message(scan_w_description_not_support);}
          compiler.verbose.comment (V_Warning,'Threadname only supported for target netware');
        current_scanner.skipspace;
        compiler.globals.nwthreadname:=current_scanner.readcomment;
      end;

      procedure TScanDir.dir_copyright; {ad}
      begin
        if not (compiler.target.info.system in [system_i386_netware,system_i386_netwlibc]) then
          {compiler.verbose.Message(scan_w_description_not_support);}
          compiler.verbose.comment (V_Warning,'Copyright only supported for target netware');
        current_scanner.skipspace;
        compiler.globals.nwcopyright:=current_scanner.readcomment;
      end;

    procedure TScanDir.dir_error;
      begin
        do_message(scan_e_user_defined);
      end;

    procedure TScanDir.dir_extendedsyntax;
      begin
        do_delphiswitch('X');
      end;

    procedure TScanDir.dir_forcefarcalls;
      begin
        if not (compiler.target.info.system in [system_i8086_msdos,system_i8086_embedded])
{$ifdef i8086}
           or (compiler.globals.current_settings.x86memorymodel in x86_near_code_models)
{$endif i8086}
            then
          begin
            compiler.verbose.Message1(scan_n_ignored_switch,current_scanner.pattern);
            exit;
          end;
        do_localswitch(cs_force_far_calls);
      end;

    procedure TScanDir.dir_fatal;
      begin
        do_message(scan_f_user_defined);
      end;

    procedure TScanDir.dir_floatingpointemulation;
      begin
        do_delphiswitch('E');
      end;

    procedure TScanDir.dir_stackchecking;
      begin
        do_delphiswitch('S');
      end;

    procedure TScanDir.dir_fputype;
      var
        fputype: tfputype;
      begin
        current_scanner.skipspace;
        undef_system_macro('FPU'+fputypestr[compiler.globals.current_settings.fputype]);
        fputype:=compiler.globals.current_settings.fputype;
        if SetFPUType(upper(current_scanner.readcomment),fputype) then
          compiler.globals.current_settings.fputype:=fputype
        else
          compiler.verbose.comment(V_Error,'Illegal FPU type');
        def_system_macro('FPU'+fputypestr[compiler.globals.current_settings.fputype]);
     end;

    procedure TScanDir.dir_frameworkpath;
      begin
        if not compiler.current_module.in_global then
         compiler.verbose.Message(scan_w_switch_is_global)
        else if not(compiler.target.info.system in systems_darwin) then
          begin
            compiler.verbose.Message(scan_w_frameworks_darwin_only);
            current_scanner.skipspace;
            current_scanner.readcomment
          end
        else
          begin
            current_scanner.skipspace;
            compiler.current_module.localframeworksearchpath.AddPath(current_scanner.readcomment,false);
          end;
      end;

    procedure TScanDir.dir_goto;
      begin
        do_moduleswitch(cs_support_goto);
      end;

    procedure TScanDir.dir_hint;
      begin
        do_message(scan_h_user_defined);
      end;

    procedure TScanDir.dir_hints;
      begin
        do_setverbose('H');
      end;

    procedure TScanDir.dir_imagebase;
      begin
        if not (compiler.target.info.system in (systems_windows+systems_wince)) then
          compiler.verbose.Message(scan_w_imagebase_not_support);
        current_scanner.skipspace;
        compiler.globals.imagebase:=current_scanner.readval;
        compiler.globals.ImageBaseSetExplicity:=true
      end;

    procedure TScanDir.dir_implicitexceptions;
      begin
        do_moduleswitch(cs_implicit_exceptions);
      end;

    procedure TScanDir.dir_importeddata;
      begin
        do_delphiswitch('G');
      end;

    procedure TScanDir.dir_includepath;
      var
        path : string;
      begin
        if not compiler.current_module.in_global then
         compiler.verbose.Message(scan_w_switch_is_global)
        else
          begin
            current_scanner.skipspace;
            path:=current_scanner.readcomment;
            compiler.current_module.localincludesearchpath.AddPath(path,false);
            compiler.verbose.Message2(general_t_includepath_local,compiler.current_module.realmodulename^,path);
          end;
      end;

    procedure TScanDir.dir_info;
      begin
        do_message(scan_i_user_defined);
      end;

    procedure TScanDir.dir_inline;
      begin
        do_localswitch(cs_do_inline);
      end;

    procedure TScanDir.dir_interfaces;
      var
        hs : string;
      begin
        {corba/com/default}
        current_scanner.skipspace;
        hs:=current_scanner.readid;
{$ifndef jvm}
        if (hs='CORBA') then
          compiler.globals.current_settings.interfacetype:=it_interfacecorba
        else if (hs='COM') then
          compiler.globals.current_settings.interfacetype:=it_interfacecom
        else
{$endif jvm}
             if (hs='DEFAULT') then
          compiler.globals.current_settings.interfacetype:=compiler.globals.init_settings.interfacetype
        else
          compiler.verbose.Message(scan_e_invalid_interface_type);
      end;

    procedure TScanDir.dir_iochecks;
      begin
        do_delphiswitch('I');
      end;

    procedure TScanDir.dir_libexport;
      begin
        {not implemented}
      end;

    procedure TScanDir.dir_librarypath;
      var
        path : string;
      begin
        if not compiler.current_module.in_global then
         compiler.verbose.Message(scan_w_switch_is_global)
        else
          begin
            current_scanner.skipspace;
            path:=current_scanner.readcomment;
            compiler.current_module.locallibrarysearchpath.AddPath(path,false);
            compiler.verbose.Message2(general_t_librarypath_local,compiler.current_module.realmodulename^,path);
          end;
      end;

    procedure TScanDir.dir_link;
      var
        s : string;
      begin
        current_scanner.skipspace;
        if current_scanner.c = '''' then
          begin
            s:= current_scanner.readquotedstring;
            current_scanner.readcomment
          end
        else
          s:= trimspace(current_scanner.readcomment);
        s:=FixFileName(s);
        if ExtractFileExt(s)='' then
          s:=ChangeFileExt(s,compiler.target.info.objext);
        compiler.current_module.linkotherofiles.add(s,link_always);
      end;

    procedure TScanDir.dir_linkframework;
      var
        s : string;
      begin
        current_scanner.skipspace;
        if current_scanner.c = '''' then
          begin
            s:= current_scanner.readquotedstring;
            current_scanner.readcomment
          end
        else
          s:= trimspace(current_scanner.readcomment);
        s:=FixFileName(s);
        if (compiler.target.info.system in systems_darwin) then
          compiler.current_module.linkotherframeworks.add(s,link_always)
        else
          compiler.verbose.Message(scan_w_frameworks_darwin_only);
      end;

    procedure TScanDir.dir_linklib;
      type
        tLinkMode=(lm_shared,lm_static);
      var
        s : string;
        quote : char;
        libext,
        libname,
        linkmodestr : string;
        p : longint;
        linkMode : tLinkMode;
      begin
        current_scanner.skipspace;
        if current_scanner.c = '''' then
          begin
            libname:= current_scanner.readquotedstring;
            s:= current_scanner.readcomment;
            p:=pos(',',s);
          end
        else
          begin
            s:= current_scanner.readcomment;
            p:=pos(',',s);
            if p=0 then
              libname:=TrimSpace(s)
            else
              libname:=TrimSpace(copy(s,1,p-1));
          end;
        if p=0 then
          linkmodeStr:=''
        else
          linkmodeStr:=Upper(TrimSpace(copy(s,p+1,255)));


        if (libname='') or (libname='''''') or (libname='""') then
         exit;
        { create library name }
        if libname[1] in ['''','"'] then
         begin
           quote:=libname[1];
           Delete(libname,1,1);
           p:=pos(quote,libname);
           if p>0 then
            Delete(libname,p,1);
         end;
        libname:=FixFileName(libname);

        { get linkmode, default is to check the extension for
          the static library, otherwise shared linking is assumed }
        linkmode:=lm_shared;
        if linkModeStr='' then
         begin
           libext:=ExtractFileExt(libname);
           if libext=compiler.target.info.staticClibext then
             linkMode:=lm_static;
         end
        else if linkModeStr='STATIC' then
         linkmode:=lm_static
        else if (LinkModeStr='SHARED') or (LinkModeStr='') then
         linkmode:=lm_shared
        else
         compiler.verbose.Comment(V_Error,'Wrong link mode specified: "'+Linkmodestr+'"');

        { add to the list of other libraries }
        if linkMode=lm_static then
         compiler.current_module.linkOtherStaticLibs.add(libname,link_always)
        else
         compiler.current_module.linkOtherSharedLibs.add(libname,link_always);
      end;

    procedure TScanDir.dir_localsymbols;
      begin
        do_delphiswitch('L');
      end;

    procedure TScanDir.dir_longstrings;
      begin
        do_delphiswitch('H');
      end;

    procedure TScanDir.dir_macro;
      begin
        do_moduleswitch(cs_support_macro);
      end;

    procedure TScanDir.dir_pascalmainname;
      var
        s: string;
      begin
        current_scanner.skipspace;
        s:=trimspace(current_scanner.readcomment);
        if assigned(compiler.current_module.mainname) and
           (s<>compiler.current_module.mainname^) then
          begin
            compiler.verbose.Message1(scan_w_multiple_main_name_overrides,compiler.current_module.mainname^);
            stringdispose(compiler.current_module.mainname)
          end
        else if (compiler.globals.mainaliasname<>defaultmainaliasname) and
                (compiler.globals.mainaliasname<>s) then
          compiler.verbose.Message1(scan_w_multiple_main_name_overrides,compiler.globals.mainaliasname);
        compiler.globals.mainaliasname:=s;
        if (compiler.globals.mainaliasname<>defaultmainaliasname) then
          compiler.current_module.mainname:=stringdup(compiler.globals.mainaliasname);
      end;

    procedure TScanDir.dir_maxfpuregisters;
      var
         l  : integer;
         hs : string;
      begin
         current_scanner.skipspace;
         if not(current_scanner.c in ['0'..'9']) then
           begin
              hs:=current_scanner.readid;
              if (hs='NORMAL') or (hs='DEFAULT') then
                compiler.globals.current_settings.maxfpuregisters:=-1
              else
                compiler.verbose.Message(scan_e_invalid_maxfpureg_value);
           end
         else
           begin
              l:=current_scanner.readval;
              case l of
                 0..8:
                   compiler.globals.current_settings.maxfpuregisters:=l;
                 else
                   compiler.verbose.Message(scan_e_invalid_maxfpureg_value);
              end;
           end;
      end;

    procedure TScanDir.dir_maxstacksize;
      begin
        if not (compiler.target.info.system in (systems_windows+systems_wince)) then
          compiler.verbose.Message(scan_w_maxstacksize_not_support);
        current_scanner.skipspace;
        compiler.globals.maxstacksize:=current_scanner.readval;
        compiler.globals.MaxStackSizeSetExplicity:=true;
      end;

    procedure TScanDir.dir_memory;
      var
        l : int64;
        heapsize_limit: int64;
        maxheapsize_limit: int64;
      begin
{$if defined(i8086)}
        if compiler.target.info.system=system_i8086_win16 then
          begin
            heapsize_limit:=65520;
            maxheapsize_limit:=65520;
          end
        else if compiler.globals.current_settings.x86memorymodel in x86_far_data_models then
          begin
            heapsize_limit:=655360;
            maxheapsize_limit:=655360;
          end
        else
          begin
            heapsize_limit:=65520;
            maxheapsize_limit:=65520;
          end;
{$elseif defined(WASM32)}
        heapsize_limit:=int64(high(uint32))+1;
        maxheapsize_limit:=int64(high(uint32))+1;
{$elseif defined(cpu16bitaddr)}
        heapsize_limit:=65520;
        maxheapsize_limit:=65520;
{$else}
        heapsize_limit:=high(longint);
        maxheapsize_limit:=high(longint);
{$endif}
        current_scanner.skipspace;
        l:=current_scanner.readval;
        if (l>=1024)
{$ifdef cpu16bitaddr}
          and (l<=65521) { TP7's $M directive allows specifying a stack size of
                           65521, but it actually sets the stack size to 65520 }
{$else cpu16bitaddr}
          and (l<67107840)
{$endif cpu16bitaddr}
        then
          compiler.globals.stacksize:=min(l,{$ifdef cpu16bitaddr}65520{$else}67107839{$endif})
        else
          compiler.verbose.Message(scan_w_invalid_stacksize);
        if current_scanner.c=',' then
          begin
            current_scanner.readchar;
            current_scanner.skipspace;
            l:=current_scanner.readval64;
            if l>=1024 then
              compiler.globals.heapsize:=min(l,heapsize_limit);
            if current_scanner.c=',' then
              begin
                current_scanner.readchar;
                current_scanner.skipspace;
                l:=current_scanner.readval64;
                if l>=compiler.globals.heapsize then
                  compiler.globals.maxheapsize:=min(l,maxheapsize_limit)
                else
                  compiler.verbose.Message(scan_w_heapmax_lessthan_heapmin);
              end;
          end;
      end;


    procedure TScanDir.dir_message;
      var
        hs : string;
        s  : AnsiString;
        w  : longint;
      begin
        w:=0;
        current_scanner.skipspace;
        { Message level specified? }
        if current_scanner.c='''' then
          w:=scan_n_user_defined
        else
          begin
            hs:=current_scanner.readid;
            if (hs='WARN') or (hs='WARNING') then
              w:=scan_w_user_defined
            else
              if (hs='ERROR') then
                w:=scan_e_user_defined
            else
              if (hs='FATAL') then
                w:=scan_f_user_defined
            else
              if (hs='HINT') then
                w:=scan_h_user_defined
            else
              if (hs='NOTE') then
                w:=scan_n_user_defined
            else
              if (hs='INFO') then
                w:=scan_i_user_defined
            else
              compiler.verbose.Message1(scan_w_illegal_directive,hs);
          end;
        { Only print message when there was no error }
        if w<>0 then
          begin
            current_scanner.skipspace;
            if current_scanner.c='''' then
              s:=current_scanner.readlongquotedstring
            else
              s:=current_scanner.readlongcomment;
            compiler.verbose.Message1(w,s);
          end
        else
          current_scanner.readcomment;
      end;


    procedure TScanDir.dir_minstacksize;
      begin
        if not (compiler.target.info.system in (systems_windows+systems_wince)) then
          compiler.verbose.Message(scan_w_minstacksize_not_support);
        current_scanner.skipspace;
        compiler.globals.minstacksize:=current_scanner.readval;
        compiler.globals.MinStackSizeSetExplicity:=true;
      end;


    procedure TScanDir.dir_mode;

    begin
      if not compiler.current_module.in_global then
        compiler.verbose.Message(scan_w_switch_is_global)
      else
        begin
          current_scanner.skipspace;
          current_scanner.readstring;
          if not compiler.current_module.mode_switch_allowed and
              not ((m_mac in compiler.globals.current_settings.modeswitches) and (current_scanner.pattern='MACPAS')) then
            compiler.verbose.Message1(scan_e_mode_switch_not_allowed,current_scanner.pattern)
          else if not SetCompileMode(current_scanner.pattern,false) then
            compiler.verbose.Message1(scan_w_illegal_switch,current_scanner.pattern)
        end;
      compiler.current_module.mode_switch_allowed:= false;
    end;

    procedure TScanDir.dir_multilinestringlineending;
      var
        s : string;
      begin
        if not (m_multiline_strings in compiler.globals.current_settings.modeswitches) then
          compiler.verbose.Message1(scan_e_illegal_directive,'MULTILINESTRINGLINEENDING');
        current_scanner.skipspace;
        s:=current_scanner.readid;
        if (s='CR') then
          compiler.globals.current_settings.lineendingtype:=le_cr
        else if (s='CRLF') then
          compiler.globals.current_settings.lineendingtype:=le_crlf
        else if (s='LF') then
          compiler.globals.current_settings.lineendingtype:=le_lf
        else if (s='PLATFORM') then
          compiler.globals.current_settings.lineendingtype:=le_platform
        else if (s='SOURCE') then
          compiler.globals.current_settings.lineendingtype:=le_source
        else
          compiler.verbose.Message(scan_e_unknown_lineending_type);
      end;

    procedure TScanDir.dir_textblock;
      var
        s : string;
      begin
        if not (m_delphi in compiler.globals.current_settings.modeswitches) then
          compiler.verbose.Message1(scan_e_illegal_directive,'TEXTBLOCK');
        current_scanner.skipspace;
        s:=current_scanner.readid;
        if (s='CR') then
          compiler.globals.current_settings.lineendingtype:=le_cr
        else if (s='CRLF') then
          compiler.globals.current_settings.lineendingtype:=le_crlf
        else if (s='LF') then
          compiler.globals.current_settings.lineendingtype:=le_lf
        else if (s='NATIVE') then
          compiler.globals.current_settings.lineendingtype:=le_platform
        else
          compiler.verbose.Message(scan_e_unknown_lineending_type);
      end;

    procedure TScanDir.dir_multilinestringtrimleft;
      var
        count : longint;
        s : string;
      begin
        if not (m_multiline_strings in compiler.globals.current_settings.modeswitches) then
          compiler.verbose.Message1(scan_e_illegal_directive,'MULTILINESTRINGTRIMLEFT');
        current_scanner.skipspace;
        if (current_scanner.c in ['0'..'9']) then
          begin
            count:=current_scanner.readval;
            if (count<0) or (count>high(word)) then
              compiler.verbose.Message(scan_e_trimcount_out_of_range)
            else
              begin
                compiler.globals.current_settings.whitespacetrimcount:=count;
                compiler.globals.current_settings.whitespacetrimauto:=false;
              end;
          end
        else
          begin
            s:=current_scanner.readid;
            if s='ALL' then
              begin
                compiler.globals.current_settings.whitespacetrimcount:=high(word);
                compiler.globals.current_settings.whitespacetrimauto:=false;
              end
            else if s='AUTO' then
              begin
                compiler.globals.current_settings.whitespacetrimcount:=0;
                compiler.globals.current_settings.whitespacetrimauto:=true;
              end
            else
              begin
                compiler.globals.current_settings.whitespacetrimcount:=0;
                compiler.globals.current_settings.whitespacetrimauto:=false;
              end;
          end;
      end;

    procedure TScanDir.dir_modeswitch;
      var
        s : string;
      begin
        if not compiler.current_module.in_global then
          compiler.verbose.Message(scan_w_switch_is_global)
        else
          begin
            current_scanner.skipspace;
            current_scanner.readstring;
            s:=current_scanner.pattern;
            { don't combine the assignments to s as the method call will be
              done before "current_scanner.pattern" is assigned to s and the method changes
              "current_scanner.pattern" }
            s:=s+current_scanner.readoptionalstate('+');
            if not SetCompileModeSwitch(s,false) then
              compiler.verbose.Message1(scan_w_illegal_switch,s)
          end;
      end;


    procedure TScanDir.dir_namespaces;

    { add namespaces to the local namespace list }
      var
        s : string;

    begin
      if not compiler.current_module.in_global then
        compiler.verbose.Message(scan_w_switch_is_global)
      else
        begin
          current_scanner.skipspace;
          current_scanner.readstring;
          s:=current_scanner.orgpattern;
          While (s<>'') do
            begin
              // We may not yet have a correct module namespacelist.
              if assigned(compiler.globals.current_namespacelist) then
                compiler.globals.current_namespacelist.Insert(s)
              else // copied when correct module is activated
                compiler.globals.premodule_namespacelist.Insert(s);
              s:='';
              if current_scanner.c=',' then
                begin
                  current_scanner.readchar;
                  current_scanner.skipspace;
                  current_scanner.readstring;
                  s:=current_scanner.orgpattern;
                end;
            end;
        end;
    end;

    procedure TScanDir.dir_namespace;
      var
        s : string;
      begin
        { used to define Java package names for all types declared in the
          current unit }
        if not compiler.current_module.in_global then
          compiler.verbose.Message(scan_w_switch_is_global)
        else
          begin
            current_scanner.skipspace;
            current_scanner.readstring;
            s:=current_scanner.orgpattern;
            while current_scanner.c='.' do
              begin
                current_scanner.readchar;
                current_scanner.readstring;
                s:=s+'.'+current_scanner.orgpattern;
              end;
            disposestr(compiler.current_module.namespace);
            compiler.current_module.namespace:=stringdup(s);
          end;
      end;

    procedure TScanDir.dir_legacyifend;
      begin
        do_localswitch(cs_legacyifend);
      end;

    procedure TScanDir.dir_mmx;
      begin
        do_localswitch(cs_mmx);
      end;

    procedure TScanDir.dir_note;
      begin
        do_message(scan_n_user_defined);
      end;

    procedure TScanDir.dir_notes;
      begin
        do_setverbose('N');
      end;

    procedure TScanDir.dir_objectpath;
      var
        path : string;
      begin
        if not compiler.current_module.in_global then
         compiler.verbose.Message(scan_w_switch_is_global)
        else
          begin
            current_scanner.skipspace;
            path:=current_scanner.readcomment;
            compiler.current_module.localobjectsearchpath.AddPath(path,false);
            compiler.verbose.Message2(general_t_objectpath_local,compiler.current_module.realmodulename^,path);
          end;
      end;

    procedure TScanDir.dir_openstrings;
      begin
        do_delphiswitch('P');
      end;

    procedure TScanDir.dir_optimization;
      var
        hs : string;
        optimizerswitches: toptimizerswitches;
      begin
        current_scanner.skipspace;
        { Support also the ON and OFF as switch }
        hs:=current_scanner.readid;
        if (hs='ON') then
          recordpendingoptimizerswitches(level2optimizerswitches)
        else if (hs='OFF') then
          recordpendingoptimizerswitches([])
        else if (hs='DEFAULT') then
          recordpendingoptimizerswitches(compiler.globals.init_settings.optimizerswitches)
        else
          begin
            optimizerswitches:=compiler.globals.current_settings.optimizerswitches;
            if UpdateOptimizerStr(hs,optimizerswitches) then
              begin
                compiler.globals.current_settings.optimizerswitches:=optimizerswitches;
                recordpendingoptimizerswitches(compiler.globals.current_settings.optimizerswitches)
              end
            else
              compiler.verbose.Message1(scan_e_illegal_optimization_specifier,hs);
          end;
      end;

    procedure TScanDir.dir_overflowchecks;
      begin
        do_delphiswitch('Q');
      end;

    procedure TScanDir.dir_packenum;
      var
        hs : string;
        v : longint;
      begin
        current_scanner.skipspace;
        if not(current_scanner.c in ['0'..'9']) then
         begin
           hs:=current_scanner.readid;
           if (hs='NORMAL') or (hs='DEFAULT') then
            recordpendingpackenum(4)
           else
            compiler.verbose.Message1(scan_e_illegal_pack_enum, hs);
         end
        else
         begin
           v:=current_scanner.readval;
           case v of
            1,2,4 : recordpendingpackenum(v);
           else
            compiler.verbose.Message1(scan_e_illegal_pack_enum, current_scanner.pattern);
           end;
         end;
      end;


    procedure TScanDir.dir_minfpconstprec;
      var
        minfpconstprec: tfloattype;
      begin
        current_scanner.skipspace;
        minfpconstprec:=compiler.globals.current_settings.minfpconstprec;
        if SetMinFPConstPrec(current_scanner.readid,minfpconstprec) then
          compiler.globals.current_settings.minfpconstprec:=minfpconstprec
        else
          compiler.verbose.Message1(scan_e_illegal_minfpconstprec, current_scanner.pattern);
      end;


    procedure TScanDir.dir_packrecords;
      var
        hs : string;
        v : longint;
      begin
        { can't change packrecords setting on managed vm targets }
        if compiler.target.info.system in systems_managed_vm then
          compiler.verbose.Message1(scanner_w_directive_ignored_on_target, 'PACKRECORDS');
        current_scanner.skipspace;
        if not(current_scanner.c in ['0'..'9']) then
         begin
           hs:=current_scanner.readid;
           { C has the special recordalignmax of C_alignment }
           if (hs='C') then
            recordpendingpackrecords(C_alignment)
           else
            if (hs='NORMAL') or (hs='DEFAULT') then
             recordpendingpackrecords(default_settings.packrecords)
           else
            compiler.verbose.Message1(scan_e_illegal_pack_records,hs);
         end
        else
         begin
           v:=current_scanner.readval;
           case v of
             1,2,4,8,16,32 : recordpendingpackrecords(v);
           else
            compiler.verbose.Message1(scan_e_illegal_pack_records,current_scanner.pattern);
           end;
         end;
      end;


    procedure TScanDir.dir_packset;
      var
        hs : string;
        v : longint;
      begin
        current_scanner.skipspace;
        if not(current_scanner.c in ['1','2','4','8']) then
         begin
           hs:=current_scanner.readid;
           if (hs='FIXED') or (hs='DEFAULT') OR (hs='NORMAL') then
            recordpendingsetalloc(0) {Fixed mode, sets are 4 or 32 bytes}
           else
            compiler.verbose.Message(scan_e_only_packset);
         end
        else
         begin
           v:=current_scanner.readval;
           case v of
            1,2,4,8 : recordpendingsetalloc(v);
           else
            compiler.verbose.Message(scan_e_only_packset);
           end;
         end;
      end;


    procedure TScanDir.dir_pic;
      begin
        { windows doesn't need/support pic }
        if tf_no_pic_supported in compiler.target.info.flags then
          compiler.verbose.Message(scan_w_pic_ignored)
        else
          do_moduleswitch(cs_create_pic);
      end;

    procedure TScanDir.dir_pop;

    begin
      if switchesstatestackpos < 1 then
        compiler.verbose.Message(scan_e_too_many_pop)
      else
        begin
          Dec(switchesstatestackpos);
          recordpendinglocalfullswitch(switchesstatestack[switchesstatestackpos].localsw);
          recordpendingverbosityfullswitch(switchesstatestack[switchesstatestackpos].verbosity);
          recordpendingalignmentfullswitch(switchesstatestack[switchesstatestackpos].alignment);
          recordpendingpackenum(switchesstatestack[switchesstatestackpos].packenum);
          recordpendingpackrecords(switchesstatestack[switchesstatestackpos].packrecords);
          recordpendingsetalloc(switchesstatestack[switchesstatestackpos].setalloc);
          recordpendingasmmode(switchesstatestack[switchesstatestackpos].asmmode);
          recordpendingoptimizerswitches(switchesstatestack[switchesstatestackpos].optimizerswitches);
          compiler.globals.pendingstate.nextmessagerecord:=switchesstatestack[switchesstatestackpos].pmessage;
          { flushpendingswitchesstate will reset the message state }
          compiler.globals.current_settings.pmessage:=nil;
          { Do not activate these changes yet, as otherwise
            you get a problem if you put a $pop just right after
            a addition for instance for which you explicitly turned the overflow check
            off by using $Q- after a $push PM 2012-08-29 }
          // flushpendingswitchesstate;
        end;
    end;

    procedure TScanDir.dir_pointermath;
      begin
        do_localswitch(cs_pointermath);
      end;

    procedure TScanDir.dir_profile;
      begin
        do_moduleswitch(cs_profile);
        { defined/undefine FPC_PROFILE }
        if cs_profile in compiler.globals.current_settings.moduleswitches then
          def_system_macro('FPC_PROFILE')
        else
          undef_system_macro('FPC_PROFILE');
      end;

    procedure TScanDir.dir_push;

    begin
      if switchesstatestackpos > switchesstatestackmax then
        compiler.verbose.Message(scan_e_too_many_push);

      { do not flush here as we might have read directives which shall not be active yet,
        see e.g. tests/webtbs/tw22744b.pp }
      if psf_alignment_changed in compiler.globals.pendingstate.flags then
        switchesstatestack[switchesstatestackpos].alignment:=compiler.globals.pendingstate.nextalignment
      else
        switchesstatestack[switchesstatestackpos].alignment:=compiler.globals.current_settings.alignment.ToRecord;

      if psf_verbosity_full_switched in compiler.globals.pendingstate.flags then
        switchesstatestack[switchesstatestackpos].verbosity:=compiler.globals.pendingstate.nextverbosityfullswitch
      else
        switchesstatestack[switchesstatestackpos].verbosity:=status.verbosity;

      if psf_local_switches_changed in compiler.globals.pendingstate.flags then
        switchesstatestack[switchesstatestackpos].localsw:=compiler.globals.pendingstate.nextlocalswitches
      else
        switchesstatestack[switchesstatestackpos].localsw:=compiler.globals.current_settings.localswitches;

      if psf_packenum_changed in compiler.globals.pendingstate.flags then
        switchesstatestack[switchesstatestackpos].packenum:=compiler.globals.pendingstate.nextpackenum
      else
        switchesstatestack[switchesstatestackpos].packenum:=compiler.globals.current_settings.packenum;

      if psf_packrecords_changed in compiler.globals.pendingstate.flags then
        switchesstatestack[switchesstatestackpos].packrecords:=compiler.globals.pendingstate.nextpackrecords
      else
        switchesstatestack[switchesstatestackpos].packrecords:=compiler.globals.current_settings.packrecords;

      if psf_setalloc_changed in compiler.globals.pendingstate.flags then
        switchesstatestack[switchesstatestackpos].setalloc:=compiler.globals.pendingstate.nextsetalloc
      else
        switchesstatestack[switchesstatestackpos].setalloc:=compiler.globals.current_settings.setalloc;

      if psf_asmmode_changed in compiler.globals.pendingstate.flags then
        switchesstatestack[switchesstatestackpos].asmmode:=compiler.globals.pendingstate.nextasmmode
      else
        switchesstatestack[switchesstatestackpos].asmmode:=compiler.globals.current_settings.asmmode;

      if psf_optimizerswitches_changed in compiler.globals.pendingstate.flags then
        switchesstatestack[switchesstatestackpos].optimizerswitches:=compiler.globals.pendingstate.nextoptimizerswitches
      else
        switchesstatestack[switchesstatestackpos].optimizerswitches:=compiler.globals.current_settings.optimizerswitches;

      switchesstatestack[switchesstatestackpos].pmessage:=compiler.globals.pendingstate.nextmessagerecord;
      Inc(switchesstatestackpos);
    end;

    procedure TScanDir.dir_rangechecks;
      begin
        do_delphiswitch('R');
      end;

    procedure TScanDir.dir_referenceinfo;
      begin
        do_delphiswitch('Y');
      end;

    procedure TScanDir.dir_resource;
      var
        s : string;
      begin
        current_scanner.skipspace;
        if current_scanner.c = '''' then
          begin
            s:= current_scanner.readquotedstring;
            current_scanner.readcomment
          end
        else
          s:= trimspace(current_scanner.readcomment);

        { replace * with the name of the main source.
          This should always be defined. }
        if s[1]='*' then
          if Assigned(compiler.current_module) then
            begin
              delete(S,1,1);
              insert(ChangeFileExt(ExtractFileName(compiler.current_module.mainsource),''),S,1 );
            end;
        s:=FixFileName(s);
        if ExtractFileExt(s)='' then
          s:=ChangeFileExt(s,compiler.target.info.resext);
        if compiler.target.info.res<>res_none then
          begin
            include(compiler.current_module.moduleflags,mf_has_resourcefiles);
            if (res_single_file in compiler.target.res.resflags) and
                                   not (compiler.current_module.ResourceFiles.Empty) then
              compiler.verbose.Message(scan_w_only_one_resourcefile_supported)
            else
              compiler.current_module.resourcefiles.insert(FixFileName(s));
          end
        else
          compiler.verbose.Message(scan_e_resourcefiles_not_supported);
      end;

    procedure TScanDir.dir_rtti;

      function read_rtti_options: trtti_visibilities;
        var
          sym: ttypesym;
          value: tnormalset;
        begin
          result:=[];
          sym:=search_system_type('TVISIBILITYCLASSES');
          if current_scanner.readpreprocset(tsetdef(sym.typedef),value,'RTTI') then
            begin
              result:=prtti_visibilities(@value)^;
              // if the set was empty we need to read the next id
              if result=[] then
                begin
                  current_scanner.skipspace;
                  current_scanner.readid
                end;
            end;
        end;

      var
        dir: trtti_directive;
        option: trtti_option;
        options: array[trtti_option] of boolean;
      begin
        { the system unit has not yet loaded which means the directive is misplaced}
        if systemunit=nil then
          begin
            compiler.verbose.Message(scan_e_misplaced_rtti_directive);
            exit;
          end;

        dir:=default(trtti_directive);

        options[ro_fields]:=false;
        options[ro_methods]:=false;
        options[ro_properties]:=false;

        { read the clause }
        current_scanner.skipspace;
        current_scanner.readid;
        case current_scanner.pattern of
          'INHERIT':
            dir.clause:=rtc_inherit;
          'EXPLICIT':
            dir.clause:=rtc_explicit;
          otherwise
            compiler.verbose.Message(scan_e_invalid_rtti_clause);
        end;

        { read the visibility options}
        current_scanner.skipspace;
        current_scanner.readid;
        { the inherit clause doesn't require any options but explicit does }
        if (current_scanner.pattern='') and (dir.clause=rtc_explicit) then
          compiler.verbose.Message(scan_e_incomplete_rtti_clause);
        while current_scanner.pattern<>'' do
          begin
            case current_scanner.pattern of
              'METHODS':
                option:=ro_methods;
              'PROPERTIES':
                option:=ro_properties;
              'FIELDS':
                option:=ro_fields;
              otherwise
                begin
                  if current_scanner.preproc_token=_ID then
                    compiler.verbose.Message1(scan_e_invalid_rtti_option,current_scanner.pattern);
                  break;
                end;
            end;
            { the option has already been used }
            if options[option] then
              begin
                compiler.verbose.Message1(scan_e_duplicate_rtti_option,current_scanner.pattern);
                break;
              end;
            dir.options[option]:=read_rtti_options;
            options[option]:=true;
          end;

        { set the directive in the module }
        compiler.current_module.rtti_directive:=dir;
      end;

    procedure TScanDir.dir_saturation;
      begin
        do_localswitch(cs_mmx_saturation);
      end;

    procedure TScanDir.dir_safefpuexceptions;
      begin
        do_localswitch(cs_fpu_fwait);
      end;

    procedure TScanDir.dir_scopedenums;
      begin
        do_localswitch(cs_scopedenums);
      end;

    function TScanDir.get_peflag_const(const ident:string;error:longint):longint;
      var
        srsym : tsym;
        srsymtable : tsymtable;
      begin
        result:=0;
        if compiler.symtablestack.searchsym(ident,srsym,srsymtable) then
          if (srsym.typ=constsym) and
              (tconstsym(srsym).consttyp=constord) and
              is_integer(tconstsym(srsym).constdef) then
            with tconstsym(srsym).value.valueord do
              if signed then
                result:=tconstsym(srsym).value.valueord.svalue
              else
                result:=tconstsym(srsym).value.valueord.uvalue
          else
            compiler.verbose.Message(error)
        else
          compiler.verbose.Message1(sym_e_id_not_found,ident);
      end;

    procedure TScanDir.dir_setpeflags;
      var
        flags : int64;
      begin
        if not (compiler.target.info.system in (systems_all_windows)) then
          compiler.verbose.Message(scan_w_setpeflags_not_support);
        if current_scanner.readpreprocint(flags,'SETPEFLAGS') then
          begin
            if flags>$ffff then
              compiler.verbose.Message(scan_e_illegal_peflag);
            compiler.globals.peflags:=compiler.globals.peflags or uint16(flags);
          end;
        compiler.globals.SetPEFlagsSetExplicity:=true;
      end;

    procedure TScanDir.dir_setpeoptflags;
      var
        flags : int64;
      begin
        if not (compiler.target.info.system in (systems_all_windows)) then
          compiler.verbose.Message(scan_w_setpeoptflags_not_support);
        if current_scanner.readpreprocint(flags,'SETPEOPTFLAGS') then
          begin
            if flags>$ffff then
              compiler.verbose.Message(scan_e_illegal_peoptflag);
            compiler.globals.peoptflags:=compiler.globals.peoptflags or uint16(flags);
          end;
        compiler.globals.SetPEOptFlagsSetExplicity:=true;
      end;

    procedure TScanDir.dir_setpeuserversion;
      var
        dummystr : string;
        dummyrev, vmajor, vminor: word;
        isset: boolean;
      begin
        if not (compiler.target.info.system in systems_all_windows) then
          compiler.verbose.Message(scan_w_setpeuserversion_not_support);
        if (not compiler.current_module.is_initial) then
          compiler.verbose.Message(scan_n_only_exe_version)
        else
          begin
            do_version(vmajor,vminor,dummyrev,dummystr,false,isset);
            compiler.globals.peuserversionmajor:=vmajor;
            compiler.globals.peuserversionminor:=vminor;
            compiler.globals.SetPEUserVersionSetExplicitely:=isset;
          end;
      end;

    procedure TScanDir.dir_setpeosversion;
      var
        dummystr : string;
        dummyrev, vmajor, vminor: word;
        isset: boolean;
      begin
        if not (compiler.target.info.system in systems_all_windows) then
          compiler.verbose.Message(scan_w_setpeosversion_not_support);
        if (not compiler.current_module.is_initial) then
          compiler.verbose.Message(scan_n_only_exe_version)
        else
          begin
            do_version(vmajor,vminor,dummyrev,dummystr,false,isset);
            compiler.globals.peosversionmajor:=vmajor;
            compiler.globals.peosversionminor:=vminor;
            compiler.globals.SetPEOSVersionSetExplicitely:=isset;
          end;
      end;

    procedure TScanDir.dir_setpesubsysversion;
      var
        dummystr : string;
        dummyrev, vmajor, vminor: word;
        isset: boolean;
      begin
        if not (compiler.target.info.system in systems_all_windows) then
          compiler.verbose.Message(scan_w_setpesubsysversion_not_support);
        if (not compiler.current_module.is_initial) then
          compiler.verbose.Message(scan_n_only_exe_version)
        else
          begin
            do_version(vmajor,vminor,dummyrev,dummystr,false,isset);
            compiler.globals.pesubsysversionmajor:=vmajor;
            compiler.globals.pesubsysversionminor:=vminor;
            compiler.globals.SetPESubSysVersionSetExplicitely:=isset;
          end;
      end;

    procedure TScanDir.dir_smartlink;
      begin
        do_moduleswitch(cs_create_smart);
        if (compiler.target.dbg.id in [dbg_dwarf2,dbg_dwarf3]) and
            not(compiler.target.info.system in (systems_darwin+[system_i8086_msdos,system_i8086_embedded])) and
            { smart linking does not yet work with DWARF debug info on most targets }
            (cs_create_smart in compiler.globals.current_settings.moduleswitches) and
            not (af_outputbinary in compiler.target._asm.flags) then
        begin
          compiler.verbose.Message(option_dwarf_smart_linking);
          compiler.globals.current_settings.moduleswitches:=compiler.globals.current_settings.moduleswitches-[cs_create_smart];
        end;
        { Also create a smartlinked version, on an assembler that
          does not support smartlink sections like nasm?
          This is not compatible with using internal linker. }
       if ((cs_link_smart in compiler.globals.current_settings.globalswitches) or
           (cs_create_smart in compiler.globals.current_settings.moduleswitches)) and
          (af_needar in compiler.target._asm.flags) and
          not (af_smartlink_sections in compiler.target._asm.flags) and
          not (cs_link_extern in compiler.globals.current_settings.globalswitches) then
         begin
           tcompiler(compiler).DoneLinker;
           compiler.verbose.Message(option_smart_link_requires_external_linker);
           compiler.globals.current_settings.globalswitches:=compiler.globals.current_settings.globalswitches+[cs_link_extern];
           tcompiler(compiler).InitLinker;
         end
      end;

    procedure TScanDir.dir_stackframes;
      begin
        do_delphiswitch('W');
      end;

    procedure TScanDir.dir_stop;
      begin
        do_message(scan_f_user_defined);
      end;

    procedure TScanDir.dir_stringchecks;
      begin
        // Delphi adds checks that ansistring and unicodestring are correct in
        // different places. Skip it for now.
      end;

    procedure TScanDir.dir_syscall;
      var
        sctype : string;
        syscall : psyscallinfo;
      begin
        current_scanner.skipspace;
        sctype:=current_scanner.readid;

        syscall:=get_syscall_by_name(sctype);
        if assigned(syscall) then
          begin
            if not (compiler.target.info.system in syscall^.validon) then
              compiler.verbose.Message(scan_w_syscall_convention_not_useable_on_target)
            else
              compiler.DefaultSyscallConvention.set_default_syscall(syscall^.procoption);
            exit;
          end;
        compiler.verbose.Message(scan_w_syscall_convention_invalid);
      end;

    procedure TScanDir.dir_targetswitch;
      var
        name, value: string;
        targetswitches: ttargetswitches;
      begin
        { note: *not* recorded in the tokenstream, so not replayed for generics }
        current_scanner.skipspace;
        name:=current_scanner.readid;
        if current_scanner.c='=' then
          begin
            current_scanner.readchar;
            current_scanner.readid;
            value:=current_scanner.orgpattern;
            targetswitches:=compiler.globals.current_settings.targetswitches;
            UpdateTargetSwitchStr(name+'='+value,targetswitches,compiler.current_module.in_global);
            compiler.globals.current_settings.targetswitches:=targetswitches;
          end
        else if current_scanner.c='-' then
          begin
            current_scanner.readchar;
            targetswitches:=compiler.globals.current_settings.targetswitches;
            UpdateTargetSwitchStr(name+'-',targetswitches,compiler.current_module.in_global);
            compiler.globals.current_settings.targetswitches:=targetswitches;
          end
        else
          begin
            targetswitches:=compiler.globals.current_settings.targetswitches;
            UpdateTargetSwitchStr(name,targetswitches,compiler.current_module.in_global);
            compiler.globals.current_settings.targetswitches:=targetswitches;
          end;
      end;

    procedure TScanDir.dir_typedaddress;
      begin
        do_delphiswitch('T');
      end;

    procedure TScanDir.dir_typeinfo;
      begin
        do_delphiswitch('M');
      end;

    procedure TScanDir.dir_unitpath;
      var
        unitpath: TPathStr;
      begin
        if not compiler.current_module.in_global then
         compiler.verbose.Message(scan_w_switch_is_global)
        else
          begin
            current_scanner.skipspace;
            unitpath:=current_scanner.readcomment;
            if (compiler.current_module.path<>'') and
               not path_absolute(unitpath) then
             unitpath:=compiler.current_module.path+source_info.DirSep+unitpath;
            compiler.current_module.localunitsearchpath.AddPath(unitpath,false);
            compiler.verbose.Message2(general_t_unitpath_local,compiler.current_module.realmodulename^,unitpath);
          end;
      end;

    procedure TScanDir.dir_varparacopyoutcheck;
      begin
        if not(compiler.target.info.system in systems_jvm) then
          begin
            compiler.verbose.Message1(scan_w_illegal_switch,current_scanner.pattern);
            exit;
          end;
        do_localswitch(cs_check_var_copyout);
      end;

    procedure TScanDir.dir_varpropsetter;
      begin
        do_localswitch(cs_varpropsetter);
      end;

    procedure TScanDir.dir_varstringchecks;
      begin
        do_delphiswitch('V');
      end;

    procedure TScanDir.dir_version;
      var
        major, minor, revision : longint;
        error : integer;
      begin
        if not (compiler.target.info.system in systems_all_windows+[system_i386_os2,system_i386_emx,
                 system_i386_netware,system_i386_wdosx,
                 system_i386_netwlibc]) then
          begin
            compiler.verbose.Message(scan_n_version_not_support);
            exit;
          end;
        if (not compiler.current_module.is_initial) then
          compiler.verbose.Message(scan_n_only_exe_version)
        else
          begin
            { change description global var in all cases }
            { it not used but in win32, os2 and netware }
            current_scanner.skipspace;
            { we should only accept Major.Minor format for win32 and os2 }
            current_scanner.readnumber;
            major:=0;
            minor:=0;
            revision:=0;
            val(current_scanner.pattern,major,error);
            if (error<>0) or (major > high(word)) or (major < 0) then
              begin
                compiler.verbose.Message1(scan_w_wrong_version_ignored,current_scanner.pattern);
                exit;
              end;
            if current_scanner.c='.' then
              begin
                current_scanner.readchar;
                current_scanner.readnumber;
                val(current_scanner.pattern,minor,error);
                if (error<>0) or (minor > high(word)) or (minor < 0) then
                  begin
                    compiler.verbose.Message1(scan_w_wrong_version_ignored,tostr(major)+'.'+current_scanner.pattern);
                    exit;
                  end;
                if (current_scanner.c='.') and
                   (compiler.target.info.system in [system_i386_netware,system_i386_netwlibc]) then
                  begin
                     current_scanner.readchar;
                     current_scanner.readnumber;
                     val(current_scanner.pattern,revision,error);
                     if (error<>0) or (revision > high(word)) or (revision < 0) then
                       begin
                          compiler.verbose.Message1(scan_w_wrong_version_ignored,tostr(revision)+'.'+current_scanner.pattern);
                          exit;
                       end;
                     compiler.globals.dllmajor:=word(major);
                     compiler.globals.dllminor:=word(minor);
                     compiler.globals.dllrevision:=word(revision);
                     compiler.globals.dllversion:=tostr(major)+','+tostr(minor)+','+tostr(revision);
                  end
                else
                  begin
                     compiler.globals.dllmajor:=word(major);
                     compiler.globals.dllminor:=word(minor);
                     compiler.globals.dllversion:=tostr(major)+'.'+tostr(minor);
                  end;
              end
            else
              compiler.globals.dllversion:=tostr(major);
          end;
      end;

    procedure TScanDir.dir_wait;
      var
        had_info : boolean;
      begin
        had_info:=(status.verbosity and V_Info)<>0;
        { this message should always appear !! }
        status.verbosity:=status.verbosity or V_Info;
        compiler.verbose.Message(scan_i_press_enter);
        readln;
        If not(had_info) then
          status.verbosity:=status.verbosity and (not V_Info);
      end;

    { delphi compatible warn directive:
      $warn <identifier> on
      $warn <identifier> off
      $warn <identifier> error
    }
    procedure TScanDir.dir_warn;
      var
        ident : string;
        state : string;
        msgstate : tmsgstate;
        i : integer;
      begin
        current_scanner.skipspace;
        ident:=current_scanner.readid;
        current_scanner.skipspace;
        if current_scanner.c in ['+','-'] then
          begin
            state:=current_scanner.c;
            current_scanner.readchar;
          end
        else
          state:=current_scanner.readid;

        { support both delphi and fpc switches }
        { use local ms_on/off/error tmsgstate values }
        if (state='ON') or (state='+') then
          msgstate:=ms_on
        else
        if (state='OFF') or (state='-') then
          msgstate:=ms_off
        else
        if (state='ERROR') then
          msgstate:=ms_error
        else
        begin
          compiler.verbose.Message1(scanner_e_illegal_warn_state,state);
          exit;
        end;

        if ident='CONSTRUCTING_ABSTRACT' then
          begin
            recordpendingmessagestate(type_w_instance_with_abstract, msgstate);
            recordpendingmessagestate(type_w_instance_abstract_class, msgstate);
          end
        else
        if ident='IMPLICIT_VARIANTS' then
          recordpendingmessagestate(parser_w_implicit_uses_of_variants_unit, msgstate)
        else
        if ident='NO_RETVAL' then
          recordpendingmessagestate(sym_w_function_result_not_set, msgstate)
        else
        if ident='SYMBOL_DEPRECATED' then
          begin
            recordpendingmessagestate(sym_w_deprecated_symbol, msgstate);
            recordpendingmessagestate(sym_w_deprecated_symbol_with_msg, msgstate);
          end
        else
        if ident='SYMBOL_EXPERIMENTAL' then
          recordpendingmessagestate(sym_w_experimental_symbol, msgstate)
        else
        if ident='SYMBOL_LIBRARY' then
          recordpendingmessagestate(sym_w_library_symbol, msgstate)
        else
        if ident='SYMBOL_PLATFORM' then
          recordpendingmessagestate(sym_w_non_portable_symbol, msgstate)
        else
        if ident='SYMBOL_UNIMPLEMENTED' then
          recordpendingmessagestate(sym_w_non_implemented_symbol, msgstate)
        else
        if ident='UNIT_DEPRECATED' then
          begin
            recordpendingmessagestate(sym_w_deprecated_unit, msgstate);
            recordpendingmessagestate(sym_w_deprecated_unit_with_msg, msgstate);
          end
        else
        if ident='UNIT_EXPERIMENTAL' then
          recordpendingmessagestate(sym_w_experimental_unit, msgstate)
        else
        if ident='UNIT_LIBRARY' then
          recordpendingmessagestate(sym_w_library_unit, msgstate)
        else
        if ident='UNIT_PLATFORM' then
          recordpendingmessagestate(sym_w_non_portable_unit, msgstate)
        else
        if ident='UNIT_UNIMPLEMENTED' then
          recordpendingmessagestate(sym_w_non_implemented_unit, msgstate)
        else
        if ident='ZERO_NIL_COMPAT' then
          recordpendingmessagestate(type_w_zero_to_nil, msgstate)
        else
        if ident='IMPLICIT_STRING_CAST' then
          recordpendingmessagestate(type_w_implicit_string_cast, msgstate)
        else
        if ident='IMPLICIT_STRING_CAST_LOSS' then
          recordpendingmessagestate(type_w_implicit_string_cast_loss, msgstate)
        else
        if ident='EXPLICIT_STRING_CAST' then
          recordpendingmessagestate(type_w_explicit_string_cast, msgstate)
        else
        if ident='EXPLICIT_STRING_CAST_LOSS' then
          recordpendingmessagestate(type_w_explicit_string_cast_loss, msgstate)
        else
        if ident='CVT_NARROWING_STRING_LOST' then
          recordpendingmessagestate(type_w_unicode_data_loss, msgstate)
        else
        if ident='INTF_RAISE_VISIBILITY' then
          recordpendingmessagestate(type_w_interface_lower_visibility, msgstate)
        else
          begin
            i:=0;
            if not compiler.verbose.ChangeMessageVerbosity(ident,i,msgstate) then
              compiler.verbose.Message1(scanner_w_illegal_warn_identifier,ident);
          end;
      end;

    procedure TScanDir.dir_warning;
      begin
        do_message(scan_w_user_defined);
      end;

    procedure TScanDir.dir_warnings;
      begin
        do_setverbose('W');
      end;

    procedure TScanDir.dir_weakpackageunit;
      begin
        { old Delphi versions seem to use merely $WEAKPACKAGEUNIT while newer
          Delphis have $WEAPACKAGEUNIT ON... :/ }
        do_moduleflagswitch(mf_package_weak, true);
      end;

    procedure TScanDir.dir_writeableconst;
      begin
        do_delphiswitch('J');
      end;

    procedure TScanDir.dir_yd;
      begin
        HandleSwitch('Y','+');
      end;

    procedure TScanDir.dir_z1;
      begin
        compiler.globals.current_settings.packenum:=1;
      end;

    procedure TScanDir.dir_z2;
      begin
        compiler.globals.current_settings.packenum:=2;
      end;

    procedure TScanDir.dir_z4;
      begin
        compiler.globals.current_settings.packenum:=4;
      end;

    procedure TScanDir.dir_externalsym;
      begin
      end;

    procedure TScanDir.dir_nodefine;
      begin
      end;

    procedure TScanDir.dir_hppemit;
      begin
      end;

    procedure TScanDir.dir_hugecode;
      begin
        if not (compiler.target.info.system in [system_i8086_msdos,system_i8086_embedded])
{$ifdef i8086}
           or (compiler.globals.current_settings.x86memorymodel in x86_near_code_models)
{$endif i8086}
            then
          begin
            compiler.verbose.Message1(scan_n_ignored_switch,current_scanner.pattern);
            exit;
          end;
        do_moduleswitch(cs_huge_code);
      end;

    procedure TScanDir.dir_hugepointernormalization;
      var
        hs : string;
      begin
        if not (compiler.target.info.system in [system_i8086_msdos,system_i8086_embedded]) then
          begin
            compiler.verbose.Message1(scanner_w_directive_ignored_on_target, 'HUGEPOINTERNORMALIZATION');
            exit;
          end;
        current_scanner.skipspace;
        hs:=current_scanner.readid;
        case hs of
          'BORLANDC':
             begin
               recordpendinglocalswitch(cs_hugeptr_arithmetic_normalization,'+');
               recordpendinglocalswitch(cs_hugeptr_comparison_normalization,'+');
             end;
          'MICROSOFTC':
             begin
               recordpendinglocalswitch(cs_hugeptr_arithmetic_normalization,'-');
               recordpendinglocalswitch(cs_hugeptr_comparison_normalization,'-');
             end;
          'WATCOMC':
             begin
               recordpendinglocalswitch(cs_hugeptr_arithmetic_normalization,'-');
               recordpendinglocalswitch(cs_hugeptr_comparison_normalization,'+');
             end;
          else
            compiler.verbose.Message(scan_e_illegal_hugepointernormalization);
        end;
      end;

    procedure TScanDir.dir_hugepointerarithmeticnormalization;
      begin
        if not (compiler.target.info.system in [system_i8086_msdos,system_i8086_embedded]) then
          begin
            compiler.verbose.Message1(scanner_w_directive_ignored_on_target, 'HUGEPOINTERARITHMETICNORMALIZATION');
            exit;
          end;
        do_localswitch(cs_hugeptr_arithmetic_normalization);
      end;

    procedure TScanDir.dir_hugepointercomparisonnormalization;
      begin
        if not (compiler.target.info.system in [system_i8086_msdos,system_i8086_embedded]) then
          begin
            compiler.verbose.Message1(scanner_w_directive_ignored_on_target, 'HUGEPOINTERCOMPARISONNORMALIZATION');
            exit;
          end;
        do_localswitch(cs_hugeptr_comparison_normalization);
      end;

    procedure TScanDir.dir_codealign;
      var
        s : string;
      begin
        current_scanner.skipspace;
        s:=current_scanner.readcomment;
        if not(UpdateAlignmentStr(s,compiler.globals.current_settings.alignment)) then
          compiler.verbose.Message(scanner_e_illegal_alignment_directive);
      end;

    procedure TScanDir.dir_codepage;
      var
         s : string;
      begin
        if not compiler.current_module.in_global then
          compiler.verbose.Message(scan_w_switch_is_global)
        else
          begin
            current_scanner.skipspace;
            s:=current_scanner.readcomment;
            if (upper(s)='UTF8') or (upper(s)='UTF-8') then
              compiler.globals.current_settings.sourcecodepage:=CP_UTF8
            else if not cpavailable(s) then
              compiler.verbose.Message1(option_code_page_not_available,s)
            else
              compiler.globals.current_settings.sourcecodepage:=codepagebyname(s);
            { we're not using the system code page now }
            compiler.globals.current_settings.modeswitches:=compiler.globals.current_settings.modeswitches-[m_systemcodepage];
            compiler.globals.current_settings.moduleswitches:=compiler.globals.current_settings.moduleswitches-[cs_system_codepage];
            compiler.globals.current_settings.moduleswitches:=compiler.globals.current_settings.moduleswitches+[cs_explicit_codepage];
          end;
      end;

    procedure TScanDir.dir_coperators;
      begin
        do_moduleswitch(cs_support_c_operators);
      end;


    procedure TScanDir.dir_bitpacking;
      begin
        do_localswitch(cs_bitpacking);
      end;

    procedure TScanDir.dir_region;
      begin
      end;

    procedure TScanDir.dir_endregion;
      begin
      end;

    procedure TScanDir.dir_zerobasesstrings;
      begin
        do_localswitch(cs_zerobasedstrings);
      end;

    constructor TScanDir.Create(ACompiler: TCompilerBase);
      begin
        FCompiler:=ACompiler;
      end;


{****************************************************************************
                         Initialize Directives
****************************************************************************}

    procedure TScanDir.InitScannerDirectives(AScanner: TScanner);
      begin
        with AScanner do
          begin
            AddDirective('A1',directive_all, @dir_a1);
            AddDirective('A2',directive_all, @dir_a2);
            AddDirective('A4',directive_all, @dir_a4);
            AddDirective('A8',directive_all, @dir_a8);
            AddDirective('ALIGN',directive_all, @dir_align);
{$ifdef m68k}
            AddDirective('APPID',directive_all, @dir_appid);
            AddDirective('APPNAME',directive_all, @dir_appname);
{$endif m68k}
            AddDirective('APPTYPE',directive_all, @dir_apptype);
            AddDirective('ASMCPU',directive_all, @dir_asmcpu);
            AddDirective('ASMMODE',directive_all, @dir_asmmode);
            AddDirective('ASSERTIONS',directive_all, @dir_assertions);
            AddDirective('BOOLEVAL',directive_all, @dir_booleval);
            AddDirective('BITPACKING',directive_all, @dir_bitpacking);
            AddDirective('CALLING',directive_all, @dir_calling);
            AddDirective('CHECKCASECOVERAGE',directive_all, @dir_checkcasecoverage);
            AddDirective('CHECKFPUEXCEPTIONS',directive_all, @dir_checkfpuexceptions);
            AddDirective('CHECKLOWADDRLOADS',directive_all, @dir_checklowaddrloads);
            AddDirective('CHECKPOINTER',directive_all, @dir_checkpointer);
            AddDirective('CODEALIGN',directive_all, @dir_codealign);
            AddDirective('CODEPAGE',directive_all, @dir_codepage);
            AddDirective('COPERATORS',directive_all, @dir_coperators);
            AddDirective('COPYRIGHT',directive_all, @dir_copyright);
            AddDirective('D',directive_all, @dir_description);
            AddDirective('DEBUGINFO',directive_all, @dir_debuginfo);
            AddDirective('DEFINITIONINFO',directive_all, @dir_definitioninfo);
            AddDirective('DENYPACKAGEUNIT',directive_all,@dir_denypackageunit);
            AddDirective('DESCRIPTION',directive_all, @dir_description);
            AddDirective('ENDREGION',directive_all, @dir_endregion);
            AddDirective('ERROR',directive_all, @dir_error);
            AddDirective('ERRORC',directive_mac, @dir_error);
            AddDirective('EXCESSPRECISION',directive_all, @dir_excessprecision);
            AddDirective('EXTENDEDSYNTAX',directive_all, @dir_extendedsyntax);
            AddDirective('EXTERNALSYM',directive_all, @dir_externalsym);
            AddDirective('F',directive_all, @dir_forcefarcalls);
            AddDirective('FARCALLS',directive_all, @dir_forcefarcalls);
            AddDirective('FATAL',directive_all, @dir_fatal);
            AddDirective('FLOATINGPOINTEMULATION',directive_all,@dir_floatingpointemulation);
            AddDirective('FPUTYPE',directive_all, @dir_fputype);
            AddDirective('FRAMEWORKPATH',directive_all, @dir_frameworkpath);
            AddDirective('GOTO',directive_all, @dir_goto);
            AddDirective('HINT',directive_all, @dir_hint);
            AddDirective('HINTS',directive_all, @dir_hints);
            AddDirective('HPPEMIT',directive_all, @dir_hppemit);
            AddDirective('HUGECODE',directive_all, @dir_hugecode);
            AddDirective('HUGEPOINTERNORMALIZATION',directive_all,@dir_hugepointernormalization);
            AddDirective('HUGEPOINTERARITHMETICNORMALIZATION',directive_all,@dir_hugepointerarithmeticnormalization);
            AddDirective('HUGEPOINTERCOMPARISONNORMALIZATION',directive_all,@dir_hugepointercomparisonnormalization);
            AddDirective('IEEEERRORS',directive_all,@dir_ieeeerrors);
            AddDirective('IOCHECKS',directive_all, @dir_iochecks);
            AddDirective('IMAGEBASE',directive_all, @dir_imagebase);
            AddDirective('IMPLICITEXCEPTIONS',directive_all, @dir_implicitexceptions);
            AddDirective('IMPORTEDDATA',directive_all, @dir_importeddata);
            AddDirective('INCLUDEPATH',directive_all, @dir_includepath);
            AddDirective('INFO',directive_all, @dir_info);
            AddDirective('INLINE',directive_all, @dir_inline);
            AddDirective('INTERFACES',directive_all, @dir_interfaces);
            AddDirective('L',directive_all, @dir_link);
            AddDirective('LEGACYIFEND',directive_all, @dir_legacyifend);
            AddDirective('LIBEXPORT',directive_mac, @dir_libexport);
            AddDirective('LIBRARYPATH',directive_all, @dir_librarypath);
            AddDirective('LINK',directive_all, @dir_link);
            AddDirective('LINKFRAMEWORK',directive_all, @dir_linkframework);
            AddDirective('LINKLIB',directive_all, @dir_linklib);
            AddDirective('LOCALSYMBOLS',directive_all, @dir_localsymbols);
            AddDirective('LONGSTRINGS',directive_all, @dir_longstrings);
            AddDirective('M',directive_all, @dir_memory);
            AddDirective('MACRO',directive_all, @dir_macro);
            AddDirective('MAXFPUREGISTERS',directive_all, @dir_maxfpuregisters);
            AddDirective('MAXSTACKSIZE',directive_all, @dir_maxstacksize);
            AddDirective('MEMORY',directive_all, @dir_memory);
            AddDirective('MESSAGE',directive_all, @dir_message);
            AddDirective('MINENUMSIZE',directive_all, @dir_packenum);
            AddDirective('MINFPCONSTPREC',directive_all, @dir_minfpconstprec);
            AddDirective('MINSTACKSIZE',directive_all, @dir_minstacksize);
            AddDirective('MMX',directive_all, @dir_mmx);
            AddDirective('MODE',directive_all, @dir_mode);
            AddDirective('MODESWITCH',directive_all, @dir_modeswitch);
            AddDirective('TEXTBLOCK',directive_all, @dir_textblock);
            AddDirective('MULTILINESTRINGLINEENDING',directive_all, @dir_multilinestringlineending);
            AddDirective('MULTILINESTRINGTRIMLEFT',directive_all, @dir_multilinestringtrimleft);
            AddDirective('NAMESPACE',directive_all, @dir_namespace);
            AddDirective('NAMESPACES',directive_all, @dir_namespaces);
            AddDirective('NODEFINE',directive_all, @dir_nodefine);
            AddDirective('NOTE',directive_all, @dir_note);
            AddDirective('NOTES',directive_all, @dir_notes);
            AddDirective('OBJECTCHECKS',directive_all, @dir_objectchecks);
            AddDirective('OBJECTPATH',directive_all, @dir_objectpath);
            AddDirective('OPENSTRINGS',directive_all, @dir_openstrings);
            AddDirective('OPTIMIZATION',directive_all, @dir_optimization);
            AddDirective('OV',directive_mac, @dir_overflowchecks);
            AddDirective('OVERFLOWCHECKS',directive_all, @dir_overflowchecks);
            AddDirective('PACKENUM',directive_all, @dir_packenum);
            AddDirective('PACKRECORDS',directive_all, @dir_packrecords);
            AddDirective('PACKSET',directive_all, @dir_packset);
            AddDirective('PASCALMAINNAME',directive_all, @dir_pascalmainname);
            AddDirective('PIC',directive_all, @dir_pic);
            AddDirective('POINTERMATH',directive_all, @dir_pointermath);
            AddDirective('POP',directive_all, @dir_pop);
            AddDirective('POPOPT',directive_all, @dir_pop);
            AddDirective('PROFILE',directive_all, @dir_profile);
            AddDirective('PUSH',directive_all, @dir_push);
            AddDirective('PUSHOPT',directive_all, @dir_push);
            AddDirective('R',directive_all, @dir_resource);
            AddDirective('RTTI',directive_all, @dir_rtti);
            AddDirective('RANGECHECKS',directive_all, @dir_rangechecks);
            AddDirective('REFERENCEINFO',directive_all, @dir_referenceinfo);
            AddDirective('REGION',directive_all, @dir_region);
            AddDirective('RESOURCE',directive_all, @dir_resource);
            AddDirective('SATURATION',directive_all, @dir_saturation);
            AddDirective('SAFEFPUEXCEPTIONS',directive_all, @dir_safefpuexceptions);
            AddDirective('SCOPEDENUMS',directive_all, @dir_scopedenums);
            AddDirective('SETPEFLAGS', directive_all, @dir_setpeflags);
            AddDirective('SETPEOPTFLAGS', directive_all, @dir_setpeoptflags);
            AddDirective('SETPEOSVERSION', directive_all, @dir_setpeosversion);
            AddDirective('SETPEUSERVERSION', directive_all, @dir_setpeuserversion);
            AddDirective('SETPESUBSYSVERSION', directive_all, @dir_setpesubsysversion);
            AddDirective('SCREENNAME',directive_all, @dir_screenname);
            AddDirective('SMARTLINK',directive_all, @dir_smartlink);
            AddDirective('STACKCHECKING',directive_all,@dir_stackchecking);
            AddDirective('STACKFRAMES',directive_all, @dir_stackframes);
            AddDirective('STOP',directive_all, @dir_stop);
            AddDirective('STRINGCHECKS', directive_all, @dir_stringchecks);
            AddDirective('SYSCALL',directive_all, @dir_syscall);
            AddDirective('TARGETSWITCH',directive_all, @dir_targetswitch);
            AddDirective('THREADNAME',directive_all, @dir_threadname);
            AddDirective('TYPEDADDRESS',directive_all, @dir_typedaddress);
            AddDirective('TYPEINFO',directive_all, @dir_typeinfo);
            AddDirective('UNITPATH',directive_all, @dir_unitpath);
            AddDirective('VARPARACOPYOUTCHECK',directive_all, @dir_varparacopyoutcheck);
            AddDirective('VARPROPSETTER',directive_all, @dir_varpropsetter);
            AddDirective('VARSTRINGCHECKS',directive_all, @dir_varstringchecks);
            AddDirective('VERSION',directive_all, @dir_version);
            AddDirective('WAIT',directive_all, @dir_wait);
            AddDirective('WARN',directive_all, @dir_warn);
            AddDirective('WARNING',directive_all, @dir_warning);
            AddDirective('WARNINGS',directive_all, @dir_warnings);
            AddDirective('WEAKPACKAGEUNIT',directive_all, @dir_weakpackageunit);
            AddDirective('WRITEABLECONST',directive_all, @dir_writeableconst);
            AdDDirective('YD',directive_all, @dir_yd);
            AddDirective('Z1',directive_all, @dir_z1);
            AddDirective('Z2',directive_all, @dir_z2);
            AddDirective('Z4',directive_all, @dir_z4);
            AddDirective('ZEROBASEDSTRINGS',directive_all, @dir_zerobasesstrings);
          end;
      end;

end.
