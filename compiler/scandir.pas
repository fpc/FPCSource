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


    procedure InitScannerDirectives;

implementation

    uses
      SysUtils,
      cutils,cfileutils,
      globtype,globals,systems,widestr,cpuinfo,
      verbose,comphook,ppu,
      scanner,switches,
      fmodule,
      symconst,symtable,
      rabase;

    const
      localswitchesstackmax = 20;

    var
      localswitchesstack: array[0..localswitchesstackmax] of tlocalswitches;
      localswitchesstackpos: Integer;

{*****************************************************************************
                                    Helpers
*****************************************************************************}

    procedure do_delphiswitch(sw:char);
      var
        state : char;
      begin
      { c contains the next char, a + or - would be fine }
        state:=current_scanner.readstate;
        if state in ['-','+'] then
          HandleSwitch(sw,state);
      end;


    procedure do_setverbose(flag:char);
      var
        state : char;
      begin
      { support ON/OFF }
        state:=current_scanner.ReadState;
        SetVerbosity(flag+state);
      end;


    procedure do_moduleswitch(sw:tmoduleswitch);
      var
        state : char;
      begin
        state:=current_scanner.readstate;
        if (sw<>cs_modulenone) and (state in ['-','+']) then
         begin
           if state='-' then
            exclude(current_settings.moduleswitches,sw)
           else
            include(current_settings.moduleswitches,sw);
         end;
      end;


    procedure do_localswitch(sw:tlocalswitch);
      var
        state : char;
      begin
        state:=current_scanner.readstate;
        if (sw<>cs_localnone) and (state in ['-','+']) then
         begin
           if not localswitcheschanged then
             nextlocalswitches:=current_settings.localswitches;
           if state='-' then
            exclude(nextlocalswitches,sw)
           else
            include(nextlocalswitches,sw);
           localswitcheschanged:=true;
         end;
      end;

    procedure do_localswitchdefault(sw:tlocalswitch);
      var
        state : char;
      begin
        state:=current_scanner.readstatedefault;
        if (sw<>cs_localnone) and (state in ['-','+','*']) then
         begin
           if not localswitcheschanged then
             nextlocalswitches:=current_settings.localswitches;
           if state='-' then
            exclude(nextlocalswitches,sw)
           else
            if state='+' then
             include(nextlocalswitches,sw)
            else
             begin
              if sw in init_settings.localswitches then
               include(nextlocalswitches,sw)
              else
               exclude(nextlocalswitches,sw);
             end;
           localswitcheschanged:=true;
         end;
      end;


    procedure do_message(w:integer);
      begin
        current_scanner.skipspace;
        Message1(w,current_scanner.readcomment);
      end;

{*****************************************************************************
                              Directive Callbacks
*****************************************************************************}

    procedure dir_align;
      var
        hs : string;
      begin
        current_scanner.skipspace;
        if not(c in ['0'..'9']) then
         begin
           { Support also the ON and OFF as switch }
           hs:=current_scanner.readid;
           if (hs='ON') then
            current_settings.packrecords:=4
           else if (hs='OFF') then
             current_settings.packrecords:=1
           else if m_mac in current_settings.modeswitches then
             begin
               { Support switches used in Apples Universal Interfaces}
               if (hs='MAC68K') then
                 current_settings.packrecords:=2
               else if (hs='POWER') then
                 current_settings.packrecords:=4
               else if (hs='RESET') then
                 current_settings.packrecords:=0
               else
                 Message1(scan_e_illegal_pack_records,hs);
             end
           else
             Message1(scan_e_illegal_pack_records,hs);
         end
        else
         begin
           case current_scanner.readval of
             1 : current_settings.packrecords:=1;
             2 : current_settings.packrecords:=2;
             4 : current_settings.packrecords:=4;
             8 : current_settings.packrecords:=8;
            16 : current_settings.packrecords:=16;
            32 : current_settings.packrecords:=32;
           else
            Message1(scan_e_illegal_pack_records,hs);
           end;
         end;
      end;

    procedure dir_a1;
      begin
        current_settings.packrecords:=1;
      end;

    procedure dir_a2;
      begin
        current_settings.packrecords:=2;
      end;

    procedure dir_a4;
      begin
        current_settings.packrecords:=4;
      end;

    procedure dir_a8;
      begin
        current_settings.packrecords:=8;
      end;

    procedure dir_asmmode;
      var
        s : string;
      begin
        current_scanner.skipspace;
        s:=current_scanner.readid;
        If Inside_asm_statement then
          Message1(scan_w_no_asm_reader_switch_inside_asm,s);
        if s='DEFAULT' then
         current_settings.asmmode:=init_settings.asmmode
        else
         if not SetAsmReadMode(s,current_settings.asmmode) then
           Message1(scan_e_illegal_asmmode_specifier,s);
      end;

{$if defined(m68k) or defined(arm)}
    procedure dir_appid;
      begin
        if target_info.system<>system_m68k_palmos then
          Message(scan_w_appid_not_support);
        { change description global var in all cases }
        { it not used but in win32 and os2 }
        current_scanner.skipspace;
        palmos_applicationid:=current_scanner.readcomment;
      end;

    procedure dir_appname;
      begin
        if target_info.system<>system_m68k_palmos then
          Message(scan_w_appname_not_support);
        { change description global var in all cases }
        { it not used but in win32 and os2 }
        current_scanner.skipspace;
        palmos_applicationname:=current_scanner.readcomment;
      end;
{$endif defined(m68k) or defined(arm)}

    procedure dir_apptype;
      var
         hs : string;
      begin
        if not (target_info.system in system_all_windows + [system_i386_os2,
                                       system_i386_emx, system_powerpc_macos,
                                       system_arm_nds]) then
          begin
            if m_delphi in current_settings.modeswitches then
              Message(scan_n_app_type_not_support)
            else
              Message(scan_w_app_type_not_support);
          end
        else
          begin
            if not current_module.in_global then
              Message(scan_w_switch_is_global)
            else
              begin
                 current_scanner.skipspace;
                 hs:=current_scanner.readid;
                 if hs='GUI' then
                   apptype:=app_gui
                 else if hs='CONSOLE' then
                   apptype:=app_cui
                 else if (hs='NATIVE') and (target_info.system in system_windows) then
                   apptype:=app_native
                 else if (hs='FS') and (target_info.system in [system_i386_os2,
                                                             system_i386_emx]) then
                   apptype:=app_fs
                 else if (hs='TOOL') and (target_info.system in [system_powerpc_macos]) then
                   apptype:=app_tool
                 else if (hs='ARM9') and (target_info.system in [system_arm_nds]) then
                   apptype:=app_arm9
                 else if (hs='ARM7') and (target_info.system in [system_arm_nds]) then
                   apptype:=app_arm7
                 else
                   Message1(scan_w_unsupported_app_type,hs);
              end;
          end;
      end;


    procedure dir_calling;
      var
         hs : string;
      begin
        current_scanner.skipspace;
        hs:=current_scanner.readid;
        if not SetAktProcCall(hs,current_settings.defproccall) then
          begin
            if (hs <> '') then
              Message1(parser_w_unknown_proc_directive_ignored,hs)
            else
              Message(parser_e_proc_directive_expected);
          end;
      end;


    procedure dir_checkpointer;
      begin
        do_localswitchdefault(cs_checkpointer);
      end;


    procedure dir_objectchecks;
      begin
        do_localswitch(cs_check_object);
      end;


    procedure dir_assertions;
      begin
        do_delphiswitch('C');
      end;

    procedure dir_booleval;
      begin
        do_delphiswitch('B');
      end;

    procedure dir_debuginfo;
      begin
        do_delphiswitch('D');
      end;

    procedure dir_description;
      begin
        if not (target_info.system in [system_i386_os2,system_i386_emx,
                 system_i386_win32,system_i386_netware,system_i386_wdosx,system_i386_netwlibc]) then
          Message(scan_w_description_not_support);
        { change description global var in all cases }
        { it not used but in win32, os2 and netware }
        current_scanner.skipspace;
        description:=current_scanner.readcomment;
        DescriptionSetExplicity:=true;
      end;

    procedure dir_screenname; {ad}
      begin
        if not (target_info.system in [system_i386_netware,system_i386_netwlibc]) then
          {Message(scan_w_decription_not_support);}
          comment (V_Warning,'Screenname only supported for target netware');
        current_scanner.skipspace;
        nwscreenname:=current_scanner.readcomment;
      end;

      procedure dir_threadname; {ad}
      begin
        if not (target_info.system in [system_i386_netware,system_i386_netwlibc]) then
          {Message(scan_w_decription_not_support);}
          comment (V_Warning,'Threadname only supported for target netware');
        current_scanner.skipspace;
        nwthreadname:=current_scanner.readcomment;
      end;

      procedure dir_copyright; {ad}
      begin
        if not (target_info.system in [system_i386_netware,system_i386_netwlibc]) then
          {Message(scan_w_decription_not_support);}
          comment (V_Warning,'Copyright only supported for target netware');
        current_scanner.skipspace;
        nwcopyright:=current_scanner.readcomment;
      end;

    procedure dir_error;
      begin
        do_message(scan_e_user_defined);
      end;

    procedure dir_extendedsyntax;
      begin
        do_delphiswitch('X');
      end;

    procedure dir_fatal;
      begin
        do_message(scan_f_user_defined);
      end;

    procedure dir_fputype;
      begin
        current_scanner.skipspace;
        { current_scanner.undef_macro('FPU'+fputypestr[current_settings.fputype]); }
        if not(SetFPUType(upper(current_scanner.readcomment),current_settings.fputype)) then
          comment(V_Error,'Illegal FPU type');
        { current_scanner.def_macro('FPU'+fputypestr[current_settings.fputype]); }
     end;

    procedure dir_goto;
      begin
        do_moduleswitch(cs_support_goto);
      end;

    procedure dir_hint;
      begin
        do_message(scan_h_user_defined);
      end;

    procedure dir_hints;
      begin
        do_setverbose('H');
      end;

    procedure dir_imagebase;
      begin
        if not (target_info.system in (system_windows+system_wince)) then
          Message(scan_w_imagebase_not_support);
        current_scanner.skipspace;
        imagebase:=current_scanner.readval;
        ImageBaseSetExplicity:=true
      end;

    procedure dir_implicitexceptions;
      begin
        do_moduleswitch(cs_implicit_exceptions);
      end;

    procedure dir_includepath;
      begin
        if not current_module.in_global then
         Message(scan_w_switch_is_global)
        else
          begin
            current_scanner.skipspace;
            current_module.localincludesearchpath.AddPath(current_scanner.readcomment,false);
          end;
      end;

    procedure dir_info;
      begin
        do_message(scan_i_user_defined);
      end;

    procedure dir_inline;
      begin
        do_localswitch(cs_do_inline);
      end;

    procedure dir_interfaces;
      var
        hs : string;
      begin
        {corba/com/default}
        current_scanner.skipspace;
        hs:=current_scanner.readid;
        if (hs='CORBA') then
          current_settings.interfacetype:=it_interfacecorba
        else if (hs='COM') then
          current_settings.interfacetype:=it_interfacecom
        else if (hs='DEFAULT') then
          current_settings.interfacetype:=init_settings.interfacetype
        else
          Message(scan_e_invalid_interface_type);
      end;

    procedure dir_iochecks;
      begin
        do_delphiswitch('I');
      end;

    procedure dir_libexport;
      begin
        {not implemented}
      end;

    procedure dir_librarypath;
      begin
        if not current_module.in_global then
         Message(scan_w_switch_is_global)
        else
          begin
            current_scanner.skipspace;
            current_module.locallibrarysearchpath.AddPath(current_scanner.readcomment,false);
          end;
      end;

    procedure dir_link;
      var
        s : string;
      begin
        current_scanner.skipspace;
        if scanner.c = '''' then
          begin
            s:= current_scanner.readquotedstring;
            current_scanner.readcomment
          end
        else
          s:= trimspace(current_scanner.readcomment);
        s:=ChangeFileExt(FixFileName(s),target_info.objext);
        current_module.linkotherofiles.add(s,link_always);
      end;

    procedure dir_linklib;
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
        if scanner.c = '''' then
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
           if libext=target_info.staticClibext then
             linkMode:=lm_static;
         end
        else if linkModeStr='STATIC' then
         linkmode:=lm_static
        else if (LinkModeStr='SHARED') or (LinkModeStr='') then
         linkmode:=lm_shared
        else
         Comment(V_Error,'Wrong link mode specified: "'+Linkmodestr+'"');

        { add to the list of other libraries }
        if linkMode=lm_static then
         current_module.linkOtherStaticLibs.add(libname,link_always)
        else
         current_module.linkOtherSharedLibs.add(libname,link_always);
      end;

    procedure dir_localsymbols;
      begin
        do_delphiswitch('L');
      end;

    procedure dir_longstrings;
      begin
        do_delphiswitch('H');
      end;

    procedure dir_macro;
      begin
        do_moduleswitch(cs_support_macro);
      end;

    procedure dir_maxfpuregisters;
      var
         l  : integer;
         hs : string;
      begin
         current_scanner.skipspace;
         if not(c in ['0'..'9']) then
           begin
              hs:=current_scanner.readid;
              if (hs='NORMAL') or (hs='DEFAULT') then
                current_settings.maxfpuregisters:=-1
              else
                Message(scan_e_invalid_maxfpureg_value);
           end
         else
           begin
              l:=current_scanner.readval;
              case l of
                 0..8:
                   current_settings.maxfpuregisters:=l;
                 else
                   Message(scan_e_invalid_maxfpureg_value);
              end;
           end;
      end;

    procedure dir_maxstacksize;
      begin
        if not (target_info.system in (system_windows+system_wince)) then
          Message(scan_w_maxstacksize_not_support);
        current_scanner.skipspace;
        maxstacksize:=current_scanner.readval;
        MaxStackSizeSetExplicity:=true;
      end;

    procedure dir_memory;
      var
        l : longint;
      begin
        current_scanner.skipspace;
        l:=current_scanner.readval;
        if l>1024 then
          stacksize:=l;
        if c=',' then
          begin
            current_scanner.readchar;
            current_scanner.skipspace;
            l:=current_scanner.readval;
            if l>1024 then
              heapsize:=l;
          end;
      end;


    procedure dir_message;
      var
        hs : string;
        w  : longint;
      begin
        w:=0;
        current_scanner.skipspace;
        { Message level specified? }
        if c='''' then
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
              Message1(scan_w_illegal_directive,hs);
          end;
        { Only print message when there was no error }
        if w<>0 then
          begin
            current_scanner.skipspace;
            if c='''' then
              hs:=current_scanner.readquotedstring
            else
              hs:=current_scanner.readcomment;
            Message1(w,hs);
          end
        else
          current_scanner.readcomment;
      end;


    procedure dir_minstacksize;
      begin
        if not (target_info.system in (system_windows+system_wince)) then
          Message(scan_w_minstacksize_not_support);
        current_scanner.skipspace;
        minstacksize:=current_scanner.readval;
        MinStackSizeSetExplicity:=true;
      end;


    procedure dir_mode;

    begin
      if not current_module.in_global then
        Message(scan_w_switch_is_global)
      else
        begin
          current_scanner.skipspace;
          current_scanner.readstring;
          if not current_module.mode_switch_allowed and
              not ((m_mac in current_settings.modeswitches) and (pattern='MACPAS')) then
            Message1(scan_e_mode_switch_not_allowed,pattern)
          else if not SetCompileMode(pattern,false) then
            Message1(scan_w_illegal_switch,pattern)
        end;
      current_module.mode_switch_allowed:= false;
    end;

    procedure dir_mmx;
      begin
        do_localswitch(cs_mmx);
      end;

    procedure dir_note;
      begin
        do_message(scan_n_user_defined);
      end;

    procedure dir_notes;
      begin
        do_setverbose('N');
      end;

    procedure dir_objectpath;
      begin
        if not current_module.in_global then
         Message(scan_w_switch_is_global)
        else
          begin
            current_scanner.skipspace;
            current_module.localobjectsearchpath.AddPath(current_scanner.readcomment,false);
          end;
      end;

    procedure dir_openstrings;
      begin
        do_delphiswitch('P');
      end;

    procedure dir_optimization;
      var
        hs : string;
      begin
        current_scanner.skipspace;
        { Support also the ON and OFF as switch }
        hs:=current_scanner.readid;
        if (hs='ON') then
          current_settings.optimizerswitches:=level2optimizerswitches
        else if (hs='OFF') then
          current_settings.optimizerswitches:=[]
        else if (hs='DEFAULT') then
          current_settings.optimizerswitches:=init_settings.optimizerswitches
        else
          begin
            if not UpdateOptimizerStr(hs,current_settings.optimizerswitches) then
              Message1(scan_e_illegal_optimization_specifier,hs);
          end;
      end;

    procedure dir_overflowchecks;
      begin
        do_delphiswitch('Q');
      end;

    procedure dir_packenum;
      var
        hs : string;
      begin
        current_scanner.skipspace;
        if not(c in ['0'..'9']) then
         begin
           hs:=current_scanner.readid;
           if (hs='NORMAL') or (hs='DEFAULT') then
            current_settings.packenum:=4
           else
            Message1(scan_e_illegal_pack_enum, hs);
         end
        else
         begin
           case current_scanner.readval of
            1 : current_settings.packenum:=1;
            2 : current_settings.packenum:=2;
            4 : current_settings.packenum:=4;
           else
            Message1(scan_e_illegal_pack_enum, pattern);
           end;
         end;
      end;

    procedure dir_packrecords;
      var
        hs : string;
      begin
        current_scanner.skipspace;
        if not(c in ['0'..'9']) then
         begin
           hs:=current_scanner.readid;
           { C has the special recordalignmax of C_alignment }
           if (hs='C') then
            current_settings.packrecords:=C_alignment
           else
            if (hs='NORMAL') or (hs='DEFAULT') then
             current_settings.packrecords:=0
           else
            Message1(scan_e_illegal_pack_records,hs);
         end
        else
         begin
           case current_scanner.readval of
             1 : current_settings.packrecords:=1;
             2 : current_settings.packrecords:=2;
             4 : current_settings.packrecords:=4;
             8 : current_settings.packrecords:=8;
            16 : current_settings.packrecords:=16;
            32 : current_settings.packrecords:=32;
           else
            Message1(scan_e_illegal_pack_records,pattern);
           end;
         end;
      end;


    procedure dir_packset;
      var
        hs : string;
      begin
        current_scanner.skipspace;
        if not(c in ['1','2','4','8']) then
         begin
           hs:=current_scanner.readid;
           if (hs='FIXED') or ((hs='DEFAULT') OR (hs='NORMAL')) then
            current_settings.setalloc:=0               {Fixed mode, sets are 4 or 32 bytes}
           else
            Message(scan_e_only_packset);
         end
        else
         begin
           case current_scanner.readval of
            1 : current_settings.setalloc:=1;
            2 : current_settings.setalloc:=2;
            4 : current_settings.setalloc:=4;
            8 : current_settings.setalloc:=8;
           else
            Message(scan_e_only_packset);
           end;
         end;
      end;


    procedure dir_pic;
      begin
        { windows doesn't need/support pic }
        if not(target_info.system in system_windows+system_wince) then
          do_moduleswitch(cs_create_pic)
        else
          message(scan_w_pic_ignored);
      end;

    procedure dir_pop;

    begin
      if localswitchesstackpos < 1 then
        Message(scan_e_too_many_pop);

      if not localswitcheschanged then
        nextlocalswitches:=current_settings.localswitches;

      Dec(localswitchesstackpos);
      nextlocalswitches:= localswitchesstack[localswitchesstackpos];

      localswitcheschanged:=true;
    end;

    procedure dir_profile;
      begin
        do_moduleswitch(cs_profile);
        { defined/undefine FPC_PROFILE }
        if cs_profile in current_settings.moduleswitches then
          def_system_macro('FPC_PROFILE')
        else
          undef_system_macro('FPC_PROFILE');
      end;

    procedure dir_push;

    begin
      if localswitchesstackpos > localswitchesstackmax then
        Message(scan_e_too_many_push);

      if localswitcheschanged then
        begin
          current_settings.localswitches:=nextlocalswitches;
          localswitcheschanged:=false;
        end;

      localswitchesstack[localswitchesstackpos]:= current_settings.localswitches;
      Inc(localswitchesstackpos);
    end;

    procedure dir_rangechecks;
      begin
        do_delphiswitch('R');
      end;

    procedure dir_referenceinfo;
      begin
        do_delphiswitch('Y');
      end;

    procedure dir_resource;
      var
        s : string;
      begin
        current_scanner.skipspace;
        if scanner.c = '''' then
          begin
            s:= current_scanner.readquotedstring;
            current_scanner.readcomment
          end
        else
          s:= trimspace(current_scanner.readcomment);

        { replace * with the name of the main source.
          This should always be defined. }
        if s[1]='*' then
          if Assigned(Current_Module) then
            begin
              delete(S,1,1);
              insert(ChangeFileExt(ExtractFileName(current_module.mainsource^),''),S,1 );
            end;
        s:=FixFileName(s);
        if ExtractFileExt(s)='' then
          s:=ChangeFileExt(s,target_info.resext);
        if target_info.res<>res_none then
          begin
          current_module.flags:=current_module.flags or uf_has_resourcefiles;
          if (target_info.res = res_emxbind) and
                                 not (Current_module.ResourceFiles.Empty) then
            Message(scan_w_only_one_resourcefile_supported)
          else
            current_module.resourcefiles.insert(FixFileName(s));
          end
        else
          Message(scan_e_resourcefiles_not_supported);
      end;

    procedure dir_saturation;
      begin
        do_localswitch(cs_mmx_saturation);
      end;

    procedure dir_savefpuexceptions;
      begin
        do_localswitch(cs_fpu_fwait);
      end;

    procedure dir_setpeflags;
      begin
        if not (target_info.system in (system_windows+system_wince)) then
          Message(scan_w_setpeflags_not_support);
        current_scanner.skipspace;
        peflags:=current_scanner.readval;
        SetPEFlagsSetExplicity:=true;
      end;

    procedure dir_smartlink;
      begin
        do_moduleswitch(cs_create_smart);
      end;

    procedure dir_stackframes;
      begin
        do_delphiswitch('W');
      end;

    procedure dir_static;
      begin
        do_moduleswitch(cs_static_keyword);
      end;

    procedure dir_stop;
      begin
        do_message(scan_f_user_defined);
      end;

{$ifdef powerpc}
    procedure dir_syscall;
      var
        sctype : string;
      begin
        { not needed on amiga/m68k for now, because there's only one }
        { syscall convention (legacy) (KB) }
        { not needed on amiga/powerpc because there's only one }
        { syscall convention (sysv) (KB) }
        if not (target_info.system in [system_powerpc_morphos]) then
          comment (V_Warning,'Syscall directive is useless on this target.');
        current_scanner.skipspace;

        sctype:=current_scanner.readid;
        if (sctype='LEGACY') or (sctype='SYSV') or (sctype='SYSVBASE') or
          (sctype='BASESYSV') or (sctype='R12BASE') then
          syscall_convention:=sctype
        else
          comment (V_Warning,'Invalid Syscall directive ignored.');
      end;
{$endif}

    procedure dir_typedaddress;
      begin
        do_delphiswitch('T');
      end;

    procedure dir_typeinfo;
      begin
        do_delphiswitch('M');
      end;

    procedure dir_unitpath;
      begin
        if not current_module.in_global then
         Message(scan_w_switch_is_global)
        else
          with current_scanner,current_module,localunitsearchpath do
            begin
              skipspace;
              AddPath(path^,readcomment,false);
            end;
      end;

    procedure dir_varstringchecks;
      begin
        do_delphiswitch('V');
      end;

    procedure dir_version;
      var
        major, minor, revision : longint;
        error : integer;
      begin
        if not (target_info.system in [system_i386_os2,system_i386_emx,
                 system_i386_win32,system_i386_netware,system_i386_wdosx,
                 system_i386_netwlibc]) then
          begin
            Message(scan_n_version_not_support);
            exit;
          end;
        if (compile_level<>1) then
          Message(scan_n_only_exe_version)
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
            val(pattern,major,error);
            if (error<>0) or (major > high(word)) or (major < 0) then
              begin
                Message1(scan_w_wrong_version_ignored,pattern);
                exit;
              end;
            if c='.' then
              begin
                current_scanner.readchar;
                current_scanner.readnumber;
                val(pattern,minor,error);
                if (error<>0) or (minor > high(word)) or (minor < 0) then
                  begin
                    Message1(scan_w_wrong_version_ignored,tostr(major)+'.'+pattern);
                    exit;
                  end;
                if (c='.') and
                   (target_info.system in [system_i386_netware,system_i386_netwlibc]) then
                  begin
                     current_scanner.readchar;
                     current_scanner.readnumber;
                     val(pattern,revision,error);
                     if (error<>0) or (revision > high(word)) or (revision < 0) then
                       begin
                          Message1(scan_w_wrong_version_ignored,tostr(revision)+'.'+pattern);
                          exit;
                       end;
                     dllmajor:=word(major);
                     dllminor:=word(minor);
                     dllrevision:=word(revision);
                     dllversion:=tostr(major)+','+tostr(minor)+','+tostr(revision);
                  end
                else
                  begin
                     dllmajor:=word(major);
                     dllminor:=word(minor);
                     dllversion:=tostr(major)+'.'+tostr(minor);
                  end;
              end
            else
              dllversion:=tostr(major);
          end;
      end;

    procedure dir_wait;
      var
        had_info : boolean;
      begin
        had_info:=(status.verbosity and V_Info)<>0;
        { this message should allways appear !! }
        status.verbosity:=status.verbosity or V_Info;
        Message(scan_i_press_enter);
        readln;
        If not(had_info) then
          status.verbosity:=status.verbosity and (not V_Info);
      end;

    { delphi compatible warn directive:
      $warn <identifier> on
      $warn <identifier> off
      not implemented yet
    }
    procedure dir_warn;
      var
        warning_string,state : string;
      begin
        current_scanner.skipspace;
        warning_string:=current_scanner.readid;
        current_scanner.skipspace;
        state:=current_scanner.readid;
        if (upper(state)='ON') then
          begin
          end
        else if (upper(state)='OFF') then
          begin
          end
        else
          Message1(scanner_e_illegal_warn_state,state);
      end;

    procedure dir_warning;
      begin
        do_message(scan_w_user_defined);
      end;

    procedure dir_warnings;
      begin
        do_setverbose('W');
      end;

    procedure dir_writeableconst;
      begin
        do_delphiswitch('J');
      end;

    procedure dir_z1;
      begin
        current_settings.packenum:=1;
      end;

    procedure dir_z2;
      begin
        current_settings.packenum:=2;
      end;

    procedure dir_z4;
      begin
        current_settings.packenum:=4;
      end;

    procedure dir_externalsym;
      begin
      end;

    procedure dir_nodefine;
      begin
      end;

    procedure dir_hppemit;
      begin
      end;

    procedure dir_weakpackageunit;
      begin
      end;

    procedure dir_codealign;
      var
        s : string;
      begin
        current_scanner.skipspace;
        s:=current_scanner.readcomment;
        UpdateAlignmentStr(s,current_settings.alignment);
      end;

    procedure dir_codepage;
      var
         s : string;
      begin
        if not current_module.in_global then
          Message(scan_w_switch_is_global)
        else
          begin
             current_scanner.skipspace;
             s:=current_scanner.readcomment;
             if (upper(s)='UTF8') or (upper(s)='UTF-8') then
               current_settings.sourcecodepage:='utf8'
             else if not(cpavailable(s)) then
               Message1(option_code_page_not_available,s)
             else
               current_settings.sourcecodepage:=s;
          end;
      end;

    procedure dir_coperators;
      begin
        do_moduleswitch(cs_support_c_operators);
      end;


    procedure dir_bitpacking;
      begin
        do_localswitch(cs_bitpacking);
      end;


{****************************************************************************
                         Initialize Directives
****************************************************************************}

    procedure InitScannerDirectives;
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
        AddDirective('ASMMODE',directive_all, @dir_asmmode);
        AddDirective('ASSERTIONS',directive_all, @dir_assertions);
        AddDirective('BOOLEVAL',directive_all, @dir_booleval);
        AddDirective('BITPACKING',directive_all, @dir_bitpacking);
        AddDirective('CALLING',directive_all, @dir_calling);
        AddDirective('CHECKPOINTER',directive_all, @dir_checkpointer);
        AddDirective('CODEALIGN',directive_all, @dir_codealign);
        AddDirective('CODEPAGE',directive_all, @dir_codepage);
        AddDirective('COPERATORS',directive_all, @dir_coperators);
        AddDirective('COPYRIGHT',directive_all, @dir_copyright);
        AddDirective('D',directive_all, @dir_description);
        AddDirective('DEBUGINFO',directive_all, @dir_debuginfo);
        AddDirective('DESCRIPTION',directive_all, @dir_description);
        AddDirective('ERROR',directive_all, @dir_error);
        AddDirective('ERRORC',directive_mac, @dir_error);
        AddDirective('EXTENDEDSYNTAX',directive_all, @dir_extendedsyntax);
        AddDirective('EXTERNALSYM',directive_all, @dir_externalsym);
        AddDirective('FATAL',directive_all, @dir_fatal);
        AddDirective('FPUTYPE',directive_all, @dir_fputype);
        AddDirective('GOTO',directive_all, @dir_goto);
        AddDirective('HINT',directive_all, @dir_hint);
        AddDirective('HINTS',directive_all, @dir_hints);
        AddDirective('HPPEMIT',directive_all, @dir_hppemit);
        AddDirective('IOCHECKS',directive_all, @dir_iochecks);
        AddDirective('IMAGEBASE',directive_all, @dir_imagebase);
        AddDirective('IMPLICITEXCEPTIONS',directive_all, @dir_implicitexceptions);
        AddDirective('INCLUDEPATH',directive_all, @dir_includepath);
        AddDirective('INFO',directive_all, @dir_info);
        AddDirective('INLINE',directive_all, @dir_inline);
        AddDirective('INTERFACES',directive_all, @dir_interfaces);
        AddDirective('L',directive_all, @dir_link);
        AddDirective('LIBEXPORT',directive_mac, @dir_libexport);
        AddDirective('LIBRARYPATH',directive_all, @dir_librarypath);
        AddDirective('LINK',directive_all, @dir_link);
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
        AddDirective('MINSTACKSIZE',directive_all, @dir_minstacksize);
        AddDirective('MMX',directive_all, @dir_mmx);
        AddDirective('MODE',directive_all, @dir_mode);
        AddDirective('NODEFINE',directive_all, @dir_nodefine);
        AddDirective('NOTE',directive_all, @dir_note);
        AddDirective('NOTES',directive_all, @dir_notes);
        AddDirective('OBJECTCHECKS',directive_all, @dir_objectchecks);
        AddDirective('OBJECTPATH',directive_all, @dir_objectpath);
        AddDirective('OPENSTRINGS',directive_all, @dir_openstrings);
        AddDirective('OPTIMIZATION',directive_all, @dir_optimization);
        AddDirective('OVERFLOWCHECKS',directive_all, @dir_overflowchecks);
        AddDirective('PACKENUM',directive_all, @dir_packenum);
        AddDirective('PACKRECORDS',directive_all, @dir_packrecords);
        AddDirective('PACKSET',directive_all, @dir_packset);
        AddDirective('PIC',directive_all, @dir_pic);
        AddDirective('POP',directive_mac, @dir_pop);
        AddDirective('PROFILE',directive_all, @dir_profile);
        AddDirective('PUSH',directive_mac, @dir_push);
        AddDirective('R',directive_all, @dir_resource);
        AddDirective('RANGECHECKS',directive_all, @dir_rangechecks);
        AddDirective('REFERENCEINFO',directive_all, @dir_referenceinfo);
        AddDirective('RESOURCE',directive_all, @dir_resource);
        AddDirective('SATURATION',directive_all, @dir_saturation);
        AddDirective('SAVEFPUEXCEPTIONS',directive_all, @dir_savefpuexceptions);
        AddDirective('SETPEFLAGS', directive_all, @dir_setpeflags);
        AddDirective('SCREENNAME',directive_all, @dir_screenname);
        AddDirective('SMARTLINK',directive_all, @dir_smartlink);
        AddDirective('STACKFRAMES',directive_all, @dir_stackframes);
        AddDirective('STATIC',directive_all, @dir_static);
        AddDirective('STOP',directive_all, @dir_stop);
{$ifdef powerpc}
        AddDirective('SYSCALL',directive_all, @dir_syscall);
{$endif powerpc}
        AddDirective('THREADNAME',directive_all, @dir_threadname);
        AddDirective('TYPEDADDRESS',directive_all, @dir_typedaddress);
        AddDirective('TYPEINFO',directive_all, @dir_typeinfo);
        AddDirective('UNITPATH',directive_all, @dir_unitpath);
        AddDirective('VARSTRINGCHECKS',directive_all, @dir_varstringchecks);
        AddDirective('VERSION',directive_all, @dir_version);
        AddDirective('WAIT',directive_all, @dir_wait);
        AddDirective('WARN',directive_all, @dir_warn);
        AddDirective('WARNING',directive_all, @dir_warning);
        AddDirective('WARNINGS',directive_all, @dir_warnings);
        AddDirective('WEAKPACKAGEUNIT',directive_all, @dir_weakpackageunit);
        AddDirective('WRITEABLECONST',directive_all, @dir_writeableconst);
        AddDirective('Z1',directive_all, @dir_z1);
        AddDirective('Z2',directive_all, @dir_z2);
        AddDirective('Z4',directive_all, @dir_z4);
      end;

begin
  localswitchesstackpos:= 0;
end.
