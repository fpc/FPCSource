{
    $Id$
    Copyright (c) 1998-2000 by Peter Vreman

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
      cutils,
      globtype,globals,systems,
      verbose,comphook,
      scanner,switches,
      fmodule;


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
            exclude(aktmoduleswitches,sw)
           else
            include(aktmoduleswitches,sw);
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
             nextaktlocalswitches:=aktlocalswitches;
           if state='-' then
            exclude(nextaktlocalswitches,sw)
           else
            include(nextaktlocalswitches,sw);
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
            aktalignment.recordalignmax:=4
           else
            if (hs='OFF') then
             aktalignment.recordalignmax:=1
           else
            Message(scan_w_only_pack_records);
         end
        else
         begin
           case current_scanner.readval of
             1 : aktalignment.recordalignmax:=1;
             2 : aktalignment.recordalignmax:=2;
             4 : aktalignment.recordalignmax:=4;
             8 : aktalignment.recordalignmax:=8;
            16 : aktalignment.recordalignmax:=16;
            32 : aktalignment.recordalignmax:=32;
           else
            Message(scan_w_only_pack_records);
           end;
         end;
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
         aktasmmode:=initasmmode
        else
         if not set_asmmode_by_string(s,aktasmmode) then
          Message1(scan_w_unsupported_asmmode_specifier,s);
      end;

{$ifdef m68k}
    procedure dir_appid;
      begin
        if target_info.target<>target_m68k_palmos then
          Message(scan_w_appid_not_support);
        { change description global var in all cases }
        { it not used but in win32 and os2 }
        current_scanner^.skipspace;
        palmos_applicationid:=current_scanner^.readcomment;
      end;

    procedure dir_appname;
      begin
        if target_info.target<>target_m68k_palmos then
          Message(scan_w_appname_not_support);
        { change description global var in all cases }
        { it not used but in win32 and os2 }
        current_scanner^.skipspace;
        palmos_applicationname:=current_scanner^.readcomment;
      end;
{$endif m68k}

    procedure dir_apptype;
      var
         hs : string;
      begin
        if (target_info.target<>target_i386_win32)
                                 and (target_info.target<>target_i386_os2) then
          Message(scan_w_app_type_not_support);
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
             else if (hs='FS') and (target_info.target=target_i386_os2) then
               apptype:=app_fs
             else
               Message1(scan_w_unsupported_app_type,hs);
          end;
      end;


    procedure dir_calling;
      var
         hs : string;
      begin
        current_scanner.skipspace;
        hs:=current_scanner.readid;
        if not SetAktProcCall(hs,false) then
          Message1(parser_w_unknown_proc_directive_ignored,hs);
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
        if not (target_info.target in [target_i386_os2,target_i386_win32,target_i386_netware,target_i386_wdosx]) then
          Message(scan_w_description_not_support);
        { change description global var in all cases }
        { it not used but in win32, os2 and netware }
        current_scanner.skipspace;
        description:=current_scanner.readcomment;
      end;

    procedure dir_screenname; {ad}
      begin
        if target_info.target <> target_i386_netware then
          {Message(scan_w_decription_not_support);}
          comment (V_Warning,'Screenname only supported for target netware');
        current_scanner.skipspace;
        nwscreenname:=current_scanner.readcomment;
      end;

      procedure dir_threadname; {ad}
      begin
        if target_info.target <> target_i386_netware then
          {Message(scan_w_decription_not_support);}
          comment (V_Warning,'Threadname only supported for target netware');
        current_scanner.skipspace;
        nwthreadname:=current_scanner.readcomment;
      end;

      procedure dir_copyright; {ad}
      begin
        if target_info.target <> target_i386_netware then
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
        do_moduleswitch(cs_support_inline);
      end;

    procedure dir_interfaces;
      var
        hs : string;
      begin
        {corba/com/default}
        current_scanner.skipspace;
        hs:=current_scanner.readid;
        if (hs='CORBA') then
          aktinterfacetype:=it_interfacecorba
        else if (hs='COM') then
          aktinterfacetype:=it_interfacecom
        else if (hs='DEFAULT') then
          aktinterfacetype:=initinterfacetype
        else
          Message(scan_e_invalid_interface_type);
      end;

    procedure dir_iochecks;
      begin
        do_delphiswitch('I');
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
        s:=AddExtension(FixFileName(current_scanner.readcomment),target_info.objext);
        current_module.linkotherofiles.add(s,link_allways);
      end;

    procedure dir_linklib;
      type
        tLinkMode=(lm_shared,lm_static);
      var
        s : string;
        quote : char;
        libname,
        linkmodestr : string;
        p : longint;
        linkMode : tLinkMode;
      begin
        current_scanner.skipspace;
        s:=current_scanner.readcomment;
        p:=pos(',',s);
        if p=0 then
         begin
           libname:=TrimSpace(s);
           linkmodeStr:='';
         end
        else
         begin
           libname:=TrimSpace(copy(s,1,p-1));
           linkmodeStr:=Upper(TrimSpace(copy(s,p+1,255)));
         end;
        if (libname='') or (libname='''''') or (libname='""') then
         exit;
        { get linkmode, default is shared linking }
        if linkModeStr='STATIC' then
         linkmode:=lm_static
        else if (LinkModeStr='SHARED') or (LinkModeStr='') then
         linkmode:=lm_shared
        else
         begin
           Comment(V_Error,'Wrong link mode specified: "'+Linkmodestr+'"');
           exit;
         end;
        { create library name }
        if libname[1] in ['''','"'] then
         begin
           quote:=libname[1];
           Delete(libname,1,1);
           p:=pos(quote,libname);
           if p>0 then
            Delete(libname,p,1);
         end;
        { add to the list of libraries to link }
        if linkMode=lm_static then
         current_module.linkOtherStaticLibs.add(FixFileName(libname),link_allways)
        else
         current_module.linkOtherSharedLibs.add(FixFileName(libname),link_allways);
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
                aktmaxfpuregisters:=-1
              else
                Message(scan_e_invalid_maxfpureg_value);
           end
         else
           begin
              l:=current_scanner.readval;
              case l of
                 0..8:
                   aktmaxfpuregisters:=l;
                 else
                   Message(scan_e_invalid_maxfpureg_value);
              end;
           end;
      end;

    procedure dir_memory;
      var
        l : longint;
      begin
        current_scanner.skipspace;
        l:=current_scanner.readval;
        if l>1024 then
         stacksize:=l;
        current_scanner.skipspace;
        if c=',' then
         begin
           current_scanner.readchar;
           current_scanner.skipspace;
           l:=current_scanner.readval;
           if l>1024 then
            heapsize:=l;
         end;
        if c=',' then
         begin
           current_scanner.readchar;
           current_scanner.skipspace;
           l:=current_scanner.readval;
           { Ignore this value, because the limit is set by the OS
             info and shouldn't be changed by the user (PFV) }
         end;
      end;

    procedure dir_message;
      begin
        do_message(scan_i_user_defined);
      end;

    procedure dir_mode;
      begin
        if not current_module.in_global then
         Message(scan_w_switch_is_global)
        else
          begin
            current_scanner.skipspace;
            current_scanner.readstring;
            if not SetCompileMode(pattern,false) then
             Message1(scan_w_illegal_switch,pattern);
          end;
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

    procedure dir_output_format;
      begin
        if not current_module.in_global then
         Message(scan_w_switch_is_global)
        else
          begin
            current_scanner.skipspace;
            if set_target_asm_by_string(current_scanner.readid) then
             aktoutputformat:=target_asm.id
            else
             Message1(scan_w_illegal_switch,pattern);
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
            aktpackenum:=4
           else
            Message(scan_w_only_pack_enum);
         end
        else
         begin
           case current_scanner.readval of
            1 : aktpackenum:=1;
            2 : aktpackenum:=2;
            4 : aktpackenum:=4;
           else
            Message(scan_w_only_pack_enum);
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
           { C has the special recordalignmax of -1 }
           if (hs='C') then
            aktalignment.recordalignmax:=-1
           else
            if (hs='NORMAL') or (hs='DEFAULT') then
             aktalignment.recordalignmax:=2
           else
            Message(scan_w_only_pack_records);
         end
        else
         begin
           case current_scanner.readval of
             1 : aktalignment.recordalignmax:=1;
             2 : aktalignment.recordalignmax:=2;
             4 : aktalignment.recordalignmax:=4;
             8 : aktalignment.recordalignmax:=8;
            16 : aktalignment.recordalignmax:=16;
            32 : aktalignment.recordalignmax:=32;
           else
            Message(scan_w_only_pack_records);
           end;
         end;
      end;

{$ifdef testvarsets}
    procedure dir_packset;
      var
        hs : string;
      begin
        current_scanner.skipspace;
        if not(c in ['1','2','4']) then
         begin
           hs:=current_scanner.readid;
           if (hs='FIXED') or ((hs='DEFAULT') OR (hs='NORMAL')) then
            aktsetalloc:=0               {Fixed mode, sets are 4 or 32 bytes}
           else
            Message(scan_w_only_packset);
         end
        else
         begin
           case current_scanner.readval of
            1 : aktsetalloc:=1;
            2 : aktsetalloc:=2;
            4 : aktsetalloc:=4;
           else
            Message(scan_w_only_packset);
           end;
         end;
      end;
{$ENDIF}

    procedure dir_profile;
      var
        mac : tmacro;
      begin
        do_moduleswitch(cs_profile);
        { defined/undefine FPC_PROFILE }
        mac:=tmacro(current_scanner.macros.search('FPC_PROFILE'));
        if not assigned(mac) then
         begin
           mac:=tmacro.create('FPC_PROFILE');
           current_scanner.macros.insert(mac);
         end;
        mac.defined:=(cs_profile in aktmoduleswitches);
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
        s:=current_scanner.readcomment;
        { replace * with current module name.
          This should always be defined. }
        if s[1]='*' then
          if Assigned(Current_Module) then
            begin
            delete(S,1,1);
            insert(lower(current_module.modulename^),S,1);
            end;
        s:=AddExtension(FixFileName(s),target_info.resext);
        if target_info.res<>res_none then
          if (target_info.res = res_emxbind) and
                                 not (Current_module.ResourceFiles.Empty) then
            Message(scan_w_only_one_resourcefile_supported)
          else
            current_module.resourcefiles.insert(FixFileName(s))
        else
          Message(scan_e_resourcefiles_not_supported);
      end;

    procedure dir_saturation;
      begin
        do_localswitch(cs_mmx_saturation);
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
          begin
            current_scanner.skipspace;
            current_module.localunitsearchpath.AddPath(current_scanner.readcomment,false);
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
        if not (target_info.target in [target_i386_os2,target_i386_win32,target_i386_netware,target_i386_wdosx]) then
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
            valint(pattern,major,error);
            if error<>0 then
              begin
                Message1(scan_w_wrong_version_ignored,pattern);
                exit;
              end;
            if c='.' then
              begin
                current_scanner.readchar;
                current_scanner.readnumber;
                valint(pattern,minor,error);
                if error<>0 then
                  begin
                    Message1(scan_w_wrong_version_ignored,tostr(major)+'.'+pattern);
                    exit;
                  end;
                if (c='.') and
                   (target_info.target = target_i386_netware) then
                  begin
                     current_scanner.readchar;
                     current_scanner.readnumber;
                     valint(pattern,revision,error);
                     if error<>0 then
                       begin
                          Message1(scan_w_wrong_version_ignored,tostr(revision)+'.'+pattern);
                          exit;
                       end;
                     dllmajor:=major;
                     dllminor:=minor;
                     dllrevision:=revision;
                     dllversion:=tostr(major)+','+tostr(minor)+','+tostr(revision);
                  end
                else
                  begin
                     dllmajor:=major;
                     dllminor:=minor;
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

    procedure dir_warning;
      begin
        do_message(scan_w_user_defined);
      end;

    procedure dir_warnings;
      begin
        do_setverbose('W');
      end;

    procedure dir_z1;
      begin
        aktpackenum:=1;
      end;

    procedure dir_z2;
      begin
        aktpackenum:=2;
      end;

    procedure dir_z4;
      begin
        aktpackenum:=4;
      end;


{****************************************************************************
                         Initialize Directives
****************************************************************************}

    procedure InitScannerDirectives;
      begin
        AddDirective('ALIGN',{$ifdef FPCPROCVAR}@{$endif}dir_align);
{$ifdef m68k}
        AddDirective('APPID',{$ifdef FPCPROCVAR}@{$endif}dir_appid);
        AddDirective('APPNAME',{$ifdef FPCPROCVAR}@{$endif}dir_appname);
{$endif m68k}
        AddDirective('APPTYPE',{$ifdef FPCPROCVAR}@{$endif}dir_apptype);
        AddDirective('ASMMODE',{$ifdef FPCPROCVAR}@{$endif}dir_asmmode);
        AddDirective('ASSERTIONS',{$ifdef FPCPROCVAR}@{$endif}dir_assertions);
        AddDirective('BOOLEVAL',{$ifdef FPCPROCVAR}@{$endif}dir_booleval);
        AddDirective('CALLING',{$ifdef FPCPROCVAR}@{$endif}dir_calling);
        AddDirective('COPYRIGHT',{$ifdef FPCPROCVAR}@{$endif}dir_copyright);
        AddDirective('D',{$ifdef FPCPROCVAR}@{$endif}dir_description);
        AddDirective('DEBUGINFO',{$ifdef FPCPROCVAR}@{$endif}dir_debuginfo);
        AddDirective('DESCRIPTION',{$ifdef FPCPROCVAR}@{$endif}dir_description);
        AddDirective('ERROR',{$ifdef FPCPROCVAR}@{$endif}dir_error);
        AddDirective('EXTENDEDSYNTAX',{$ifdef FPCPROCVAR}@{$endif}dir_extendedsyntax);
        AddDirective('FATAL',{$ifdef FPCPROCVAR}@{$endif}dir_fatal);
        AddDirective('GOTO',{$ifdef FPCPROCVAR}@{$endif}dir_goto);
        AddDirective('HINT',{$ifdef FPCPROCVAR}@{$endif}dir_hint);
        AddDirective('HINTS',{$ifdef FPCPROCVAR}@{$endif}dir_hints);
        AddDirective('IOCHECKS',{$ifdef FPCPROCVAR}@{$endif}dir_iochecks);
        AddDirective('INCLUDEPATH',{$ifdef FPCPROCVAR}@{$endif}dir_includepath);
        AddDirective('INFO',{$ifdef FPCPROCVAR}@{$endif}dir_info);
        AddDirective('INLINE',{$ifdef FPCPROCVAR}@{$endif}dir_inline);
        AddDirective('INTERFACES',{$ifdef FPCPROCVAR}@{$endif}dir_interfaces);
        AddDirective('L',{$ifdef FPCPROCVAR}@{$endif}dir_link);
        AddDirective('LIBRARYPATH',{$ifdef FPCPROCVAR}@{$endif}dir_librarypath);
        AddDirective('LINK',{$ifdef FPCPROCVAR}@{$endif}dir_link);
        AddDirective('LINKLIB',{$ifdef FPCPROCVAR}@{$endif}dir_linklib);
        AddDirective('LOCALSYMBOLS',{$ifdef FPCPROCVAR}@{$endif}dir_localsymbols);
        AddDirective('LONGSTRINGS',{$ifdef FPCPROCVAR}@{$endif}dir_longstrings);
        AddDirective('M',{$ifdef FPCPROCVAR}@{$endif}dir_memory);
        AddDirective('MACRO',{$ifdef FPCPROCVAR}@{$endif}dir_macro);
        AddDirective('MAXFPUREGISTERS',{$ifdef FPCPROCVAR}@{$endif}dir_maxfpuregisters);
        AddDirective('MEMORY',{$ifdef FPCPROCVAR}@{$endif}dir_memory);
        AddDirective('MESSAGE',{$ifdef FPCPROCVAR}@{$endif}dir_message);
        AddDirective('MINENUMSIZE',{$ifdef FPCPROCVAR}@{$endif}dir_packenum);
        AddDirective('MMX',{$ifdef FPCPROCVAR}@{$endif}dir_mmx);
        AddDirective('MODE',{$ifdef FPCPROCVAR}@{$endif}dir_mode);
        AddDirective('NOTE',{$ifdef FPCPROCVAR}@{$endif}dir_note);
        AddDirective('NOTES',{$ifdef FPCPROCVAR}@{$endif}dir_notes);
        AddDirective('OBJECTPATH',{$ifdef FPCPROCVAR}@{$endif}dir_objectpath);
        AddDirective('OPENSTRINGS',{$ifdef FPCPROCVAR}@{$endif}dir_openstrings);
        AddDirective('OUTPUT_FORMAT',{$ifdef FPCPROCVAR}@{$endif}dir_output_format);
        AddDirective('OVERFLOWCHECKS',{$ifdef FPCPROCVAR}@{$endif}dir_overflowchecks);
        AddDirective('PACKENUM',{$ifdef FPCPROCVAR}@{$endif}dir_packenum);
        AddDirective('PACKRECORDS',{$ifdef FPCPROCVAR}@{$endif}dir_packrecords);
{$IFDEF TestVarsets}
        AddDirective('PACKSET',{$ifdef FPCPROCVAR}@{$endif}dir_packset);
{$ENDIF}
        AddDirective('PROFILE',{$ifdef FPCPROCVAR}@{$endif}dir_profile);
        AddDirective('R',{$ifdef FPCPROCVAR}@{$endif}dir_resource);
        AddDirective('RANGECHECKS',{$ifdef FPCPROCVAR}@{$endif}dir_rangechecks);
        AddDirective('REFERENCEINFO',{$ifdef FPCPROCVAR}@{$endif}dir_referenceinfo);
        AddDirective('SATURATION',{$ifdef FPCPROCVAR}@{$endif}dir_saturation);
        {ad 18.05.2001: Screen and Threadname for Netware}
        AddDirective('SCREENNAME',{$ifdef FPCPROCVAR}@{$endif}dir_screenname);

        AddDirective('SMARTLINK',{$ifdef FPCPROCVAR}@{$endif}dir_smartlink);
        AddDirective('STACKFRAMES',{$ifdef FPCPROCVAR}@{$endif}dir_stackframes);
        AddDirective('STATIC',{$ifdef FPCPROCVAR}@{$endif}dir_static);
        AddDirective('STOP',{$ifdef FPCPROCVAR}@{$endif}dir_stop);
        {ad 18.05.2001: Screen and Threadname for Netware}
        AddDirective('THREADNAME',{$ifdef FPCPROCVAR}@{$endif}dir_threadname);

        AddDirective('TYPEDADDRESS',{$ifdef FPCPROCVAR}@{$endif}dir_typedaddress);
        AddDirective('TYPEINFO',{$ifdef FPCPROCVAR}@{$endif}dir_typeinfo);
        AddDirective('UNITPATH',{$ifdef FPCPROCVAR}@{$endif}dir_unitpath);
        AddDirective('VARSTRINGCHECKS',{$ifdef FPCPROCVAR}@{$endif}dir_varstringchecks);
        AddDirective('VERSION',{$ifdef FPCPROCVAR}@{$endif}dir_version);
        AddDirective('WAIT',{$ifdef FPCPROCVAR}@{$endif}dir_wait);
        AddDirective('WARNING',{$ifdef FPCPROCVAR}@{$endif}dir_warning);
        AddDirective('WARNINGS',{$ifdef FPCPROCVAR}@{$endif}dir_warnings);
        AddDirective('Z1',{$ifdef FPCPROCVAR}@{$endif}dir_z1);
        AddDirective('Z2',{$ifdef FPCPROCVAR}@{$endif}dir_z2);
        AddDirective('Z4',{$ifdef FPCPROCVAR}@{$endif}dir_z4);
      end;



end.
{
  $Log$
  Revision 1.14  2002-05-16 19:46:44  carl
  + defines.inc -> fpcdefs.inc to avoid conflicts if compiling by hand
  + try to fix temp allocation (still in ifdef)
  + generic constructor calls
  + start of tassembler / tmodulebase class cleanup

  Revision 1.12  2002/04/07 13:34:20  carl
  + wdosx target

  Revision 1.11  2002/04/04 19:06:05  peter
    * removed unused units
    * use tlocation.size in cg.a_*loc*() routines

  Revision 1.10  2001/11/02 23:16:52  peter
    * removed obsolete chainprocsym and test_procsym code

  Revision 1.9  2001/10/23 21:49:42  peter
    * $calling directive and -Cc commandline patch added
      from Pavel Ozerski

  Revision 1.8  2001/09/02 21:18:28  peter
    * split constsym.value in valueord,valueordptr,valueptr. The valueordptr
      is used for holding target platform pointer values. As those can be
      bigger than the source platform.

  Revision 1.7  2001/08/19 11:22:24  peter
    * palmos support from v10 merged

  Revision 1.6  2001/08/07 18:47:13  peter
    * merged netbsd start
    * profile for win32

  Revision 1.5  2001/07/01 20:16:16  peter
    * alignmentinfo record added
    * -Oa argument supports more alignment settings that can be specified
      per type: PROC,LOOP,VARMIN,VARMAX,CONSTMIN,CONSTMAX,RECORDMIN
      RECORDMAX,LOCALMIN,LOCALMAX. It is possible to set the mimimum
      required alignment and the maximum usefull alignment. The final
      alignment will be choosen per variable size dependent on these
      settings

  Revision 1.4  2001/06/03 20:20:27  peter
    * Align directive supports also values to be Kylix compatible. It's
      strange because the help of kylix still shows only On and Off as
      possible values, so still support those. On means 4 bytes and Off
      means 1 byte alignment.

  Revision 1.3  2001/05/30 21:35:49  peter
    * netware patches for copyright, screenname, threadname directives

  Revision 1.2  2001/04/18 22:01:58  peter
    * registration of targets and assemblers

  Revision 1.1  2001/04/13 18:00:36  peter
    * easier registration of directives

  Revision 1.20  2001/04/13 01:22:13  peter
    * symtable change to classes
    * range check generation and errors fixed, make cycle DEBUG=1 works
    * memory leaks fixed

  Revision 1.19  2001/03/13 18:45:07  peter
    * fixed some memory leaks

  Revision 1.18  2001/02/20 21:41:18  peter
    * new fixfilename, findfile for unix. Look first for lowercase, then
      NormalCase and last for UPPERCASE names.

  Revision 1.17  2001/01/20 18:32:52  hajny
    + APPTYPE support under OS/2, app_fs, GetEnvPChar for OS/2

  Revision 1.16  2001/01/13 00:09:21  peter
    * made Pavel O. happy ;)

  Revision 1.15  2000/12/25 00:07:28  peter
    + new tlinkedlist class (merge of old tstringqueue,tcontainer and
      tlinkedlist objects)

  Revision 1.14  2000/12/24 12:24:38  peter
    * moved preprocessfile into a conditional

  Revision 1.13  2000/12/12 19:48:52  peter
    * fixed lost char after $I directive (merged)

  Revision 1.12  2000/11/12 22:17:47  peter
    * some realname updates for messages

  Revision 1.11  2000/11/04 14:25:21  florian
    + merged Attila's changes for interfaces, not tested yet

  Revision 1.10  2000/10/31 22:02:51  peter
    * symtable splitted, no real code changes

  Revision 1.9  2000/09/26 10:50:41  jonas
    * initmodeswitches is changed is you change the compiler mode from the
      command line (the -S<x> switches didn't work anymore for changing the
      compiler mode) (merged from fixes branch)

  Revision 1.8  2000/09/24 21:33:47  peter
    * message updates merges

  Revision 1.7  2000/09/24 15:06:27  peter
    * use defines.inc

  Revision 1.6  2000/09/11 17:00:23  florian
    + first implementation of Netware Module support, thanks to
      Armin Diehl (diehl@nordrhein.de) for providing the patches

  Revision 1.5  2000/09/10 21:18:15  peter
    * macro warning (merged)

  Revision 1.4  2000/08/12 15:30:44  peter
    * IDE patch for stream reading (merged)

  Revision 1.3  2000/08/08 19:28:57  peter
    * memdebug/memory patches (merged)
    * only once illegal directive (merged)

  Revision 1.2  2000/07/13 11:32:49  michael
  + removed logs

}
