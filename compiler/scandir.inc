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
const
   directivelen=15;
type
   directivestr=string[directivelen];
   tdirectivetoken=(
     _DIR_NONE,
     _DIR_ALIGN,_DIR_APPTYPE,_DIR_ASMMODE,_DIR_ASSERTIONS,
     _DIR_BOOLEVAL,
     _DIR_D,_DIR_DEBUGINFO,_DIR_DEFINE,_DIR_DESCRIPTION,
     _DIR_ELSE,_DIR_ENDIF,_DIR_ERROR,_DIR_EXTENDEDSYNTAX,
     _DIR_FATAL,
     _DIR_GOTO,
     _DIR_HINT,_DIR_HINTS,
     _DIR_I,_DIR_I386_ATT,_DIR_I386_DIRECT,_DIR_I386_INTEL,_DIR_IOCHECKS,
       _DIR_IF,_DIR_IFDEF,_DIR_IFNDEF,_DIR_IFOPT,_DIR_INCLUDE,_DIR_INCLUDEPATH,
       _DIR_INFO,_DIR_INLINE,
     _DIR_L,_DIR_LIBRARYPATH,_DIR_LINK,_DIR_LINKLIB,_DIR_LOCALSYMBOLS,
       _DIR_LONGSTRINGS,
     _DIR_M,_DIR_MACRO,_DIR_MAXFPUREGISTERS,_DIR_MEMORY,_DIR_MESSAGE,_DIR_MINENUMSIZE,_DIR_MMX,_DIR_MODE,
     _DIR_NOTE,_DIR_NOTES,
     _DIR_OBJECTPATH,_DIR_OPENSTRINGS,_DIR_OUTPUT_FORMAT,_DIR_OVERFLOWCHECKS,
     _DIR_PACKENUM,_DIR_PACKRECORDS,
     {$IFDEF Testvarsets}
      _DIR_PACKSET,
     {$ENDIF}
     _DIR_R,_DIR_RANGECHECKS,_DIR_REFERENCEINFO,
     _DIR_SATURATION,_DIR_SMARTLINK,_DIR_STACKFRAMES,_DIR_STATIC,_DIR_STOP,
     _DIR_TYPEDADDRESS,_DIR_TYPEINFO,
     _DIR_UNDEF,_DIR_UNITPATH,
     _DIR_VARSTRINGCHECKS,_DIR_VERSION,
     _DIR_WAIT,_DIR_WARNING,_DIR_WARNINGS,
     _DIR_Z1,_DIR_Z2,_DIR_Z4
     );
const
   firstdirective=_DIR_NONE;
   lastdirective=_DIR_Z4;
   directive:array[tdirectivetoken] of directivestr=(
     {12345678901234567890 (To determine longest string.)}
     '',
     'ALIGN',
     'APPTYPE',
     'ASMMODE',
     'ASSERTIONS',
     'BOOLEVAL',
     'D',
     'DEBUGINFO',
     'DEFINE',
     'DESCRIPTION',
     'ELSE',
     'ENDIF',
     'ERROR',
     'EXTENDEDSYNTAX',
     'FATAL',
     'GOTO',
     'HINT',
     'HINTS',
     'I',
     {12345678901234567890 (To determine longest string.)}
     'I386_ATT',
     'I386_DIRECT',
     'I386_INTEL',
     'IOCHECKS',
     'IF',
     'IFDEF',
     'IFNDEF',
     'IFOPT',
     'INCLUDE',
     'INCLUDEPATH',
     'INFO',
     'INLINE',
     'L',
     'LIBRARYPATH',
     'LINK',
     'LINKLIB',
     'LOCALSYMBOLS',
     'LONGSTRINGS',
     'M',
     {12345678901234567890 (To determine longest string.)}
     'MACRO',
     'MAXFPUREGISTERS',
     'MEMORY',
     'MESSAGE',
     'MINENUMSIZE',
     'MMX',
     'MODE',
     'NOTE',
     'NOTES',
     'OBJECTPATH',
     'OPENSTRINGS',
     'OUTPUT_FORMAT',
     'OVERFLOWCHECKS',
     'PACKENUM',
     'PACKRECORDS',
     {$IFDEF testvarsets}
     'PACKSET',
     {$ENDIF}
     'R',
     'RANGECHECKS',
     'REFERENCEINFO',
     'SATURATION',
     'SMARTLINK',
     {12345678901234567890 (To determine longest string.)}
     'STACKFRAMES',
     'STATIC',
     'STOP',
     'TYPEDADDRESS',
     'TYPEINFO',
     'UNDEF',
     'UNITPATH',
     'VARSTRINGCHECKS',
     'VERSION',
     'WAIT',
     'WARNING',
     'WARNINGS',
     'Z1',
     'Z2',
     'Z4'
     );



    function Get_Directive(const hs:string):tdirectivetoken;
      var
        i : tdirectivetoken;
      begin
        for i:=firstdirective to lastdirective do
         if directive[i]=hs then
          begin
            Get_Directive:=i;
            exit;
          end;
        Get_Directive:=_DIR_NONE;
     end;


  {-------------------------------------------
           IF Conditional Handling
  -------------------------------------------}

    var
      preprocpat    : string;
      preproc_token : ttoken;

    procedure preproc_consume(t : ttoken);
      begin
        if t<>preproc_token then
         Message(scan_e_preproc_syntax_error);
        preproc_token:=current_scanner^.readpreproc;
      end;

    function read_expr : string;forward;

    function read_factor : string;
      var
         hs : string;
         mac : pmacrosym;
         len : byte;
      begin
         if preproc_token=_ID then
           begin
              if preprocpat='NOT' then
                begin
                   preproc_consume(_ID);
                   hs:=read_expr;
                   if hs='0' then
                     read_factor:='1'
                   else
                     read_factor:='0';
                end
              else
                begin
                   mac:=pmacrosym(macros^.search(hs));
                   hs:=preprocpat;
                   preproc_consume(_ID);
                   if assigned(mac) then
                     begin
                        if mac^.defined and assigned(mac^.buftext) then
                          begin
                             if mac^.buflen>255 then
                               begin
                                  len:=255;
                                  Message(scan_w_macro_cut_after_255_chars);
                               end
                             else
                               len:=mac^.buflen;
                             {$ifndef TP}
                               {$ifopt H+}
                                 setlength(hs,len);
                               {$else}
                                 hs[0]:=char(len);
                               {$endif}
                             {$else}
                               hs[0]:=char(len);
                             {$endif}
                             move(mac^.buftext^,hs[1],len);
                          end
                        else
                          read_factor:='';
                     end
                   else
                     read_factor:=hs;
                end
           end
         else if preproc_token=_LKLAMMER then
           begin
              preproc_consume(_LKLAMMER);
              read_factor:=read_expr;
              preproc_consume(_RKLAMMER);
           end
         else
           Message(scan_e_error_in_preproc_expr);
      end;


    function read_term : string;
      var
         hs1,hs2 : string;
      begin
         hs1:=read_factor;
         while true do
           begin
              if (preproc_token=_ID) then
                begin
                   if preprocpat='AND' then
                     begin
                        preproc_consume(_ID);
                        hs2:=read_factor;
                        if (hs1<>'0') and (hs2<>'0') then
                          hs1:='1';
                     end
                   else
                     break;
                end
              else
                break;
           end;
         read_term:=hs1;
      end;


    function read_simple_expr : string;
      var
         hs1,hs2 : string;
      begin
         hs1:=read_term;
         while true do
           begin
              if (preproc_token=_ID) then
                begin
                   if preprocpat='OR' then
                     begin
                        preproc_consume(_ID);
                        hs2:=read_term;
                        if (hs1<>'0') or (hs2<>'0') then
                          hs1:='1';
                     end
                   else
                     break;
                end
              else
                break;
           end;
         read_simple_expr:=hs1;
      end;


    function read_expr : string;
      var
         hs1,hs2 : string;
         b : boolean;
         t : ttoken;
         w : integer;
         l1,l2 : longint;
      begin
         hs1:=read_simple_expr;
         t:=preproc_token;
         if not(t in [_EQUAL,_UNEQUAL,_LT,_GT,_LTE,_GTE]) then
           begin
              read_expr:=hs1;
              exit;
           end;
         preproc_consume(t);
         hs2:=read_simple_expr;
         if is_number(hs1) and is_number(hs2) then
           begin
              valint(hs1,l1,w);
              valint(hs2,l2,w);
              case t of
                 _EQUAL : b:=l1=l2;
               _UNEQUAL : b:=l1<>l2;
                    _LT : b:=l1<l2;
                    _GT : b:=l1>l2;
                   _GTE : b:=l1>=l2;
                   _LTE : b:=l1<=l2;
              end;
           end
         else
           begin
              case t of
                 _EQUAL : b:=hs1=hs2;
               _UNEQUAL : b:=hs1<>hs2;
                    _LT : b:=hs1<hs2;
                    _GT : b:=hs1>hs2;
                   _GTE : b:=hs1>=hs2;
                   _LTE : b:=hs1<=hs2;
              end;
           end;
         if b then
           read_expr:='1'
         else
           read_expr:='0';
     end;

  {-------------------------------------------
                Directives
  -------------------------------------------}

    function is_conditional(t:tdirectivetoken):boolean;
      begin
        is_conditional:=(t in [_DIR_ENDIF,_DIR_IFDEF,_DIR_IFNDEF,_DIR_IFOPT,_DIR_IF,_DIR_ELSE]);
      end;


    procedure dir_conditional(t:tdirectivetoken);
      var
        hs    : string;
        mac   : pmacrosym;
        found : boolean;
        state : char;
        oldaktfilepos : tfileposinfo;
      begin
        oldaktfilepos:=aktfilepos;
        while true do
         begin
           current_scanner^.gettokenpos;
           case t of
   _DIR_ENDIF : begin
                  current_scanner^.poppreprocstack;
                end;
    _DIR_ELSE : begin
                  current_scanner^.elsepreprocstack;
                end;
   _DIR_IFDEF : begin
                  current_scanner^.skipspace;
                  hs:=current_scanner^.readid;
                  mac:=pmacrosym(macros^.search(hs));
                  if assigned(mac) then
                    mac^.is_used:=true;
                  current_scanner^.addpreprocstack(pp_ifdef,assigned(mac) and mac^.defined,hs,scan_c_ifdef_found);
                end;
   _DIR_IFOPT : begin
                  current_scanner^.skipspace;
                  hs:=current_scanner^.readid;
                  if (length(hs)>1) then
                   Message(scan_w_illegal_switch)
                  else
                   begin
                     state:=current_scanner^.ReadState;
                     if state in ['-','+'] then
                      found:=CheckSwitch(hs[1],state);
                   end;
                  current_scanner^.addpreprocstack(pp_ifopt,found,hs,scan_c_ifopt_found);
                end;
      _DIR_IF : begin
                  current_scanner^.skipspace;
                  { start preproc expression scanner }
                  preproc_token:=current_scanner^.readpreproc;
                  hs:=read_expr;
                  current_scanner^.addpreprocstack(pp_if,hs<>'0',hs,scan_c_if_found);
                end;
  _DIR_IFNDEF : begin
                  current_scanner^.skipspace;
                  hs:=current_scanner^.readid;
                  mac:=pmacrosym(macros^.search(hs));
                  if assigned(mac) then
                    mac^.is_used:=true;
                  current_scanner^.addpreprocstack(pp_ifndef,not(assigned(mac) and mac^.defined),hs,scan_c_ifndef_found);
                end;
           end;
         { accept the text ? }
           if (current_scanner^.preprocstack=nil) or current_scanner^.preprocstack^.accept then
            break
           else
            begin
              current_scanner^.gettokenpos;
              Message(scan_c_skipping_until);
              repeat
                current_scanner^.skipuntildirective;
                t:=Get_Directive(current_scanner^.readid);
              until is_conditional(t);
              current_scanner^.gettokenpos;
              Message1(scan_d_handling_switch,'$'+directive[t]);
            end;
         end;
        aktfilepos:=oldaktfilepos;
      end;


    procedure dir_define(t:tdirectivetoken);
      var
        hs  : string;
        bracketcount : longint;
        mac : pmacrosym;
        macropos : longint;
        macrobuffer : pmacrobuffer;
      begin
        current_scanner^.skipspace;
        hs:=current_scanner^.readid;
        mac:=pmacrosym(macros^.search(hs));
        if not assigned(mac) then
          begin
            mac:=new(pmacrosym,init(hs));
            mac^.defined:=true;
            Message1(parser_m_macro_defined,mac^.name);
            macros^.insert(mac);
          end
        else
          begin
            Message1(parser_m_macro_defined,mac^.name);
            mac^.defined:=true;
          { delete old definition }
            if assigned(mac^.buftext) then
             begin
               freemem(mac^.buftext,mac^.buflen);
               mac^.buftext:=nil;
             end;
          end;
        mac^.is_used:=true;
        if (cs_support_macro in aktmoduleswitches) then
          begin
          { key words are never substituted }
             if is_keyword(hs) then
              Message(scan_e_keyword_cant_be_a_macro);
           { !!!!!! handle macro params, need we this? }
             current_scanner^.skipspace;
           { may be a macro? }
             if c=':' then
               begin
                  current_scanner^.readchar;
                  if c='=' then
                    begin
                       new(macrobuffer);
                       macropos:=0;
                       { parse macro, brackets are counted so it's possible
                         to have a $ifdef etc. in the macro }
                       bracketcount:=0;
                       repeat
                         current_scanner^.readchar;
                         case c of
                           '}' :
                             if (bracketcount=0) then
                              break
                             else
                              dec(bracketcount);
                           '{' :
                             inc(bracketcount);
                           #26 :
                             current_scanner^.end_of_file;
                         end;
                         macrobuffer^[macropos]:=c;
                         inc(macropos);
                         if macropos>maxmacrolen then
                          Message(scan_f_macro_buffer_overflow);
                       until false;
                       { free buffer of macro ?}
                       if assigned(mac^.buftext) then
                         freemem(mac^.buftext,mac^.buflen);
                       { get new mem }
                       getmem(mac^.buftext,macropos);
                       mac^.buflen:=macropos;
                       { copy the text }
                       move(macrobuffer^,mac^.buftext^,macropos);
                       dispose(macrobuffer);
                    end;
               end;
          end;
      end;


    procedure dir_undef(t:tdirectivetoken);
      var
        hs  : string;
        mac : pmacrosym;
      begin
        current_scanner^.skipspace;
        hs:=current_scanner^.readid;
        mac:=pmacrosym(macros^.search(hs));
        if not assigned(mac) then
          begin
             mac:=new(pmacrosym,init(hs));
             Message1(parser_m_macro_undefined,mac^.name);
             mac^.defined:=false;
             macros^.insert(mac);
          end
        else
          begin
             Message1(parser_m_macro_undefined,mac^.name);
             mac^.defined:=false;
             { delete old definition }
             if assigned(mac^.buftext) then
               begin
                  freemem(mac^.buftext,mac^.buflen);
                  mac^.buftext:=nil;
               end;
          end;
        mac^.is_used:=true;
      end;


    procedure dir_message(t:tdirectivetoken);
      var
        w   : longint;
      begin
        case t of
       _DIR_STOP,
      _DIR_FATAL : w:=scan_f_user_defined;
      _DIR_ERROR : w:=scan_e_user_defined;
    _DIR_WARNING : w:=scan_w_user_defined;
       _DIR_HINT : w:=scan_h_user_defined;
       _DIR_NOTE : w:=scan_n_user_defined;
    _DIR_MESSAGE,
       _DIR_INFO : w:=scan_i_user_defined;
        end;
        current_scanner^.skipspace;
        Message1(w,current_scanner^.readcomment);
      end;


    procedure dir_moduleswitch(t:tdirectivetoken);
      var
        sw : tmoduleswitch;
        state : char;
      begin
        sw:=cs_modulenone;
        case t of
          _DIR_GOTO      : sw:=cs_support_goto;
          _DIR_MACRO     : sw:=cs_support_macro;
          _DIR_INLINE    : sw:=cs_support_inline;
          _DIR_SMARTLINK : sw:=cs_create_smart;
          _DIR_STATIC    : sw:=cs_static_keyword;
        end;
        state:=current_scanner^.readstate;
        if (sw<>cs_modulenone) and (state in ['-','+']) then
         begin
           if state='-' then
            aktmoduleswitches:=aktmoduleswitches-[sw]
           else
            aktmoduleswitches:=aktmoduleswitches+[sw];
         end;
      end;


    procedure dir_localswitch(t:tdirectivetoken);
      var
        sw : tlocalswitch;
        state : char;
      begin
        sw:=cs_localnone;
{$ifdef SUPPORT_MMX}
        case t of
          _DIR_MMX : sw:=cs_mmx;
          _DIR_SATURATION : sw:=cs_mmx_saturation;
        end;
{$endif}
        state:=current_scanner^.readstate;
        if (sw<>cs_localnone) and (state in ['-','+']) then
         begin
           if not localswitcheschanged then
             nextaktlocalswitches:=aktlocalswitches;
           if state='-' then
            nextaktlocalswitches:=nextaktlocalswitches-[sw]
           else
            nextaktlocalswitches:=nextaktlocalswitches+[sw];
           localswitcheschanged:=true;
         end;
      end;


    procedure dir_include(t:tdirectivetoken);
      var
        hs    : string;
        path  : dirstr;
        name  : namestr;
        ext   : extstr;
        hp    : pinputfile;
        i     : longint;
        found : boolean;
      begin
        current_scanner^.skipspace;
        hs:=current_scanner^.readcomment;
        i:=length(hs);
        while (i>0) and (hs[i]=' ') do
         dec(i);
        Delete(hs,i+1,length(hs)-i);
        if hs='' then
         exit;
        if (hs[1]='%') then
         begin
         { case insensitive }
           hs:=upper(hs);
         { remove %'s }
           Delete(hs,1,1);
           if hs[length(hs)]='%' then
            Delete(hs,length(hs),1);
         { save old }
           path:=hs;
         { first check for internal macros }
           if hs='TIME' then
            hs:=gettimestr
           else
            if hs='DATE' then
             hs:=getdatestr
           else
            if hs='FILE' then
             hs:=current_module^.sourcefiles^.get_file_name(aktfilepos.fileindex)
           else
            if hs='LINE' then
             hs:=tostr(aktfilepos.line)
           else
            if hs='FPCVERSION' then
             hs:=version_string
           else
            if hs='FPCTARGET' then
             hs:=target_cpu_string
           else
             hs:=getenv(hs);
           if hs='' then
            Message1(scan_w_include_env_not_found,path);
           { make it a stringconst }
           hs:=''''+hs+'''';
           current_scanner^.insertmacro(path,@hs[1],length(hs));
         end
        else
         begin
           hs:=FixFileName(hs);
           fsplit(hs,path,name,ext);
         { look for the include file
            1. specified path,path of current inputfile,current dir
            2. local includepath
            3. global includepath }
           found:=false;
           if path<>'' then
             path:=path+';';
           path:=FindFile(name+ext,path+current_scanner^.inputfile^.path^+';.'+DirSep,found);
           if (not found) then
            path:=current_module^.localincludesearchpath.FindFile(name+ext,found);
           if (not found) then
            path:=includesearchpath.FindFile(name+ext,found);
         { shutdown current file }
           current_scanner^.tempcloseinputfile;
         { load new file }
           hp:=new(pinputfile,init(path+name+ext));
           current_scanner^.addfile(hp);
           if not current_scanner^.openinputfile then
            Message1(scan_f_cannot_open_includefile,hs);
           Message1(scan_t_start_include_file,current_scanner^.inputfile^.path^+current_scanner^.inputfile^.name^);
           current_scanner^.reload;
         { process first read char }
           case c of
            #26 : current_scanner^.reload;
            #10,
            #13 : current_scanner^.linebreak;
           end;
         { register for refs }
           current_module^.sourcefiles^.register_file(hp);
         end;
      end;


    procedure dir_description(t:tdirectivetoken);
      begin
        if not (target_info.target in [target_i386_os2,target_i386_win32]) then
          Message(scan_w_decription_not_support);
        { change description global var in all cases }
        { it not used but in win32 and os2 }
        current_scanner^.skipspace;
        description:=current_scanner^.readcomment;
      end;


    procedure dir_version(t:tdirectivetoken);
      var
        major, minor : longint;
        error : integer;
      begin
        if not (target_info.target in [target_i386_os2,target_i386_win32]) then
          begin
            Message(scan_n_version_not_support);
            exit;
          end;
        if (compile_level<>1) then
          Message(scan_n_only_exe_version)
        else
          begin
            { change description global var in all cases }
            { it not used but in win32 and os2 }
            current_scanner^.skipspace;
            { we should only accept Major.Minor format }
            current_scanner^.readnumber;
            major:=0;
            minor:=0;
            valint(pattern,major,error);
            if error<>0 then
              begin
                Message1(scan_w_wrong_version_ignored,pattern);
                exit;
              end;
            if c='.' then
              begin
                current_scanner^.readchar;
                current_scanner^.readnumber;
                valint(pattern,minor,error);
                if error<>0 then
                  begin
                    Message1(scan_w_wrong_version_ignored,tostr(major)+'.'+pattern);
                    exit;
                  end;
                dllmajor:=major;
                dllminor:=minor;
                dllversion:=tostr(major)+'.'+tostr(minor);
              end
            else
              dllversion:=tostr(major);
          end;
      end;


    procedure dir_linkobject(t:tdirectivetoken);
      var
        s : string;
      begin
        current_scanner^.skipspace;
        s:=AddExtension(FixFileName(current_scanner^.readcomment),target_info.objext);
      {$IFDEF NEWST}
        current_module^.linkotherofiles.
         insert(new(Plinkitem,init(s,link_allways)));
      {$ELSE}
        current_module^.linkotherofiles.
         insert(s,link_allways);
      {$ENDIF NEWST}
      end;


    procedure dir_resource(t:tdirectivetoken);
      var
        s : string;
      begin
        current_scanner^.skipspace;
        s:=current_scanner^.readcomment;
        { replace * with current module name.
          This should always be defined. }
        if s[1]='*' then
          if Assigned(Current_Module) then
            begin
            delete(S,1,1);
            insert(lower(current_module^.modulename^),S,1);
            end;
        s:=AddExtension(FixFileName(s),target_info.resext);
        if target_info.res<>res_none then
          if (target_info.res = res_i386_emx) and
                                 not (Current_Module^.ResourceFiles.Empty) then
            Message(scan_w_only_one_resourcefile_supported)
          else
            current_module^.resourcefiles.insert(FixFileName(s))
        else
          Message(scan_e_resourcefiles_not_supported);
      end;

{$ifndef PAVEL_LINKLIB}
    procedure dir_linklib(t:tdirectivetoken);
      var
        s : string;
        quote : char;
      begin
        current_scanner^.skipspace;
        { This way spaces are also allowed in library names
          if quoted PM }
        if (c='''') or (c='"') then
          begin
            quote:=c;
            current_scanner^.readchar;
            s:=current_scanner^.readcomment;
            if pos(quote,s)>0 then
              s:=copy(s,1,pos(quote,s)-1);
          end
        else
          begin
            current_scanner^.readstring;
            s:=orgpattern;
            if c='.' then
              begin
                s:=s+'.';
                current_scanner^.readchar;
                current_scanner^.readstring;
                s:=s+orgpattern;
              end;
          end;
      {$IFDEF NEWST}
        current_module^.linkOtherSharedLibs.
         insert(new(Plinkitem,init(s,link_allways)));
      {$ELSE}
        current_module^.linkOtherSharedLibs.
         insert(s,link_allways);
      {$ENDIF}
      end;
{$else PAVEL_LINKLIB}
    procedure dir_linklib(t:tdirectivetoken);
      var
       s:string;
       libname,linkmodeStr:string;
       p:longint;
      type
       tLinkMode=(lm_dynamic,lm_static);
      var
       linkMode:tLinkMode;
      function ExtractLinkMode:tLinkMode;
       var
        p:longint;
       begin
        p:=pos(',',linkmodeStr);
        if p>0 then
         linkmodeStr:=copy(linkmodeStr,1,pred(p));
        for p:=1 to length(linkmodeStr)do
         linkmodeStr[p]:=upcase(linkmodeStr[p]);
        if linkmodeStr='STATIC' then
         ExtractLinkMode:=lm_static
        else
         ExtractLinkMode:=lm_dynamic
       end;
      procedure MangleLibName(mode:tLinkMode);
       begin
        if (libname[1]='''')and(libname[length(libname)]='''')then
         begin
          delete(libname,1,1);
          delete(libname,length(libname),1);
         end
        else
         begin
          libname:=target_os.libprefix+libname;
          case mode of
           lm_static:
            libname:=AddExtension(FixFileName(libname),target_os.staticlibext);
           lm_dynamic:
            libname:=AddExtension(FixFileName(libname),target_os.sharedlibext);
          end;
         end;
       end;
      begin
       current_scanner^.skipspace;
       s:=current_scanner^.readcomment;
       p:=pos(',',s);
       if p=0 then
        begin
         libname:=s;
         linkmodeStr:=''
        end
       else
        begin
         libname:=copy(s,1,pred(p));
         linkmodeStr:=copy(s,succ(p),255);
        end;
       if(libname='')or(libname='''''')then
        exit;
       linkMode:=ExtractLinkMode;
       MangleLibName(linkMode);
       if linkMode=lm_static then
{$IFDEF NEWST}
        current_module^.linkOtherStaticLibs.
         insert(new(Plinkitem,init(FixFileName(libname),link_allways)))
{$ELSE}
        current_module^.linkOtherStaticLibs.
         insert(FixFileName(libname),link_allways)
{$ENDIF}
       else
{$IFDEF NEWST}
        current_module^.linkOtherSharedLibs.
         insert(new(Plinkitem,init(FixFileName(libname),link_allways)));
{$ELSE}
        current_module^.linkOtherSharedLibs.
         insert(FixFileName(libname),link_allways);
{$ENDIF}
      end;


{$endif PAVEL_LINKLIB}


    procedure dir_outputformat(t:tdirectivetoken);
      begin
        if not current_module^.in_global then
         Message(scan_w_switch_is_global)
        else
          begin
            current_scanner^.skipspace;
            if set_string_asm(current_scanner^.readid) then
             aktoutputformat:=target_asm.id
            else
             Message(scan_w_illegal_switch);
          end;
      end;


    procedure dir_unitpath(t:tdirectivetoken);
      begin
        if not current_module^.in_global then
         Message(scan_w_switch_is_global)
        else
          begin
            current_scanner^.skipspace;
            current_module^.localunitsearchpath.AddPath(current_scanner^.readcomment,false);
          end;
      end;


    procedure dir_includepath(t:tdirectivetoken);
      begin
        if not current_module^.in_global then
         Message(scan_w_switch_is_global)
        else
          begin
            current_scanner^.skipspace;
            current_module^.localincludesearchpath.AddPath(current_scanner^.readcomment,false);
          end;
      end;


    procedure dir_librarypath(t:tdirectivetoken);
      begin
        if not current_module^.in_global then
         Message(scan_w_switch_is_global)
        else
          begin
            current_scanner^.skipspace;
            current_module^.locallibrarysearchpath.AddPath(current_scanner^.readcomment,false);
          end;
      end;


    procedure dir_objectpath(t:tdirectivetoken);
      begin
        if not current_module^.in_global then
         Message(scan_w_switch_is_global)
        else
          begin
            current_scanner^.skipspace;
            current_module^.localobjectsearchpath.AddPath(current_scanner^.readcomment,false);
          end;
      end;


    procedure dir_mode(t:tdirectivetoken);
      begin
        if not current_module^.in_global then
         Message(scan_w_switch_is_global)
        else
          begin
            current_scanner^.skipspace;
            current_scanner^.readstring;
            if pattern='DEFAULT' then
             aktmodeswitches:=initmodeswitches
            else
             if pattern='DELPHI' then
              aktmodeswitches:=delphimodeswitches
            else
             if pattern='TP' then
              aktmodeswitches:=tpmodeswitches
            else
             if pattern='FPC' then
              aktmodeswitches:=fpcmodeswitches
            else
             if pattern='OBJFPC' then
              aktmodeswitches:=objfpcmodeswitches
            else
             if pattern='GPC' then
              aktmodeswitches:=gpcmodeswitches
            else
             Message(scan_w_illegal_switch);
          end;
      end;


    procedure dir_packrecords(t:tdirectivetoken);
      var
        hs : string;
      begin
        current_scanner^.skipspace;
        if not(c in ['0'..'9']) then
         begin
           hs:=current_scanner^.readid;
           if (hs='C') then
            aktpackrecords:=packrecord_C
           else
            if (hs='NORMAL') or (hs='DEFAULT') then
             aktpackrecords:=packrecord_2
           else
            Message(scan_w_only_pack_records);
         end
        else
         begin
           case current_scanner^.readval of
             1 : aktpackrecords:=packrecord_1;
             2 : aktpackrecords:=packrecord_2;
             4 : aktpackrecords:=packrecord_4;
             8 : aktpackrecords:=packrecord_8;
            16 : aktpackrecords:=packrecord_16;
            32 : aktpackrecords:=packrecord_32;
           else
            Message(scan_w_only_pack_records);
           end;
         end;
      end;

    procedure dir_maxfpuregisters(t:tdirectivetoken);

      var
         l : longint;
         hs : string;

      begin
         current_scanner^.skipspace;
         if not(c in ['0'..'9']) then
           begin
              hs:=current_scanner^.readid;
              if (hs='NORMAL') or (hs='DEFAULT') then
                aktmaxfpuregisters:=-1
              else
                Message(scan_e_invalid_maxfpureg_value);
           end
         else
           begin
              l:=current_scanner^.readval;
              case l of
                 0..8:
                   aktmaxfpuregisters:=l;
                 else
                   Message(scan_e_invalid_maxfpureg_value);
              end;
           end;
      end;


    procedure dir_packenum(t:tdirectivetoken);
      var
        hs : string;
      begin
        if t in [_DIR_Z1,_DIR_Z2,_DIR_Z4] then
         begin
           aktpackenum:=ord(pattern[2])-ord('0');
           exit;
         end;
        current_scanner^.skipspace;
        if not(c in ['0'..'9']) then
         begin
           hs:=current_scanner^.readid;
           if (hs='NORMAL') or (hs='DEFAULT') then
            aktpackenum:=4
           else
            Message(scan_w_only_pack_enum);
         end
        else
         begin
           case current_scanner^.readval of
            1 : aktpackenum:=1;
            2 : aktpackenum:=2;
            4 : aktpackenum:=4;
           else
            Message(scan_w_only_pack_enum);
           end;
         end;
      end;

{$ifdef testvarsets}
    procedure dir_setalloc(t:tdirectivetoken);
      var
        hs : string;
      begin
        current_scanner^.skipspace;
        if not(c in ['1','2','4']) then
         begin
           hs:=current_scanner^.readid;
           if (hs='FIXED') or ((hs='DEFAULT') OR (hs='NORMAL')) then
           aktsetalloc:=0               {Fixed mode, sets are 4 or 32 bytes}
          else
           Message(scan_w_only_packset);
          end
        else
         begin
           case current_scanner^.readval of
            1 : aktpackenum:=1;
            2 : aktpackenum:=2;
            4 : aktpackenum:=4;
           else
            Message(scan_w_only_packset);
           end;
         end;
      end;
{$ENDIF}
    procedure dir_apptype(t:tdirectivetoken);

      var
         hs : string;

      begin
        if target_info.target<>target_i386_win32 then
          Message(scan_w_app_type_not_support);
        if not current_module^.in_global then
          Message(scan_w_switch_is_global)
        else
          begin
             current_scanner^.skipspace;
             hs:=current_scanner^.readid;
             if hs='GUI' then
               apptype:=at_gui
             else if hs='CONSOLE' then
               apptype:=at_cui
             else
               Message1(scan_w_unsupported_app_type,hs);
          end;
      end;

    procedure dir_wait(t:tdirectivetoken);
      var had_info : boolean;
      begin
        had_info:=(status.verbosity and V_Info)<>0;
        { this message should allways appear !! }
        status.verbosity:=status.verbosity or V_Info;
        Message(scan_i_press_enter);
        readln;
        If not(had_info) then
          status.verbosity:=status.verbosity and (not V_Info);
      end;


    procedure dir_asmmode(t:tdirectivetoken);
      var
        s : string;
      begin
        current_scanner^.skipspace;
        s:=current_scanner^.readid;
        If Inside_asm_statement then
          Message1(scan_w_no_asm_reader_switch_inside_asm,s);
        if s='DEFAULT' then
         aktasmmode:=initasmmode
        else
         if not set_string_asmmode(s,aktasmmode) then
          Message1(scan_w_unsupported_asmmode_specifier,s);
      end;


    procedure dir_oldasmmode(t:tdirectivetoken);
      begin
        If Inside_asm_statement then
          Message1(scan_w_no_asm_reader_switch_inside_asm,directive[t]);
{$ifdef i386}
        case t of
         _DIR_I386_ATT    : aktasmmode:=asmmode_i386_att;
         _DIR_I386_DIRECT : aktasmmode:=asmmode_i386_direct;
         _DIR_I386_INTEL  : aktasmmode:=asmmode_i386_intel;
        end;
{$endif i386}
      end;


    procedure dir_delphiswitch(t:tdirectivetoken);
      var
        sw,state : char;
      begin
        case t of
           _DIR_ALIGN : sw:='A';
      _DIR_ASSERTIONS : sw:='C';
        _DIR_BOOLEVAL : sw:='B';
       _DIR_DEBUGINFO : sw:='D';
        _DIR_IOCHECKS : sw:='I';
    _DIR_LOCALSYMBOLS : sw:='L';
     _DIR_LONGSTRINGS : sw:='H';
     _DIR_OPENSTRINGS : sw:='P';
  _DIR_OVERFLOWCHECKS : sw:='Q';
     _DIR_RANGECHECKS : sw:='R';
   _DIR_REFERENCEINFO : sw:='Y';
     _DIR_STACKFRAMES : sw:='W';
    _DIR_TYPEDADDRESS : sw:='T';
        _DIR_TYPEINFO : sw:='M';
 _DIR_VARSTRINGCHECKS : sw:='V';
        else
         exit;
        end;
      { c contains the next char, a + or - would be fine }
        state:=current_scanner^.readstate;
        if state in ['-','+'] then
          HandleSwitch(sw,state);
      end;


    procedure dir_memory(t:tdirectivetoken);
      var
        l : longint;
      begin
        current_scanner^.skipspace;
        l:=current_scanner^.readval;
        if l>1024 then
         stacksize:=l;
        current_scanner^.skipspace;
        if c=',' then
         begin
           current_scanner^.readchar;
           current_scanner^.skipspace;
           l:=current_scanner^.readval;
           if l>1024 then
            heapsize:=l;
         end;
        if c=',' then
         begin
           current_scanner^.readchar;
           current_scanner^.skipspace;
           l:=current_scanner^.readval;
           { Ignore this value, because the limit is set by the OS
             info and shouldn't be changed by the user (PFV) }
         end;
      end;


    procedure dir_setverbose(t:tdirectivetoken);
      var
        flag,
        state : char;
      begin
        case t of
         _DIR_HINTS : flag:='H';
      _DIR_WARNINGS : flag:='W';
         _DIR_NOTES : flag:='N';
        else
         exit;
        end;
      { support ON/OFF }
        state:=current_scanner^.ReadState;
        SetVerbosity(flag+state);
      end;


      type
        tdirectiveproc=procedure(t:tdirectivetoken);
      const
        directiveproc:array[tdirectivetoken] of tdirectiveproc=(
         {_DIR_NONE} nil,
         {_DIR_ALIGN} dir_delphiswitch,
         {_DIR_APPTYPE} dir_apptype,
         {_DIR_ASMMODE} dir_asmmode,
         {_DIR_ASSERTION} dir_delphiswitch,
         {_DIR_BOOLEVAL} dir_delphiswitch,
         {_DIR_D} dir_description,
         {_DIR_DEBUGINFO} dir_delphiswitch,
         {_DIR_DEFINE} dir_define,
         {_DIR_DESCRIPTION} dir_description,
         {_DIR_ELSE} dir_conditional,
         {_DIR_ENDIF} dir_conditional,
         {_DIR_ERROR} dir_message,
         {_DIR_EXTENDEDSYNTAX} dir_delphiswitch,
         {_DIR_FATAL} dir_message,
         {_DIR_GOTO} dir_moduleswitch,
         {_DIR_HINT} dir_message,
         {_DIR_HINTS} dir_setverbose,
         {_DIR_I} dir_include,
         {_DIR_I386_ATT} dir_oldasmmode,
         {_DIR_I386_DIRECT} dir_oldasmmode,
         {_DIR_I386_INTEL} dir_oldasmmode,
         {_DIR_IOCHECKS} dir_delphiswitch,
         {_DIR_IF} dir_conditional,
         {_DIR_IFDEF} dir_conditional,
         {_DIR_IFNDEF} dir_conditional,
         {_DIR_IFOPT} dir_conditional,
         {_DIR_INCLUDE} dir_include,
         {_DIR_INCLUDEPATH} dir_includepath,
         {_DIR_INFO} dir_message,
         {_DIR_INLINE} dir_moduleswitch,
         {_DIR_L} dir_linkobject,
         {_DIR_LIBRARYPATH} dir_librarypath,
         {_DIR_LINK} dir_linkobject,
         {_DIR_LINKLIB} dir_linklib,
         {_DIR_LOCALSYMBOLS} dir_delphiswitch,
         {_DIR_LONGSTRINGS} dir_delphiswitch,
         {_DIR_M} dir_memory,
         {_DIR_MACRO} dir_moduleswitch,
         {_DIR_MAXFPUREGISTERS} dir_maxfpuregisters,
         {_DIR_MEMORY} dir_memory,
         {_DIR_MESSAGE} dir_message,
         {_DIR_MINENUMSIZE} dir_packenum,
         {_DIR_MMX} dir_localswitch,
         {_DIR_MODE} dir_mode,
         {_DIR_NOTE} dir_message,
         {_DIR_NOTES} dir_setverbose,
         {_DIR_OBJECTPATH} dir_objectpath,
         {_DIR_OPENSTRINGS} dir_delphiswitch,
         {_DIR_OUTPUT_FORMAT} dir_outputformat,
         {_DIR_OVERFLOWCHECKS} dir_delphiswitch,
         {_DIR_PACKENUM} dir_packenum,
         {_DIR_PACKRECORDS} dir_packrecords,
         {$IFDEF TestVarsets}
         {_DIR_PACKSET} dir_packset,
         {$ENDIF}
         {_DIR_R} dir_resource,
         {_DIR_RANGECHECKS} dir_delphiswitch,
         {_DIR_REFERENCEINFO} dir_delphiswitch,
         {_DIR_SATURATION} dir_localswitch,
         {_DIR_SMARTLINK} dir_moduleswitch,
         {_DIR_STACKFRAMES} dir_delphiswitch,
         {_DIR_STATIC} dir_moduleswitch,
         {_DIR_STOP} dir_message,
         {_DIR_TYPEDADDRESS} dir_delphiswitch,
         {_DIR_TYPEINFO} dir_delphiswitch,
         {_DIR_UNDEF} dir_undef,
         {_DIR_UNITPATH} dir_unitpath,
         {_DIR_VARSTRINGCHECKS} dir_delphiswitch,
         {_DIR_VERSION} dir_version,
         {_DIR_WAIT} dir_wait,
         {_DIR_WARNING} dir_message,
         {_DIR_WARNINGS} dir_setverbose,
         {_DIR_Z1} dir_packenum,
         {_DIR_Z2} dir_packenum,
         {_DIR_Z4} dir_packenum
         );

  {-------------------------------------------
            Main switches handling
  -------------------------------------------}

    procedure handledirectives;
      var
        t  : tdirectivetoken;
        p  : tdirectiveproc;
        hs : string;
      begin
         current_scanner^.gettokenpos;
         current_scanner^.readchar; {Remove the $}
         hs:=current_scanner^.readid;
         if parapreprocess then
          begin
            t:=Get_Directive(hs);
            if not(is_conditional(t) or (t=_DIR_DEFINE) or (t=_DIR_UNDEF)) then
             begin
               preprocfile^.AddSpace;
               preprocfile^.Add('{$'+hs+current_scanner^.readcomment+'}');
               exit;
             end;
          end;
         Message1(scan_d_handling_switch,'$'+hs);
         if hs='' then
          Message1(scan_w_illegal_switch,'$'+hs);
      { Check for compiler switches }
         while (length(hs)=1) and (c in ['-','+']) do
          begin
            HandleSwitch(hs[1],c);
            current_scanner^.readchar; {Remove + or -}
            if c=',' then
             begin
               current_scanner^.readchar;   {Remove , }
             { read next switch, support $v+,$+}
               hs:=current_scanner^.readid;
               if (hs='') then
                begin
                  if (c='$') and (m_fpc in aktmodeswitches) then
                   begin
                     current_scanner^.readchar;  { skip $ }
                     hs:=current_scanner^.readid;
                   end;
                  if (hs='') then
                   Message1(scan_w_illegal_directive,'$'+c);
                end
               else
                Message1(scan_d_handling_switch,'$'+hs);
             end
            else
             hs:='';
          end;
      { directives may follow switches after a , }
         if hs<>'' then
          begin
            t:=Get_Directive(hs);
            if t<>_DIR_NONE then
             begin
               p:=directiveproc[t];
             {$ifndef TP}
               if assigned(p) then
             {$else}
               if @p<>nil then
             {$endif}
                p(t);
             end
            else
             Message1(scan_w_illegal_directive,'$'+hs);
          { conditionals already read the comment }
            if (current_scanner^.comment_level>0) then
             current_scanner^.readcomment;
            { we've read the whole comment }
            aktcommentstyle:=comment_none;
          end;
      end;

{
  $Log$
  Revision 1.83  2000-06-30 20:23:38  peter
    * new message files layout with msg numbers (but still no code to
      show the number on the screen)

  Revision 1.82  2000/06/25 19:08:27  hajny
    + $R support for OS/2 (EMX) added

  Revision 1.81  2000/05/23 20:18:25  pierre
    + pavel's code integrated, but onyl inside
      ifdef pavel_linklib !

  Revision 1.80  2000/05/09 21:31:50  pierre
   * fix problem when modifying several local switches in a row

  Revision 1.79  2000/05/03 14:36:58  pierre
   * fix for tests/test/testrang.pp bug

  Revision 1.78  2000/04/14 11:16:10  pierre
    * partial linklib change
      I could not use Pavel's code because it broke the current way
      linklib is used, which is messy :(
    + add postw32 call if external linking on win32

  Revision 1.77  2000/04/08 20:18:53  michael
  * Fixed bug in readcomment that was dropping * characters

  Revision 1.76  2000/02/28 17:23:57  daniel
  * Current work of symtable integration committed. The symtable can be
    activated by defining 'newst', but doesn't compile yet. Changes in type
    checking and oop are completed. What is left is to write a new
    symtablestack and adapt the parser to use it.

  Revision 1.75  2000/02/14 20:58:43  marco
   * Basic structures for new sethandling implemented.

  Revision 1.74  2000/02/09 13:23:03  peter
    * log truncated

  Revision 1.73  2000/01/14 14:28:40  pierre
   * avoid searching of include file in start dir first

  Revision 1.72  2000/01/07 01:14:37  peter
    * updated copyright to 2000

  Revision 1.71  2000/01/04 15:15:53  florian
    + added compiler switch $maxfpuregisters
    + fixed a small problem in secondvecn

  Revision 1.70  1999/12/20 23:23:30  pierre
   + $description $version

  Revision 1.69  1999/12/02 17:34:34  peter
    * preprocessor support. But it fails on the caret in type blocks

  Revision 1.68  1999/11/24 11:39:53  pierre
   * asmmode message was placed too early

  Revision 1.67  1999/11/12 11:03:50  peter
    * searchpaths changed to stringqueue object

  Revision 1.66  1999/11/06 14:34:26  peter
    * truncated log to 20 revs

  Revision 1.65  1999/10/30 12:32:30  peter
    * fixed line counter when the first line had #10 only. This was buggy
      for both the main file as for include files

  Revision 1.64  1999/09/27 23:38:17  peter
    * bracket support for macro define

  Revision 1.63  1999/09/20 16:39:02  peter
    * cs_create_smart instead of cs_smartlink
    * -CX is create smartlink
    * -CD is create dynamic, but does nothing atm.

  Revision 1.62  1999/09/03 10:00:49  peter
    * included the 1.60 version of Pierre which was lost !

  Revision 1.61  1999/09/02 18:47:46  daniel
    * Could not compile with TP, some arrays moved to heap
    * NOAG386BIN default for TP
    * AG386* files were not compatible with TP, fixed.

  Revision 1.60  1999/08/31 15:55:45  pierre
    + tmacrosym.is_used set

  Revision 1.59  1999/08/05 16:53:10  peter
    * V_Fatal=1, all other V_ are also increased
    * Check for local procedure when assigning procvar
    * fixed comment parsing because directives
    * oldtp mode directives better supported
    * added some messages to errore.msg

  Revision 1.58  1999/08/04 13:03:03  jonas
    * all tokens now start with an underscore
    * PowerPC compiles!!

  Revision 1.57  1999/07/26 14:55:36  florian
    * $mode gives now a warning if an unknown mode keyword follows

  Revision 1.56  1999/07/23 16:05:27  peter
    * alignment is now saved in the symtable
    * C alignment added for records
    * PPU version increased to solve .12 <-> .13 probs

}