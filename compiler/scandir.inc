{
    $Id$
    Copyright (c) 1998 by Peter Vreman

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
   directivelen=16;
type
   directivestr=string[directivelen];
   tdirectivetoken=(
     _DIR_NONE,
     _DIR_ALIGN,_DIR_APPTYPE,_DIR_ASMMODE,_DIR_ASSERTIONS,
     _DIR_BOOLEVAL,
     _DIR_D,_DIR_DEBUGINFO,_DIR_DEFINE,_DIR_DESCRIPTION,
     _DIR_ELSE,_DIR_ENDIF,_DIR_ERROR,_DIR_EXTENDEDSYNTAX,
     _DIR_FATAL,
     _DIR_HINT,_DIR_HINTS,
     _DIR_I,_DIR_I386_ATT,_DIR_I386_DIRECT,_DIR_I386_INTEL,_DIR_IOCHECKS,
       _DIR_IF,_DIR_IFDEF,_DIR_IFNDEF,_DIR_IFOPT,_DIR_INCLUDE,_DIR_INFO,
     _DIR_L,_DIR_LINK,_DIR_LINKLIB,_DIR_LOCALSYMBOLS,_DIR_LONGSTRINGS,
     _DIR_M,_DIR_MEMORY,_DIR_MESSAGE,_DIR_MINENUMSIZE,_DIR_MMX,_DIR_MODE,
     _DIR_NOTE,_DIR_NOTES,
     _DIR_OPENSTRINGS,_DIR_OUTPUT_FORMAT,_DIR_OVERFLOWCHECKS,
     _DIR_PACKENUM,_DIR_PACKRECORDS,
     _DIR_RANGECHECKS,_DIR_REFERENCEINFO,
     _DIR_SATURATION,_DIR_SMARTLINK,_DIR_STACKFRAMES,_DIR_STOP,
     _DIR_TYPEDADDRESS,_DIR_TYPEINFO,
     _DIR_UNDEF,
     _DIR_VARSTRINGCHECKS,
     _DIR_WAIT,_DIR_WARNING,_DIR_WARNINGS,
     _DIR_Z1,_DIR_Z2,_DIR_Z4
     );
const
   firstdirective=_DIR_NONE;
   lastdirective=_DIR_Z4;
   directive:array[tdirectivetoken] of directivestr=(
     '',
     'ALIGN','APPTYPE','ASMMODE','ASSERTIONS',
     'BOOLEVAL',
     'D','DEBUGINFO','DEFINE','DESCRIPTION',
     'ELSE','ENDIF','ERROR','EXTENDEDSYNTAX',
     'FATAL',
     'HINT','HINTS',
     'I','I386_ATT','I386_DIRECT','I386_INTEL','IOCHECKS',
       'IF','IFDEF','IFNDEF','IFOPT','INCLUDE','INFO',
     'L','LINK','LINKLIB','LOCALSYMBOLS','LONGSTRINGS',
     'M','MEMORY','MESSAGE','MINENUMSIZE','MMX','MODE',
     'NOTE','NOTES',
     'OPENSTRINGS','OUTPUT_FORMAT','OVERFLOWCHECKS',
     'PACKENUM','PACKRECORDS',
     'RANGECHECKS','REFERENCEINFO',
     'SATURATION','SMARTLINK','STACKFRAMES','STOP',
     'TYPEDADDRESS','TYPEINFO',
     'UNDEF',
     'VARSTRINGCHECKS',
     'WAIT','WARNING','WARNINGS',
     'Z1','Z2','Z4'
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
         if preproc_token=ID then
           begin
              if preprocpat='NOT' then
                begin
                   preproc_consume(ID);
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
                   preproc_consume(ID);
                   if assigned(mac) then
                     begin
                        if mac^.defined and assigned(mac^.buftext) then
                          begin
                             if mac^.buflen>255 then
                               begin
                                  len:=255;
                                  Message(scan_w_marco_cut_after_255_chars);
                               end
                             else
                               len:=mac^.buflen;
{$ifopt H+}
                             setlength(hs,len);
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
         else if preproc_token=LKLAMMER then
           begin
              preproc_consume(LKLAMMER);
              read_factor:=read_expr;
              preproc_consume(RKLAMMER);
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
              if (preproc_token=ID) then
                begin
                   if preprocpat='AND' then
                     begin
                        preproc_consume(ID);
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
              if (preproc_token=ID) then
                begin
                   if preprocpat='OR' then
                     begin
                        preproc_consume(ID);
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
         if not(t in [EQUAL,UNEQUAL,LT,GT,LTE,GTE]) then
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
                 EQUAL : b:=l1=l2;
               UNEQUAL : b:=l1<>l2;
                    LT : b:=l1<l2;
                    GT : b:=l1>l2;
                   GTE : b:=l1>=l2;
                   LTE : b:=l1<=l2;
              end;
           end
         else
           begin
              case t of
                 EQUAL : b:=hs1=hs2;
               UNEQUAL : b:=hs1<>hs2;
                    LT : b:=hs1<hs2;
                    GT : b:=hs1>hs2;
                   GTE : b:=hs1>=hs2;
                   LTE : b:=hs1<=hs2;
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
                  current_scanner^.addpreprocstack(assigned(mac) and mac^.defined,hs,scan_c_ifdef_found);
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
                  current_scanner^.addpreprocstack(found,hs,scan_c_ifopt_found);
                end;
      _DIR_IF : begin
                  current_scanner^.skipspace;
                  { start preproc expression scanner }
                  preproc_token:=current_scanner^.readpreproc;
                  hs:=read_expr;
                  current_scanner^.addpreprocstack(hs<>'0',hs,scan_c_if_found);
                end;
  _DIR_IFNDEF : begin
                  current_scanner^.skipspace;
                  hs:=current_scanner^.readid;
                  mac:=pmacrosym(macros^.search(hs));
                  current_scanner^.addpreprocstack(not(assigned(mac) and mac^.defined),hs,scan_c_ifndef_found);
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
        oldaktfilepos:=aktfilepos;
      end;


    procedure dir_define(t:tdirectivetoken);
      var
        hs  : string;
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
                       { first char }
                       current_scanner^.readchar;
                       while (c<>'}') do
                         begin
                            macrobuffer^[macropos]:=c;
                            current_scanner^.readchar;
                            if c=#26 then Message(scan_f_end_of_file);
                            inc(macropos);
                            if macropos>maxmacrolen then
                             Message(scan_f_macro_buffer_overflow);
                         end;
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
      end;


    procedure dir_message(t:tdirectivetoken);
      var
        w   : tmsgconst;
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
           _DIR_SMARTLINK : sw:=cs_smartlink;
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
           if state='-' then
            aktlocalswitches:=aktlocalswitches-[sw]
           else
            aktlocalswitches:=aktlocalswitches+[sw];
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
{$ifopt H+}
        setlength(hs,i);
{$else}
        hs[0]:=chr(i);
{$endif}
        if hs='' then
         exit;
        if (hs[1]='%') then
         begin
         { case insensitive }
           hs:=upper(hs);
         { save old }
           path:=hs;
         { remove %'s }
           Delete(hs,1,1);
           if hs[length(hs)]='%' then
            Delete(hs,length(hs),1);
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
             hs:=target_string
           else
            hs:=getenv(hs);
           if hs='' then
            Comment(V_Warning,'Include environment '+path+' not found in environment')
           else
            begin
              { make it a stringconst }
              hs:=''''+hs+'''';
              current_scanner^.insertmacro(@hs[1],length(hs));
            end;
         end
        else
         begin
           hs:=FixFileName(hs);
           fsplit(hs,path,name,ext);
         { first look in the path of _d then currentmodule }
           path:=search(name+ext,path+';'+current_scanner^.inputfile^.path^+';'+includesearchpath,found);
         { shutdown current file }
           current_scanner^.tempcloseinputfile;
         { load new file }
           hp:=new(pinputfile,init(path+name+ext));
           current_scanner^.addfile(hp);
           if not current_scanner^.openinputfile then
            Message1(scan_f_cannot_open_includefile,hs);
           Message1(scan_t_start_include_file,current_scanner^.inputfile^.path^+current_scanner^.inputfile^.name^);
           current_scanner^.reload;
         { register for refs }
           current_module^.sourcefiles^.register_file(hp);
         end;
      end;


    procedure dir_description(t:tdirectivetoken);
      begin
      end;


    procedure dir_linkobject(t:tdirectivetoken);
      begin
        current_scanner^.skipspace;
        current_scanner^.readstring;
        current_module^.linkofiles.insert(FixFileName(orgpattern));
      end;


    procedure dir_linklib(t:tdirectivetoken);
      begin
        current_scanner^.skipspace;
        current_scanner^.readstring;
        current_module^.linkSharedLibs.insert(orgpattern);
      end;


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
              aktmodeswitches:=gpcmodeswitches;
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
           if (hs='NORMAL') or (hs='DEFAULT') then
            aktpackrecords:=2
           else
            Message(scan_w_only_pack_records);
         end
        else
         begin
           case current_scanner^.readval of
            1 : aktpackrecords:=1;
            2 : aktpackrecords:=2;
            4 : aktpackrecords:=4;
            8 : aktpackrecords:=8;
           16 : aktpackrecords:=16;
           32 : aktpackrecords:=32;
           else
            Message(scan_w_only_pack_records);
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
        if s='DEFAULT' then
         aktasmmode:=initasmmode
        else
         if not set_string_asmmode(s,aktasmmode) then
          Message1(scan_w_unsupported_asmmode_specifier,s);
      end;


    procedure dir_oldasmmode(t:tdirectivetoken);
      begin
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
           if l>1024 then
            maxheapsize:=l;
         end;
        if heapsize>maxheapsize then
          message(scan_w_illegal_switch);
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
         {_DIR_INFO} dir_message,
         {_DIR_L} dir_linkobject,
         {_DIR_LINK} dir_linkobject,
         {_DIR_LINKLIB} dir_linklib,
         {_DIR_LOCALSYMBOLS} dir_delphiswitch,
         {_DIR_LONGSTRINGS} dir_delphiswitch,
         {_DIR_M} dir_memory,
         {_DIR_MEMORY} dir_memory,
         {_DIR_MESSAGE} dir_message,
         {_DIR_MINENUMSIZE} dir_packenum,
         {_DIR_MMX} dir_localswitch,
         {_DIR_MODE} dir_mode,
         {_DIR_NOTE} dir_message,
         {_DIR_NOTES} dir_setverbose,
         {_DIR_OPENSTRINGS} dir_delphiswitch,
         {_DIR_OUTPUT_FORMAT} dir_outputformat,
         {_DIR_OVERFLOWCHECKS} dir_delphiswitch,
         {_DIR_PACKENUM} dir_packenum,
         {_DIR_PACKRECORDS} dir_packrecords,
         {_DIR_RANGECHECKS} dir_delphiswitch,
         {_DIR_REFERENCEINFO} dir_delphiswitch,
         {_DIR_SATURATION} dir_localswitch,
         {_DIR_SMARTLINK} dir_moduleswitch,
         {_DIR_STACKFRAMES} dir_delphiswitch,
         {_DIR_STOP} dir_message,
         {_DIR_TYPEDADDRESS} dir_delphiswitch,
         {_DIR_TYPEINFO} dir_delphiswitch,
         {_DIR_UNDEF} dir_undef,
         {_DIR_VARSTRINGCHECKS} dir_delphiswitch,
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
             {$ifdef FPC}
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
          end;
      end;

{
  $Log$
  Revision 1.42  1998-11-16 12:18:04  peter
    * H+ fixes

  Revision 1.41  1998/10/22 17:54:08  florian
    + switch $APPTYPE for win32 added

  Revision 1.40  1998/10/21 20:16:04  peter
    * beter line info for conditionals

  Revision 1.39  1998/10/13 16:50:21  pierre
    * undid some changes of Peter that made the compiler wrong
      for m68k (I had to reinsert some ifdefs)
    * removed several memory leaks under m68k
    * removed the meory leaks for assembler readers
    * cross compiling shoud work again better
      ( crosscompiling sysamiga works
       but as68k still complain about some code !)

  Revision 1.38  1998/10/13 13:10:28  peter
    * new style for m68k/i386 infos and enums

  Revision 1.37  1998/10/08 23:29:04  peter
    * -vu shows unit info, -vt shows tried/used files

  Revision 1.36  1998/10/08 17:17:29  pierre
    * current_module old scanner tagged as invalid if unit is recompiled
    + added ppheap for better info on tracegetmem of heaptrc
      (adds line column and file index)
    * several memory leaks removed ith help of heaptrc !!

  Revision 1.35  1998/10/08 13:44:39  peter
    * fixed $I %date% not in uppercase

  Revision 1.34  1998/09/28 16:57:24  pierre
    * changed all length(p^.value_str^) into str_length(p)
      to get it work with and without ansistrings
    * changed sourcefiles field of tmodule to a pointer

  Revision 1.33  1998/09/26 17:45:40  peter
    + idtoken and only one token table

  Revision 1.32  1998/09/24 23:49:19  peter
    + aktmodeswitches

  Revision 1.31  1998/09/18 16:03:44  florian
    * some changes to compile with Delphi

  Revision 1.30  1998/09/16 16:41:47  peter
    * merged fixes

  Revision 1.28.2.1  1998/09/16 16:09:51  peter
    * on/off support also for the local/module switches

  Revision 1.29  1998/09/11 09:19:53  daniel
  * Removed tabs.

  Revision 1.28  1998/09/10 15:25:36  daniel
  + Added maxheapsize.
  * Corrected semi-bug in calling the assembler and the linker

  Revision 1.27  1998/09/09 15:33:59  peter
    * removed warnings

  Revision 1.26  1998/09/03 11:24:02  peter
    * moved more inputfile things from tscannerfile to tinputfile
    * changed ifdef Sourceline to cs_asm_source

  Revision 1.25  1998/09/02 15:13:31  peter
    * fixed typo in directive table

  Revision 1.24  1998/09/01 12:52:06  peter
    + a lot of delphi switches

  Revision 1.23  1998/08/26 15:35:34  peter
    * fixed scannerfiles for macros
    + $I %<environment>%

  Revision 1.22  1998/08/19 14:57:50  peter
    * small fix for aktfilepos

  Revision 1.20  1998/08/18 15:11:52  peter
    * recompiles again

  Revision 1.19  1998/08/18 09:24:44  pierre
    * small warning position bug fixed
    * support_mmx switches splitting was missing
    * rhide error and warning output corrected

  Revision 1.18  1998/08/10 14:50:25  peter
    + localswitches, moduleswitches, globalswitches splitting

  Revision 1.17  1998/08/10 09:56:04  peter
    * path to the include file is also written to the debug output

  Revision 1.16  1998/08/04 22:03:44  michael
  + fixed dir_include search() call

  Revision 1.15  1998/07/14 21:46:55  peter
    * updated messages file

  Revision 1.14  1998/07/14 14:47:03  peter
    * released NEWINPUT

  Revision 1.13  1998/07/07 12:32:54  peter
    * status.currentsource is now calculated in verbose (more accurated)

  Revision 1.12  1998/07/07 11:20:10  peter
    + NEWINPUT for a better inputfile and scanner object

  Revision 1.11  1998/06/04 23:51:59  peter
    * m68k compiles
    + .def file creation moved to gendef.pas so it could also be used
      for win32

  Revision 1.10  1998/05/30 14:31:10  peter
    + $ASMMODE

  Revision 1.9  1998/05/23 01:21:28  peter
    + aktasmmode, aktoptprocessor, aktoutputformat
    + smartlink per module $SMARTLINK-/+ (like MMX) and moved to aktswitches
    + $LIBNAME to set the library name where the unit will be put in
    * splitted cgi386 a bit (codeseg to large for bp7)
    * nasm, tasm works again. nasm moved to ag386nsm.pas

  Revision 1.8  1998/05/11 13:07:57  peter
    + $ifdef NEWPPU for the new ppuformat
    + $define GDB not longer required
    * removed all warnings and stripped some log comments
    * no findfirst/findnext anymore to remove smartlink *.o files

  Revision 1.7  1998/05/08 09:21:20  michael
  * Added missing -Fl message to messages file.
  * Corrected mangling of file names when doing Linklib
  * -Fl now actually WORKS.
  * Librarysearchpath is now a field in linker object.

  Revision 1.6  1998/05/04 17:54:28  peter
    + smartlinking works (only case jumptable left todo)
    * redesign of systems.pas to support assemblers and linkers
    + Unitname is now also in the PPU-file, increased version to 14

  Revision 1.5  1998/04/30 15:59:42  pierre
    * GDB works again better :
      correct type info in one pass
    + UseTokenInfo for better source position
    * fixed one remaining bug in scanner for line counts
    * several little fixes

  Revision 1.4  1998/04/29 13:42:27  peter
    + $IOCHECKS and $ALIGN to test already, other will follow soon
    * fixed the wrong linecounting with comments

  Revision 1.3  1998/04/28 11:45:53  florian
    * make it compilable with TP
    + small COM problems solved to compile classes.pp

  Revision 1.2  1998/04/28 10:09:54  pierre
    * typo error in asm style reading corrected

  Revision 1.1  1998/04/27 23:13:53  peter
    + the new files for the scanner

}
