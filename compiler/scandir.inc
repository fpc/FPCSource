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
     _DIR_ALIGN,_DIR_ASMMODE,
     _DIR_D,_DIR_DEFINE,_DIR_DESCRIPTION,
     _DIR_ELSE,_DIR_ENDIF,_DIR_ERROR,
     _DIR_FATAL,
     _DIR_I,_DIR_I386_ATT,_DIR_I386_DIRECT,_DIR_I386_INTEL,_DIR_IOCHECKS,
       _DIR_IF,_DIR_IFDEF,_DIR_IFNDEF,_DIR_IFOPT,_DIR_INFO,
     _DIR_L,_DIR_LINKLIB,
     _DIR_MESSAGE,_DIR_MMX,
     _DIR_NOTE,
     _DIR_OUTPUT_FORMAT,
     _DIR_PACKRECORDS,
     _DIR_SATURATION,_DIR_SMARTLINK,_DIR_STOP,
     _DIR_UNDEF,
     _DIR_WAIT,_DIR_WARNING
     );
const
   firstdirective=_DIR_NONE;
   lastdirective=_DIR_WARNING;
   directive:array[tdirectivetoken] of directivestr=(
     '',
     'ALIGN','ASMMODE',
     'D','DEFINE','DESCRIPTION',
     'ELSE','ENDIF','ERROR',
     'FATAL',
     'I','I386_ATT','I386_DIRECT','I386_INTEL','IOCHECKS',
       'IF','IFDEF','IFNDEF','IFOPT','INFO',
     'L','LINKLIB',
     'MESSAGE','MMX',
     'NOTE',
     'OUTPUT_FORMAT',
     'PACKRECORDS',
     'SATURATION','SMARTLINK','STOP',
     'UNDEF',
     'WAIT','WARNING'
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

{$ifndef NEWINPUT}
    function readpreproc:ttoken;
      begin
         skipspace;
         case c of
        'A'..'Z',
        'a'..'z',
    '_','0'..'9' : begin
                     preprocpat:=readid;
                     readpreproc:=ID;
                   end;
             '(' : begin
                     readchar;
                     readpreproc:=LKLAMMER;
                   end;
             ')' : begin
                     readchar;
                     readpreproc:=RKLAMMER;
                   end;
             '+' : begin
                     readchar;
                     readpreproc:=PLUS;
                   end;
             '-' : begin
                     readchar;
                     readpreproc:=MINUS;
                   end;
             '*' : begin
                     readchar;
                     readpreproc:=STAR;
                   end;
             '/' : begin
                     readchar;
                     readpreproc:=SLASH;
                   end;
             '=' : begin
                     readchar;
                     readpreproc:=EQUAL;
                   end;
             '>' : begin
                     readchar;
                     if c='=' then
                      begin
                        readchar;
                        readpreproc:=GTE;
                      end
                     else
                      readpreproc:=GT;
                   end;
             '<' : begin
                     readchar;
                     case c of
                      '>' : begin
                              readchar;
                              readpreproc:=UNEQUAL;
                            end;
                      '=' : begin
                              readchar;
                              readpreproc:=LTE;
                            end;
                     else   readpreproc:=LT;
                     end;
                   end;
             #26 : Message(scan_f_end_of_file);
         else
          begin
            readpreproc:=_EOF;
          end;
         end;
      end;
{$endif}

    procedure preproc_consume(t : ttoken);
      begin
        if t<>preproc_token then
         Message(scan_e_preproc_syntax_error);
        preproc_token:={$ifdef NEWINPUT}current_scanner^.{$endif}readpreproc;
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
                             hs[0]:=char(len);
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
         w : word;
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
                 EQUAL:
                   b:=l1=l2;
                 UNEQUAL:
                   b:=l1<>l2;
                 LT:
                   b:=l1<l2;
                 GT:
                   b:=l1>l2;
                 GTE:
                   b:=l1>=l2;
                 LTE:
                   b:=l1<=l2;
              end;
           end
         else
           begin
              case t of
                 EQUAL:
                   b:=hs1=hs2;
                 UNEQUAL:
                   b:=hs1<>hs2;
                 LT:
                   b:=hs1<hs2;
                 GT:
                   b:=hs1>hs2;
                 GTE:
                   b:=hs1>=hs2;
                 LTE:
                   b:=hs1<=hs2;
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
      begin
        while true do
         begin
           case t of
   _DIR_ENDIF : begin
                  {$ifdef NEWINPUT}current_scanner^.{$endif}poppreprocstack;
                end;
    _DIR_ELSE : begin
                  {$ifdef NEWINPUT}current_scanner^.{$endif}elsepreprocstack;
                end;
   _DIR_IFDEF : begin
{$ifdef NEWINPUT}
                  current_scanner^.skipspace;
                  hs:=current_scanner^.readid;
                  mac:=pmacrosym(macros^.search(hs));
                  current_scanner^.addpreprocstack(assigned(mac) and mac^.defined,hs,scan_c_ifdef_found);
{$else}
                  skipspace;
                  hs:=readid;
                  mac:=pmacrosym(macros^.search(hs));
                  addpreprocstack(assigned(mac) and mac^.defined,hs,scan_c_ifdef_found);
{$endif}
                end;
   _DIR_IFOPT : begin
{$ifdef NEWINPUT}
                  current_scanner^.skipspace;
                  hs:=current_scanner^.readid;
                  if (length(hs)=1) and (c in ['-','+']) then
                   begin
                     found:=CheckSwitch(hs[1],c);
                     current_scanner^.readchar; {read + or -}
                   end
                  else
                   Message(scan_w_illegal_switch);
                  current_scanner^.addpreprocstack(found,hs,scan_c_ifopt_found);
{$else}
                  skipspace;
                  hs:=readid;
                  if (length(hs)=1) and (c in ['-','+']) then
                   begin
                     found:=CheckSwitch(hs[1],c);
                     readchar; {read + or -}
                   end
                  else
                   Message(scan_w_illegal_switch);
                  addpreprocstack(found,hs,scan_c_ifopt_found);
{$endif}
                end;
      _DIR_IF : begin
{$ifdef NEWINPUT}
                  current_scanner^.skipspace;
                  { start preproc expression scanner }
                  preproc_token:=current_scanner^.readpreproc;
                  hs:=read_expr;
                  current_scanner^.addpreprocstack(hs<>'0',hs,scan_c_if_found);
{$else}
                  skipspace;
                  { start preproc expression scanner }
                  preproc_token:=readpreproc;
                  hs:=read_expr;
                  addpreprocstack(hs<>'0',hs,scan_c_if_found);
{$endif}
                end;
  _DIR_IFNDEF : begin
{$ifdef NEWINPUT}
                  current_scanner^.skipspace;
                  hs:=current_scanner^.readid;
                  mac:=pmacrosym(macros^.search(hs));
                  current_scanner^.addpreprocstack(not(assigned(mac) and mac^.defined),hs,scan_c_ifndef_found);
{$else}
                  skipspace;
                  hs:=readid;
                  mac:=pmacrosym(macros^.search(hs));
                  addpreprocstack(not(assigned(mac) and mac^.defined),hs,scan_c_ifndef_found);
{$endif}
                end;
           end;
         { accept the text ? }
{$ifdef NEWINPUT}
           if (current_scanner^.preprocstack=nil) or current_scanner^.preprocstack^.accept then
            break
           else
            begin
              Message(scan_c_skipping_until);
              repeat
                current_scanner^.skipuntildirective;
                t:=Get_Directive(current_scanner^.readid);
              until is_conditional(t);
              Message1(scan_d_handling_switch,'$'+directive[t]);
            end;
         end;
{$else}
           if (preprocstack=nil) or preprocstack^.accept then
            break
           else
            begin
              Message(scan_c_skipping_until);
              repeat
                skipuntildirective;
                t:=Get_Directive(readid);
              until is_conditional(t);
            end;
         end;
{$endif}
      end;


    procedure dir_define(t:tdirectivetoken);
      var
        ht  : ttoken;
        hs2,
        hs  : string;
        mac : pmacrosym;
        macropos : longint;
        macrobuffer : pmacrobuffer;
      begin
        {$ifdef NEWINPUT}current_scanner^.{$endif}skipspace;
        hs:={$ifdef NEWINPUT}current_scanner^.{$endif}readid;
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
        if support_macros then
          begin
          { key words are never substituted }
             hs2:=pattern;
             pattern:=hs;
             if is_keyword(ht) then
              Message(scan_e_keyword_cant_be_a_macro);
             pattern:=hs2;
           { !!!!!! handle macro params, need we this? }
             {$ifdef NEWINPUT}current_scanner^.{$endif}skipspace;
           { may be a macro? }
             if c=':' then
               begin
                  {$ifdef NEWINPUT}current_scanner^.{$endif}readchar;
                  if c='=' then
                    begin
                       new(macrobuffer);
                       macropos:=0;
                       { first char }
                       {$ifdef NEWINPUT}current_scanner^.{$endif}readchar;
                       while (c<>'}') do
                         begin
                            macrobuffer^[macropos]:=c;
                            {$ifdef NEWINPUT}current_scanner^.{$endif}readchar;
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
        {$ifdef NEWINPUT}current_scanner^.{$endif}skipspace;
        hs:={$ifdef NEWINPUT}current_scanner^.{$endif}readid;
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
       _DIR_NOTE : w:=scan_n_user_defined;
    _DIR_MESSAGE,
       _DIR_INFO : w:=scan_i_user_defined;
        end;
        {$ifdef NEWINPUT}current_scanner^.{$endif}skipspace;
        Message1(w,{$ifdef NEWINPUT}current_scanner^.{$endif}readcomment);
      end;


    procedure dir_switch(t:tdirectivetoken);
      var
        sw : tcswitch;
      begin
        case t of
{$ifdef SUPPORT_MMX}
          _DIR_MMX : sw:=cs_mmx;
   _DIR_SATURATION : sw:=cs_mmx_saturation;
{$endif}
   _DIR_SMARTLINK : sw:=cs_smartlink;
        end;
        {$ifdef NEWINPUT}current_scanner^.{$endif}skipspace;
        if c='-' then
         aktswitches:=aktswitches-[sw]
        else
         aktswitches:=aktswitches+[sw];
      end;


    procedure dir_include(t:tdirectivetoken);
      var
        hs    : string;
        path  : dirstr;
        name  : namestr;
        ext   : extstr;
        hp    : pinputfile;
        found : boolean;
      begin
        {$ifdef NEWINPUT}current_scanner^.{$endif}skipspace;
        hs:={$ifdef NEWINPUT}current_scanner^.{$endif}readcomment;
        while (hs<>'') and (hs[length(hs)]=' ') do
         dec(byte(hs[0]));
        hs:=FixFileName(hs);
        fsplit(hs,path,name,ext);
{$ifdef NEWINPUT}
      { first look in the path of _d then currentmodule }
        path:=search(hs,path+';'+current_scanner^.inputfile^.path^+';'+includesearchpath,found);
      { shutdown current file }
        current_scanner^.close;
      { load new file }
        hp:=new(pinputfile,init(path+name+ext));
        current_scanner^.addfile(hp);
        if not current_scanner^.open then
         Message1(scan_f_cannot_open_includefile,hs);
{        status.currentsource:=current_scanner^.inputfile^.name^; }
        Message1(scan_u_start_include_file,current_scanner^.inputfile^.name^);
        current_scanner^.reload;
      { register for refs }
        current_module^.sourcefiles.register_file(hp);
        current_module^.current_index:=hp^.ref_index;
{$else}
      { first look in the path of _d then currentmodule }
        path:=search(hs,path+';'+current_module^.current_inputfile^.path^+';'+includesearchpath,found);
        hp:=new(pinputfile,init(path,name,ext));
        hp^.reset;
        if ioresult=0 then
         begin
           current_module^.current_inputfile^.bufpos:=longint(inputpointer)-longint(inputbuffer);
           hp^.next:=current_module^.current_inputfile;
           current_module^.current_inputfile:=hp;
           status.currentsource:=current_module^.current_inputfile^.name^+current_module^.current_inputfile^.ext^;
           current_module^.sourcefiles.register_file(hp);
           current_module^.current_index:=hp^.ref_index;
           inputbuffer:=current_module^.current_inputfile^.buf;
           Message1(scan_u_start_include_file,current_module^.current_inputfile^.name^);
           reload;
         end
        else
         Message1(scan_f_cannot_open_includefile,hs);
{$endif NEWINPUT}
      end;


    procedure dir_description(t:tdirectivetoken);
      begin
      end;


    procedure dir_linkobject(t:tdirectivetoken);
      begin
        {$ifdef NEWINPUT}current_scanner^.{$endif}skipspace;
        {$ifdef NEWINPUT}current_scanner^.{$endif}readstring;
        current_module^.linkofiles.insert(FixFileName(orgpattern));
      end;


    procedure dir_linklib(t:tdirectivetoken);
      begin
        {$ifdef NEWINPUT}current_scanner^.{$endif}skipspace;
        {$ifdef NEWINPUT}current_scanner^.{$endif}readstring;
        current_module^.linkSharedLibs.insert(orgpattern);
      end;


    procedure dir_outputformat(t:tdirectivetoken);
      begin
        if not current_module^.in_main then
         Message(scan_w_switch_is_global)
        else
          begin
            {$ifdef NEWINPUT}current_scanner^.{$endif}skipspace;
            if set_string_asm({$ifdef NEWINPUT}current_scanner^.{$endif}readid) then
             aktoutputformat:=target_asm.id
            else
             Message(scan_w_illegal_switch);
          end;
      end;


    procedure dir_packrecords(t:tdirectivetoken);
      var
        hs : string;
      begin
        {$ifdef NEWINPUT}current_scanner^.{$endif}skipspace;
        if upcase(c)='N' then
         begin
           hs:={$ifdef NEWINPUT}current_scanner^.{$endif}readid;
           if hs='NORMAL' then
            aktpackrecords:=2
           else
            Message(scan_w_only_pack_records);
         end
        else
         begin
           case {$ifdef NEWINPUT}current_scanner^.{$endif}readval of
            1 : aktpackrecords:=1;
            2 : aktpackrecords:=2;
            4 : aktpackrecords:=4;
           else
            Message(scan_w_only_pack_records);
           end;
         end;
      end;

    procedure dir_wait(t:tdirectivetoken);
      begin
        Message(scan_i_press_enter);
        readln;
      end;

    procedure dir_asmmode(t:tdirectivetoken);
      var
        s : string;
      begin
        {$ifdef NEWINPUT}current_scanner^.{$endif}skipspace;
        s:={$ifdef NEWINPUT}current_scanner^.{$endif}readid;
        if s='DEFAULT' then
         aktasmmode:=initasmmode
        else
         if not set_string_asmmode(s,aktasmmode) then
          Comment(V_Warning,'Unsupported asm mode specified '+s);
      end;

    procedure dir_oldasmmode(t:tdirectivetoken);
      begin
{$ifdef i386}
        case t of
         _DIR_I386_ATT    : aktasmmode:=I386_ATT;
         _DIR_I386_DIRECT : aktasmmode:=I386_DIRECT;
         _DIR_I386_INTEL  : aktasmmode:=I386_INTEL;
        end;
{$endif}
      end;


    procedure dir_delphiswitch(t:tdirectivetoken);
      var
        sw : char;
      begin
        case t of
         _DIR_ALIGN : sw:='A';
      _DIR_IOCHECKS : sw:='I';
        else
         exit;
        end;
      { c contains the next char, a + or - would be fine }
        HandleSwitch(sw,c);
      end;


      type
        tdirectiveproc=procedure(t:tdirectivetoken);
      const
        directiveproc:array[tdirectivetoken] of tdirectiveproc=(
         {_DIR_NONE} nil,
         {_DIR_ALIGN} dir_delphiswitch,
         {_DIR_ASMMODE} dir_asmmode,
         {_DIR_D} dir_description,
         {_DIR_DEFINE} dir_define,
         {_DIR_DESCRIPTION} dir_description,
         {_DIR_ELSE} dir_conditional,
         {_DIR_ENDIF} dir_conditional,
         {_DIR_ERROR} dir_message,
         {_DIR_FATAL} dir_message,
         {_DIR_I} dir_include,
         {_DIR_I386_ATT} dir_oldasmmode,
         {_DIR_I386_DIRECT} dir_oldasmmode,
         {_DIR_I386_INTEL} dir_oldasmmode,
         {_DIR_IOCHECKS} dir_delphiswitch,
         {_DIR_IF} dir_conditional,
         {_DIR_IFDEF} dir_conditional,
         {_DIR_IFNDEF} dir_conditional,
         {_DIR_IFOPT} dir_conditional,
         {_DIR_INFO} dir_message,
         {_DIR_L} dir_linkobject,
         {_DIR_LINKLIB} dir_linklib,
         {_DIR_MESSAGE} dir_message,
         {_DIR_MMX} dir_switch,
         {_DIR_NOTE} dir_message,
         {_DIR_OUTPUT_FORMAT} dir_outputformat,
         {_DIR_PACKRECORDS} dir_packrecords,
         {_DIR_SATURATION} dir_switch,
         {_DIR_SMARTLINK} dir_switch,
         {_DIR_STOP} dir_message,
         {_DIR_UNDEF} dir_undef,
         {_DIR_WAIT} dir_wait,
         {_DIR_WARNING} dir_message
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
         {$ifdef NEWINPUT}current_scanner^.{$endif}gettokenpos;
         {$ifdef NEWINPUT}current_scanner^.{$endif}readchar; {Remove the $}
         hs:={$ifdef NEWINPUT}current_scanner^.{$endif}readid;
         Message1(scan_d_handling_switch,'$'+hs);
         if hs='' then
          Message1(scan_w_illegal_switch,'$'+hs);
      { Check for compiler switches }
         while (length(hs)=1) and (c in ['-','+']) do
          begin
            HandleSwitch(hs[1],c);
            {$ifdef NEWINPUT}current_scanner^.{$endif}readchar; {Remove + or -}
            if c=',' then
             begin
               {$ifdef NEWINPUT}current_scanner^.{$endif}readchar;   {Remove , }
               hs:={$ifdef NEWINPUT}current_scanner^.{$endif}readid; {Check for multiple switches on one line}
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
            if ({$ifdef NEWINPUT}current_scanner^.{$endif}comment_level>0) then
             {$ifdef NEWINPUT}current_scanner^.{$endif}readcomment;
          end;
      end;

{
  $Log$
  Revision 1.13  1998-07-07 12:32:54  peter
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
