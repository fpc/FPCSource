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
     _DIR_ALIGN,
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
     _DIR_SATURATION,_DIR_STOP,
     _DIR_UNDEF,
     _DIR_WAIT,_DIR_WARNING
     );
const
   firstdirective=_DIR_NONE;
   lastdirective=_DIR_WARNING;
   directive:array[tdirectivetoken] of directivestr=(
     '',
     'ALIGN',
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
     'SATURATION','STOP',
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

    function read_preproc : ttoken;
      begin
         skipspace;
         case c of
        'A'..'Z',
        'a'..'z',
    '_','0'..'9' : begin
                     preprocpat:=readid;
                     read_preproc:=ID;
                   end;
             '(' : begin
                     readchar;
                     read_preproc:=LKLAMMER;
                   end;
             ')' : begin
                     readchar;
                     read_preproc:=RKLAMMER;
                   end;
             '+' : begin
                     readchar;
                     read_preproc:=PLUS;
                   end;
             '-' : begin
                     readchar;
                     read_preproc:=MINUS;
                   end;
             '*' : begin
                     readchar;
                     read_preproc:=STAR;
                   end;
             '/' : begin
                     readchar;
                     read_preproc:=SLASH;
                   end;
             '=' : begin
                     readchar;
                     read_preproc:=EQUAL;
                   end;
             '>' : begin
                     readchar;
                     if c='=' then
                      begin
                        readchar;
                        read_preproc:=GTE;
                      end
                     else
                      read_preproc:=GT;
                   end;
             '<' : begin
                     readchar;
                     case c of
                      '>' : begin
                              readchar;
                              read_preproc:=UNEQUAL;
                            end;
                      '=' : begin
                              readchar;
                              read_preproc:=LTE;
                            end;
                     else   read_preproc:=LT;
                     end;
                   end;
             #26 : Message(scan_f_end_of_file);
         else
          begin
            read_preproc:=_EOF;
          end;
         end;
      end;


    procedure preproc_consume(t : ttoken);
      begin
         if t<>preproc_token then
          Message(scan_e_preproc_syntax_error);
         preproc_token:=read_preproc;
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

       procedure newpreproc(isifdef,a:boolean;const s:string;w:tmsgconst);
       begin
         preprocstack:=new(ppreprocstack,init(isifdef,
           ((preprocstack=nil) or preprocstack^.accept) and a,preprocstack));
         preprocstack^.name:=s;
         preprocstack^.line_nb:=current_module^.current_inputfile^.line_no;
         if preprocstack^.accept then
          Message2(w,preprocstack^.name,'accepted')
         else
          Message2(w,preprocstack^.name,'rejected');
       end;

      var
        hs    : string;
        mac   : pmacrosym;
        found : boolean;
      begin
        while true do
         begin
           case t of
   _DIR_ENDIF : begin
                { we can always accept an ELSE }
                  if assigned(preprocstack) then
                   begin
                     Message1(scan_c_endif_found,preprocstack^.name);
                     if not preprocstack^.isifdef then
                      popstack;
                   end
                  else
                   Message(scan_e_endif_without_if);
                { now pop the condition }
                  if assigned(preprocstack) then
                   begin
                   { we only use $ifdef in the stack }
                     if preprocstack^.isifdef then
                      popstack
                     else
                      Message(scan_e_too_much_endifs);
                   end
                  else
                   Message(scan_e_endif_without_if);
                end;
    _DIR_ELSE : begin
                  if assigned(preprocstack) then
                   begin
                     preprocstack:=new(ppreprocstack,init(false,
                       not(preprocstack^.accept) and
                       ((preprocstack^.next=nil) or (preprocstack^.next^.accept)),preprocstack));
                     preprocstack^.line_nb:=current_module^.current_inputfile^.line_no;
                     preprocstack^.name:=preprocstack^.next^.name;
                     if preprocstack^.accept then
                      Message2(scan_c_else_found,preprocstack^.name,'accepted')
                     else
                      Message2(scan_c_else_found,preprocstack^.name,'rejected');
                   end
                  else
                   Message(scan_e_endif_without_if);
                end;
   _DIR_IFDEF : begin
                  skipspace;
                  hs:=readid;
                  mac:=pmacrosym(macros^.search(hs));
                  newpreproc(true,assigned(mac) and mac^.defined,hs,scan_c_ifdef_found);
                end;
   _DIR_IFOPT : begin
                  skipspace;
                  hs:=readid;
                  if (length(hs)=1) and (c in ['-','+']) then
                   begin
                     found:=CheckSwitch(hs[1],c);
                     readchar; {read + or -}
                   end
                  else
                   Message(scan_w_illegal_switch);
                  newpreproc(true,found,hs,scan_c_ifopt_found);
                end;
      _DIR_IF : begin
                  skipspace;
                  { start preproc expression scanner }
                  preproc_token:=read_preproc;
                  hs:=read_expr;
                  newpreproc(true,hs<>'0',hs,scan_c_if_found);
                end;
  _DIR_IFNDEF : begin
                  skipspace;
                  hs:=readid;
                  mac:=pmacrosym(macros^.search(hs));
                  newpreproc(true,not(assigned(mac) and mac^.defined),hs,scan_c_ifndef_found);
                end;
           end;
         { accept the text ? }
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
      end;


    procedure dir_define(t:tdirectivetoken);
      var
        ht  : ttoken;
        hs2,
        hs  : string;
        mac : pmacrosym;
      begin
        skipspace;
        hs:=readid;
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
             skipspace;
           { may be a macro? }
             if c=':' then
               begin
                  readchar;
                  if c='=' then
                    begin
                       { first char }
                       readchar;
                       macropos:=0;
                       while (c<>'}') do
                         begin
                            macrobuffer^[macropos]:=c;
                            readchar;
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
                    end;
               end;
          end;
      end;


    procedure dir_undef(t:tdirectivetoken);
      var
        hs  : string;
        mac : pmacrosym;
      begin
        skipspace;
        hs:=readid;
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
        skipspace;
        Message1(w,readcomment);
      end;


    procedure dir_switch(t:tdirectivetoken);
{$ifdef SUPPORT_MMX}
      var
        sw : tcswitch;
{$endif}
      begin
{$ifdef SUPPORT_MMX}
        case t of
          _DIR_MMX : sw:=cs_mmx;
   _DIR_SATURATION : sw:=cs_mmx_saturation;
        end;
{$endif}
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
        skipspace;
        hs:=readcomment;
        while (hs<>'') and (hs[length(hs)]=' ') do
         dec(byte(hs[0]));
        hs:=FixFileName(hs);
        fsplit(hs,path,name,ext);
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
      end;


    procedure dir_description(t:tdirectivetoken);
      begin
      end;


    procedure dir_linkobject(t:tdirectivetoken);
      var
        path,hs : string;
        found   : boolean;
      begin
        skipspace;
        hs:=FixFileName(readstring);
        current_module^.linkofiles.insert(hs);
      end;


    procedure dir_linklib(t:tdirectivetoken);
      var
        hs : string;
      begin
        skipspace;
        hs:=readstring;
        current_module^.linkSharedLibs.insert(hs);
      end;


    procedure dir_outputformat(t:tdirectivetoken);
      var
        hs : string;
      begin
        if not current_module^.in_main then
         Message(scan_w_switch_is_global)
        else
          begin
            skipspace;
            hs:=readid;
{$ifdef i386}
            if hs='NASM' then
              current_module^.output_format:=of_nasm
            else
             if hs='MASM' then
              current_module^.output_format:=of_masm
            else
             if hs='O' then
              current_module^.output_format:=of_o
            else
             if hs='OBJ' then
              current_module^.output_format:=of_obj
            else
{$endif}
              Message(scan_w_illegal_switch);
          end;
      { for use in globals }
        output_format:=current_module^.output_format;
      end;

    procedure dir_packrecords(t:tdirectivetoken);
      var
        hs : string;
      begin
        skipspace;
        if upcase(c)='N' then
         begin
           hs:=readid;
           if hs='NORMAL' then
            aktpackrecords:=2
           else
            Message(scan_w_only_pack_records);
         end
        else
         begin
           case readval of
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
        ReadComment;
      end;


      type
        tdirectiveproc=procedure(t:tdirectivetoken);
      const
        directiveproc:array[tdirectivetoken] of tdirectiveproc=(
         {_DIR_NONE} nil,
         {_DIR_ALIGN} dir_delphiswitch,
         {_DIR_D} dir_description,
         {_DIR_DEFINE} dir_define,
         {_DIR_DESCRIPTION} dir_description,
         {_DIR_ELSE} dir_conditional,
         {_DIR_ENDIF} dir_conditional,
         {_DIR_ERROR} dir_message,
         {_DIR_FATAL} dir_message,
         {_DIR_I} dir_include,
         {_DIR_I386_ATT} dir_asmmode,
         {_DIR_I386_DIRECT} dir_asmmode,
         {_DIR_I386_INTEL} dir_asmmode,
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
        t     : tdirectivetoken;
        p     : tdirectiveproc;
        hs    : string;
      begin
         readchar; {Remove the $}
         hs:=readid;
         Message1(scan_d_handling_switch,'$'+hs);
         if hs='' then
          Message1(scan_w_illegal_switch,'$'+hs);
      { Check for compiler switches }
         while (length(hs)=1) and (c in ['-','+']) do
          begin
            HandleSwitch(hs[1],c);
            readchar; {Remove + or -}
            if c=',' then
             begin
               readchar;   {Remove , }
               hs:=readid; {Check for multiple switches on one line}
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
            if (comment_level>0) then
             readcomment;
          end;
      end;

{
  $Log$
  Revision 1.7  1998-05-08 09:21:20  michael
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
