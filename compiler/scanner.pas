{
    $Id$
    Copyright (c) 1993,97 by Florian Klaempfl

    This unit implements the scanner part and handling of the switches

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
unit scanner;

  interface

    uses
       strings,dos,cobjects,globals,symtable,systems,files,verbose,link;

    const
       id_len = 14;

    type
       ident = string[id_len];

    const
{$ifdef L_C}
       anz_keywords = 32;

       keyword : array[1..anz_keywords] of ident = (
          'auto','break','case','char','const','continue','default','do',
          'double','else','enum','extern','float','for','goto','if',
          'int','long','register','return','short','signed','sizeof','static',
          'struct','switch','typedef','union','unsigned','void','volatile',
          'while');
{$else}
       anz_keywords = 71;

       keyword : array[1..anz_keywords] of ident = (
{                'ABSOLUTE',}
                 'AND',
                 'ARRAY','AS','ASM',
{                'ASSEMBLER',}
                 'BEGIN',
                 'BREAK','CASE','CLASS',
                 'CONST','CONSTRUCTOR','CONTINUE',
                 'DESTRUCTOR','DISPOSE','DIV','DO','DOWNTO','ELSE','END',
                 'EXCEPT',
                 'EXIT',
{                'EXPORT',}
                 'EXPORTS',
{                'EXTERNAL',}
                 'FAIL','FALSE',
{                'FAR',}
                 'FILE','FINALLY','FOR',
{                'FORWARD',}
                 'FUNCTION','GOTO','IF','IMPLEMENTATION','IN',
                 'INHERITED','INITIALIZATION',
{                'INLINE',} {INLINE is a reserved word in TP. Why?}
                 'INTERFACE',
{                'INTERRUPT',}
                 'IS',
                 'LABEL','LIBRARY','MOD',
{                'NEAR',}
                 'NEW','NIL','NOT','OBJECT',
                 'OF','ON','OPERATOR','OR','OTHERWISE','PACKED',
                 'PROCEDURE','PROGRAM','PROPERTY',
                 'RAISE','RECORD','REPEAT','SELF',
                 'SET','SHL','SHR','STRING','THEN','TO',
                 'TRUE','TRY','TYPE','UNIT','UNTIL',
                 'USES','VAR',
{                'VIRTUAL',}
                 'WHILE','WITH','XOR');
{***}

       keyword_token : array[1..anz_keywords] of ttoken = (
{                _ABSOLUTE,}
                 _AND,
                 _ARRAY,_AS,_ASM,
{                _ASSEMBLER,}
                 _BEGIN,
                 _BREAK,_CASE,_CLASS,
                 _CONST,_CONSTRUCTOR,_CONTINUE,
                 _DESTRUCTOR,_DISPOSE,_DIV,_DO,_DOWNTO,
                 _ELSE,_END,_EXCEPT,
                 _EXIT,
{                _EXPORT,}
                 _EXPORTS,
{                _EXTERNAL,}
                 _FAIL,_FALSE,
{                _FAR,}
                 _FILE,_FINALLY,_FOR,
{                _FORWARD,}
                 _FUNCTION,_GOTO,_IF,_IMPLEMENTATION,_IN,
                 _INHERITED,_INITIALIZATION,
{                _INLINE,}
                 _INTERFACE,
{                _INTERRUPT,}
                 _IS,
                 _LABEL,_LIBRARY,_MOD,
{                _NEAR,}
                 _NEW,_NIL,_NOT,_OBJECT,
                 _OF,_ON,_OPERATOR,_OR,_OTHERWISE,_PACKED,
                 _PROCEDURE,_PROGRAM,_PROPERTY,
                 _RAISE,_RECORD,_REPEAT,_SELF,
                 _SET,_SHL,_SHR,_STRING,_THEN,_TO,
                 _TRUE,_TRY,_TYPE,_UNIT,_UNTIL,
                 _USES,_VAR,
{                _VIRTUAL,}
                 _WHILE,_WITH,_XOR);
{$endif}

    function yylex : ttoken;
    procedure initscanner(const fn: string);
    procedure donescanner(compiled_at_higher_level : boolean);

    { the asm parser use this function getting the input }
    function asmgetchar : char;

    { this procedure is called at the end of each line }
    { and the function does the statistics }
    procedure write_line;
    { this procedure must be called before starting another scanner }
    procedure update_line;

    type
       tpreproctoken = (PP_IFDEF,PP_IFNDEF,PP_ELSE,PP_ENDIF,PP_IFOPT);

       ppreprocstack = ^tpreprocstack;

       tpreprocstack = object
          t : tpreproctoken;
          accept : boolean;
          next : ppreprocstack;
          name : string;
          line_nb : longint;
          constructor init(_t : tpreproctoken;a : boolean;n : ppreprocstack);
          destructor done;
       end;

    var
       pattern,orgpattern : string;

    { macros }

    const
{$ifdef TP}
       maxmacrolen = 1024;
{$else}
       maxmacrolen = 16*1024;
{$endif}

    type
       tmacrobuffer = array[0..maxmacrolen-1] of char;

    var
       macropos : longint;
       macrobuffer : ^tmacrobuffer;
       preprocstack : ppreprocstack;
       inputbuffer : pchar;
       inputpointer : word;
       s_point : boolean;
       c : char;
       comment_level : word;
{this is usefull to get the write filename
for the last instruction of an include file !}
       Const        FileHasChanged : Boolean = False;

  implementation

    uses
       pbase;

    const
       newline = #10;

    { const
       line_count : longint = 0; stored in tinputfile }

    { used to get better line info }
    procedure update_line;

      begin
         inc(current_module^.current_inputfile^.line_no,
           current_module^.current_inputfile^.line_count);
         current_module^.current_inputfile^.line_count:=0;
      end;

    procedure reload;

      var
         readsize : word;
         i,saveline,count : longint;

      begin
         if filehaschanged then
           begin
{$ifdef EXTDEBUG}
              writeln ('Note: Finished reading ',current_module^.current_inputfile^.name^);
              write  (' Coming back to ');
              current_module^.current_inputfile^.next^.write_file_line(output);
              writeln;
{$endif EXTDEBUG}
              current_module^.current_inputfile:=current_module^.current_inputfile^.next;

              { this was missing !}
              c:=inputbuffer[inputpointer];
              inc(inputpointer);
{$ifdef EXTDEBUG}
              write('Next 16 char "');
              for i:=-1 to 14 do
                write(inputbuffer[inputpointer+i]);
              writeln('"');
{$endif EXTDEBUG}
              filehaschanged:=false;
              exit;
           end;
         if current_module^.current_inputfile=nil then
           internalerror(14);
         if current_module^.current_inputfile^.filenotatend then
           begin
              { load the next piece of source }
              blockread(current_module^.current_inputfile^.f,inputbuffer^,
                current_module^.current_inputfile^.bufsize-1,readsize);
              { check if non-empty file }

              { this is an aweful hack FK }
              if readsize > 0 then
              begin
                { check if null character before readsize }
                { this mixed up the scanner..             }

                { force proper line counting }
                saveline:=current_module^.current_inputfile^.line_no;
                i:=0;
                while i<readsize do
                  begin
                     if inputbuffer[i] in [#10,#13] then
                       begin
                          if (byte(inputbuffer[i+1])+byte(inputbuffer[i])=23) then
                            inc(i);
                          inc(current_module^.current_inputfile^.line_no);
                       end;
                     if inputbuffer[i] = #0 then
                       Message(scan_f_illegal_char);
                     inc(i);
                  end;
                current_module^.current_inputfile^.line_no:=saveline;
              end;

              inputbuffer[readsize]:=#0;
              c:=inputbuffer[0];

              { inputpointer points always to the _next_ character to read }
              inputpointer:=1;
              if eof(current_module^.current_inputfile^.f) then
                begin
                   current_module^.current_inputfile^.filenotatend:=false;

                   { if this is the main source file then EOF }
                   if current_module^.current_inputfile^.next=nil then
                     inputbuffer[readsize]:=#26;
                end;
           end
         else
           begin
              current_module^.current_inputfile^.close;
              inputbuffer:=current_module^.current_inputfile^.next^.buf;
              inputpointer:=current_module^.current_inputfile^.next^.bufpos;

              if assigned(current_module^.current_inputfile^.next) then
                begin
                   c:=inputbuffer[inputpointer];
                   filehaschanged:=True;
{$ifdef EXTDEBUG}
                   write('Next 16 char "');
                   for i := 0 to 15 do write(inputbuffer[inputpointer+i]);
                     writeln('"');
{$endif}
                   inputbuffer[inputpointer] := #0;
                   { if c=newline writeline is called but increment the old
                     inputstack instead of the new one }
                   if c=newline then
                     begin
                        inc(current_module^.current_inputfile^.next^.line_no);
                        dec(current_module^.current_inputfile^.line_no);
                     end;
                end;
            end;
      end;


    procedure write_line;

      var
         status : tcompilestatus;

      begin
{$ifdef ver0_6}
         status.totalcompiledlines:=abslines;
         status.currentline:=current_module^.current_inputfile^.line_no
           +current_module^.current_inputfile^.line_count;
         status.currentsource:=current_module^.current_inputfile^.name^+current_module^.current_inputfile^.ext^;
         status.totallines:=0;
{$else}
         with status do
           begin
              totalcompiledlines:=abslines;
              currentline:=current_module^.current_inputfile^.line_no
                +current_module^.current_inputfile^.line_count;
              currentsource:=current_module^.current_inputfile^.name^+current_module^.current_inputfile^.ext^;
              totallines:=0;
           end;
{$endif}
         if compilestatusproc(status) then
          stop;
         inc(current_module^.current_inputfile^.line_count);
         lastlinepointer:=inputpointer;
         inc(abslines);
      end;

    procedure src_comment;forward;


    var prevc : char;

    procedure nextchar;

      begin
        c:=inputbuffer[inputpointer];
        inc(inputpointer);
        if c=#0 then
         reload;

        if c in [#10,#13] then
         begin
           { here there was a problem if the inputbuffer
             stopped at #13 and the next at #10
             because two newlines where counted !! }
           write_line;
           if (byte(inputbuffer[inputpointer])+byte(c)=23) then
            inc(inputpointer);
           if (inputbuffer[inputpointer]=#0) and
             current_module^.current_inputfile^.filenotatend then
             begin
                prevc:=c;
                reload;
                if (byte(c)+byte(prevc)=23) then
                  inc(inputpointer);
             end;
           c:=newline;
         end;
      end;


    procedure skipspace;
      var
        lastc : byte;
      begin
         lastc:=0;
         while c in [' ',#9,#10,#12,#13] do
           begin
             nextchar;
             if c='{' then
              src_comment;
           end;
      end;


    function is_keyword(var token : ttoken) : boolean;

      var
         m,n,k : integer;

      begin
         { there are no keywords with a length less than 2 }
         if length(pattern)<=1 then
           begin
              is_keyword:=false;
              exit;
           end;

         m:=1;
         n:=anz_keywords;
         while m<=n do
           begin
              k:=m+(n-m) shr 1;
              if pattern=keyword[k] then
                begin
                   token:=keyword_token[k];
                   is_keyword:=true;
                   exit;
                end
              else if pattern>keyword[k] then m:=k+1 else n:=k-1;
          end;
        is_keyword:=false;
     end;

{*****************************************************************************
                              Preprocessor
*****************************************************************************}

    function readmessage:string;
    var
      i : longint;
    begin
      i:=0;
      repeat
        case c of
         '}' : break;
         #26 : Message(scan_f_end_of_file);
        else
          begin
            if (i<255) then
             begin
               inc(i);
               readmessage[i]:=c;
             end;
          end;
        end;
        nextchar;
      until false;
      readmessage[0]:=chr(i);
    end;

    constructor tpreprocstack.init(_t : tpreproctoken;a : boolean;n : ppreprocstack);

      begin
         t:=_t;
         accept:=a;
         next:=n;
      end;

    destructor tpreprocstack.done;

      begin
      end;

    procedure dec_comment_level;

      begin
         if cs_tp_compatible in aktswitches then
           comment_level:=0
         else
           dec(comment_level);
      end;

    procedure handle_switches;

      function read_original_string : string;

        var
           hs : string;

        begin
           hs:='';
           while c in ['A'..'Z','a'..'z','_','0'..'9'] do
            begin
              hs:=hs+c;
              nextchar;
            end;
           read_original_string:=hs;
        end;

      function read_string : string;

        begin
           read_string:=upper(read_original_string);
        end;

      function read_number : longint;

        var
           hs : string;
           l : longint;
           w : word;

        begin
           read_number:=0;
           hs:='';
           while c in ['0'..'9'] do
             begin
                hs:=hs+c;
                nextchar;
             end;
           valint(hs,l,w);
           read_number:=l;
        end;

      var
         preprocpat : string;
         preproc_token : ttoken;

      function read_preproc : ttoken;

{        var
           y : ttoken;
           code : word;
           l : longint;
           hs : string;
           hp : pinputfile;
           hp2 : pchar;}
        label
           preproc_exit;


        begin
           while c in [' ',#9,#13,#12,#10] do
             begin
{                if c=#10 then write_line;}
                nextchar;
             end;
           case c of
              'A'..'Z','a'..'z','_','0'..'9' :
                   begin
                        preprocpat:=c;
                      nextchar;
                      while c in ['A'..'Z','a'..'z','0'..'9','_'] do
                        begin
                           preprocpat:=preprocpat+c;
                           nextchar;
                        end;
                      uppervar(preprocpat);
                      read_preproc:=ID;
                      goto preproc_exit;
                   end;
              '('      : begin
                            nextchar;
                            read_preproc:=LKLAMMER;
                            goto preproc_exit;
                         end;
              ')'      : begin
                            nextchar;
                            read_preproc:=RKLAMMER;
                            goto preproc_exit;
                         end;
              '+'      : begin
                            nextchar;
                              read_preproc:=PLUS;
                            goto preproc_exit;
                         end;
              '-'      : begin
                            nextchar;
                            read_preproc:=MINUS;
                            goto preproc_exit;
                         end;
              '*'      : begin
                            nextchar;
                            read_preproc:=STAR;
                            goto preproc_exit;
                         end;
              '/'      : begin
                            nextchar;
                            read_preproc:=SLASH;
                            goto preproc_exit;
                         end;
              '='      : begin
                            nextchar;
                            read_preproc:=EQUAL;
                            goto preproc_exit;
                         end;
              '>'      : begin
                            nextchar;
                            if c='=' then
                              begin
                                 nextchar;
                                 read_preproc:=GTE;
                                 goto preproc_exit;
                              end
                            else
                              begin
                                 read_preproc:=GT;
                                 goto preproc_exit;
                              end;
                         end;
              '<'      : begin
                            nextchar;
                            if c='>' then
                              begin
                                 nextchar;
                                 read_preproc:=UNEQUAL;
                                 goto preproc_exit;
                              end
                            else if c='=' then
                              begin
                                 nextchar;
                                 read_preproc:=LTE;
                                 goto preproc_exit;
                              end
                            else
                              begin
                                 read_preproc:=LT;
                                 goto preproc_exit;
                              end;
                         end;
              #26:
                begin
                   update_line;
                   Message(scan_f_end_of_file);
                end
              else
                begin
                   read_preproc:=_EOF;
                end;
           end;
        preproc_exit :
           update_line;
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

    procedure skip_until_pragma;
      var
        found : longint;
      begin
         found:=0;
         repeat
           case c of
            #26 : Message(scan_f_end_of_file);
    {        newline : begin
                         write_line;
                         found:=0;
                       end; }
            '{' : begin
                    if comment_level=0 then
                     found:=1;
                    inc(comment_level);
                  end;
            '}' : begin
                    dec_comment_level;
                    found:=0;
                  end;
            '$' : begin
                    if found=1 then
                     found:=2;
                  end;
           else
            found:=0;
           end;
           nextchar;
         until (found=2);
         update_line;
      end;

      function Is_conditional(const hs:string):boolean;
      begin
        Is_Conditional:=((hs='ELSE') or (hs='IFDEF') or (hs='IFNDEF') or
                        (hs='IFOPT') or (hs='ENDIF') or (hs='ELSE') or (hs='IF'));
      end;

      var
         path,hs : string;
         hp : pinputfile;
         mac : pmacrosym;
         found : boolean;
         ht : ttoken;

      procedure popstack;

        var
           hp : ppreprocstack;

        begin
           hp:=preprocstack^.next;
           dispose(preprocstack,done);
           preprocstack:=hp;
        end;

      var
         _d : dirstr;
         _n : namestr;
         _e : extstr;
         hs2,
         msg : string;

      begin
         nextchar;
         hs:=read_string;
         update_line;
         Message1(scan_d_handling_switch,hs);
         if hs='I' then
           begin
              skipspace;
              hs:=c;
              nextchar;
              while not(c in [' ','}','*',#13,newline]) do
                begin
                   hs:=hs+c;
                   nextchar;
                   if c=#26 then Message(scan_f_end_of_file);
                end;
{              if c=newline then write_line;}
              { read until end of comment }
              while c<>'}' do
                begin
                   nextchar;
                   if c=#26 then Message(scan_f_end_of_file);
{                   if c=newline then write_line;}
                end;
              {
              dec(comment_level);
              }
              { Initialization }

              if (hs[1]='-') then
                {exclude(aktswitches,cs_iocheck) Not yet supported.}
                aktswitches:=aktswitches-[cs_iocheck]
              else if (hs[1]='+') then
                {include(aktswitches,cs_iocheck) Not supported yet.}
                aktswitches:=aktswitches+[cs_iocheck]
              else
                begin
                   fsplit(hs,_d,_n,_e);
                   update_line;
                   { directory where the current file is first inspected }
                   path:=search(hs,current_module^.current_inputfile^.path^,found);
                   if found then
                     hp:=new(pinputfile,init(path+_d,_n,_e))
                   else
                     begin
                        path:=search(hs,includesearchpath,found);
                        hp:=new(pinputfile,init(path+_d,_n,_e));
                     end;
                   hp^.reset;
                   if ioresult=0 then
                     begin
                        current_module^.current_inputfile^.bufpos:=inputpointer;
                        hp^.next:=current_module^.current_inputfile;
                        current_module^.current_inputfile:=hp;
                        current_module^.sourcefiles.register_file(hp);

                        inputbuffer:=current_module^.current_inputfile^.buf;
                        Message1(scan_u_start_include_file,current_module^.current_inputfile^.name^);
                        reload;

                        { we have read the }
                        { comment end      }
                        dec_comment_level;
                        { only warn for over one => incompatible with BP }
                         if (comment_level>1) then
                          Message1(scan_w_comment_level,tostr(comment_level));
                     end
                   else
                     Message1(scan_f_cannot_open_includefile,_d+_n+_e);
                end;
           end
         { conditional compiling ? }
         else if Is_Conditional(hs) then
           begin
              while true do
                begin
                   if hs='ENDIF' then
                     begin
                        { we can always accept an ELSE }
                        if assigned(preprocstack) then
                          begin
                            Message1(scan_c_endif_found,preprocstack^.name);
                             if preprocstack^.t=PP_ELSE then
                               popstack;
                          end
                        else
                          Message(scan_e_endif_without_if);

                        { now pop the condition }
                        if assigned(preprocstack) then
                          begin
                             { we only use $ifdef in the stack }
                             if (preprocstack^.t=PP_IFDEF) then
                               popstack
                             else
                               Message(scan_e_too_much_endifs);
                          end
                       else
                          Message(scan_e_endif_without_if);
                     end
                   else if hs='IFDEF' then
                     begin
                        skipspace;
                        hs:=read_string;
                        mac:=pmacrosym(macros^.search(hs));
                        preprocstack:=new(ppreprocstack,init(PP_IFDEF,
                          { the block before must be accepted }
                          { the symbole must be exist and be defined }
                          (
                           (preprocstack=nil) or
                            preprocstack^.accept
                          ) and
                           assigned(mac) and
                           mac^.defined,
                          preprocstack));
                        preprocstack^.name:=hs;
                        preprocstack^.line_nb:=current_module^.current_inputfile^.line_no;
                        if preprocstack^.accept then
                         Message2(scan_c_ifdef_found,preprocstack^.name,'accepted')
                        else
                         Message2(scan_c_ifdef_found,preprocstack^.name,'rejected');
                     end
                   else if hs='IFOPT' then
                     begin
                        skipspace;
                        hs:=read_string;
                        { !!!! read switch state }

                        { PP_IFDEF is correct, we doesn't distinguish between }
                        { ifopt and ifdef                                     }
                        preprocstack:=new(ppreprocstack,init(PP_IFDEF,
                          { the block before must be accepted }
                          (
                           (preprocstack=nil) or
                            preprocstack^.accept
                          ) and
                          { !!!! subject to change: }
                          false,
                          preprocstack));
                        preprocstack^.name:=hs;
                        preprocstack^.line_nb:=current_module^.current_inputfile^.line_no;
                        if preprocstack^.accept then
                         Message2(scan_c_ifopt_found,preprocstack^.name,'accepted')
                        else
                         Message2(scan_c_ifopt_found,preprocstack^.name,'rejected');
                     end
                   else if hs='IF' then
                     begin
                        skipspace;
                        { start preproc expression scanner }
                        preproc_token:=read_preproc;
                        hs:=read_expr;

                        { PP_IFDEF is correct, we doesn't distinguish between }
                        { if, ifopt and ifdef                                 }
                        preprocstack:=new(ppreprocstack,init(PP_IFDEF,
                          { the block before must be accepted }
                          (
                           (preprocstack=nil) or
                            preprocstack^.accept
                          ) and
                          (hs<>'0'),
                          preprocstack));
                        preprocstack^.name:=hs;
                        preprocstack^.line_nb:=current_module^.current_inputfile^.line_no;
                        if preprocstack^.accept then
                         Message2(scan_c_if_found,preprocstack^.name,'accepted')
                        else
                         Message2(scan_c_if_found,preprocstack^.name,'rejected');
                     end
                   else if hs='IFNDEF' then
                     begin
                        skipspace;
                        hs:=read_string;
                        mac:=pmacrosym(macros^.search(hs));
                        preprocstack:=new(ppreprocstack,init(PP_IFDEF,
                          { the block before must be accepted }
                          (
                           (preprocstack=nil) or
                           preprocstack^.accept
                          ) and
                           not(assigned(mac) and
                           mac^.defined),
                          preprocstack));
                        preprocstack^.name:=hs;
                        preprocstack^.line_nb:=current_module^.current_inputfile^.line_no;
                        if preprocstack^.accept then
                         Message2(scan_c_ifndef_found,preprocstack^.name,'accepted')
                        else
                         Message2(scan_c_ifndef_found,preprocstack^.name,'rejected');
                     end
                   else if hs='ELSE' then
                     begin
                        if assigned(preprocstack) then
                          begin
                             preprocstack:=new(ppreprocstack,init(PP_ELSE,
                             { invert }
                             not(preprocstack^.accept) and
                             { but only true, if only the ifdef block is }
                             { not accepted                              }
                             (
                               (preprocstack^.next=nil) or
                               (preprocstack^.next^.accept)
                             ),
                             preprocstack));
                             preprocstack^.line_nb := current_module^.current_inputfile^.line_no;
                             preprocstack^.name := preprocstack^.next^.name;
                             if preprocstack^.accept then
                              Message2(scan_c_else_found,preprocstack^.name,'accepted')
                             else
                              Message2(scan_c_else_found,preprocstack^.name,'rejected');
                          end
                        else
                          Message(scan_e_endif_without_if);
                     end
                   else if hs='IFOPT' then
                     begin
                        skipspace;
                        hs:=read_string;
                        preprocstack:=new(ppreprocstack,init(PP_IFDEF,
                          false,
                          preprocstack));
                     end;

                   { accept the text ? }
                   if (preprocstack=nil) or preprocstack^.accept then
                     break
                   else
                     begin
                       Message(scan_c_skipping_until);
                       repeat
                          skip_until_pragma;
                          hs:=read_string;
                       until Is_Conditional(hs);
                     end;
                end;
           end
         else if (hs='WAIT') then
           begin
              Message(scan_i_press_enter);
              readln;
           end
         else if (hs='INFO') or (hs='MESSAGE') then
           begin
              skipspace;
              Message1(scan_i_user_defined,readmessage);
           end
         else if hs='NOTE' then
           begin
              skipspace;
              Message1(scan_n_user_defined,readmessage);
           end
         else if hs='WARNING' then
           begin
              skipspace;
              Message1(scan_w_user_defined,readmessage);
           end
         else if hs='ERROR' then
           begin
              skipspace;
              Message1(scan_e_user_defined,readmessage);
           end
         else if (hs='FATALERROR') or (hs='STOP') then
           begin
              skipspace;
              Message1(scan_f_user_defined,readmessage);
           end
         else if hs='L' then
           begin
              skipspace;
              hs:='';
              while not(c in [' ','}',#9,newline,#13]) do
                begin
                   hs:=hs+c;
                   nextchar;
                   if c=#26 then Message(scan_f_end_of_file);
                end;
              hs:=FixFileName(hs);
              if not path_absolute(hs) and (current_module^.current_inputfile^.path<>nil) then
               path:=search(hs,current_module^.current_inputfile^.path^+';'+objectsearchpath,found);
              Linker.AddObjectFile(path+hs);
              current_module^.linkofiles.insert(hs);
           end
         else if hs='D' then
           begin
              if current_module^.in_main then
                Message(scan_w_switch_is_global)
              else
                begin
                   if c='-' then
                    aktswitches:=aktswitches-[cs_debuginfo]
                   else
                    aktswitches:=aktswitches+[cs_debuginfo];
                end;
           end
         else if hs='R' then
           begin
               if c='-' then
                {exclude(aktswitches,cs_rangechecking) Not yet supported.}
                aktswitches:=aktswitches-[cs_rangechecking]
               else
                {include(aktswitches,cs_rangechecking); Not yet supported.}
                aktswitches:=aktswitches+[cs_rangechecking];
           end
         else if hs='Q' then
           begin
               if c='-' then
                 {include(aktswitches,cs_check_overflow) Not yet supported.}
                 aktswitches:=aktswitches-[cs_check_overflow]
               else
                 {include(aktswitches,cs_check_overflow); Not yet supported.}
                 aktswitches:=aktswitches+[cs_check_overflow]
           end
         else if hs='T' then
           begin
               if c='-' then
                 aktswitches:=aktswitches-[cs_typed_addresses]
               else
                 aktswitches:=aktswitches+[cs_typed_addresses]
           end
         else if hs='V' then
           begin
               if c='-' then
                 aktswitches:=aktswitches-[cs_strict_var_strings]
               else
                 aktswitches:=aktswitches+[cs_strict_var_strings]
           end
         else if hs='F' then
           begin
               Message(scan_n_far_directive_ignored);
           end
         else if hs='S' then
           begin
              if target_info.target<>target_linux then
                begin
                  case c of
                   '-' : aktswitches:=aktswitches-[cs_check_stack];
                   '+' : aktswitches:=aktswitches+[cs_check_stack];
                  else
                   Message(scan_w_illegal_switch);
                  end;
                end
              else
                begin
                   if c in ['+','-'] then
                     Message(scan_n_stack_check_global_under_linux)
                   else
                     Message(scan_w_illegal_switch);
                 end;
           end
         else if hs='E' then
           begin
              { This is a global switch which affects all units }
              if ((current_module = main_module) and (main_module^.in_main = false)) then
                begin
                  case c of
                   '-' : aktswitches:=aktswitches-[cs_fp_emulation];
                   '+' : aktswitches:=aktswitches+[cs_fp_emulation];
                  else
                   Message(scan_w_illegal_switch);
                  end;
                end
              else
                Message(scan_w_switch_is_global);
           end
         else if hs='X' then
           begin
              { This is a global switch which only affects the unit/program }
              { being compiled                                              }
              if not (current_module^.in_main) then
                begin
                  case c of
                   '-' : aktswitches:=aktswitches-[cs_extsyntax];
                   '+' : aktswitches:=aktswitches+[cs_extsyntax];
                  else
                   Message(scan_w_illegal_switch);
                  end;
                end
              else
               Message(scan_w_switch_is_global);
           end
         else if hs='LINKLIB' then
           begin
             skipspace;
             hs:=read_original_string;
             Linker.AddLibraryFile(hs);
             current_module^.linklibfiles.insert(hs);
           end
{$ifdef i386}
         else if hs='OUTPUT_FORMAT' then
           begin
              { this is a global switch }
              if current_module^.in_main then
               Message(scan_w_switch_is_global)
              else
                begin
                   skipspace;
                   hs:=upper(read_string);
                   if hs='NASM' then
                     current_module^.output_format:=of_nasm
                   else if hs='MASM' then
                     current_module^.output_format:=of_masm
                   else if hs='O' then
                     current_module^.output_format:=of_o
                   else if hs='OBJ' then
                     current_module^.output_format:=of_obj
                   else
                     Message(scan_w_illegal_switch);
                end;
              { for use in globals }
              output_format:=current_module^.output_format;
           end
{$endif}
{$ifdef SUPPORT_MMX}
         else if hs='MMX' then
           begin
               if c='-' then
                 aktswitches:=aktswitches-[cs_mmx]
               else
                 aktswitches:=aktswitches+[cs_mmx];
           end
         else if hs='SATURATION' then
           begin
               if c='-' then
                 aktswitches:=aktswitches-[cs_mmx_saturation]
               else
                 aktswitches:=aktswitches+[cs_mmx_saturation];
           end
{$endif SUPPORT_MMX}
         else if hs='DEFINE' then
           begin
              skipspace;
              hs:=read_string;
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

                   skipspace;
                   { !!!!!! handle macro params, need we this? }

                   { may be a macro? }
                   if c=':' then
                     begin
                        nextchar;
                        if c='=' then
                          begin
                             { first char }
                             nextchar;
                             macropos:=0;
                             while (c<>'}') do
                               begin
                                  macrobuffer^[macropos]:=c;
{                                  if c=newline then write_line;}
                                  nextchar;
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
           end
         else if hs='UNDEF' then
           begin
              skipspace;
              hs:=read_string;
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
           end
         else if hs='PACKRECORDS' then
           begin
              skipspace;
              if upcase(c)='N' then
                begin
                   hs:=read_string;
                   if hs='NORMAL' then
                     aktpackrecords:=2
                   else
                    Message(scan_w_only_pack_records);
                end
              else
                case read_number of
                   1 : aktpackrecords:=1;
                   2 : aktpackrecords:=2;
                   4 : aktpackrecords:=4;
                   else Message(scan_w_only_pack_records);
                end;
           end
{$ifdef i386}
         else if hs='I386_INTEL' then
           aktasmmode:=I386_INTEL
         else if hs='I386_DIRECT' then
           aktasmmode:=I386_DIRECT
         else if hs='I386_ATT' then
           aktasmmode:=I386_ATT
{$endif}
         else
           begin
              Message(scan_w_illegal_switch);
           end;
      end;

    procedure src_comment;

      begin
         inc(comment_level);
         { only warn for over one => incompatible with BP }
         if (comment_level>1) then
          Message1(scan_w_comment_level,tostr(comment_level));
         nextchar;
         while true do
           begin
              { handle compiler switches }
              if (comment_level=1) and (c='$') then
                handle_switches;
              { handle_switches can dec comment_level, }
              { if there is an include file             }
              while (c<>'}') and (comment_level>0) do
                begin
                   if c='{' then
                     src_comment
                   else
                     begin
                        if c=#26 then Message(scan_f_end_of_file);
{                        if c=newline then write_line;}
                        nextchar;
                     end;
                end;
              { this is needed for the include files      }
              { if there is a end of comment then read it }
              if c='}' then
                begin
                   nextchar;
                   dec_comment_level;
                   { only warn for over one => incompatible with BP }
                   if (comment_level>1) then
                    Message1(scan_w_comment_level,tostr(comment_level));
                end;
              { checks }{ }
              if c='{' then
                begin
                   inc(comment_level);
                   { only warn for over one => incompatible with BP }
                   if (comment_level>1) then
                    Message1(scan_w_comment_level,tostr(comment_level));
                   nextchar;
                end
              else
                break;
           end;
      end;

    procedure delphi_comment;
      begin
        { C++/Delphi styled comment }
        inc(comment_level);
        nextchar;
        { this is currently not supported }
        if c='$' then
          Message(scan_e_wrong_styled_switch);
        while c<>newline do
          begin
             if c=#26 then Message(scan_f_end_of_file);
             nextchar;
          end;
        dec(comment_level);
      end;

   const
      yylexcount : longint = 0;

   function yylex : ttoken;

     var
        y : ttoken;
        code : word;
        l : longint;
        hs : string;
        mac : pmacrosym;
        hp : pinputfile;
        hp2 : pchar;
     label
        yylex_exit;

     begin
        { was the last character a point ? }

        { this code is needed because the scanner if there is a 1. found if  }
        { this is a floating point number or range like 1..3                 }
        if s_point then
          begin
             s_point:=false;
             if c='.' then
               begin
                  nextchar;
                  yylex:=POINTPOINT;
                  goto yylex_exit;
               end;
             yylex:=POINT;
             goto yylex_exit;
          end;

        if c='{' then src_comment;
        skipspace;
        lasttokenpos:=inputpointer-1;
        case c of
           'A'..'Z','a'..'z','_' :
                begin
                   orgpattern:=c;
                   nextchar;
                   while c in ['A'..'Z','a'..'z','0'..'9','_'] do
                     begin
                        orgpattern:=orgpattern+c;
                        nextchar;
                     end;
                   pattern:=orgpattern;
                   uppervar(pattern);
                   if is_keyword(y) then
                     yylex:=y
                   else
                     begin
                        { this takes some time ... }
                        if support_macros then
                          begin
                             mac:=pmacrosym(macros^.search(pattern));
                             if assigned(mac) and (assigned(mac^.buftext)) then
                               begin
                                  { don't forget the last char }
                                  dec(inputpointer);
                                  current_module^.current_inputfile^.bufpos:=inputpointer;

                                  { this isn't a proper way, but ... }
                                  hp:=new(pinputfile,init('','Macro '+pattern,''));

                                  hp^.next:=current_module^.current_inputfile;
                                  current_module^.current_inputfile:=hp;
                                  current_module^.sourcefiles.register_file(hp);

                                  { set an own buffer }
                                  getmem(hp2,mac^.buflen+1);
                                  current_module^.current_inputfile^.setbuf(hp2,mac^.buflen+1);

                                  inputbuffer:=current_module^.current_inputfile^.buf;

                                  { copy text }
                                  move(mac^.buftext^,inputbuffer^,mac^.buflen);

                                  { put end sign }
                                  inputbuffer[mac^.buflen+1]:=#0;

                                  { load c }
                                  c:=inputbuffer[0];

                                  { point to the next char }
                                  inputpointer:=1;

                                  { handle empty macros }
                                  if c=#0 then reload;

                                  { play it again ... }
                                  inc(yylexcount);
                                  if yylexcount>16 then
                                    Message(scan_w_macro_deep_ten);
{$ifdef TP}
                                  yylex:=yylex;
{$else}
                                  yylex:=yylex();
{$endif}
                                { that's all folks }
                                dec(yylexcount);
                                goto yylex_exit;
                              end;
                           end;
                           yylex:=ID;
                        end;
                      goto yylex_exit;
                   end;
           '$'      : begin
                         pattern:=c;
                         nextchar;
                         while ((ord(c)>=ord('0')) and (ord(c)<=ord('9'))) or
                                (ord(upcase(c))>=ord('A')) and (ord(upcase(c))<=ord('F')) do
                           begin
                              pattern:=pattern+c;
                              nextchar;
                           end;
                         yylex:=INTCONST;
                         goto yylex_exit;
                      end;
{why ?ifdef FPC}
{ because the tp val doesn't recognize this, }
{ so it's useless in TP versions             }
{ it's solved with valint                    }
           '%'      : begin
                         pattern:=c;
                         nextchar;
                         while c in ['0','1'] do
                           begin
                              pattern:=pattern+c;
                              nextchar;
                           end;
                         yylex:=INTCONST;
                         goto yylex_exit;
                      end;
{cond removed endif}
           '0'..'9' : begin
                         pattern:=c;
                         nextchar;
                         while c in ['0'..'9'] do
                           begin
                              pattern:=pattern+c;
                              nextchar;
                           end;
                         if c in ['.','e','E'] then
                           begin
                              if c='.' then
                                begin
                                   nextchar;
                                   if not(c in ['0'..'9']) then
                                     begin
                                        s_point:=true;
                                        yylex:=INTCONST;
                                        goto yylex_exit;
                                     end;
                                   pattern:=pattern+'.';
                                   while c in ['0'..'9'] do
                                     begin
                                        pattern:=pattern+c;
                                        nextchar;
                                     end;
                                end;
                              if upcase(c)='E' then
                                begin
                                   pattern:=pattern+'E';
                                   nextchar;
                                   if c in ['-','+'] then
                                     begin
                                        pattern:=pattern+c;
                                        nextchar;
                                     end;
                                   if not(c in ['0'..'9']) then
                                     Message(scan_f_illegal_char);
                                   while c in ['0'..'9'] do
                                     begin
                                        pattern:=pattern+c;
                                        nextchar;
                                     end;
                                end;
                              yylex:=REALNUMBER;
                              goto yylex_exit;
                           end;
                         yylex:=INTCONST;
                         goto yylex_exit;
                      end;
           ';'      : begin
                         nextchar;
                         yylex:=SEMICOLON;
                         exit;
                      end;
           '['      : begin
                         nextchar;
                         yylex:=LECKKLAMMER;
                         goto yylex_exit;
                      end;
           ']'      : begin
                         nextchar;
                         yylex:=RECKKLAMMER;
                         goto yylex_exit;
                      end;
           '('      : begin
                         nextchar;
                         if c='*' then
                           begin
                              inc(comment_level);
                              nextchar;
                              while true do
                                begin
                                   { this is currently not supported }
                                   if c='$' then
                                    Message(scan_e_wrong_styled_switch);
                                   repeat
                                      while c<>'*' do
                                        begin
                                           if c=#26 then Message(scan_f_end_of_file);
{                                           if c=newline then write_line;}
                                           nextchar;
                                        end;
                                      if c=#26 then Message(scan_f_end_of_file);
                                      {if c=newline then write_line;}
                                      nextchar;
                                   until c=')';
                                   dec(comment_level);

                                   nextchar;
                                   { check for *)(* }
                                   if c='(' then
                                     begin
                                        nextchar;
                                        if c<>'*' then
                                          begin
                                             yylex:=LKLAMMER;
                                             goto yylex_exit;
                                          end;
                                        inc(comment_level);
                                        nextchar;
                                     end
                                   else
                                     begin
{$ifndef TP}
                                        yylex:=yylex();
{$else TP}
                                        yylex:=yylex;
{$endif TP}
                                        goto yylex_exit;
                                     end;
                                end;
                           end;
                         yylex:=LKLAMMER;
                         goto yylex_exit;
                      end;

           ')'      : begin
                         nextchar;
                         yylex:=RKLAMMER;
                         goto yylex_exit;
                      end;
           '+'      : begin
                         nextchar;
                         if (c='=') and c_like_operators then
                           begin
                              nextchar;
                              yylex:=_PLUSASN;
                              goto yylex_exit;
                           end
                         else
                           begin
                              yylex:=PLUS;
                              goto yylex_exit;
                           end;
                      end;
           '-'      : begin
                         nextchar;
                         if (c='=') and c_like_operators then
                           begin
                              nextchar;
                              yylex:=_MINUSASN;
                              goto yylex_exit;
                           end
                         else
                           begin
                              yylex:=MINUS;
                              goto yylex_exit;
                           end;
                      end;
           ':'      : begin
                         nextchar;
                         if c='=' then
                           begin
                              nextchar;
                              yylex:=ASSIGNMENT;
                              goto yylex_exit;
                           end
                         else
                           begin
                              yylex:=COLON;
                              goto yylex_exit;
                           end;
                      end;
           '*'      : begin
                         nextchar;
                         if (c='=') and c_like_operators then
                           begin
                              nextchar;
                              yylex:=_STARASN;
                              goto yylex_exit;
                           end
                         else
                           begin
                              yylex:=STAR;
                              goto yylex_exit;
                           end;
                      end;
           '/'      : begin
                         nextchar;
                         if (c='=') and c_like_operators then
                           begin
                              nextchar;
                              yylex:=_SLASHASN;
                              goto yylex_exit;
                           end
                         else if (c='/') then
                           begin
                              delphi_comment;
{$ifndef TP}
                              yylex:=yylex();
{$else TP}
                              yylex:=yylex;
{$endif TP}
                              goto yylex_exit;
                           end
                         else
                           begin
                              yylex:=SLASH;
                              goto yylex_exit;
                           end;
                      end;
           '='      : begin
                         nextchar;
                         yylex:=EQUAL;
                         goto yylex_exit;
                      end;
           '.'      : begin
                         nextchar;
                         if c='.' then
                           begin
                              nextchar;
                              yylex:=POINTPOINT;
                              goto yylex_exit;
                           end
                         else
                         yylex:=POINT;
                         goto yylex_exit;
                      end;
           '@'      : begin
                         nextchar;
                         if c='@' then
                           begin
                              nextchar;
                              yylex:=DOUBLEADDR;
                           end
                         else
                           yylex:=KLAMMERAFFE;
                         goto yylex_exit;
                      end;
           ','      : begin
                         nextchar;
                         yylex:=COMMA;
                         exit;
                      end;
           '''','#','^' :
                      begin
                         if c='^' then
                           begin
                              nextchar;
                              c:=upcase(c);
                              if not(block_type=bt_type) and (c in ['A'..'Z']) then
                                begin
                                   pattern:=chr(ord(c)-64);
                                   nextchar;
                                end
                              else
                                begin
                                   yylex:=CARET;
                                   goto yylex_exit;
                                end;
                           end
                         else pattern:='';
                         while true do
                           case c of
                             '#' :
                                begin
                                   hs:='';
                                   nextchar;
                                   if c='$' then
                                     begin
                                        hs:='$';
                                        nextchar;
                                        while c in (['0'..'9','a'..'f','A'..'F']) do
                                          begin
                                             hs:=hs+upcase(c);
                                             nextchar;
                                          end;
                                     end
                                   else
                                   { FPC supports binary constants }
                                   { %10101 evalutes to 37         }
                                   if c='%' then
                                     begin
                                        nextchar;
                                        while c in ['0','1'] do
                                          begin
                                             hs:=hs+upcase(c);
                                             nextchar;
                                          end;
                                     end
                                   else
                                     begin
                                        while (ord(c)>=ord('0')) and (ord(c)<=ord('9')) do
                                          begin
                                             hs:=hs+c;
                                             nextchar;
                                          end;
                                     end;
                                   valint(hs,l,code);
                                   if (code<>0) or (l<0) or (l>255) then
                                     Message(scan_e_illegal_char_const);
                                    pattern:=pattern+chr(l);
                                 end;
                             '''' :
                                begin
                                   repeat
                                     nextchar;
                     case c of
                       #26 : begin
                               Message(scan_f_end_of_file);
                               break;
                             end;
                       #13,
                               newline : begin
                                            Message(scan_f_string_exceeds_line);
                                            break;
                                         end;
                       '''' : begin
                                  nextchar;
                                  if c<>'''' then
                                      break;
                              end;
                   end;
                                     pattern:=pattern+c;
                                   until false;
                                end;
                             '^' : begin
                                      nextchar;
                                      c:=upcase(c);
                                      if c in ['A'..'Z'] then
                                        pattern:=pattern+chr(ord(c)-64)
                                      else Message(scan_f_illegal_char);
                                      nextchar;
                                   end;
                             else break;
                           end;
                         { strings with length 1 become const chars }
                         if length(pattern)=1 then
                           yylex:=CCHAR
                           else yylex:=CSTRING;
                         goto yylex_exit;
                      end;
           '>'      : begin
                         nextchar;
                         if c='=' then
                           begin
                              nextchar;
                              yylex:=GTE;
                              goto yylex_exit;
                           end
                         else if c='>' then
                           begin
                              nextchar;
                              yylex:=_SHR;
                              goto yylex_exit;
                           end
                         else if c='<' then
                           begin
                              nextchar;
                              { >< is for a symetric diff for sets }
                              yylex:=SYMDIF;
                              goto yylex_exit;
                           end
                         else
                           begin
                              yylex:=GT;
                              goto yylex_exit;
                           end;
                      end;
           '<'      : begin
                         nextchar;
                         if c='>' then
                           begin
                              nextchar;
                              yylex:=UNEQUAL;
                              goto yylex_exit;
                           end
                         else if c='=' then
                           begin
                              nextchar;
                              yylex:=LTE;
                              goto yylex_exit;
                           end
                         else if c='<' then
                           begin
                              nextchar;
                              yylex:=_SHL;
                              goto yylex_exit;
                           end
                         else
                           begin
                              yylex:=LT;
                              goto yylex_exit;
                           end;
                      end;
           #26      : begin
                         yylex:=_EOF;
                         goto yylex_exit;
                      end;
           else
             begin
                update_line;
                Message(scan_f_illegal_char);
             end;
           end;
     yylex_exit :
        update_line;
     end;

    const last_asmgetchar_was_a_comment : boolean = false;

    function asmgetchar : char;
      begin
         if c='{' then
           begin
              src_comment;
              { a comment is a seperator }
              asmgetchar:=';';
              last_asmgetchar_was_a_comment:=true;
           end
         else
           begin
              update_line;
              if last_asmgetchar_was_a_comment then
                begin
                   last_asmgetchar_was_a_comment:=false;
                   asmgetchar:=c;
                   exit;
                end;
              nextchar;
              asmgetchar:=c;
              if c='/' then
               begin
                 nextchar;
                 if c='/' then
                  begin
                    delphi_comment;
                    asmgetchar:=c;
                  end
                 else
                  begin
                    last_asmgetchar_was_a_comment:=true;
                    asmgetchar:='/';
                  end;
               end;
           end;
      end;

   procedure initscanner(const fn: string);
     var
       d:dirstr;
       n:namestr;
       e:extstr;
     begin
        fsplit(fn,d,n,e);

        current_module^.current_inputfile:=new(pinputfile,init(d,n,e));
        current_module^.current_inputfile^.reset;

        current_module^.sourcefiles.register_file(current_module^.current_inputfile);

        if ioresult<>0 then
         Message(scan_f_cannot_open_input);

        inputbuffer:=current_module^.current_inputfile^.buf;
        preprocstack:=nil;
        reload;
        comment_level:=0;
        lasttokenpos:=0;
        lastlinepointer:=0;
        s_point:=false;
     end;

   procedure donescanner(compiled_at_higher_level : boolean);

     var
        st : string;

     begin
        if not (compiled_at_higher_level) and  assigned(preprocstack) then
          begin
             if preprocstack^.t=PP_IFDEF then
               st:='$IF(N)(DEF)'
             else
               st:='$ELSE';
             Message3(scan_e_endif_expected,st,preprocstack^.name,tostr(preprocstack^.line_nb));
          end;
     end;

end.
{
  $Log$
  Revision 1.5  1998-04-07 22:45:05  florian
    * bug0092, bug0115 and bug0121 fixed
    + packed object/class/array

  Revision 1.4  1998/04/07 13:19:49  pierre
    * bugfixes for reset_gdb_info
      in MEM parsing for go32v2
      better external symbol creation
      support for rhgdb.exe (lowercase file names)

  Revision 1.3  1998/03/29 17:27:59  florian
    * aopt386 compiles with TP
    * correct line number is displayed, if a #0 is in the input

  Revision 1.2  1998/03/28 23:09:57  florian
    * secondin bugfix (m68k and i386)
    * overflow checking bugfix (m68k and i386) -- pretty useless in
      secondadd, since everything is done using 32-bit
    * loading pointer to routines hopefully fixed (m68k)
    * flags problem with calls to RTL internal routines fixed (still strcmp
      to fix) (m68k)
    * #ELSE was still incorrect (didn't take care of the previous level)
    * problem with filenames in the command line solved
    * problem with mangledname solved
    * linking name problem solved (was case insensitive)
    * double id problem and potential crash solved
    * stop after first error
    * and=>test problem removed
    * correct read for all float types
    * 2 sigsegv fixes and a cosmetic fix for Internal Error
    * push/pop is now correct optimized (=> mov (%esp),reg)

  Revision 1.1.1.1  1998/03/25 11:18:15  root
  * Restored version

  Revision 1.43  1998/03/24 21:48:34  florian
    * just a couple of fixes applied:
         - problem with fixed16 solved
         - internalerror 10005 problem fixed
         - patch for assembler reading
         - small optimizer fix
         - mem is now supported

  Revision 1.42  1998/03/10 17:19:29  peter
    * fixed bug0108
    * better linebreak scanning (concentrated in nextchar(), it supports
      #10, #13, #10#13, #13#10

  Revision 1.41  1998/03/10 16:27:45  pierre
    * better line info in stabs debug
    * symtabletype and lexlevel separated into two fields of tsymtable
    + ifdef MAKELIB for direct library output, not complete
    + ifdef CHAINPROCSYMS for overloaded seach across units, not fully
      working
    + ifdef TESTFUNCRET for setting func result in underfunction, not
      working

  Revision 1.40  1998/03/10 01:17:27  peter
    * all files have the same header
    * messages are fully implemented, EXTDEBUG uses Comment()
    + AG... files for the Assembler generation

  Revision 1.39  1998/03/09 12:58:14  peter
    * FWait warning is only showed for Go32V2 and $E+
    * opcode tables moved to i386.pas/m68k.pas to reduce circular uses (and
      for m68k the same tables are removed)
    + $E for i386

  Revision 1.38  1998/03/06 00:52:52  peter
    * replaced all old messages from errore.msg, only ExtDebug and some
      Comment() calls are left
    * fixed options.pas

  Revision 1.37  1998/03/04 17:34:06  michael
  + Changed ifdef FPK to ifdef FPC

  Revision 1.36  1998/03/03 22:38:34  peter
    * the last 3 files

  Revision 1.35  1998/03/02 01:49:26  peter
    * renamed target_DOS to target_GO32V1
    + new verbose system, merged old errors and verbose units into one new
      verbose.pas, so errors.pas is obsolete

  Revision 1.34  1998/02/26 11:57:16  daniel
  * New assembler optimizations commented out, because of bugs.
  * Use of dir-/name- and extstr.

  Revision 1.33  1998/02/22 23:03:32  peter
    * renamed msource->mainsource and name->unitname
    * optimized filename handling, filename is not seperate anymore with
      path+name+ext, this saves stackspace and a lot of fsplit()'s
    * recompiling of some units in libraries fixed
    * shared libraries are working again
    + $LINKLIB <lib> to support automatic linking to libraries
    + libraries are saved/read from the ppufile, also allows more libraries
      per ppufile

  Revision 1.32  1998/02/17 21:20:59  peter
    + Script unit
    + __EXIT is called again to exit a program
    - target_info.link/assembler calls
    * linking works again for dos
    * optimized a few filehandling functions
    * fixed stabs generation for procedures

  Revision 1.31  1998/02/16 12:51:44  michael
  + Implemented linker object

  Revision 1.30  1998/02/13 10:35:45  daniel
  * Made Motorola version compilable.
  * Fixed optimizer

  Revision 1.29  1998/02/12 17:19:25  florian
    * fixed to get remake3 work, but needs additional fixes (output, I don't like
      also that aktswitches isn't a pointer)

  Revision 1.28  1998/02/12 11:50:44  daniel
  Yes! Finally! After three retries, my patch!

  Changes:

  Complete rewrite of psub.pas.
  Added support for DLL's.
  Compiler requires less memory.
  Platform units for each platform.

  Revision 1.27  1998/02/07 09:39:27  florian
    * correct handling of in_main
    + $D,$T,$X,$V like tp

  Revision 1.26  1998/02/05 22:27:06  florian
    * small problems fixed: remake3 should now work

  Revision 1.25  1998/02/03 22:13:35  florian
    * clean up

  Revision 1.24  1998/02/02 23:42:38  florian
    * data is now dword aligned per default else the stack ajustements are useless
    + $wait directive: stops compiling til return is presseed (a message is
      also written, useful to give the user a change to notice a message

  Revision 1.23  1998/02/02 13:13:28  pierre
    * line_count transfered to tinputfile, to avoid crosscounting

  Revision 1.22  1998/01/30 17:30:10  pierre
    + better line counting mechanism
      line count updated only when important tokens are read
      (not for comment , ; )

  Revision 1.21  1998/01/26 19:09:52  peter
    * fixed EOF in open string constant reading

  Revision 1.20  1998/01/22 08:56:55  peter
    * Fixed string exceeds end of line problem (#13 is not a linux
      linebreak)

  Revision 1.19  1998/01/20 18:18:46  peter
    * fixed skip_until_pragma, bug0044 and the compiler recompile good

  Revision 1.18  1998/01/20 16:30:17  pierre
    * bug with braces in log from Peter removed

  Revision 1.17  1998/01/20 15:14:33  peter
    * fixes bug 44 with multiple $'s between skipped $IFDEF and $ENDIF

  Revision 1.16  1998/01/13 16:16:06  pierre
    *  bug in interdependent units handling
       - primary unit was not in loaded_units list
       - current_module^.symtable was assigned too early
       - donescanner must not call error if the compilation
       of the unit was done at a higher level.

  Revision 1.15  1998/01/09 23:08:34  florian
    + C++/Delphi styled //-comments
    * some bugs in Delphi object model fixed
    + override directive

  Revision 1.14  1998/01/09 18:01:17  florian
    * VIRTUAL isn't anymore a common keyword
    + DYNAMIC is equal to VIRTUAL

  Revision 1.13  1998/01/09 13:39:57  florian
    * public, protected and private aren't anymore key words
    + published is equal to public

  Revision 1.12  1997/12/12 13:28:41  florian
  + version 0.99.0
  * all WASM options changed into MASM
  + -O2 for Pentium II optimizations

  Revision 1.11  1997/12/10 23:07:30  florian
  * bugs fixed: 12,38 (also m68k),39,40,41
  + warning if a system unit is without -Us compiled
  + warning if a method is virtual and private (was an error)
  * some indentions changed
  + factor does a better error recovering (omit some crashes)
  + problem with @type(x) removed (crashed the compiler)

  Revision 1.10  1997/12/09 14:09:15  carl
  * bugfix of Runerror 216 when reading a null character (such as trying to
    compile a binary file)

  Revision 1.9  1997/12/08 11:51:12  pierre
    * corrected some buggy code in hexadecimal number reading

  Revision 1.8  1997/12/05 14:22:20  daniel
  * Did some source code beutification.

  Revision 1.7  1997/12/03 13:43:14  carl
  + OUTPUT_FORMAT switch is processor specific to i386.

  Revision 1.6  1997/12/02 16:00:55  carl
  * bugfix of include files - now gives out a fatalerror if not found,
  otherwise would create invalid pointer operations everywhere.
  * bugfix of $i+xyz now the $i+/- switch is correctly recognized as io
  checking and ont an include directive.

  Revision 1.5  1997/11/28 18:14:48  pierre
   working version with several bug fixes

  Revision 1.4  1997/11/28 14:26:26  florian
  Fixed some bugs

  Revision 1.3  1997/11/27 17:47:14  carl
  * fixed bug with assem switches and m68k.

  Revision 1.2  1997/11/27 17:40:48  carl
  + assem type scanning switches for intel targets.

  Revision 1.1.1.1  1997/11/27 08:33:01  michael
  FPC Compiler CVS start

  Pre-CVS log:

  CEC    Carl-Eric Codere
  FK     Florian Klaempfl
  PM     Pierre Muller
  +      feature added
  -      removed
  *      bug fixed or changed

  History:
       6th september 1997:
         + added support for global switches (i.e $X and $E (for m68k)) (CEC)
       1st october 1997:
         + added $ifopt as dummy which is always rejected (FK)
      13th october 1997:
         * user defined message are now written via the errors unit
           and exterror (FK)
         + compiler switch $INFO added, does the same like $MESSAGE,
           the text is written via comment(v_info,...) (FK)
         + $STOP and $FATALERROR added: they are equivalent, the
           following message is written and the compiler stops (FK)
         - write_c, no more necessary (FK)
      14th october 1997:
         + wrong line counting corrected: <comment start> $I test
                                          <comment end>
           (FK)
      17th october 1997:
         + support of $if expr   (FK)
         * $define a=1234 to a:=1234   (FK)
         + -So allows now <comment start> <comment start> <comment end>
           as comment (preocedure dec_comment_level)    (FK)
      22th october 1997:
         + $NOTE  (FK)
       9th november 1997:
          + added updating of line_no in asmgetchar. (CEC)
      14th november 1997:
          * fixed problem with asm line counting. (CEC)
      17th november 1997:
         + kommentar renamed src_comment and kommentarebene renamed comment_level (PM)

}

