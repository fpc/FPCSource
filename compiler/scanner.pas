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
{$ifdef tp}
  {$F+,N+,E+,R-}
{$endif}
unit scanner;

  interface

    uses
       cobjects,globals,files;

    const
{$ifdef TP}
       maxmacrolen = 1024;
{$else}
       maxmacrolen = 16*1024;
{$endif}

       id_len = 14;
       Newline = #10;

    type
       ident = string[id_len];

    const
      max_keywords = 71;
      anz_keywords : longint = max_keywords;

      keyword : array[1..max_keywords] of ident = (
{        'ABSOLUTE',}
         'AND',
         'ARRAY','AS','ASM',
{        'ASSEMBLER',}
         'BEGIN',
         'BREAK','CASE','CLASS',
         'CONST','CONSTRUCTOR','CONTINUE',
         'DESTRUCTOR','DISPOSE','DIV','DO','DOWNTO','ELSE','END',
         'EXCEPT',
         'EXIT',
{        'EXPORT',}
         'EXPORTS',
{        'EXTERNAL',}
         'FAIL','FALSE',
{        'FAR',}
         'FILE','FINALLY','FOR',
{        'FORWARD',}
         'FUNCTION','GOTO','IF','IMPLEMENTATION','IN',
         'INHERITED','INITIALIZATION',
{        'INLINE',} {INLINE is a reserved word in TP. Why?}
         'INTERFACE',
{        'INTERRUPT',}
         'IS',
         'LABEL','LIBRARY','MOD',
{        'NEAR',}
         'NEW','NIL','NOT','OBJECT',
         'OF','ON','OPERATOR','OR','OTHERWISE','PACKED',
         'PROCEDURE','PROGRAM','PROPERTY',
         'RAISE','RECORD','REPEAT','SELF',
         'SET','SHL','SHR','STRING','THEN','TO',
         'TRUE','TRY','TYPE','UNIT','UNTIL',
         'USES','VAR',
{        'VIRTUAL',}
         'WHILE','WITH','XOR');

       keyword_token : array[1..max_keywords] of ttoken = (
{        _ABSOLUTE,}
         _AND,
         _ARRAY,_AS,_ASM,
{        _ASSEMBLER,}
         _BEGIN,
         _BREAK,_CASE,_CLASS,
         _CONST,_CONSTRUCTOR,_CONTINUE,
         _DESTRUCTOR,_DISPOSE,_DIV,_DO,_DOWNTO,
         _ELSE,_END,_EXCEPT,
         _EXIT,
{        _EXPORT,}
         _EXPORTS,
{        _EXTERNAL,}
         _FAIL,_FALSE,
{        _FAR,}
         _FILE,_FINALLY,_FOR,
{        _FORWARD,}
         _FUNCTION,_GOTO,_IF,_IMPLEMENTATION,_IN,
         _INHERITED,_INITIALIZATION,
{        _INLINE,}
         _INTERFACE,
{        _INTERRUPT,}
         _IS,
         _LABEL,_LIBRARY,_MOD,
{        _NEAR,}
         _NEW,_NIL,_NOT,_OBJECT,
         _OF,_ON,_OPERATOR,_OR,_OTHERWISE,_PACKED,
         _PROCEDURE,_PROGRAM,_PROPERTY,
         _RAISE,_RECORD,_REPEAT,_SELF,
         _SET,_SHL,_SHR,_STRING,_THEN,_TO,
         _TRUE,_TRY,_TYPE,_UNIT,_UNTIL,
         _USES,_VAR,
{        _VIRTUAL,}
         _WHILE,_WITH,_XOR);


    type
      pmacrobuffer = ^tmacrobuffer;
      tmacrobuffer = array[0..maxmacrolen-1] of char;

      ppreprocstack = ^tpreprocstack;
      tpreprocstack = object
         isifdef,
         accept  : boolean;
         next    : ppreprocstack;
         name    : stringid;
         line_nb : longint;
         constructor init(ifdef,a:boolean;n:ppreprocstack);
         destructor done;
      end;

    var
        c              : char;
        orgpattern,
        pattern        : string;
        macrobuffer    : ^tmacrobuffer;
        comment_level  : word;
        inputbuffer    : pchar;
        inputpointer   : pchar;
        parse_types,                      { true, if type declarations are parsed }
        s_point        : boolean;
        yylexcount,
        macropos,
        lastlinepos,
        lasttokenpos   : longint;
        lastasmgetchar : char;
        preprocstack   : ppreprocstack;


{$ifdef UseTokenInfo}
    type
      ttokeninfo = record
                 token : ttoken;
                 fi : tfileposinfo;
                 end;
      ptokeninfo = ^ttokeninfo;
{$endif UseTokenInfo}

      {public}
        procedure syntaxerror(const s : string);
{$ifndef UseTokenInfo}
        function yylex : ttoken;
{$else UseTokenInfo}
        function yylex : ptokeninfo;
{$endif UseTokenInfo}
        function asmgetchar : char;
        function get_current_col : longint;
        procedure get_cur_file_pos(var fileinfo : tfileposinfo);
        procedure set_cur_file_pos(const fileinfo : tfileposinfo);

        procedure InitScanner(const fn: string);
        procedure DoneScanner(testendif:boolean);


  implementation

     uses
       dos,verbose,pbase,
       symtable,switches,link;

     var
    { this is usefull to get the write filename
      for the last instruction of an include file !}
       FileHasChanged : Boolean;
         status : tcompilestatus;


{*****************************************************************************
                              TPreProcStack
*****************************************************************************}

    constructor tpreprocstack.init(ifdef,a:boolean;n:ppreprocstack);
      begin
         isifdef:=ifdef;
         accept:=a;
         next:=n;
      end;


    destructor tpreprocstack.done;
      begin
      end;


    procedure popstack;
      var
         hp : ppreprocstack;
      begin
         hp:=preprocstack^.next;
         dispose(preprocstack,done);
         preprocstack:=hp;
      end;

{*****************************************************************************
                              Helper routines
*****************************************************************************}

    function is_keyword(var token : ttoken) : boolean;
      var
         high,low,mid : longint;
      begin
         low:=1;
         high:=anz_keywords;
         while low<high do
          begin
            mid:=(high+low+1) shr 1;
            if pattern<keyword[mid] then
             high:=mid-1
            else
             low:=mid;
          end;
         if pattern=keyword[high] then
          begin
            token:=keyword_token[high];
            is_keyword:=true;
          end
         else
          is_keyword:=false;
     end;


    procedure remove_keyword(const s : string);
      var
         i,j : longint;
      begin
         for i:=1 to anz_keywords do
           begin
              if keyword[i]=s then
                begin
                   for j:=i to anz_keywords-1 do
                     begin
                        keyword[j]:=keyword[j+1];
                        keyword_token[j]:=keyword_token[j+1];
                     end;
                   dec(anz_keywords);
                   break;
                end;
           end;
      end;


    function get_current_col : longint;
      begin
         if lastlinepos<=lasttokenpos then
           get_current_col:=lasttokenpos-lastlinepos
         else
           get_current_col:=0;
      end;


    procedure inc_comment_level;
      begin
         inc(comment_level);
         if (comment_level>1) then
          Message1(scan_w_comment_level,tostr(comment_level));
      end;


    procedure dec_comment_level;
      begin
         if cs_tp_compatible in aktswitches then
           comment_level:=0
         else
           dec(comment_level);
      end;


    procedure syntaxerror(const s : string);
      begin
         Message2(scan_f_syn_expected,tostr(get_current_col),s);
      end;


{*****************************************************************************
                                 Scanner
*****************************************************************************}

    procedure reload;
      var
         readsize   : word;
         i,saveline : longint;
      begin
        if not assigned(current_module^.current_inputfile) then
          internalerror(14);
        if current_module^.current_inputfile^.filenotatend then
         begin
         { load the next piece of source }
           blockread(current_module^.current_inputfile^.f,inputbuffer^,
             current_module^.current_inputfile^.bufsize-1,readsize);
         { Scan the buffer for #0 chars, which are not alllowed }
           if readsize > 0 then
            begin
            { force proper line counting }
              saveline:=current_module^.current_inputfile^.line_no;
              i:=0;
              inputpointer:=inputbuffer;
              while i<readsize do
               begin
                 c:=inputpointer^;
                 case c of
                  #0 : Message(scan_f_illegal_char);
             #10,#13 : begin
                         if (byte(c)+byte(inputpointer[1])=23) then
                          begin
                            inc(longint(inputpointer));
                            inc(i);
                          end;
                         inc(current_module^.current_inputfile^.line_no);
                       end;
                 end;
                 inc(i);
                 inc(longint(inputpointer));
               end;
              current_module^.current_inputfile^.line_no:=saveline;
            end;
           inputbuffer[readsize]:=#0;
           inputpointer:=inputbuffer;
         { Set EOF when main source and at endoffile }
           if eof(current_module^.current_inputfile^.f) then
            begin
              current_module^.current_inputfile^.filenotatend:=false;
              if current_module^.current_inputfile^.next=nil then
               inputbuffer[readsize]:=#26;
            end;
         end
        else
         begin
           current_module^.current_inputfile^.close;
         { load next module }
           current_module^.current_inputfile:=current_module^.current_inputfile^.next;
           current_module^.current_index:=current_module^.current_inputfile^.ref_index;
           status.currentsource:=current_module^.current_inputfile^.name^+current_module^.current_inputfile^.ext^;
           inputbuffer:=current_module^.current_inputfile^.buf;
           inputpointer:=inputbuffer+current_module^.current_inputfile^.bufpos;
         end;
      { load next char }
        c:=inputpointer^;
        inc(longint(inputpointer));
      end;


    procedure linebreak;
      var
         cur : char;
      begin
        cur:=c;
        if (byte(inputpointer^)=0) and
           current_module^.current_inputfile^.filenotatend then
          begin
             reload;
             if byte(cur)+byte(c)<>23 then
               dec(longint(inputpointer));
          end
        else
        { Fix linebreak to be only newline (=#10) for all types of linebreaks }
        if (byte(inputpointer^)+byte(c)=23) then
          inc(longint(inputpointer));
        c:=newline;
      { Update Status and show status }
        with status do
         begin
           totalcompiledlines:=abslines;
           currentline:=current_module^.current_inputfile^.line_no
               +current_module^.current_inputfile^.line_count;
           { you call strcopy here at each line !!! }
           {currentsource:=current_module^.current_inputfile^.name^+current_module^.current_inputfile^.ext^;}
           totallines:=0;
         end;
        if compilestatusproc(status) then
         stop;
        inc(current_module^.current_inputfile^.line_no);
        inc(abslines);
        lastlinepos:=longint(inputpointer);
      end;


    procedure readchar;
      begin
        c:=inputpointer^;
        if c=#0 then
         reload
        else
         inc(longint(inputpointer));
        if c in [#10,#13] then
         linebreak;
      end;


    function readstring:string;
      var
        i : longint;
      begin
        i:=0;
      { 'in []' splitted, so it will be CMP's and no SET_IN_BYTE (PFV) }
        while (c in ['A'..'Z','a'..'z']) or (c in ['0'..'9','_']) do
         begin
           if i<255 then
            begin
              inc(i);
              readstring[i]:=c;
            end;
        { get next char }
           readchar;
         end;
        readstring[0]:=chr(i);
      end;


    function readid:string;
      begin
        readid:=upper(readstring);
      end;


    function readnumber:string;
      var
        base,
        i  : longint;
      begin
        case c of
         '%' : begin
                 readchar;
                 base:=2;
                 readnumber[1]:='%';
                 i:=1;
               end;
         '$' : begin
                 readchar;
                 base:=16;
                 readnumber[1]:='$';
                 i:=1;
               end;
        else
         begin
           base:=10;
           i:=0;
         end;
        end;
        while ((base>=10) and (c in ['0'..'9'])) or
              ((base=16) and (c in ['A'..'F','a'..'f'])) or
              ((base=2) and (c in ['0'..'1'])) do
         begin
           if i<255 then
            begin
              inc(i);
              readnumber[i]:=c;
            end;
        { get next char }
           readchar;
         end;
        readnumber[0]:=chr(i);
      { was the next char a linebreak ? }
      {  if c in [#10,#13] then
         linebreak; }
      end;


    function readval:longint;
      var
        l : longint;
        w : word;
      begin
        val(readnumber,l,w);
        readval:=l;
      end;

    function readcomment:string;
      var
        i : longint;
      begin
        i:=0;
        repeat
          case c of
           '}' : begin
                   readchar;
                   dec_comment_level;
                   break;
                 end;
           #26 : Message(scan_f_end_of_file);
          else
            begin
              if (i<255) then
               begin
                 inc(i);
                 readcomment[i]:=c;
               end;
            end;
          end;
          readchar;
        until false;
        readcomment[0]:=chr(i);
      end;


    procedure skipspace;
      begin
        while c in [' ',#9..#13] do
         begin
           readchar;
           {c:=inputpointer^;
           if c=#0 then
            reload
           else
            inc(longint(inputpointer));
           if c in [#10,#13] then
            linebreak; }
         end;
      end;


    procedure skipuntildirective;
      var
        found : longint;
      begin
         found:=0;
         repeat
           case c of
            #26 : Message(scan_f_end_of_file);
            '{' : begin
                    if comment_level=0 then
                     found:=1;
                    inc_comment_level;
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
           readchar;
           {c:=inputpointer^;
           if c=#0 then
            reload
           else
            inc(longint(inputpointer));}
         until (found=2);
      end;

{$i scandir.inc}

    procedure skipcomment;
      begin
        readchar;
        inc_comment_level;
      { handle compiler switches }
        if (c='$') then
         handledirectives;
      { handle_switches can dec comment_level,  }
        while (comment_level>0) do
         begin
           case c of
            '{' : inc_comment_level;
            '}' : dec_comment_level;
            #26 : Message(scan_f_end_of_file);
           end;
           readchar;
           {c:=inputpointer^;
           if c=#0 then
            reload
           else
            inc(longint(inputpointer));}
         end;
       {if (c=#10) or (c=#13) then linebreak;}
      end;


    procedure skipdelphicomment;
      begin
        inc_comment_level;
        readchar;
      { this is currently not supported }
        if c='$' then
          Message(scan_e_wrong_styled_switch);
      { skip comment }
        while c<>newline do
         begin
           if c=#26 then
             Message(scan_f_end_of_file);
           readchar;
         end;
        dec_comment_level;
      end;


    procedure skipoldtpcomment;
      var
        found : longint;
      begin
        inc_comment_level;
        readchar;
      { this is currently not supported }
        if c='$' then
         Message(scan_e_wrong_styled_switch);
      { skip comment }
        while (comment_level>0) do
         begin
           found:=0;
           repeat
             case c of
              #26 : Message(scan_f_end_of_file);
              '*' : begin
                      if found=3 then
                       inc_comment_level
                      else
                       found:=1;
                    end;
              ')' : begin
                      if found=1 then
                       begin
                         dec_comment_level;
                         if comment_level=0 then
                          found:=2;
                       end;
                    end;
              '(' : found:=3;
             else
              found:=0;
             end;
             readchar;
             {c:=inputpointer^;
             if c=#0 then
              reload
             else
              inc(longint(inputpointer));}
           until (found=2);
         end;
      end;


{$ifndef UseTokenInfo}
        function yylex : ttoken;
{$else UseTokenInfo}
        function yylex : ptokeninfo;
{$endif UseTokenInfo}
     var
        y    : ttoken;
{$ifdef UseTokenInfo}
        newyylex : ptokeninfo;
        line,column : longint;
{$endif UseTokenInfo}
        code : word;
        l    : longint;
        mac  : pmacrosym;
        hp   : pinputfile;
        hp2  : pchar;
{$ifdef UseTokenInfo}
      label
         exit_label;
{$endif UseTokenInfo}
     begin
{$ifdef UseTokenInfo}
        line:=current_module^.current_inputfile^.line_no;
        column:=get_current_col;
{$endif UseTokenInfo}
        { was the last character a point ? }
        { this code is needed because the scanner if there is a 1. found if  }
        { this is a floating point number or range like 1..3                 }
        if s_point then
          begin
             s_point:=false;
             if c='.' then
               begin
                  readchar;
{$ifndef UseTokenInfo}
                  yylex:=POINTPOINT;
                  exit;
               end;
             yylex:=POINT;
             exit;
{$else UseTokenInfo}
                  y:=POINTPOINT;
                  goto exit_label;
               end;
             y:=POINT;
             goto exit_label;
{$endif UseTokenInfo}
          end;

        repeat
          case c of
           '{' : skipcomment;
   ' ',#9..#13 : skipspace;
          else
           break;
          end;
        until false;

        lasttokenpos:=longint(inputpointer);
{$ifdef UseTokenInfo}
        line:=current_module^.current_inputfile^.line_no;
        column:=get_current_col;
        { will become line:=lasttokenpos ??;}
{$endif UseTokenInfo}
        case c of
       '_','A'..'Z',
           'a'..'z' : begin
                        orgpattern:=readstring;
                        pattern:=upper(orgpattern);
                        if (length(pattern) in [2..id_len]) and is_keyword(y) then
{$ifndef UseTokenInfo}
                         yylex:=y
{$endif UseTokenInfo}
                        else
                         begin
                         { this takes some time ... }
                           if support_macros then
                            begin
                              mac:=pmacrosym(macros^.search(pattern));
                              if assigned(mac) and (assigned(mac^.buftext)) then
                               begin
                               { don't forget the last char }
                                 dec(longint(inputpointer));
                                 current_module^.current_inputfile^.bufpos:=inputpointer-inputbuffer;
                               { this isn't a proper way, but ... }
                                 hp:=new(pinputfile,init('','Macro '+pattern,''));
                                 hp^.next:=current_module^.current_inputfile;
                                 current_module^.current_inputfile:=hp;
                                 status.currentsource:=current_module^.current_inputfile^.name^;
                                 current_module^.sourcefiles.register_file(hp);
                                 current_module^.current_index:=hp^.ref_index;
                               { set an own buffer }
                                 getmem(hp2,mac^.buflen+1);
                                 current_module^.current_inputfile^.setbuf(hp2,mac^.buflen+1);
                                 inputbuffer:=current_module^.current_inputfile^.buf;
                               { copy text }
                                 move(mac^.buftext^,inputbuffer^,mac^.buflen);
                               { put end sign }
                                 inputbuffer[mac^.buflen+1]:=#0;
                               { load c }
                                 c:=inputbuffer^;
                                 inputpointer:=inputbuffer+1;
                               { handle empty macros }
                                 if c=#0 then
                                  reload;
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
                                 exit;
                               end;
                            end;
{$ifndef UseTokenInfo}
                           yylex:=ID;
                         end;
                        exit;
{$else UseTokenInfo}
                           y:=ID;
                         end;
                        goto exit_label;
{$endif UseTokenInfo}
                      end;
                '$' : begin
                         pattern:=readnumber;
{$ifndef UseTokenInfo}
                         yylex:=INTCONST;
                         exit;
{$else UseTokenInfo}
                         y:=INTCONST;
                         goto exit_label;
{$endif UseTokenInfo}
                      end;
                '%' : begin
                         pattern:=readnumber;
{$ifndef UseTokenInfo}
                         yylex:=INTCONST;
                         exit;
{$else UseTokenInfo}
                         y:=INTCONST;
                         goto exit_label;
{$endif UseTokenInfo}
                      end;
           '0'..'9' : begin
                        pattern:=readnumber;
                        case c of
                         '.' : begin
                                 readchar;
                                 if not(c in ['0'..'9']) then
                                  begin
                                    s_point:=true;
{$ifndef UseTokenInfo}
                                    yylex:=INTCONST;
                                    exit;
{$else UseTokenInfo}
                                    y:=INTCONST;
                                    goto exit_label;
{$endif UseTokenInfo}
                                  end;
                                 pattern:=pattern+'.';
                                 while c in ['0'..'9'] do
                                  begin
                                    pattern:=pattern+c;
                                    readchar;
                                  end;
{$ifndef UseTokenInfo}
                                 yylex:=REALNUMBER;
                                 exit;
{$else UseTokenInfo}
                                 y:=REALNUMBER;
                                 goto exit_label;
{$endif UseTokenInfo}
                               end;
                     'e','E' : begin
                                 pattern:=pattern+'E';
                                 readchar;
                                 if c in ['-','+'] then
                                  begin
                                    pattern:=pattern+c;
                                    readchar;
                                  end;
                                 if not(c in ['0'..'9']) then
                                  Message(scan_f_illegal_char);
                                 while c in ['0'..'9'] do
                                  begin
                                    pattern:=pattern+c;
                                    readchar;
                                  end;
{$ifndef UseTokenInfo}
                                 yylex:=REALNUMBER;
                                 exit;
{$else UseTokenInfo}
                                 y:=REALNUMBER;
                                 goto exit_label;
{$endif UseTokenInfo}
                               end;
                        end;
{$ifndef UseTokenInfo}
                        yylex:=INTCONST;
                        exit;
{$else UseTokenInfo}
                        y:=INTCONST;
                        goto exit_label;
{$endif UseTokenInfo}
                      end;
                ';' : begin
                        readchar;
{$ifndef UseTokenInfo}
                        yylex:=SEMICOLON;
                        exit;
{$else UseTokenInfo}
                        y:=SEMICOLON;
                        goto exit_label;
{$endif UseTokenInfo}
                      end;
                '[' : begin
                        readchar;
{$ifndef UseTokenInfo}
                        yylex:=LECKKLAMMER;
                        exit;
{$else UseTokenInfo}
                        y:=LECKKLAMMER;
                        goto exit_label;
{$endif UseTokenInfo}
                      end;
                ']' : begin
                        readchar;
{$ifndef UseTokenInfo}
                        yylex:=RECKKLAMMER;
                        exit;
{$else UseTokenInfo}
                        y:=RECKKLAMMER;
                        goto exit_label;
{$endif UseTokenInfo}
                      end;
                '(' : begin
                        readchar;
                        if c='*' then
                         begin
                           skipoldtpcomment;
{$ifndef TP}
                           yylex:=yylex();
{$else TP}
                           yylex:=yylex;
{$endif TP}
                           exit;
                         end;
{$ifndef UseTokenInfo}
                        yylex:=LKLAMMER;
                        exit;
{$else UseTokenInfo}
                        y:=LKLAMMER;
                        goto exit_label;
{$endif UseTokenInfo}
                      end;
                ')' : begin
                        readchar;
{$ifndef UseTokenInfo}
                        yylex:=RKLAMMER;
                        exit;
{$else UseTokenInfo}
                        y:=RKLAMMER;
                        goto exit_label;
{$endif UseTokenInfo}
                      end;
                '+' : begin
                        readchar;
                        if (c='=') and c_like_operators then
                         begin
                           readchar;
{$ifndef UseTokenInfo}
                           yylex:=_PLUSASN;
                           exit;
{$else UseTokenInfo}
                           y:=_PLUSASN;
                           goto exit_label;
{$endif UseTokenInfo}
                         end;
{$ifndef UseTokenInfo}
                        yylex:=PLUS;
                        exit;
{$else UseTokenInfo}
                        y:=PLUS;
                        goto exit_label;
{$endif UseTokenInfo}
                      end;
                '-' : begin
                        readchar;
                        if (c='=') and c_like_operators then
                         begin
                           readchar;
{$ifndef UseTokenInfo}
                           yylex:=_MINUSASN;
                           exit;
{$else UseTokenInfo}
                           y:=_MINUSASN;
                           goto exit_label;
{$endif UseTokenInfo}
                         end;
{$ifndef UseTokenInfo}
                        yylex:=MINUS;
                        exit;
{$else UseTokenInfo}
                        y:=MINUS;
                        goto exit_label;
{$endif UseTokenInfo}
                      end;
                ':' : begin
                        readchar;
                        if c='=' then
                         begin
                           readchar;
{$ifndef UseTokenInfo}
                           yylex:=ASSIGNMENT;
                           exit;
{$else UseTokenInfo}
                           y:=ASSIGNMENT;
                           goto exit_label;
{$endif UseTokenInfo}
                         end;
{$ifndef UseTokenInfo}
                        yylex:=COLON;
                        exit;
{$else UseTokenInfo}
                        y:=COLON;
                        goto exit_label;
{$endif UseTokenInfo}
                      end;
                '*' : begin
                        readchar;
                        if (c='=') and c_like_operators then
                         begin
                           readchar;
{$ifndef UseTokenInfo}
                           yylex:=_STARASN;
{$else UseTokenInfo}
                           y:=_STARASN;
{$endif UseTokenInfo}
                         end else if c='*' then
                         begin
                           readchar;
{$ifndef UseTokenInfo}
                           yylex:=STARSTAR;
{$else UseTokenInfo}
                           y:=STARSTAR;
{$endif UseTokenInfo}
                         end
                        else
{$ifndef UseTokenInfo}
                          yylex:=STAR;
                        exit;
{$else UseTokenInfo}
                          y:=STAR;
                        goto exit_label;
{$endif UseTokenInfo}
                      end;
                '/' : begin
                        readchar;
                        case c of
                         '=' : begin
                                 if c_like_operators then
                                  begin
                                    readchar;
{$ifndef UseTokenInfo}
                                    yylex:=_SLASHASN;
                                    exit;
{$else UseTokenInfo}
                                    y:=_SLASHASN;
                                    goto exit_label;
{$endif UseTokenInfo}
                                  end;
                               end;
                         '/' : begin
                                 skipdelphicomment;
{$ifndef TP}
                                 yylex:=yylex();
{$else TP}
                                 yylex:=yylex;
{$endif TP}
                                 exit;
                               end;
                        end;
{$ifndef UseTokenInfo}
                        yylex:=SLASH;
                        exit;
{$else UseTokenInfo}
                        y:=SLASH;
                        goto exit_label;
{$endif UseTokenInfo}
                      end;
           '='      : begin
                        readchar;
{$ifndef UseTokenInfo}
                        yylex:=EQUAL;
                        exit;
{$else UseTokenInfo}
                        y:=EQUAL;
                        goto exit_label;
{$endif UseTokenInfo}
                      end;
           '.'      : begin
                        readchar;
                        if c='.' then
                         begin
                           readchar;
{$ifndef UseTokenInfo}
                           yylex:=POINTPOINT;
                           exit;
{$else UseTokenInfo}
                           y:=POINTPOINT;
                           goto exit_label;
{$endif UseTokenInfo}
                         end
                        else
{$ifndef UseTokenInfo}
                         yylex:=POINT;
                        exit;
{$else UseTokenInfo}
                         y:=POINT;
                        goto exit_label;
{$endif UseTokenInfo}
                      end;
                '@' : begin
                        readchar;
                        if c='@' then
                         begin
                           readchar;
{$ifndef UseTokenInfo}
                           yylex:=DOUBLEADDR;
{$else UseTokenInfo}
                           y:=DOUBLEADDR;
{$endif UseTokenInfo}
                         end
                        else
{$ifndef UseTokenInfo}
                         yylex:=KLAMMERAFFE;
                        exit;
{$else UseTokenInfo}
                         y:=KLAMMERAFFE;
                        goto exit_label;
{$endif UseTokenInfo}
                      end;
                ',' : begin
                        readchar;
{$ifndef UseTokenInfo}
                        yylex:=COMMA;
                        exit;
{$else UseTokenInfo}
                        y:=COMMA;
                        goto exit_label;
{$endif UseTokenInfo}
                      end;
      '''','#','^' :  begin
                        if c='^' then
                         begin
                           readchar;
                              c:=upcase(c);
                              if not(block_type=bt_type) and (c in ['A'..'Z']) then
{                           if not(block_type=bt_type) and (c in [#64..#128]) then}
                            begin
                              pattern:=chr(ord(c)-64);
                              readchar;
                            end
                           else
                            begin
{$ifndef UseTokenInfo}
                              yylex:=CARET;
                              exit;
{$else UseTokenInfo}
                              y:=CARET;
                              goto exit_label;
{$endif UseTokenInfo}
                            end;
                         end
                        else
                         pattern:='';
                        repeat
                          case c of
                           '#' : begin
                                   readchar; { read # }
                                   valint(readnumber,l,code);
                                   if (code<>0) or (l<0) or (l>255) then
                                    Message(scan_e_illegal_char_const);
                                   pattern:=pattern+chr(l);
                                 end;
                          '''' : begin
                                   repeat
                                     readchar;
                                     case c of
                                    #26 : Message(scan_f_end_of_file);
                                newline : Message(scan_f_string_exceeds_line);
                                   '''' : begin
                                            readchar;
                                            if c<>'''' then
                                             break;
                                          end;
                                     end;
                                     pattern:=pattern+c;
                                   until false;
                                 end;
                           '^' : begin
                                   readchar;
                                   if c<#64 then
                                    c:=chr(ord(c)+64)
                                   else
                                    c:=chr(ord(c)-64);
                                   pattern:=pattern+c;
                                   readchar;
                                 end;
                          else
                           break;
                          end;
                        until false;
                      { strings with length 1 become const chars }
{$ifndef UseTokenInfo}
                        if length(pattern)=1 then
                         yylex:=CCHAR
                        else
                         yylex:=CSTRING;
                        exit;
{$else UseTokenInfo}
                        if length(pattern)=1 then
                         y:=CCHAR
                        else
                         y:=CSTRING;
                        goto exit_label;
{$endif UseTokenInfo}
                      end;
                '>' : begin
                        readchar;
                        case c of
                         '=' : begin
                                 readchar;
{$ifndef UseTokenInfo}
                                 yylex:=GTE;
                                 exit;
{$else UseTokenInfo}
                                 y:=GTE;
                                 goto exit_label;
{$endif UseTokenInfo}
                               end;
                         '>' : begin
                                 readchar;
{$ifndef UseTokenInfo}
                                 yylex:=_SHR;
                                 exit;
{$else UseTokenInfo}
                                 y:=_SHR;
                                 goto exit_label;
{$endif UseTokenInfo}
                               end;
                         '<' : begin { >< is for a symetric diff for sets }
                                 readchar;
{$ifndef UseTokenInfo}
                                 yylex:=SYMDIF;
                                 exit;
{$else UseTokenInfo}
                                 y:=SYMDIF;
                                 goto exit_label;
{$endif UseTokenInfo}
                               end;
                        end;
{$ifndef UseTokenInfo}
                        yylex:=GT;
                        exit;
{$else UseTokenInfo}
                        y:=GT;
                        goto exit_label;
{$endif UseTokenInfo}
                      end;
                '<' : begin
                        readchar;
                        case c of
                         '>' : begin
                                 readchar;
{$ifndef UseTokenInfo}
                                 yylex:=UNEQUAL;
                                 exit;
{$else UseTokenInfo}
                                 y:=UNEQUAL;
                                 goto exit_label;
{$endif UseTokenInfo}
                               end;
                         '=' : begin
                                 readchar;
{$ifndef UseTokenInfo}
                                 yylex:=LTE;
                                 exit;
{$else UseTokenInfo}
                                 y:=LTE;
                                 goto exit_label;
{$endif UseTokenInfo}
                               end;
                         '<' : begin
                                 readchar;
{$ifndef UseTokenInfo}
                                 yylex:=_SHL;
                                 exit;
{$else UseTokenInfo}
                                 y:=_SHL;
                                 goto exit_label;
{$endif UseTokenInfo}
                               end;
                        end;
{$ifndef UseTokenInfo}
                        yylex:=LT;
                        exit;
{$else UseTokenInfo}
                        y:=LT;
                        goto exit_label;
{$endif UseTokenInfo}
                      end;
                #26 : begin
{$ifndef UseTokenInfo}
                        yylex:=_EOF;
                        exit;
{$else UseTokenInfo}
                        y:=_EOF;
                        goto exit_label;
{$endif UseTokenInfo}
                      end;
           else
            begin
              Message(scan_f_illegal_char);
            end;
           end;
{$ifdef UseTokenInfo}
      exit_label:
        new(newyylex);
        newyylex^.token:=y;
        newyylex^.fi.fileindex:=current_module^.current_index;
        newyylex^.fi.line:=line;
        newyylex^.fi.column:=column;
        yylex:=newyylex;
{$endif UseTokenInfo}
     end;


    function asmgetchar : char;
      begin
         if lastasmgetchar<>#0 then
          begin
            c:=lastasmgetchar;
            lastasmgetchar:=#0;
          end
         else
          readchar;
         case c of
          '{' : begin
                  skipcomment;
                  lastasmgetchar:=c;
                  asmgetchar:=';';
                  exit;
                end;
          '/' : begin
                  readchar;
                  if c='/' then
                   begin
                     skipdelphicomment;
                     asmgetchar:=';';
                   end
                  else
                   asmgetchar:='/';
                  lastasmgetchar:=c;
                  exit;
                end;
          '(' : begin
                  readchar;
                  if c='*' then
                   begin
                     skipoldtpcomment;
                     asmgetchar:=';';
                   end
                  else
                   asmgetchar:='(';
                  lastasmgetchar:=c;
                  exit;
                end;
         else
          begin
            asmgetchar:=c;
          end;
         end;
      end;


   procedure InitScanner(const fn: string);
     var
       d:dirstr;
       n:namestr;
       e:extstr;
     begin
        fsplit(fn,d,n,e);
        current_module^.current_inputfile:=new(pinputfile,init(d,n,e));
        current_module^.current_inputfile^.reset;
        current_module^.sourcefiles.register_file(current_module^.current_inputfile);
        current_module^.current_index:=current_module^.current_inputfile^.ref_index;
        status.currentsource:=current_module^.current_inputfile^.name^+current_module^.current_inputfile^.ext^;
        if ioresult<>0 then
         Message(scan_f_cannot_open_input);
        inputbuffer:=current_module^.current_inputfile^.buf;
        reload;
        preprocstack:=nil;
        comment_level:=0;
        lasttokenpos:=0;
        lastlinepos:=0;
        s_point:=false;
     end;

   procedure get_cur_file_pos(var fileinfo : tfileposinfo);

     begin
        fileinfo.line:=current_module^.current_inputfile^.line_no;
        {fileinfo.fileindex:=current_module^.current_inputfile^.ref_index;}
        { should allways be the same !! }
        fileinfo.fileindex:=current_module^.current_index;
        fileinfo.column:=get_current_col;
     end;

   procedure set_cur_file_pos(const fileinfo : tfileposinfo);

     begin
        current_module^.current_index:=fileinfo.fileindex;
        current_module^.current_inputfile:=
          pinputfile(current_module^.sourcefiles.get_file(fileinfo.fileindex));
        current_module^.current_inputfile^.line_no:=fileinfo.line;
        {fileinfo.fileindex:=current_module^.current_inputfile^.ref_index;}
        { should allways be the same !! }
        { fileinfo.column:=get_current_col; }
     end;

   procedure DoneScanner(testendif:boolean);
     var
       st : string[16];
     begin
       if (not testendif) then
        begin
          while assigned(preprocstack) do
           begin
             if preprocstack^.isifdef then
              st:='$IF(N)(DEF)'
             else
              st:='$ELSE';
             Message3(scan_e_endif_expected,st,preprocstack^.name,tostr(preprocstack^.line_nb));
             popstack;
           end;
        end;
     end;

end.
{
  $Log$
  Revision 1.14  1998-04-30 15:59:42  pierre
    * GDB works again better :
      correct type info in one pass
    + UseTokenInfo for better source position
    * fixed one remaining bug in scanner for line counts
    * several little fixes

  Revision 1.13  1998/04/29 13:42:27  peter
    + $IOCHECKS and $ALIGN to test already, other will follow soon
    * fixed the wrong linecounting with comments

  Revision 1.12  1998/04/29 10:34:04  pierre
    + added some code for ansistring (not complete nor working yet)
    * corrected operator overloading
    * corrected nasm output
    + started inline procedures
    + added starstarn : use ** for exponentiation (^ gave problems)
    + started UseTokenInfo cond to get accurate positions

  Revision 1.11  1998/04/27 23:10:29  peter
    + new scanner
    * $makelib -> if smartlink
    * small filename fixes pmodule.setfilename
    * moved import from files.pas -> import.pas

}
