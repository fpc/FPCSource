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
       cobjects,globals,verbose,comphook,files;

    const
{$ifdef TP}
       maxmacrolen=1024;
       InputFileBufSize=1024;
       linebufincrease=64;
{$else}
       maxmacrolen=16*1024;
       InputFileBufSize=32*1024;
       linebufincrease=512;
{$endif}

       id_len = 14;
       Newline = #10;

    type
       ident = string[id_len];

    const
      max_keywords = 70;
      anz_keywords : longint = max_keywords;

      { the following keywords are no keywords in TP, they
        are internal procedures

      CONTINUE, DISPOSE, EXIT, FAIL, FALSE, NEW, SELF
      TRUE
      }
      { INLINE is a keyword in TP, but only an modifier in FPC }
      keyword : array[1..max_keywords] of ident = (
{        'ABSOLUTE',}
         'AND',
         'ARRAY','AS','ASM',
{        'ASSEMBLER',}
         'BEGIN',
         'CASE','CLASS',
         'CONST','CONSTRUCTOR',
         'DESTRUCTOR','DISPOSE','DIV','DO','DOWNTO','ELSE','END',
         'EXCEPT',
         'EXIT',
{        'EXPORT',}
         'EXPORTS',
{        'EXTERNAL',}
         'FAIL','FALSE',
{        'FAR',}
         'FILE','FINALIZATION','FINALLY','FOR',
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
         _CASE,_CLASS,
         _CONST,_CONSTRUCTOR,
         _DESTRUCTOR,_DISPOSE,_DIV,_DO,_DOWNTO,
         _ELSE,_END,_EXCEPT,
         _EXIT,
{        _EXPORT,}
         _EXPORTS,
{        _EXTERNAL,}
         _FAIL,_FALSE,
{        _FAR,}
         _FILE,_FINALIZATION,_FINALLY,_FOR,
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
          accept  : boolean;
          next    : ppreprocstack;
          name    : stringid;
          line_nb : longint;
          constructor init(a:boolean;n:ppreprocstack);
          destructor done;
       end;

       pscannerfile = ^tscannerfile;
       tscannerfile = object
          inputfile    : pinputfile; { current inputfile list }

          f            : file;       { current file handle }
          filenotatend,              { still bytes left to read }
          closed       : boolean;    { is the file closed }

          inputbufsize : longint;    { max size of the input buffer }

          inputbuffer,
          inputpointer : pchar;

          bufstart,
          bufsize      : longint;

          line_no,
          lasttokenpos,
          lastlinepos  : longint;
          lasttoken    : ttoken;

          maxlinebuf   : longint;
          linebuf      : plongint;

          do_special,                 { 1=point after nr, 2=caret after id }
          comment_level,
          yylexcount     : longint;
          lastasmgetchar : char;
          preprocstack   : ppreprocstack;

          constructor init(const fn:string);
          destructor done;
        { File buffer things }
          function  open:boolean;
          procedure close;
          function  reopen:boolean;
          procedure seekbuf(fpos:longint);
          procedure readbuf;
          procedure saveinputfile;
          procedure restoreinputfile;
          procedure nextfile;
          procedure addfile(hp:pinputfile);
          procedure reload;
          procedure setbuf(p:pchar;l:longint);
        { Scanner things }
          procedure gettokenpos;
          procedure inc_comment_level;
          procedure dec_comment_level;
          procedure checkpreprocstack;
          procedure poppreprocstack;
          procedure addpreprocstack(a:boolean;const s:string;w:tmsgconst);
          procedure elsepreprocstack;
          procedure linebreak;
          procedure readchar;
          procedure readstring;
          procedure readnumber;
          function  readid:string;
          function  readval:longint;
          function  readcomment:string;
          procedure skipspace;
          procedure skipuntildirective;
          procedure skipcomment;
          procedure skipdelphicomment;
          procedure skipoldtpcomment;
          function  yylex:ttoken;
          function  readpreproc:ttoken;
          function  asmgetchar:char;
       end;

    var
        c              : char;
        orgpattern,
        pattern        : string;
        current_scanner : pscannerfile;

    { changes to keywords to be tp compatible }
    procedure change_to_tp_keywords;

implementation

    uses
      dos,systems,symtable,switches;

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


    procedure change_to_tp_keywords;
      const
        non_tp : array[0..14] of string[id_len] = (
           'AS','CLASS','EXCEPT','FINALLY','INITIALIZATION','IS',
           'ON','OPERATOR','OTHERWISE','PROPERTY','RAISE','TRY',
           'EXPORTS','LIBRARY','FINALIZATION');
      var
        i : longint;
      begin
        for i:=0 to 13 do
         remove_keyword(non_tp[i]);
      end;


{*****************************************************************************
                              TPreProcStack
*****************************************************************************}

    constructor tpreprocstack.init(a:boolean;n:ppreprocstack);
      begin
        accept:=a;
        next:=n;
      end;


    destructor tpreprocstack.done;
      begin
      end;


{****************************************************************************
                                TSCANNERFILE
 ****************************************************************************}

    constructor tscannerfile.init(const fn:string);
      begin
        inputfile:=new(pinputfile,init(fn));
        current_module^.sourcefiles.register_file(inputfile);
        current_module^.current_index:=inputfile^.ref_index;
      { reset scanner }
        preprocstack:=nil;
        comment_level:=0;
        do_special:=0;
        block_type:=bt_general;
      { reset buf }
        closed:=true;
        filenotatend:=true;
        inputbufsize:=InputFileBufSize;
        inputbuffer:=nil;
        inputpointer:=nil;
        bufstart:=0;
        bufsize:=0;
      { line }
        line_no:=0;
        lastlinepos:=0;
        lasttokenpos:=0;
        linebuf:=nil;
        maxlinebuf:=0;
      { load block }
        if not open then
         Message(scan_f_cannot_open_input);
        reload;
      end;


    destructor tscannerfile.done;
      begin
        checkpreprocstack;
      { close file }
        if not closed then
         close;
      end;


    procedure tscannerfile.seekbuf(fpos:longint);
      begin
        if closed then
         exit;
        seek(f,fpos);
        bufstart:=fpos;
        bufsize:=0;
      end;


    procedure tscannerfile.readbuf;
    {$ifdef TP}
      var
        w : word;
    {$endif}
      begin
        if closed then
         exit;
        inc(bufstart,bufsize);
      {$ifdef TP}
        blockread(f,inputbuffer^,inputbufsize-1,w);
        bufsize:=w;
      {$else}
        blockread(f,inputbuffer^,inputbufsize-1,bufsize);
      {$endif}
        inputbuffer[bufsize]:=#0;
        Filenotatend:=(bufsize=inputbufsize-1);
      end;


    function tscannerfile.open:boolean;
      var
        ofm : byte;
      begin
        open:=false;
        if not closed then
         exit;
        ofm:=filemode;
        filemode:=0;
        Assign(f,inputfile^.path^+inputfile^.name^);
        {$I-}
         reset(f,1);
        {$I+}
        filemode:=ofm;
        if ioresult<>0 then
         exit;
      { file }
        closed:=false;
        filenotatend:=true;
        Getmem(inputbuffer,inputbufsize);
        inputpointer:=inputbuffer;
        bufstart:=0;
        bufsize:=0;
      { line }
        line_no:=0;
        lastlinepos:=0;
        lasttokenpos:=0;
        open:=true;
      end;


    procedure tscannerfile.close;
      var
        i : word;
      begin
        inc(bufstart,inputpointer-inputbuffer);
        if not closed then
         begin
           {$I-}
            system.close(f);
           {$I+}
           i:=ioresult;
           Freemem(inputbuffer,InputFileBufSize);
           inputbuffer:=nil;
           inputpointer:=nil;
           closed:=true;
         end;
      end;


    function tscannerfile.reopen:boolean;
      var
        ofm : byte;
      begin
        reopen:=false;
        if not closed then
         exit;
        ofm:=filemode;
        filemode:=0;
        Assign(f,inputfile^.path^+inputfile^.name^);
        {$I-}
         reset(f,1);
        {$I+}
        filemode:=ofm;
        if ioresult<>0 then
         exit;
        closed:=false;
      { get new mem }
        Getmem(inputbuffer,inputbufsize);
        inputpointer:=inputbuffer;
      { restore state }
        seek(f,BufStart);
        bufsize:=0;
        readbuf;
        reopen:=true;
      end;


    procedure tscannerfile.saveinputfile;
      begin
        inputfile^.savebufstart:=bufstart;
        inputfile^.savebufsize:=bufsize;
        inputfile^.savelastlinepos:=lastlinepos;
        inputfile^.saveline_no:=line_no;
        inputfile^.saveinputbuffer:=inputbuffer;
        inputfile^.saveinputpointer:=inputpointer;
        inputfile^.linebuf:=linebuf;
        inputfile^.maxlinebuf:=maxlinebuf;
      end;


    procedure tscannerfile.restoreinputfile;
      begin
        bufstart:=inputfile^.savebufstart;
        bufsize:=inputfile^.savebufsize;
        lastlinepos:=inputfile^.savelastlinepos;
        line_no:=inputfile^.saveline_no;
        inputbuffer:=inputfile^.saveinputbuffer;
        inputpointer:=inputfile^.saveinputpointer;
        linebuf:=inputfile^.linebuf;
        maxlinebuf:=inputfile^.maxlinebuf;
      end;


    procedure tscannerfile.nextfile;
      begin
        if assigned(inputfile^.next) then
         begin
           inputfile:=inputfile^.next;
           restoreinputfile;
         end;
      end;


    procedure tscannerfile.addfile(hp:pinputfile);
      begin
        saveinputfile;
      { add to list }
        hp^.next:=inputfile;
        inputfile:=hp;
      { load new inputfile }
        restoreinputfile;
      end;


    procedure tscannerfile.reload;
      begin
      { safety check }
        if closed then
         exit;
        repeat
        { still more to read?, then change the #0 to a space so its seen
          as a seperator }
          if (bufsize>0) and (inputpointer-inputbuffer<bufsize) then
           begin
             c:=' ';
             inc(longint(inputpointer));
             exit;
           end;
        { can we read more from this file ? }
          if filenotatend then
           begin
             readbuf;
             if line_no=0 then
              line_no:=1;
             inputpointer:=inputbuffer;
           end
          else
           begin
             close;
           { no next module, than EOF }
             if not assigned(inputfile^.next) then
              begin
                c:=#26;
                exit;
              end;
           { load next file and reopen it }
             nextfile;
             reopen;
           { status }
             Message1(scan_d_back_in,inputfile^.name^);
           { load some current_module fields }
             current_module^.current_index:=inputfile^.ref_index;
           end;
        { load next char }
          c:=inputpointer^;
          inc(longint(inputpointer));
        until c<>#0; { if also end, then reload again }
      end;


    procedure tscannerfile.setbuf(p:pchar;l:longint);
      begin
        inputbuffer:=p;
        inputbufsize:=l;
        inputpointer:=inputbuffer;
      end;


    procedure tscannerfile.gettokenpos;
    { load the values of tokenpos and lasttokenpos }
      begin
        lasttokenpos:=bufstart+(inputpointer-inputbuffer);
        tokenpos.line:=line_no;
        tokenpos.column:=lasttokenpos-lastlinepos;
        tokenpos.fileindex:=current_module^.current_index;
        aktfilepos:=tokenpos;
      end;


    procedure tscannerfile.inc_comment_level;
      begin
         inc(comment_level);
         if (comment_level>1) then
          Message1(scan_w_comment_level,tostr(comment_level));
      end;


    procedure tscannerfile.dec_comment_level;
      begin
         if (cs_tp_compatible in aktswitches) or
            (cs_delphi2_compatible in aktswitches) then
           comment_level:=0
         else
           dec(comment_level);
      end;


    procedure tscannerfile.linebreak;
      var
         cur : char;
{$ifdef SourceLine}
         hp  : plongint;
{$endif SourceLine}
      begin
        if (byte(inputpointer^)=0) and
           filenotatend then
          begin
             cur:=c;
             reload;
             if byte(cur)+byte(c)<>23 then
               dec(longint(inputpointer));
          end
        else
         begin
         { Fix linebreak to be only newline (=#10) for all types of linebreaks }
           if (byte(inputpointer^)+byte(c)=23) then
             inc(longint(inputpointer));
         end;
        c:=newline;
      { increase line counters }
        lastlinepos:=bufstart+(inputpointer-inputbuffer);
        inc(line_no);
      { update linebuffer }
{$ifdef SourceLine}
        if line_no>maxlinebuf then
         begin
           { create new linebuf and move old info }
           getmem(hp,maxlinebuf+linebufincrease);
           if assigned(linebuf) then
            begin
              move(linebuf^,hp^,maxlinebuf shl 2);
              freemem(linebuf,maxlinebuf);
            end;
           { set new linebuf }
           linebuf:=hp;
           inc(maxlinebuf,linebufincrease);
         end;
        plongint(longint(linebuf)+line_no*2)^:=lastlinepos;
{$endif SourceLine}
      { update for status and call the show status routine }
        aktfilepos.line:=line_no; { update for v_status }
        inc(status.compiledlines);
        ShowStatus;
      end;


    procedure tscannerfile.checkpreprocstack;
      begin
      { check for missing ifdefs }
        while assigned(preprocstack) do
         begin
           Message3(scan_e_endif_expected,'$IF(N)(DEF)',preprocstack^.name,tostr(preprocstack^.line_nb));
           poppreprocstack;
         end;
      end;


    procedure tscannerfile.poppreprocstack;
      var
         hp : ppreprocstack;
      begin
        if assigned(preprocstack) then
         begin
           hp:=preprocstack^.next;
           dispose(preprocstack,done);
           preprocstack:=hp;
         end
        else
         Message(scan_e_endif_without_if);
      end;


    procedure tscannerfile.addpreprocstack(a:boolean;const s:string;w:tmsgconst);
      begin
        preprocstack:=new(ppreprocstack,init(((preprocstack=nil) or preprocstack^.accept) and a,preprocstack));
        preprocstack^.name:=s;
        preprocstack^.line_nb:=line_no;
        if preprocstack^.accept then
         Message2(w,preprocstack^.name,'accepted')
        else
         Message2(w,preprocstack^.name,'rejected');
      end;


    procedure tscannerfile.elsepreprocstack;
      begin
        if assigned(preprocstack) then
         begin
           if not(assigned(preprocstack^.next)) or (preprocstack^.next^.accept) then
            preprocstack^.accept:=not preprocstack^.accept;
           if preprocstack^.accept then
            Message2(scan_c_else_found,preprocstack^.name,'accepted')
           else
            Message2(scan_c_else_found,preprocstack^.name,'rejected');
         end
        else
         Message(scan_e_endif_without_if);
      end;


    procedure tscannerfile.readchar;
      begin
        c:=inputpointer^;
        if c=#0 then
         reload
        else
         inc(longint(inputpointer));
        if c in [#10,#13] then
         linebreak;
      end;


    procedure tscannerfile.readstring;
      var
        i : longint;
      begin
        i:=0;
        repeat
          case c of
                 '_',
            '0'..'9',
            'A'..'Z' : begin
                         if i<255 then
                          begin
                            inc(i);
                            orgpattern[i]:=c;
                            pattern[i]:=c;
                          end;
                         c:=inputpointer^;
                         inc(longint(inputpointer));
                       end;
            'a'..'z' : begin
                         if i<255 then
                          begin
                            inc(i);
                            orgpattern[i]:=c;
                            pattern[i]:=chr(ord(c)-32)
                          end;
                         c:=inputpointer^;
                         inc(longint(inputpointer));
                       end;
                  #0 : reload;
             #13,#10 : begin
                         linebreak;
                         break;
                       end;
          else
           break;
          end;
        until false;
        orgpattern[0]:=chr(i);
        pattern[0]:=chr(i);
      end;


    procedure tscannerfile.readnumber;
      var
        base,
        i  : longint;
      begin
        case c of
         '%' : begin
                 readchar;
                 base:=2;
                 pattern[1]:='%';
                 i:=1;
               end;
         '$' : begin
                 readchar;
                 base:=16;
                 pattern[1]:='$';
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
              pattern[i]:=c;
            end;
        { get next char }
           c:=inputpointer^;
           if c=#0 then
            reload
           else
            inc(longint(inputpointer));
         end;
      { was the next char a linebreak ? }
        if c in [#10,#13] then
         linebreak;
        pattern[0]:=chr(i);
      end;


    function tscannerfile.readid:string;
      begin
        readstring;
        readid:=pattern;
      end;


    function tscannerfile.readval:longint;
      var
        l : longint;
        w : word;
      begin
        readnumber;
        valint(pattern,l,w);
        readval:=l;
      end;


    function tscannerfile.readcomment:string;
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
          c:=inputpointer^;
          if c=#0 then
           reload
          else
           inc(longint(inputpointer));
          if c in [#10,#13] then
           linebreak;
        until false;
        readcomment[0]:=chr(i);
      end;


    procedure tscannerfile.skipspace;
      begin
        while c in [' ',#9..#13] do
         begin
           c:=inputpointer^;
           if c=#0 then
            reload
           else
            inc(longint(inputpointer));
           if c in [#10,#13] then
            linebreak;
         end;
      end;


    procedure tscannerfile.skipuntildirective;
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
           c:=inputpointer^;
           if c=#0 then
            reload
           else
            inc(longint(inputpointer));
           if c in [#10,#13] then
            linebreak;
         until (found=2);
      end;

{$i scandir.inc}

    procedure tscannerfile.skipcomment;
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
           c:=inputpointer^;
           if c=#0 then
            reload
           else
            inc(longint(inputpointer));
           if c in [#10,#13] then
            linebreak;
         end;
      end;


    procedure tscannerfile.skipdelphicomment;
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


    procedure tscannerfile.skipoldtpcomment;
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
             c:=inputpointer^;
             if c=#0 then
              reload
             else
              inc(longint(inputpointer));
             if c in [#10,#13] then
              linebreak;
           until (found=2);
         end;
      end;


    function tscannerfile.yylex : ttoken;
      var
        y       : ttoken;
        code    : word;
        l       : longint;
        mac     : pmacrosym;
        hp      : pinputfile;
        macbuf  : pchar;
        asciinr : string[3];
      label
         exit_label;
      begin
        { was the last character a point ? }
        { this code is needed because the scanner if there is a 1. found if  }
        { this is a floating point number or range like 1..3                 }
        if do_special>0 then
          begin
             gettokenpos;
             l:=do_special;
             do_special:=0;
             case l of
              1 : begin
                    if c='.' then
                     begin
                       readchar;
                       yylex:=POINTPOINT;
                       goto exit_label;
                     end;
                    yylex:=POINT;
                    goto exit_label;
                  end;
              2 : begin
                    yylex:=CARET;
                    readchar;
                    goto exit_label;
                  end;
             end;
          end;

      { Skip all spaces and comments }
        repeat
          case c of
           '{' : skipcomment;
   ' ',#9..#13 : skipspace;
          else
           break;
          end;
        until false;

      { Save current token position }
        gettokenpos;

      { Check first for a identifier/keyword, this is 20+% faster (PFV) }
        if c in ['_','A'..'Z','a'..'z'] then
         begin
           readstring;
           if (length(pattern) in [2..id_len]) and is_keyword(y) then
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
                    dec(longint(inputpointer));
                    hp:=new(pinputfile,init('Macro '+pattern));
                    addfile(hp);
                    getmem(macbuf,mac^.buflen+1);
                    setbuf(macbuf,mac^.buflen+1);
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
              yylex:=ID;
            end;
           if (c='^') then
            do_special:=2;
           goto exit_label;
         end
        else
         begin
           case c of
                '$' : begin
                         readnumber;
                         yylex:=INTCONST;
                         goto exit_label;
                      end;
                '%' : begin
                         readnumber;
                         yylex:=INTCONST;
                         goto exit_label;
                      end;
           '0'..'9' : begin
                        readnumber;
                        if (c in ['.','e','E']) then
                         begin
                         { first check for a . }
                           if c='.' then
                            begin
                              readchar;
                              if not(c in ['0'..'9']) then
                               begin
                                 do_special:=1;
                                 yylex:=INTCONST;
                                 goto exit_label;
                               end;
                              pattern:=pattern+'.';
                              while c in ['0'..'9'] do
                               begin
                                 pattern:=pattern+c;
                                 readchar;
                               end;
                            end;
                         { E can also follow after a point is scanned }
                           if c in ['e','E'] then
                            begin
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
                            end;
                           yylex:=REALNUMBER;
                           goto exit_label;
                         end;
                        yylex:=INTCONST;
                        goto exit_label;
                      end;
                ';' : begin
                        readchar;
                        yylex:=SEMICOLON;
                        goto exit_label;
                      end;
                '[' : begin
                        readchar;
                        yylex:=LECKKLAMMER;
                        goto exit_label;
                      end;
                ']' : begin
                        readchar;
                        yylex:=RECKKLAMMER;
                        goto exit_label;
                      end;
                '(' : begin
                        readchar;
                        if c='*' then
                         begin
                           skipoldtpcomment;
                        {$ifndef TP}
                           yylex:=yylex();
                        {$else}
                           yylex:=yylex;
                        {$endif}
                           exit;
                         end;
                        yylex:=LKLAMMER;
                        goto exit_label;
                      end;
                ')' : begin
                        readchar;
                        yylex:=RKLAMMER;
                        goto exit_label;
                      end;
                '+' : begin
                        readchar;
                        if (c='=') and support_c_operators then
                         begin
                           readchar;
                           yylex:=_PLUSASN;
                           goto exit_label;
                         end;
                        yylex:=PLUS;
                        goto exit_label;
                      end;
                '-' : begin
                        readchar;
                        if (c='=') and support_c_operators then
                         begin
                           readchar;
                           yylex:=_MINUSASN;
                           goto exit_label;
                         end;
                        yylex:=MINUS;
                        goto exit_label;
                      end;
                ':' : begin
                        readchar;
                        if c='=' then
                         begin
                           readchar;
                           yylex:=ASSIGNMENT;
                           goto exit_label;
                         end;
                        yylex:=COLON;
                        goto exit_label;
                      end;
                '*' : begin
                        readchar;
                        if (c='=') and support_c_operators then
                         begin
                           readchar;
                           yylex:=_STARASN;
                         end
                        else
                         if c='*' then
                          begin
                            readchar;
                            yylex:=STARSTAR;
                          end
                        else
                         yylex:=STAR;
                        goto exit_label;
                      end;
                '/' : begin
                        readchar;
                        case c of
                         '=' : begin
                                 if support_c_operators then
                                  begin
                                    readchar;
                                    yylex:=_SLASHASN;
                                    goto exit_label;
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
                        yylex:=SLASH;
                        goto exit_label;
                      end;
           '='      : begin
                        readchar;
                        yylex:=EQUAL;
                        goto exit_label;
                      end;
           '.'      : begin
                        readchar;
                        if c='.' then
                         begin
                           readchar;
                           yylex:=POINTPOINT;
                           goto exit_label;
                         end
                        else
                         yylex:=POINT;
                        goto exit_label;
                      end;
                '@' : begin
                        readchar;
                        if c='@' then
                         begin
                           readchar;
                           yylex:=DOUBLEADDR;
                         end
                        else
                         yylex:=KLAMMERAFFE;
                        goto exit_label;
                      end;
                ',' : begin
                        readchar;
                        yylex:=COMMA;
                        goto exit_label;
                      end;
      '''','#','^' :  begin
                        if c='^' then
                         begin
                           readchar;
                           c:=upcase(c);
                           if not(block_type=bt_type) and (c in ['A'..'Z']) then
                            begin
                              pattern:=chr(ord(c)-64);
                              readchar;
                            end
                           else
                            begin
                              yylex:=CARET;
                              goto exit_label;
                            end;
                         end
                        else
                         pattern:='';
                        repeat
                          case c of
                           '#' : begin
                                   readchar; { read # }
                                   if c='$' then
                                     begin
                                        readchar; { read leading $ }
                                        asciinr:='$';
                                        while (upcase(c) in ['A'..'F','0'..'9']) and (length(asciinr)<3) do
                                         begin
                                           asciinr:=asciinr+c;
                                           readchar;
                                         end;
                                     end
                                   else
                                     begin
                                        asciinr:='';
                                        while (c in ['0'..'9']) and (length(asciinr)<3) do
                                         begin
                                           asciinr:=asciinr+c;
                                           readchar;
                                         end;
                                     end;
                                   valint(asciinr,l,code);
                                   if (asciinr='') or (code<>0) or
                                      (l<0) or (l>255) then
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
                        if length(pattern)=1 then
                         yylex:=CCHAR
                        else
                         yylex:=CSTRING;
                        goto exit_label;
                      end;
                '>' : begin
                        readchar;
                        case c of
                         '=' : begin
                                 readchar;
                                 yylex:=GTE;
                                 goto exit_label;
                               end;
                         '>' : begin
                                 readchar;
                                 yylex:=_SHR;
                                 goto exit_label;
                               end;
                         '<' : begin { >< is for a symetric diff for sets }
                                 readchar;
                                 yylex:=SYMDIF;
                                 goto exit_label;
                               end;
                        end;
                        yylex:=GT;
                        goto exit_label;
                      end;
                '<' : begin
                        readchar;
                        case c of
                         '>' : begin
                                 readchar;
                                 yylex:=UNEQUAL;
                                 goto exit_label;
                               end;
                         '=' : begin
                                 readchar;
                                 yylex:=LTE;
                                 goto exit_label;
                               end;
                         '<' : begin
                                 readchar;
                                 yylex:=_SHL;
                                 goto exit_label;
                               end;
                        end;
                        yylex:=LT;
                        goto exit_label;
                      end;
                #26 : begin
                        yylex:=_EOF;
                        goto exit_label;
                      end;
           else
            begin
              Message(scan_f_illegal_char);
            end;
           end;
        end;
exit_label:
      end;


    function tscannerfile.readpreproc:ttoken;
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


    function tscannerfile.asmgetchar : char;
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

end.
{
  $Log$
  Revision 1.38  1998-08-10 10:18:34  peter
    + Compiler,Comphook unit which are the new interface units to the
      compiler

  Revision 1.37  1998/07/23 12:40:41  michael
  No nested comments in Delphi mode.

  Revision 1.36  1998/07/20 22:17:17  florian
    * hex constants in numeric char (#$54#$43 ...) are now allowed
    * there was a bug in record_var_dec which prevents the used
      of nested variant records (for example drivers.tevent of tv)

  Revision 1.35  1998/07/14 21:38:13  peter
    + support for with p^do constructs

  Revision 1.34  1998/07/14 14:47:04  peter
    * released NEWINPUT

  Revision 1.33  1998/07/10 10:48:40  peter
    * fixed realnumber scanning
    * [] after asmblock was not uppercased anymore

  Revision 1.31  1998/07/07 17:39:38  peter
    * fixed $I  with following eof

  Revision 1.30  1998/07/07 12:32:55  peter
    * status.currentsource is now calculated in verbose (more accurated)

  Revision 1.29  1998/07/07 11:20:11  peter
    + NEWINPUT for a better inputfile and scanner object

  Revision 1.28  1998/07/01 15:26:57  peter
    * better bufferfile.reset error handling

  Revision 1.27  1998/06/25 08:48:19  florian
    * first version of rtti support

  Revision 1.26  1998/06/16 08:56:30  peter
    + targetcpu
    * cleaner pmodules for newppu

  Revision 1.25  1998/06/13 00:10:15  peter
    * working browser and newppu
    * some small fixes against crashes which occured in bp7 (but not in
      fpc?!)

  Revision 1.24  1998/06/12 10:32:36  pierre
    * column problem hopefully solved
    + C vars declaration changed

  Revision 1.23  1998/06/03 22:49:02  peter
    + wordbool,longbool
    * rename bis,von -> high,low
    * moved some systemunit loading/creating to psystem.pas

  Revision 1.21  1998/05/27 00:20:32  peter
    * some scanner optimizes
    * automaticly aout2exe for go32v1
    * fixed dynamiclinker option which was added at the wrong place

  Revision 1.20  1998/05/23 01:21:30  peter
    + aktasmmode, aktoptprocessor, aktoutputformat
    + smartlink per module $SMARTLINK-/+ (like MMX) and moved to aktswitches
    + $LIBNAME to set the library name where the unit will be put in
    * splitted cgi386 a bit (codeseg to large for bp7)
    * nasm, tasm works again. nasm moved to ag386nsm.pas

  Revision 1.19  1998/05/20 09:42:37  pierre
    + UseTokenInfo now default
    * unit in interface uses and implementation uses gives error now
    * only one error for unknown symbol (uses lastsymknown boolean)
      the problem came from the label code !
    + first inlined procedures and function work
      (warning there might be allowed cases were the result is still wrong !!)
    * UseBrower updated gives a global list of all position of all used symbols
      with switch -gb

  Revision 1.18  1998/05/12 10:47:00  peter
    * moved printstatus to verb_def
    + V_Normal which is between V_Error and V_Warning and doesn't have a
      prefix like error: warning: and is included in V_Default
    * fixed some messages
    * first time parameter scan is only for -v and -T
    - removed old style messages

  Revision 1.17  1998/05/06 08:38:47  pierre
    * better position info with UseTokenInfo
      UseTokenInfo greatly simplified
    + added check for changed tree after first time firstpass
      (if we could remove all the cases were it happen
      we could skip all firstpass if firstpasscount > 1)
      Only with ExtDebug

  Revision 1.16  1998/05/04 17:54:28  peter
    + smartlinking works (only case jumptable left todo)
    * redesign of systems.pas to support assemblers and linkers
    + Unitname is now also in the PPU-file, increased version to 14

  Revision 1.15  1998/05/01 16:38:46  florian
    * handling of private and protected fixed
    + change_keywords_to_tp implemented to remove
      keywords which aren't supported by tp
    * break and continue are now symbols of the system unit
    + widestring, longstring and ansistring type released

  Revision 1.14  1998/04/30 15:59:42  pierre
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
