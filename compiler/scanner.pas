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
{$ifdef Delphi}
       dmisc,
{$endif Delphi}
       globtype,version,tokens,
       cobjects,globals,verbose,comphook,files;

    const
{$ifdef TP}
       maxmacrolen=1024;
{$else}
       maxmacrolen=16*1024;
{$endif}
       Newline = #10;


    type
       pmacrobuffer = ^tmacrobuffer;
       tmacrobuffer = array[0..maxmacrolen-1] of char;

       preproctyp = (pp_ifdef,pp_ifndef,pp_if,pp_ifopt,pp_else);
       ppreprocstack = ^tpreprocstack;
       tpreprocstack = object
          typ     : preproctyp;
          accept  : boolean;
          next    : ppreprocstack;
          name    : stringid;
          line_nb : longint;
          constructor init(atyp:preproctyp;a:boolean;n:ppreprocstack);
          destructor done;
       end;

       pscannerfile = ^tscannerfile;
       tscannerfile = object
          inputfile    : pinputfile;  { current inputfile list }

          inputbuffer,                { input buffer }
          inputpointer : pchar;
          inputstart   : longint;

          line_no,                    { line }
          lastlinepos  : longint;

          lasttokenpos : longint;     { token }
          lasttoken,
          nexttoken    : ttoken;

          comment_level,
          yylexcount     : longint;
          lastasmgetchar : char;
          preprocstack   : ppreprocstack;
          invalid        : boolean; { flag if sourcefiles have been destroyed ! }

          constructor init(const fn:string);
          destructor done;
        { File buffer things }
          function  openinputfile:boolean;
          procedure closeinputfile;
          function  tempopeninputfile:boolean;
          procedure tempcloseinputfile;
          procedure saveinputfile;
          procedure restoreinputfile;
          procedure nextfile;
          procedure addfile(hp:pinputfile);
          procedure reload;
          procedure insertmacro(p:pchar;len:longint);
        { Scanner things }
          procedure gettokenpos;
          procedure inc_comment_level;
          procedure dec_comment_level;
          procedure checkpreprocstack;
          procedure poppreprocstack;
          procedure addpreprocstack(atyp : preproctyp;a:boolean;const s:string;w:tmsgconst);
          procedure elsepreprocstack;
          procedure linebreak;
          procedure readchar;
          procedure readstring;
          procedure readnumber;
          function  readid:string;
          function  readval:longint;
          function  readcomment:string;
          function  readstate:char;
          procedure skipspace;
          procedure skipuntildirective;
          procedure skipcomment;
          procedure skipdelphicomment;
          procedure skipoldtpcomment;
          procedure skipccomment;
          procedure readtoken;
          function  readpreproc:ttoken;
          function  asmgetchar:char;
       end;

    var
        c              : char;
        orgpattern,
        pattern        : string;
        current_scanner : pscannerfile;


implementation

    uses
      dos,systems,symtable,switches;

{*****************************************************************************
                              Helper routines
*****************************************************************************}

    type
      tokenidxrec=record
        first,last : ttoken;
      end;
    var
      tokenidx:array[2..tokenidlen,'A'..'Z'] of tokenidxrec;

    const
      { use any special name that is an invalid file name to avoid problems }
      macro_special_name = '____Macro____';
      preprocstring : array [preproctyp] of string[7]
        = ('$IFDEF','$IFNDEF','$IF','$IFOPT','$ELSE');


    procedure create_tokenidx;
    { create an index with the first and last token for every possible token
      length, so a search only will be done in that small part }
      var
        t : ttoken;
      begin
        fillchar(tokenidx,sizeof(tokenidx),0);
        for t:=low(ttoken) to high(ttoken) do
         begin
           if not tokeninfo[t].special then
            begin
              if ord(tokenidx[length(tokeninfo[t].str),tokeninfo[t].str[1]].first)=0 then
               tokenidx[length(tokeninfo[t].str),tokeninfo[t].str[1]].first:=t;
              tokenidx[length(tokeninfo[t].str),tokeninfo[t].str[1]].last:=t;
            end;
         end;
      end;


    function is_keyword(const s:string):boolean;
      var
        low,high,mid : longint;
      begin
        if not (length(s) in [2..tokenidlen]) then
         begin
           is_keyword:=false;
           exit;
         end;
        low:=ord(tokenidx[length(s),s[1]].first);
        high:=ord(tokenidx[length(s),s[1]].last);
        while low<high do
         begin
           mid:=(high+low+1) shr 1;
           if pattern<tokeninfo[ttoken(mid)].str then
            high:=mid-1
           else
            low:=mid;
         end;
        is_keyword:=(pattern=tokeninfo[ttoken(high)].str) and
                    (tokeninfo[ttoken(high)].keyword in aktmodeswitches);
      end;


{*****************************************************************************
                              TPreProcStack
*****************************************************************************}

    constructor tpreprocstack.init(atyp : preproctyp;a:boolean;n:ppreprocstack);
      begin
        accept:=a;
        typ:=atyp;
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
        current_module^.sourcefiles^.register_file(inputfile);
      { reset localinput }
        inputbuffer:=nil;
        inputpointer:=nil;
        inputstart:=0;
      { reset scanner }
        preprocstack:=nil;
        comment_level:=0;
        yylexcount:=0;
        block_type:=bt_general;
        line_no:=0;
        lastlinepos:=0;
        lasttokenpos:=0;
        lasttoken:=NOTOKEN;
        nexttoken:=NOTOKEN;
        lastasmgetchar:=#0;
        invalid:=false;
      { load block }
        if not openinputfile then
         Message1(scan_f_cannot_open_input,fn);
        reload;
      end;


    destructor tscannerfile.done;
      begin
        if not invalid then
          begin
             if status.errorcount=0 then
              checkpreprocstack;
           { close file, but only if we are the first compile }
           { probably not necessary anymore with invalid flag PM }
             if not current_module^.in_second_compile then
              begin
                if not inputfile^.closed then
                 closeinputfile;
              end;
          end;
       end;


    function tscannerfile.openinputfile:boolean;
      begin
        openinputfile:=inputfile^.open;
      { load buffer }
        inputbuffer:=inputfile^.buf;
        inputpointer:=inputfile^.buf;
        inputstart:=inputfile^.bufstart;
      { line }
        line_no:=0;
        lastlinepos:=0;
        lasttokenpos:=0;
      end;


    procedure tscannerfile.closeinputfile;
      begin
        inputfile^.close;
      { reset buffer }
        inputbuffer:=nil;
        inputpointer:=nil;
        inputstart:=0;
      { reset line }
        line_no:=0;
        lastlinepos:=0;
        lasttokenpos:=0;
      end;


    function tscannerfile.tempopeninputfile:boolean;
      begin
        tempopeninputfile:=inputfile^.tempopen;
      { reload buffer }
        inputbuffer:=inputfile^.buf;
        inputpointer:=inputfile^.buf;
        inputstart:=inputfile^.bufstart;
      end;


    procedure tscannerfile.tempcloseinputfile;
      begin
        inputfile^.setpos(inputstart+(inputpointer-inputbuffer));
        inputfile^.tempclose;
      { reset buffer }
        inputbuffer:=nil;
        inputpointer:=nil;
        inputstart:=0;
      end;


    procedure tscannerfile.saveinputfile;
      begin
        inputfile^.saveinputpointer:=inputpointer;
        inputfile^.savelastlinepos:=lastlinepos;
        inputfile^.saveline_no:=line_no;
      end;


    procedure tscannerfile.restoreinputfile;
      begin
        inputpointer:=inputfile^.saveinputpointer;
        lastlinepos:=inputfile^.savelastlinepos;
        line_no:=inputfile^.saveline_no;
        if not inputfile^.is_macro then
          parser_current_file:=inputfile^.name^;
      end;


    procedure tscannerfile.nextfile;
      var
        to_dispose : pinputfile;
      begin
        if assigned(inputfile^.next) then
         begin
           if inputfile^.is_macro then
             to_dispose:=inputfile
           else
             to_dispose:=nil;
           { we can allways close the file, no ? }
           inputfile^.close;
           inputfile:=inputfile^.next;
           if assigned(to_dispose) then
             dispose(to_dispose,done);
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
        with inputfile^ do
         begin
           repeat
           { still more to read?, then change the #0 to a space so its seen
             as a seperator }
             if (c=#0) and (bufsize>0) and (inputpointer-inputbuffer<bufsize) then
              begin
                c:=' ';
                inc(longint(inputpointer));
                exit;
              end;
           { can we read more from this file ? }
             if (c<>#26) and (not endoffile) then
              begin
                readbuf;
                inputpointer:=buf;
                inputbuffer:=buf;
                inputstart:=bufstart;
              { first line? }
                if line_no=0 then
                 begin
                   line_no:=1;
                   if cs_asm_source in aktglobalswitches then
                     inputfile^.setline(line_no,bufstart);
                 end;
              end
             else
              begin
              { load eof position in tokenpos/aktfilepos }
                gettokenpos;
              { close file }
                closeinputfile;
              { no next module, than EOF }
                if not assigned(inputfile^.next) then
                 begin
                   c:=#26;
                   exit;
                 end;
              { load next file and reopen it }
                nextfile;
                tempopeninputfile;
              { status }
                Message1(scan_t_back_in,inputfile^.name^);
              end;
           { load next char }
             c:=inputpointer^;
             inc(longint(inputpointer));
           until c<>#0; { if also end, then reload again }
         end;
      end;


    procedure tscannerfile.insertmacro(p:pchar;len:longint);
      var
        hp : pinputfile;
      begin
      { save old postion and decrease linebreak }
        if c=newline then
         dec(line_no);
        dec(longint(inputpointer));
        tempcloseinputfile;
      { create macro 'file' }
        { use special name to dispose after !! }
        hp:=new(pinputfile,init(macro_special_name));
        addfile(hp);
        with inputfile^ do
         begin
           setmacro(p,len);
         { local buffer }
           inputbuffer:=buf;
           inputpointer:=buf;
           inputstart:=bufstart;
         end;
      { reset line }
        line_no:=0;
        lastlinepos:=0;
        lasttokenpos:=0;
      { load new c }
        c:=inputpointer^;
        inc(longint(inputpointer));
      end;


    procedure tscannerfile.gettokenpos;
    { load the values of tokenpos and lasttokenpos }
      begin
        lasttokenpos:=inputstart+(inputpointer-inputbuffer);
        tokenpos.line:=line_no;
        tokenpos.column:=lasttokenpos-lastlinepos;
        tokenpos.fileindex:=inputfile^.ref_index;
        aktfilepos:=tokenpos;
      end;


    procedure tscannerfile.inc_comment_level;
      var
         oldaktfilepos : tfileposinfo;
      begin
         if (m_nested_comment in aktmodeswitches) then
           inc(comment_level)
         else
           comment_level:=1;
         if (comment_level>1) then
          begin
             oldaktfilepos:=aktfilepos;
             gettokenpos; { update for warning }
             Message1(scan_w_comment_level,tostr(comment_level));
             aktfilepos:=oldaktfilepos;
          end;
      end;


    procedure tscannerfile.dec_comment_level;
      begin
         if (m_nested_comment in aktmodeswitches) then
           dec(comment_level)
         else
           comment_level:=0;
      end;


    procedure tscannerfile.linebreak;
      var
         cur : char;
         oldtokenpos,
         oldaktfilepos : tfileposinfo;
      begin
        with inputfile^ do
         begin
           if (byte(inputpointer^)=0) and not(endoffile) then
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
           if cs_asm_source in aktglobalswitches then
             inputfile^.setline(line_no,lastlinepos);
         { update for status and call the show status routine,
           but don't touch aktfilepos ! }
           oldaktfilepos:=aktfilepos;
           oldtokenpos:=tokenpos;
           gettokenpos; { update for v_status }
           inc(status.compiledlines);
           ShowStatus;
           aktfilepos:=oldaktfilepos;
           tokenpos:=oldtokenpos;
         end;
      end;


    procedure tscannerfile.checkpreprocstack;
      begin
      { check for missing ifdefs }
        while assigned(preprocstack) do
         begin
           Message3(scan_e_endif_expected,preprocstring[preprocstack^.typ],preprocstack^.name,tostr(preprocstack^.line_nb));
           poppreprocstack;
         end;
      end;


    procedure tscannerfile.poppreprocstack;
      var
        hp : ppreprocstack;
      begin
        if assigned(preprocstack) then
         begin
           Message1(scan_c_endif_found,preprocstack^.name);
           hp:=preprocstack^.next;
           dispose(preprocstack,done);
           preprocstack:=hp;
         end
        else
         Message(scan_e_endif_without_if);
      end;


    procedure tscannerfile.addpreprocstack(atyp : preproctyp;a:boolean;const s:string;w:tmsgconst);
      begin
        preprocstack:=new(ppreprocstack,init(atyp,((preprocstack=nil) or preprocstack^.accept) and a,preprocstack));
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
           preprocstack^.typ:=pp_else;
           preprocstack^.line_nb:=line_no;
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
        case c of
         #26 : reload;
         #10,
         #13 : linebreak;
        end;
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
              #26 : begin
                      reload;
                      if c=#26 then
                        break;
                    end;
             #13,#10 : begin
                         linebreak;
                         break;
                       end;
          else
           break;
          end;
        until false;
        {$ifndef TP}
          {$ifopt H+}
            setlength(orgpattern,i);
            setlength(pattern,i);
          {$else}
            orgpattern[0]:=chr(i);
            pattern[0]:=chr(i);
          {$endif}
        {$else}
          orgpattern[0]:=chr(i);
          pattern[0]:=chr(i);
        {$endif}
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
        case c of
         #26 : reload;
         #10,
         #13 : linebreak;
        end;
        {$ifndef TP}
          {$ifopt H+}
            setlength(pattern,i);
          {$else}
            pattern[0]:=chr(i);
          {$endif}
        {$else}
          pattern[0]:=chr(i);
        {$endif}
      end;


    function tscannerfile.readid:string;
      begin
        readstring;
        readid:=pattern;
      end;


    function tscannerfile.readval:longint;
      var
        l : longint;
        w : integer;
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
        {$ifndef TP}
          {$ifopt H+}
            setlength(readcomment,i);
          {$else}
            readcomment[0]:=chr(i);
          {$endif}
        {$else}
          readcomment[0]:=chr(i);
        {$endif}
      end;


    function tscannerfile.readstate:char;
      var
        state : char;
      begin
        state:=' ';
        if c=' ' then
         begin
           current_scanner^.skipspace;
           current_scanner^.readid;
           if pattern='ON' then
            state:='+'
           else
            if pattern='OFF' then
             state:='-';
         end
        else
         state:=c;
        if not (state in ['+','-']) then
         Message(scan_e_wrong_switch_toggle);
        readstate:=state;
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
           case c of
            #26 : reload;
            #10,
            #13 : linebreak;
           end;
         end;
      end;


    procedure tscannerfile.skipuntildirective;
      var
        found : longint;
        next_char_loaded : boolean;
      begin
         found:=0;
         next_char_loaded:=false;
         repeat
           case c of
             #26 :
               Message(scan_f_end_of_file);
             '{' :
               begin
                 if comment_level=0 then
                  found:=1;
                 inc_comment_level;
               end;
             '}' :
               begin
                 dec_comment_level;
                 found:=0;
               end;
             '$' :
               begin
                 if found=1 then
                  found:=2;
               end;
             '(' :
               if (m_tp in aktmodeswitches) then
                begin
                  readchar;
                  if c='*' then
                    skipoldtpcomment
                  else
                    next_char_loaded:=true;
                end;
             else
                found:=0;
           end;
           if next_char_loaded then
             next_char_loaded:=false
           else
             begin
                c:=inputpointer^;
                if c=#0 then
                  reload
                else
                  inc(longint(inputpointer));
                case c of
                  #26 : reload;
                  #10,
                  #13 : linebreak;
                end;
             end;
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
           case c of
            #26 : reload;
            #10,
            #13 : linebreak;
           end;
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
                       begin
                         inc_comment_level;
                         found:=0;
                       end
                      else
                       found:=1;
                    end;
              ')' : begin
                      if found=1 then
                       begin
                         dec_comment_level;
                         if comment_level=0 then
                          found:=2
                         else
                          found:=0;
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
             case c of
              #26 : reload;
              #10,
              #13 : linebreak;
             end;
           until (found=2);
         end;
      end;


    procedure tscannerfile.skipccomment;
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
                       begin
                         inc_comment_level;
                         found:=0;
                       end
                      else
                       found:=1;
                    end;
              '/' : begin
                      if found=1 then
                       begin
                         dec_comment_level;
                         if comment_level=0 then
                          found:=2
                         else
                          found:=0;
                       end
                      else found:=3;
                    end;
             else
              found:=0;
             end;
             c:=inputpointer^;
             if c=#0 then
              reload
             else
              inc(longint(inputpointer));
             case c of
              #26 : reload;
              #10,
              #13 : linebreak;
             end;
           until (found=2);
         end;
      end;


    procedure tscannerfile.readtoken;
      var
        code    : integer;
        low,high,mid : longint;
        m       : longint;
        mac     : pmacrosym;
        asciinr : string[3];
      label
         exit_label;
      begin
      { was there already a token read, then return that token }
        if nexttoken<>NOTOKEN then
         begin
           token:=nexttoken;
           nexttoken:=NOTOKEN;
           goto exit_label;
         end;

      { Skip all spaces and comments }
        repeat
          case c of
            '{' :
              skipcomment;
            ' ',#9..#13 :
              skipspace;
            else
              break;
          end;
        until false;

      { Save current token position, for EOF its already loaded }
        if c<>#26 then
         gettokenpos;

      { Check first for a identifier/keyword, this is 20+% faster (PFV) }
        if c in ['A'..'Z','a'..'z','_'] then
         begin
           readstring;
           token:=ID;
           idtoken:=ID;
         { keyword or any other known token,
           pattern is always uppercased }
           if (pattern[1]<>'_') and (length(pattern) in [2..tokenidlen]) then
            begin
              low:=ord(tokenidx[length(pattern),pattern[1]].first);
              high:=ord(tokenidx[length(pattern),pattern[1]].last);
              while low<high do
               begin
                 mid:=(high+low+1) shr 1;
                 if pattern<tokeninfo[ttoken(mid)].str then
                  high:=mid-1
                 else
                  low:=mid;
               end;
              if pattern=tokeninfo[ttoken(high)].str then
               begin
                 if tokeninfo[ttoken(high)].keyword in aktmodeswitches then
                  token:=ttoken(high);
                 idtoken:=ttoken(high);
               end;
            end;
         { Only process identifiers and not keywords }
           if token=ID then
            begin
            { this takes some time ... }
              if (cs_support_macro in aktmoduleswitches) then
               begin
                 mac:=pmacrosym(macros^.search(pattern));
                 if assigned(mac) and (assigned(mac^.buftext)) then
                  begin
                    insertmacro(mac^.buftext,mac^.buflen);
                  { handle empty macros }
                    if c=#0 then
                     reload;
                  { play it again ... }
                    inc(yylexcount);
                    if yylexcount>16 then
                     Message(scan_w_macro_deep_ten);
                    readtoken;
                  { that's all folks }
                    dec(yylexcount);
                    exit;
                  end;
               end;
            end;
         { return token }
           goto exit_label;
         end
        else
         begin
           idtoken:=NOID;
           case c of

             '$' :
               begin
                 readnumber;
                 token:=INTCONST;
                 goto exit_label;
               end;

             '%' :
               begin
                 readnumber;
                 token:=INTCONST;
                 goto exit_label;
               end;

             '0'..'9' :
               begin
                 readnumber;
                 if (c in ['.','e','E']) then
                  begin
                  { first check for a . }
                    if c='.' then
                     begin
                       readchar;
                       { is it a .. from a range? }
                       case c of
                         '.' :
                           begin
                             readchar;
                             token:=INTCONST;
                             nexttoken:=POINTPOINT;
                             goto exit_label;
                           end;
                         ')' :
                           begin
                             readchar;
                             token:=INTCONST;
                             nexttoken:=RECKKLAMMER;
                             goto exit_label;
                           end;
                       end;
                       { insert the number after the . }
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
                    token:=REALNUMBER;
                    goto exit_label;
                  end;
                 token:=INTCONST;
                 goto exit_label;
               end;

             ';' :
               begin
                 readchar;
                 token:=SEMICOLON;
                 goto exit_label;
               end;

             '[' :
               begin
                 readchar;
                 token:=LECKKLAMMER;
                 goto exit_label;
               end;

             ']' :
               begin
                 readchar;
                 token:=RECKKLAMMER;
                 goto exit_label;
               end;

             '(' :
               begin
                 readchar;
                 case c of
                   '*' :
                     begin
                       skipoldtpcomment;
                       readtoken;
                       exit;
                     end;
                   '.' :
                     begin
                       readchar;
                       token:=LECKKLAMMER;
                       goto exit_label;
                     end;
                 end;
                 token:=LKLAMMER;
                 goto exit_label;
               end;

             ')' :
               begin
                 readchar;
                 token:=RKLAMMER;
                 goto exit_label;
               end;

             '+' :
               begin
                 readchar;
                 if (c='=') and (cs_support_c_operators in aktmoduleswitches) then
                  begin
                    readchar;
                    token:=_PLUSASN;
                    goto exit_label;
                  end;
                 token:=PLUS;
                 goto exit_label;
               end;

             '-' :
               begin
                 readchar;
                 if (c='=') and (cs_support_c_operators in aktmoduleswitches) then
                  begin
                    readchar;
                    token:=_MINUSASN;
                    goto exit_label;
                  end;
                 token:=MINUS;
                 goto exit_label;
               end;

             ':' :
               begin
                 readchar;
                 if c='=' then
                  begin
                    readchar;
                    token:=ASSIGNMENT;
                    goto exit_label;
                  end;
                 token:=COLON;
                 goto exit_label;
               end;

             '*' :
               begin
                 readchar;
                 if (c='=') and (cs_support_c_operators in aktmoduleswitches) then
                  begin
                    readchar;
                    token:=_STARASN;
                  end
                 else
                  if c='*' then
                   begin
                     readchar;
                     token:=STARSTAR;
                   end
                 else
                  token:=STAR;
                 goto exit_label;
               end;

             '/' :
               begin
                 readchar;
                 case c of
                   '=' :
                     begin
                       if (cs_support_c_operators in aktmoduleswitches) then
                        begin
                          readchar;
                          token:=_SLASHASN;
                          goto exit_label;
                        end;
                     end;
                   '/' :
                     begin
                       skipdelphicomment;
                       readtoken;
                       exit;
                     end;
                  '*' :
                    begin
                      skipccomment;
                      readtoken;
                      exit;
                    end;
                 end;
                 token:=SLASH;
                 goto exit_label;
               end;

             '=' :
               begin
                 readchar;
                 token:=EQUAL;
                 goto exit_label;
               end;

             '.' :
               begin
                 readchar;
                 case c of
                   '.' :
                     begin
                       readchar;
                       token:=POINTPOINT;
                       goto exit_label;
                     end;
                   ')' :
                     begin
                       readchar;
                       token:=RECKKLAMMER;
                       goto exit_label;
                     end;
                 end;
                 token:=POINT;
                 goto exit_label;
               end;

             '@' :
               begin
                 readchar;
                 if c='@' then
                  begin
                    readchar;
                    token:=DOUBLEADDR;
                  end
                 else
                  token:=KLAMMERAFFE;
                 goto exit_label;
               end;

             ',' :
               begin
                 readchar;
                 token:=COMMA;
                 goto exit_label;
               end;

             '''','#','^' :
               begin
                 if c='^' then
                  begin
                    readchar;
                    c:=upcase(c);
                    if (block_type=bt_type) or
                       (lasttoken=ID) or
                       (lasttoken=RKLAMMER) or (lasttoken=RECKKLAMMER) or (lasttoken=CARET) then
                     begin
                       token:=CARET;
                       goto exit_label;
                     end
                    else
                     begin
                       if c<#64 then
                        pattern:=chr(ord(c)+64)
                       else
                        pattern:=chr(ord(c)-64);
                       readchar;
                     end;
                  end
                 else
                  pattern:='';
                 repeat
                   case c of
                     '#' :
                       begin
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
                         valint(asciinr,m,code);
                         if (asciinr='') or (code<>0) or
                            (m<0) or (m>255) then
                          Message(scan_e_illegal_char_const);
                         pattern:=pattern+chr(m);
                       end;
                     '''' :
                       begin
                         repeat
                           readchar;
                           case c of
                             #26 :
                               Message(scan_f_end_of_file);
                             newline :
                               Message(scan_f_string_exceeds_line);
                             '''' :
                               begin
                                 readchar;
                                 if c<>'''' then
                                  break;
                               end;
                           end;
                           pattern:=pattern+c;
                         until false;
                       end;
                     '^' :
                       begin
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
                  token:=CCHAR
                 else
                  token:=CSTRING;
                 goto exit_label;
               end;

             '>' :
               begin
                 readchar;
                 case c of
                   '=' :
                     begin
                       readchar;
                       token:=GTE;
                       goto exit_label;
                     end;
                   '>' :
                     begin
                       readchar;
                       token:=_SHR;
                       goto exit_label;
                     end;
                   '<' :
                     begin { >< is for a symetric diff for sets }
                       readchar;
                       token:=SYMDIF;
                       goto exit_label;
                     end;
                 end;
                 token:=GT;
                 goto exit_label;
               end;

             '<' :
               begin
                 readchar;
                 case c of
                   '>' :
                     begin
                       readchar;
                       token:=UNEQUAL;
                       goto exit_label;
                     end;
                   '=' :
                     begin
                       readchar;
                       token:=LTE;
                       goto exit_label;
                     end;
                   '<' :
                     begin
                       readchar;
                       token:=_SHL;
                       goto exit_label;
                     end;
                 end;
                 token:=LT;
                 goto exit_label;
               end;

             #26 :
               begin
                 token:=_EOF;
                 checkpreprocstack;
                 goto exit_label;
               end;
             else
               begin
                 Message(scan_f_illegal_char);
               end;
           end;
        end;
exit_label:
        lasttoken:=token;
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
             #26 : begin
                     checkpreprocstack;
                     Message(scan_f_end_of_file);
                   end;
         else
          begin
            readpreproc:=_EOF;
            checkpreprocstack;
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
                  asmgetchar:=c;
                  exit;
                end;
          '/' : begin
                  readchar;
                  if c='/' then
                   begin
                     skipdelphicomment;
                     asmgetchar:=c;
                   end
                  else
                   begin
                     asmgetchar:='/';
                     lastasmgetchar:=c;
                   end;
                  exit;
                end;
          '(' : begin
                  readchar;
                  if c='*' then
                   begin
                     skipoldtpcomment;
                     asmgetchar:=c;
                   end
                  else
                   begin
                     asmgetchar:='(';
                     lastasmgetchar:=c;
                   end;
                  exit;
                end;
         else
          begin
            asmgetchar:=c;
          end;
         end;
      end;

begin
  create_tokenidx;
end.
{
  $Log$
  Revision 1.85  1999-06-02 22:25:49  pierre
  types.pas

  Revision 1.84  1999/05/31 23:28:42  pierre
   * problem with main file end without newline

  Revision 1.83  1999/05/20 14:57:29  peter
    * fixed line counting with macro's

  Revision 1.82  1999/05/04 21:45:04  florian
    * changes to compile it with Delphi 4.0

  Revision 1.81  1999/04/07 14:36:44  pierre
   + better preproc stack checking and report

  Revision 1.80  1999/04/06 11:20:59  peter
    * more hashing for keyword table

  Revision 1.79  1999/04/01 22:05:59  peter
    * '1.' is now parsed as a real

  Revision 1.78  1999/03/26 19:10:06  peter
    * support also ^^

  Revision 1.77  1999/03/26 00:05:45  peter
    * released valintern
    + deffile is now removed when compiling is finished
    * ^( compiles now correct
    + static directive
    * shrd fixed

  Revision 1.76  1999/03/24 23:17:24  peter
    * fixed bugs 212,222,225,227,229,231,233

  Revision 1.75  1999/03/16 21:00:27  peter
    * fixed old tp comment behaviour within directives

  Revision 1.74  1999/03/11 10:46:29  daniel
  * Reverted C styled comments

  Revision 1.73  1999/03/02 22:49:35  peter
    * fixed nested comments for old style tp and c

  Revision 1.72  1999/02/02 00:15:10  florian
    + c styled comments

  Revision 1.71  1999/01/27 13:05:45  pierre
   * give include file name on error

  Revision 1.70  1999/01/19 12:14:38  peter
    * fixed eof bug with includefiles

  Revision 1.69  1998/12/11 00:03:46  peter
    + globtype,tokens,version unit splitted from globals

  Revision 1.68  1998/11/16 15:41:44  peter
    * tp7 didn't like my ifopt H+ :(

  Revision 1.67  1998/11/16 12:18:06  peter
    * H+ fixes

  Revision 1.66  1998/11/05 23:48:29  peter
    * recordtype.field support in constant expressions
    * fixed imul for oa_imm8 which was not allowed
    * fixed reading of local typed constants
    * fixed comment reading which is not any longer a separator

  Revision 1.65  1998/11/03 11:35:02  peter
    * don't check for endif if error

  Revision 1.64  1998/10/21 20:16:05  peter
    * beter line info for conditionals

  Revision 1.63  1998/10/16 14:20:57  daniel
  * Faster keyword scanning.
  * Import library and smartlink library in one file.

  Revision 1.62  1998/10/15 12:22:23  pierre
    * close include files immediately after end reading
      instead of waiting until unit compilation ended !

  Revision 1.61  1998/10/09 11:08:15  peter
    * fixed inputfile^.name^ bug

  Revision 1.60  1998/10/09 08:56:31  pierre
    * several memory leaks fixed

  Revision 1.59  1998/10/08 23:29:05  peter
    * -vu shows unit info, -vt shows tried/used files

  Revision 1.58  1998/10/08 17:17:30  pierre
    * current_module old scanner tagged as invalid if unit is recompiled
    + added ppheap for better info on tracegetmem of heaptrc
      (adds line column and file index)
    * several memory leaks removed ith help of heaptrc !!

  Revision 1.57  1998/10/08 13:45:25  peter
    * EOF position is now correctly saved in aktfilepos

  Revision 1.56  1998/09/30 16:43:38  peter
    * fixed unit interdependency with circular uses

  Revision 1.55  1998/09/28 16:57:26  pierre
    * changed all length(p^.value_str^) into str_length(p)
      to get it work with and without ansistrings
    * changed sourcefiles field of tmodule to a pointer

  Revision 1.54  1998/09/26 17:45:41  peter
    + idtoken and only one token table

  Revision 1.53  1998/09/24 23:49:20  peter
    + aktmodeswitches

  Revision 1.52  1998/09/18 16:03:45  florian
    * some changes to compile with Delphi

  Revision 1.51  1998/09/16 16:41:49  peter
    * merged fixes

  Revision 1.50.2.1  1998/09/16 16:09:49  peter
    * on/off support also for the local/module switches

  Revision 1.50  1998/09/04 08:36:06  peter
    + (. and .) which are equal to [ and ]

  Revision 1.49  1998/09/03 11:24:03  peter
    * moved more inputfile things from tscannerfile to tinputfile
    * changed ifdef Sourceline to cs_asm_source

  Revision 1.48  1998/09/01 12:51:02  peter
    * close also resets lastlinepos

  Revision 1.47  1998/09/01 09:01:52  peter
    * initialize all object variables in .init

  Revision 1.46  1998/08/29 13:49:00  peter
    * fixed freemem calls which had the wrong size sometimes

  Revision 1.45  1998/08/26 15:35:35  peter
    * fixed scannerfiles for macros
    + $I %<environment>%

  Revision 1.44  1998/08/20 16:09:55  pierre
    * tokenpos has to be restored also after
      printstatus

  Revision 1.43  1998/08/20 09:26:45  pierre
    + funcret setting in underproc testing
      compile with _dTEST_FUNCRET

  Revision 1.42  1998/08/19 14:57:51  peter
    * small fix for aktfilepos

  Revision 1.41  1998/08/18 14:17:10  pierre
    * bug about assigning the return value of a function to
      a procvar fixed : warning
      assigning a proc to a procvar need @ in FPC mode !!
    * missing file/line info restored

  Revision 1.40  1998/08/11 14:04:33  peter
    * auto close an open file and better error msg

  Revision 1.39  1998/08/10 14:50:26  peter
    + localswitches, moduleswitches, globalswitches splitting

  Revision 1.38  1998/08/10 10:18:34  peter
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
