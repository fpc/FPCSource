{
    $Id$
    Copyright (c) 1998-2000 by Florian Klaempfl

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
{$ifdef FPC}
  {$goto on}
{$endif FPC}

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
       preprocbufsize=1024;
{$else}
       maxmacrolen=16*1024;
       preprocbufsize=32*1024;
{$endif}
       Newline = #10;


    type
       tcommentstyle = (comment_none,comment_tp,comment_oldtp,comment_delphi,comment_c);

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
          procedure insertmacro(const macname:string;p:pchar;len:longint);
        { Scanner things }
          procedure gettokenpos;
          procedure inc_comment_level;
          procedure dec_comment_level;
          procedure illegal_char(c:char);
          procedure end_of_file;
          procedure checkpreprocstack;
          procedure poppreprocstack;
          procedure addpreprocstack(atyp : preproctyp;a:boolean;const s:string;w:longint);
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
          procedure readtoken;
          function  readpreproc:ttoken;
          function  asmgetchar:char;
       end;

       ppreprocfile=^tpreprocfile;
       tpreprocfile=object
         f   : text;
         buf : pointer;
         spacefound,
         eolfound : boolean;
         constructor init(const fn:string);
         destructor  done;
         procedure Add(const s:string);
         procedure AddSpace;
       end;


    var
        c              : char;
        orgpattern,
        pattern        : string;
        current_scanner : pscannerfile;
        aktcommentstyle : tcommentstyle; { needed to use read_comment from directives }

        preprocfile : ppreprocfile; { used with only preprocessing }


implementation

    uses
{$ifndef delphi}
      dos,
{$endif delphi}
      systems,symtable,switches
{$IFDEF NEWST}
      ,symbols
{$ENDIF NEWST};

{*****************************************************************************
                              Helper routines
*****************************************************************************}

    const
      { use any special name that is an invalid file name to avoid problems }
      preprocstring : array [preproctyp] of string[7]
        = ('$IFDEF','$IFNDEF','$IF','$IFOPT','$ELSE');


    function is_keyword(const s:string):boolean;
      var
        low,high,mid : longint;
      begin
        if not (length(s) in [2..tokenidlen]) then
         begin
           is_keyword:=false;
           exit;
         end;
        low:=ord(tokenidx^[length(s),s[1]].first);
        high:=ord(tokenidx^[length(s),s[1]].last);
        while low<high do
         begin
           mid:=(high+low+1) shr 1;
           if pattern<tokeninfo^[ttoken(mid)].str then
            high:=mid-1
           else
            low:=mid;
         end;
        is_keyword:=(pattern=tokeninfo^[ttoken(high)].str) and
                    (tokeninfo^[ttoken(high)].keyword in aktmodeswitches);
      end;


{*****************************************************************************
                            Preprocessor writting
*****************************************************************************}

    constructor tpreprocfile.init(const fn:string);
      begin
      { open outputfile }
        assign(f,fn);
        {$I-}
         rewrite(f);
        {$I+}
        if ioresult<>0 then
         Comment(V_Fatal,'can''t create file '+fn);
        getmem(buf,preprocbufsize);
        settextbuf(f,buf^,preprocbufsize);
      { reset }
        eolfound:=false;
        spacefound:=false;
      end;


    destructor tpreprocfile.done;
      begin
        close(f);
        freemem(buf,preprocbufsize);
      end;


    procedure tpreprocfile.add(const s:string);
      begin
        write(f,s);
      end;

    procedure tpreprocfile.addspace;
      begin
        if eolfound then
         begin
           writeln(f,'');
           eolfound:=false;
           spacefound:=false;
         end
        else
         if spacefound then
          begin
            write(f,' ');
            spacefound:=false;
          end;
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
        if assigned(current_module) then
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
      { process first read char }
        case c of
         #26 : reload;
         #10,
         #13 : linebreak;
        end;
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
           { when nothing more to read then leave immediatly, so we
             don't change the aktfilepos and leave it point to the last
             char }
           if (c=#26) and (not assigned(next)) then
            exit;
           repeat
           { still more to read?, then change the #0 to a space so its seen
             as a seperator, this can't be used for macro's which can change
             the place of the #0 in the buffer with tempopen }
             if (c=#0) and (bufsize>0) and
                not(inputfile^.is_macro) and
                (inputpointer-inputbuffer<bufsize) then
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


    procedure tscannerfile.insertmacro(const macname:string;p:pchar;len:longint);
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
        hp:=new(pinputfile,init('_Macro_.'+macname));
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


    procedure tscannerfile.illegal_char(c:char);
      var
        s : string;
      begin
        if c in [#32..#255] then
         s:=''''+c+''''
        else
         s:='#'+tostr(ord(c));
        Message2(scan_f_illegal_char,s,'$'+hexstr(ord(c),2));
      end;


    procedure tscannerfile.end_of_file;
      begin
        checkpreprocstack;
        Message(scan_f_end_of_file);
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


    procedure tscannerfile.addpreprocstack(atyp : preproctyp;a:boolean;const s:string;w:longint);
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
           '{' :
             if aktcommentstyle=comment_tp then
              inc_comment_level;
           '}' :
             if aktcommentstyle=comment_tp then
              begin
                readchar;
                dec_comment_level;
                if comment_level=0 then
                 break
                else
                 continue;
              end;
           '*' :
             if aktcommentstyle=comment_oldtp then
              begin
                readchar;
                if c=')' then
                 begin
                   readchar;
                   dec_comment_level;
                   break;
                 end
                else
                 { Add both characters !!}
                 if (i<255) then
                   begin
                   inc(i);
                   readcomment[i]:='*';
                   if (i<255) then
                     begin
                     inc(i);
                     readcomment[i]:='*';
                     end;
                   end;
              end
             else
              { Not old TP comment, so add...}
              begin
              if (i<255) then
               begin
               inc(i);
               readcomment[i]:='*';
               end;
              end;
           #26 :
              end_of_file;
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
            #26 :
              reload;
            #10,
            #13 :
              linebreak;
           end;
         end;
      end;


    procedure tscannerfile.skipuntildirective;
      var
        found : longint;
        next_char_loaded : boolean;
        oldcommentstyle : tcommentstyle;
      begin
         found:=0;
         next_char_loaded:=false;
         oldcommentstyle:=aktcommentstyle;
         repeat
           case c of
             #26 :
               end_of_file;
             '{' :
               begin
                 if not(m_nested_comment in aktmodeswitches) or
                    (comment_level=0) then
                  begin
                    found:=1;
                    aktcommentstyle:=comment_tp;
                  end;
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
             '''' :
               if not(m_nested_comment in aktmodeswitches) then
                begin
                  repeat
                    readchar;
                    case c of
                      #26 :
                        end_of_file;
                      newline :
                        break;
                      '''' :
                        begin
                          readchar;
                          if c<>'''' then
                           break;
                        end;
                    end;
                  until false;
                end;
             '(' :
               begin
                 readchar;
                 if c='*' then
                   begin
                     readchar;
                     if c='$' then
                      begin
                        found:=2;
                        inc_comment_level;
                        aktcommentstyle:=comment_oldtp;
                      end
                     else
                      begin
                        skipoldtpcomment;
                        aktcommentstyle:=oldcommentstyle;
                      end;
                   end
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


{****************************************************************************
                      Include directive scanning/parsing
****************************************************************************}

{$i scandir.inc}


{****************************************************************************
                             Comment Handling
****************************************************************************}

    procedure tscannerfile.skipcomment;
      begin
        aktcommentstyle:=comment_tp;
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
            #26 : end_of_file;
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
        aktcommentstyle:=comment_none;
      end;


    procedure tscannerfile.skipdelphicomment;
      begin
        aktcommentstyle:=comment_delphi;
        inc_comment_level;
        readchar;
      { this is currently not supported }
        if c='$' then
          Message(scan_e_wrong_styled_switch);
      { skip comment }
        while c<>newline do
         begin
           if c=#26 then
            end_of_file;
           readchar;
         end;
        dec_comment_level;
        aktcommentstyle:=comment_none;
      end;


    procedure tscannerfile.skipoldtpcomment;
      var
        found : longint;
      begin
        aktcommentstyle:=comment_oldtp;
        inc_comment_level;
        readchar;
      { this is currently not supported }
        if (c='$') then
         handledirectives;
      { skip comment }
        while (comment_level>0) do
         begin
           found:=0;
           repeat
             case c of
               #26 :
                 end_of_file;
               '*' :
                 begin
                   if found=3 then
                    found:=4
                   else
                    found:=1;
                 end;
               ')' :
                 begin
                   if found in [1,4] then
                    begin
                      dec_comment_level;
                      if comment_level=0 then
                       found:=2
                      else
                       found:=0;
                    end;
                 end;
               '(' :
                 begin
                   if found=4 then
                    inc_comment_level;
                   found:=3;
                 end;
               else
                 begin
                   if found=4 then
                    inc_comment_level;
                   found:=0;
                 end;
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
        aktcommentstyle:=comment_none;
      end;



{****************************************************************************
                               Token Scanner
****************************************************************************}

    procedure tscannerfile.readtoken;
      var
        code    : integer;
        low,high,mid : longint;
        m       : longint;
        mac     : pmacrosym;
        asciinr : string[6];
      label
         exit_label;
      begin
        if localswitcheschanged then
          begin
            aktlocalswitches:=nextaktlocalswitches;
            localswitcheschanged:=false;
          end;
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
              begin
                if parapreprocess then
                 begin
                   if c=#10 then
                    preprocfile^.eolfound:=true
                   else
                    preprocfile^.spacefound:=true;
                 end;
                skipspace;
              end
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
           token:=_ID;
           idtoken:=_ID;
         { keyword or any other known token,
           pattern is always uppercased }
           if (pattern[1]<>'_') and (length(pattern) in [2..tokenidlen]) then
            begin
              low:=ord(tokenidx^[length(pattern),pattern[1]].first);
              high:=ord(tokenidx^[length(pattern),pattern[1]].last);
              while low<high do
               begin
                 mid:=(high+low+1) shr 1;
                 if pattern<tokeninfo^[ttoken(mid)].str then
                  high:=mid-1
                 else
                  low:=mid;
               end;
              if pattern=tokeninfo^[ttoken(high)].str then
               begin
                 if tokeninfo^[ttoken(high)].keyword in aktmodeswitches then
                  if tokeninfo^[ttoken(high)].op=NOTOKEN then
                    token:=ttoken(high)
                  else
                    token:=tokeninfo^[ttoken(high)].op;
                 idtoken:=ttoken(high);
               end;
            end;
         { Only process identifiers and not keywords }
           if token=_ID then
            begin
            { this takes some time ... }
              if (cs_support_macro in aktmoduleswitches) then
               begin
                 mac:=pmacrosym(macros^.search(pattern));
                 if assigned(mac) and (assigned(mac^.buftext)) then
                  begin
                    insertmacro(pattern,mac^.buftext,mac^.buflen);
                  { handle empty macros }
                    if c=#0 then
                     begin
                       reload;
                       case c of
                        #26 : reload;
                        #10,
                        #13 : linebreak;
                       end;
                     end;
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
           idtoken:=_NOID;
           case c of

             '$' :
               begin
                 readnumber;
                 token:=_INTCONST;
                 goto exit_label;
               end;

             '%' :
               begin
                 if (m_tp in aktmodeswitches) then
                  Illegal_Char(c)
                 else
                  begin
                    readnumber;
                    token:=_INTCONST;
                    goto exit_label;
                  end;
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
                             token:=_INTCONST;
                             nexttoken:=_POINTPOINT;
                             goto exit_label;
                           end;
                         ')' :
                           begin
                             readchar;
                             token:=_INTCONST;
                             nexttoken:=_RECKKLAMMER;
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
                        Illegal_Char(c);
                       while c in ['0'..'9'] do
                        begin
                          pattern:=pattern+c;
                          readchar;
                        end;
                     end;
                    token:=_REALNUMBER;
                    goto exit_label;
                  end;
                 token:=_INTCONST;
                 goto exit_label;
               end;

             ';' :
               begin
                 readchar;
                 token:=_SEMICOLON;
                 goto exit_label;
               end;

             '[' :
               begin
                 readchar;
                 token:=_LECKKLAMMER;
                 goto exit_label;
               end;

             ']' :
               begin
                 readchar;
                 token:=_RECKKLAMMER;
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
                       token:=_LECKKLAMMER;
                       goto exit_label;
                     end;
                 end;
                 token:=_LKLAMMER;
                 goto exit_label;
               end;

             ')' :
               begin
                 readchar;
                 token:=_RKLAMMER;
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
                 token:=_PLUS;
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
                 token:=_MINUS;
                 goto exit_label;
               end;

             ':' :
               begin
                 readchar;
                 if c='=' then
                  begin
                    readchar;
                    token:=_ASSIGNMENT;
                    goto exit_label;
                  end;
                 token:=_COLON;
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
                     token:=_STARSTAR;
                   end
                 else
                  token:=_STAR;
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
                 end;
                 token:=_SLASH;
                 goto exit_label;
               end;

             '=' :
               begin
                 readchar;
                 token:=_EQUAL;
                 goto exit_label;
               end;

             '.' :
               begin
                 readchar;
                 case c of
                   '.' :
                     begin
                       readchar;
                       token:=_POINTPOINT;
                       goto exit_label;
                     end;
                   ')' :
                     begin
                       readchar;
                       token:=_RECKKLAMMER;
                       goto exit_label;
                     end;
                 end;
                 token:=_POINT;
                 goto exit_label;
               end;

             '@' :
               begin
                 readchar;
                 if c='@' then
                  begin
                    readchar;
                    token:=_DOUBLEADDR;
                  end
                 else
                  token:=_KLAMMERAFFE;
                 goto exit_label;
               end;

             ',' :
               begin
                 readchar;
                 token:=_COMMA;
                 goto exit_label;
               end;

             '''','#','^' :
               begin
                 if c='^' then
                  begin
                    readchar;
                    c:=upcase(c);
                    if (block_type=bt_type) or
                       (lasttoken=_ID) or
                       (lasttoken=_RKLAMMER) or (lasttoken=_RECKKLAMMER) or (lasttoken=_CARET) then
                     begin
                       token:=_CARET;
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
                              while (upcase(c) in ['A'..'F','0'..'9']) and (length(asciinr)<6) do
                               begin
                                 asciinr:=asciinr+c;
                                 readchar;
                               end;
                           end
                         else
                           begin
                              asciinr:='';
                              while (c in ['0'..'9']) and (length(asciinr)<6) do
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
                               end_of_file;
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
                         c:=upcase(c);
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
                  token:=_CCHAR
                 else
                  token:=_CSTRING;
                 goto exit_label;
               end;

             '>' :
               begin
                 readchar;
                 case c of
                   '=' :
                     begin
                       readchar;
                       token:=_GTE;
                       goto exit_label;
                     end;
                   '>' :
                     begin
                       readchar;
                       token:=_OP_SHR;
                       goto exit_label;
                     end;
                   '<' :
                     begin { >< is for a symetric diff for sets }
                       readchar;
                       token:=_SYMDIF;
                       goto exit_label;
                     end;
                 end;
                 token:=_GT;
                 goto exit_label;
               end;

             '<' :
               begin
                 readchar;
                 case c of
                   '>' :
                     begin
                       readchar;
                       token:=_UNEQUAL;
                       goto exit_label;
                     end;
                   '=' :
                     begin
                       readchar;
                       token:=_LTE;
                       goto exit_label;
                     end;
                   '<' :
                     begin
                       readchar;
                       token:=_OP_SHL;
                       goto exit_label;
                     end;
                 end;
                 token:=_LT;
                 goto exit_label;
               end;

             #26 :
               begin
                 token:=_EOF;
                 checkpreprocstack;
                 goto exit_label;
               end;
             else
               Illegal_Char(c);
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
                     readpreproc:=_ID;
                   end;
             '}' : begin
                     readpreproc:=_END;
                   end;
             '(' : begin
                     readchar;
                     readpreproc:=_LKLAMMER;
                   end;
             ')' : begin
                     readchar;
                     readpreproc:=_RKLAMMER;
                   end;
             '+' : begin
                     readchar;
                     readpreproc:=_PLUS;
                   end;
             '-' : begin
                     readchar;
                     readpreproc:=_MINUS;
                   end;
             '*' : begin
                     readchar;
                     readpreproc:=_STAR;
                   end;
             '/' : begin
                     readchar;
                     readpreproc:=_SLASH;
                   end;
             '=' : begin
                     readchar;
                     readpreproc:=_EQUAL;
                   end;
             '>' : begin
                     readchar;
                     if c='=' then
                      begin
                        readchar;
                        readpreproc:=_GTE;
                      end
                     else
                      readpreproc:=_GT;
                   end;
             '<' : begin
                     readchar;
                     case c of
                      '>' : begin
                              readchar;
                              readpreproc:=_UNEQUAL;
                            end;
                      '=' : begin
                              readchar;
                              readpreproc:=_LTE;
                            end;
                     else   readpreproc:=_LT;
                     end;
                   end;
             #26 :
               end_of_file;
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

end.
{
  $Log$
  Revision 1.2  2000-07-13 11:32:49  michael
  + removed logs

}
