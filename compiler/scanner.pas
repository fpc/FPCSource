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
unit scanner;

{$i defines.inc}

interface

    uses
       cclasses,
       globtype,globals,version,tokens,
       verbose,comphook,
       finput,
       widestr;

    const
       maxmacrolen=16*1024;
       preprocbufsize=32*1024;
       Newline = #10;


    type
       tcommentstyle = (comment_none,comment_tp,comment_oldtp,comment_delphi,comment_c);

       pmacrobuffer = ^tmacrobuffer;
       tmacrobuffer = array[0..maxmacrolen-1] of char;
       tscannerfile = class;

       tmacro = class(TNamedIndexItem)
          defined,
          defined_at_startup,
          is_used : boolean;
          buftext : pchar;
          buflen  : longint;
          constructor Create(const n : string);
          destructor  destroy;override;
       end;

       preproctyp = (pp_ifdef,pp_ifndef,pp_if,pp_ifopt,pp_else);

       tpreprocstack = class
          typ     : preproctyp;
          accept  : boolean;
          next    : tpreprocstack;
          name    : stringid;
          line_nb : longint;
          owner   : tscannerfile;
          constructor Create(atyp:preproctyp;a:boolean;n:tpreprocstack);
       end;

       tdirectiveproc=procedure;

       tdirectiveitem = class(TNamedIndexItem)
       public
          is_conditional : boolean;
          proc : tdirectiveproc;
          constructor Create(const n:string;p:tdirectiveproc);
          constructor CreateCond(const n:string;p:tdirectiveproc);
       end;

       tscannerfile = class
          inputfile    : tinputfile;  { current inputfile list }

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
          ignoredirectives : tstringlist; { ignore directives, used to give warnings only once }
          preprocstack   : tpreprocstack;
          invalid        : boolean; { flag if sourcefiles have been destroyed ! }
          macros         : Tdictionary;
          in_asm_string  : boolean;

          preproc_pattern : string;
          preproc_token   : ttoken;

          constructor Create(const fn:string);
          destructor Destroy;override;
        { File buffer things }
          function  openinputfile:boolean;
          procedure closeinputfile;
          function  tempopeninputfile:boolean;
          procedure tempcloseinputfile;
          procedure saveinputfile;
          procedure restoreinputfile;
          procedure firstfile;
          procedure nextfile;
          procedure addfile(hp:tinputfile);
          procedure reload;
          procedure insertmacro(const macname:string;p:pchar;len:longint);
        { Scanner things }
          procedure def_macro(const s : string);
          procedure set_macro(const s : string;value : string);
          procedure gettokenpos;
          procedure inc_comment_level;
          procedure dec_comment_level;
          procedure illegal_char(c:char);
          procedure end_of_file;
          procedure checkpreprocstack;
          procedure poppreprocstack;
          procedure addpreprocstack(atyp : preproctyp;a:boolean;const s:string;w:longint);
          procedure elsepreprocstack;
          procedure handleconditional(p:tdirectiveitem);
          procedure handledirectives;
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

{$ifdef PREPROCWRITE}
       tpreprocfile=class
         f   : text;
         buf : pointer;
         spacefound,
         eolfound : boolean;
         constructor create(const fn:string);
         destructor  destroy;
         procedure Add(const s:string);
         procedure AddSpace;
       end;
{$endif PREPROCWRITE}

    var
        { read strings }
        c              : char;
        orgpattern,
        pattern        : string;
        patternw       : pcompilerwidestring;

        { token }
        token,                        { current token being parsed }
        idtoken    : ttoken;          { holds the token if the pattern is a known word }

        current_scanner : tscannerfile;  { current scanner in use }

        scannerdirectives : tdictionary; { dictionary with the supported directives }

        aktcommentstyle : tcommentstyle; { needed to use read_comment from directives }
{$ifdef PREPROCWRITE}
        preprocfile     : tpreprocfile;  { used with only preprocessing }
{$endif PREPROCWRITE}

    procedure adddirective(const s:string;p:tdirectiveproc);
    procedure addconditional(const s:string;p:tdirectiveproc);

    procedure InitScanner;
    procedure DoneScanner;


implementation

    uses
{$ifdef delphi}
      dmisc,
{$else}
      dos,
{$endif delphi}
      cutils,
      systems,
      switches,
      fmodule;

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
        if not (length(s) in [tokenlenmin..tokenlenmax]) then
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
                           Conditional Directives
*****************************************************************************}

    procedure dir_else;
      begin
        current_scanner.elsepreprocstack;
      end;


    procedure dir_endif;
      begin
        current_scanner.poppreprocstack;
      end;


    procedure dir_ifdef;
      var
        hs    : string;
        mac   : tmacro;
      begin
        current_scanner.skipspace;
        hs:=current_scanner.readid;
        mac:=tmacro(current_scanner.macros.search(hs));
        if assigned(mac) then
          mac.is_used:=true;
        current_scanner.addpreprocstack(pp_ifdef,assigned(mac) and mac.defined,hs,scan_c_ifdef_found);
      end;


    procedure dir_ifndef;
      var
        hs    : string;
        mac   : tmacro;
      begin
        current_scanner.skipspace;
        hs:=current_scanner.readid;
        mac:=tmacro(current_scanner.macros.search(hs));
        if assigned(mac) then
          mac.is_used:=true;
        current_scanner.addpreprocstack(pp_ifndef,not(assigned(mac) and mac.defined),hs,scan_c_ifndef_found);
      end;


    procedure dir_ifopt;
      var
        hs    : string;
        found : boolean;
        state : char;
      begin
        current_scanner.skipspace;
        hs:=current_scanner.readid;
        if (length(hs)>1) then
         Message1(scan_w_illegal_switch,hs)
        else
         begin
           state:=current_scanner.ReadState;
           if state in ['-','+'] then
            found:=CheckSwitch(hs[1],state);
         end;
        current_scanner.addpreprocstack(pp_ifopt,found,hs,scan_c_ifopt_found);
      end;


    procedure dir_if;

        function read_expr : string; forward;

        procedure preproc_consume(t : ttoken);
        begin
          if t<>current_scanner.preproc_token then
           Message(scan_e_preproc_syntax_error);
          current_scanner.preproc_token:=current_scanner.readpreproc;
        end;

        function readpreproc: string;
        var
          hs: string;
          mac : tmacro;
          len : integer;
        begin
          hs := current_scanner.preproc_pattern;
          mac:=tmacro(current_scanner.macros.search(hs));
          if assigned(mac) then
          begin
            if mac.defined and assigned(mac.buftext) then
            begin
              if mac.buflen>255 then
              begin
                len:=255;
                Message(scan_w_macro_cut_after_255_chars);
              end
              else
                len:=mac.buflen;
              hs[0]:=char(len);
              move(mac.buftext^,hs[1],len);
            end;
          end;
          readpreproc := hs;
        end;

        function read_factor : string;
        var
           hs : string;
           mac: tmacro;
        begin
           if current_scanner.preproc_token=_ID then
             begin
                if readpreproc='DEFINED' then
                begin
                  preproc_consume(_ID);
                  current_scanner.skipspace;
                  if current_scanner.preproc_token =_LKLAMMER then
                  begin
                    preproc_consume(_LKLAMMER);
                    current_scanner.skipspace;
                  end
                  else
                    Message(scan_e_error_in_preproc_expr);
                  if current_scanner.preproc_token =_ID then
                  begin
                    hs := current_scanner.preproc_pattern;
                    mac := tmacro(current_scanner.macros.search(hs));
                    if assigned(mac) then
                      hs := '1'
                    else
                      hs := '0';
                    read_factor := hs;
                    preproc_consume(_ID);
                    current_scanner.skipspace;
                  end
                  else
                    Message(scan_e_error_in_preproc_expr);
                  if current_scanner.preproc_token =_RKLAMMER then
                    preproc_consume(_RKLAMMER)
                  else
                    Message(scan_e_error_in_preproc_expr);
                end
                else
                if readpreproc='NOT' then
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
                     hs:=readpreproc;
                     preproc_consume(_ID);
                     read_factor:=hs;
                  end
             end
           else if current_scanner.preproc_token =_LKLAMMER then
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
           l1,l2 : longint;
           w : integer;
        begin
           hs1:=read_factor;
           while true do
             begin
                if (current_scanner.preproc_token=_ID) then
                  begin
                     if readpreproc='AND' then
                       begin
                          preproc_consume(_ID);
                          hs2:=read_expr;
                          valint(hs1,l1,w); valint(hs2,l2,w);
                          if (l1>0) and (l2>0) then
                            hs1:='1'
                          else
                            hs1:='0';
                          read_term := hs1;
                          exit;
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
           l1,l2 : longint;
           w : integer;
        begin
           hs1:=read_term;
           while true do
             begin
                if (current_scanner.preproc_token=_ID) then
                  begin
                     if readpreproc='OR' then
                       begin
                          preproc_consume(_ID);
                          hs2:=read_expr;
                          valint(hs1,l1,w); valint(hs2,l2,w);
                          if (l1>0) or (l2>0) then
                            hs1:='1'
                          else
                            hs1:='0';
                          read_simple_expr := hs1;
                          exit;
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
           t:=current_scanner.preproc_token;
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

      var
        hs : string;
      begin
        current_scanner.skipspace;
        { start preproc expression scanner }
        current_scanner.preproc_token:=current_scanner.readpreproc;
        hs:=read_expr;
        current_scanner.addpreprocstack(pp_if,hs<>'0',hs,scan_c_if_found);
      end;


    procedure dir_define;
      var
        hs  : string;
        bracketcount : longint;
        mac : tmacro;
        macropos : longint;
        macrobuffer : pmacrobuffer;
      begin
        current_scanner.skipspace;
        hs:=current_scanner.readid;
        mac:=tmacro(current_scanner.macros.search(hs));
        if not assigned(mac) then
          begin
            mac:=tmacro.create(hs);
            mac.defined:=true;
            Message1(parser_m_macro_defined,mac.name);
            current_scanner.macros.insert(mac);
          end
        else
          begin
            Message1(parser_m_macro_defined,mac.name);
            mac.defined:=true;
          { delete old definition }
            if assigned(mac.buftext) then
             begin
               freemem(mac.buftext,mac.buflen);
               mac.buftext:=nil;
             end;
          end;
        mac.is_used:=true;
        if (cs_support_macro in aktmoduleswitches) then
          begin
          { key words are never substituted }
             if is_keyword(hs) then
              Message(scan_e_keyword_cant_be_a_macro);
           { !!!!!! handle macro params, need we this? }
             current_scanner.skipspace;
           { may be a macro? }
             if c=':' then
               begin
                  current_scanner.readchar;
                  if c='=' then
                    begin
                       new(macrobuffer);
                       macropos:=0;
                       { parse macro, brackets are counted so it's possible
                         to have a $ifdef etc. in the macro }
                       bracketcount:=0;
                       repeat
                         current_scanner.readchar;
                         case c of
                           '}' :
                             if (bracketcount=0) then
                              break
                             else
                              dec(bracketcount);
                           '{' :
                             inc(bracketcount);
                           #26 :
                             current_scanner.end_of_file;
                         end;
                         macrobuffer^[macropos]:=c;
                         inc(macropos);
                         if macropos>maxmacrolen then
                          Message(scan_f_macro_buffer_overflow);
                       until false;
                       { free buffer of macro ?}
                       if assigned(mac.buftext) then
                         freemem(mac.buftext,mac.buflen);
                       { get new mem }
                       getmem(mac.buftext,macropos);
                       mac.buflen:=macropos;
                       { copy the text }
                       move(macrobuffer^,mac.buftext^,macropos);
                       dispose(macrobuffer);
                    end;
               end;
          end
        else
          begin
           { check if there is an assignment, then we need to give a
             warning }
             current_scanner.skipspace;
             if c=':' then
              begin
                current_scanner.readchar;
                if c='=' then
                  Message(scan_w_macro_support_turned_off);
              end;
          end;
      end;


    procedure dir_undef;
      var
        hs  : string;
        mac : tmacro;
      begin
        current_scanner.skipspace;
        hs:=current_scanner.readid;
        mac:=tmacro(current_scanner.macros.search(hs));
        if not assigned(mac) then
          begin
             mac:=tmacro.create(hs);
             Message1(parser_m_macro_undefined,mac.name);
             mac.defined:=false;
             current_scanner.macros.insert(mac);
          end
        else
          begin
             Message1(parser_m_macro_undefined,mac.name);
             mac.defined:=false;
             { delete old definition }
             if assigned(mac.buftext) then
               begin
                  freemem(mac.buftext,mac.buflen);
                  mac.buftext:=nil;
               end;
          end;
        mac.is_used:=true;
      end;

    procedure dir_include;

        function findincludefile(const path,name,ext:string;var foundfile:string):boolean;
        var
          found : boolean;
          hpath : string;
        begin
         { look for the include file
            1. specified path,path of current inputfile,current dir
            2. local includepath
            3. global includepath }
           found:=false;
           foundfile:='';
           hpath:='';
           if path<>'' then
             begin
               if not path_absolute(path) then
                 hpath:=current_scanner.inputfile.path^+path
               else
                 hpath:=path+';'+current_scanner.inputfile.path^;
             end
           else
             hpath:=current_scanner.inputfile.path^;
           found:=FindFile(name+ext,hpath+';.'+source_info.DirSep,foundfile);
           if (not found) then
            found:=current_module.localincludesearchpath.FindFile(name+ext,foundfile);
           if (not found) then
            found:=includesearchpath.FindFile(name+ext,foundfile);
           findincludefile:=found;
        end;


      var
        foundfile,
        hs    : string;
        path  : dirstr;
        name  : namestr;
        ext   : extstr;
        hp    : tinputfile;
        i     : longint;
        found : boolean;
      begin
        current_scanner.skipspace;
        hs:=current_scanner.readcomment;
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
             hs:=current_module.sourcefiles.get_file_name(aktfilepos.fileindex)
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
            if hs='FPCTARGETCPU' then
             hs:=target_cpu_string
           else
            if hs='FPCTARGETOS' then
             hs:=target_info.shortname
           else
             hs:=getenv(hs);
           if hs='' then
            Message1(scan_w_include_env_not_found,path);
           { make it a stringconst }
           hs:=''''+hs+'''';
           current_scanner.insertmacro(path,@hs[1],length(hs));
         end
        else
         begin
           hs:=FixFileName(hs);
           fsplit(hs,path,name,ext);
           { try to find the file }
           found:=findincludefile(path,name,ext,foundfile);
           if (ext='') then
            begin
              { try default extensions .inc , .pp and .pas }
              if (not found) then
               found:=findincludefile(path,name,'.inc',foundfile);
              if (not found) then
               found:=findincludefile(path,name,target_info.sourceext,foundfile);
              if (not found) then
               found:=findincludefile(path,name,target_info.pasext,foundfile);
            end;
         { save old postion and decrease linebreak }
           if c=newline then
            dec(current_scanner.line_no);
           dec(longint(current_scanner.inputpointer));
         { shutdown current file }
           current_scanner.tempcloseinputfile;
         { load new file }
           hp:=do_openinputfile(foundfile);
           current_scanner.addfile(hp);
           current_module.sourcefiles.register_file(hp);
           if not current_scanner.openinputfile then
            Message1(scan_f_cannot_open_includefile,hs);
           Message1(scan_t_start_include_file,current_scanner.inputfile.path^+current_scanner.inputfile.name^);
           current_scanner.reload;
         { process first read char }
           case c of
            #26 : current_scanner.reload;
            #10,
            #13 : current_scanner.linebreak;
           end;
         end;
      end;



{*****************************************************************************
                                 TMacro
*****************************************************************************}

    constructor tmacro.create(const n : string);
      begin
         inherited createname(n);
         defined:=true;
         defined_at_startup:=false;
         is_used:=false;
         buftext:=nil;
         buflen:=0;
      end;


    destructor tmacro.destroy;
      begin
         if assigned(buftext) then
           freemem(buftext,buflen);
         inherited destroy;
      end;


{*****************************************************************************
                            Preprocessor writting
*****************************************************************************}

{$ifdef PREPROCWRITE}
    constructor tpreprocfile.create(const fn:string);
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


    destructor tpreprocfile.destroy;
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
{$endif PREPROCWRITE}


{*****************************************************************************
                              TPreProcStack
*****************************************************************************}

    constructor tpreprocstack.create(atyp : preproctyp;a:boolean;n:tpreprocstack);
      begin
        accept:=a;
        typ:=atyp;
        next:=n;
      end;


{*****************************************************************************
                              TDirectiveItem
*****************************************************************************}

    constructor TDirectiveItem.Create(const n:string;p:tdirectiveproc);
      begin
        inherited CreateName(n);
        is_conditional:=false;
        proc:={$ifndef FPCPROCVAR}@{$endif}p;
      end;


    constructor TDirectiveItem.CreateCond(const n:string;p:tdirectiveproc);
      begin
        inherited CreateName(n);
        is_conditional:=true;
        proc:={$ifndef FPCPROCVAR}@{$endif}p;
      end;

{****************************************************************************
                                TSCANNERFILE
 ****************************************************************************}

    constructor tscannerfile.create(const fn:string);
      begin
        inputfile:=do_openinputfile(fn);
        if assigned(current_module) then
          current_module.sourcefiles.register_file(inputfile);
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
        ignoredirectives:=TStringList.Create;
        invalid:=false;
        in_asm_string:=false;
        macros:=tdictionary.create;
      end;


    procedure tscannerfile.firstfile;
      begin
      { load block }
        if not openinputfile then
          Message1(scan_f_cannot_open_input,inputfile.name^);
        reload;
      { process first read char }
        case c of
         #26 : reload;
         #10,
         #13 : linebreak;
        end;
      end;


    destructor tscannerfile.destroy;
      begin
        if not invalid then
          begin
             if status.errorcount=0 then
              checkpreprocstack
             else
              begin
                while assigned(preprocstack) do
                 poppreprocstack;
              end;
           { close file, but only if we are the first compile }
           { probably not necessary anymore with invalid flag PM }
             if not current_module.in_second_compile then
              begin
                if not inputfile.closed then
                 closeinputfile;
              end;
          end;
         ignoredirectives.free;
         macros.free;
       end;


    procedure tscannerfile.def_macro(const s : string);
      var
        mac : tmacro;
      begin
         mac:=tmacro(macros.search(s));
         if mac=nil then
           begin
             mac:=tmacro.create(s);
             Message1(parser_m_macro_defined,mac.name);
             macros.insert(mac);
           end;
         mac.defined:=true;
         mac.defined_at_startup:=true;
      end;


    procedure tscannerfile.set_macro(const s : string;value : string);
      var
        mac : tmacro;
      begin
         mac:=tmacro(macros.search(s));
         if mac=nil then
           begin
             mac:=tmacro.create(s);
             macros.insert(mac);
           end
         else
           begin
              if assigned(mac.buftext) then
                freemem(mac.buftext,mac.buflen);
           end;
         Message2(parser_m_macro_set_to,mac.name,value);
         mac.buflen:=length(value);
         getmem(mac.buftext,mac.buflen);
         move(value[1],mac.buftext^,mac.buflen);
         mac.defined:=true;
         mac.defined_at_startup:=true;
      end;


    function tscannerfile.openinputfile:boolean;
      begin
        openinputfile:=inputfile.open;
      { load buffer }
        inputbuffer:=inputfile.buf;
        inputpointer:=inputfile.buf;
        inputstart:=inputfile.bufstart;
      { line }
        line_no:=0;
        lastlinepos:=0;
        lasttokenpos:=0;
      end;


    procedure tscannerfile.closeinputfile;
      begin
        inputfile.close;
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
        tempopeninputfile:=inputfile.tempopen;
      { reload buffer }
        inputbuffer:=inputfile.buf;
        inputpointer:=inputfile.buf;
        inputstart:=inputfile.bufstart;
      end;


    procedure tscannerfile.tempcloseinputfile;
      begin
        inputfile.setpos(inputstart+(inputpointer-inputbuffer));
        inputfile.tempclose;
      { reset buffer }
        inputbuffer:=nil;
        inputpointer:=nil;
        inputstart:=0;
      end;


    procedure tscannerfile.saveinputfile;
      begin
        inputfile.saveinputpointer:=inputpointer;
        inputfile.savelastlinepos:=lastlinepos;
        inputfile.saveline_no:=line_no;
      end;


    procedure tscannerfile.restoreinputfile;
      begin
        inputpointer:=inputfile.saveinputpointer;
        lastlinepos:=inputfile.savelastlinepos;
        line_no:=inputfile.saveline_no;
        if not inputfile.is_macro then
          parser_current_file:=inputfile.name^;
      end;


    procedure tscannerfile.nextfile;
      var
        to_dispose : tinputfile;
      begin
        if assigned(inputfile.next) then
         begin
           if inputfile.is_macro then
             to_dispose:=inputfile
           else
             to_dispose:=nil;
           { we can allways close the file, no ? }
           inputfile.close;
           inputfile:=inputfile.next;
           if assigned(to_dispose) then
             to_dispose.free;
           restoreinputfile;
         end;
      end;


    procedure tscannerfile.addfile(hp:tinputfile);
      begin
        saveinputfile;
      { add to list }
        hp.next:=inputfile;
        inputfile:=hp;
      { load new inputfile }
        restoreinputfile;
      end;


    procedure tscannerfile.reload;
      begin
        with inputfile do
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
                not(inputfile.is_macro) and
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
                     inputfile.setline(line_no,bufstart);
                 end;
              end
             else
              begin
              { load eof position in tokenpos/aktfilepos }
                gettokenpos;
              { close file }
                closeinputfile;
              { no next module, than EOF }
                if not assigned(inputfile.next) then
                 begin
                   c:=#26;
                   exit;
                 end;
              { load next file and reopen it }
                nextfile;
                tempopeninputfile;
              { status }
                Message1(scan_t_back_in,inputfile.name^);
              end;
           { load next char }
             c:=inputpointer^;
             inc(longint(inputpointer));
           until c<>#0; { if also end, then reload again }
         end;
      end;


    procedure tscannerfile.insertmacro(const macname:string;p:pchar;len:longint);
      var
        hp : tinputfile;
      begin
      { save old postion and decrease linebreak }
        if c=newline then
         dec(line_no);
        dec(longint(inputpointer));
        tempcloseinputfile;
      { create macro 'file' }
        { use special name to dispose after !! }
        hp:=do_openinputfile('_Macro_.'+macname);
        addfile(hp);
        with inputfile do
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
        akttokenpos.line:=line_no;
        akttokenpos.column:=lasttokenpos-lastlinepos;
        akttokenpos.fileindex:=inputfile.ref_index;
        aktfilepos:=akttokenpos;
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
        with inputfile do
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
             inputfile.setline(line_no,lastlinepos);
         { update for status and call the show status routine,
           but don't touch aktfilepos ! }
           oldaktfilepos:=aktfilepos;
           oldtokenpos:=akttokenpos;
           gettokenpos; { update for v_status }
           inc(status.compiledlines);
           ShowStatus;
           aktfilepos:=oldaktfilepos;
           akttokenpos:=oldtokenpos;
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

  {-------------------------------------------
           IF Conditional Handling
  -------------------------------------------}

    procedure tscannerfile.checkpreprocstack;
      begin
      { check for missing ifdefs }
        while assigned(preprocstack) do
         begin
           Message4(scan_e_endif_expected,preprocstring[preprocstack.typ],preprocstack.name,
             preprocstack.owner.inputfile.name^,tostr(preprocstack.line_nb));
           poppreprocstack;
         end;
      end;


    procedure tscannerfile.poppreprocstack;
      var
        hp : tpreprocstack;
      begin
        if assigned(preprocstack) then
         begin
           Message1(scan_c_endif_found,preprocstack.name);
           hp:=preprocstack.next;
           preprocstack.free;
           preprocstack:=hp;
         end
        else
         Message(scan_e_endif_without_if);
      end;


    procedure tscannerfile.addpreprocstack(atyp : preproctyp;a:boolean;const s:string;w:longint);
      begin
        preprocstack:=tpreprocstack.create(atyp,((preprocstack=nil) or preprocstack.accept) and a,preprocstack);
        preprocstack.name:=s;
        preprocstack.line_nb:=line_no;
        preprocstack.owner:=self;
        if preprocstack.accept then
         Message2(w,preprocstack.name,'accepted')
        else
         Message2(w,preprocstack.name,'rejected');
      end;


    procedure tscannerfile.elsepreprocstack;
      begin
        if assigned(preprocstack) then
         begin
           preprocstack.typ:=pp_else;
           preprocstack.line_nb:=line_no;
           if not(assigned(preprocstack.next)) or (preprocstack.next.accept) then
            preprocstack.accept:=not preprocstack.accept;
           if preprocstack.accept then
            Message2(scan_c_else_found,preprocstack.name,'accepted')
           else
            Message2(scan_c_else_found,preprocstack.name,'rejected');
         end
        else
         Message(scan_e_endif_without_if);
      end;


    procedure tscannerfile.handleconditional(p:tdirectiveitem);
      var
        oldaktfilepos : tfileposinfo;
      begin
        oldaktfilepos:=aktfilepos;
        repeat
          current_scanner.gettokenpos;
          p.proc{$ifdef FPCPROCVAR}(){$endif};
          { accept the text ? }
          if (current_scanner.preprocstack=nil) or current_scanner.preprocstack.accept then
           break
          else
           begin
             current_scanner.gettokenpos;
             Message(scan_c_skipping_until);
             repeat
               current_scanner.skipuntildirective;
               p:=tdirectiveitem(scannerdirectives.search(current_scanner.readid));
             until assigned(p) and (p.is_conditional);
             current_scanner.gettokenpos;
             Message1(scan_d_handling_switch,'$'+p.name);
           end;
        until false;
        aktfilepos:=oldaktfilepos;
      end;


    procedure tscannerfile.handledirectives;
      var
         t  : tdirectiveitem;
         hs : string;
      begin
         gettokenpos;
         readchar; {Remove the $}
         hs:=readid;
{$ifdef PREPROCWRITE}
         if parapreprocess then
          begin
            t:=Get_Directive(hs);
            if not(is_conditional(t) or (t=_DIR_DEFINE) or (t=_DIR_UNDEF)) then
             begin
               preprocfile^.AddSpace;
               preprocfile^.Add('{$'+hs+current_scanner.readcomment+'}');
               exit;
             end;
          end;
{$endif PREPROCWRITE}
         { skip this directive? }
         if (ignoredirectives.find(hs)<>nil) then
          begin
            if (comment_level>0) then
             readcomment;
            { we've read the whole comment }
            aktcommentstyle:=comment_none;
            exit;
          end;
         if hs='' then
          begin
            Message1(scan_w_illegal_switch,'$'+hs);
          end;
      { Check for compiler switches }
         while (length(hs)=1) and (c in ['-','+']) do
          begin
            HandleSwitch(hs[1],c);
            current_scanner.readchar; {Remove + or -}
            if c=',' then
             begin
               current_scanner.readchar;   {Remove , }
             { read next switch, support $v+,$+}
               hs:=current_scanner.readid;
               if (hs='') then
                begin
                  if (c='$') and (m_fpc in aktmodeswitches) then
                   begin
                     current_scanner.readchar;  { skip $ }
                     hs:=current_scanner.readid;
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
            t:=tdirectiveitem(scannerdirectives.search(hs));
            if assigned(t) then
             begin
               if t.is_conditional then
                handleconditional(t)
               else
                begin
                  Message1(scan_d_handling_switch,'$'+hs);
                  t.proc{$ifdef FPCPROCVAR}(){$endif};
                end;
             end
            else
             begin
               current_scanner.ignoredirectives.insert(hs);
               Message1(scan_w_illegal_directive,'$'+hs);
             end;
          { conditionals already read the comment }
            if (current_scanner.comment_level>0) then
             current_scanner.readcomment;
            { we've read the whole comment }
            aktcommentstyle:=comment_none;
          end;
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
        case c of
         #26 : reload;
         #10,
         #13 : linebreak;
        end;
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
        readcomment[0]:=chr(i);
      end;


    function tscannerfile.readstate:char;
      var
        state : char;
      begin
        state:=' ';
        if c=' ' then
         begin
           current_scanner.skipspace;
           current_scanner.readid;
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
        incomment : boolean;
        found : longint;
        next_char_loaded : boolean;
        oldcommentstyle : tcommentstyle;
      begin
         found:=0;
         next_char_loaded:=false;
         incomment:=true;
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
                 incomment:=true;
               end;
             '}' :
               begin
                 dec_comment_level;
                 found:=0;
                 incomment:=false;
               end;
             '$' :
               begin
                 if found=1 then
                  found:=2;
               end;
             '''' :
               if not incomment then
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
                           begin
                             next_char_loaded:=true;
                             break;
                           end;
                        end;
                    end;
                  until false;
                end;
             '(' :
               begin
                 if not incomment then
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
                  end
                 else
                  found:=0;
               end;
             '/' :
               begin
                 if not incomment then
                  begin
                    readchar;
                    if c='/' then
                     begin
                       readchar;
                       skipdelphicomment;
                       aktcommentstyle:=oldcommentstyle;
                     end
                    else
                     next_char_loaded:=true;
                  end
                 else
                  found:=0;
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
        { this is not supported }
        if c='$' then
          Message(scan_e_wrong_styled_switch);
        { skip comment }
        while not (c in [newline,#26]) do
         readchar;
        dec_comment_level;
        aktcommentstyle:=comment_none;
      end;


    procedure tscannerfile.skipoldtpcomment;
      var
        found : longint;
      begin
        aktcommentstyle:=comment_oldtp;
        inc_comment_level;
        { only load a char if last already processed,
          was cause of bug1634 PM }
        if c=#0 then
          readchar;
      { this is now supported }
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
        len,
        low,high,mid : longint;
        m       : longint;
        mac     : tmacro;
        asciinr : string[6];
        msgwritten,
        iswidestring : boolean;
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
{$ifdef PREPROCWRITE}
                if parapreprocess then
                 begin
                   if c=#10 then
                    preprocfile.eolfound:=true
                   else
                    preprocfile.spacefound:=true;
                 end;
{$endif PREPROCWRITE}
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
           if (pattern[1]<>'_') and (length(pattern) in [tokenlenmin..tokenlenmax]) then
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
                 mac:=tmacro(macros.search(pattern));
                 if assigned(mac) and (assigned(mac.buftext)) then
                  begin
                    insertmacro(pattern,mac.buftext,mac.buflen);
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
                 if not(m_fpc in aktmodeswitches) then
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
                       c:=#0;{Signal skipoldtpcomment to reload a char }
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
                 len:=0;
                 msgwritten:=false;
                 pattern:='';
                 iswidestring:=false;
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
                       inc(len);
                       if c<#64 then
                        pattern[len]:=chr(ord(c)+64)
                       else
                        pattern[len]:=chr(ord(c)-64);
                       readchar;
                     end;
                  end;
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
                         if (asciinr='') or (code<>0) then
                           Message(scan_e_illegal_char_const)
                         else if (m<0) or (m>255) then
                           begin
                              if (m>=0) and (m<=65535) then
                                begin
                                  if not iswidestring then
                                   begin
                                     ascii2unicode(@pattern[1],len,patternw);
                                     iswidestring:=true;
                                     len:=0;
                                   end;
                                  concatwidestringchar(patternw,tcompilerwidechar(m));
                                end
                              else
                                Message(scan_e_illegal_char_const)
                           end
                         else if iswidestring then
                           concatwidestringchar(patternw,asciichar2unicode(char(m)))
                         else
                           begin
                             if len<255 then
                              begin
                                inc(len);
                                pattern[len]:=chr(m);
                              end
                             else
                              begin
                                if not msgwritten then
                                 begin
                                   Message(scan_e_string_exceeds_255_chars);
                                   msgwritten:=true;
                                 end;
                              end;
                           end;
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
                           if iswidestring then
                             concatwidestringchar(patternw,asciichar2unicode(c))
                           else
                             begin
                               if len<255 then
                                begin
                                  inc(len);
                                  pattern[len]:=c;
                                end
                               else
                                begin
                                  if not msgwritten then
                                   begin
                                     Message(scan_e_string_exceeds_255_chars);
                                     msgwritten:=true;
                                   end;
                                end;
                             end;
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

                         if iswidestring then
                           concatwidestringchar(patternw,asciichar2unicode(c))
                         else
                           begin
                             if len<255 then
                              begin
                                inc(len);
                                pattern[len]:=c;
                              end
                             else
                              begin
                                if not msgwritten then
                                 begin
                                   Message(scan_e_string_exceeds_255_chars);
                                   msgwritten:=true;
                                 end;
                              end;
                           end;

                         readchar;
                       end;
                     else
                      break;
                   end;
                 until false;
                 { strings with length 1 become const chars }
                 if iswidestring then
                   begin
                      if patternw^.len=1 then
                       token:=_CWCHAR
                      else
                       token:=_CWSTRING;
                   end
                 else
                   begin
                      pattern[0]:=chr(len);
                      if len=1 then
                       token:=_CCHAR
                      else
                       token:=_CSTRING;
                   end;
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
                     current_scanner.preproc_pattern:=readid;
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
         if in_asm_string then
           begin
             asmgetchar:=c;
             exit;
           end;
         repeat
           case c of
             '{' :
               skipcomment;
             '/' :
               begin
                  readchar;
                  if c='/' then
                   skipdelphicomment
                  else
                   begin
                     asmgetchar:='/';
                     lastasmgetchar:=c;
                     exit;
                   end;
               end;
             '(' :
               begin
                  readchar;
                  if c='*' then
                   begin
                     c:=#0;{Signal skipoldtpcomment to reload a char }
                     skipoldtpcomment;
                   end
                  else
                   begin
                     asmgetchar:='(';
                     lastasmgetchar:=c;
                     exit;
                   end;
               end;
             else
               begin
                 asmgetchar:=c;
                 exit;
               end;
           end;
         until false;
      end;


{*****************************************************************************
                                   Helpers
*****************************************************************************}

    procedure adddirective(const s:string;p:tdirectiveproc);
      begin
        scannerdirectives.insert(tdirectiveitem.create(s,p));
      end;


    procedure addconditional(const s:string;p:tdirectiveproc);
      begin
        scannerdirectives.insert(tdirectiveitem.createcond(s,p));
      end;


{*****************************************************************************
                                Initialization
*****************************************************************************}

    procedure InitScanner;
      begin
        InitWideString(patternw);
        scannerdirectives:=TDictionary.Create;
        { Default directives }
        AddDirective('DEFINE',{$ifdef FPCPROCVAR}@{$endif}dir_define);
        AddDirective('UNDEF',{$ifdef FPCPROCVAR}@{$endif}dir_undef);
        AddDirective('I',{$ifdef FPCPROCVAR}@{$endif}dir_include);
        AddDirective('INCLUDE',{$ifdef FPCPROCVAR}@{$endif}dir_include);
        { Default conditionals }
        AddConditional('ELSE',{$ifdef FPCPROCVAR}@{$endif}dir_else);
        AddConditional('ENDIF',{$ifdef FPCPROCVAR}@{$endif}dir_endif);
        AddConditional('IF',{$ifdef FPCPROCVAR}@{$endif}dir_if);
        AddConditional('IFDEF',{$ifdef FPCPROCVAR}@{$endif}dir_ifdef);
        AddConditional('IFNDEF',{$ifdef FPCPROCVAR}@{$endif}dir_ifndef);
        AddConditional('IFOPT',{$ifdef FPCPROCVAR}@{$endif}dir_ifopt);
      end;


    procedure DoneScanner;
      begin
        scannerdirectives.Free;
        DoneWideString(patternw);
      end;


end.
{
  $Log$
  Revision 1.36  2002-04-21 18:57:23  peter
    * fixed memleaks when file can't be opened

  Revision 1.35  2002/04/21 15:22:26  carl
  * first check .inc file extension

  Revision 1.34  2002/04/21 07:24:09  carl
  - remove my fixes until Peter agrees on the fix (sorry Peter)

  Revision 1.32  2002/04/19 15:42:11  peter
    * default extension checking for include files

  Revision 1.31  2002/03/01 14:39:44  peter
    * fixed // and (* parsing to not be done when already parsing a
      tp comment in skipuntildirective

  Revision 1.30  2002/03/01 12:39:26  peter
    * support // parsing in skipuntildirective

  Revision 1.29  2002/01/27 21:44:26  peter
    * FPCTARGETOS/FPCTARGETCPU added as internal environment variable

  Revision 1.28  2002/01/24 18:25:50  peter
   * implicit result variable generation for assembler routines
   * removed m_tp modeswitch, use m_tp7 or not(m_fpc) instead

  Revision 1.27  2001/10/22 20:25:49  peter
    * fixed previous commit

  Revision 1.26  2001/10/22 19:55:44  peter
    * give error with string constants longer than 255 chars, this is
      compatible with kylix

  Revision 1.25  2001/10/17 22:41:05  florian
    * several widechar fixes, case works now

  Revision 1.24  2001/10/12 16:02:34  peter
    * fix bug 1634 (merged)

  Revision 1.23  2001/09/30 21:23:59  peter
    * merged delphi comment fix

  Revision 1.22  2001/09/18 11:30:48  michael
  * Fixes win32 linking problems with import libraries
  * LINKLIB Libraries are now looked for using C file extensions
  * get_exepath fix

  Revision 1.21  2001/07/30 20:59:27  peter
    * m68k updates from v10 merged

  Revision 1.20  2001/07/15 11:56:21  peter
    * merged fixed relative path fix

  Revision 1.19  2001/07/08 21:00:16  peter
    * various widestring updates, it works now mostly without charset
      mapping supported

  Revision 1.18  2001/06/03 21:57:38  peter
    + hint directive parsing support

  Revision 1.17  2001/05/27 14:30:55  florian
    + some widestring stuff added

  Revision 1.16  2001/04/13 22:12:34  peter
    * fixed comment after comment parsing in assembler blocks

  Revision 1.15  2001/04/13 18:00:36  peter
    * easier registration of directives

  Revision 1.14  2001/04/13 01:22:13  peter
    * symtable change to classes
    * range check generation and errors fixed, make cycle DEBUG=1 works
    * memory leaks fixed

  Revision 1.13  2000/12/25 00:07:28  peter
    + new tlinkedlist class (merge of old tstringqueue,tcontainer and
      tlinkedlist objects)

  Revision 1.12  2000/12/24 12:24:38  peter
    * moved preprocessfile into a conditional

  Revision 1.11  2000/12/18 17:59:01  peter
    * fixed skipuntildirective

  Revision 1.10  2000/12/16 15:36:02  peter
    * fixed parsing of strings and comments in skipuntildirective

  Revision 1.9  2000/11/30 20:27:51  peter
    * merged fix for bug 1229

  Revision 1.8  2000/11/29 00:30:40  florian
    * unused units removed from uses clause
    * some changes for widestrings

  Revision 1.7  2000/10/31 22:02:51  peter
    * symtable splitted, no real code changes

  Revision 1.6  2000/09/24 15:06:28  peter
    * use defines.inc

  Revision 1.5  2000/08/27 16:11:53  peter
    * moved some util functions from globals,cobjects to cutils
    * splitted files into finput,fmodule

  Revision 1.4  2000/08/12 15:30:44  peter
    * IDE patch for stream reading (merged)

  Revision 1.3  2000/08/08 19:28:57  peter
    * memdebug/memory patches (merged)
    * only once illegal directive (merged)

  Revision 1.2  2000/07/13 11:32:49  michael
  + removed logs

}
