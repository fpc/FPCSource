{
    Copyright (c) 1998-2002 by Florian Klaempfl

    Contains some helper routines for the parser

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
unit pbase;

{$i fpcdefs.inc}

interface

    uses
       cutils,cclasses,
       tokens,globtype,compilerbase,
       symconst,symbase,symtype,symdef,symsym,symtable
       ;

    const
      { tokens that end a block or statement. And don't require
        a ; on the statement before }
      endtokens = [_SEMICOLON,_END,_ELSE,_UNTIL,_EXCEPT,_FINALLY];

    type
      tconsume_unitsym_flag = (
        cuf_consume_id,
        cuf_allow_specialize,
        cuf_check_attr_suffix
      );
      tconsume_unitsym_flags = set of tconsume_unitsym_flag;

      TParserBaseHelpers = class
      private
        FCompiler: TCompilerBase;
      public
        { special for function reference vars }
        getfuncrefdef : tobjectdef;

        { for operators }
        optoken : ttoken;

        { true, if only routine headers should be parsed }
        parse_only : boolean;

        { true, if we found a name for a named arg }
        found_arg_name : boolean;

        { true, if we are parsing generic declaration }
        parse_generic : boolean;

        { just for an accurate position of the end of a procedure (PM) }
        last_endtoken_filepos: tfileposinfo;

        constructor Create(ACompiler: TCompilerBase);

        procedure identifier_not_found(const s:string);
        procedure identifier_not_found(const s:string;const filepos:tfileposinfo);

    {    function tokenstring(i : ttoken):string;}

        { consumes token i, if the current token is unequal i }
        { a syntax error is written                           }
        procedure consume(i : ttoken);

        { Same as consume, but will not attempt to read next token if the token is a point }

        procedure consume_last_dot;

        {Tries to consume the token i, and returns true if it was consumed:
         if token=i.}
        function try_to_consume(i:Ttoken):boolean;

        { consumes all tokens til atoken (for error recovering }
        procedure consume_all_until(atoken : ttoken);

        { consumes tokens while they are semicolons }
        procedure consume_emptystats;

        { reads a list of identifiers into a string list }
        { consume a symbol, if not found give an error and
          and return an errorsym }
        function consume_sym(var srsym:tsym;var srsymtable:TSymtable):boolean;
        function consume_sym_orgid(var srsym:tsym;var srsymtable:TSymtable;var s : string):boolean;

        function try_consume_unitsym(var srsym:tsym;var srsymtable:TSymtable;var tokentoconsume:ttoken;flags:tconsume_unitsym_flags;out is_specialize:boolean;sympattern:TSymStr):boolean;
        function try_consume_unitsym_no_specialize(var srsym:tsym;var srsymtable:TSymtable;var tokentoconsume:ttoken;flags:tconsume_unitsym_flags;sympattern:TSymStr):boolean;

        function try_consume_hintdirective(var symopt:tsymoptions; var deprecatedmsg:pshortstring):boolean;
      end;


implementation

    uses
       globals,scanner,verbose,fmodule,compiler;

     constructor TParserBaseHelpers.Create(ACompiler: TCompilerBase);
       begin
         FCompiler:=ACompiler;
         getfuncrefdef:=nil;
       end;


{****************************************************************************
                               Token Parsing
****************************************************************************}

     procedure TParserBaseHelpers.identifier_not_found(const s:string);
       begin
         Message1(sym_e_id_not_found,s);
         { show a fatal that you need -S2 or -Sd, but only
           if we just parsed the a token that has m_class }
         if not(m_class in current_settings.modeswitches) and
            (Upper(s)=current_scanner.pattern) and
            (m_class in tokeninfo^[current_scanner.idtoken].keyword) then
           Message(parser_f_need_objfpc_or_delphi_mode);
       end;


     procedure TParserBaseHelpers.identifier_not_found(const s:string;const filepos:tfileposinfo);
       begin
         MessagePos1(filepos,sym_e_id_not_found,s);
         { show a fatal that you need -S2 or -Sd, but only
           if we just parsed the a token that has m_class }
         if not(m_class in current_settings.modeswitches) and
            (Upper(s)=current_scanner.pattern) and
            (m_class in tokeninfo^[current_scanner.idtoken].keyword) then
           MessagePos(filepos,parser_f_need_objfpc_or_delphi_mode);
       end;


    { consumes token i, write error if token is different }

    procedure TParserBaseHelpers.consume(i : ttoken);

    begin
        if (current_scanner.token<>i) and (current_scanner.idtoken<>i) then
          begin
            if current_scanner.had_multiline_string then
              Message2(scan_f_unterminated_multiline_string,
                       tostr(current_scanner.multiline_start_line),
                       tostr(current_scanner.multiline_start_column))
            else if current_scanner.token=_id then
              Message2(scan_f_syn_expected,tokeninfo^[i].str,'identifier '+current_scanner.pattern)
            else
              Message2(scan_f_syn_expected,tokeninfo^[i].str,tokeninfo^[current_scanner.token].str);
          end
        else
          begin
            if current_scanner.token=_END then
              last_endtoken_filepos:=current_tokenpos;
            current_scanner.readtoken(true);
          end;
      end;

    procedure TParserBaseHelpers.consume_last_dot;

    begin
        if (current_scanner.token<>_POINT) then
          begin
          if current_scanner.token=_id then
            Message2(scan_f_syn_expected,tokeninfo^[_POINT].str,'identifier '+current_scanner.pattern)
          else
            Message2(scan_f_syn_expected,tokeninfo^[_POINT].str,tokeninfo^[current_scanner.token].str)
          end
        else if current_scanner.c<>#0 then
          current_scanner.readtoken(true);
    end;

    function TParserBaseHelpers.try_to_consume(i:Ttoken):boolean;
      begin
        try_to_consume:=false;
        if (current_scanner.token=i) or (current_scanner.idtoken=i) then
         begin
           try_to_consume:=true;
           if current_scanner.token=_END then
            last_endtoken_filepos:=current_tokenpos;
           current_scanner.readtoken(true);
         end;
      end;


    procedure TParserBaseHelpers.consume_all_until(atoken : ttoken);
      begin
         while (current_scanner.token<>atoken) and (current_scanner.idtoken<>atoken) do
          begin
            consume(current_scanner.token);
            if current_scanner.token=_EOF then
             begin
               consume(atoken);
               if current_scanner.had_multiline_string then
                 Message2(scan_f_unterminated_multiline_string,
                          tostr(current_scanner.multiline_start_line),
                          tostr(current_scanner.multiline_start_column))
               else
                 Message(scan_f_end_of_file);
               exit;
             end;
          end;
      end;


    procedure TParserBaseHelpers.consume_emptystats;
      begin
         repeat
         until not try_to_consume(_SEMICOLON);
      end;


    { check if a symbol contains the hint directive, and if so gives out a hint
      if required.

      If this code is changed, it's likly that consume_sym_orgid and factor_read_id
      must be changed as well (FK)
    }
    function TParserBaseHelpers.consume_sym(var srsym:tsym;var srsymtable:TSymtable):boolean;
      var
        compiler: TCompilerBase absolute current_compiler;  { TODO: fix node compiler reference!!! }
      var
        t : ttoken;
      begin
        { first check for identifier }
        if current_scanner.token<>_ID then
          begin
            consume(_ID);
            srsym:=generrorsym;
            srsymtable:=nil;
            result:=false;
            exit;
          end;
        compiler.symtablestack.searchsym(current_scanner.pattern,srsym,srsymtable);
        { handle unit specification like System.Writeln }
        try_consume_unitsym_no_specialize(srsym,srsymtable,t,[cuf_consume_id],current_scanner.pattern);
        { if nothing found give error and return errorsym }
        if assigned(srsym) then
          check_hints(srsym,srsym.symoptions,srsym.deprecatedmsg)
        else
          begin
            identifier_not_found(current_scanner.orgpattern);
            srsym:=generrorsym;
            srsymtable:=nil;
          end;
        consume(t);
        result:=assigned(srsym);
      end;


    { check if a symbol contains the hint directive, and if so gives out a hint
      if required and returns the id with it's original casing
    }
    function TParserBaseHelpers.consume_sym_orgid(var srsym:tsym;var srsymtable:TSymtable;var s : string):boolean;
      var
        compiler: TCompilerBase absolute current_compiler;  { TODO: fix node compiler reference!!! }
      var
        t : ttoken;
      begin
        { first check for identifier }
        if current_scanner.token<>_ID then
          begin
            consume(_ID);
            srsym:=generrorsym;
            srsymtable:=nil;
            result:=false;
            exit;
          end;
        compiler.symtablestack.searchsym(current_scanner.pattern,srsym,srsymtable);
        { handle unit specification like System.Writeln }
        try_consume_unitsym_no_specialize(srsym,srsymtable,t,[cuf_consume_id],current_scanner.pattern);
        { if nothing found give error and return errorsym }
        if assigned(srsym) then
          check_hints(srsym,srsym.symoptions,srsym.deprecatedmsg)
        else
          begin
            identifier_not_found(current_scanner.orgpattern);
            srsym:=generrorsym;
            srsymtable:=nil;
          end;
        s:=current_scanner.orgpattern;
        consume(t);
        result:=assigned(srsym);
      end;


    function TParserBaseHelpers.try_consume_unitsym(var srsym:tsym;var srsymtable:TSymtable;var tokentoconsume:ttoken;flags:tconsume_unitsym_flags;out is_specialize:boolean;sympattern:TSymStr):boolean;
      var
        hmodule: tmodule;
        ns:ansistring;
        nssym:tsym;
        nsitem : TCmdStrListItem;

        procedure consume_namespace;
          begin
            while assigned(srsym) and (srsym.typ=namespacesym) do
              begin
                { we have a namespace. the next identifier should be either a namespace or a unit }
                searchsym_in_module(hmodule,ns+'.'+current_scanner.pattern,srsym,srsymtable);
                if assigned(srsym) and (srsym.typ in [namespacesym,unitsym]) then
                  begin
                    ns:=ns+'.'+current_scanner.pattern;
                    nssym:=srsym;
                    consume(_ID);
                    consume(_POINT);
                  end;
              end;
            { check if there is a hidden unit with this current_scanner.pattern in the namespace }
            if not assigned(srsym) and
               assigned(nssym) and (nssym.typ=namespacesym) and assigned(tnamespacesym(nssym).unitsym) then
              srsym:=tnamespacesym(nssym).unitsym;
          end;

      begin
        result:=false;
        tokentoconsume:=_ID;
        is_specialize:=false;

        if not assigned(srsym) and (current_scanner.pattern<>'') and (namespacelist.count>0) then
          begin
            hmodule:=get_module(current_filepos.moduleindex);
            if not assigned(hmodule) then
              internalerror(2018050301);

            nsitem:=TCmdStrListItem(namespacelist.first);
            while assigned(nsitem) do
              begin
                ns:=upper(nsitem.str)+'.'+sympattern;

                if searchsym_in_module(hmodule,ns,srsym,srsymtable) and
                    (srsym.typ in [unitsym,namespacesym]) then
                  break;

                nsitem:=TCmdStrListItem(nsitem.next);
              end;
          end;

        if assigned(srsym) and (srsym.typ in [unitsym,namespacesym]) then
          begin
            if not(srsym.owner.symtabletype in [staticsymtable,globalsymtable]) then
              internalerror(2005011503);
            { only allow unit.symbol access if the name was
              found in the current module
              we can use iscurrentunit because generic specializations does not
              change current_unit variable }
            hmodule:=find_module_from_symtable(srsym.Owner);
            if not Assigned(hmodule) then
              internalerror(2010011201);
            if hmodule.moduleid=current_filepos.moduleindex then
              begin
                if cuf_consume_id in flags then
                  consume(_ID);
                consume(_POINT);
                if srsym.typ=namespacesym then
                  begin
                    ns:=srsym.name;
                    nssym:=srsym;
                    consume_namespace;
                    if not assigned(srsym) and (namespacelist.count>0) then
                      begin
                        nsitem:=TCmdStrListItem(namespacelist.first);
                        while assigned(nsitem) do
                          begin
                            ns:=upper(nsitem.str)+'.'+nssym.name;

                            if searchsym_in_module(hmodule,ns,srsym,srsymtable) and
                                (srsym.typ in [unitsym,namespacesym]) then
                              begin
                                consume_namespace;
                                break;
                              end;

                            nsitem:=TCmdStrListItem(nsitem.next);
                          end;
                      end;
                    if assigned(srsym) and (srsym.typ<>unitsym) then
                      internalerror(2011082601);
                    if not assigned(srsym) then
                      begin
                        result:=true;
                        srsymtable:=nil;
                        exit;
                      end;
                  end;
                case current_scanner.token of
                  _ID:
                    begin
                      if cuf_check_attr_suffix in flags then
                        begin
                          if searchsym_in_module(tunitsym(srsym).module,current_scanner.pattern+custom_attribute_suffix,srsym,srsymtable) then
                            exit(true);
                        end;
                      { system.char? (char=widechar comes from the implicit
                        uachar/uuchar unit -> override) }
                      if (current_scanner.pattern='CHAR') and
                         (tmodule(tunitsym(srsym).module).globalsymtable=systemunit) then
                        begin
                          if m_default_unicodestring in current_settings.modeswitches then
                            searchsym_in_module(tunitsym(srsym).module,'WIDECHAR',srsym,srsymtable)
                          else
                            searchsym_in_module(tunitsym(srsym).module,'ANSICHAR',srsym,srsymtable)
                        end
                      else
                        if (cuf_allow_specialize in flags) and (current_scanner.idtoken=_SPECIALIZE) then
                          begin
                            consume(_ID);
                            is_specialize:=true;
                            if current_scanner.token=_ID then
                              begin
                                if (cuf_check_attr_suffix in flags) and
                                    searchsym_in_module(tunitsym(srsym).module,current_scanner.pattern+custom_attribute_suffix,srsym,srsymtable) then
                                  exit(true);
                                searchsym_in_module(tunitsym(srsym).module,current_scanner.pattern,srsym,srsymtable);
                              end;
                          end
                        else
                          searchsym_in_module(tunitsym(srsym).module,current_scanner.pattern,srsym,srsymtable);
                     end;
                  _STRING:
                    begin
                      if cs_compilesystem in current_settings.moduleswitches then
                        Message(parser_e_nostringaliasinsystem);
                      { system.string? }
                      if tmodule(tunitsym(srsym).module).globalsymtable=systemunit then
                        begin
                          if cs_refcountedstrings in current_settings.localswitches then
                            begin
                              if m_default_unicodestring in current_settings.modeswitches then
                                searchsym_in_module(tunitsym(srsym).module,'UNICODESTRING',srsym,srsymtable)
                              else
                                searchsym_in_module(tunitsym(srsym).module,'ANSISTRING',srsym,srsymtable)
                            end
                          else
                            searchsym_in_module(tunitsym(srsym).module,'SHORTSTRING',srsym,srsymtable);
                          tokentoconsume:=_STRING;
                        end;
                    end
                  else
                    ;
                  end;
              end
            else
              begin
                srsym:=nil;
                srsymtable:=nil;
              end;
            result:=true;
          end;
      end;


    function TParserBaseHelpers.try_consume_unitsym_no_specialize(var srsym:tsym;var srsymtable:TSymtable;var tokentoconsume:ttoken;flags:tconsume_unitsym_flags;sympattern:TSymStr):boolean;
      var
        dummy: Boolean;
      begin
        exclude(flags,cuf_allow_specialize);
        result:=try_consume_unitsym(srsym,srsymtable,tokentoconsume,flags,dummy,sympattern);
      end;

    function TParserBaseHelpers.try_consume_hintdirective(var symopt:tsymoptions; var deprecatedmsg:pshortstring):boolean;
      var
        last_is_deprecated:boolean;
      begin
        try_consume_hintdirective:=false;
        if not(m_hintdirective in current_settings.modeswitches) then
          exit;
        repeat
          last_is_deprecated:=false;
          case current_scanner.idtoken of
            _LIBRARY:
              begin
                if sp_hint_library in symopt then
                  Message1(parser_e_dir_not_allowed,arraytokeninfo[current_scanner.idtoken].str)
                else
                  include(symopt,sp_hint_library);
                try_consume_hintdirective:=true;
              end;
            _DEPRECATED:
              begin
                if sp_hint_deprecated in symopt then
                  Message1(parser_e_dir_not_allowed,arraytokeninfo[current_scanner.idtoken].str)
                else
                  include(symopt,sp_hint_deprecated);
                try_consume_hintdirective:=true;
                last_is_deprecated:=true;
              end;
            _EXPERIMENTAL:
              begin
                if sp_hint_experimental in symopt then
                  Message1(parser_e_dir_not_allowed,arraytokeninfo[current_scanner.idtoken].str)
                else
                  include(symopt,sp_hint_experimental);
                try_consume_hintdirective:=true;
              end;
            _PLATFORM:
              begin
                if sp_hint_platform in symopt then
                  Message1(parser_e_dir_not_allowed,arraytokeninfo[current_scanner.idtoken].str)
                else
                  include(symopt,sp_hint_platform);
                try_consume_hintdirective:=true;
              end;
            _UNIMPLEMENTED:
              begin
                if sp_hint_unimplemented in symopt then
                  Message1(parser_e_dir_not_allowed,arraytokeninfo[current_scanner.idtoken].str)
                else
                  include(symopt,sp_hint_unimplemented);
                try_consume_hintdirective:=true;
              end;
            else
              break;
          end;
          consume(current_scanner.token);
          { handle deprecated message }
          if ((current_scanner.token=_CSTRING) or (current_scanner.token=_CCHAR)) and last_is_deprecated then
            begin
              if not assigned(deprecatedmsg) then
                begin
                  if current_scanner.token=_CSTRING then
                    deprecatedmsg:=stringdup(current_scanner.cstringpattern)
                  else
                    deprecatedmsg:=stringdup(current_scanner.pattern);
                end;
              consume(current_scanner.token);
              include(symopt,sp_has_deprecated_msg);
            end;
        until false;
      end;

end.
