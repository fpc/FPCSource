{
    $Id$
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
       tokens,globals,
       symconst,symbase,symtype,symdef,symsym,symtable
       ;

    const
       { tokens that end a block or statement. And don't require
         a ; on the statement before }
       endtokens = [_SEMICOLON,_END,_ELSE,_UNTIL];

       { true, if we are after an assignement }
       afterassignment : boolean = false;

       { true, if we are parsing arguments }
       in_args : boolean = false;

       { true, if we got an @ to get the address }
       got_addrn  : boolean = false;

       { special for handling procedure vars }
       getprocvardef : tprocvardef = nil;

    var
       { size of data segment, set by proc_unit or proc_program }
       datasize : longint;

       { for operators }
       optoken : ttoken;

       { symtable were unit references are stored }
       refsymtable : tsymtable;

       { true, if only routine headers should be parsed }
       parse_only : boolean;

       { true, if we should ignore an equal in const x : 1..2=2 }
       ignore_equal : boolean;


    procedure identifier_not_found(const s:string);

    function tokenstring(i : ttoken):string;

    { consumes token i, if the current token is unequal i }
    { a syntax error is written                           }
    procedure consume(i : ttoken);

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
    function consume_sym(var srsym:tsym;var srsymtable:tsymtable):boolean;

    function try_consume_hintdirective(var symopt:tsymoptions):boolean;

    procedure check_hints(const srsym: tsym);

    { just for an accurate position of the end of a procedure (PM) }
    var
       last_endtoken_filepos: tfileposinfo;


implementation

    uses
       globtype,scanner,systems,verbose;

{****************************************************************************
                               Token Parsing
****************************************************************************}

     procedure identifier_not_found(const s:string);
       begin
         Message1(sym_e_id_not_found,s);
         { show a fatal that you need -S2 or -Sd, but only
           if we just parsed the a token that has m_class }
         if not(m_class in aktmodeswitches) and
            (Upper(s)=pattern) and
            (tokeninfo^[idtoken].keyword=m_class) then
           Message(parser_f_need_objfpc_or_delphi_mode);
       end;


    function tokenstring(i : ttoken):string;
      begin
        tokenstring:=tokeninfo^[i].str;
      end;


    { consumes token i, write error if token is different }
    procedure consume(i : ttoken);
      begin
        if (token<>i) and (idtoken<>i) then
          if token=_id then
            Message2(scan_f_syn_expected,tokeninfo^[i].str,'identifier '+pattern)
          else
            Message2(scan_f_syn_expected,tokeninfo^[i].str,tokeninfo^[token].str)
        else
          begin
            if token=_END then
              last_endtoken_filepos:=akttokenpos;
            current_scanner.readtoken;
          end;
      end;


    function try_to_consume(i:Ttoken):boolean;
      begin
        try_to_consume:=false;
        if (token=i) or (idtoken=i) then
         begin
           try_to_consume:=true;
           if token=_END then
            last_endtoken_filepos:=akttokenpos;
           current_scanner.readtoken;
         end;
      end;


    procedure consume_all_until(atoken : ttoken);
      begin
         while (token<>atoken) and (idtoken<>atoken) do
          begin
            Consume(token);
            if token=_EOF then
             begin
               Consume(atoken);
               Message(scan_f_end_of_file);
               exit;
             end;
          end;
      end;


    procedure consume_emptystats;
      begin
         repeat
         until not try_to_consume(_SEMICOLON);
      end;


    { check if a symbol contains the hint directive, and if so gives out a hint
      if required.
    }
    procedure check_hints(const srsym: tsym);
     begin
       if not assigned(srsym) then
         exit;
       if sp_hint_deprecated in srsym.symoptions then
         Message1(sym_w_deprecated_symbol,srsym.realname);
       if sp_hint_platform in srsym.symoptions then
         Message1(sym_w_non_portable_symbol,srsym.realname);
       if sp_hint_unimplemented in srsym.symoptions then
         Message1(sym_w_non_implemented_symbol,srsym.realname);
     end;



    function consume_sym(var srsym:tsym;var srsymtable:tsymtable):boolean;
      begin
        { first check for identifier }
        if token<>_ID then
         begin
           consume(_ID);
           srsym:=generrorsym;
           srsymtable:=nil;
           consume_sym:=false;
           exit;
         end;
        searchsym(pattern,srsym,srsymtable);
        check_hints(srsym);
        if assigned(srsym) then
         begin
           if (srsym.typ=unitsym) then
            begin
              { only allow unit.symbol access if the name was
                found in the current module }
              if srsym.owner.unitid=0 then
               begin
                 consume(_ID);
                 consume(_POINT);
                 srsymtable:=tunitsym(srsym).unitsymtable;
                 srsym:=searchsymonlyin(srsymtable,pattern);
               end
              else
               srsym:=nil;
            end;
         end;
        { if nothing found give error and return errorsym }
        if srsym=nil then
         begin
           identifier_not_found(orgpattern);
           srsym:=generrorsym;
           srsymtable:=nil;
         end;
        consume(_ID);
        consume_sym:=assigned(srsym);
      end;


    function try_consume_hintdirective(var symopt:tsymoptions):boolean;
      begin
        try_consume_hintdirective:=false;
        if not(m_hintdirective in aktmodeswitches) then
         exit;
        repeat
          case idtoken of
            _LIBRARY :
              begin
                include(symopt,sp_hint_library);
                try_consume_hintdirective:=true;
              end;
            _DEPRECATED :
              begin
                include(symopt,sp_hint_deprecated);
                try_consume_hintdirective:=true;
              end;
            _PLATFORM :
              begin
                include(symopt,sp_hint_platform);
                try_consume_hintdirective:=true;
              end;
            _UNIMPLEMENTED :
              begin
                include(symopt,sp_hint_unimplemented);
                try_consume_hintdirective:=true;
              end;
            else
              break;
          end;
          consume(Token);
        until false;
      end;

end.
{
  $Log$
  Revision 1.24  2003-05-15 18:58:53  peter
    * removed selfpointer_offset, vmtpointer_offset
    * tvarsym.adjusted_address
    * address in localsymtable is now in the real direction
    * removed some obsolete globals

  Revision 1.23  2003/03/17 18:55:30  peter
    * allow more tokens instead of only semicolon after inherited

  Revision 1.22  2002/12/05 19:28:05  carl
    - remove lower in hint

  Revision 1.21  2002/11/30 11:12:48  carl
    + checking for symbols used with hint directives is done mostly in pexpr
      only now

  Revision 1.20  2002/11/29 22:31:19  carl
    + unimplemented hint directive added
    * hint directive parsing implemented
    * warning on these directives

  Revision 1.19  2002/09/09 17:34:15  peter
    * tdicationary.replace added to replace and item in a dictionary. This
      is only allowed for the same name
    * varsyms are inserted in symtable before the types are parsed. This
      fixes the long standing "var longint : longint" bug
    - consume_idlist and idstringlist removed. The loops are inserted
      at the callers place and uses the symtable for duplicate id checking

  Revision 1.18  2002/08/17 09:23:38  florian
    * first part of procinfo rewrite

  Revision 1.17  2002/05/18 13:34:11  peter
    * readded missing revisions

  Revision 1.16  2002/05/16 19:46:42  carl
  + defines.inc -> fpcdefs.inc to avoid conflicts if compiling by hand
  + try to fix temp allocation (still in ifdef)
  + generic constructor calls
  + start of tassembler / tmodulebase class cleanup

  Revision 1.14  2002/01/06 21:47:32  peter
    * removed getprocvar, use only getprocvardef

}
