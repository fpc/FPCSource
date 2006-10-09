{
    Copyright (c) 1998-2002 by Eindhoven University of Technology

    Parses formal annotation in declarations.

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
unit pdecformal;

{$i fpcdefs.inc}

interface
  
    uses
      symsym, symdef;

    procedure read_formal_decs;

implementation
  
    uses
       { common }
       cutils,cclasses,
       { global }
       globtype,globals,tokens,verbose,
       systems,
       { symtable }
       symconst,symbase,symtype,symtable,defutil,defcmp,
       fmodule,
       { pass 1 }
       node,pass_1,
       nmat,nadd,ncal,nset,ncnv,ninl,ncon,nld,nflw,nmem,
       { codegen }
       ncgutil,
       { parser }
       scanner,
       pbase,pexpr,ptype,ptconst,pdecsub,
       { link }
       import,
       { exceptions }
       sysutils
       ;


    procedure read_proposition; forward;
    procedure read_specvar; forward;
    procedure read_precondition; forward;
    procedure read_postcondition; forward;

    type
      EInvalidAnnotation = class(Exception);

    procedure read_formal_decs;
    { Reads the formal declarations of specification variables ('specvar's),
    propositions, pre- and postconditions into a symtablestack. }
      begin
        if (token=_CLOSE_FORMAL) then
          begin
            consume(_CLOSE_FORMAL);
            exit;
          end;

        try
          repeat
            { expect "def", "specvar", "pre" or "post" }
            if (token<>_ID) then
              consume(_ID);

            if upcase(pattern)='DEF' then
              begin
                consume(_ID);
                read_proposition;
              end
            else if upcase(pattern)='SPECVAR' then
              begin
                consume(_ID);
                read_specvar;
              end
            else if upcase(pattern)='PRE' then
              begin
                consume(_ID);
                read_precondition;
              end
            else if upcase(pattern)='POST' then
              begin
                consume(_ID);
                read_postcondition;
              end
            else
              begin
                raise EInvalidAnnotation.Create('specvar, pre, post or def expected');
              end;

          until not try_to_consume(_SEMICOLON);
          consume(_CLOSE_FORMAL);

        except
          on e: EInvalidAnnotation do
            begin
              { Consume the whole annotation }
              while token<>_CLOSE_FORMAL do
                consume(token);
              consume(_CLOSE_FORMAL);
              Message1(parser_e_invalid_formal_annotation, e.message);
            end;
        end; { try..except }
      
      end; { procedure read_formal_decs }
    
    procedure read_proposition;
      var
        prop_name : string;
        expr : tnode;
        vs : tabstractvarsym;
      begin
        { parse P : expression }
        prop_name := pattern;
        consume(_ID);
        consume(_COLON);
        expr:=comp_expr(true);
        do_resulttypepass(expr);
        if not is_boolean(expr.resulttype.def) then
          begin
            Message1(type_e_boolean_expr_expected, expr.resulttype.def.typename);
            exit;
          end;
        if not(symtablestack.symtabletype in [localsymtable,globalsymtable]) then
          begin
            { TODO : more descriptive error message }
            raise EInvalidAnnotation.Create('Proposition definition outside local '+
            'or global declaration context');
          end;
        vs:=tpropositionsym.create(prop_name, expr.resulttype, expr);
        symtablestack.insert(vs);
      end;

    procedure read_specvar;
      begin
        raise EInvalidAnnotation.Create('Specification variables are not yet implemented');
      end;

    procedure read_precondition;
      begin
        raise EInvalidAnnotation.Create('Pre-/postconditions are not yet implemented');
      end;

    procedure read_postcondition;
      begin
        raise EInvalidAnnotation.Create('Pre-/postconditions are not yet implemented');
      end;

end.



