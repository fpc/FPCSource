{
    Copyright (c) 2006 by Eindhoven University of Technology

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

    { Read the formal declaration in procedure/function/method "headers"
    and in the main program }
    procedure read_formal_decs;
    
    { Read the formal declaration in class declarations }
    procedure read_formal_decs_in_class;


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
       sysutils,
       { procinfo }
       procinfo
       ;


    procedure read_definition; forward;
    procedure read_specvar; forward;
    procedure read_precondition; forward;
    procedure read_postcondition; forward;
    procedure read_retcondition; forward;

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
                read_definition;
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
            else if upcase(pattern)='RET' then
              begin
                consume(_ID);
                read_retcondition;
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
    
    procedure read_definition;
      var
        def_name : string;
        def_type : ttype;
        def_expr : tnode;
        def : tabstractvarsym;
        type_given : boolean;
        prev_ignore_equal : boolean;
      begin
        { parse A[,A]*, where A::= P [: TYPE ] = EXPRESSION }
        repeat
          def_name:=pattern;
          consume(_ID);
          
          if try_to_consume(_COLON) then
            begin
              prev_ignore_equal:=ignore_equal;
              ignore_equal:=true;{ Signals to read_type to ignore the = token }
              read_type(def_type, '', false);
              ignore_equal:=prev_ignore_equal;
              type_given:=true;
            end
          else
            type_given:=false;

          consume(_EQUAL);

          def_expr:=comp_expr_in_formal_context(true);
          do_resulttypepass(def_expr);

          if type_given then
            { Match the given type and the type of the expression }
            inserttypeconv(def_expr, def_type)
          else
            { Use the computed type of the expression }
            def_type:=def_expr.resulttype;

          if not(symtablestack.symtabletype in [localsymtable,globalsymtable,staticsymtable]) then
            begin
              { TODO : more descriptive error message }
              raise EInvalidAnnotation.Create('Proposition definition outside local '+
              'or global declaration context');
            end;
          def:=tdefinitionsym.create(def_name, def_type, def_expr);
          symtablestack.insert(def);
        until not try_to_consume(_COMMA);
      end;

    procedure read_specvar;
      var
        sv : tspecvarsym;
        sv_name : string;
        sv_type : ttype;
        sv_expr : tnode;
        type_given : boolean;
        prev_ignore_equal : boolean;
      begin
        { parse A[,A]* where A::= ID[:TYPE] = EXPRESSION }
        repeat
          sv_name:=pattern;
          consume(_ID);

          if try_to_consume(_COLON) then
            begin
              type_given:=true;
              prev_ignore_equal:=ignore_equal;
              ignore_equal:=true; { Signals to read_type that the = token
                should be ignored }
              read_type(sv_type, '', false);
              ignore_equal:=prev_ignore_equal;
            end
          else
            type_given:=false;

          consume(_EQUAL);

          sv_expr:=comp_expr_in_formal_context(true);
          do_resulttypepass(sv_expr);

          if type_given then
            { Match the given type and the type of the expression }
            inserttypeconv(sv_expr, sv_type)
          else
            { Use the computed type of the expression }
            sv_type:=sv_expr.resulttype;

          if not(symtablestack.symtabletype in [localsymtable,globalsymtable,staticsymtable]) then
            raise EInvalidAnnotation.Create('Specvars can only be defined in local or global contexts');

          sv:=tspecvarsym.create(sv_name, sv_type, sv_expr);
          symtablestack.insert(sv);

          if (not assigned(current_procinfo)) or (not assigned(current_procinfo.procdef)) then
            raise EInvalidAnnotation.Create('Specvars cannot be defined here');

          current_procinfo.procdef.specvars.Add(sv);

        until not try_to_consume(_COMMA);
      end;

  
    procedure read_precondition;
      var
        expr : tnode;
      begin
        consume(_EQUAL);
        expr:=comp_expr_in_formal_context(true);
        { Check here the result type of the expression.
        This will be checked later on as well (after conversion to "assert"),
        but then an error message would contain the wrong position }
        do_resulttypepass(expr);
        if not is_boolean(expr.resulttype.def) then
          begin
            Message1(type_e_boolean_expr_expected, expr.resulttype.def.typename);
            exit;
          end;
        if assigned(current_procinfo.procdef.precondition) then
          { There was already a precondition specified,
          use the conjunction of expr and the previous precondition }
          expr:=caddnode.create(andn, current_procinfo.procdef.precondition, expr);
        current_procinfo.procdef.precondition:=expr;
      end;

    procedure read_postcondition;
      var
        expr : tnode;
      begin
        consume(_EQUAL);
        expr:=comp_expr_in_formal_context(true);
        { Check here the result type of the expression.
        This will be checked later on as well (after conversion to "assert"),
        but then an error message would contain the wrong position }
        do_resulttypepass(expr);
        if not is_boolean(expr.resulttype.def) then
          begin
            Message1(type_e_boolean_expr_expected, expr.resulttype.def.typename);
            exit;
          end;
        if assigned(current_procinfo.procdef.postcondition) then
          { There was already a postcondition specified,
          use the conjunction of expr and the previous postcondition }
          expr:=caddnode.create(andn, current_procinfo.procdef.postcondition, expr);
        current_procinfo.procdef.postcondition:=expr;
      end;

    procedure read_retcondition;
      var
        expr : tnode;
        evald : tnode;
        rst : tnode;
      begin
        consume(_EQUAL);
        { Proposition variables are not allowed here }
        expr:=comp_expr(true);
        { Convert this to "Result = expr" } 
        if not assigned(current_procinfo.procdef.funcretsym) then
          raise EInvalidAnnotation.Create('{@ ret : expr } in a non-returning something'); { TODO }
        rst:=cloadnode.create(current_procinfo.procdef.funcretsym,current_procinfo.procdef.localst);
        evald:=caddnode.create(equaln, rst, expr);
        if assigned(current_procinfo.procdef.postcondition) then
          { there was already a postcondition specified,
          use the conjunction of the previous postcondition and result = expr }
          evald:=caddnode.create(andn, current_procinfo.procdef.postcondition, evald);
        current_procinfo.procdef.postcondition:=evald;  
      end;


    procedure read_formal_decs_in_class;

      var
        inv_name : string;
        inv_expr : tnode;

      begin
        if not (try_to_consume(_CLOSE_FORMAL)) then
          begin
            try
              if (upcase(pattern)<>'INV') then
                raise EInvalidAnnotation.Create('inv expected');
              consume(_ID);

              inv_name:=pattern;
              consume(_ID);

              consume(_EQUAL);

              inv_expr:=comp_expr_in_formal_context_class_header(true);

              do_resulttypepass(inv_expr);
              if not is_boolean(inv_expr.resulttype.def) then
                raise EInvalidAnnotation.Create('boolean expression expected');

              if assigned(aktobjectdef.invariant) then
                aktobjectdef.invariant:=caddnode.create(andn,
                  aktobjectdef.invariant,
                  inv_expr)
              else
                aktobjectdef.invariant:=inv_expr;


              consume(_CLOSE_FORMAL);
            except
              on e: EInvalidAnnotation do
                begin
                  { Consume the whole annotation }
                  while token<>_CLOSE_FORMAL do
                    consume(token);
                  consume(_CLOSE_FORMAL);
                  Message1(parser_e_invalid_formal_annotation, e.message);  
                end; { on EInvalidAnnotation }
            end; { try..except }
          end; { if not try_to_consume(_CLOSE_FORMAL) }
      end;
end.



