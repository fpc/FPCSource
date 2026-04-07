{
    Dead store elimination

    Copyright (c) 2005-2012 by Jeppe Johansen and Florian Klaempfl

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
unit optdeadstore;

{$i fpcdefs.inc}

{ $define DEBUG_DEADSTORE}
{ $define EXTDEBUG_DEADSTORE}

  interface

    uses
      compilerbase,node,nutils;

type
  TDeadStoreEliminationOptimization = class
  private
    FCompiler: TCompilerBase;
    function deadstoreelim(var n: tnode; arg: pointer): foreachnoderesult;
    property Compiler: TCompilerBase read FCompiler;
  public
    constructor Create(ACompiler: TCompilerBase);
    function do_optdeadstoreelim(var rootnode : tnode;out changed: boolean) : tnode;
  end;

  implementation

    uses
      verbose,globtype,cdynset,globals,
      procinfo,pass_1,compiler,
      nbas,nld,
      optbase,
      symsym,symconst;


    constructor TDeadStoreEliminationOptimization.Create(ACompiler: TCompilerBase);
      begin
        FCompiler:=ACompiler;
      end;


    function TDeadStoreEliminationOptimization.deadstoreelim(var n: tnode; arg: pointer): foreachnoderesult;
      var
        a: tassignmentnode;
        redundant: boolean;
      begin
        result:=fen_true;
        if (n.nodetype=statementn) and
           assigned(tstatementnode(n).statement) then
          begin
            if tstatementnode(n).statement.nodetype=assignn then
              begin
                a:=tassignmentnode(tstatementnode(n).statement);

                { we need to have dfa for the node }
                if assigned(a.left.optinfo) and
                   { node must be either a local or parameter load node }
                   (a.left.nodetype=loadn) and
                   { its address cannot have escaped the current routine }
                   not(tabstractvarsym(tloadnode(a.left).symtableentry).addr_taken) and
                   ((
                     (tloadnode(a.left).symtableentry.typ=localvarsym) and
                     (tloadnode(a.left).symtable=compiler.current_procinfo.procdef.localst)) or
                    ((tloadnode(a.left).symtableentry.typ=paravarsym) and
                     (tloadnode(a.left).symtable=compiler.current_procinfo.procdef.parast) and
                     (tparavarsym(tloadnode(a.left).symtableentry).varspez in [vs_const,vs_value])) or
                    ((tloadnode(a.left).symtableentry.typ=staticvarsym) and
                     (tloadnode(a.left).symtable.symtabletype=staticsymtable) and
                     (compiler.current_procinfo.procdef.proctypeoption<>potype_unitinit) and
                     not(vsa_different_scope in tstaticvarsym(tloadnode(a.left).symtableentry).varsymaccess)
                    )
                   ) and
                    ((a.right.nodetype in [niln,stringconstn,pointerconstn,setconstn,guidconstn]) or
                     ((a.right.nodetype=ordconstn) and not(cs_check_range in compiler.globals.current_settings.localswitches)) or
                     ((a.right.nodetype=realconstn) and not(cs_ieee_errors in compiler.globals.current_settings.localswitches)) or
                    ((cs_opt_dead_values in compiler.globals.current_settings.optimizerswitches) and not(might_have_sideeffects(a.right,[mhs_exceptions])))
                   ) then
                  begin
                    redundant:=not(assigned(a.successor)) or not(DynSetIn(a.successor.optinfo^.life,a.left.optinfo^.index));

                    if redundant then
                      begin
{$ifdef DEBUG_DEADSTORE}
                        writeln('************************** Redundant write *********************************');
                        printnode(a);
                        writeln('****************************************************************************');
{$endif DEBUG_DEADSTORE}
                        pboolean(arg)^:=true;

                        tstatementnode(n).statement.free;

                        tstatementnode(n).statement:=compiler.cnothingnode;
                        { do not run firstpass on n here, as it will remove the statement node
                          and this will make foreachnodestatic process the wrong nodes as the current statement
                          node will disappear }
                      end
                  end;
              end;
          end;
      end;


    function TDeadStoreEliminationOptimization.do_optdeadstoreelim(var rootnode: tnode;out changed: boolean): tnode;
      begin
        changed:=false;
{$ifdef EXTDEBUG_DEADSTORE}
        writeln('******************* Tree before deadstore elimination **********************');
        printnode(rootnode);
        writeln('****************************************************************************');
{$endif EXTDEBUG_DEADSTORE}
        if not(pi_dfaavailable in compiler.current_procinfo.flags) then
          internalerror(2013110201);
        if not compiler.current_procinfo.has_nestedprocs then
          foreachnode(pm_postprocess, rootnode, @deadstoreelim, @changed);
{$ifdef DEBUG_DEADSTORE}
        if changed then
          begin
            writeln('******************** Tree after deadstore elimination **********************');
            printnode(rootnode);
            writeln('****************************************************************************');
          end;
{$endif DEBUG_DEADSTORE}
        result:=rootnode;
      end;

end.

