{
    $Id$
    Copyright (c) 1998-2002 by Florian Klaempfl

    Type checking and register allocation for inline nodes

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
unit nutils;

{$i fpcdefs.inc}

interface

  uses
    symsym,node;

  const
    NODE_COMPLEXITY_INF = 255;

  type
    { resulttype of functions that process on all nodes in a (sub)tree }
    foreachnoderesult = (
      { false, continue recursion }
      fen_false,
      { false, stop recursion }
      fen_norecurse_false,
      { true, continue recursion }
      fen_true,
      { true, stop recursion }
      fen_norecurse_true
    );


    foreachnodefunction = function(var n: tnode; arg: pointer): foreachnoderesult of object;
    staticforeachnodefunction = function(var n: tnode; arg: pointer): foreachnoderesult;

    function foreachnode(var n: tnode; f: foreachnodefunction; arg: pointer): boolean;
    function foreachnodestatic(var n: tnode; f: staticforeachnodefunction; arg: pointer): boolean;

    procedure load_procvar_from_calln(var p1:tnode);
    function maybe_call_procvar(var p1:tnode;tponly:boolean):boolean;
    function load_high_value_node(vs:tvarsym):tnode;
    function load_self_node:tnode;
    function load_result_node:tnode;
    function load_self_pointer_node:tnode;
    function load_vmt_pointer_node:tnode;
    function is_self_node(p:tnode):boolean;

    function call_fail_node:tnode;
    function initialize_data_node(p:tnode):tnode;
    function finalize_data_node(p:tnode):tnode;

    function node_complexity(p: tnode): cardinal;

implementation

    uses
      globtype,globals,verbose,
      symconst,symbase,symtype,symdef,symtable,
      defutil,
      nbas,ncon,ncnv,nld,nflw,nset,ncal,nadd,nmem,
      cgbase,procinfo,
      pass_1;

  function foreachnode(var n: tnode; f: foreachnodefunction; arg: pointer): boolean;
    begin
      result := false;
      if not assigned(n) then
        exit;
      case f(n,arg) of
        fen_norecurse_false:
          exit;
        fen_norecurse_true:
          begin
            result := true;
            exit;
          end;
        fen_true:
          result := true;
       { result is already false
        fen_false:
          result := false; }
      end;
      case n.nodetype of
        calln:
          begin
            { not in one statement, won't work because of b- }
            result := foreachnode(tcallnode(n).methodpointer,f,arg) or result;
            result := foreachnode(tcallnode(n).inlinecode,f,arg) or result;
          end;
        ifn, whilerepeatn, forn:
          begin
            { not in one statement, won't work because of b- }
            result := foreachnode(tloopnode(n).t1,f,arg) or result;
            result := foreachnode(tloopnode(n).t2,f,arg) or result;
          end;
        raisen:
          result := foreachnode(traisenode(n).frametree,f,arg) or result;
        casen:
          result := foreachnode(tcasenode(n). elseblock,f,arg) or result;
      end;
      if n.inheritsfrom(tbinarynode) then
        begin
          result := foreachnode(tbinarynode(n).right,f,arg) or result;
          result := foreachnode(tbinarynode(n).left,f,arg) or result;
        end
      else if n.inheritsfrom(tunarynode) then
        result := foreachnode(tunarynode(n).left,f,arg) or result;
    end;


  function foreachnodestatic(var n: tnode; f: staticforeachnodefunction; arg: pointer): boolean;
    begin
      result := false;
      if not assigned(n) then
        exit;
      case f(n,arg) of
        fen_norecurse_false:
          exit;
        fen_norecurse_true:
          begin
            result := true;
            exit;
          end;
        fen_true:
          result := true;
       { result is already false
        fen_false:
          result := false; }
      end;
      case n.nodetype of
        calln:
          begin
            result := foreachnodestatic(tcallnode(n).methodpointer,f,arg) or result;
            result := foreachnodestatic(tcallnode(n).inlinecode,f,arg) or result;
          end;
        ifn, whilerepeatn, forn:
          begin
            { not in one statement, won't work because of b- }
            result := foreachnodestatic(tloopnode(n).t1,f,arg) or result;
            result := foreachnodestatic(tloopnode(n).t2,f,arg) or result;
          end;
        raisen:
          result := foreachnodestatic(traisenode(n).frametree,f,arg) or result;
        casen:
          result := foreachnodestatic(tcasenode(n). elseblock,f,arg) or result;
      end;
      if n.inheritsfrom(tbinarynode) then
        begin
          result := foreachnodestatic(tbinarynode(n).right,f,arg) or result;
          result := foreachnodestatic(tbinarynode(n).left,f,arg) or result;
        end
      else if n.inheritsfrom(tunarynode) then
        result := foreachnodestatic(tunarynode(n).left,f,arg) or result;
    end;


    procedure load_procvar_from_calln(var p1:tnode);
      var
        p2 : tnode;
      begin
        if p1.nodetype<>calln then
          internalerror(200212251);
        { was it a procvar, then we simply remove the calln and
          reuse the right }
        if assigned(tcallnode(p1).right) then
          begin
            p2:=tcallnode(p1).right;
            tcallnode(p1).right:=nil;
          end
        else
          begin
            p2:=cloadnode.create_procvar(tcallnode(p1).symtableprocentry,
               tprocdef(tcallnode(p1).procdefinition),tcallnode(p1).symtableproc);
            { when the methodpointer is typen we've something like:
              tobject.create. Then only the address is needed of the
              method without a self pointer }
            if assigned(tcallnode(p1).methodpointer) and
               (tcallnode(p1).methodpointer.nodetype<>typen) then
             begin
               tloadnode(p2).set_mp(tcallnode(p1).methodpointer);
               tcallnode(p1).methodpointer:=nil;
             end;
          end;
        resulttypepass(p2);
        p1.free;
        p1:=p2;
      end;


    function maybe_call_procvar(var p1:tnode;tponly:boolean):boolean;
      var
        hp : tnode;
      begin
        result:=false;
        if (p1.resulttype.def.deftype<>procvardef) or
           (tponly and
            not(m_tp_procvar in aktmodeswitches)) then
          exit;
        { ignore vecn,subscriptn }
        hp:=p1;
        repeat
          case hp.nodetype of
            vecn,
            derefn,
            typeconvn,
            subscriptn :
              hp:=tunarynode(hp).left;
            else
              break;
          end;
        until false;
        { a tempref is used when it is loaded from a withsymtable }
        if (hp.nodetype in [loadn,temprefn]) then
          begin
            hp:=ccallnode.create_procvar(nil,p1);
            resulttypepass(hp);
            p1:=hp;
            result:=true;
          end;
      end;


    function load_high_value_node(vs:tvarsym):tnode;
      var
        srsym : tsym;
        srsymtable : tsymtable;
      begin
        result:=nil;
        srsymtable:=vs.owner;
        srsym:=searchsymonlyin(srsymtable,'high'+vs.name);
        if assigned(srsym) then
          begin
            result:=cloadnode.create(srsym,srsymtable);
            resulttypepass(result);
          end
        else
          CGMessage(parser_e_illegal_expression);
      end;


    function load_self_node:tnode;
      var
        srsym : tsym;
        srsymtable : tsymtable;
      begin
        result:=nil;
        searchsym('self',srsym,srsymtable);
        if assigned(srsym) then
          begin
            result:=cloadnode.create(srsym,srsymtable);
            resulttypepass(result);
          end
        else
          CGMessage(parser_e_illegal_expression);
      end;


    function load_result_node:tnode;
      var
        srsym : tsym;
        srsymtable : tsymtable;
      begin
        result:=nil;
        searchsym('result',srsym,srsymtable);
        if assigned(srsym) then
          begin
            result:=cloadnode.create(srsym,srsymtable);
            resulttypepass(result);
          end
        else
          CGMessage(parser_e_illegal_expression);
      end;


    function load_self_pointer_node:tnode;
      var
        srsym : tsym;
        srsymtable : tsymtable;
      begin
        result:=nil;
        searchsym('self',srsym,srsymtable);
        if assigned(srsym) then
          begin
            result:=cloadnode.create(srsym,srsymtable);
            include(result.flags,nf_load_self_pointer);
            resulttypepass(result);
          end
        else
          CGMessage(parser_e_illegal_expression);
      end;


    function load_vmt_pointer_node:tnode;
      var
        srsym : tsym;
        srsymtable : tsymtable;
      begin
        result:=nil;
        searchsym('vmt',srsym,srsymtable);
        if assigned(srsym) then
          begin
            result:=cloadnode.create(srsym,srsymtable);
            resulttypepass(result);
          end
        else
          CGMessage(parser_e_illegal_expression);
      end;


    function is_self_node(p:tnode):boolean;
      begin
        is_self_node:=(p.nodetype=loadn) and
                      (tloadnode(p).symtableentry.typ=varsym) and
                      (vo_is_self in tvarsym(tloadnode(p).symtableentry).varoptions);
      end;



    function call_fail_node:tnode;
      var
        para : tcallparanode;
        newstatement : tstatementnode;
        srsym : tsym;
      begin
        result:=internalstatements(newstatement);

        { call fail helper and exit normal }
        if is_class(current_procinfo.procdef._class) then
          begin
            srsym:=search_class_member(current_procinfo.procdef._class,'FREEINSTANCE');
            if assigned(srsym) and
               (srsym.typ=procsym) then
              begin
                { if self<>0 and vmt=1 then freeinstance }
                addstatement(newstatement,cifnode.create(
                    caddnode.create(andn,
                        caddnode.create(unequaln,
                            load_self_pointer_node,
                            cnilnode.create),
                        caddnode.create(equaln,
                            ctypeconvnode.create(
                                load_vmt_pointer_node,
                                voidpointertype),
                            cpointerconstnode.create(1,voidpointertype))),
                    ccallnode.create(nil,tprocsym(srsym),srsym.owner,load_self_node,[]),
                    nil));
              end
            else
              internalerror(200305108);
          end
        else
          if is_object(current_procinfo.procdef._class) then
            begin
              { parameter 3 : vmt_offset }
              { parameter 2 : pointer to vmt }
              { parameter 1 : self pointer }
              para:=ccallparanode.create(
                        cordconstnode.create(current_procinfo.procdef._class.vmt_offset,s32inttype,false),
                    ccallparanode.create(
                        ctypeconvnode.create_explicit(
                            load_vmt_pointer_node,
                            voidpointertype),
                    ccallparanode.create(
                        ctypeconvnode.create_explicit(
                            load_self_pointer_node,
                            voidpointertype),
                    nil)));
              addstatement(newstatement,
                  ccallnode.createintern('fpc_help_fail',para));
            end
        else
          internalerror(200305132);
        { self:=nil }
        addstatement(newstatement,cassignmentnode.create(
            load_self_pointer_node,
            cnilnode.create));
        { exit }
        addstatement(newstatement,cexitnode.create(nil));
      end;


    function initialize_data_node(p:tnode):tnode;
      begin
        if not assigned(p.resulttype.def) then
          resulttypepass(p);
        if is_ansistring(p.resulttype.def) or
           is_widestring(p.resulttype.def) or
           is_interfacecom(p.resulttype.def) or
           is_dynamic_array(p.resulttype.def) then
          begin
            result:=cassignmentnode.create(
               ctypeconvnode.create_explicit(p,voidpointertype),
               cnilnode.create
               );
          end
        else
          begin
            result:=ccallnode.createintern('fpc_initialize',
                  ccallparanode.create(
                      caddrnode.create(
                          crttinode.create(
                              tstoreddef(p.resulttype.def),initrtti)),
                  ccallparanode.create(
                      caddrnode.create(p),
                  nil)));
          end;
      end;


    function finalize_data_node(p:tnode):tnode;
      begin
        if not assigned(p.resulttype.def) then
          resulttypepass(p);
        result:=ccallnode.createintern('fpc_finalize',
              ccallparanode.create(
                  caddrnode.create(
                      crttinode.create(
                          tstoreddef(p.resulttype.def),initrtti)),
              ccallparanode.create(
                  caddrnode.create(p),
              nil)));
      end;


    { this function must return a very high value ("infinity") for   }
    { trees containing a call, the rest can be balanced more or less }
    { at will, probably best mainly in terms of required memory      }
    { accesses                                                       }
    function node_complexity(p: tnode): cardinal;
      begin
        result := 0;
        while true do
          begin
            case p.nodetype of
              temprefn:
                begin
                  result := 1;
                  exit;
                end;
              loadn:
                begin
		  { threadvars need a helper call }
                  if (tloadnode(p).symtableentry.typ=varsym) and
		     (vo_is_thread_var in tvarsym(tloadnode(p).symtableentry).varoptions) then
                    inc(result,5)
                  else
                    inc(result);
                  if (result >= NODE_COMPLEXITY_INF) then
                    result := NODE_COMPLEXITY_INF;
                  exit;
                end;
              subscriptn,
              blockn:
                p := tunarynode(p).left;
              derefn,
              { may be more complex in some cases }
              typeconvn:
                begin
                  inc(result);
                  if (result = NODE_COMPLEXITY_INF) then
                    exit;
                  p := tunarynode(p).left;
                end;
              vecn,
              statementn:
                begin
                  inc(result,node_complexity(tbinarynode(p).left));
                  if (result >= NODE_COMPLEXITY_INF) then
                    begin
                      result := NODE_COMPLEXITY_INF;
                      exit;
                    end;
                  p := tbinarynode(p).right;
                end;
              { better: make muln/divn/modn more expensive }
              addn,subn,orn,andn,xorn,muln,divn,modn,symdifn,
              assignn:
                begin
                  inc(result,node_complexity(tbinarynode(p).left)+1);
                  if (result >= NODE_COMPLEXITY_INF) then
                    begin
                      result := NODE_COMPLEXITY_INF;
                      exit;
                    end;
                  p := tbinarynode(p).right;
                end;
              tempcreaten,
              tempdeleten,
              ordconstn,
              pointerconstn:
                exit;
              else
                begin
                  result := NODE_COMPLEXITY_INF;
                  exit;
                end;
            end;
        end;
      end;


end.

{
  $Log$
  Revision 1.19  2004-08-25 15:54:46  peter
    * fix possible wrong typecast

  Revision 1.18  2004/08/04 08:35:59  jonas
    * some improvements to node complexity calculations

  Revision 1.17  2004/07/15 20:59:58  jonas
    * fixed complexity function so it doesn't always return infinity when a
      load node is encountered

  Revision 1.16  2004/07/15 19:55:40  jonas
    + (incomplete) node_complexity function to assess the complexity of a
      tree
    + support for inlining value and const parameters at the node level
      (all procedures without local variables and without formal parameters
       can now be inlined at the node level)

  Revision 1.15  2004/07/12 09:14:04  jonas
    * inline procedures at the node tree level, but only under some very
      limited circumstances for now (only procedures, and only if they have
      no or only vs_out/vs_var parameters).
    * fixed ppudump for inline procedures
    * fixed ppudump for ppc

  Revision 1.14  2004/06/20 08:55:29  florian
    * logs truncated

  Revision 1.13  2004/06/16 20:07:09  florian
    * dwarf branch merged

  Revision 1.12  2004/05/23 18:28:41  peter
    * methodpointer is loaded into a temp when it was a calln

  Revision 1.11  2004/05/23 15:04:49  peter
    * generate better code for ansistring initialization

  Revision 1.10.2.1  2004/04/28 19:55:52  peter
    * new warning for ordinal-pointer when size is different
    * fixed some cg_e_ messages to the correct section type_e_ or parser_e_

  Revision 1.10  2004/02/20 21:55:59  peter
    * procvar cleanup

}
