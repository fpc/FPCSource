{
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
    globtype,
    symtype,symsym,symbase,symtable,
    node;

  const
    NODE_COMPLEXITY_INF = 255;

  type
    { resultdef of functions that process on all nodes in a (sub)tree }
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

    tforeachprocmethod = (pm_preprocess,pm_postprocess);

    foreachnodefunction = function(var n: tnode; arg: pointer): foreachnoderesult of object;
    staticforeachnodefunction = function(var n: tnode; arg: pointer): foreachnoderesult;

    function foreachnode(var n: tnode; f: foreachnodefunction; arg: pointer): boolean;
    function foreachnode(procmethod : tforeachprocmethod; var n: tnode; f: foreachnodefunction; arg: pointer): boolean;
    function foreachnodestatic(var n: tnode; f: staticforeachnodefunction; arg: pointer): boolean;
    function foreachnodestatic(procmethod : tforeachprocmethod; var n: tnode; f: staticforeachnodefunction; arg: pointer): boolean;

    { checks if the given node tree contains only nodes of the given type,
      if this isn't the case, an ie is thrown
    }
    procedure checktreenodetypes(n : tnode;typeset : tnodetypeset);

    procedure load_procvar_from_calln(var p1:tnode);
    function maybe_call_procvar(var p1:tnode;tponly:boolean):boolean;
    function get_high_value_sym(vs: tparavarsym):tsym; { marking it as inline causes IE 200311075 during loading from ppu file }
    function load_high_value_node(vs:tparavarsym):tnode;
    function load_self_node:tnode;
    function load_result_node:tnode;
    function load_self_pointer_node:tnode;
    function load_vmt_pointer_node:tnode;
    function is_self_node(p:tnode):boolean;

    function call_fail_node:tnode;
    function initialize_data_node(p:tnode):tnode;
    function finalize_data_node(p:tnode):tnode;

    function node_complexity(p: tnode): cardinal;
    function node_resources_fpu(p: tnode): cardinal;
    procedure node_tree_set_filepos(var n:tnode;const filepos:tfileposinfo);

    { tries to simplify the given node }
    procedure dosimplify(var n : tnode);

    procedure propaccesslist_to_node(var p1:tnode;st:TSymtable;pl:tpropaccesslist);
    function node_to_propaccesslist(p1:tnode):tpropaccesslist;

implementation

    uses
      cutils,verbose,constexp,globals,
      symconst,symdef,
      defutil,defcmp,
      nbas,ncon,ncnv,nld,nflw,nset,ncal,nadd,nmem,
      cpubase,cgbase,procinfo,
      pass_1;

  function foreachnode(procmethod : tforeachprocmethod;var n: tnode; f: foreachnodefunction; arg: pointer): boolean;

    function process_children(res : boolean) : boolean;
      var
        i: longint;
      begin
        result:=res;
        case n.nodetype of
        asn:
          if assigned(tasnode(n).call) then
            begin
              result := foreachnode(procmethod,tasnode(n).call,f,arg);
              exit
            end;
          calln:
            begin
              result := foreachnode(procmethod,tcallnode(n).callinitblock,f,arg) or result;
              result := foreachnode(procmethod,tcallnode(n).methodpointer,f,arg) or result;
              result := foreachnode(procmethod,tcallnode(n).callcleanupblock,f,arg) or result;
            end;
          ifn, whilerepeatn, forn, tryexceptn, tryfinallyn:
            begin
              { not in one statement, won't work because of b- }
              result := foreachnode(procmethod,tloopnode(n).t1,f,arg) or result;
              result := foreachnode(procmethod,tloopnode(n).t2,f,arg) or result;
            end;
          raisen:
            { frame tree }
            result := foreachnode(traisenode(n).third,f,arg) or result;
          casen:
            begin
              for i := 0 to tcasenode(n).blocks.count-1 do
                if assigned(tcasenode(n).blocks[i]) then
                  result := foreachnode(procmethod,pcaseblock(tcasenode(n).blocks[i])^.statement,f,arg) or result;
              result := foreachnode(procmethod,tcasenode(n).elseblock,f,arg) or result;
            end;
        end;
        if n.inheritsfrom(tbinarynode) then
          begin
            { first process the "payload" of statementnodes }
            result := foreachnode(procmethod,tbinarynode(n).left,f,arg) or result;
            result := foreachnode(procmethod,tbinarynode(n).right,f,arg) or result;
          end
        else if n.inheritsfrom(tunarynode) then
          result := foreachnode(procmethod,tunarynode(n).left,f,arg) or result;
      end;

    begin
      result := false;
      if not assigned(n) then
        exit;
      if procmethod=pm_preprocess then
        result:=process_children(result);
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
      if procmethod=pm_postprocess then
        result:=process_children(result);
    end;


    function foreachnode(var n: tnode; f: foreachnodefunction; arg: pointer): boolean;
      begin
        result:=foreachnode(pm_postprocess,n,f,arg);
      end;


  function foreachnodestatic(procmethod : tforeachprocmethod;var n: tnode; f: staticforeachnodefunction; arg: pointer): boolean;

    function process_children(res : boolean) : boolean;
      var
        i: longint;
      begin
        result:=res;
        case n.nodetype of
        asn:
          if assigned(tasnode(n).call) then
            begin
              result := foreachnodestatic(procmethod,tasnode(n).call,f,arg);
              exit
            end;
          calln:
            begin
              result := foreachnodestatic(procmethod,tcallnode(n).callinitblock,f,arg) or result;
              result := foreachnodestatic(procmethod,tcallnode(n).methodpointer,f,arg) or result;
              result := foreachnodestatic(procmethod,tcallnode(n).callcleanupblock,f,arg) or result;
            end;
          ifn, whilerepeatn, forn, tryexceptn, tryfinallyn:
            begin
              { not in one statement, won't work because of b- }
              result := foreachnodestatic(procmethod,tloopnode(n).t1,f,arg) or result;
              result := foreachnodestatic(procmethod,tloopnode(n).t2,f,arg) or result;
            end;
          raisen:
            { frame tree }
            result := foreachnodestatic(traisenode(n).third,f,arg) or result;
          casen:
            begin
              for i := 0 to tcasenode(n).blocks.count-1 do
                if assigned(tcasenode(n).blocks[i]) then
                  result := foreachnodestatic(procmethod,pcaseblock(tcasenode(n).blocks[i])^.statement,f,arg) or result;
              result := foreachnodestatic(procmethod,tcasenode(n).elseblock,f,arg) or result;
            end;
        end;
        if n.inheritsfrom(tbinarynode) then
          begin
            { first process the "payload" of statementnodes }
            result := foreachnodestatic(procmethod,tbinarynode(n).left,f,arg) or result;
            result := foreachnodestatic(procmethod,tbinarynode(n).right,f,arg) or result;
          end
        else if n.inheritsfrom(tunarynode) then
          result := foreachnodestatic(procmethod,tunarynode(n).left,f,arg) or result;
      end;

    begin
      result := false;
      if not assigned(n) then
        exit;
      if procmethod=pm_preprocess then
        result:=process_children(result);
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
      if procmethod=pm_postprocess then
        result:=process_children(result);
    end;


    function foreachnodestatic(var n: tnode; f: staticforeachnodefunction; arg: pointer): boolean;
      begin
        result:=foreachnodestatic(pm_postprocess,n,f,arg);
      end;


    function do_check(var n: tnode; arg: pointer): foreachnoderesult;
      begin
        if not(n.nodetype in pnodetypeset(arg)^) then
          internalerror(200610141);
        result:=fen_true;
      end;


    procedure checktreenodetypes(n : tnode;typeset : tnodetypeset);
      begin
        foreachnodestatic(n,@do_check,@typeset);
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
              tloadnode(p2).set_mp(tcallnode(p1).methodpointer.getcopy);
          end;
        typecheckpass(p2);
        p1.free;
        p1:=p2;
      end;


    function maybe_call_procvar(var p1:tnode;tponly:boolean):boolean;
      var
        hp : tnode;
      begin
        result:=false;
        if (p1.resultdef.typ<>procvardef) or
           (tponly and
            not(m_tp_procvar in current_settings.modeswitches)) then
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
        if (hp.nodetype in [calln,loadn,temprefn]) then
          begin
            hp:=ccallnode.create_procvar(nil,p1);
            typecheckpass(hp);
            p1:=hp;
            result:=true;
          end;
      end;


    function get_high_value_sym(vs: tparavarsym):tsym;
      begin
        result := tsym(vs.owner.Find('high'+vs.name));
      end;


    function get_local_or_para_sym(const aname:string):tsym;
      var
        pd : tprocdef;
      begin
        { we can't use searchsym here, because the
          symtablestack is not fully setup when pass1
          is run for nested procedures }
        pd:=current_procinfo.procdef;
        repeat
          result := tsym(pd.localst.Find(aname));
          if assigned(result) then
            break;
          result := tsym(pd.parast.Find(aname));
          if assigned(result) then
            break;
          { try the parent of a nested function }
          if assigned(pd.owner.defowner) and
             (pd.owner.defowner.typ=procdef) then
            pd:=tprocdef(pd.owner.defowner)
          else
            break;
        until false;
      end;


    function load_high_value_node(vs:tparavarsym):tnode;
      var
        srsym : tsym;
      begin
        result:=nil;
        srsym:=get_high_value_sym(vs);
        if assigned(srsym) then
          begin
            result:=cloadnode.create(srsym,vs.owner);
            typecheckpass(result);
          end
        else
          CGMessage(parser_e_illegal_expression);
      end;


    function load_self_node:tnode;
      var
        srsym : tsym;
      begin
        result:=nil;

        srsym:=get_local_or_para_sym('self');
        if assigned(srsym) then
          begin
            result:=cloadnode.create(srsym,srsym.owner);
            include(result.flags,nf_is_self);
          end
        else
          begin
            result:=cerrornode.create;
            CGMessage(parser_e_illegal_expression);
          end;
        typecheckpass(result);
      end;


    function load_result_node:tnode;
      var
        srsym : tsym;
      begin
        result:=nil;
        srsym:=get_local_or_para_sym('result');
        if assigned(srsym) then
          result:=cloadnode.create(srsym,srsym.owner)
        else
          begin
            result:=cerrornode.create;
            CGMessage(parser_e_illegal_expression);
          end;
        typecheckpass(result);
      end;


    function load_self_pointer_node:tnode;
      var
        srsym : tsym;
      begin
        result:=nil;
        srsym:=get_local_or_para_sym('self');
        if assigned(srsym) then
          begin
            result:=cloadnode.create(srsym,srsym.owner);
            include(result.flags,nf_load_self_pointer);
          end
        else
          begin
            result:=cerrornode.create;
            CGMessage(parser_e_illegal_expression);
          end;
        typecheckpass(result);
      end;


    function load_vmt_pointer_node:tnode;
      var
        srsym : tsym;
      begin
        result:=nil;
        srsym:=get_local_or_para_sym('vmt');
        if assigned(srsym) then
          result:=cloadnode.create(srsym,srsym.owner)
        else
          begin
            result:=cerrornode.create;
            CGMessage(parser_e_illegal_expression);
          end;
        typecheckpass(result);
      end;


    function is_self_node(p:tnode):boolean;
      begin
        is_self_node:=(p.nodetype=loadn) and
                      (tloadnode(p).symtableentry.typ=paravarsym) and
                      (vo_is_self in tparavarsym(tloadnode(p).symtableentry).varoptions);
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
                { if self<>0 and vmt<>0 then freeinstance }
                addstatement(newstatement,cifnode.create(
                    caddnode.create(andn,
                        caddnode.create(unequaln,
                            load_self_pointer_node,
                            cnilnode.create),
                        caddnode.create(unequaln,
                            load_vmt_pointer_node,
                            cnilnode.create)),
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
                        ctypeconvnode.create_internal(
                            load_vmt_pointer_node,
                            voidpointertype),
                    ccallparanode.create(
                        ctypeconvnode.create_internal(
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
        if not assigned(p.resultdef) then
          typecheckpass(p);
        if is_ansistring(p.resultdef) or
           is_widestring(p.resultdef) or
           is_interfacecom(p.resultdef) or
           is_dynamic_array(p.resultdef) then
          begin
            result:=cassignmentnode.create(
               ctypeconvnode.create_internal(p,voidpointertype),
               cnilnode.create
               );
          end
        else
          begin
            result:=ccallnode.createintern('fpc_initialize',
                  ccallparanode.create(
                      caddrnode.create_internal(
                          crttinode.create(
                              tstoreddef(p.resultdef),initrtti,rdt_normal)),
                  ccallparanode.create(
                      caddrnode.create_internal(p),
                  nil)));
          end;
      end;


    function finalize_data_node(p:tnode):tnode;
      var
        newstatement : tstatementnode;
      begin
        if not assigned(p.resultdef) then
          typecheckpass(p);
        if is_ansistring(p.resultdef) then
          begin
            result:=internalstatements(newstatement);
            addstatement(newstatement,ccallnode.createintern('fpc_ansistr_decr_ref',
                  ccallparanode.create(
                    ctypeconvnode.create_internal(p,voidpointertype),
                  nil)));
            addstatement(newstatement,cassignmentnode.create(
               ctypeconvnode.create_internal(p.getcopy,voidpointertype),
               cnilnode.create
               ));
          end
        else if is_widestring(p.resultdef) then
          begin
            result:=internalstatements(newstatement);
            addstatement(newstatement,ccallnode.createintern('fpc_widestr_decr_ref',
                  ccallparanode.create(
                    ctypeconvnode.create_internal(p,voidpointertype),
                  nil)));
            addstatement(newstatement,cassignmentnode.create(
               ctypeconvnode.create_internal(p.getcopy,voidpointertype),
               cnilnode.create
               ));
          end
        else if is_interfacecom(p.resultdef) then
          begin
            result:=internalstatements(newstatement);
            addstatement(newstatement,ccallnode.createintern('fpc_intf_decr_ref',
                  ccallparanode.create(
                    ctypeconvnode.create_internal(p,voidpointertype),
                  nil)));
            addstatement(newstatement,cassignmentnode.create(
               ctypeconvnode.create_internal(p.getcopy,voidpointertype),
               cnilnode.create
               ));
          end
        else
          result:=ccallnode.createintern('fpc_finalize',
                ccallparanode.create(
                    caddrnode.create_internal(
                        crttinode.create(
                            tstoreddef(p.resultdef),initrtti,rdt_normal)),
                ccallparanode.create(
                    caddrnode.create_internal(p),
                nil)));
      end;


    { this function must return a very high value ("infinity") for   }
    { trees containing a call, the rest can be balanced more or less }
    { at will, probably best mainly in terms of required memory      }
    { accesses                                                       }
    function node_complexity(p: tnode): cardinal;
      begin
        result := 0;
        while assigned(p) do
          begin
            case p.nodetype of
              temprefn,
              loadvmtaddrn,
              { main reason for the next one: we can't take the address of }
              { loadparentfpnode, so replacing it by a temp which is the   }
              { address of this node's location and then dereferencing     }
              { doesn't work. If changed, check whether webtbs/tw0935      }
              { still works with nodeinlining (JM)                         }
              loadparentfpn:
                begin
                  result := 1;
                  exit;
                end;
              loadn:
                begin
                  { threadvars need a helper call }
                  if (tloadnode(p).symtableentry.typ=staticvarsym) and
                     (vo_is_thread_var in tstaticvarsym(tloadnode(p).symtableentry).varoptions) then
                    inc(result,5)
                  else
                    inc(result);
                  if (result >= NODE_COMPLEXITY_INF) then
                    result := NODE_COMPLEXITY_INF;
                  exit;
                end;
              subscriptn:
                begin
                  if is_class_or_interface(tunarynode(p).left.resultdef) then
                    inc(result);
                  if (result = NODE_COMPLEXITY_INF) then
                    exit;
                  p := tunarynode(p).left;
                end;
              blockn,
              callparan:
                p := tunarynode(p).left;
              notn,
              derefn :
                begin
                  inc(result);
                  if (result = NODE_COMPLEXITY_INF) then
                    exit;
                  p := tunarynode(p).left;
                end;
              typeconvn:
                begin
                  { may be more complex in some cases }
                  if not(ttypeconvnode(p).convtype in [tc_equal,tc_int_2_int,tc_bool_2_bool,tc_real_2_real,tc_cord_2_pointer]) then
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
              addn,subn,orn,andn,xorn,muln,divn,modn,symdifn,
              shln,shrn,
              equaln,unequaln,gtn,gten,ltn,lten,
              assignn:
                begin
                  inc(result,node_complexity(tbinarynode(p).left)+1);
                  if (p.nodetype in [muln,divn,modn]) then
                    inc(result,5);
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
              pointerconstn,
              nothingn,
              niln:
                exit;
              else
                begin
                  result := NODE_COMPLEXITY_INF;
                  exit;
                end;
            end;
        end;
      end;


    { this function returns an indication how much fpu registers
      will be required.
      Note: The algorithms need to be pessimistic to prevent a
      fpu stack overflow on i386 }
    function node_resources_fpu(p: tnode): cardinal;
      var
        res1,res2,res3 : cardinal;
      begin
        result:=0;
        res1:=0;
        res2:=0;
        res3:=0;
        if p.inheritsfrom(tunarynode) then
          begin
            if assigned(tunarynode(p).left) then
              res1:=node_resources_fpu(tunarynode(p).left);
            if p.inheritsfrom(tbinarynode) then
              begin
                if assigned(tbinarynode(p).right) then
                  res2:=node_resources_fpu(tbinarynode(p).right);
                if p.inheritsfrom(ttertiarynode) and assigned(ttertiarynode(p).third) then
                  res3:=node_resources_fpu(ttertiarynode(p).third)
              end;
          end;
        result:=max(max(res1,res2),res3);
        case p.nodetype of
          calln:
            { it could be a recursive call, so we never really know the number of used fpu registers }
            result:=maxfpuregs;
          realconstn,
          typeconvn,
          loadn :
            begin
              if p.expectloc in [LOC_CFPUREGISTER,LOC_FPUREGISTER] then
                result:=max(result,1);
            end;
          assignn,
          addn,subn,muln,slashn,
          equaln,unequaln,gtn,gten,ltn,lten :
            begin
              if (tbinarynode(p).left.expectloc in [LOC_CFPUREGISTER,LOC_FPUREGISTER]) or
                 (tbinarynode(p).right.expectloc in [LOC_CFPUREGISTER,LOC_FPUREGISTER])then
                result:=max(result,2);
              if(p.expectloc in [LOC_CFPUREGISTER,LOC_FPUREGISTER]) then
                inc(result);
            end;
        end;
      end;


    function setnodefilepos(var n: tnode; arg: pointer): foreachnoderesult;
      begin
        result:=fen_true;
        n.fileinfo:=pfileposinfo(arg)^;
      end;


    procedure node_tree_set_filepos(var n:tnode;const filepos:tfileposinfo);
      begin
        foreachnodestatic(n,@setnodefilepos,@filepos);
      end;

{$ifdef FPCMT}
    threadvar
{$else FPCMT}
    var
{$endif FPCMT}
      treechanged : boolean;

    function callsimplify(var n: tnode; arg: pointer): foreachnoderesult;
      var
        hn : tnode;
      begin
        result:=fen_false;

//        do_typecheckpass(n);

        hn:=n.simplify;
        if assigned(hn) then
          begin
            treechanged:=true;
            n.free;
            n:=hn;
            typecheckpass(n);
          end;
      end;


    { tries to simplify the given node calling the simplify method recursively }
    procedure dosimplify(var n : tnode);
      begin
        repeat
          treechanged:=false;
          foreachnodestatic(pm_preprocess,n,@callsimplify,nil);
        until not(treechanged);
      end;


    procedure propaccesslist_to_node(var p1:tnode;st:TSymtable;pl:tpropaccesslist);
      var
        plist : ppropaccesslistitem;
      begin
        plist:=pl.firstsym;
        while assigned(plist) do
         begin
           case plist^.sltype of
             sl_load :
               begin
                 addsymref(plist^.sym);
                 if not assigned(st) then
                   st:=plist^.sym.owner;
                 { p1 can already contain the loadnode of
                   the class variable. When there is no tree yet we
                   may need to load it for with or objects }
                 if not assigned(p1) then
                  begin
                    case st.symtabletype of
                      withsymtable :
                        p1:=tnode(twithsymtable(st).withrefnode).getcopy;
                      ObjectSymtable :
                        p1:=load_self_node;
                    end;
                  end;
                 if assigned(p1) then
                  p1:=csubscriptnode.create(plist^.sym,p1)
                 else
                  p1:=cloadnode.create(plist^.sym,st);
               end;
             sl_subscript :
               begin
                 addsymref(plist^.sym);
                 p1:=csubscriptnode.create(plist^.sym,p1);
               end;
             sl_typeconv :
               p1:=ctypeconvnode.create_explicit(p1,plist^.def);
             sl_absolutetype :
               begin
                 p1:=ctypeconvnode.create(p1,plist^.def);
                 include(p1.flags,nf_absolute);
               end;
             sl_vec :
               p1:=cvecnode.create(p1,cordconstnode.create(plist^.value,plist^.valuedef,true));
             else
               internalerror(200110205);
           end;
           plist:=plist^.next;
         end;
      end;


    function node_to_propaccesslist(p1:tnode):tpropaccesslist;
      var
        sl : tpropaccesslist;

        procedure addnode(p:tnode);
        begin
          case p.nodetype of
            subscriptn :
              begin
                addnode(tsubscriptnode(p).left);
                sl.addsym(sl_subscript,tsubscriptnode(p).vs);
              end;
            typeconvn :
              begin
                addnode(ttypeconvnode(p).left);
                if nf_absolute in ttypeconvnode(p).flags then
                  sl.addtype(sl_absolutetype,ttypeconvnode(p).totypedef)
                else
                  sl.addtype(sl_typeconv,ttypeconvnode(p).totypedef);
              end;
            vecn :
              begin
                addnode(tvecnode(p).left);
                if tvecnode(p).right.nodetype=ordconstn then
                  sl.addconst(sl_vec,tordconstnode(tvecnode(p).right).value,tvecnode(p).right.resultdef)
                else
                  begin
                    Message(parser_e_illegal_expression);
                    { recovery }
                    sl.addconst(sl_vec,0,tvecnode(p).right.resultdef);
                  end;
             end;
            loadn :
              sl.addsym(sl_load,tloadnode(p).symtableentry);
            else
              internalerror(200310282);
          end;
        end;

      begin
        sl:=tpropaccesslist.create;
        addnode(p1);
        result:=sl;
      end;


end.
