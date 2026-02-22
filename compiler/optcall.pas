{
    Replaces calls by inline code

    Copyright (c) 1998-2026 by Florian Klaempfl and others

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

unit optcall;

{$i fpcdefs.inc}

{ $define EXTDEBUG_INLINE}

  interface

    uses
      node;

    procedure do_optinline(var rootnode : tnode;out changed: boolean);

  implementation

    uses
      cclasses,
      verbose,globals,
      defutil,defcmp,
      symconst,symtype,symdef,symsym,
      procinfo,
      nutils,
      fmodule,
      pass_1,
      nbas,ncal,nld;

    { this procedure removes the user code flag because it prevents optimizations }
    function removeusercodeflag(var n : tnode; arg : pointer) : foreachnoderesult;
      begin
        result:=fen_false;
        if nf_usercode_entry in n.flags then
          begin
            exclude(n.flags,nf_usercode_entry);
            result:=fen_norecurse_true;
          end;
      end;


    function setinlinelevel(var n:tnode; arg:pointer):foreachnoderesult;
      begin
        if n.nodetype=calln then
          tcallnode(n).inlinelevel:=PtrUInt(arg);
        result:=fen_false;
      end;


    { reference symbols that are imported from another unit }
    function importglobalsyms(var n:tnode; arg:pointer):foreachnoderesult;
      var
        sym : tsym;
      begin
        result:=fen_false;
        if n.nodetype=loadn then
          begin
            sym:=tloadnode(n).symtableentry;
            if sym.typ=staticvarsym then
              begin
                if FindUnitSymtable(tloadnode(n).symtable).moduleid<>current_module.moduleid then
                  current_module.addimportedsym(sym);
              end
            else if (sym.typ=constsym) and (tconstsym(sym).consttyp in [constwresourcestring,constresourcestring]) then
              begin
                if tloadnode(n).symtableentry.owner.moduleid<>current_module.moduleid then
                  current_module.addimportedsym(sym);
              end;
          end
        else if (n.nodetype=calln) then
          begin
            if (assigned(tcallnode(n).procdefinition)) and
               (tcallnode(n).procdefinition.typ=procdef) and
               (findunitsymtable(tcallnode(n).procdefinition.owner).moduleid<>current_module.moduleid) then
              current_module.addimportedsym(tprocdef(tcallnode(n).procdefinition).procsym);
          end;
      end;


    function doinline(var _n: tnode; arg: pointer): foreachnoderesult;
      var
        n,
        body : tnode;
        para : tcallparanode;
        inlineblock,
        inlinecleanupblock : tblocknode;
        callnode: tcallnode;
      begin
        result:=fen_false;
        if not(_n.nodetype=calln) or not(po_inline in tcallnode(_n).procdefinition.procoptions) then
          exit;
        callnode:=tcallnode(_n);

        if (po_inline in callnode.procdefinition.procoptions) and
          (callnode.procdefinition.typ=procdef) and
          ((pio_inline_not_possible in tprocdef(callnode.procdefinition).implprocoptions) or
           not(cnf_do_inline in callnode.callnodeflags)) then
          begin
            if not(po_compilerproc in callnode.procdefinition.procoptions) then
              Message1(cg_n_no_inline,tprocdef(callnode.procdefinition).customprocname([pno_proctypeoption, pno_paranames,pno_ownername, pno_noclassmarker, pno_prettynames]));
            exit;
          end;

        if not(assigned(tprocdef(callnode.procdefinition).inlininginfo) and
          assigned(tprocdef(callnode.procdefinition).inlininginfo^.code)) then
          internalerror(200412021);

        callnode.inlinelocals:=TFPObjectList.create(true);

        { inherit flags }
        current_procinfo.flags:=current_procinfo.flags+
          ((callnode.procdefinition as tprocdef).inlininginfo^.flags*inherited_inlining_flags);

        { Create new code block for inlining }
        inlineblock:=internalstatements(callnode.inlineinitstatement);
        { make sure that valid_for_assign() returns false for this block
          (otherwise assigning values to the block will result in assigning
           values to the inlined function's result) }
        include(inlineblock.flags,nf_no_lvalue);
        inlinecleanupblock:=internalstatements(callnode.inlinecleanupstatement);

        if assigned(callnode.callinitblock) then
          addstatement(callnode.inlineinitstatement,callnode.callinitblock.getcopy);

        { replace complex parameters with temps }
        callnode.createinlineparas;

        { create a copy of the body and replace parameter loads with the parameter values }
        body:=tprocdef(callnode.procdefinition).inlininginfo^.code.getcopy;
        foreachnodestatic(pm_postprocess,body,@ removeusercodeflag,nil);
        foreachnodestatic(pm_postprocess,body,@importglobalsyms,nil);
        foreachnodestatic(pm_postprocess,body,@setinlinelevel,pointer(callnode.inlinelevel+1));
        foreachnode(pm_preprocess,body,@callnode.replaceparaload,@callnode.fileinfo);

        { Concat the body and finalization parts }
        addstatement(callnode.inlineinitstatement,body);
        addstatement(callnode.inlineinitstatement,inlinecleanupblock);
        inlinecleanupblock:=nil;

        if assigned(callnode.callcleanupblock) then
          addstatement(callnode.inlineinitstatement,callnode.callcleanupblock.getcopy);

        { the last statement of the new inline block must return the
          location and type of the function result.
          This is not needed when the result is not used, also the tempnode is then
          already destroyed  by a tempdelete in the callcleanupblock tree }
        if not is_void(callnode.resultdef) and
           (cnf_return_value_used in callnode.callnodeflags) then
          begin
            if assigned(callnode.funcretnode) then
              addstatement(callnode.inlineinitstatement,callnode.funcretnode.getcopy)
            else
              begin
                para:=tcallparanode(callnode.left);
                while assigned(para) do
                  begin
                    if (vo_is_hidden_para in para.parasym.varoptions) and
                       (vo_is_funcret in para.parasym.varoptions) then
                      begin
                        addstatement(callnode.inlineinitstatement,para.left.getcopy);
                        break;
                      end;
                    para:=tcallparanode(para.right);
                  end;
              end;
          end;

        typecheckpass(tnode(inlineblock));
        doinlinesimplify(tnode(inlineblock));
        firstpass(tnode(inlineblock));
        _n:=inlineblock;

        { if the function result is used then verify that the blocknode
          returns the same result type as the original callnode }
        if (cnf_return_value_used in callnode.callnodeflags) and
           not(equal_defs(_n.resultdef,callnode.resultdef)) then
          internalerror(200709171);

        { free the temps for the locals }
        callnode.inlinelocals.free;
        callnode.inlinelocals:=nil;
        callnode.inlineinitstatement:=nil;
        callnode.inlinecleanupstatement:=nil;

        n:=callnode.optimize_funcret_assignment(inlineblock);
        if assigned(n) then
          begin
            inlineblock.free;
            inlineblock:=nil;
            _n:=n;
          end;

        PBoolean(arg)^:=true;

{$ifdef EXTDEBUG_INLINE}
        writeln;
        writeln('**************************************************************************************************************');
        writeln('************************** Inlined ',tprocdef(callnode.procdefinition).mangledname,'**************************');
        writeln('**************************************************************************************************************');
{$endif EXTDEBUG_INLINE}
      end;


    procedure do_optinline(var rootnode: tnode;out changed: boolean);
      begin
        changed:=false;
{$ifdef EXTDEBUG_INLINE}
        writeln('************************ Tree before inlining ******************************');
        printnode(rootnode);
        writeln('****************************************************************************');
{$endif EXTDEBUG_INLINE}
        foreachnodestatic(pm_postprocess, rootnode, @doinline, @changed);
{$ifdef EXTDEBUG_INLINE}
        if changed then
          begin
            writeln('************************ Tree after inlining ******************************');
            printnode(rootnode);
            writeln('****************************************************************************');
          end;
{$endif EXTDEBUG_INLINE}
      end;

end.

