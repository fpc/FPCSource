{
    $Id$
    Copyright (c) 2000 by Florian Klaempfl

    Type checking and register allocation for load/assignment nodes

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
unit nld;

{$i defines.inc}

interface

    uses
       node,symtable;

    type
       tloadnode = class(tunarynode)
          symtableentry : psym;
          symtable : psymtable;
          constructor create(v : psym;st : psymtable);virtual;
          function getcopy : tnode;override;
          function pass_1 : tnode;override;
       end;

       { different assignment types }
       tassigntype = (at_normal,at_plus,at_minus,at_star,at_slash);

       tassignmentnode = class(tbinarynode)
          assigntype : tassigntype;
          constructor create(l,r : tnode);virtual;
          function getcopy : tnode;override;
          function pass_1 : tnode;override;
       end;

       tfuncretnode = class(tnode)
          funcretprocinfo : pointer;
{$IFDEF NEWST}
          retsym : Psym;
{$ELSE}
          rettype : ttype;
{$ENDIF}
          constructor create;virtual;
          function getcopy : tnode;override;
          function pass_1 : tnode;override;
       end;

       tarrayconstructorrangenode = class(tbinarynode)
          constructor create(l,r : tnode);virtual;
          function pass_1 : tnode;override;
       end;

       tarrayconstructnode = class(tbinarynode)
          constructdef : pdef;
          constructor create(l,r : tnode);virtual;
          function getcopy : tnode;
          function pass_1 : tnode;override;
       end;

       ttypenode = class(tnode)
          typenodetype : pdef;
          typenodesym:ptypesym;
          constructor create(t : pdef;sym:ptypesym);virtual;
          function getcopy : tnode;override;
          function pass_1 : tnode;override;
       end;

    var
       cloadnode : class of tloadnode;
       cassignmentnode : class of tassignmentnode;
       cfuncretnode : class of tfuncretnode;
       carrayconstructorrangenode : class of tarrayconstructorrangenode;
       carrayconstructnode : class of tarrayconstructnode;
       ctypenode : class of ttypenode;

    function genloadnode(v : pvarsym;st : psymtable) : tloadnode;
    function gentypenode(t : pdef;sym:ptypesym) : ttypenode;
    function genloadcallnode(v: pprocsym;st: psymtable): tloadnode;
    function genloadmethodcallnode(v: pprocsym;st: psymtable; mp: tnode): tloadnode;
    function gentypedconstloadnode(sym : ptypedconstsym;st : psymtable) : tloadnode;

implementation

    uses
      cutils,cobjects,verbose,globtype,globals,systems,
      symconst,aasm,types,
      htypechk,pass_1,
      ncnv,nmem,cpubase
{$ifdef newcg}
      ,cgbase
      ,tgobj
      ,tgcpu
{$else newcg}
      ,hcodegen
{$ifdef i386}
      ,tgeni386
{$endif}
{$endif newcg}
      ;

    function genloadnode(v : pvarsym;st : psymtable) : tloadnode;

      var
         n : tloadnode;

      begin
         n:=cloadnode.create(v,st);
{$ifdef NEWST}
         n.resulttype:=v^.definition;
{$else NEWST}
         n.resulttype:=v^.vartype.def;
{$endif NEWST}
         genloadnode:=n;
      end;

    function genloadcallnode(v: pprocsym;st: psymtable): tloadnode;
      var
         n : tloadnode;

      begin
         n:=cloadnode.create(v,st);
{$ifdef NEWST}
         n.resulttype:=nil; {We don't know which overloaded procedure is
                              wanted...}
{$else NEWST}
         n.resulttype:=v^.definition;
{$endif NEWST}
         genloadcallnode:=n;
      end;

    function genloadmethodcallnode(v: pprocsym;st: psymtable; mp: tnode): tloadnode;
      var
         n : tloadnode;

      begin
         n:=cloadnode.create(v,st);
{$ifdef NEWST}
         n.resulttype:=nil; {We don't know which overloaded procedure is
                              wanted...}
{$else NEWST}
         n.resulttype:=v^.definition;
{$endif NEWST}
         n.left:=mp;
         genloadmethodcallnode:=n;
      end;


    function gentypedconstloadnode(sym : ptypedconstsym;st : psymtable) : tloadnode;

      var
         n : tloadnode;

      begin
         n:=cloadnode.create(sym,st);
{$ifdef NEWST}
         n.resulttype:=sym^.definition;
{$else NEWST}
         n.resulttype:=sym^.typedconsttype.def;
{$endif NEWST}
         gentypedconstloadnode:=n;
      end;

    function gentypenode(t : pdef;sym:ptypesym) : ttypenode;
      begin
         gentypenode:=ctypenode.create(t,sym);
      end;

{*****************************************************************************
                             TLOADNODE
*****************************************************************************}

    constructor tloadnode.create(v : psym;st : psymtable);

      begin
         inherited create(loadn,nil);
         symtableentry:=v;
         symtable:=st;
      end;

    function tloadnode.getcopy : tnode;

      var
         n : tloadnode;

      begin
         n:=tloadnode(inherited getcopy);
         n.symtable:=symtable;
         n.symtableentry:=symtableentry;
      end;

    function tloadnode.pass_1 : tnode;
      var
         p1 : tnode;
      begin
         if (symtable^.symtabletype=withsymtable) and
            (pwithsymtable(symtable)^.direct_with) and
            (symtableentry^.typ=varsym) then
           begin
              p1:=tnode(pwithsymtable(symtable)^.withrefnode).getcopy;
              p1:=gensubscriptnode(pvarsym(symtableentry),p1);
              left:=nil;
              firstpass(p1);
              pass_1:=p1;
              exit;
           end;

         location.loc:=LOC_REFERENCE;
         registers32:=0;
         registersfpu:=0;
{$ifdef SUPPORT_MMX}
         registersmmx:=0;
{$endif SUPPORT_MMX}
         { handle first absolute as it will replace the symtableentry }
         if symtableentry^.typ=absolutesym then
           begin
             resulttype:=pabsolutesym(symtableentry)^.vartype.def;
             { replace the symtableentry when it points to a var, else
               we are finished }
             if pabsolutesym(symtableentry)^.abstyp=tovar then
              begin
                symtableentry:=pabsolutesym(symtableentry)^.ref;
                symtable:=symtableentry^.owner;
                include(flags,nf_absolute);
              end
             else
              exit;
           end;
         case symtableentry^.typ of
            funcretsym :
              begin
                p1:=cfuncretnode.create;
                tfuncretnode(p1).funcretprocinfo:=pprocinfo(pfuncretsym(symtableentry)^.funcretprocinfo);
                tfuncretnode(p1).rettype:=pfuncretsym(symtableentry)^.rettype;
                firstpass(p1);
                { if it's refered as absolute then we need to have the
                  type of the absolute instead of the function return,
                  the function return is then also assigned }
                if nf_absolute in flags then
                 begin
                   pprocinfo(tfuncretnode(p1).funcretprocinfo)^.funcret_state:=vs_assigned;
                   p1.resulttype:=resulttype;
                 end;
                left:=nil;
                pass_1:=p1;
              end;
            constsym:
              begin
                 if pconstsym(symtableentry)^.consttyp=constresourcestring then
                   begin
                      resulttype:=cansistringdef;
                      { we use ansistrings so no fast exit here }
                      if assigned(procinfo) then
                        procinfo^.no_fast_exit:=true;
                      location.loc:=LOC_MEM;
                   end
                 else
                   internalerror(22799);
              end;
            varsym :
                begin
                { if it's refered by absolute then it's used }
                if nf_absolute in flags then
                 pvarsym(symtableentry)^.varstate:=vs_used
                else
                 if (resulttype=nil) then
                     resulttype:=pvarsym(symtableentry)^.vartype.def;
                   if (symtable^.symtabletype in [parasymtable,localsymtable]) and
                      (lexlevel>symtable^.symtablelevel) then
                     begin
                       { if the variable is in an other stackframe then we need
                         a register to dereference }
                       if (symtable^.symtablelevel)>0 then
                        begin
                          registers32:=1;
                          { further, the variable can't be put into a register }
                          pvarsym(symtableentry)^.varoptions:=
                            pvarsym(symtableentry)^.varoptions-[vo_fpuregable,vo_regable];
                        end;
                     end;
                   if (pvarsym(symtableentry)^.varspez=vs_const) then
                     location.loc:=LOC_MEM;
                   { we need a register for call by reference parameters }
                   if (pvarsym(symtableentry)^.varspez in [vs_var,vs_out]) or
                      ((pvarsym(symtableentry)^.varspez=vs_const) and
                      push_addr_param(pvarsym(symtableentry)^.vartype.def)) or
                      { call by value open arrays are also indirect addressed }
                      is_open_array(pvarsym(symtableentry)^.vartype.def) then
                     registers32:=1;
                   if symtable^.symtabletype=withsymtable then
                     inc(registers32);

                   if ([vo_is_thread_var,vo_is_dll_var]*pvarsym(symtableentry)^.varoptions)<>[] then
                     registers32:=1;
                   { a class variable is a pointer !!!
                     yes, but we have to resolve the reference in an
                     appropriate tree node (FK)

                   if (pvarsym(symtableentry)^.definition^.deftype=objectdef) and
                      ((pobjectdef(pvarsym(symtableentry)^.definition)^.options and oo_is_class)<>0) then
                     registers32:=1;
                   }

                   { count variable references }

                     { this will create problem with local var set by
                     under_procedures
                     if (assigned(pvarsym(symtableentry)^.owner) and assigned(aktprocsym)
                       and ((pvarsym(symtableentry)^.owner = aktprocsym^.definition^.localst)
                       or (pvarsym(symtableentry)^.owner = aktprocsym^.definition^.localst))) then }
                   if t_times<1 then
                     inc(pvarsym(symtableentry)^.refs)
                   else
                     inc(pvarsym(symtableentry)^.refs,t_times);
                end;
            typedconstsym :
                if not(nf_absolute in flags) then
                  resulttype:=ptypedconstsym(symtableentry)^.typedconsttype.def;
            procsym :
                begin
                   if assigned(pprocsym(symtableentry)^.definition^.nextoverloaded) then
                     CGMessage(parser_e_no_overloaded_procvars);
                   resulttype:=pprocsym(symtableentry)^.definition;
                   { if the owner of the procsym is a object,  }
                   { left must be set, if left isn't set       }
                   { it can be only self                       }
                   { this code is only used in TP procvar mode }
                   if (m_tp_procvar in aktmodeswitches) and
                      not(assigned(left)) and
                     (pprocsym(symtableentry)^.owner^.symtabletype=objectsymtable) then
                      left:=genselfnode(pobjectdef(symtableentry^.owner^.defowner));
                   { method pointer ? }
                   if assigned(left) then
                     begin
                        firstpass(left);
                        registers32:=max(registers32,left.registers32);
                        registersfpu:=max(registersfpu,left.registersfpu);
 {$ifdef SUPPORT_MMX}
                        registersmmx:=max(registersmmx,left.registersmmx);
 {$endif SUPPORT_MMX}
                     end;
                end;
           else
             internalerror(3);
         end;
      end;


{*****************************************************************************
                             TASSIGNMENTNODE
*****************************************************************************}

    constructor tassignmentnode.create(l,r : tnode);

      begin
         inherited create(assignn,l,r);
         assigntype:=at_normal;
      end;

    function tassignmentnode.getcopy : tnode;

      var
         n : tassignmentnode;

      begin
         n:=tassignmentnode(inherited getcopy);
         n.assigntype:=assigntype;
         getcopy:=n;
      end;

    function tassignmentnode.pass_1 : tnode;
{$ifdef newoptimizations2}
      var
        hp : tnode;
{$endif newoptimizations2}
      begin
         { must be made unique }
         if assigned(left) then
           begin
              left.set_unique;

              { set we the function result? }
              left.set_funcret_is_valid;
           end;

         firstpass(left);
         left.set_varstate(false);
         if codegenerror then
           exit;

         { assignements to open arrays aren't allowed }
         if is_open_array(left.resulttype) then
           CGMessage(type_e_mismatch);

         { test if we can avoid copying string to temp
           as in s:=s+...; (PM) }
{$ifdef dummyi386}
         if ((right.treetype=addn) or (right.treetype=subn)) and
            equal_trees(left,right.left) and
            (ret_in_acc(left.resulttype)) and
            (not cs_rangechecking in aktmoduleswitches^) then
           begin
              disposetree(right.left);
              hp:=right;
              right:=right.right;
              if hp.treetype=addn then
                assigntyp:=at_plus
              else
                assigntyp:=at_minus;
              putnode(hp);
           end;
         if assigntyp<>at_normal then
           begin
              { for fpu type there is no faster way }
              if is_fpu(left.resulttype) then
                case assigntyp of
                  at_plus  : right:=gennode(addn,getcopy(left),right);
                  at_minus : right:=gennode(subn,getcopy(left),right);
                  at_star  : right:=gennode(muln,getcopy(left),right);
                  at_slash : right:=gennode(slashn,getcopy(left),right);
                end;
           end;
{$endif i386}
         firstpass(right);
         right.set_varstate(true);
         if codegenerror then
           exit;

         { some string functions don't need conversion, so treat them separatly }
         if is_shortstring(left.resulttype) and (assigned(right.resulttype)) then
          begin
            if not (is_shortstring(right.resulttype) or
                    is_ansistring(right.resulttype) or
                    is_char(right.resulttype)) then
             begin
               right:=gentypeconvnode(right,left.resulttype);
               firstpass(right);
               if codegenerror then
                exit;
             end;
            { we call STRCOPY }
            procinfo^.flags:=procinfo^.flags or pi_do_call;
            { test for s:=s+anything ... }
            { the problem is for
              s:=s+s+s;
              this is broken here !! }
{$ifdef newoptimizations2}
            { the above is fixed now, but still problem with s := s + f(); if }
            { f modifies s (bad programming, so only enable if uncertain      }
            { optimizations are on) (JM)                                      }
            if (cs_UncertainOpts in aktglobalswitches) then
              begin
                hp := right;
                while hp.treetype=addn do hp:=hp.left;
                if equal_trees(left,hp) and
                   not multiple_uses(left,right) then
                  begin
                    concat_string:=true;
                    hp:=right;
                    while hp.treetype=addn do
                      begin
                        hp.use_strconcat:=true;
                        hp:=hp.left;
                      end;
                  end;
              end;
{$endif newoptimizations2}
          end
         else
          begin
            right:=gentypeconvnode(right,left.resulttype);
            firstpass(right);
            if codegenerror then
             exit;
          end;

         { test if node can be assigned, properties are allowed }
         valid_for_assign(left,true);

         { check if local proc/func is assigned to procvar }
         if right.resulttype^.deftype=procvardef then
           test_local_to_procvar(pprocvardef(right.resulttype),left.resulttype);

         resulttype:=voiddef;
         {
           registers32:=max(left.registers32,right.registers32);
           registersfpu:=max(left.registersfpu,right.registersfpu);
         }
         registers32:=left.registers32+right.registers32;
         registersfpu:=max(left.registersfpu,right.registersfpu);
{$ifdef SUPPORT_MMX}
         registersmmx:=max(left.registersmmx,right.registersmmx);
{$endif SUPPORT_MMX}
      end;


{*****************************************************************************
                                 TFUNCRETNODE
*****************************************************************************}

    constructor tfuncretnode.create;

      begin
         inherited create(funcretn);
         funcretprocinfo:=nil;
      end;

    function tfuncretnode.getcopy : tnode;

      var
         n : tfuncretnode;

      begin
         n:=tfuncretnode(inherited getcopy);
         n.funcretprocinfo:=funcretprocinfo;
{$ifdef NEWST}
         n.retsym:=retsym;
{$else NEWST}
         n.rettype:=rettype;
{$endif NEWST}
         getcopy:=n;
      end;

    function tfuncretnode.pass_1 : tnode;
      begin
         resulttype:=rettype.def;
         location.loc:=LOC_REFERENCE;
         if ret_in_param(rettype.def) or
            (procinfo<>pprocinfo(funcretprocinfo)) then
           registers32:=1;
      end;


{*****************************************************************************
                           TARRAYCONSTRUCTRANGENODE
*****************************************************************************}

    constructor tarrayconstructorrangenode.create(l,r : tnode);

      begin
         inherited create(arrayconstructn,l,r);
      end;

    function tarrayconstructorrangenode.pass_1 : tnode;
      begin
        firstpass(left);
        left.set_varstate(true);
        firstpass(right);
        right.set_varstate(true);
        calcregisters(self,0,0,0);
        resulttype:=left.resulttype;
      end;


{****************************************************************************
                            TARRAYCONSTRUCTNODE
*****************************************************************************}

    constructor tarrayconstructnode.create(l,r : tnode);

      begin
         inherited create(arrayconstructn,l,r);
         constructdef:=nil;
      end;

    function tarrayconstructnode.getcopy : tnode;

      var
         n : tarrayconstructnode;

      begin
         n:=tarrayconstructnode(inherited getcopy);
         n.constructdef:=constructdef;
      end;

    function tarrayconstructnode.pass_1 : tnode;
      var
        pd : pdef;
        thp,
        chp,
        hp : tarrayconstructnode;
        len : longint;
        varia : boolean;

      procedure postprocess(t : tnode);

        begin
           calcregisters(t,0,0,0);
           { looks a little bit dangerous to me            }
           { len-1 gives problems with is_open_array if len=0, }
           { is_open_array checks now for isconstructor (FK)   }
           { if no type is set then we set the type to voiddef to overcome a
           0 addressing }
           if not assigned(pd) then
             pd:=voiddef;
           { skip if already done ! (PM) }
           if not assigned(t.resulttype) or
              (t.resulttype^.deftype<>arraydef) or
              not parraydef(t.resulttype)^.IsConstructor or
              (parraydef(t.resulttype)^.lowrange<>0) or
              (parraydef(t.resulttype)^.highrange<>len-1) then
             t.resulttype:=new(parraydef,init(0,len-1,s32bitdef));

           parraydef(t.resulttype)^.elementtype.def:=pd;
           parraydef(t.resulttype)^.IsConstructor:=true;
           parraydef(t.resulttype)^.IsVariant:=varia;
           t.location.loc:=LOC_MEM;
        end;
      begin
      { are we allowing array constructor? Then convert it to a set }
        if not allow_array_constructor then
         begin
           hp:=tarrayconstructnode(getcopy);
           arrayconstructor_to_set(hp);
           firstpass(hp);
           pass_1:=hp;
           exit;
         end;
      { only pass left tree, right tree contains next construct if any }
        pd:=constructdef;
        len:=0;
        varia:=false;
        if assigned(left) then
         begin
           hp:=self;
           while assigned(hp) do
            begin
              firstpass(hp.left);
              hp.left.set_varstate(true);
              if (not get_para_resulttype) and
                (not(nf_novariaallowed in flags)) then
               begin
                 case hp.left.resulttype^.deftype of
                   enumdef :
                     begin
                       hp.left:=gentypeconvnode(hp.left,s32bitdef);
                       firstpass(hp.left);
                     end;
                   orddef :
                     begin
                       if is_integer(hp.left.resulttype) and
                         not(is_64bitint(hp.left.resulttype)) then
                        begin
                          hp.left:=gentypeconvnode(hp.left,s32bitdef);
                          firstpass(hp.left);
                        end;
                     end;
                   floatdef :
                     begin
                       hp.left:=gentypeconvnode(hp.left,bestrealdef^);
                       firstpass(hp.left);
                     end;
                   stringdef :
                     begin
                       if nf_cargs in flags then
                        begin
                          hp.left:=gentypeconvnode(hp.left,charpointerdef);
                          firstpass(hp.left);
                        end;
                     end;
                   procvardef :
                     begin
                       hp.left:=gentypeconvnode(hp.left,voidpointerdef);
                       firstpass(hp.left);
                     end;
                   pointerdef,
                   classrefdef,
                   objectdef : ;
                   else
                     CGMessagePos1(hp.left.fileinfo,type_e_wrong_type_in_array_constructor,hp.left.resulttype^.typename);
                 end;
               end;
              if (pd=nil) then
               pd:=hp.left.resulttype
              else
               begin
                 if ((nf_novariaallowed in flags) or (not varia)) and
                    (not is_equal(pd,hp.left.resulttype)) then
                  begin
                    { if both should be equal try inserting a conversion }
                    if nf_novariaallowed in flags then
                     begin
                       hp.left:=gentypeconvnode(hp.left,pd);
                       firstpass(hp.left);
                     end;
                    varia:=true;
                  end;
               end;
              inc(len);
              hp:=tarrayconstructnode(hp.right);
            end;
         { swap the tree for cargs }
           if (nf_cargs in flags) and (not(nf_cargswap in flags)) then
            begin
              chp:=nil;
              { we need a copy here, because self is destroyed }
              { by firstpass later                             }
              hp:=tarrayconstructnode(getcopy);
              while assigned(hp) do
               begin
                 thp:=tarrayconstructnode(hp.right);
                 hp.right:=chp;
                 chp:=hp;
                 hp:=thp;
               end;
              include(chp.flags,nf_cargs);
              include(chp.flags,nf_cargswap);
              postprocess(chp);
              pass_1:=chp;
              exit;
            end;
         end;
         postprocess(self);
      end;


{*****************************************************************************
                              TTYPENODE
*****************************************************************************}

    constructor ttypenode.create(t : pdef;sym:ptypesym);

      begin
         inherited create(typen);
         resulttype:=generrordef;
         typenodetype:=t;
         typenodesym:=sym;
      end;

    function ttypenode.getcopy : tnode;

      var
         n : ttypenode;

      begin
         n:=ttypenode(inherited getcopy);
         n.typenodetype:=typenodetype;
         n.typenodesym:=typenodesym;
      end;

    function ttypenode.pass_1 : tnode;
      begin
         pass_1:=nil;
         { do nothing, resulttype is already set }
      end;


begin
   cloadnode:=tloadnode;
   cassignmentnode:=tassignmentnode;
   cfuncretnode:=tfuncretnode;
   carrayconstructorrangenode:=tarrayconstructorrangenode;
   carrayconstructnode:=tarrayconstructnode;
   ctypenode:=ttypenode;
end.
{
  $Log$
  Revision 1.3  2000-09-27 18:14:31  florian
    * fixed a lot of syntax errors in the n*.pas stuff

  Revision 1.2  2000/09/25 15:37:14  florian
    * more fixes

  Revision 1.1  2000/09/25 14:55:05  florian
    * initial revision
}