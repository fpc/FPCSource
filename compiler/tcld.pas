{
    $Id$
    Copyright (c) 1998-2000 by Florian Klaempfl

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
unit tcld;

{$i defines.inc}

interface

    uses
      tree;

    procedure firstload(var p : ptree);
    procedure firstassignment(var p : ptree);
    procedure firstfuncret(var p : ptree);
    procedure firstarrayconstructrange(var p:ptree);
    procedure firstarrayconstruct(var p : ptree);
    procedure firsttype(var p : ptree);


implementation

    uses
      cutils,cobjects,verbose,globtype,globals,systems,
      symconst,symtable,aasm,types,
      htypechk,pass_1,
      tccnv,cpubase
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

{*****************************************************************************
                               FirstLoad
*****************************************************************************}

    procedure firstload(var p : ptree);
      var
         p1 : ptree;
      begin
         if (p^.symtable^.symtabletype=withsymtable) and
            (pwithsymtable(p^.symtable)^.direct_with) and
            (p^.symtableentry^.typ=varsym) then
           begin
              p1:=getcopy(ptree(pwithsymtable(p^.symtable)^.withrefnode));
              p1:=gensubscriptnode(pvarsym(p^.symtableentry),p1);
              putnode(p);
              p:=p1;
              firstpass(p);
              exit;
           end;

         p^.location.loc:=LOC_REFERENCE;
         p^.registers32:=0;
         p^.registersfpu:=0;
{$ifdef SUPPORT_MMX}
         p^.registersmmx:=0;
{$endif SUPPORT_MMX}
         { handle first absolute as it will replace the p^.symtableentry }
         if p^.symtableentry^.typ=absolutesym then
           begin
             p^.resulttype:=pabsolutesym(p^.symtableentry)^.vartype.def;
             { replace the symtableentry when it points to a var, else
               we are finished }
             if pabsolutesym(p^.symtableentry)^.abstyp=tovar then
              begin
                p^.symtableentry:=pabsolutesym(p^.symtableentry)^.ref;
                p^.symtable:=p^.symtableentry^.owner;
                p^.is_absolute:=true;
              end
             else
              exit;
           end;
         case p^.symtableentry^.typ of
            funcretsym :
              begin
                p1:=genzeronode(funcretn);
                p1^.funcretprocinfo:=pprocinfo(pfuncretsym(p^.symtableentry)^.funcretprocinfo);
                p1^.rettype:=pfuncretsym(p^.symtableentry)^.rettype;
                firstpass(p1);
                { if it's refered as absolute then we need to have the
                  type of the absolute instead of the function return,
                  the function return is then also assigned }
                if p^.is_absolute then
                 begin
                   pprocinfo(p1^.funcretprocinfo)^.funcret_state:=vs_assigned;
                   p1^.resulttype:=p^.resulttype;
                 end;
                putnode(p);
                p:=p1;
              end;
            constsym:
              begin
                 if pconstsym(p^.symtableentry)^.consttyp=constresourcestring then
                   begin
                      p^.resulttype:=cansistringdef;
                      { we use ansistrings so no fast exit here }
                      if assigned(procinfo) then
                        procinfo^.no_fast_exit:=true;
                      p^.location.loc:=LOC_MEM;
                   end
                 else
                   internalerror(22799);
              end;
            varsym :
                begin
                { if it's refered by absolute then it's used }
                if p^.is_absolute then
                 pvarsym(p^.symtableentry)^.varstate:=vs_used
                else
                 if (p^.resulttype=nil) then
                     p^.resulttype:=pvarsym(p^.symtableentry)^.vartype.def;
                   if (p^.symtable^.symtabletype in [parasymtable,localsymtable]) and
                      (lexlevel>p^.symtable^.symtablelevel) then
                     begin
                       { if the variable is in an other stackframe then we need
                         a register to dereference }
                       if (p^.symtable^.symtablelevel)>0 then
                        begin
                          p^.registers32:=1;
                          { further, the variable can't be put into a register }
                          pvarsym(p^.symtableentry)^.varoptions:=
                            pvarsym(p^.symtableentry)^.varoptions-[vo_fpuregable,vo_regable];
                        end;
                     end;
                   if (pvarsym(p^.symtableentry)^.varspez=vs_const) then
                     p^.location.loc:=LOC_MEM;
                   { we need a register for call by reference parameters }
                   if (pvarsym(p^.symtableentry)^.varspez in [vs_var,vs_out]) or
                      ((pvarsym(p^.symtableentry)^.varspez=vs_const) and
                      push_addr_param(pvarsym(p^.symtableentry)^.vartype.def)) or
                      { call by value open arrays are also indirect addressed }
                      is_open_array(pvarsym(p^.symtableentry)^.vartype.def) then
                     p^.registers32:=1;
                   if p^.symtable^.symtabletype=withsymtable then
                     inc(p^.registers32);

                   if ([vo_is_thread_var,vo_is_dll_var]*pvarsym(p^.symtableentry)^.varoptions)<>[] then
                     p^.registers32:=1;
                   { a class variable is a pointer !!!
                     yes, but we have to resolve the reference in an
                     appropriate tree node (FK)

                   if (pvarsym(p^.symtableentry)^.definition^.deftype=objectdef) and
                      ((pobjectdef(pvarsym(p^.symtableentry)^.definition)^.options and oo_is_class)<>0) then
                     p^.registers32:=1;
                   }

                   { count variable references }

                     { this will create problem with local var set by
                     under_procedures
                     if (assigned(pvarsym(p^.symtableentry)^.owner) and assigned(aktprocsym)
                       and ((pvarsym(p^.symtableentry)^.owner = aktprocsym^.definition^.localst)
                       or (pvarsym(p^.symtableentry)^.owner = aktprocsym^.definition^.localst))) then }
                   if t_times<1 then
                     inc(pvarsym(p^.symtableentry)^.refs)
                   else
                     inc(pvarsym(p^.symtableentry)^.refs,t_times);
                end;
            typedconstsym :
                if not p^.is_absolute then
                  p^.resulttype:=ptypedconstsym(p^.symtableentry)^.typedconsttype.def;
            procsym :
                begin
                   if assigned(pprocsym(p^.symtableentry)^.definition^.nextoverloaded) then
                     CGMessage(parser_e_no_overloaded_procvars);
                   p^.resulttype:=pprocsym(p^.symtableentry)^.definition;
                   { if the owner of the procsym is a object,  }
                   { left must be set, if left isn't set       }
                   { it can be only self                       }
                   { this code is only used in TP procvar mode }
                   if (m_tp_procvar in aktmodeswitches) and
                      not(assigned(p^.left)) and
                     (pprocsym(p^.symtableentry)^.owner^.symtabletype=objectsymtable) then
                      p^.left:=genselfnode(pobjectdef(p^.symtableentry^.owner^.defowner));
                   { method pointer ? }
                   if assigned(p^.left) then
                     begin
                        firstpass(p^.left);
                        p^.registers32:=max(p^.registers32,p^.left^.registers32);
                        p^.registersfpu:=max(p^.registersfpu,p^.left^.registersfpu);
 {$ifdef SUPPORT_MMX}
                        p^.registersmmx:=max(p^.registersmmx,p^.left^.registersmmx);
 {$endif SUPPORT_MMX}
                     end;
                end;
           else
             internalerror(3);
         end;
      end;


{*****************************************************************************
                             FirstAssignment
*****************************************************************************}

    procedure firstassignment(var p : ptree);
{$ifdef newoptimizations2}
      var
        hp : ptree;
{$endif newoptimizations2}
      begin
         { must be made unique }
         set_unique(p^.left);

         { set we the function result? }
         set_funcret_is_valid(p^.left);

         firstpass(p^.left);
         set_varstate(p^.left,false);
         if codegenerror then
           exit;

         { assignements to open arrays aren't allowed }
         if is_open_array(p^.left^.resulttype) then
           CGMessage(type_e_mismatch);

         { test if we can avoid copying string to temp
           as in s:=s+...; (PM) }
{$ifdef dummyi386}
         if ((p^.right^.treetype=addn) or (p^.right^.treetype=subn)) and
            equal_trees(p^.left,p^.right^.left) and
            (ret_in_acc(p^.left^.resulttype)) and
            (not cs_rangechecking in aktmoduleswitches^) then
           begin
              disposetree(p^.right^.left);
              hp:=p^.right;
              p^.right:=p^.right^.right;
              if hp^.treetype=addn then
                p^.assigntyp:=at_plus
              else
                p^.assigntyp:=at_minus;
              putnode(hp);
           end;
         if p^.assigntyp<>at_normal then
           begin
              { for fpu type there is no faster way }
              if is_fpu(p^.left^.resulttype) then
                case p^.assigntyp of
                  at_plus  : p^.right:=gennode(addn,getcopy(p^.left),p^.right);
                  at_minus : p^.right:=gennode(subn,getcopy(p^.left),p^.right);
                  at_star  : p^.right:=gennode(muln,getcopy(p^.left),p^.right);
                  at_slash : p^.right:=gennode(slashn,getcopy(p^.left),p^.right);
                end;
           end;
{$endif i386}
         firstpass(p^.right);
         set_varstate(p^.right,true);
         if codegenerror then
           exit;

         { some string functions don't need conversion, so treat them separatly }
         if is_shortstring(p^.left^.resulttype) and (assigned(p^.right^.resulttype)) then
          begin
            if not (is_shortstring(p^.right^.resulttype) or
                    is_ansistring(p^.right^.resulttype) or
                    is_char(p^.right^.resulttype)) then
             begin
               p^.right:=gentypeconvnode(p^.right,p^.left^.resulttype);
               firstpass(p^.right);
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
                hp := p^.right;
                while hp^.treetype=addn do hp:=hp^.left;
                if equal_trees(p^.left,hp) and
                   not multiple_uses(p^.left,p^.right) then
                  begin
                    p^.concat_string:=true;
                    hp:=p^.right;
                    while hp^.treetype=addn do
                      begin
                        hp^.use_strconcat:=true;
                        hp:=hp^.left;
                      end;
                  end;
              end;
{$endif newoptimizations2}
          end
         else
          begin
            p^.right:=gentypeconvnode(p^.right,p^.left^.resulttype);
            firstpass(p^.right);
            if codegenerror then
             exit;
          end;

         { test if node can be assigned, properties are allowed }
         valid_for_assign(p^.left,true);

         { check if local proc/func is assigned to procvar }
         if p^.right^.resulttype^.deftype=procvardef then
           test_local_to_procvar(pprocvardef(p^.right^.resulttype),p^.left^.resulttype);

         p^.resulttype:=voiddef;
         {
           p^.registers32:=max(p^.left^.registers32,p^.right^.registers32);
           p^.registersfpu:=max(p^.left^.registersfpu,p^.right^.registersfpu);
         }
         p^.registers32:=p^.left^.registers32+p^.right^.registers32;
         p^.registersfpu:=max(p^.left^.registersfpu,p^.right^.registersfpu);
{$ifdef SUPPORT_MMX}
         p^.registersmmx:=max(p^.left^.registersmmx,p^.right^.registersmmx);
{$endif SUPPORT_MMX}
      end;


{*****************************************************************************
                             FirstFuncRet
*****************************************************************************}

    procedure firstfuncret(var p : ptree);
      begin
         p^.resulttype:=p^.rettype.def;
         p^.location.loc:=LOC_REFERENCE;
         if ret_in_param(p^.rettype.def) or
            (procinfo<>pprocinfo(p^.funcretprocinfo)) then
           p^.registers32:=1;
      end;


{*****************************************************************************
                           FirstArrayConstructRange
*****************************************************************************}

    procedure firstarrayconstructrange(var p:ptree);
      begin
        firstpass(p^.left);
        set_varstate(p^.left,true);
        firstpass(p^.right);
        set_varstate(p^.right,true);
        calcregisters(p,0,0,0);
        p^.resulttype:=p^.left^.resulttype;
      end;


{*****************************************************************************
                           FirstArrayConstruct
*****************************************************************************}

    procedure firstarrayconstruct(var p : ptree);
      var
        pd : pdef;
        thp,
        chp,
        hp : ptree;
        len : longint;
        varia : boolean;
      begin
      { are we allowing array constructor? Then convert it to a set }
        if not allow_array_constructor then
         begin
           arrayconstructor_to_set(p);
           firstpass(p);
           exit;
         end;
      { only pass left tree, right tree contains next construct if any }
        pd:=p^.constructdef;
        len:=0;
        varia:=false;
        if assigned(p^.left) then
         begin
           hp:=p;
           while assigned(hp) do
            begin
              firstpass(hp^.left);
              set_varstate(hp^.left,true);
              if (not get_para_resulttype) and (not p^.novariaallowed) then
               begin
                 case hp^.left^.resulttype^.deftype of
                   enumdef :
                     begin
                       hp^.left:=gentypeconvnode(hp^.left,s32bitdef);
                       firstpass(hp^.left);
                     end;
                   orddef :
                     begin
                       if is_integer(hp^.left^.resulttype) and
                         not(is_64bitint(hp^.left^.resulttype)) then
                        begin
                          hp^.left:=gentypeconvnode(hp^.left,s32bitdef);
                          firstpass(hp^.left);
                        end;
                     end;
                   floatdef :
                     begin
                       hp^.left:=gentypeconvnode(hp^.left,bestrealdef^);
                       firstpass(hp^.left);
                     end;
                   stringdef :
                     begin
                       if p^.cargs then
                        begin
                          hp^.left:=gentypeconvnode(hp^.left,charpointerdef);
                          firstpass(hp^.left);
                        end;
                     end;
                   procvardef :
                     begin
                       hp^.left:=gentypeconvnode(hp^.left,voidpointerdef);
                       firstpass(hp^.left);
                     end;
                   pointerdef,
                   classrefdef,
                   objectdef : ;
                   else
                     CGMessagePos1(hp^.left^.fileinfo,type_e_wrong_type_in_array_constructor,hp^.left^.resulttype^.typename);
                 end;
               end;
              if (pd=nil) then
               pd:=hp^.left^.resulttype
              else
               begin
                 if ((p^.novariaallowed) or (not varia)) and
                    (not is_equal(pd,hp^.left^.resulttype)) then
                  begin
                    { if both should be equal try inserting a conversion }
                    if p^.novariaallowed then
                     begin
                       hp^.left:=gentypeconvnode(hp^.left,pd);
                       firstpass(hp^.left);
                     end;
                    varia:=true;
                  end;
               end;
              inc(len);
              hp:=hp^.right;
            end;
         { swap the tree for cargs }
           if p^.cargs and (not p^.cargswap) then
            begin
              chp:=nil;
              hp:=p;
              while assigned(hp) do
               begin
                 thp:=hp^.right;
                 hp^.right:=chp;
                 chp:=hp;
                 hp:=thp;
               end;
              p:=chp;
              p^.cargs:=true;
              p^.cargswap:=true;
            end;
         end;
        calcregisters(p,0,0,0);
        { looks a little bit dangerous to me            }
        { len-1 gives problems with is_open_array if len=0, }
        { is_open_array checks now for isconstructor (FK)   }
      { if no type is set then we set the type to voiddef to overcome a
        0 addressing }
        if not assigned(pd) then
         pd:=voiddef;
      { skip if already done ! (PM) }
        if not assigned(p^.resulttype) or
           (p^.resulttype^.deftype<>arraydef) or
           not parraydef(p^.resulttype)^.IsConstructor or
           (parraydef(p^.resulttype)^.lowrange<>0) or
           (parraydef(p^.resulttype)^.highrange<>len-1) then
          p^.resulttype:=new(parraydef,init(0,len-1,s32bitdef));
        parraydef(p^.resulttype)^.elementtype.def:=pd;
        parraydef(p^.resulttype)^.IsConstructor:=true;
        parraydef(p^.resulttype)^.IsVariant:=varia;
        p^.location.loc:=LOC_MEM;
      end;


{*****************************************************************************
                                 Type
*****************************************************************************}

    procedure firsttype(var p : ptree);
      begin
      { do nothing, p^.resulttype is already set }
      end;



end.
{
  $Log$
  Revision 1.8  2000-09-24 21:19:53  peter
    * delphi compile fixes

  Revision 1.7  2000/08/27 16:11:55  peter
    * moved some util functions from globals,cobjects to cutils
    * splitted files into finput,fmodule

  Revision 1.6  2000/08/15 03:41:27  peter
    * previous commit was wrong file :(

  Revision 1.5  2000/08/13 19:21:13  peter
    * fix for absolute to mem address (merged)

  Revision 1.4  2000/08/13 08:42:59  peter
    * support absolute refering to funcret (merged)

  Revision 1.3  2000/07/13 12:08:28  michael
  + patched to 1.1.0 with former 1.09patch from peter

  Revision 1.2  2000/07/13 11:32:52  michael
  + removed logs

}
