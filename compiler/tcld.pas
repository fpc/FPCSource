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
      cobjects,verbose,globtype,globals,systems,
      symconst,symtable,aasm,types,
      hcodegen,htypechk,pass_1,
      tccnv,cpubase
{$ifdef i386}
      ,tgeni386
{$endif}
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
         if p^.symtableentry^.typ=funcretsym then
           begin
              p1:=genzeronode(funcretn);
              p1^.funcretprocinfo:=pprocinfo(pfuncretsym(p^.symtableentry)^.funcretprocinfo);
              p1^.rettype:=pfuncretsym(p^.symtableentry)^.rettype;
              firstpass(p1);
              putnode(p);
              p:=p1;
              exit;
           end;
         if p^.symtableentry^.typ=absolutesym then
           begin
              p^.resulttype:=pabsolutesym(p^.symtableentry)^.vartype.def;
              if pabsolutesym(p^.symtableentry)^.abstyp=tovar then
                p^.symtableentry:=pabsolutesym(p^.symtableentry)^.ref;
              p^.symtable:=p^.symtableentry^.owner;
              p^.is_absolute:=true;
           end;
         case p^.symtableentry^.typ of
            absolutesym :;
            constsym:
              begin
                 if pconstsym(p^.symtableentry)^.consttyp=constresourcestring then
                   begin
                      p^.resulttype:=cansistringdef;
                      { we use ansistrings so no fast exit here }
                      procinfo^.no_fast_exit:=true;
                      p^.location.loc:=LOC_MEM;
                   end
                 else
                   internalerror(22799);
              end;
            varsym :
                begin
                   if not(p^.is_absolute) and (p^.resulttype=nil) then
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
{$ifdef INCLUDEOK}
                          exclude(pvarsym(p^.symtableentry)^.varoptions,vo_regable);
{$else}
                          pvarsym(p^.symtableentry)^.varoptions:=pvarsym(p^.symtableentry)^.varoptions-[vo_regable];
{$endif}
                        end;
                     end;
                   if (pvarsym(p^.symtableentry)^.varspez=vs_const) then
                     p^.location.loc:=LOC_MEM;
                   { we need a register for call by reference parameters }
                   if (pvarsym(p^.symtableentry)^.varspez=vs_var) or
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
                      p^.left:=genselfnode(procinfo^._class);
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
            else internalerror(3);
         end;
      end;


{*****************************************************************************
                             FirstAssignment
*****************************************************************************}

    procedure firstassignment(var p : ptree);
      var
         hp : ptree;
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
            hp:=p^.right;
            { test for s:=s+anything ... }
            { the problem is for
              s:=s+s+s;
              this is broken here !! }
            { while hp^.treetype=addn do hp:=hp^.left;
            if equal_trees(p^.left,hp) then
              begin
                p^.concat_string:=true;
                hp:=p^.right;
                while hp^.treetype=addn do
                  begin
                    hp^.use_strconcat:=true;
                    hp:=hp^.left;
                  end;
              end; }
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
                       if is_integer(hp^.left^.resulttype) then
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
  Revision 1.57  2000-01-07 01:14:46  peter
    * updated copyright to 2000

  Revision 1.56  2000/01/06 01:08:59  pierre
   * fix for web bug 776

  Revision 1.55  1999/12/31 14:26:27  peter
    * fixed crash with empty array constructors

  Revision 1.54  1999/12/09 23:18:05  pierre
   * no_fast_exit if procedure contains implicit termination code

  Revision 1.53  1999/12/02 17:28:53  peter
    * fixed procvar -> pointer for array of const

  Revision 1.52  1999/11/30 10:40:58  peter
    + ttype, tsymlist

  Revision 1.51  1999/11/18 15:34:50  pierre
    * Notes/Hints for local syms changed to
      Set_varstate function

  Revision 1.50  1999/11/17 17:05:07  pierre
   * Notes/hints changes

  Revision 1.49  1999/11/06 14:34:30  peter
    * truncated log to 20 revs

  Revision 1.48  1999/10/26 12:30:46  peter
    * const parameter is now checked
    * better and generic check if a node can be used for assigning
    * export fixes
    * procvar equal works now (it never had worked at least from 0.99.8)
    * defcoll changed to linkedlist with pparaitem so it can easily be
      walked both directions

  Revision 1.47  1999/10/13 10:35:27  peter
    * var must match exactly error msg extended with got and expected type
    * array constructor type check now gives error on wrong types

  Revision 1.46  1999/09/27 23:45:01  peter
    * procinfo is now a pointer
    * support for result setting in sub procedure

  Revision 1.45  1999/09/17 17:14:12  peter
    * @procvar fixes for tp mode
    * @<id>:= gives now an error

  Revision 1.44  1999/09/11 19:47:26  florian
    * bug fix for @tobject.method, fixes bug 557, 605 and 606

  Revision 1.43  1999/09/11 09:08:34  florian
    * fixed bug 596
    * fixed some problems with procedure variables and procedures of object,
      especially in TP mode. Procedure of object doesn't apply only to classes,
      it is also allowed for objects !!

  Revision 1.42  1999/09/10 18:48:11  florian
    * some bug fixes (e.g. must_be_valid and procinfo^.funcret_is_valid)
    * most things for stored properties fixed

  Revision 1.41  1999/08/16 23:23:41  peter
    * arrayconstructor -> openarray type conversions for element types

  Revision 1.40  1999/08/13 21:33:17  peter
    * support for array constructors extended and more error checking

  Revision 1.39  1999/08/05 16:53:24  peter
    * V_Fatal=1, all other V_ are also increased
    * Check for local procedure when assigning procvar
    * fixed comment parsing because directives
    * oldtp mode directives better supported
    * added some messages to errore.msg

  Revision 1.38  1999/08/04 00:23:41  florian
    * renamed i386asm and i386base to cpuasm and cpubase

  Revision 1.37  1999/08/03 22:03:33  peter
    * moved bitmask constants to sets
    * some other type/const renamings

  Revision 1.36  1999/07/22 09:38:00  florian
    + resourcestring implemented
    + start of longstring support

  Revision 1.35  1999/06/17 13:19:59  pierre
   * merged from 0_99_12 branch

  Revision 1.34.2.1  1999/06/17 12:33:39  pierre
   * avoid warning with extdebug for arrayconstruct

  Revision 1.34  1999/06/01 19:26:39  peter
    * fixed bug 249

  Revision 1.33  1999/05/27 19:45:21  peter
    * removed oldasm
    * plabel -> pasmlabel
    * -a switches to source writing automaticly
    * assembler readers OOPed
    * asmsymbol automaticly external
    * jumptables and other label fixes for asm readers

  Revision 1.32  1999/05/23 18:42:22  florian
    * better error recovering in typed constants
    * some problems with arrays of const fixed, some problems
      due my previous
       - the location type of array constructor is now LOC_MEM
       - the pushing of high fixed
       - parameter copying fixed
       - zero temp. allocation removed
    * small problem in the assembler writers fixed:
      ref to nil wasn't written correctly

  Revision 1.31  1999/05/19 15:26:41  florian
    * if a non local variables isn't initialized the compiler doesn't write
      any longer "local var. seems not to be ..."

  Revision 1.30  1999/05/19 10:31:55  florian
    * two bugs reported by Romio (bugs 13) are fixed:
        - empty array constructors are now handled correctly (e.g. for sysutils.format)
        - comparsion of ansistrings was sometimes coded wrong

}