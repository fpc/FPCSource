{
    $Id$
    Copyright (c) 1993-98 by Florian Klaempfl

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
              p1^.retdef:=pfuncretsym(p^.symtableentry)^.funcretdef;
              firstpass(p1);
              putnode(p);
              p:=p1;
              exit;
           end;
         if p^.symtableentry^.typ=absolutesym then
           begin
              p^.resulttype:=pabsolutesym(p^.symtableentry)^.definition;
              if pabsolutesym(p^.symtableentry)^.abstyp=tovar then
                p^.symtableentry:=pabsolutesym(p^.symtableentry)^.ref;
              p^.symtable:=p^.symtableentry^.owner;
              p^.is_absolute:=true;
           end;
         case p^.symtableentry^.typ of
            absolutesym :;
            constsym:
              begin
                 if pconstsym(p^.symtableentry)^.consttype=constresourcestring then
                   begin
                      p^.resulttype:=cansistringdef;
                      p^.location.loc:=LOC_MEM;
                   end
                 else
                   internalerror(22799);
              end;
            varsym :
                begin
                   if not(p^.is_absolute) and (p^.resulttype=nil) then
                     p^.resulttype:=pvarsym(p^.symtableentry)^.definition;
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
                      push_addr_param(pvarsym(p^.symtableentry)^.definition)) or
                      { call by value open arrays are also indirect addressed }
                      is_open_array(pvarsym(p^.symtableentry)^.definition) then
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

                   if must_be_valid and p^.is_first then
                     begin
                       if pvarsym(p^.symtableentry)^.varstate=vs_declared2 then
                        if (assigned(pvarsym(p^.symtableentry)^.owner) and
                           assigned(aktprocsym) and
                           (pvarsym(p^.symtableentry)^.owner = aktprocsym^.definition^.localst)) then
                         begin
                           if p^.symtable^.symtabletype=localsymtable then
                            CGMessage1(sym_n_uninitialized_local_variable,pvarsym(p^.symtableentry)^.name)
                           else
                            CGMessage1(sym_n_uninitialized_variable,pvarsym(p^.symtableentry)^.name);
                         end;
                     end;
                   if count_ref then
                     begin
                        if (p^.is_first) then
                          begin
                            if pvarsym(p^.symtableentry)^.varstate=vs_declared2 then
                             pvarsym(p^.symtableentry)^.varstate:=vs_used;
                            p^.is_first:=false;
                          end;
                     end;
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
                  p^.resulttype:=ptypedconstsym(p^.symtableentry)^.definition;
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
         store_valid : boolean;
         hp : ptree;
      begin
         store_valid:=must_be_valid;
         must_be_valid:=false;

         { must be made unique }
         set_unique(p^.left);

         { set we the function result? }
         set_funcret_is_valid(p^.left);

         firstpass(p^.left);
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
         must_be_valid:=true;
         firstpass(p^.right);
         must_be_valid:=store_valid;
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
         p^.resulttype:=p^.retdef;
         p^.location.loc:=LOC_REFERENCE;
         if ret_in_param(p^.retdef) or
            (procinfo<>pprocinfo(p^.funcretprocinfo)) then
           p^.registers32:=1;
         { no claim if setting higher return value_str }
         if must_be_valid and
            (procinfo=pprocinfo(p^.funcretprocinfo)) and
            not procinfo^.funcret_is_valid then
           CGMessage(sym_w_function_result_not_set);
         {
         if count_ref then
           pprocinfo(p^.funcretprocinfo)^.funcret_is_valid:=true;
         }
      end;


{*****************************************************************************
                           FirstArrayConstructRange
*****************************************************************************}

    procedure firstarrayconstructrange(var p:ptree);
      begin
        firstpass(p^.left);
        firstpass(p^.right);
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
      { skip if already done ! (PM) }
        if not assigned(p^.resulttype) or
           (p^.resulttype^.deftype<>arraydef) or
           not parraydef(p^.resulttype)^.IsConstructor or
           (parraydef(p^.resulttype)^.lowrange<>0) or
           (parraydef(p^.resulttype)^.highrange<>len-1) then
          p^.resulttype:=new(parraydef,init(0,len-1,s32bitdef));
        parraydef(p^.resulttype)^.definition:=pd;
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
  Revision 1.48  1999-10-26 12:30:46  peter
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

  Revision 1.29  1999/05/17 23:51:45  peter
    * with temp vars now use a reference with a persistant temp instead
      of setting datasize

  Revision 1.27  1999/05/12 00:20:02  peter
    * removed R_DEFAULT_SEG
    * uniform float names

  Revision 1.26  1999/05/06 09:05:36  peter
    * generic write_float and str_float
    * fixed constant float conversions

  Revision 1.25  1999/05/01 13:24:54  peter
    * merged nasm compiler
    * old asm moved to oldasm/

  Revision 1.24  1999/04/28 06:02:17  florian
    * changes of Bruessel:
       + message handler can now take an explicit self
       * typinfo fixed: sometimes the type names weren't written
       * the type checking for pointer comparisations and subtraction
         and are now more strict (was also buggy)
       * small bug fix to link.pas to support compiling on another
         drive
       * probable bug in popt386 fixed: call/jmp => push/jmp
         transformation didn't count correctly the jmp references
       + threadvar support
       * warning if ln/sqrt gets an invalid constant argument

  Revision 1.23  1999/04/21 21:57:33  pierre
   * previous log corrected

  Revision 1.22  1999/04/21 16:31:47  pierre
  * some wrong code in firstfuncret corrected

  Revision 1.21  1999/04/01 21:59:57  peter
    * type error for array constructor with array,record as argument

  Revision 1.20  1999/03/24 23:17:39  peter
    * fixed bugs 212,222,225,227,229,231,233

  Revision 1.19  1999/03/18 11:21:52  peter
    * convert only to s32bit if integer or enum

  Revision 1.18  1999/03/16 21:02:10  peter
    * all array of const enum/ord are converted to s32bit

  Revision 1.17  1999/03/10 13:24:23  pierre
   * array of const type to definition field

  Revision 1.16  1999/02/22 02:15:52  peter
    * updates for ag386bin

  Revision 1.15  1999/02/15 13:13:19  pierre
   * fix for bug0216

  Revision 1.14  1999/01/27 00:13:58  florian
    * "procedure of object"-stuff fixed

  Revision 1.13  1999/01/21 16:41:07  pierre
   * fix for constructor inside with statements

  Revision 1.12  1998/12/30 13:41:19  peter
    * released valuepara

  Revision 1.11  1998/11/18 17:45:28  peter
    * fixes for VALUEPARA

  Revision 1.10  1998/11/18 15:44:23  peter
    * VALUEPARA for tp7 compatible value parameters

  Revision 1.9  1998/11/17 00:36:49  peter
    * more ansistring fixes

  Revision 1.8  1998/11/10 10:09:18  peter
    * va_list -> array of const

  Revision 1.7  1998/11/05 14:26:48  peter
    * fixed variant warning with was sometimes said with sets

  Revision 1.6  1998/10/19 08:55:12  pierre
    * wrong stabs info corrected once again !!
    + variable vmt offset with vmt field only if required
      implemented now !!!

  Revision 1.5  1998/10/06 20:49:12  peter
    * m68k compiler compiles again

  Revision 1.4  1998/09/28 11:07:40  peter
    + floatdef support for array of const

  Revision 1.3  1998/09/27 10:16:27  florian
    * type casts pchar<->ansistring fixed
    * ansistring[..] calls does now an unique call

  Revision 1.2  1998/09/24 15:13:48  peter
    * fixed type node which was always set to void :(

  Revision 1.1  1998/09/23 20:42:24  peter
    * splitted pass_1

}
