{
    $Id$
    Copyright (c) 1998-2000 by Florian Klaempfl

    Type checking and register allocation for memory related nodes

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
unit tcmem;
interface

    uses
      tree;

    procedure firstloadvmt(var p : ptree);
    procedure firsthnew(var p : ptree);
    procedure firstnew(var p : ptree);
    procedure firsthdispose(var p : ptree);
    procedure firstsimplenewdispose(var p : ptree);
    procedure firstaddr(var p : ptree);
    procedure firstdoubleaddr(var p : ptree);
    procedure firstderef(var p : ptree);
    procedure firstsubscript(var p : ptree);
    procedure firstvec(var p : ptree);
    procedure firstself(var p : ptree);
    procedure firstwith(var p : ptree);


implementation

    uses
      globtype,systems,
      cobjects,verbose,globals,
      symconst,symtable,aasm,types,
      htypechk,pass_1,cpubase
{$ifdef newcg}
      ,cgbase
{$else newcg}
      ,hcodegen
{$endif newcg}
      ;
{*****************************************************************************
                            FirstLoadVMT
*****************************************************************************}

    procedure firstloadvmt(var p : ptree);
      begin
         p^.registers32:=1;
         p^.location.loc:=LOC_REGISTER;
      end;


{*****************************************************************************
                             FirstHNew
*****************************************************************************}

    procedure firsthnew(var p : ptree);
      begin
      end;


{*****************************************************************************
                             FirstNewN
*****************************************************************************}

    procedure firstnew(var p : ptree);
      begin
         { Standardeinleitung }
         if assigned(p^.left) then
           firstpass(p^.left);

         if codegenerror then
           exit;
         if assigned(p^.left) then
           begin
              p^.registers32:=p^.left^.registers32;
              p^.registersfpu:=p^.left^.registersfpu;
{$ifdef SUPPORT_MMX}
              p^.registersmmx:=p^.left^.registersmmx;
{$endif SUPPORT_MMX}
           end;
         { result type is already set }
         procinfo^.flags:=procinfo^.flags or pi_do_call;
         if assigned(p^.left) then
           p^.location.loc:=LOC_REGISTER
         else
           p^.location.loc:=LOC_REFERENCE;
      end;


{*****************************************************************************
                            FirstDispose
*****************************************************************************}

    procedure firsthdispose(var p : ptree);
      begin
         firstpass(p^.left);

         if codegenerror then
           exit;

         p^.registers32:=p^.left^.registers32;
         p^.registersfpu:=p^.left^.registersfpu;
{$ifdef SUPPORT_MMX}
         p^.registersmmx:=p^.left^.registersmmx;
{$endif SUPPORT_MMX}
         if p^.registers32<1 then
           p^.registers32:=1;
         {
         if p^.left^.location.loc<>LOC_REFERENCE then
           CGMessage(cg_e_illegal_expression);
         }
         if p^.left^.location.loc=LOC_CREGISTER then
           inc(p^.registers32);
         p^.location.loc:=LOC_REFERENCE;
         p^.resulttype:=ppointerdef(p^.left^.resulttype)^.pointertype.def;
      end;


{*****************************************************************************
                        FirstSimpleNewDispose
*****************************************************************************}

    procedure firstsimplenewdispose(var p : ptree);
      begin
         { this cannot be in a register !! }
         make_not_regable(p^.left);

         firstpass(p^.left);
         if codegenerror then
          exit;

         { check the type }
         if p^.left^.resulttype=nil then
          p^.left^.resulttype:=generrordef;
         if (p^.left^.resulttype^.deftype<>pointerdef) then
           CGMessage1(type_e_pointer_type_expected,p^.left^.resulttype^.typename);

         if (p^.left^.location.loc<>LOC_REFERENCE) {and
            (p^.left^.location.loc<>LOC_CREGISTER)} then
           CGMessage(cg_e_illegal_expression);

         p^.registers32:=p^.left^.registers32;
         p^.registersfpu:=p^.left^.registersfpu;
{$ifdef SUPPORT_MMX}
         p^.registersmmx:=p^.left^.registersmmx;
{$endif SUPPORT_MMX}
         p^.resulttype:=voiddef;
         procinfo^.flags:=procinfo^.flags or pi_do_call;
      end;


{*****************************************************************************
                             FirstAddr
*****************************************************************************}

    procedure firstaddr(var p : ptree);
      var
         hp  : ptree;
         hp2 : pparaitem;
         hp3 : pabstractprocdef;
      begin
         make_not_regable(p^.left);
         if not(assigned(p^.resulttype)) then
           begin
              { tp @procvar support (type of @procvar is a void pointer)
                Note: we need to leave the addrn in the tree,
                else we can't see the difference between @procvar and procvar.
                we set the procvarload flag so a secondpass does nothing for
                this node (PFV) }
              if (m_tp_procvar in aktmodeswitches) then
               begin
                 hp:=p^.left;
                 case hp^.treetype of
                   calln :
                     begin
                       { is it a procvar? }
                       hp:=hp^.right;
                       if assigned(hp) then
                         begin
                           { remove calln node }
                           putnode(p^.left);
                           p^.left:=hp;
                           firstpass(hp);
                           p^.procvarload:=true;
                         end;
                     end;
                   loadn,
                   subscriptn,
                   typeconvn,
                   vecn,
                   derefn :
                     begin
                       firstpass(hp);
                       if codegenerror then
                        exit;
                       if hp^.resulttype^.deftype=procvardef then
                        begin
                          p^.procvarload:=true;
                        end;
                     end;
                 end;
               end;
              if p^.procvarload then
               begin
                 p^.registers32:=p^.left^.registers32;
                 p^.registersfpu:=p^.left^.registersfpu;
{$ifdef SUPPORT_MMX}
                 p^.registersmmx:=p^.left^.registersmmx;
{$endif SUPPORT_MMX}
                 if p^.registers32<1 then
                   p^.registers32:=1;
                 p^.location.loc:=p^.left^.location.loc;
                 p^.resulttype:=voidpointerdef;
                 exit;
               end;

              { proc 2 procvar ? }
              if p^.left^.treetype=calln then
                begin
                  { generate a methodcallnode or proccallnode }
                  { we shouldn't convert things like @tcollection.load }
                  if (p^.left^.symtableprocentry^.owner^.symtabletype=objectsymtable) and
                    not(assigned(p^.left^.methodpointer) and (p^.left^.methodpointer^.treetype=typen)) then
                   begin
                     hp:=genloadmethodcallnode(pprocsym(p^.left^.symtableprocentry),p^.left^.symtableproc,
                       getcopy(p^.left^.methodpointer));
                     disposetree(p);
                     firstpass(hp);
                     p:=hp;
                     exit;
                   end
                  else
                   hp:=genloadcallnode(pprocsym(p^.left^.symtableprocentry),p^.left^.symtableproc);

                  { result is a procedure variable }
                  { No, to be TP compatible, you must return a pointer to
                    the procedure that is stored in the procvar.}
                  if not(m_tp_procvar in aktmodeswitches) then
                    begin
                       p^.resulttype:=new(pprocvardef,init);

                    { it could also be a procvar, not only pprocsym ! }
                       if p^.left^.symtableprocentry^.typ=varsym then
                        hp3:=pabstractprocdef(pvarsym(p^.left^.symtableentry)^.vartype.def)
                       else
                        hp3:=pabstractprocdef(pprocsym(p^.left^.symtableprocentry)^.definition);

                       pprocvardef(p^.resulttype)^.proctypeoption:=hp3^.proctypeoption;
                       pprocvardef(p^.resulttype)^.proccalloptions:=hp3^.proccalloptions;
                       pprocvardef(p^.resulttype)^.procoptions:=hp3^.procoptions;
                       pprocvardef(p^.resulttype)^.rettype:=hp3^.rettype;
                       pprocvardef(p^.resulttype)^.symtablelevel:=hp3^.symtablelevel;

                     { method ? then set the methodpointer flag }
                       if (hp3^.owner^.symtabletype=objectsymtable) and
                          (pobjectdef(hp3^.owner^.defowner)^.is_class) then
{$ifdef INCLUDEOK}
                         include(pprocvardef(p^.resulttype)^.procoptions,po_methodpointer);
{$else}
                         pprocvardef(p^.resulttype)^.procoptions:=pprocvardef(p^.resulttype)^.procoptions+[po_methodpointer];
{$endif}
                       { we need to process the parameters reverse so they are inserted
                         in the correct right2left order (PFV) }
                       hp2:=pparaitem(hp3^.para^.last);
                       while assigned(hp2) do
                         begin
                            pprocvardef(p^.resulttype)^.concatpara(hp2^.paratype,hp2^.paratyp);
                            hp2:=pparaitem(hp2^.previous);
                         end;
                    end
                  else
                    p^.resulttype:=voidpointerdef;

                  disposetree(p^.left);
                  p^.left:=hp;
                end
              else
                begin
                  firstpass(p^.left);
                  { what are we getting the address from an absolute sym? }
                  hp:=p^.left;
                  while assigned(hp) and (hp^.treetype in [vecn,derefn,subscriptn]) do
                   hp:=hp^.left;
                  if assigned(hp) and (hp^.treetype=loadn) and
                     ((hp^.symtableentry^.typ=absolutesym) and
                      pabsolutesym(hp^.symtableentry)^.absseg) then
                   begin
                     if not(cs_typed_addresses in aktlocalswitches) then
                       p^.resulttype:=voidfarpointerdef
                     else
                       p^.resulttype:=new(ppointerdef,initfardef(p^.left^.resulttype));
                   end
                  else
                   begin
                     if not(cs_typed_addresses in aktlocalswitches) then
                       p^.resulttype:=voidpointerdef
                     else
                       p^.resulttype:=new(ppointerdef,initdef(p^.left^.resulttype));
                   end;
                end;
           end;
         firstpass(p^.left);
         { this is like the function addr }
         inc(parsing_para_level);
         set_varstate(p^.left,false);
         dec(parsing_para_level);
         if codegenerror then
           exit;

         { don't allow constants }
         if is_constnode(p^.left) then
          begin
            aktfilepos:=p^.left^.fileinfo;
            CGMessage(type_e_no_addr_of_constant);
          end
         else
           begin
             { we should allow loc_mem for @string }
             if not(p^.left^.location.loc in [LOC_MEM,LOC_REFERENCE]) then
               begin
                 aktfilepos:=p^.left^.fileinfo;
                 CGMessage(cg_e_illegal_expression);
               end;
           end;

         p^.registers32:=p^.left^.registers32;
         p^.registersfpu:=p^.left^.registersfpu;
{$ifdef SUPPORT_MMX}
         p^.registersmmx:=p^.left^.registersmmx;
{$endif SUPPORT_MMX}
         if p^.registers32<1 then
           p^.registers32:=1;
         { is this right for object of methods ?? }
         p^.location.loc:=LOC_REGISTER;
      end;


{*****************************************************************************
                           FirstDoubleAddr
*****************************************************************************}

    procedure firstdoubleaddr(var p : ptree);
      begin
         make_not_regable(p^.left);
         firstpass(p^.left);
         inc(parsing_para_level);
         set_varstate(p^.left,false);
         dec(parsing_para_level);
         if p^.resulttype=nil then
           p^.resulttype:=voidpointerdef;
         if codegenerror then
           exit;

         if (p^.left^.resulttype^.deftype)<>procvardef then
           CGMessage(cg_e_illegal_expression);

         if (p^.left^.location.loc<>LOC_REFERENCE) then
           CGMessage(cg_e_illegal_expression);

         p^.registers32:=p^.left^.registers32;
         p^.registersfpu:=p^.left^.registersfpu;
{$ifdef SUPPORT_MMX}
         p^.registersmmx:=p^.left^.registersmmx;
{$endif SUPPORT_MMX}
         if p^.registers32<1 then
           p^.registers32:=1;
         p^.location.loc:=LOC_REGISTER;
      end;


{*****************************************************************************
                             FirstDeRef
*****************************************************************************}

    procedure firstderef(var p : ptree);
      begin
         firstpass(p^.left);
         set_varstate(p^.left,true);
         if codegenerror then
           begin
             p^.resulttype:=generrordef;
             exit;
           end;

         p^.registers32:=max(p^.left^.registers32,1);
         p^.registersfpu:=p^.left^.registersfpu;
{$ifdef SUPPORT_MMX}
         p^.registersmmx:=p^.left^.registersmmx;
{$endif SUPPORT_MMX}

         if p^.left^.resulttype^.deftype<>pointerdef then
          CGMessage(cg_e_invalid_qualifier);

         p^.resulttype:=ppointerdef(p^.left^.resulttype)^.pointertype.def;
         p^.location.loc:=LOC_REFERENCE;
      end;


{*****************************************************************************
                            FirstSubScript
*****************************************************************************}

    procedure firstsubscript(var p : ptree);
      begin
         firstpass(p^.left);
         if codegenerror then
           begin
             p^.resulttype:=generrordef;
             exit;
           end;
         p^.resulttype:=p^.vs^.vartype.def;

         p^.registers32:=p^.left^.registers32;
         p^.registersfpu:=p^.left^.registersfpu;
{$ifdef SUPPORT_MMX}
         p^.registersmmx:=p^.left^.registersmmx;
{$endif SUPPORT_MMX}
         { classes must be dereferenced implicit }
         if (p^.left^.resulttype^.deftype=objectdef) and
           pobjectdef(p^.left^.resulttype)^.is_class then
           begin
              if p^.registers32=0 then
                p^.registers32:=1;
              p^.location.loc:=LOC_REFERENCE;
           end
         else
           begin
              if (p^.left^.location.loc<>LOC_MEM) and
                (p^.left^.location.loc<>LOC_REFERENCE) then
                CGMessage(cg_e_illegal_expression);
              set_location(p^.location,p^.left^.location);
           end;
      end;


{*****************************************************************************
                               FirstVec
*****************************************************************************}

    procedure firstvec(var p : ptree);
      var
         harr : pdef;
         ct : tconverttype;
{$ifdef consteval}
         tcsym : ptypedconstsym;
{$endif}
      begin
         firstpass(p^.left);
         firstpass(p^.right);
         if codegenerror then
           exit;

         { range check only for arrays }
         if (p^.left^.resulttype^.deftype=arraydef) then
           begin
              if (isconvertable(p^.right^.resulttype,parraydef(p^.left^.resulttype)^.rangetype.def,
                    ct,ordconstn,false)=0) and
                 not(is_equal(p^.right^.resulttype,parraydef(p^.left^.resulttype)^.rangetype.def)) then
                CGMessage(type_e_mismatch);
           end;
         { Never convert a boolean or a char !}
         { maybe type conversion }
         if (p^.right^.resulttype^.deftype<>enumdef) and
            not(is_char(p^.right^.resulttype)) and
            not(is_boolean(p^.right^.resulttype)) then
           begin
             p^.right:=gentypeconvnode(p^.right,s32bitdef);
             firstpass(p^.right);
             if codegenerror then
              exit;
           end;

         { are we accessing a pointer[], then convert the pointer to
           an array first }
         if (p^.left^.resulttype^.deftype=pointerdef) then
           begin
             { convert pointer to array }
             harr:=new(parraydef,init(0,$7fffffff,s32bitdef));
             parraydef(harr)^.elementtype.def:=ppointerdef(p^.left^.resulttype)^.pointertype.def;
             p^.left:=gentypeconvnode(p^.left,harr);
             firstpass(p^.left);
             if codegenerror then
               exit;
             p^.resulttype:=parraydef(harr)^.elementtype.def
           end;

         { determine return type }
         if not assigned(p^.resulttype) then
           if p^.left^.resulttype^.deftype=arraydef then
             p^.resulttype:=parraydef(p^.left^.resulttype)^.elementtype.def
           else if p^.left^.resulttype^.deftype=stringdef then
             begin
                { indexed access to strings }
                case pstringdef(p^.left^.resulttype)^.string_typ of
                   {
                   st_widestring : p^.resulttype:=cwchardef;
                   }
                   st_ansistring : p^.resulttype:=cchardef;
                   st_longstring : p^.resulttype:=cchardef;
                   st_shortstring : p^.resulttype:=cchardef;
                end;
             end
           else
             CGMessage(type_e_mismatch);
         { the register calculation is easy if a const index is used }
         if p^.right^.treetype=ordconstn then
           begin
{$ifdef consteval}
              { constant evaluation }
              if (p^.left^.treetype=loadn) and
                 (p^.left^.symtableentry^.typ=typedconstsym) then
               begin
                 tcsym:=ptypedconstsym(p^.left^.symtableentry);
                 if tcsym^.defintion^.typ=stringdef then
                  begin

                  end;
               end;
{$endif}
              p^.registers32:=p^.left^.registers32;

              { for ansi/wide strings, we need at least one register }
              if is_ansistring(p^.left^.resulttype) or
                is_widestring(p^.left^.resulttype) then
                p^.registers32:=max(p^.registers32,1);
           end
         else
           begin
              { this rules are suboptimal, but they should give }
              { good results                                }
              p^.registers32:=max(p^.left^.registers32,p^.right^.registers32);

              { for ansi/wide strings, we need at least one register }
              if is_ansistring(p^.left^.resulttype) or
                is_widestring(p^.left^.resulttype) then
                p^.registers32:=max(p^.registers32,1);

              { need we an extra register when doing the restore ? }
              if (p^.left^.registers32<=p^.right^.registers32) and
              { only if the node needs less than 3 registers }
              { two for the right node and one for the       }
              { left address                             }
                (p^.registers32<3) then
                inc(p^.registers32);

              { need we an extra register for the index ? }
              if (p^.right^.location.loc<>LOC_REGISTER)
              { only if the right node doesn't need a register }
                and (p^.right^.registers32<1) then
                inc(p^.registers32);

              { not correct, but what works better ?
              if p^.left^.registers32>0 then
                p^.registers32:=max(p^.registers32,2)
              else
                 min. one register
                p^.registers32:=max(p^.registers32,1);
              }
           end;
         p^.registersfpu:=max(p^.left^.registersfpu,p^.right^.registersfpu);
{$ifdef SUPPORT_MMX}
         p^.registersmmx:=max(p^.left^.registersmmx,p^.right^.registersmmx);
{$endif SUPPORT_MMX}
         if p^.left^.location.loc in [LOC_CREGISTER,LOC_REFERENCE] then
           p^.location.loc:=LOC_REFERENCE
         else
           p^.location.loc:=LOC_MEM;
      end;


{*****************************************************************************
                               FirstSelf
*****************************************************************************}

    procedure firstself(var p : ptree);
      begin
         if (p^.resulttype^.deftype=classrefdef) or
           ((p^.resulttype^.deftype=objectdef)
             and pobjectdef(p^.resulttype)^.is_class
           ) then
           p^.location.loc:=LOC_CREGISTER
         else
           p^.location.loc:=LOC_REFERENCE;
      end;


{*****************************************************************************
                               FirstWithN
*****************************************************************************}

    procedure firstwith(var p : ptree);
      var
         symtable : pwithsymtable;
         i : longint;
      begin
         if assigned(p^.left) and assigned(p^.right) then
            begin
               firstpass(p^.left);
               { is this correct ?  At least after is like if used
               set_varstate(p^.left,false);
                 already done in _with_statment }
               p^.left^.varstateset:=false;
               set_varstate(p^.left,true);
               if codegenerror then
                 exit;
               symtable:=p^.withsymtable;
               for i:=1 to p^.tablecount do
                 begin
                    if (p^.left^.treetype=loadn) and
                       (p^.left^.symtable=aktprocsym^.definition^.localst) then
                      symtable^.direct_with:=true;
                    symtable^.withnode:=p;
                    symtable:=pwithsymtable(symtable^.next);
                  end;
               firstpass(p^.right);
               if codegenerror then
                 exit;

               left_right_max(p);
               p^.resulttype:=voiddef;
            end
         else
           begin
              { optimization }
              disposetree(p);
              p:=nil;
           end;
      end;


end.
{
  $Log$
  Revision 1.45  2000-04-08 09:30:01  peter
     * fixed pointer->array conversion when resulttype was already set

  Revision 1.44  2000/03/23 16:29:32  jonas
    * real fix for web bug882

  Revision 1.43  2000/03/22 15:41:10  jonas
    * fixed webbug 882

  Revision 1.42  2000/02/17 14:53:43  florian
    * some updates for the newcg

  Revision 1.41  2000/02/09 13:23:08  peter
    * log truncated

  Revision 1.40  2000/01/10 16:38:43  pierre
   * suppress wrong warning for with vars

  Revision 1.39  2000/01/10 00:42:44  pierre
   * fix for bug 776

  Revision 1.38  2000/01/07 09:36:24  pierre
   * With argument is set as used to avoid unnecessary warnings

  Revision 1.37  2000/01/07 01:14:46  peter
    * updated copyright to 2000

  Revision 1.36  1999/11/30 10:40:58  peter
    + ttype, tsymlist

  Revision 1.35  1999/11/29 22:36:48  florian
    * problem with taking the address of abstract procedures fixed

  Revision 1.34  1999/11/18 15:34:51  pierre
    * Notes/Hints for local syms changed to
      Set_varstate function

  Revision 1.33  1999/11/17 17:05:07  pierre
   * Notes/hints changes

  Revision 1.32  1999/11/06 14:34:30  peter
    * truncated log to 20 revs

  Revision 1.31  1999/10/26 12:30:46  peter
    * const parameter is now checked
    * better and generic check if a node can be used for assigning
    * export fixes
    * procvar equal works now (it never had worked at least from 0.99.8)
    * defcoll changed to linkedlist with pparaitem so it can easily be
      walked both directions

  Revision 1.30  1999/10/13 10:40:55  peter
    * subscript support for tp_procvar

  Revision 1.29  1999/09/27 23:45:02  peter
    * procinfo is now a pointer
    * support for result setting in sub procedure

  Revision 1.28  1999/09/17 17:14:12  peter
    * @procvar fixes for tp mode
    * @<id>:= gives now an error

  Revision 1.27  1999/09/11 11:10:39  florian
    * fix of my previous commit, make cycle was broken

  Revision 1.26  1999/09/11 09:08:34  florian
    * fixed bug 596
    * fixed some problems with procedure variables and procedures of object,
      especially in TP mode. Procedure of object doesn't apply only to classes,
      it is also allowed for objects !!

  Revision 1.25  1999/08/23 23:34:15  pierre
   * one more register needed if hnewn with CREGISTER

  Revision 1.24  1999/08/05 16:53:25  peter
    * V_Fatal=1, all other V_ are also increased
    * Check for local procedure when assigning procvar
    * fixed comment parsing because directives
    * oldtp mode directives better supported
    * added some messages to errore.msg

  Revision 1.23  1999/08/04 00:23:44  florian
    * renamed i386asm and i386base to cpuasm and cpubase

  Revision 1.22  1999/08/03 22:03:35  peter
    * moved bitmask constants to sets
    * some other type/const renamings

}
