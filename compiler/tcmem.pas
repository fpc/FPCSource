{
    $Id$
    Copyright (c) 1993-98 by Florian Klaempfl

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
      hcodegen,htypechk,pass_1,cpubase;

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
         p^.resulttype:=ppointerdef(p^.left^.resulttype)^.definition;
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
         hp2 : pdefcoll;
         store_valid : boolean;
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
                        hp3:=pabstractprocdef(pvarsym(p^.left^.symtableentry)^.definition)
                       else
                        hp3:=pabstractprocdef(pprocsym(p^.left^.symtableprocentry)^.definition);

                       pprocvardef(p^.resulttype)^.proctypeoption:=hp3^.proctypeoption;
                       pprocvardef(p^.resulttype)^.proccalloptions:=hp3^.proccalloptions;
                       pprocvardef(p^.resulttype)^.procoptions:=hp3^.procoptions;
                       pprocvardef(p^.resulttype)^.retdef:=hp3^.retdef;
                       pprocvardef(p^.resulttype)^.symtablelevel:=hp3^.symtablelevel;

                     { method ? then set the methodpointer flag }
                       if (hp3^.owner^.symtabletype=objectsymtable) and
                          (pobjectdef(hp3^.owner^.defowner)^.is_class) then
{$ifdef INCLUDEOK}
                         include(pprocvardef(p^.resulttype)^.procoptions,po_methodpointer);
{$else}
                         pprocvardef(p^.resulttype)^.procoptions:=pprocvardef(p^.resulttype)^.procoptions+[po_methodpointer];
{$endif}
                       hp2:=hp3^.para1;
                       while assigned(hp2) do
                         begin
                            pprocvardef(p^.resulttype)^.concatdef(hp2^.data,hp2^.paratyp);
                            hp2:=hp2^.next;
                         end;
                    end
                  else
                    p^.resulttype:=voidpointerdef;

                  disposetree(p^.left);
                  p^.left:=hp;
                end
              else
                begin
                  { what are we getting the address from an absolute sym? }
                  hp:=p^.left;
                  while assigned(hp) and (hp^.treetype in [vecn,subscriptn]) do
                   hp:=hp^.left;
                  if assigned(hp) and (hp^.treetype=loadn) and
                     ((hp^.symtableentry^.typ=absolutesym) and
                      pabsolutesym(hp^.symtableentry)^.absseg) then
                   begin
                     if not(cs_typed_addresses in aktlocalswitches) then
                       p^.resulttype:=voidfarpointerdef
                     else
                       p^.resulttype:=new(ppointerdef,initfar(p^.left^.resulttype));
                   end
                  else
                   begin
                     if not(cs_typed_addresses in aktlocalswitches) then
                       p^.resulttype:=voidpointerdef
                     else
                       p^.resulttype:=new(ppointerdef,init(p^.left^.resulttype));
                   end;
                end;
           end;
         store_valid:=must_be_valid;
         must_be_valid:=false;
         firstpass(p^.left);
         must_be_valid:=store_valid;
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
         p^.location.loc:=LOC_REGISTER;
      end;


{*****************************************************************************
                           FirstDoubleAddr
*****************************************************************************}

    procedure firstdoubleaddr(var p : ptree);
      begin
         make_not_regable(p^.left);
         firstpass(p^.left);
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

         p^.resulttype:=ppointerdef(p^.left^.resulttype)^.definition;
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

         p^.resulttype:=p^.vs^.definition;
         { this must be done in the parser
         if count_ref and not must_be_valid then
           if (p^.vs^.properties and sp_protected)<>0 then
             CGMessage(parser_e_cant_write_protected_member);
         }
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
              if (isconvertable(p^.right^.resulttype,parraydef(p^.left^.resulttype)^.rangedef,
                    ct,ordconstn,false)=0) and
                 not(is_equal(p^.right^.resulttype,parraydef(p^.left^.resulttype)^.rangedef)) then
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

         { determine return type }
         if not assigned(p^.resulttype) then
           if p^.left^.resulttype^.deftype=arraydef then
             p^.resulttype:=parraydef(p^.left^.resulttype)^.definition
           else if (p^.left^.resulttype^.deftype=pointerdef) then
             begin
                { convert pointer to array }
                harr:=new(parraydef,init(0,$7fffffff,s32bitdef));
                parraydef(harr)^.definition:=ppointerdef(p^.left^.resulttype)^.definition;
                p^.left:=gentypeconvnode(p^.left,harr);
                firstpass(p^.left);

                if codegenerror then
                  exit;
                p^.resulttype:=parraydef(harr)^.definition
             end
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
  Revision 1.30  1999-10-13 10:40:55  peter
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

  Revision 1.21  1999/07/16 10:04:39  peter
    * merged

  Revision 1.20  1999/07/05 20:25:41  peter
    * merged

  Revision 1.19  1999/07/05 16:24:17  peter
    * merged

  Revision 1.18.2.4  1999/07/16 09:54:59  peter
    * @procvar support in tp7 mode works again

  Revision 1.18.2.3  1999/07/05 20:06:47  peter
    * give error instead of warning for ln(0) and sqrt(0)

  Revision 1.18.2.2  1999/07/05 16:22:56  peter
    * error if @constant

  Revision 1.18.2.1  1999/06/28 00:33:53  pierre
   * better error position bug0269

  Revision 1.18  1999/06/03 09:34:12  peter
    * better methodpointer check for proc->procvar

  Revision 1.17  1999/05/27 19:45:24  peter
    * removed oldasm
    * plabel -> pasmlabel
    * -a switches to source writing automaticly
    * assembler readers OOPed
    * asmsymbol automaticly external
    * jumptables and other label fixes for asm readers

  Revision 1.16  1999/05/18 09:52:21  peter
    * procedure of object and addrn fixes

  Revision 1.15  1999/05/17 23:51:46  peter
    * with temp vars now use a reference with a persistant temp instead
      of setting datasize

  Revision 1.14  1999/05/01 13:24:57  peter
    * merged nasm compiler
    * old asm moved to oldasm/

  Revision 1.13  1999/04/26 18:30:05  peter
    * farpointerdef moved into pointerdef.is_far

  Revision 1.12  1999/03/02 18:24:24  peter
    * fixed overloading of array of char

  Revision 1.11  1999/02/22 02:15:54  peter
    * updates for ag386bin

  Revision 1.10  1999/02/04 11:44:47  florian
    * fixed indexed access of ansistrings to temp. ansistring, i.e.
      c:=(s1+s2)[i], the temp is now correctly remove and the generated
      code is also fixed

  Revision 1.9  1999/01/22 12:18:34  pierre
   * with bug introduced with DIRECTWITH removed

  Revision 1.8  1999/01/21 16:41:08  pierre
   * fix for constructor inside with statements

  Revision 1.7  1998/12/30 22:15:59  peter
    + farpointer type
    * absolutesym now also stores if its far

  Revision 1.6  1998/12/15 17:16:02  peter
    * fixed const s : ^string
    * first things for const pchar : @string[1]

  Revision 1.5  1998/12/11 00:03:57  peter
    + globtype,tokens,version unit splitted from globals

  Revision 1.4  1998/11/25 19:12:53  pierre
    * var:=new(pointer_type) support added

  Revision 1.3  1998/09/26 15:03:05  florian
    * small problems with DOM and excpetions fixed (code generation
      of raise was wrong and self was sometimes destroyed :()

  Revision 1.2  1998/09/24 23:49:24  peter
    + aktmodeswitches

  Revision 1.1  1998/09/23 20:42:24  peter
    * splitted pass_1

}

