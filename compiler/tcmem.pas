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
      cobjects,verbose,globals,systems,
      symtable,aasm,types,
      hcodegen,htypechk,pass_1
{$ifdef i386}
      ,i386
{$endif}
{$ifdef m68k}
      ,m68k
{$endif}
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
         firstpass(p^.left);

         if codegenerror then
           exit;
         p^.registers32:=p^.left^.registers32;
         p^.registersfpu:=p^.left^.registersfpu;
{$ifdef SUPPORT_MMX}
         p^.registersmmx:=p^.left^.registersmmx;
{$endif SUPPORT_MMX}
         { result type is already set }
         procinfo.flags:=procinfo.flags or pi_do_call;
         p^.location.loc:=LOC_REGISTER;
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
         if (p^.left^.resulttype=nil) or (p^.left^.resulttype^.deftype<>pointerdef) then
           CGMessage(type_e_pointer_type_expected);

         if (p^.left^.location.loc<>LOC_REFERENCE) {and
            (p^.left^.location.loc<>LOC_CREGISTER)} then
           CGMessage(cg_e_illegal_expression);

         p^.registers32:=p^.left^.registers32;
         p^.registersfpu:=p^.left^.registersfpu;
{$ifdef SUPPORT_MMX}
         p^.registersmmx:=p^.left^.registersmmx;
{$endif SUPPORT_MMX}
         p^.resulttype:=voiddef;
         procinfo.flags:=procinfo.flags or pi_do_call;
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
              if p^.left^.treetype=calln then
                begin
                     { it could also be a procvar, not only pprocsym ! }
                     if p^.left^.symtableprocentry^.typ=varsym then
                        hp:=genloadnode(pvarsym(p^.left^.symtableprocentry),p^.left^.symtableproc)
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
                         hp3:=pabstractprocdef(pvarsym(p^.left^.symtableprocentry)^.definition)
                        else
                         hp3:=pabstractprocdef(pprocsym(p^.left^.symtableprocentry)^.definition);

                        pprocvardef(p^.resulttype)^.options:=hp3^.options;
                        pprocvardef(p^.resulttype)^.retdef:=hp3^.retdef;

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
                  if not(cs_typed_addresses in aktlocalswitches) then
                    p^.resulttype:=voidpointerdef
                  else p^.resulttype:=new(ppointerdef,init(p^.left^.resulttype));
                end;
           end;
         store_valid:=must_be_valid;
         must_be_valid:=false;
         firstpass(p^.left);
         must_be_valid:=store_valid;
         if codegenerror then
           exit;

         { we should allow loc_mem for @string }
         if (p^.left^.location.loc<>LOC_REFERENCE) and
            (p^.left^.location.loc<>LOC_MEM) then
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
           pobjectdef(p^.left^.resulttype)^.isclass then
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
      begin
         firstpass(p^.left);
         firstpass(p^.right);
         if codegenerror then
           exit;

         { range check only for arrays }
         if (p^.left^.resulttype^.deftype=arraydef) then
           begin
              if not(isconvertable(p^.right^.resulttype,
                parraydef(p^.left^.resulttype)^.rangedef,
                ct,ordconstn,false)) and
              not(is_equal(p^.right^.resulttype,
                parraydef(p^.left^.resulttype)^.rangedef)) then
                CGMessage(type_e_mismatch);
           end;
         { Never convert a boolean or a char !}
         { maybe type conversion }
         if (p^.right^.resulttype^.deftype<>enumdef) and
          not ((p^.right^.resulttype^.deftype=orddef) and
          (Porddef(p^.right^.resulttype)^.typ in [bool8bit,bool16bit,bool32bit,uchar])) then
                begin
                        p^.right:=gentypeconvnode(p^.right,s32bitdef);
                        { once more firstpass }
                        {?? It's better to only firstpass when the tree has
                         changed, isn't it ?}
                        firstpass(p^.right);
                end;
         if codegenerror then
           exit;

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
              p^.registers32:=p^.left^.registers32;

              { for ansi/wide strings, we need at least one register }
              if is_ansistring(p^.left^.resulttype) or
                is_widestring(p^.left^.resulttype) then
                p^.registers32:=max(p^.registers32,1);
           end
         else
           begin
              { this rules are suboptimal, but they should give }
              { good results                                    }
              p^.registers32:=max(p^.left^.registers32,p^.right^.registers32);

              { for ansi/wide strings, we need at least one register }
              if is_ansistring(p^.left^.resulttype) or
                is_widestring(p^.left^.resulttype) then
                p^.registers32:=max(p^.registers32,1);

              { need we an extra register when doing the restore ? }
              if (p^.left^.registers32<=p^.right^.registers32) and
              { only if the node needs less than 3 registers }
              { two for the right node and one for the       }
              { left address                                 }
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
         p^.location.loc:=p^.left^.location.loc;
      end;


{*****************************************************************************
                               FirstSelf
*****************************************************************************}

    procedure firstself(var p : ptree);
      begin
         if (p^.resulttype^.deftype=classrefdef) or
           ((p^.resulttype^.deftype=objectdef)
             and pobjectdef(p^.resulttype)^.isclass
           ) then
           p^.location.loc:=LOC_CREGISTER
         else
           p^.location.loc:=LOC_REFERENCE;
      end;


{*****************************************************************************
                               FirstWithN
*****************************************************************************}

    procedure firstwith(var p : ptree);
      begin
         if assigned(p^.left) and assigned(p^.right) then
            begin
               firstpass(p^.left);
               if codegenerror then
                 exit;

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
  Revision 1.3  1998-09-26 15:03:05  florian
    * small problems with DOM and excpetions fixed (code generation
      of raise was wrong and self was sometimes destroyed :()

  Revision 1.2  1998/09/24 23:49:24  peter
    + aktmodeswitches

  Revision 1.1  1998/09/23 20:42:24  peter
    * splitted pass_1

}

