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
      cobjects,verbose,globals,systems,
      symtable,aasm,types,
      hcodegen,htypechk,pass_1,
      tccnv
{$ifdef i386}
{$ifdef ag386bin}
      ,i386base
{$else}
      ,i386
{$endif}
      ,tgeni386
{$endif}
{$ifdef m68k}
      ,m68k,tgen68k
{$endif}
      ;

{*****************************************************************************
                               FirstLoad
*****************************************************************************}

    procedure firstload(var p : ptree);
      var
         p1 : ptree;

      begin
{$ifndef NODIRECTWITH}
         if (p^.symtable^.symtabletype=withsymtable) and
            (pwithsymtable(p^.symtable)^.direct_with) then
           begin
              p1:=getcopy(ptree(pwithsymtable(p^.symtable)^.withrefnode));
              p1:=gensubscriptnode(pvarsym(p^.symtableentry),p1);
              putnode(p);
              p:=p1;
              firstpass(p);
              exit;
           end;
{$endif ndef NODIRECTWITH}

         p^.location.loc:=LOC_REFERENCE;
         p^.registers32:=0;
         p^.registersfpu:=0;
{$ifdef SUPPORT_MMX}
         p^.registersmmx:=0;
{$endif SUPPORT_MMX}
         clear_reference(p^.location.reference);
         if p^.symtableentry^.typ=funcretsym then
           begin
              putnode(p);
              p:=genzeronode(funcretn);
              p^.funcretprocinfo:=pprocinfo(pfuncretsym(p^.symtableentry)^.funcretprocinfo);
              p^.retdef:=pfuncretsym(p^.symtableentry)^.funcretdef;
              firstpass(p);
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
                          pvarsym(p^.symtableentry)^.var_options:=
                            pvarsym(p^.symtableentry)^.var_options and not vo_regable;
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
                     if pvarsym(p^.symtableentry)^.is_valid=2 then
                       if (assigned(pvarsym(p^.symtableentry)^.owner) and assigned(aktprocsym)
                       and (pvarsym(p^.symtableentry)^.owner = aktprocsym^.definition^.localst)) then
                       CGMessage1(sym_n_uninitialized_local_variable,pvarsym(p^.symtableentry)^.name);
                     end;
                   if count_ref then
                     begin
                        if (p^.is_first) then
                          begin
                             if (pvarsym(p^.symtableentry)^.is_valid=2) then
                               pvarsym(p^.symtableentry)^.is_valid:=1;
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
            procinfo.flags:=procinfo.flags or pi_do_call;
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
            if (p^.right^.treetype=realconstn) then
              begin
                 if p^.left^.resulttype^.deftype=floatdef then
                   begin
                      case pfloatdef(p^.left^.resulttype)^.typ of
                        s32real : p^.right^.realtyp:=ait_real_32bit;
                        s64real : p^.right^.realtyp:=ait_real_64bit;
                        s80real : p^.right^.realtyp:=ait_real_extended;
                        { what about f32bit and s64bit }
                      else
                        begin
                           p^.right:=gentypeconvnode(p^.right,p^.left^.resulttype);

                           { nochmal firstpass wegen der Typkonvertierung aufrufen }
                           firstpass(p^.right);

                           if codegenerror then
                             exit;
                        end;
                      end;
                   end;
               end
             else
               begin
                 p^.right:=gentypeconvnode(p^.right,p^.left^.resulttype);
                 firstpass(p^.right);
                 if codegenerror then
                  exit;
               end;
          end;

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
            (@procinfo<>pprocinfo(p^.funcretprocinfo)) then
           p^.registers32:=1;
         { no claim if setting higher return value_str }
         if must_be_valid and
            (@procinfo=pprocinfo(p^.funcretprocinfo)) and
            not procinfo.funcret_is_valid then
           CGMessage(sym_w_function_result_not_set);
         if count_ref then
           pprocinfo(p^.funcretprocinfo)^.funcret_is_valid:=true;
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
        pd:=nil;
        len:=0;
        varia:=false;
        if assigned(p^.left) then
         begin
           hp:=p;
           while assigned(hp) do
            begin
              firstpass(hp^.left);
              case hp^.left^.resulttype^.deftype of
               floatdef : begin
                            hp^.left:=gentypeconvnode(hp^.left,s80floatdef);
                            firstpass(hp^.left);
                          end;
              stringdef : begin
                            if p^.cargs then
                             begin
                               hp^.left:=gentypeconvnode(hp^.left,charpointerdef);
                               firstpass(hp^.left);
                             end;
                          end;
              end;
              if (pd=nil) then
               pd:=hp^.left^.resulttype
              else
               if (not varia) and (not is_equal(pd,hp^.left^.resulttype)) then
                varia:=true;
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
        p^.resulttype:=new(parraydef,init(0,len-1,pd));
        parraydef(p^.resulttype)^.IsConstructor:=true;
        parraydef(p^.resulttype)^.IsVariant:=varia;
        p^.location.loc:=LOC_REFERENCE;
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
  Revision 1.16  1999-02-22 02:15:52  peter
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
