{
    $Id$
    Copyright (c) 2000-2002 by Florian Klaempfl

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
unit nmem;

{$i fpcdefs.inc}

interface

    uses
       node,
       symtype,symppu,symdef,symsym,symtable,
       cpubase;

    type
       tloadvmtnode = class(tunarynode)
          constructor create(l : tnode);virtual;
          function pass_1 : tnode;override;
          function det_resulttype:tnode;override;
       end;
       tloadvmtnodeclass = class of tloadvmtnode;

       thnewnode = class(tnode)
          objtype : ttype;
          constructor create(t:ttype);virtual;
          constructor ppuload(t:tnodetype;ppufile:tcompilerppufile);override;
          procedure ppuwrite(ppufile:tcompilerppufile);override;
          procedure derefimpl;override;
          function pass_1 : tnode;override;
          function det_resulttype:tnode;override;
       end;
       thnewnodeclass = class of thnewnode;

       thdisposenode = class(tunarynode)
          constructor create(l : tnode);virtual;
          function pass_1 : tnode;override;
          function det_resulttype:tnode;override;
       end;
       thdisposenodeclass = class of thdisposenode;

       taddrnode = class(tunarynode)
          getprocvardef : tprocvardef;
          constructor create(l : tnode);virtual;
          constructor ppuload(t:tnodetype;ppufile:tcompilerppufile);override;
          procedure ppuwrite(ppufile:tcompilerppufile);override;
          procedure mark_write;override;
          procedure derefimpl;override;
          function getcopy : tnode;override;
          function pass_1 : tnode;override;
          function det_resulttype:tnode;override;
       end;
       taddrnodeclass = class of taddrnode;

       tdoubleaddrnode = class(tunarynode)
          constructor create(l : tnode);virtual;
          function pass_1 : tnode;override;
          function det_resulttype:tnode;override;
       end;
       tdoubleaddrnodeclass = class of tdoubleaddrnode;

       tderefnode = class(tunarynode)
          constructor create(l : tnode);virtual;
          function pass_1 : tnode;override;
          function det_resulttype:tnode;override;
          procedure mark_write;override;
       end;
       tderefnodeclass = class of tderefnode;

       tsubscriptnode = class(tunarynode)
          vs : tvarsym;
          constructor create(varsym : tsym;l : tnode);virtual;
          constructor ppuload(t:tnodetype;ppufile:tcompilerppufile);override;
          procedure ppuwrite(ppufile:tcompilerppufile);override;
          procedure derefimpl;override;
          function getcopy : tnode;override;
          function pass_1 : tnode;override;
          function docompare(p: tnode): boolean; override;
          function det_resulttype:tnode;override;
          procedure mark_write;override;
       end;
       tsubscriptnodeclass = class of tsubscriptnode;

       tvecnode = class(tbinarynode)
          constructor create(l,r : tnode);virtual;
          function pass_1 : tnode;override;
          function det_resulttype:tnode;override;
          procedure mark_write;override;
       end;
       tvecnodeclass = class of tvecnode;

       tselfnode = class(tnode)
          classdef : tdef; { objectdef or classrefdef }
          constructor create(_class : tdef);virtual;
          constructor ppuload(t:tnodetype;ppufile:tcompilerppufile);override;
          procedure ppuwrite(ppufile:tcompilerppufile);override;
          procedure derefimpl;override;
          function pass_1 : tnode;override;
          function det_resulttype:tnode;override;
       end;
       tselfnodeclass = class of tselfnode;

       twithnode = class(tbinarynode)
          withsymtable  : twithsymtable;
          tablecount    : longint;
          withreference : treference;
          constructor create(symtable : twithsymtable;l,r : tnode;count : longint);virtual;
          destructor destroy;override;
          constructor ppuload(t:tnodetype;ppufile:tcompilerppufile);override;
          procedure ppuwrite(ppufile:tcompilerppufile);override;
          function getcopy : tnode;override;
          function pass_1 : tnode;override;
          function docompare(p: tnode): boolean; override;
          function det_resulttype:tnode;override;
       end;
       twithnodeclass = class of twithnode;

    var
       cloadvmtnode : tloadvmtnodeclass;
       chnewnode : thnewnodeclass;
       chdisposenode : thdisposenodeclass;
       caddrnode : taddrnodeclass;
       cdoubleaddrnode : tdoubleaddrnodeclass;
       cderefnode : tderefnodeclass;
       csubscriptnode : tsubscriptnodeclass;
       cvecnode : tvecnodeclass;
       cselfnode : tselfnodeclass;
       cwithnode : twithnodeclass;

implementation

    uses
      globtype,systems,
      cutils,verbose,globals,
      symconst,symbase,defutil,defcmp,
      nbas,
      htypechk,pass_1,ncal,nld,ncon,ncnv,cgbase
      ;

{*****************************************************************************
                            TLOADVMTNODE
*****************************************************************************}

    constructor tloadvmtnode.create(l : tnode);
      begin
         inherited create(loadvmtn,l);
      end;

    function tloadvmtnode.det_resulttype:tnode;
      begin
        result:=nil;
        resulttypepass(left);
        if codegenerror then
         exit;

        resulttype.setdef(tclassrefdef.create(left.resulttype));
      end;

    function tloadvmtnode.pass_1 : tnode;
      begin
         result:=nil;
         registers32:=1;
         location.loc:=LOC_REGISTER;
      end;

{*****************************************************************************
                             THNEWNODE
*****************************************************************************}

    constructor thnewnode.create(t:ttype);
      begin
         inherited create(hnewn);
         objtype:=t;
      end;


    constructor thnewnode.ppuload(t:tnodetype;ppufile:tcompilerppufile);
      begin
        inherited ppuload(t,ppufile);
        ppufile.gettype(objtype);
      end;


    procedure thnewnode.ppuwrite(ppufile:tcompilerppufile);
      begin
        inherited ppuwrite(ppufile);
        ppufile.puttype(objtype);
      end;


    procedure thnewnode.derefimpl;
      begin
        inherited derefimpl;
        objtype.resolve;
      end;


    function thnewnode.det_resulttype:tnode;
      begin
        result:=nil;
        if objtype.def.deftype<>objectdef then
          Message(parser_e_pointer_to_class_expected);
        resulttype:=objtype;
      end;


    function thnewnode.pass_1 : tnode;
      begin
         result:=nil;
      end;


{*****************************************************************************
                            THDISPOSENODE
*****************************************************************************}

    constructor thdisposenode.create(l : tnode);
      begin
         inherited create(hdisposen,l);
      end;


    function thdisposenode.det_resulttype:tnode;
      begin
        result:=nil;
        resulttypepass(left);
        if codegenerror then
         exit;
        if (left.resulttype.def.deftype<>pointerdef) then
          CGMessage1(type_e_pointer_type_expected,left.resulttype.def.typename);
        resulttype:=tpointerdef(left.resulttype.def).pointertype;
      end;


    function thdisposenode.pass_1 : tnode;
      begin
         result:=nil;
         firstpass(left);
         if codegenerror then
           exit;

         registers32:=left.registers32;
         registersfpu:=left.registersfpu;
{$ifdef SUPPORT_MMX}
         registersmmx:=left.registersmmx;
{$endif SUPPORT_MMX}
         if registers32<1 then
           registers32:=1;
         {
         if left.location.loc<>LOC_REFERENCE then
           CGMessage(cg_e_illegal_expression);
         }
         if left.location.loc=LOC_CREGISTER then
           inc(registers32);
         location.loc:=LOC_REFERENCE;
      end;


{*****************************************************************************
                             TADDRNODE
*****************************************************************************}

    constructor taddrnode.create(l : tnode);

      begin
         inherited create(addrn,l);
         getprocvardef:=nil;
      end;


    constructor taddrnode.ppuload(t:tnodetype;ppufile:tcompilerppufile);
      begin
        inherited ppuload(t,ppufile);
        getprocvardef:=tprocvardef(ppufile.getderef);
      end;


    procedure taddrnode.ppuwrite(ppufile:tcompilerppufile);
      begin
        inherited ppuwrite(ppufile);
        ppufile.putderef(getprocvardef);
      end;

    procedure Taddrnode.mark_write;

    begin
      {@procvar:=nil is legal in Delphi mode.}
      left.mark_write;
    end;

    procedure taddrnode.derefimpl;
      begin
        inherited derefimpl;
        resolvedef(pointer(getprocvardef));
      end;


    function taddrnode.getcopy : tnode;

      var
         p : taddrnode;

      begin
         p:=taddrnode(inherited getcopy);
         p.getprocvardef:=getprocvardef;
         getcopy:=p;
      end;


    function taddrnode.det_resulttype:tnode;
      var
         hp  : tnode;
         hp2 : TParaItem;
         hp3 : tabstractprocdef;
      begin
        result:=nil;
        resulttypepass(left);
        if codegenerror then
         exit;

        { don't allow constants }
        if is_constnode(left) then
         begin
           aktfilepos:=left.fileinfo;
           CGMessage(type_e_no_addr_of_constant);
           exit;
         end;

        { tp @procvar support (type of @procvar is a void pointer)
          Note: we need to leave the addrn in the tree,
          else we can't see the difference between @procvar and procvar.
          we set the procvarload flag so a secondpass does nothing for
          this node (PFV) }
        if (m_tp_procvar in aktmodeswitches) then
         begin
           case left.nodetype of
             calln :
               begin
                 { a load of a procvar can't have parameters }
                 if assigned(tcallnode(left).left) then
                   CGMessage(cg_e_illegal_expression);
                 { is it a procvar? }
                 hp:=tcallnode(left).right;
                 if assigned(hp) then
                   begin
                     { remove calln node }
                     tcallnode(left).right:=nil;
                     left.free;
                     left:=hp;
                     include(flags,nf_procvarload);
                   end;
               end;
             loadn,
             subscriptn,
             typeconvn,
             vecn,
             derefn :
               begin
                 if left.resulttype.def.deftype=procvardef then
                   include(flags,nf_procvarload);
               end;
           end;
           if nf_procvarload in flags then
            begin
              resulttype:=voidpointertype;
              exit;
            end;
         end;

        { proc 2 procvar ? }
        if left.nodetype=calln then
         { if it were a valid construct, the addr node would already have }
         { been removed in the parser. This happens for (in FPC mode)     }
         { procvar1 := @procvar2(parameters);                             }
         CGMessage(cg_e_illegal_expression)
        else
         if (left.nodetype=loadn) and (tloadnode(left).symtableentry.typ=procsym) then
          begin
            { the address is already available when loading a procedure of object }
            if assigned(tloadnode(left).left) then
             include(flags,nf_procvarload);

            { result is a procedure variable }
            { No, to be TP compatible, you must return a voidpointer to
              the procedure that is stored in the procvar.}
            if not(m_tp_procvar in aktmodeswitches) then
              begin
                 if assigned(getprocvardef) then
                  hp3:=getprocvardef
                 else
                  hp3:=tabstractprocdef(tprocsym(tloadnode(left).symtableentry).first_procdef);

                 { create procvardef }
                 resulttype.setdef(tprocvardef.create);
                 tprocvardef(resulttype.def).proctypeoption:=hp3.proctypeoption;
                 tprocvardef(resulttype.def).proccalloption:=hp3.proccalloption;
                 tprocvardef(resulttype.def).procoptions:=hp3.procoptions;
                 tprocvardef(resulttype.def).rettype:=hp3.rettype;
                 tprocvardef(resulttype.def).symtablelevel:=hp3.symtablelevel;

                 { method ? then set the methodpointer flag }
                 if (hp3.owner.symtabletype=objectsymtable) then
                   include(tprocvardef(resulttype.def).procoptions,po_methodpointer);

                 { only need the address of the method? this is needed
                   for @tobject.create }
                 if not assigned(tloadnode(left).left) then
                   include(tprocvardef(resulttype.def).procoptions,po_addressonly);

                 { we need to process the parameters reverse so they are inserted
                   in the correct right2left order (PFV) }
                 hp2:=TParaItem(hp3.Para.last);
                 while assigned(hp2) do
                   begin
                      tprocvardef(resulttype.def).concatpara(hp2.paratype,hp2.parasym,hp2.paratyp,hp2.defaultvalue);
                      hp2:=TParaItem(hp2.previous);
                   end;
              end
            else
              resulttype:=voidpointertype;
          end
        else
          begin
            { what are we getting the address from an absolute sym? }
            hp:=left;
            while assigned(hp) and (hp.nodetype in [vecn,derefn,subscriptn]) do
             hp:=tunarynode(hp).left;
            if assigned(hp) and (hp.nodetype=loadn) and
               ((tloadnode(hp).symtableentry.typ=absolutesym) and
                tabsolutesym(tloadnode(hp).symtableentry).absseg) then
             begin
               if not(cs_typed_addresses in aktlocalswitches) then
                 resulttype:=voidfarpointertype
               else
                 resulttype.setdef(tpointerdef.createfar(left.resulttype));
             end
            else
             begin
               if not(cs_typed_addresses in aktlocalswitches) then
                 resulttype:=voidpointertype
               else
                 resulttype.setdef(tpointerdef.create(left.resulttype));
             end;
          end;

         { this is like the function addr }
         inc(parsing_para_level);
         set_varstate(left,false);
         dec(parsing_para_level);

      end;


    function taddrnode.pass_1 : tnode;
      begin
         result:=nil;
         firstpass(left);
         if codegenerror then
          exit;

         make_not_regable(left);
         if nf_procvarload in flags then
          begin
            registers32:=left.registers32;
            registersfpu:=left.registersfpu;
{$ifdef SUPPORT_MMX}
            registersmmx:=left.registersmmx;
{$endif SUPPORT_MMX}
            if registers32<1 then
             registers32:=1;
            location.loc:=left.location.loc;
            exit;
          end;

         { we should allow loc_mem for @string }
         if not(left.location.loc in [LOC_CREFERENCE,LOC_REFERENCE]) then
           begin
             aktfilepos:=left.fileinfo;
             CGMessage(cg_e_illegal_expression);
           end;

         registers32:=left.registers32;
         registersfpu:=left.registersfpu;
{$ifdef SUPPORT_MMX}
         registersmmx:=left.registersmmx;
{$endif SUPPORT_MMX}
         if registers32<1 then
           registers32:=1;
         { is this right for object of methods ?? }
         location.loc:=LOC_REGISTER;
      end;


{*****************************************************************************
                           TDOUBLEADDRNODE
*****************************************************************************}

    constructor tdoubleaddrnode.create(l : tnode);
      begin
         inherited create(doubleaddrn,l);
      end;


    function tdoubleaddrnode.det_resulttype:tnode;
      begin
        result:=nil;
         resulttypepass(left);
         if codegenerror then
          exit;

         inc(parsing_para_level);
         set_varstate(left,false);
         dec(parsing_para_level);

         if (left.resulttype.def.deftype)<>procvardef then
           CGMessage(cg_e_illegal_expression);

         resulttype:=voidpointertype;
      end;


    function tdoubleaddrnode.pass_1 : tnode;
      begin
         result:=nil;
         make_not_regable(left);
         firstpass(left);
         if codegenerror then
           exit;

         if (left.location.loc<>LOC_REFERENCE) then
           CGMessage(cg_e_illegal_expression);

         registers32:=left.registers32;
         registersfpu:=left.registersfpu;
{$ifdef SUPPORT_MMX}
         registersmmx:=left.registersmmx;
{$endif SUPPORT_MMX}
         if registers32<1 then
           registers32:=1;
         location.loc:=LOC_REGISTER;
      end;


{*****************************************************************************
                             TDEREFNODE
*****************************************************************************}

    constructor tderefnode.create(l : tnode);

      begin
         inherited create(derefn,l);

      end;

    function tderefnode.det_resulttype:tnode;
      begin
         result:=nil;
         resulttypepass(left);
         set_varstate(left,true);
         if codegenerror then
          exit;

         if left.resulttype.def.deftype=pointerdef then
          resulttype:=tpointerdef(left.resulttype.def).pointertype
         else
          CGMessage(cg_e_invalid_qualifier);
      end;

    procedure Tderefnode.mark_write;

    begin
      include(flags,nf_write);
    end;

    function tderefnode.pass_1 : tnode;
      begin
         result:=nil;
         firstpass(left);
         if codegenerror then
          exit;

         registers32:=max(left.registers32,1);
         registersfpu:=left.registersfpu;
{$ifdef SUPPORT_MMX}
         registersmmx:=left.registersmmx;
{$endif SUPPORT_MMX}

         location.loc:=LOC_REFERENCE;
      end;


{*****************************************************************************
                            TSUBSCRIPTNODE
*****************************************************************************}

    constructor tsubscriptnode.create(varsym : tsym;l : tnode);

      begin
         inherited create(subscriptn,l);
         { vs should be changed to tsym! }
         vs:=tvarsym(varsym);
      end;

    constructor tsubscriptnode.ppuload(t:tnodetype;ppufile:tcompilerppufile);
      begin
        inherited ppuload(t,ppufile);
        vs:=tvarsym(ppufile.getderef);
      end;


    procedure tsubscriptnode.ppuwrite(ppufile:tcompilerppufile);
      begin
        inherited ppuwrite(ppufile);
        ppufile.putderef(vs);
      end;


    procedure tsubscriptnode.derefimpl;
      begin
        inherited derefimpl;
        resolvesym(pointer(vs));
      end;


    function tsubscriptnode.getcopy : tnode;

      var
         p : tsubscriptnode;

      begin
         p:=tsubscriptnode(inherited getcopy);
         p.vs:=vs;
         getcopy:=p;
      end;


    function tsubscriptnode.det_resulttype:tnode;
      begin
        result:=nil;
        resulttypepass(left);
        resulttype:=vs.vartype;
      end;

    procedure Tsubscriptnode.mark_write;

    begin
      include(flags,nf_write);
    end;

    function tsubscriptnode.pass_1 : tnode;
      begin
         result:=nil;
         firstpass(left);
         if codegenerror then
          exit;

         registers32:=left.registers32;
         registersfpu:=left.registersfpu;
{$ifdef SUPPORT_MMX}
         registersmmx:=left.registersmmx;
{$endif SUPPORT_MMX}
         { classes must be dereferenced implicit }
         if is_class_or_interface(left.resulttype.def) then
           begin
              if registers32=0 then
                registers32:=1;
              location.loc:=LOC_REFERENCE;
           end
         else
           begin
              if (left.location.loc<>LOC_CREFERENCE) and
                 (left.location.loc<>LOC_REFERENCE) then
                CGMessage(cg_e_illegal_expression);
              location.loc:=left.location.loc;
           end;
      end;

    function tsubscriptnode.docompare(p: tnode): boolean;
      begin
        docompare :=
          inherited docompare(p) and
          (vs = tsubscriptnode(p).vs);
      end;


{*****************************************************************************
                               TVECNODE
*****************************************************************************}

    constructor tvecnode.create(l,r : tnode);

      begin
         inherited create(vecn,l,r);
      end;


    function tvecnode.det_resulttype:tnode;
      var
         htype : ttype;
         ct : tconverttype;
      begin
         result:=nil;
         resulttypepass(left);
         resulttypepass(right);
         if codegenerror then
          exit;

         { maybe type conversion for the index value, but
           do not convert enums,booleans,char }
         if (right.resulttype.def.deftype<>enumdef) and
            not(is_char(right.resulttype.def)) and
            not(is_boolean(right.resulttype.def)) then
           begin
             inserttypeconv(right,s32bittype);
           end;

         case left.resulttype.def.deftype of
           arraydef :
             begin
               { check type of the index value }
               if (compare_defs(right.resulttype.def,tarraydef(left.resulttype.def).rangetype.def,right.nodetype)=te_incompatible) then
                 CGMessage(type_e_mismatch);
               resulttype:=tarraydef(left.resulttype.def).elementtype;
             end;
           pointerdef :
             begin
               { are we accessing a pointer[], then convert the pointer to
                 an array first, in FPC this is allowed for all pointers in
                 delphi/tp7 it's only allowed for pchars }
               if (m_fpc in aktmodeswitches) or
                  is_pchar(left.resulttype.def) or
                  is_pwidechar(left.resulttype.def) then
                begin
                  { convert pointer to array }
                  htype.setdef(tarraydef.create(0,$7fffffff,s32bittype));
                  tarraydef(htype.def).setelementtype(tpointerdef(left.resulttype.def).pointertype);
                  inserttypeconv(left,htype);

                  resulttype:=tarraydef(htype.def).elementtype;
                end
               else
                CGMessage(type_e_array_required);
             end;
           stringdef :
             begin
                { indexed access to 0 element is only allowed for shortstrings }
                if (right.nodetype=ordconstn) and
                   (tordconstnode(right).value=0) and
                   not(is_shortstring(left.resulttype.def)) then
                  CGMessage(cg_e_can_access_element_zero);
                case tstringdef(left.resulttype.def).string_typ of
                   st_widestring :
                     resulttype:=cwidechartype;
                   st_ansistring :
                     resulttype:=cchartype;
                   st_longstring :
                     resulttype:=cchartype;
                   st_shortstring :
                     resulttype:=cchartype;
                end;
             end
           else
             CGMessage(type_e_array_required);
        end;
      end;

    procedure Tvecnode.mark_write;

    begin
      include(flags,nf_write);
    end;

    function tvecnode.pass_1 : tnode;
{$ifdef consteval}
      var
         tcsym : ttypedconstsym;
{$endif}
      begin
         result:=nil;
         firstpass(left);
         firstpass(right);
         if codegenerror then
           exit;

         { the register calculation is easy if a const index is used }
         if right.nodetype=ordconstn then
           begin
{$ifdef consteval}
              { constant evaluation }
              if (left.nodetype=loadn) and
                 (left.symtableentry.typ=typedconstsym) then
               begin
                 tcsym:=ttypedconstsym(left.symtableentry);
                 if tcsym.defintion^.typ=stringdef then
                  begin

                  end;
               end;
{$endif}
              registers32:=left.registers32;

              { for ansi/wide strings, we need at least one register }
              if is_ansistring(left.resulttype.def) or
                is_widestring(left.resulttype.def) or
              { ... as well as for dynamic arrays }
                is_dynamic_array(left.resulttype.def) then
                registers32:=max(registers32,1);
           end
         else
           begin
              { this rules are suboptimal, but they should give }
              { good results                                }
              registers32:=max(left.registers32,right.registers32);

              { for ansi/wide strings, we need at least one register }
              if is_ansistring(left.resulttype.def) or
                is_widestring(left.resulttype.def) or
              { ... as well as for dynamic arrays }
                is_dynamic_array(left.resulttype.def) then
                registers32:=max(registers32,1);

              { need we an extra register when doing the restore ? }
              if (left.registers32<=right.registers32) and
              { only if the node needs less than 3 registers }
              { two for the right node and one for the       }
              { left address                             }
                (registers32<3) then
                inc(registers32);

              { need we an extra register for the index ? }
              if (right.location.loc<>LOC_REGISTER)
              { only if the right node doesn't need a register }
                and (right.registers32<1) then
                inc(registers32);

              { not correct, but what works better ?
              if left.registers32>0 then
                registers32:=max(registers32,2)
              else
                 min. one register
                registers32:=max(registers32,1);
              }
           end;
         registersfpu:=max(left.registersfpu,right.registersfpu);
{$ifdef SUPPORT_MMX}
         registersmmx:=max(left.registersmmx,right.registersmmx);
{$endif SUPPORT_MMX}
         if left.location.loc in [LOC_CREGISTER,LOC_REFERENCE] then
           location.loc:=LOC_REFERENCE
         else
           location.loc:=LOC_CREFERENCE;
      end;


{*****************************************************************************
                               TSELFNODE
*****************************************************************************}

    constructor tselfnode.create(_class : tdef);

      begin
         inherited create(selfn);
         classdef:=_class;
      end;

    constructor tselfnode.ppuload(t:tnodetype;ppufile:tcompilerppufile);
      begin
        inherited ppuload(t,ppufile);
        classdef:=tdef(ppufile.getderef);
      end;


    procedure tselfnode.ppuwrite(ppufile:tcompilerppufile);
      begin
        inherited ppuwrite(ppufile);
        ppufile.putderef(classdef);
      end;


    procedure tselfnode.derefimpl;
      begin
        inherited derefimpl;
        resolvedef(pointer(classdef));
      end;


    function tselfnode.det_resulttype:tnode;
      begin
        result:=nil;
        resulttype.setdef(classdef);
      end;

    function tselfnode.pass_1 : tnode;
      begin
         result:=nil;
         if (resulttype.def.deftype=classrefdef) or
           is_class(resulttype.def) then
           location.loc:=LOC_CREGISTER
         else
           location.loc:=LOC_REFERENCE;
      end;


{*****************************************************************************
                               TWITHNODE
*****************************************************************************}

    constructor twithnode.create(symtable : twithsymtable;l,r : tnode;count : longint);

      begin
         inherited create(withn,l,r);
         withsymtable:=symtable;
         tablecount:=count;
         FillChar(withreference,sizeof(withreference),0);
         set_file_line(l);
      end;


    destructor twithnode.destroy;
      var
        hsymt,
        symt : tsymtable;
        i    : longint;
      begin
        symt:=withsymtable;
        for i:=1 to tablecount do
         begin
           if assigned(symt) then
            begin
              hsymt:=symt.next;
              symt.free;
              symt:=hsymt;
            end;
         end;
        inherited destroy;
      end;


    constructor twithnode.ppuload(t:tnodetype;ppufile:tcompilerppufile);
      begin
        inherited ppuload(t,ppufile);
        internalerror(200208192);
      end;


    procedure twithnode.ppuwrite(ppufile:tcompilerppufile);
      begin
        inherited ppuwrite(ppufile);
        internalerror(200208193);
      end;


    function twithnode.getcopy : tnode;

      var
         p : twithnode;

      begin
         p:=twithnode(inherited getcopy);
         p.withsymtable:=withsymtable;
         p.tablecount:=tablecount;
         p.withreference:=withreference;
         result:=p;
      end;

    function twithnode.det_resulttype:tnode;
      var
         symtable : tsymtable;
         i : longint;
      begin
         result:=nil;
         resulttype:=voidtype;
         if assigned(left) and assigned(right) then
          begin
            resulttypepass(left);
            unset_varstate(left);
            set_varstate(left,true);
            if codegenerror then
             exit;

            symtable:=withsymtable;
            for i:=1 to tablecount do
             begin
               if (left.nodetype=loadn) and
                  (tloadnode(left).symtable=aktprocdef.localst) then
                twithsymtable(symtable).direct_with:=true;
               twithsymtable(symtable).withnode:=self;
               symtable:=symtable.next;
             end;

            resulttypepass(right);
            if codegenerror then
             exit;
          end;
        resulttype:=voidtype;
      end;


    function twithnode.pass_1 : tnode;
      begin
         result:=nil;
         if assigned(left) and assigned(right) then
            begin
               firstpass(left);
               firstpass(right);
               if codegenerror then
                 exit;

               left_right_max;
            end
         else
           begin
              { optimization }
              result:=nil;
           end;
      end;

    function twithnode.docompare(p: tnode): boolean;
      begin
        docompare :=
          inherited docompare(p) and
          (withsymtable = twithnode(p).withsymtable) and
          (tablecount = twithnode(p).tablecount);
      end;

begin
  cloadvmtnode := tloadvmtnode;
  chnewnode := thnewnode;
  chdisposenode := thdisposenode;
  caddrnode := taddrnode;
  cdoubleaddrnode := tdoubleaddrnode;
  cderefnode := tderefnode;
  csubscriptnode := tsubscriptnode;
  cvecnode := tvecnode;
  cselfnode := tselfnode;
  cwithnode := twithnode;
end.
{
  $Log$
  Revision 1.44  2003-01-06 21:16:52  peter
    * po_addressonly added to retrieve the address of a methodpointer
      only, this is used for @tclass.method which has no self pointer

  Revision 1.43  2003/01/04 15:54:03  daniel
    * Fixed mark_write for @ operator
      (can happen when compiling @procvar:=nil (Delphi mode construction))

  Revision 1.42  2003/01/03 12:15:56  daniel
    * Removed ifdefs around notifications
      ifdefs around for loop optimizations remain

  Revision 1.41  2002/11/25 17:43:20  peter
    * splitted defbase in defutil,symutil,defcmp
    * merged isconvertable and is_equal into compare_defs(_ext)
    * made operator search faster by walking the list only once

  Revision 1.40  2002/09/27 21:13:28  carl
    * low-highval always checked if limit ober 2GB is reached (to avoid overflow)

  Revision 1.39  2002/09/01 18:44:17  peter
    * cleanup of tvecnode.det_resulttype
    * move 0 element of string access check to resulttype

  Revision 1.38  2002/09/01 13:28:38  daniel
   - write_access fields removed in favor of a flag

  Revision 1.37  2002/09/01 08:01:16  daniel
   * Removed sets from Tcallnode.det_resulttype
   + Added read/write notifications of variables. These will be usefull
     for providing information for several optimizations. For example
     the value of the loop variable of a for loop does matter is the
     variable is read after the for loop, but if it's no longer used
     or written, it doesn't matter and this can be used to optimize
     the loop code generation.

  Revision 1.36  2002/08/19 19:36:43  peter
    * More fixes for cross unit inlining, all tnodes are now implemented
    * Moved pocall_internconst to po_internconst because it is not a
      calling type at all and it conflicted when inlining of these small
      functions was requested

  Revision 1.35  2002/07/23 09:51:23  daniel
  * Tried to make Tprocsym.defs protected. I didn't succeed but the cleanups
    are worth comitting.

  Revision 1.34  2002/07/20 11:57:54  florian
    * types.pas renamed to defbase.pas because D6 contains a types
      unit so this would conflicts if D6 programms are compiled
    + Willamette/SSE2 instructions to assembler added

  Revision 1.33  2002/05/18 13:34:10  peter
    * readded missing revisions

  Revision 1.32  2002/05/16 19:46:39  carl
  + defines.inc -> fpcdefs.inc to avoid conflicts if compiling by hand
  + try to fix temp allocation (still in ifdef)
  + generic constructor calls
  + start of tassembler / tmodulebase class cleanup

  Revision 1.30  2002/05/12 16:53:07  peter
    * moved entry and exitcode to ncgutil and cgobj
    * foreach gets extra argument for passing local data to the
      iterator function
    * -CR checks also class typecasts at runtime by changing them
      into as
    * fixed compiler to cycle with the -CR option
    * fixed stabs with elf writer, finally the global variables can
      be watched
    * removed a lot of routines from cga unit and replaced them by
      calls to cgobj
    * u32bit-s32bit updates for and,or,xor nodes. When one element is
      u32bit then the other is typecasted also to u32bit without giving
      a rangecheck warning/error.
    * fixed pascal calling method with reversing also the high tree in
      the parast, detected by tcalcst3 test

  Revision 1.29  2002/04/21 19:02:04  peter
    * removed newn and disposen nodes, the code is now directly
      inlined from pexpr
    * -an option that will write the secondpass nodes to the .s file, this
      requires EXTDEBUG define to actually write the info
    * fixed various internal errors and crashes due recent code changes

  Revision 1.28  2002/04/20 21:32:23  carl
  + generic FPC_CHECKPOINTER
  + first parameter offset in stack now portable
  * rename some constants
  + move some cpu stuff to other units
  - remove unused constents
  * fix stacksize for some targets
  * fix generic size problems which depend now on EXTEND_SIZE constant

  Revision 1.27  2002/04/02 17:11:29  peter
    * tlocation,treference update
    * LOC_CONSTANT added for better constant handling
    * secondadd splitted in multiple routines
    * location_force_reg added for loading a location to a register
      of a specified size
    * secondassignment parses now first the right and then the left node
      (this is compatible with Kylix). This saves a lot of push/pop especially
      with string operations
    * adapted some routines to use the new cg methods

  Revision 1.26  2002/04/01 20:57:13  jonas
    * fixed web bug 1907
    * fixed some other procvar related bugs (all related to accepting procvar
        constructs with either too many or too little parameters)
    (both merged, includes second typo fix of pexpr.pas)

}
