{
    $Id$
    Copyright (c) 2000 by Florian Klaempfl

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

{$i defines.inc}

interface

    uses
       node,
       symtype,symdef,symsym,symtable,
       cpubase;

    type
       tloadvmtnode = class(tunarynode)
          constructor create(l : tnode);virtual;
          function pass_1 : tnode;override;
          function det_resulttype:tnode;override;
       end;
       tloadvmtnodeclass = class of tloadvmtnode;

       thnewnode = class(tnode)
          constructor create;virtual;
          function pass_1 : tnode;override;
          function det_resulttype:tnode;override;
       end;
       thnewnodeclass = class of thnewnode;

       tnewnode = class(tunarynode)
          constructor create(l : tnode);virtual;
          function pass_1 : tnode;override;
          function det_resulttype:tnode;override;
       end;
       tnewnodeclass = class of tnewnode;

       thdisposenode = class(tunarynode)
          constructor create(l : tnode);virtual;
          function pass_1 : tnode;override;
          function det_resulttype:tnode;override;
       end;
       thdisposenodeclass = class of thdisposenode;

       tsimplenewdisposenode = class(tunarynode)
          constructor create(n : tnodetype;l : tnode);
          function pass_1 : tnode;override;
          function det_resulttype:tnode;override;
       end;
       tsimplenewdisposenodeclass = class of tsimplenewdisposenode;

       taddrnode = class(tunarynode)
          getprocvardef : tprocvardef;
          constructor create(l : tnode);virtual;
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
       end;
       tderefnodeclass = class of tderefnode;

       tsubscriptnode = class(tunarynode)
          vs : tvarsym;
          constructor create(varsym : tsym;l : tnode);virtual;
          function getcopy : tnode;override;
          function pass_1 : tnode;override;
          function docompare(p: tnode): boolean; override;
          function det_resulttype:tnode;override;
       end;
       tsubscriptnodeclass = class of tsubscriptnode;

       tvecnode = class(tbinarynode)
          constructor create(l,r : tnode);virtual;
          function pass_1 : tnode;override;
          function det_resulttype:tnode;override;
       end;
       tvecnodeclass = class of tvecnode;

       tselfnode = class(tnode)
          classdef : tobjectdef;
          constructor create(_class : tobjectdef);virtual;
          function pass_1 : tnode;override;
          function det_resulttype:tnode;override;
       end;
       tselfnodeclass = class of tselfnode;

       twithnode = class(tbinarynode)
          withsymtable : twithsymtable;
          tablecount : longint;
          withreference : preference;
          constructor create(symtable : twithsymtable;l,r : tnode;count : longint);virtual;
          destructor destroy;override;
          function getcopy : tnode;override;
          function pass_1 : tnode;override;
          function docompare(p: tnode): boolean; override;
          function det_resulttype:tnode;override;
       end;
       twithnodeclass = class of twithnode;

    var
       cloadvmtnode : tloadvmtnodeclass;
       chnewnode : thnewnodeclass;
       cnewnode : tnewnodeclass;
       chdisposenode : thdisposenodeclass;
       csimplenewdisposenode : tsimplenewdisposenodeclass;
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
      symconst,symbase,types,
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

    constructor thnewnode.create;
      begin
         inherited create(hnewn);
      end;


    function thnewnode.det_resulttype:tnode;
      begin
        result:=nil;
        resulttype:=voidtype;
      end;


    function thnewnode.pass_1 : tnode;
      begin
         result:=nil;
      end;


{*****************************************************************************
                              TNEWNODE
*****************************************************************************}

    constructor tnewnode.create(l : tnode);
      begin
         inherited create(newn,l);
      end;


    function tnewnode.det_resulttype:tnode;
      begin
        result:=nil;
        if assigned(left) then
         resulttypepass(left);
        resulttype:=voidtype;
      end;


    function tnewnode.pass_1 : tnode;
{$ifdef NEW_COMPILERPROC}
      var
        temp          : ttempcreatenode;
        newstatement  : tstatementnode;
        newblock      : tblocknode;
{$endif NEW_COMPILERPROC}
      begin
         result:=nil;
{$ifdef NEW_COMPILERPROC}
         { create the blocknode which will hold the generated statements + }
         { an initial dummy statement                                      }
         newstatement := cstatementnode.create(nil,cnothingnode.create);
         newblock := cblocknode.create(newstatement);

         { create temp for result }
         temp := ctempcreatenode.create(resulttype,
                                        resulttype.size,true);
         newstatement.left := cstatementnode.create(nil,temp);

         { create parameter }
         sizepara := ccallparanode.create(cordconstnode.create
             (tpointerdef(resulttype.def).pointertype.def.size,s32bittype),nil);

         { create the call and assign the result to dest  }
         { the assignment will take care of rangechecking }
         newstatement.left := cstatementnode.create(nil,cassignmentnode.create(
           ctemprefnode.create(tempcode),
           ccallnode.createintern('fpc_getmem',sizepara)));
         newstatement := tstatementnode(newstatement.left);

         if tpointerdef(resulttype.def).pointertype.def.needs_inittable then
          begin
            para := ccallparanode.create(cloadnode.create
                       (tpointerdef(resulttype.def).pointertype.def.size,s32bittype),
                    ccallparanode.create(cordconstnode.create
                       (tpointerdef(resulttype.def).pointertype.def.size,s32bittype),nil));
            newstatement.left := cstatementnode.create(nil,cassignmentnode.create(
              ctemprefnode.create(tempcode),
              ccallnode.createintern('fpc_initialize',sizepara)));
            newstatement := tstatementnode(newstatement.left);
                   new(r);
                   reset_reference(r^);
                   r^.symbol:=tstoreddef(tpointerdef(resulttype.def).pointertype.def).get_rtti_label(initrtti);
                   emitpushreferenceaddr(r^);
                   dispose(r);
                   { push pointer we just allocated, we need to initialize the
                     data located at that pointer not the pointer self (PFV) }
                   emit_push_loc(location);
                   emitcall('FPC_INITIALIZE');
          end;

         { and return it }
         result := newblock;
{$endif NEW_COMPILERPROC}

         if assigned(left) then
          begin
            firstpass(left);
            if codegenerror then
             exit;

            registers32:=left.registers32;
            registersfpu:=left.registersfpu;
{$ifdef SUPPORT_MMX}
            registersmmx:=left.registersmmx;
{$endif SUPPORT_MMX}
            location.loc:=LOC_REGISTER
          end
         else
          location.loc:=LOC_REFERENCE;
         procinfo^.flags:=procinfo^.flags or pi_do_call;
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
                        TSIMPLENEWDISPOSENODE
*****************************************************************************}

    constructor tsimplenewdisposenode.create(n : tnodetype;l : tnode);

      begin
         inherited create(n,l);
      end;


    function tsimplenewdisposenode.det_resulttype:tnode;
      begin
        result:=nil;
        resulttypepass(left);
        if codegenerror then
         exit;
        if (left.resulttype.def.deftype<>pointerdef) then
          CGMessage1(type_e_pointer_type_expected,left.resulttype.def.typename);
        resulttype:=voidtype;
      end;


    function tsimplenewdisposenode.pass_1 : tnode;
      begin
         result:=nil;
         { this cannot be in a register !! }
         make_not_regable(left);

         firstpass(left);
         if codegenerror then
          exit;

         registers32:=left.registers32;
         registersfpu:=left.registersfpu;
{$ifdef SUPPORT_MMX}
         registersmmx:=left.registersmmx;
{$endif SUPPORT_MMX}
         procinfo^.flags:=procinfo^.flags or pi_do_call;
      end;


{*****************************************************************************
                             TADDRNODE
*****************************************************************************}

    constructor taddrnode.create(l : tnode);

      begin
         inherited create(addrn,l);
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
                  hp3:=tabstractprocdef(tprocsym(tloadnode(left).symtableentry).defs^.def);

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
         if not(left.location.loc in [LOC_MEM,LOC_REFERENCE]) then
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
              if (left.location.loc<>LOC_MEM) and
                (left.location.loc<>LOC_REFERENCE) then
                CGMessage(cg_e_illegal_expression);
              set_location(location,left.location);
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

         { range check only for arrays }
         if (left.resulttype.def.deftype=arraydef) then
           begin
              if (isconvertable(right.resulttype.def,tarraydef(left.resulttype.def).rangetype.def,
                    ct,ordconstn,false)=0) and
                 not(is_equal(right.resulttype.def,tarraydef(left.resulttype.def).rangetype.def)) then
                CGMessage(type_e_mismatch);
           end;
         { Never convert a boolean or a char !}
         { maybe type conversion }
         if (right.resulttype.def.deftype<>enumdef) and
            not(is_char(right.resulttype.def)) and
            not(is_boolean(right.resulttype.def)) then
           begin
             inserttypeconv(right,s32bittype);
           end;

         { are we accessing a pointer[], then convert the pointer to
           an array first, in FPC this is allowed for all pointers in
           delphi/tp7 it's only allowed for pchars }
         if (left.resulttype.def.deftype=pointerdef) and
            ((m_fpc in aktmodeswitches) or
             is_pchar(left.resulttype.def) or
             is_pwidechar(left.resulttype.def)) then
          begin
            { convert pointer to array }
            htype.setdef(tarraydef.create(0,$7fffffff,s32bittype));
            tarraydef(htype.def).elementtype:=tpointerdef(left.resulttype.def).pointertype;
            inserttypeconv(left,htype);

            resulttype:=tarraydef(htype.def).elementtype;
          end;

         { determine return type }
         if not assigned(resulttype.def) then
           if left.resulttype.def.deftype=arraydef then
             resulttype:=tarraydef(left.resulttype.def).elementtype
           else if left.resulttype.def.deftype=stringdef then
             begin
                { indexed access to strings }
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
           location.loc:=LOC_MEM;
      end;


{*****************************************************************************
                               TSELFNODE
*****************************************************************************}

    constructor tselfnode.create(_class : tobjectdef);

      begin
         inherited create(selfn);
         classdef:=_class;
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
         withreference:=nil;
         set_file_line(l);
      end;


    destructor twithnode.destroy;
      var
        symt : tsymtable;
        i    : longint;
      begin
        symt:=withsymtable;
        for i:=1 to tablecount do
         begin
           if assigned(symt) then
            begin
              withsymtable:=twithsymtable(symt.next);
              symt.free;
            end;
           symt:=withsymtable;
         end;
        inherited destroy;
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
         symtable : twithsymtable;
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
                symtable.direct_with:=true;
               symtable.withnode:=self;
               symtable:=twithsymtable(symtable.next);
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
  cnewnode := tnewnode;
  chdisposenode := thdisposenode;
  csimplenewdisposenode := tsimplenewdisposenode;
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
  Revision 1.26  2002-04-01 20:57:13  jonas
    * fixed web bug 1907
    * fixed some other procvar related bugs (all related to accepting procvar
        constructs with either too many or too little parameters)
    (both merged, includes second typo fix of pexpr.pas)

  Revision 1.25  2001/12/06 17:57:34  florian
    + parasym to tparaitem added

  Revision 1.24  2001/12/03 21:48:42  peter
    * freemem change to value parameter
    * torddef low/high range changed to int64

  Revision 1.23  2001/11/02 22:58:02  peter
    * procsym definition rewrite

  Revision 1.22  2001/10/28 17:22:25  peter
    * allow assignment of overloaded procedures to procvars when we know
      which procedure to take

  Revision 1.20  2001/09/02 21:12:07  peter
    * move class of definitions into type section for delphi

  Revision 1.19  2001/08/26 13:36:42  florian
    * some cg reorganisation
    * some PPC updates

  Revision 1.18  2001/04/13 22:15:21  peter
    * removed wrongly placed set_varstate in subscriptnode

  Revision 1.17  2001/04/13 01:22:10  peter
    * symtable change to classes
    * range check generation and errors fixed, make cycle DEBUG=1 works
    * memory leaks fixed

  Revision 1.16  2001/04/02 21:20:31  peter
    * resulttype rewrite

  Revision 1.15  2001/03/23 00:16:07  florian
    + some stuff to compile FreeCLX added

  Revision 1.14  2000/12/31 11:14:11  jonas
    + implemented/fixed docompare() mathods for all nodes (not tested)
    + nopt.pas, nadd.pas, i386/n386opt.pas: optimized nodes for adding strings
      and constant strings/chars together
    * n386add.pas: don't copy temp strings (of size 256) to another temp string
      when adding

  Revision 1.13  2000/12/25 00:07:26  peter
    + new tlinkedlist class (merge of old tstringqueue,tcontainer and
      tlinkedlist objects)

  Revision 1.12  2000/12/05 15:19:50  jonas
    * fixed webbug 1268 ("merged")

  Revision 1.11  2000/11/29 00:30:34  florian
    * unused units removed from uses clause
    * some changes for widestrings

  Revision 1.10  2000/11/04 14:25:20  florian
    + merged Attila's changes for interfaces, not tested yet

  Revision 1.9  2000/10/31 22:02:49  peter
    * symtable splitted, no real code changes

  Revision 1.8  2000/10/21 18:16:11  florian
    * a lot of changes:
       - basic dyn. array support
       - basic C++ support
       - some work for interfaces done
       ....

  Revision 1.7  2000/10/14 21:52:55  peter
    * fixed memory leaks

  Revision 1.6  2000/10/14 10:14:51  peter
    * moehrendorf oct 2000 rewrite

  Revision 1.5  2000/10/01 19:48:24  peter
    * lot of compile updates for cg11

  Revision 1.4  2000/09/28 19:49:52  florian
  *** empty log message ***

  Revision 1.3  2000/09/25 15:37:14  florian
    * more fixes

  Revision 1.2  2000/09/25 15:05:25  florian
    * some updates

  Revision 1.1  2000/09/25 09:58:22  florian
    * first revision for testing purpose
}
