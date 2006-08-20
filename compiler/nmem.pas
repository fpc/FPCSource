{
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
       symdef,symsym,symtable,symtype;

    type
       tloadvmtaddrnode = class(tunarynode)
          constructor create(l : tnode);virtual;
          function pass_1 : tnode;override;
          function det_resulttype:tnode;override;
       end;
       tloadvmtaddrnodeclass = class of tloadvmtaddrnode;

       tloadparentfpnode = class(tunarynode)
          parentpd : tprocdef;
          parentpdderef : tderef;
          constructor create(pd:tprocdef);virtual;
          constructor ppuload(t:tnodetype;ppufile:tcompilerppufile);override;
          procedure ppuwrite(ppufile:tcompilerppufile);override;
          procedure buildderefimpl;override;
          procedure derefimpl;override;
          function pass_1 : tnode;override;
          function det_resulttype:tnode;override;
          function _getcopy : tnode;override;
       end;
       tloadparentfpnodeclass = class of tloadparentfpnode;

       taddrnode = class(tunarynode)
          getprocvardef : tprocvardef;
          getprocvardefderef : tderef;
          constructor create(l : tnode);virtual;
          constructor create_internal(l : tnode); virtual;
          constructor ppuload(t:tnodetype;ppufile:tcompilerppufile);override;
          procedure ppuwrite(ppufile:tcompilerppufile);override;
          procedure mark_write;override;
          procedure buildderefimpl;override;
          procedure derefimpl;override;
          function _getcopy : tnode;override;
          function pass_1 : tnode;override;
          function det_resulttype:tnode;override;
       end;
       taddrnodeclass = class of taddrnode;

       tderefnode = class(tunarynode)
          constructor create(l : tnode);virtual;
          function pass_1 : tnode;override;
          function det_resulttype:tnode;override;
          procedure mark_write;override;
       end;
       tderefnodeclass = class of tderefnode;

       tsubscriptnode = class(tunarynode)
          vs : tfieldvarsym;
          vsderef : tderef;
          constructor create(varsym : tsym;l : tnode);virtual;
          constructor ppuload(t:tnodetype;ppufile:tcompilerppufile);override;
          procedure ppuwrite(ppufile:tcompilerppufile);override;
          procedure buildderefimpl;override;
          procedure derefimpl;override;
          function _getcopy : tnode;override;
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

       twithnode = class(tunarynode)
          constructor create(l:tnode);
          destructor destroy;override;
          constructor ppuload(t:tnodetype;ppufile:tcompilerppufile);override;
          procedure ppuwrite(ppufile:tcompilerppufile);override;
          function _getcopy : tnode;override;
          function pass_1 : tnode;override;
          function docompare(p: tnode): boolean; override;
          function det_resulttype:tnode;override;
       end;
       twithnodeclass = class of twithnode;

    var
       cloadvmtaddrnode : tloadvmtaddrnodeclass;
       cloadparentfpnode : tloadparentfpnodeclass;
       caddrnode : taddrnodeclass;
       cderefnode : tderefnodeclass;
       csubscriptnode : tsubscriptnodeclass;
       cvecnode : tvecnodeclass;
       cwithnode : twithnodeclass;

implementation

    uses
      globtype,systems,
      cutils,verbose,globals,
      symconst,symbase,defutil,defcmp,
      nbas,nutils,
      htypechk,pass_1,ncal,nld,ncon,ncnv,cgbase,procinfo
      ;

{*****************************************************************************
                            TLOADVMTADDRNODE
*****************************************************************************}

    constructor tloadvmtaddrnode.create(l : tnode);
      begin
         inherited create(loadvmtaddrn,l);
      end;


    function tloadvmtaddrnode.det_resulttype:tnode;
      begin
        result:=nil;
        resulttypepass(left);
        if codegenerror then
         exit;

        case left.resulttype.def.deftype of
          classrefdef :
            resulttype:=left.resulttype;
          objectdef :
            resulttype.setdef(tclassrefdef.create(left.resulttype));
          else
            Message(parser_e_pointer_to_class_expected);
        end;
      end;


    function tloadvmtaddrnode.pass_1 : tnode;
      begin
         result:=nil;
         expectloc:=LOC_REGISTER;
         if left.nodetype<>typen then
           begin
             firstpass(left);
             registersint:=left.registersint;
           end;
         if registersint<1 then
           registersint:=1;
      end;


{*****************************************************************************
                        TLOADPARENTFPNODE
*****************************************************************************}

    constructor tloadparentfpnode.create(pd:tprocdef);
      begin
        inherited create(loadparentfpn,nil);
        if not assigned(pd) then
          internalerror(200309288);
        if (pd.parast.symtablelevel>current_procinfo.procdef.parast.symtablelevel) then
          internalerror(200309284);
        parentpd:=pd;
      end;


    constructor tloadparentfpnode.ppuload(t:tnodetype;ppufile:tcompilerppufile);
      begin
        inherited ppuload(t,ppufile);
        ppufile.getderef(parentpdderef);
      end;


    procedure tloadparentfpnode.ppuwrite(ppufile:tcompilerppufile);
      begin
        inherited ppuwrite(ppufile);
        ppufile.putderef(parentpdderef);
      end;


    procedure tloadparentfpnode.buildderefimpl;
      begin
        inherited buildderefimpl;
        parentpdderef.build(parentpd);
      end;


    procedure tloadparentfpnode.derefimpl;
      begin
        inherited derefimpl;
        parentpd:=tprocdef(parentpdderef.resolve);
      end;


    function tloadparentfpnode._getcopy : tnode;
      var
         p : tloadparentfpnode;
      begin
         p:=tloadparentfpnode(inherited _getcopy);
         p.parentpd:=parentpd;
         _getcopy:=p;
      end;


    function tloadparentfpnode.det_resulttype:tnode;
{$ifdef dummy}
      var
        currpi : tprocinfo;
        hsym   : tparavarsym;
{$endif dummy}
      begin
        result:=nil;
        resulttype:=voidpointertype;
{$ifdef dummy}
        { currently parentfps are never loaded in registers (FK) }

        if (current_procinfo.procdef.parast.symtablelevel<>parentpd.parast.symtablelevel) then
          begin
            currpi:=current_procinfo;
            { walk parents }
            while (currpi.procdef.owner.symtablelevel>parentpd.parast.symtablelevel) do
              begin
                currpi:=currpi.parent;
                if not assigned(currpi) then
                  internalerror(2005040602);
                hsym:=tparavarsym(currpi.procdef.parast.search('parentfp'));
                if not assigned(hsym) then
                  internalerror(2005040601);
                hsym.varregable:=vr_none;
              end;
          end;
{$endif dummy}
      end;


    function tloadparentfpnode.pass_1 : tnode;
      begin
        result:=nil;
        expectloc:=LOC_REGISTER;
        registersint:=1;
      end;


{*****************************************************************************
                             TADDRNODE
*****************************************************************************}

    constructor taddrnode.create(l : tnode);

      begin
         inherited create(addrn,l);
         getprocvardef:=nil;
      end;


    constructor taddrnode.create_internal(l : tnode);
      begin
        self.create(l);
        include(flags,nf_internal);
      end;


    constructor taddrnode.ppuload(t:tnodetype;ppufile:tcompilerppufile);
      begin
        inherited ppuload(t,ppufile);
        ppufile.getderef(getprocvardefderef);
      end;


    procedure taddrnode.ppuwrite(ppufile:tcompilerppufile);
      begin
        inherited ppuwrite(ppufile);
        ppufile.putderef(getprocvardefderef);
      end;

    procedure Taddrnode.mark_write;

    begin
      {@procvar:=nil is legal in Delphi mode.}
      left.mark_write;
    end;

    procedure taddrnode.buildderefimpl;
      begin
        inherited buildderefimpl;
        getprocvardefderef.build(getprocvardef);
      end;


    procedure taddrnode.derefimpl;
      begin
        inherited derefimpl;
        getprocvardef:=tprocvardef(getprocvardefderef.resolve);
      end;


    function taddrnode._getcopy : tnode;

      var
         p : taddrnode;

      begin
         p:=taddrnode(inherited _getcopy);
         p.getprocvardef:=getprocvardef;
         _getcopy:=p;
      end;


    function taddrnode.det_resulttype:tnode;
      var
         hp  : tnode;
         hsym : tfieldvarsym;
         isprocvar : boolean;
      begin
        result:=nil;
        resulttypepass(left);
        if codegenerror then
         exit;

        make_not_regable(left,vr_addr);

        { don't allow constants }
        if is_constnode(left) then
         begin
           aktfilepos:=left.fileinfo;
           CGMessage(type_e_no_addr_of_constant);
           exit;
         end;

        { Handle @proc special, also @procvar in tp-mode needs
          special handling }
        if (left.resulttype.def.deftype=procdef) or
           (
            (left.resulttype.def.deftype=procvardef) and
            ((m_tp_procvar in aktmodeswitches) or
             (m_mac_procvar in aktmodeswitches))
           ) then
          begin
            isprocvar:=(left.resulttype.def.deftype=procvardef);

            if not isprocvar then
              begin
                left:=ctypeconvnode.create_proc_to_procvar(left);
                resulttypepass(left);
              end;

            { In tp procvar mode the result is always a voidpointer. Insert
              a typeconversion to voidpointer. For methodpointers we need
              to load the proc field }
            if (m_tp_procvar in aktmodeswitches) or
               (m_mac_procvar in aktmodeswitches) then
              begin
                if tabstractprocdef(left.resulttype.def).is_addressonly then
                  begin
                    result:=ctypeconvnode.create_internal(left,voidpointertype);
                    include(result.flags,nf_load_procvar);
                    left:=nil;
                  end
                else
                  begin
                    { For procvars we need to return the proc field of the
                      methodpointer }
                    if isprocvar then
                      begin
                        { find proc field in methodpointer record }
                        hsym:=tfieldvarsym(trecorddef(methodpointertype.def).symtable.search('proc'));
                        if not assigned(hsym) then
                          internalerror(200412041);
                        { Load tmehodpointer(left).proc }
                        result:=csubscriptnode.create(
                                     hsym,
                                     ctypeconvnode.create_internal(left,methodpointertype));
                        left:=nil;
                      end
                    else
                      CGMessage(type_e_variable_id_expected);
                  end;
              end
            else
              begin
                { Return the typeconvn only }
                result:=left;
                left:=nil;
              end;
          end
        else
          begin
            { what are we getting the address from an absolute sym? }
            hp:=left;
            while assigned(hp) and (hp.nodetype in [typeconvn,vecn,derefn,subscriptn]) do
              hp:=tunarynode(hp).left;
            if not assigned(hp) then
              internalerror(200412042);
{$ifdef i386}
            if (hp.nodetype=loadn) and
               ((tloadnode(hp).symtableentry.typ=absolutevarsym) and
               tabsolutevarsym(tloadnode(hp).symtableentry).absseg) then
              begin
                if not(nf_typedaddr in flags) then
                  resulttype:=voidfarpointertype
                else
                  resulttype.setdef(tpointerdef.createfar(left.resulttype));
              end
            else
{$endif i386}
              if (nf_internal in flags) or
                 valid_for_addr(left,true) then
                begin
                  if not(nf_typedaddr in flags) then
                    resulttype:=voidpointertype
                  else
                    resulttype.setdef(tpointerdef.create(left.resulttype));
                end
            else
              CGMessage(type_e_variable_id_expected);
          end;

         { this is like the function addr }
         inc(parsing_para_level);
         { This is actually only "read", but treat it nevertheless as  }
         { modified due to the possible use of pointers                }
         { To avoid false positives regarding "uninitialised"          }
         { warnings when using arrays, perform it in two steps         }
         set_varstate(left,vs_written,[]);
         set_varstate(left,vs_read,[]);
         dec(parsing_para_level);
      end;


    function taddrnode.pass_1 : tnode;
      begin
         result:=nil;
         firstpass(left);
         if codegenerror then
          exit;

         registersint:=left.registersint;
         registersfpu:=left.registersfpu;
{$ifdef SUPPORT_MMX}
         registersmmx:=left.registersmmx;
{$endif SUPPORT_MMX}
         if registersint<1 then
           registersint:=1;
         { is this right for object of methods ?? }
         expectloc:=LOC_REGISTER;
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
         set_varstate(left,vs_read,[vsf_must_be_valid]);
         if codegenerror then
          exit;

         { tp procvar support }
         maybe_call_procvar(left,true);

         if left.resulttype.def.deftype=pointerdef then
          resulttype:=tpointerdef(left.resulttype.def).pointertype
         else
          CGMessage(parser_e_invalid_qualifier);
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

         registersint:=max(left.registersint,1);
         registersfpu:=left.registersfpu;
{$ifdef SUPPORT_MMX}
         registersmmx:=left.registersmmx;
{$endif SUPPORT_MMX}

         expectloc:=LOC_REFERENCE;
      end;


{*****************************************************************************
                            TSUBSCRIPTNODE
*****************************************************************************}

    constructor tsubscriptnode.create(varsym : tsym;l : tnode);

      begin
         inherited create(subscriptn,l);
         { vs should be changed to tsym! }
         vs:=tfieldvarsym(varsym);
      end;

    constructor tsubscriptnode.ppuload(t:tnodetype;ppufile:tcompilerppufile);
      begin
        inherited ppuload(t,ppufile);
        ppufile.getderef(vsderef);
      end;


    procedure tsubscriptnode.ppuwrite(ppufile:tcompilerppufile);
      begin
        inherited ppuwrite(ppufile);
        ppufile.putderef(vsderef);
      end;


    procedure tsubscriptnode.buildderefimpl;
      begin
        inherited buildderefimpl;
        vsderef.build(vs);
      end;


    procedure tsubscriptnode.derefimpl;
      begin
        inherited derefimpl;
        vs:=tfieldvarsym(vsderef.resolve);
      end;


    function tsubscriptnode._getcopy : tnode;

      var
         p : tsubscriptnode;

      begin
         p:=tsubscriptnode(inherited _getcopy);
         p.vs:=vs;
         _getcopy:=p;
      end;


    function tsubscriptnode.det_resulttype:tnode;
      begin
        result:=nil;
        resulttypepass(left);
        { tp procvar support }
        maybe_call_procvar(left,true);
        resulttype:=vs.vartype;

        // don't put records from which we load fields which aren't regable in integer registers
        if (left.resulttype.def.deftype = recorddef) and
           not(tstoreddef(resulttype.def).is_intregable) then
          make_not_regable(left,vr_addr);
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

         registersint:=left.registersint;
         registersfpu:=left.registersfpu;
{$ifdef SUPPORT_MMX}
         registersmmx:=left.registersmmx;
{$endif SUPPORT_MMX}
         { classes must be dereferenced implicit }
         if is_class_or_interface(left.resulttype.def) then
           begin
              if registersint=0 then
                registersint:=1;
              expectloc:=LOC_REFERENCE;
           end
         else
           begin
             case left.expectloc of
               LOC_REGISTER,
               LOC_SUBSETREG:
                 // can happen for function results on win32 and darwin/x86
                 if (left.resulttype.def.size > sizeof(aint)) then
                   expectloc:=LOC_REFERENCE
                 else
                   expectloc:=LOC_SUBSETREG;
               LOC_CREGISTER,
               LOC_CSUBSETREG:
                 expectloc:=LOC_CSUBSETREG;
               LOC_REFERENCE,
               LOC_CREFERENCE:
                 expectloc:=left.expectloc;
               else internalerror(20060521);
              end;
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
         valid : boolean;
      begin
         result:=nil;
         resulttypepass(left);
         resulttypepass(right);

         { implicitly convert stringconstant to stringdef,
           see tbs/tb0476.pp for a test }
         if (left.nodetype=stringconstn) and
            (tstringconstnode(left).cst_type=cst_conststring) then
           begin
             if tstringconstnode(left).len>255 then
               inserttypeconv(left,cansistringtype)
             else
               inserttypeconv(left,cshortstringtype);
           end;

         { In p[1] p is always valid, it is not possible to
           declared a shortstring or normal array that has
           undefined number of elements. Dynamic array and
           ansi/widestring needs to be valid }
         valid:=is_dynamic_array(left.resulttype.def) or
                is_ansistring(left.resulttype.def) or
                is_widestring(left.resulttype.def) or
                { implicit pointer dereference -> pointer is read }
                (left.resulttype.def.deftype = pointerdef);
         if valid then
           set_varstate(left,vs_read,[vsf_must_be_valid]);
{
         A vecn is, just like a loadn, always part of an expression with its
         own read/write and must_be_valid semantics. Therefore we don't have
         to do anything else here, just like for loadn's
}
         set_varstate(right,vs_read,[vsf_must_be_valid]);
         if codegenerror then
          exit;

         { maybe type conversion for the index value, but
           do not convert enums,booleans,char }
         if ((right.resulttype.def.deftype<>enumdef) and
             not(is_char(right.resulttype.def) or is_widechar(right.resulttype.def)) and
             not(is_boolean(right.resulttype.def))) or
            (left.resulttype.def.deftype <> arraydef) then
           begin
             inserttypeconv(right,sinttype);
           end;

         case left.resulttype.def.deftype of
           arraydef :
             begin
               { check type of the index value }
               if (compare_defs(right.resulttype.def,tarraydef(left.resulttype.def).rangetype.def,right.nodetype)=te_incompatible) then
                 IncompatibleTypes(right.resulttype.def,tarraydef(left.resulttype.def).rangetype.def);
               resulttype:=tarraydef(left.resulttype.def).elementtype;
             end;
           pointerdef :
             begin
               { are we accessing a pointer[], then convert the pointer to
                 an array first, in FPC this is allowed for all pointers
                 (except voidpointer) in delphi/tp7 it's only allowed for pchars. }
               if not is_voidpointer(left.resulttype.def) and
                  (
                   (m_fpc in aktmodeswitches) or
                   is_pchar(left.resulttype.def) or
                   is_pwidechar(left.resulttype.def)
                  ) then
                begin
                  { convert pointer to array }
                  htype.setdef(tarraydef.create_from_pointer(tpointerdef(left.resulttype.def).pointertype));
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
             end;
           variantdef :
             resulttype:=cvarianttype;
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

         if (nf_callunique in flags) and
            (is_ansistring(left.resulttype.def) or
             (is_widestring(left.resulttype.def) and not(tf_winlikewidestring in target_info.flags))) then
           begin
             left := ctypeconvnode.create_internal(ccallnode.createintern('fpc_'+tstringdef(left.resulttype.def).stringtypname+'_unique',
               ccallparanode.create(
                 ctypeconvnode.create_internal(left,voidpointertype),nil)),
               left.resulttype);
             firstpass(left);
             { double resulttype passes somwhere else may cause this to be }
             { reset though :/                                             }
             exclude(flags,nf_callunique);
           end
         else if is_widestring(left.resulttype.def) and (tf_winlikewidestring in target_info.flags) then
           exclude(flags,nf_callunique);

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
              registersint:=left.registersint;

              { for ansi/wide strings, we need at least one register }
              if is_ansistring(left.resulttype.def) or
                is_widestring(left.resulttype.def) or
              { ... as well as for dynamic arrays }
                is_dynamic_array(left.resulttype.def) then
                registersint:=max(registersint,1);
           end
         else
           begin
              { this rules are suboptimal, but they should give }
              { good results                                }
              registersint:=max(left.registersint,right.registersint);

              { for ansi/wide strings, we need at least one register }
              if is_ansistring(left.resulttype.def) or
                is_widestring(left.resulttype.def) or
              { ... as well as for dynamic arrays }
                is_dynamic_array(left.resulttype.def) then
                registersint:=max(registersint,1);

              { need we an extra register when doing the restore ? }
              if (left.registersint<=right.registersint) and
              { only if the node needs less than 3 registers }
              { two for the right node and one for the       }
              { left address                             }
                (registersint<3) then
                inc(registersint);

              { need we an extra register for the index ? }
              if (right.expectloc<>LOC_REGISTER)
              { only if the right node doesn't need a register }
                and (right.registersint<1) then
                inc(registersint);

              { not correct, but what works better ?
              if left.registersint>0 then
                registersint:=max(registersint,2)
              else
                 min. one register
                registersint:=max(registersint,1);
              }
           end;
         registersfpu:=max(left.registersfpu,right.registersfpu);
{$ifdef SUPPORT_MMX}
         registersmmx:=max(left.registersmmx,right.registersmmx);
{$endif SUPPORT_MMX}
         if (not is_packed_array(left.resulttype.def)) or
            ((tarraydef(left.resulttype.def).elepackedbitsize mod 8) = 0) then
           if left.expectloc=LOC_CREFERENCE then
             expectloc:=LOC_CREFERENCE
           else
             expectloc:=LOC_REFERENCE
         else
           if left.expectloc=LOC_CREFERENCE then
             expectloc:=LOC_CSUBSETREF
           else
             expectloc:=LOC_SUBSETREF;
      end;


{*****************************************************************************
                               TWITHNODE
*****************************************************************************}

    constructor twithnode.create(l:tnode);
      begin
         inherited create(withn,l);
         fileinfo:=l.fileinfo;
      end;


    destructor twithnode.destroy;
      begin
        inherited destroy;
      end;


    constructor twithnode.ppuload(t:tnodetype;ppufile:tcompilerppufile);
      begin
        inherited ppuload(t,ppufile);
      end;


    procedure twithnode.ppuwrite(ppufile:tcompilerppufile);
      begin
        inherited ppuwrite(ppufile);
      end;


    function twithnode._getcopy : tnode;
      var
         p : twithnode;
      begin
         p:=twithnode(inherited _getcopy);
         result:=p;
      end;


    function twithnode.det_resulttype:tnode;
      begin
        result:=nil;
        resulttype:=voidtype;
        if assigned(left) then
          resulttypepass(left);
      end;


    function twithnode.pass_1 : tnode;
      begin
        result:=nil;
        expectloc:=LOC_VOID;
        registersint:=left.registersint;
        registersfpu:=left.registersfpu;
{$ifdef SUPPORT_MMX}
        registersmmx:=left.registersmmx;
{$endif SUPPORT_MMX}
      end;


    function twithnode.docompare(p: tnode): boolean;
      begin
        docompare :=
          inherited docompare(p);
      end;

begin
  cloadvmtaddrnode := tloadvmtaddrnode;
  caddrnode := taddrnode;
  cderefnode := tderefnode;
  csubscriptnode := tsubscriptnode;
  cvecnode := tvecnode;
  cwithnode := twithnode;
end.
