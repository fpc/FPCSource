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
       node,symtable,cpubase;

    type
       tloadvmtnode = class(tnode)
          constructor create;virtual;
          function pass_1 : tnode;override;
       end;

       thnewnode = class(tnode)
          constructor create;virtual;
          function pass_1 : tnode;override;
       end;

       tnewnode = class(tunarynode)
          constructor create(l : tnode);virtual;
          function pass_1 : tnode;override;
       end;

       thdisposenode = class(tunarynode)
          constructor create(l : tnode);virtual;
          function pass_1 : tnode;override;
       end;

       tsimplenewdisposenode = class(tunarynode)
          constructor create(n : tnodetype;l : tnode);
          function pass_1 : tnode;override;
       end;

       taddrnode = class(tunarynode)
          constructor create(l : tnode);virtual;
          function pass_1 : tnode;override;
       end;

       tdoubleaddrnode = class(tunarynode)
          constructor create(l : tnode);virtual;
          function pass_1 : tnode;override;
       end;

       tderefnode = class(tunarynode)
          constructor create(l : tnode);virtual;
          function pass_1 : tnode;override;
       end;

       tsubscriptnode = class(tunarynode)
          vs : pvarsym;
          constructor create(varsym : psym;l : tnode);virtual;
          function getcopy : tnode;
          function pass_1 : tnode;override;
       end;

       tvecnode = class(tbinarynode)
          constructor create(l,r : tnode);virtual;
          function pass_1 : tnode;override;
       end;

       tselfnode = class(tnode)
          constructor create(_class : pdef);virtual;
          function pass_1 : tnode;override;
       end;

       twithnode = class(tbinarynode)
{$IFDEF NEWST}
          withsymtables : Pcollection;
          withreference : preference;

{$ELSE}
          withsymtable : pwithsymtable;
          tablecount : longint;
          withreference:preference;
{$ENDIF NEWST}
          constructor create(symtable : pwithsymtable;l,r : tnode;count : longint);virtual;
          function getcopy : tnode;
          function pass_1 : tnode;override;
       end;

    function gensubscriptnode(varsym : pvarsym;l : tnode) : tsubscriptnode;
    function genselfnode(_class : pdef) : tselfnode;
{$IFDEF NEWST}
    function genwithnode(symtables:Pcollection;l,r : tnode) : twithnode;
{$ELSE}
    function genwithnode(symtable:pwithsymtable;l,r : tnode;count : longint) : twithnode;
{$ENDIF NEWST}

    var
       cloadvmtnode : class of tloadvmtnode;
       chnewnode : class of thnewnode;
       cnewnode : class of tnewnode;
       chdisposenode : class of thdisposenode;
       csimplenewdisposenode : class of tsimplenewdisposenode;
       caddrnode : class of taddrnode;
       cdoubleaddrnode : class of tdoubleaddrnode;
       cderefnode : class of tderefnode;
       csubscriptnode : class of tsubscriptnode;
       cvecnode : class of tvecnode;
       cselfnode : class of tselfnode;
       cwithnode : class of twithnode;

implementation

    uses
      globtype,systems,
      cutils,cobjects,verbose,globals,
      symconst,aasm,types,
      htypechk,pass_1,ncal
{$ifdef newcg}
      ,cgbase
{$else newcg}
      ,hcodegen
{$endif newcg}
      ;

    function genselfnode(_class : pdef) : tselfnode;

      var
         p : tnode;

      begin
         genselfnode:=cselfnode.create(_class);
      end;

{$IFDEF NEWST}
   function genwithnode(symtables:Pcollection;l,r : tnode) : tnode;

      var
         p : tnode;

      begin
         !!!!!!!!! fixme
         p:=getnode;
         disposetyp:=dt_with;
         treetype:=withn;
         left:=l;
         right:=r;
         registers32:=0;
{$ifdef SUPPORT_MMX}
         registersmmx:=0;
{$endif SUPPORT_MMX}
         resulttype:=nil;
         withsymtables:=symtables;
         withreference:=nil;
         set_file_line(l,p);
         genwithnode:=p;
      end;
{$ELSE}
   function genwithnode(symtable : pwithsymtable;l,r : tnode;count : longint) : twithnode;

      begin
         genwithnode:=cwithnode.create(symtable,l,r,count);
      end;
{$ENDIF NEWST}

    function gensubscriptnode(varsym : pvarsym;l : tnode) : tsubscriptnode;

      begin
         gensubscriptnode:=csubscriptnode.create(varsym,l);
      end;

{*****************************************************************************
                            TLOADVMTNODE
*****************************************************************************}

    constructor tloadvmtnode.create;

      begin
         inherited create(loadvmtn);
      end;

    function tloadvmtnode.pass_1 : tnode;
      begin
         pass_1:=nil;
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

    function thnewnode.pass_1 : tnode;
      begin
         pass_1:=nil;
      end;


{*****************************************************************************
                              TNEWNODE
*****************************************************************************}

    constructor tnewnode.create(l : tnode);

      begin
         inherited create(newn,l);
      end;

    function tnewnode.pass_1 : tnode;
      begin
         pass_1:=nil;
         if assigned(left) then
           firstpass(left);

         if codegenerror then
           exit;
         if assigned(left) then
           begin
              registers32:=left.registers32;
              registersfpu:=left.registersfpu;
{$ifdef SUPPORT_MMX}
              registersmmx:=left.registersmmx;
{$endif SUPPORT_MMX}
           end;
         { result type is already set }
         procinfo^.flags:=procinfo^.flags or pi_do_call;
         if assigned(left) then
           location.loc:=LOC_REGISTER
         else
           location.loc:=LOC_REFERENCE;
      end;


{*****************************************************************************
                            THDISPOSENODE
*****************************************************************************}

    constructor thdisposenode.create(l : tnode);

      begin
         inherited create(hdisposen,l);
      end;

    function thdisposenode.pass_1 : tnode;
      begin
         pass_1:=nil;
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
         resulttype:=ppointerdef(left.resulttype)^.pointertype.def;
      end;


{*****************************************************************************
                        TSIMPLENEWDISPOSENODE
*****************************************************************************}

    constructor tsimplenewdisposenode.create(n : tnodetype;l : tnode);

      begin
         inherited create(n,l);
      end;

    function tsimplenewdisposenode.pass_1 : tnode;
      begin
         pass_1:=nil;
         { this cannot be in a register !! }
         make_not_regable(left);

         firstpass(left);
         if codegenerror then
          exit;

         { check the type }
         if left.resulttype=nil then
          left.resulttype:=generrordef;
         if (left.resulttype^.deftype<>pointerdef) then
           CGMessage1(type_e_pointer_type_expected,left.resulttype^.typename);

         if (left.location.loc<>LOC_REFERENCE) {and
            (left.location.loc<>LOC_CREGISTER)} then
           CGMessage(cg_e_illegal_expression);

         registers32:=left.registers32;
         registersfpu:=left.registersfpu;
{$ifdef SUPPORT_MMX}
         registersmmx:=left.registersmmx;
{$endif SUPPORT_MMX}
         resulttype:=voiddef;
         procinfo^.flags:=procinfo^.flags or pi_do_call;
      end;


{*****************************************************************************
                             TADDRNODE
*****************************************************************************}

    constructor taddrnode.create(l : tnode);

      begin
         inherited create(addrn,l);
      end;

    function taddrnode.pass_1 : tnode;
      var
         hp  : tnode;
         hp2 : pparaitem;
         hp3 : pabstractprocdef;
      begin
         pass_1:=nil;
         make_not_regable(left);
         if not(assigned(resulttype)) then
           begin
              { tp @procvar support (type of @procvar is a void pointer)
                Note: we need to leave the addrn in the tree,
                else we can't see the difference between @procvar and procvar.
                we set the procvarload flag so a secondpass does nothing for
                this node (PFV) }
              if (m_tp_procvar in aktmodeswitches) then
               begin
                 hp:=left;
                 case hp.nodetype of
                   calln :
                     begin
                       { is it a procvar? }
                       hp:=tcallnode(hp).right;
                       if assigned(hp) then
                         begin
                           { remove calln node }
                           tcallnode(left).right:=nil;
                           left.free;
                           left:=hp;
                           firstpass(hp);
                           include(flags,nf_procvarload);
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
                       if hp.resulttype^.deftype=procvardef then
                         include(flags,nf_procvarload);
                     end;
                 end;
               end;
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
                 resulttype:=voidpointerdef;
                 exit;
               end;

              { proc 2 procvar ? }
              if left.nodetype=calln then
                begin
                  { generate a methodcallnode or proccallnode }
                  { we shouldn't convert things like @tcollection.load }
                  if (tcallnode(left).symtableprocentry^.owner^.symtabletype=objectsymtable) and
                    not(assigned(tcallnode(left).methodpointer) and (tcallnode(left).methodpointer.nodetype=typen)) then
                   begin
                     hp:=genloadmethodcallnode(pprocsym(tcallnode(left).symtableprocentry),tcallnode(left).symtableproc,
                       tcallnode(left).methodpointer.getcopy);
                     firstpass(hp);
                     pass_1:=hp;
                     exit;
                   end
                  else
                   hp:=genloadcallnode(pprocsym(tcallnode(left).symtableprocentry),
                     tcallnode(left).symtableproc);

                  { result is a procedure variable }
                  { No, to be TP compatible, you must return a pointer to
                    the procedure that is stored in the procvar.}
                  if not(m_tp_procvar in aktmodeswitches) then
                    begin
                       resulttype:=new(pprocvardef,init);

                    { it could also be a procvar, not only pprocsym ! }
                       if tcallnode(left).symtableprocentry^.typ=varsym then
                        hp3:=pabstractprocdef(pvarsym(tloadnode(left).symtableentry)^.vartype.def)
                       else
                        hp3:=pabstractprocdef(pprocsym(tcallnode(left).symtableprocentry)^.definition);

                       pprocvardef(resulttype)^.proctypeoption:=hp3^.proctypeoption;
                       pprocvardef(resulttype)^.proccalloptions:=hp3^.proccalloptions;
                       pprocvardef(resulttype)^.procoptions:=hp3^.procoptions;
                       pprocvardef(resulttype)^.rettype:=hp3^.rettype;
                       pprocvardef(resulttype)^.symtablelevel:=hp3^.symtablelevel;

                     { method ? then set the methodpointer flag }
                       if (hp3^.owner^.symtabletype=objectsymtable) and
                          (pobjectdef(hp3^.owner^.defowner)^.is_class) then
                         include(pprocvardef(resulttype)^.procoptions,po_methodpointer);
                       { we need to process the parameters reverse so they are inserted
                         in the correct right2left order (PFV) }
                       hp2:=pparaitem(hp3^.para^.last);
                       while assigned(hp2^.) do
                         begin
                            pprocvardef(resulttype)^.concatpara(hp2^.paratype,hp2^.paratyp,hp2^.defaultvalue);
                            hp2^.:=pparaitem(hp2^.previous);
                         end;
                    end
                  else
                    resulttype:=voidpointerdef;

                  disposetree(left);
                  left:=hp;
                end
              else
                begin
                  firstpass(left);
                  { what are we getting the address from an absolute sym? }
                  hp:=left;
                  while assigned(hp) and (hp.treetype in [vecn,derefn,subscriptn]) do
                   hp:=hp.left;
                  if assigned(hp) and (hp.treetype=loadn) and
                     ((hp.symtableentry^.typ=absolutesym) and
                      pabsolutesym(hp.symtableentry)^.absseg) then
                   begin
                     if not(cs_typed_addresses in aktlocalswitches) then
                       resulttype:=voidfarpointerdef
                     else
                       resulttype:=new(ppointerdef,initfardef(left.resulttype));
                   end
                  else
                   begin
                     if not(cs_typed_addresses in aktlocalswitches) then
                       resulttype:=voidpointerdef
                     else
                       resulttype:=new(ppointerdef,initdef(left.resulttype));
                   end;
                end;
           end;
         firstpass(left);
         { this is like the function addr }
         inc(parsing_para_level);
         set_varstate(left,false);
         dec(parsing_para_level);
         if codegenerror then
           exit;

         { don't allow constants }
         if is_constnode(left) then
          begin
            aktfilepos:=left.fileinfo;
            CGMessage(type_e_no_addr_of_constant);
          end
         else
           begin
             { we should allow loc_mem for @string }
             if not(left.location.loc in [LOC_MEM,LOC_REFERENCE]) then
               begin
                 aktfilepos:=left.fileinfo;
                 CGMessage(cg_e_illegal_expression);
               end;
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

    function tdoubleaddrnode.pass_1 : tnode;
      begin
         pass_1:=nil;
         make_not_regable(left);
         firstpass(left);
         inc(parsing_para_level);
         set_varstate(left,false);
         dec(parsing_para_level);
         if resulttype=nil then
           resulttype:=voidpointerdef;
         if codegenerror then
           exit;

         if (left.resulttype^.deftype)<>procvardef then
           CGMessage(cg_e_illegal_expression);

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

    function tderefnode.pass_1 : tnode;
      begin
         pass_1:=nil;
         firstpass(left);
         set_varstate(left,true);
         if codegenerror then
           begin
             resulttype:=generrordef;
             exit;
           end;

         registers32:=max(left.registers32,1);
         registersfpu:=left.registersfpu;
{$ifdef SUPPORT_MMX}
         registersmmx:=left.registersmmx;
{$endif SUPPORT_MMX}

         if left.resulttype^.deftype<>pointerdef then
          CGMessage(cg_e_invalid_qualifier);

         resulttype:=ppointerdef(left.resulttype)^.pointertype.def;
         location.loc:=LOC_REFERENCE;
      end;


{*****************************************************************************
                            TSUBSCRIPTNODE
*****************************************************************************}

    constructor tsubscriptnode.create(varsym : pvarsym;l : tnode);

      begin
         inherited create(subscriptn,l);
         vs:=varsym;
      end;

    function tsubscriptnode.getcopy : tnode;

      var
         p : tsubscriptnode;

      begin
         p:=tsubscriptnode(inherited getcopy);
         p.vs:=vs;
         getcopy:=p;
      end;

    function tsubscriptnode.pass_1 : tnode;
      begin
         pass_1:=nil;
         firstpass(left);
         if codegenerror then
           begin
             resulttype:=generrordef;
             exit;
           end;
         resulttype:=vs^.vartype.def;

         registers32:=left.registers32;
         registersfpu:=left.registersfpu;
{$ifdef SUPPORT_MMX}
         registersmmx:=left.registersmmx;
{$endif SUPPORT_MMX}
         { classes must be dereferenced implicit }
         if (left.resulttype^.deftype=objectdef) and
           pobjectdef(left.resulttype)^.is_class then
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


{*****************************************************************************
                               TVECNODE
*****************************************************************************}

    constructor tvecnode.create(l,r : tnode);

      begin
         inherited create(vecn,l,r);
      end;

    function tvecnode.pass_1 : tnode;
      var
         harr : pdef;
         ct : tconverttype;
{$ifdef consteval}
         tcsym : ptypedconstsym;
{$endif}
      begin
         pass_1:=nil;
         firstpass(left);
         firstpass(right);
         if codegenerror then
           exit;

         { range check only for arrays }
         if (left.resulttype^.deftype=arraydef) then
           begin
              if (isconvertable(right.resulttype,parraydef(left.resulttype)^.rangetype.def,
                    ct,ordconstn,false)=0) and
                 not(is_equal(right.resulttype,parraydef(left.resulttype)^.rangetype.def)) then
                CGMessage(type_e_mismatch);
           end;
         { Never convert a boolean or a char !}
         { maybe type conversion }
         if (right.resulttype^.deftype<>enumdef) and
            not(is_char(right.resulttype)) and
            not(is_boolean(right.resulttype)) then
           begin
             right:=gentypeconvnode(right,s32bitdef);
             firstpass(right);
             if codegenerror then
              exit;
           end;

         { are we accessing a pointer[], then convert the pointer to
           an array first, in FPC this is allowed for all pointers in
           delphi/tp7 it's only allowed for pchars }
         if (left.resulttype^.deftype=pointerdef) and
            ((m_fpc in aktmodeswitches) or
             is_pchar(left.resulttype)) then
          begin
            { convert pointer to array }
            harr:=new(parraydef,init(0,$7fffffff,s32bitdef));
            parraydef(harr)^.elementtype.def:=ppointerdef(left.resulttype)^.pointertype.def;
            left:=gentypeconvnode(left,harr);
            firstpass(left);
            if codegenerror then
             exit;
            resulttype:=parraydef(harr)^.elementtype.def
          end;

         { determine return type }
         if not assigned(resulttype) then
           if left.resulttype^.deftype=arraydef then
             resulttype:=parraydef(left.resulttype)^.elementtype.def
           else if left.resulttype^.deftype=stringdef then
             begin
                { indexed access to strings }
                case pstringdef(left.resulttype)^.string_typ of
                   {
                   st_widestring : resulttype:=cwchardef;
                   }
                   st_ansistring : resulttype:=cchardef;
                   st_longstring : resulttype:=cchardef;
                   st_shortstring : resulttype:=cchardef;
                end;
             end
           else
             CGMessage(type_e_array_required);

         { the register calculation is easy if a const index is used }
         if right.treetype=ordconstn then
           begin
{$ifdef consteval}
              { constant evaluation }
              if (left.treetype=loadn) and
                 (left.symtableentry^.typ=typedconstsym) then
               begin
                 tcsym:=ptypedconstsym(left.symtableentry);
                 if tcsym^.defintion^.typ=stringdef then
                  begin

                  end;
               end;
{$endif}
              registers32:=left.registers32;

              { for ansi/wide strings, we need at least one register }
              if is_ansistring(left.resulttype) or
                is_widestring(left.resulttype) then
                registers32:=max(registers32,1);
           end
         else
           begin
              { this rules are suboptimal, but they should give }
              { good results                                }
              registers32:=max(left.registers32,right.registers32);

              { for ansi/wide strings, we need at least one register }
              if is_ansistring(left.resulttype) or
                is_widestring(left.resulttype) then
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

    constructor tselfnode.create(_class : pdef);

      begin
         inherited create(selfn);
         resulttype:=_class;
      end;

    function tselfnode.pass_1 : tnode;
      begin
         pass_1:=nil;
         if (resulttype^.deftype=classrefdef) or
           ((resulttype^.deftype=objectdef)
             and pobjectdef(resulttype)^.is_class
           ) then
           location.loc:=LOC_CREGISTER
         else
           location.loc:=LOC_REFERENCE;
      end;


{*****************************************************************************
                               TWITHNODE
*****************************************************************************}

    constructor twithnode.create(symtable : pwithsymtable;l,r : tnode;count : longint);

      begin
         inherited create(withn,l,r);
         withsymtable:=symtable;
         tablecount:=count;
         withreference:=nil;
         set_file_line(l);
      end;

    function twithnode.getcopy : tnode;

      var
         p : twithnode;

      begin
         p:=twithnode(inherited getcopy);
         p.withsymtable:=withsymtable;
         p.tablecount:=count;
         p.withreference:=withreference;
      end;

    function twithnode.pass_1 : tnode;
      var
         symtable : pwithsymtable;
         i : longint;
      begin
         pass_1:=nil;
         if assigned(left) and assigned(right) then
            begin
               firstpass(left);
               left.unset_varstate;
               left.set_varstate(true);
               if codegenerror then
                 exit;
               symtable:=withsymtable;
               for i:=1 to tablecount do
                 begin
                    if (left.treetype=loadn) and
                       (left.symtable=aktprocsym^.definition^.localst) then
                      symtable^.direct_with:=true;
                    symtable^.withnode:=p;
                    symtable:=pwithsymtable(symtable^.next);
                  end;
               firstpass(right);
               if codegenerror then
                 exit;

               left_right_max(p);
               resulttype:=voiddef;
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
  Revision 1.2  2000-09-25 15:05:25  florian
    * some updates

  Revision 1.1  2000/09/25 09:58:22  florian
    * first revision for testing purpose
}