{
    Copyright (c) 1998-2002 by Florian Klaempfl

    This unit exports some help routines for the type checking

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
unit htypechk;

{$i fpcdefs.inc}

interface

    uses
      tokens,cpuinfo,
      node,globtype,
      symconst,symtype,symdef,symsym,symbase;

    type
      Ttok2nodeRec=record
        tok : ttoken;
        nod : tnodetype;
        op_overloading_supported : boolean;
      end;

      pcandidate = ^tcandidate;
      tcandidate = record
         next         : pcandidate;
         data         : tprocdef;
         wrongparaidx,
         firstparaidx : integer;
         exact_count,
         equal_count,
         cl1_count,
         cl2_count,
         cl3_count,
         cl4_count,
         cl5_count,
         coper_count : integer; { should be signed }
         ordinal_distance : double;
         invalid     : boolean;
         wrongparanr : byte;
      end;

      tcallcandidates = class
      private
        FProcSym    : tprocsym;
        FProcs      : pcandidate;
        FProcVisibleCnt,
        FProcCnt    : integer;
        FParaNode   : tnode;
        FParaLength : smallint;
        FAllowVariant : boolean;
        function proc_add(ps:tprocsym;pd:tprocdef):pcandidate;
      public
        constructor create(sym:tprocsym;st:TSymtable;ppn:tnode;isprop,ignorevis : boolean);
        constructor create_operator(op:ttoken;ppn:tnode);
        destructor destroy;override;
        procedure list(all:boolean);
{$ifdef EXTDEBUG}
        procedure dump_info(lvl:longint);
{$endif EXTDEBUG}
        procedure get_information;
        function  choose_best(var bestpd:tabstractprocdef; singlevariant: boolean):integer;
        procedure find_wrong_para;
        property  Count:integer read FProcCnt;
        property  VisibleCount:integer read FProcVisibleCnt;
      end;

    type
      tregableinfoflag = (
         // can be put in a register if it's the address of a var/out/const parameter
         ra_addr_regable,
         // orthogonal to above flag: the address of the node is taken and may
         // possibly escape the block in which this node is declared (e.g. a
         // local variable is passed as var parameter to another procedure)
         ra_addr_taken);
      tregableinfoflags = set of tregableinfoflag;

    const
      tok2nodes=24;
      tok2node:array[1..tok2nodes] of ttok2noderec=(
        (tok:_PLUS    ;nod:addn;op_overloading_supported:true),      { binary overloading supported }
        (tok:_MINUS   ;nod:subn;op_overloading_supported:true),      { binary and unary overloading supported }
        (tok:_STAR    ;nod:muln;op_overloading_supported:true),      { binary overloading supported }
        (tok:_SLASH   ;nod:slashn;op_overloading_supported:true),    { binary overloading supported }
        (tok:_EQUAL   ;nod:equaln;op_overloading_supported:true),    { binary overloading supported }
        (tok:_GT      ;nod:gtn;op_overloading_supported:true),       { binary overloading supported }
        (tok:_LT      ;nod:ltn;op_overloading_supported:true),       { binary overloading supported }
        (tok:_GTE     ;nod:gten;op_overloading_supported:true),      { binary overloading supported }
        (tok:_LTE     ;nod:lten;op_overloading_supported:true),      { binary overloading supported }
        (tok:_SYMDIF  ;nod:symdifn;op_overloading_supported:true),   { binary overloading supported }
        (tok:_STARSTAR;nod:starstarn;op_overloading_supported:true), { binary overloading supported }
        (tok:_OP_AS     ;nod:asn;op_overloading_supported:false),     { binary overloading NOT supported }
        (tok:_OP_IN     ;nod:inn;op_overloading_supported:false),     { binary overloading NOT supported }
        (tok:_OP_IS     ;nod:isn;op_overloading_supported:false),     { binary overloading NOT supported }
        (tok:_OP_OR     ;nod:orn;op_overloading_supported:true),     { binary overloading supported }
        (tok:_OP_AND    ;nod:andn;op_overloading_supported:true),    { binary overloading supported }
        (tok:_OP_DIV    ;nod:divn;op_overloading_supported:true),    { binary overloading supported }
        (tok:_OP_NOT    ;nod:notn;op_overloading_supported:true),    { unary overloading supported }
        (tok:_OP_MOD    ;nod:modn;op_overloading_supported:true),    { binary overloading supported }
        (tok:_OP_SHL    ;nod:shln;op_overloading_supported:true),    { binary overloading supported }
        (tok:_OP_SHR    ;nod:shrn;op_overloading_supported:true),    { binary overloading supported }
        (tok:_OP_XOR    ;nod:xorn;op_overloading_supported:true),    { binary overloading supported }
        (tok:_ASSIGNMENT;nod:assignn;op_overloading_supported:true), { unary overloading supported }
        (tok:_UNEQUAL ;nod:unequaln;op_overloading_supported:false)   { binary overloading NOT supported  overload = instead }
      );
    const
      allow_array_constructor : boolean = false;

    function node2opstr(nt:tnodetype):string;

    { check operator args and result type }
    function isbinaryoperatoroverloadable(treetyp:tnodetype;ld:tdef;lt:tnodetype;rd:tdef;rt:tnodetype) : boolean;
    function isoperatoracceptable(pf : tprocdef; optoken : ttoken) : boolean;
    function isunaryoverloaded(var t : tnode) : boolean;
    function isbinaryoverloaded(var t : tnode) : boolean;

    { Register Allocation }
    procedure make_not_regable(p : tnode; how: tregableinfoflags);

    { procvar handling }
    function  is_procvar_load(p:tnode):boolean;
    procedure test_local_to_procvar(from_def:tprocvardef;to_def:tdef);

    { sets varsym varstate field correctly }
    type
      tvarstateflag = (vsf_must_be_valid,vsf_use_hints);
      tvarstateflags = set of tvarstateflag;
    procedure set_varstate(p:tnode;newstate:tvarstate;varstateflags:tvarstateflags);

    { sets the callunique flag, if the node is a vecn, }
    { takes care of type casts etc.                 }
    procedure set_unique(p : tnode);

    function  valid_for_formal_var(p : tnode; report_errors: boolean) : boolean;
    function  valid_for_formal_const(p : tnode; report_errors: boolean) : boolean;
    function  valid_for_var(p:tnode; report_errors: boolean):boolean;
    function  valid_for_assignment(p:tnode; report_errors: boolean):boolean;
    function  valid_for_loopvar(p:tnode; report_errors: boolean):boolean;
    function  valid_for_addr(p : tnode; report_errors: boolean) : boolean;

    function allowenumop(nt:tnodetype):boolean;

    procedure check_hints(const srsym: tsym; const symoptions: tsymoptions);

    procedure check_ranges(const location: tfileposinfo; source: tnode; destdef: tdef);

implementation

    uses
       sysutils,
       systems,constexp,globals,
       cutils,cclasses,verbose,
       symtable,
       defutil,defcmp,
       nbas,ncnv,nld,nmem,ncal,nmat,ninl,nutils,ncon,
       cgbase,procinfo
       ;

    type
      TValidAssign=(Valid_Property,Valid_Void,Valid_Const,Valid_Addr,Valid_Packed);
      TValidAssigns=set of TValidAssign;


    function node2opstr(nt:tnodetype):string;
      var
        i : integer;
      begin
        result:='<unknown>';
        for i:=1 to tok2nodes do
          if tok2node[i].nod=nt then
            begin
              result:=tokeninfo^[tok2node[i].tok].str;
              break;
            end;
       end;


    function isbinaryoperatoroverloadable(treetyp:tnodetype;ld:tdef;lt:tnodetype;rd:tdef;rt:tnodetype) : boolean;

        function internal_check(treetyp:tnodetype;ld:tdef;lt:tnodetype;rd:tdef;rt:tnodetype;var allowed:boolean):boolean;
        begin
          internal_check:=true;
          case ld.typ of
            formaldef,
            recorddef,
            variantdef :
              begin
                allowed:=true;
              end;
            procvardef :
              begin
                if (rd.typ in [pointerdef,procdef,procvardef]) then
                 begin
                   allowed:=false;
                   exit;
                 end;
                allowed:=true;
              end;
            pointerdef :
              begin
                if ((rd.typ in [orddef,enumdef,pointerdef,classrefdef,procvardef]) or
                    is_class_or_interface(rd)) then
                 begin
                   allowed:=false;
                   exit;
                 end;

                { don't allow pchar+string }
                if (is_pchar(ld) or is_pwidechar(ld)) and
                   ((rd.typ=stringdef) or
                    is_pchar(rd) or
                    is_pwidechar(rd) or
                    is_chararray(rd) or
                    is_widechararray(rd)) then
                 begin
                   allowed:=false;
                   exit;
                 end;
                allowed:=true;
              end;
            arraydef :
              begin
                { not vector/mmx }
                if ((cs_mmx in current_settings.localswitches) and
                   is_mmx_able_array(ld)) or
                   ((cs_support_vectors in current_settings.globalswitches) and
                   is_vector(ld)) then
                 begin
                   allowed:=false;
                   exit;
                 end;
                { not chararray+[(wide)char,(wide)string,(wide)chararray] }
                if (is_chararray(ld) or is_widechararray(ld) or
                    is_open_chararray(ld) or is_open_widechararray(ld))
                   and
                   ((rd.typ in [stringdef,orddef,enumdef]) or
                    is_pchar(rd) or
                    is_pwidechar(rd) or
                    is_chararray(rd) or
                    is_widechararray(rd) or
                    is_open_chararray(rd) or
                    is_open_widechararray(rd) or
                    (rt=niln)) then
                 begin
                   allowed:=false;
                   exit;
                 end;
                { dynamic array compare with niln }
                if ((is_dynamic_array(ld) and
                   (rt=niln)) or
                   (is_dynamic_array(ld) and is_dynamic_array(rd)))
                   and
                   (treetyp in [equaln,unequaln]) then
                 begin
                   allowed:=false;
                   exit;
                 end;
                allowed:=true;
              end;
            objectdef :
              begin
                { <> and = are defined for classes }
                if (treetyp in [equaln,unequaln]) and
                   is_class_or_interface(ld) then
                 begin
                   allowed:=false;
                   exit;
                 end;
                allowed:=true;
              end;
            stringdef :
              begin
                if (rd.typ in [orddef,enumdef,stringdef]) or
                   is_pchar(rd) or
                   is_pwidechar(rd) or
                   is_chararray(rd) or
                   is_widechararray(rd) or
                   is_open_chararray(rd) or
                   is_open_widechararray(rd) then
                 begin
                   allowed:=false;
                   exit;
                 end;
                allowed:=true;
              end;
            else
              internal_check:=false;
          end;
        end;

      var
        allowed : boolean;
      begin
        { power ** is always possible }
        if (treetyp=starstarn) then
         begin
           isbinaryoperatoroverloadable:=true;
           exit;
         end;
        { order of arguments does not matter so we have to check also
          the reversed order }
        allowed:=false;
        if not internal_check(treetyp,ld,lt,rd,rt,allowed) then
          internal_check(treetyp,rd,rt,ld,lt,allowed);
        isbinaryoperatoroverloadable:=allowed;
      end;


    function isunaryoperatoroverloadable(treetyp : tnodetype;ld : tdef) : boolean;
      begin
        result:=false;
        case treetyp of
          subn,
          unaryminusn :
            begin
              if (ld.typ in [orddef,enumdef,floatdef]) then
                exit;

{$ifdef SUPPORT_MMX}
              if (cs_mmx in current_settings.localswitches) and
                 is_mmx_able_array(ld) then
                exit;
{$endif SUPPORT_MMX}

              result:=true;
            end;

          notn :
            begin
              if (ld.typ in [orddef,enumdef,floatdef]) then
                exit;

{$ifdef SUPPORT_MMX}
              if (cs_mmx in current_settings.localswitches) and
                 is_mmx_able_array(ld) then
                exit;
{$endif SUPPORT_MMX}

              result:=true;
            end;
        end;
      end;


    function isoperatoracceptable(pf : tprocdef; optoken : ttoken) : boolean;
      var
        ld,rd : tdef;
        i : longint;
        eq : tequaltype;
        conv : tconverttype;
        pd : tprocdef;
        oldcount,
        count: longint;
        parasym : tparavarsym;
      begin
        result:=false;
        count := pf.parast.SymList.count;

        oldcount:=count;
        while count > 0 do
          begin
            parasym:=tparavarsym(pf.parast.SymList[count-1]);
            if is_boolean(parasym.vardef) then
              begin
                if parasym.name='RANGECHECK' then
                  begin
                    Include(parasym.varoptions, vo_is_hidden_para);
                    Include(parasym.varoptions, vo_is_range_check);
                    Dec(count);
                  end
                else if parasym.name='OVERFLOWCHECK' then
                  begin
                    Include(parasym.varoptions, vo_is_hidden_para);
                    Include(parasym.varoptions, vo_is_overflow_check);
                    Dec(count);
                  end
                else
                  break;
              end
            else
              break;
          end;
        if count<>oldcount then
          pf.calcparas;

        case count of
          1 : begin
                ld:=tparavarsym(pf.parast.SymList[0]).vardef;
                { assignment is a special case }
                if optoken=_ASSIGNMENT then
                  begin
                    eq:=compare_defs_ext(ld,pf.returndef,nothingn,conv,pd,[cdo_explicit]);
                    result:=(eq=te_incompatible);
                  end
                else
                  begin
                    for i:=1 to tok2nodes do
                      if tok2node[i].tok=optoken then
                        begin
                          result:=
                            tok2node[i].op_overloading_supported and
                            isunaryoperatoroverloadable(tok2node[i].nod,ld);
                          break;
                        end;
                  end;
              end;
          2 : begin
                for i:=1 to tok2nodes do
                  if tok2node[i].tok=optoken then
                    begin
                      ld:=tparavarsym(pf.parast.SymList[0]).vardef;
                      rd:=tparavarsym(pf.parast.SymList[1]).vardef;
                      result:=
                        tok2node[i].op_overloading_supported and
                        isbinaryoperatoroverloadable(tok2node[i].nod,ld,nothingn,rd,nothingn);
                      break;
                    end;
              end;
        end;
      end;


    function isunaryoverloaded(var t : tnode) : boolean;
      var
        ld      : tdef;
        optoken : ttoken;
        operpd  : tprocdef;
        ppn     : tcallparanode;
        candidates : tcallcandidates;
        cand_cnt : integer;
      begin
        result:=false;
        operpd:=nil;

        { load easier access variables }
        ld:=tunarynode(t).left.resultdef;
        if not isunaryoperatoroverloadable(t.nodetype,ld) then
          exit;

        { operator overload is possible }
        result:=true;

        case t.nodetype of
           notn:
             optoken:=_OP_NOT;
           unaryminusn:
             optoken:=_MINUS;
           else
             begin
               CGMessage(parser_e_operator_not_overloaded);
               t:=cnothingnode.create;
               exit;
             end;
        end;

        { generate parameter nodes }
        ppn:=ccallparanode.create(tunarynode(t).left.getcopy,nil);
        ppn.get_paratype;
        candidates:=tcallcandidates.create_operator(optoken,ppn);

        { stop when there are no operators found }
        if candidates.count=0 then
          begin
            CGMessage(parser_e_operator_not_overloaded);
            candidates.free;
            ppn.free;
            t:=cnothingnode.create;
            exit;
          end;

        { Retrieve information about the candidates }
        candidates.get_information;
{$ifdef EXTDEBUG}
        { Display info when multiple candidates are found }
        candidates.dump_info(V_Debug);
{$endif EXTDEBUG}
        cand_cnt:=candidates.choose_best(operpd,false);

        { exit when no overloads are found }
        if cand_cnt=0 then
          begin
            CGMessage(parser_e_operator_not_overloaded);
            candidates.free;
            ppn.free;
            t:=cnothingnode.create;
            exit;
          end;

        { Multiple candidates left? }
        if cand_cnt>1 then
          begin
            CGMessage(type_e_cant_choose_overload_function);
{$ifdef EXTDEBUG}
            candidates.dump_info(V_Hint);
{$else EXTDEBUG}
            candidates.list(false);
{$endif EXTDEBUG}
            { we'll just use the first candidate to make the
              call }
          end;
        candidates.free;

        addsymref(operpd.procsym);

        { the nil as symtable signs firstcalln that this is
          an overloaded operator }
        t:=ccallnode.create(ppn,Tprocsym(operpd.procsym),nil,nil,[]);

        { we already know the procdef to use, so it can
          skip the overload choosing in callnode.pass_typecheck }
        tcallnode(t).procdefinition:=operpd;
      end;


    function isbinaryoverloaded(var t : tnode) : boolean;
      var
        rd,ld   : tdef;
        optoken : ttoken;
        operpd  : tprocdef;
        ht      : tnode;
        ppn     : tcallparanode;
        candidates : tcallcandidates;
        cand_cnt : integer;
      begin
        isbinaryoverloaded:=false;
        operpd:=nil;
        { load easier access variables }
        ld:=tbinarynode(t).left.resultdef;
        rd:=tbinarynode(t).right.resultdef;
        if not isbinaryoperatoroverloadable(t.nodetype,ld,tbinarynode(t).left.nodetype,rd,tbinarynode(t).right.nodetype) then
          exit;

        { operator overload is possible }
        result:=true;

        case t.nodetype of
           equaln,
           unequaln :
             optoken:=_EQUAL;
           addn:
             optoken:=_PLUS;
           subn:
             optoken:=_MINUS;
           muln:
             optoken:=_STAR;
           starstarn:
             optoken:=_STARSTAR;
           slashn:
             optoken:=_SLASH;
           ltn:
             optoken:=_LT;
           gtn:
             optoken:=_GT;
           lten:
             optoken:=_LTE;
           gten:
             optoken:=_GTE;
           symdifn :
             optoken:=_SYMDIF;
           modn :
             optoken:=_OP_MOD;
           orn :
             optoken:=_OP_OR;
           xorn :
             optoken:=_OP_XOR;
           andn :
             optoken:=_OP_AND;
           divn :
             optoken:=_OP_DIV;
           shln :
             optoken:=_OP_SHL;
           shrn :
             optoken:=_OP_SHR;
           else
             begin
               CGMessage(parser_e_operator_not_overloaded);
               t:=cnothingnode.create;
               exit;
             end;
        end;

        { generate parameter nodes }
        ppn:=ccallparanode.create(tbinarynode(t).right.getcopy,ccallparanode.create(tbinarynode(t).left.getcopy,nil));
        ppn.get_paratype;
        candidates:=tcallcandidates.create_operator(optoken,ppn);

        { for commutative operators we can swap arguments and try again }
        if (candidates.count=0) and
           not(optoken in [_OP_SHL,_OP_SHR,_OP_DIV,_OP_MOD,_STARSTAR,_SLASH,_MINUS]) then
          begin
            candidates.free;
            reverseparameters(ppn);
            { reverse compare operators }
            case optoken of
              _LT:
                optoken:=_GTE;
              _GT:
                optoken:=_LTE;
              _LTE:
                optoken:=_GT;
              _GTE:
                optoken:=_LT;
            end;
            candidates:=tcallcandidates.create_operator(optoken,ppn);
          end;

        { stop when there are no operators found }
        if candidates.count=0 then
          begin
            CGMessage(parser_e_operator_not_overloaded);
            candidates.free;
            ppn.free;
            t:=cnothingnode.create;
            exit;
          end;

        { Retrieve information about the candidates }
        candidates.get_information;
{$ifdef EXTDEBUG}
        { Display info when multiple candidates are found }
        candidates.dump_info(V_Debug);
{$endif EXTDEBUG}
        cand_cnt:=candidates.choose_best(operpd,false);

        { exit when no overloads are found }
        if cand_cnt=0 then
          begin
            CGMessage(parser_e_operator_not_overloaded);
            candidates.free;
            ppn.free;
            t:=cnothingnode.create;
            exit;
          end;

        { Multiple candidates left? }
        if cand_cnt>1 then
          begin
            CGMessage(type_e_cant_choose_overload_function);
{$ifdef EXTDEBUG}
            candidates.dump_info(V_Hint);
{$else EXTDEBUG}
            candidates.list(false);
{$endif EXTDEBUG}
            { we'll just use the first candidate to make the
              call }
          end;
        candidates.free;

        addsymref(operpd.procsym);

        { the nil as symtable signs firstcalln that this is
          an overloaded operator }
        ht:=ccallnode.create(ppn,Tprocsym(operpd.procsym),nil,nil,[]);

        { we already know the procdef to use, so it can
          skip the overload choosing in callnode.pass_typecheck }
        tcallnode(ht).procdefinition:=operpd;

        if t.nodetype=unequaln then
          ht:=cnotnode.create(ht);
        t:=ht;
      end;


{****************************************************************************
                          Register Calculation
****************************************************************************}

    { marks an lvalue as "unregable" }
    procedure make_not_regable_intern(p : tnode; how: tregableinfoflags; records_only: boolean);
      var
        update_regable: boolean;
      begin
        update_regable:=true;
        repeat
          case p.nodetype of
            subscriptn:
              begin
                records_only:=true;
                p:=tsubscriptnode(p).left;
              end;
            vecn:
              begin
                { arrays are currently never regable and pointers indexed like }
                { arrays do not have be made unregable, but we do need to      }
                { propagate the ra_addr_taken info                             }
                update_regable:=false;
                p:=tvecnode(p).left;
              end;
            typeconvn :
               begin
                 if (ttypeconvnode(p).resultdef.typ = recorddef) then
                   records_only:=false;
                 p:=ttypeconvnode(p).left;
               end;
            loadn :
              begin
                if (tloadnode(p).symtableentry.typ in [staticvarsym,localvarsym,paravarsym]) then
                  begin
                    if (ra_addr_taken in how) then
                      tabstractvarsym(tloadnode(p).symtableentry).addr_taken:=true;
                    if update_regable and
                       (tabstractvarsym(tloadnode(p).symtableentry).varregable <> vr_none) and
                       ((not records_only) or
                        (tabstractvarsym(tloadnode(p).symtableentry).vardef.typ = recorddef)) then
                      if (tloadnode(p).symtableentry.typ = paravarsym) and
                         (ra_addr_regable in how) then
                        tabstractvarsym(tloadnode(p).symtableentry).varregable:=vr_addr
                      else
                        tabstractvarsym(tloadnode(p).symtableentry).varregable:=vr_none;
                  end;
                break;
              end;
            temprefn :
              begin
                if (ra_addr_taken in how) then
                  include(ttemprefnode(p).tempinfo^.flags,ti_addr_taken);
                if update_regable and
                   (ti_may_be_in_reg in ttemprefnode(p).tempinfo^.flags) and
                   ((not records_only) or
                    (ttemprefnode(p).tempinfo^.typedef.typ = recorddef)) then
                  exclude(ttemprefnode(p).tempinfo^.flags,ti_may_be_in_reg);
                break;
              end;
            else
              break;
          end;
        until false;
      end;

    procedure make_not_regable(p : tnode; how: tregableinfoflags);
      begin
        make_not_regable_intern(p,how,false);
      end;


{****************************************************************************
                          Subroutine Handling
****************************************************************************}

    function is_procvar_load(p:tnode):boolean;
      begin
        result:=false;
        { remove voidpointer typecast for tp procvars }
        if ((m_tp_procvar in current_settings.modeswitches) or
            (m_mac_procvar in current_settings.modeswitches)) and
           (p.nodetype=typeconvn) and
           is_voidpointer(p.resultdef) then
          p:=tunarynode(p).left;
        result:=(p.nodetype=typeconvn) and
                (ttypeconvnode(p).convtype=tc_proc_2_procvar);
      end;


    { local routines can't be assigned to procvars }
    procedure test_local_to_procvar(from_def:tprocvardef;to_def:tdef);
      begin
         if (from_def.parast.symtablelevel>normal_function_level) and
            (to_def.typ=procvardef) then
           CGMessage(type_e_cannot_local_proc_to_procvar);
      end;


    procedure set_varstate(p:tnode;newstate:tvarstate;varstateflags:tvarstateflags);
      const
        vstrans: array[tvarstate,tvarstate] of tvarstate = (
          { vs_none -> ... }
          (vs_none,vs_declared,vs_initialised,vs_read,vs_read_not_warned,vs_referred_not_inited,vs_written,vs_readwritten),
          { vs_declared -> ... }
          (vs_none,vs_declared,vs_initialised,vs_read,vs_read_not_warned,vs_referred_not_inited,vs_written,vs_readwritten),
          { vs_initialised -> ... }
          (vs_none,vs_initialised,vs_initialised,vs_read,vs_read,vs_read,vs_written,vs_readwritten),
          { vs_read -> ... }
          (vs_none,vs_read,vs_read,vs_read,vs_read,vs_read,vs_readwritten,vs_readwritten),
          { vs_read_not_warned -> ... }
          (vs_none,vs_read_not_warned,vs_read,vs_read,vs_read_not_warned,vs_read_not_warned,vs_readwritten,vs_readwritten),
          { vs_referred_not_inited }
          (vs_none,vs_referred_not_inited,vs_read,vs_read,vs_read_not_warned,vs_referred_not_inited,vs_written,vs_readwritten),
          { vs_written -> ... }
          (vs_none,vs_written,vs_written,vs_readwritten,vs_readwritten,vs_written,vs_written,vs_readwritten),
          { vs_readwritten -> ... }
          (vs_none,vs_readwritten,vs_readwritten,vs_readwritten,vs_readwritten,vs_readwritten,vs_readwritten,vs_readwritten));
      var
        hsym : tabstractvarsym;
      begin
        { make sure we can still warn about uninitialised use after high(v), @v etc }
        if (newstate = vs_read) and
           not(vsf_must_be_valid in varstateflags) then
          newstate := vs_referred_not_inited;

        while assigned(p) do
         begin
           case p.nodetype of
             derefn:
               begin
                 if (tderefnode(p).left.nodetype=temprefn) and
                    assigned(ttemprefnode(tderefnode(p).left).tempinfo^.withnode) then
                   p:=ttemprefnode(tderefnode(p).left).tempinfo^.withnode
                 else
                   break;
               end;
             typeconvn :
               begin
                 case ttypeconvnode(p).convtype of
                   tc_cchar_2_pchar,
                   tc_cstring_2_pchar,
                   tc_array_2_pointer :
                     exclude(varstateflags,vsf_must_be_valid);
                   tc_pchar_2_string,
                   tc_pointer_2_array :
                     include(varstateflags,vsf_must_be_valid);
                 end;
                 p:=tunarynode(p).left;
               end;
             subscriptn :
               begin
                 if is_class_or_interface(tunarynode(p).left.resultdef) then
                   newstate := vs_read;
                 p:=tunarynode(p).left;
               end;
             vecn:
               begin
                 set_varstate(tbinarynode(p).right,vs_read,[vsf_must_be_valid]);
                 if (newstate in [vs_read,vs_readwritten]) or
                    not(tunarynode(p).left.resultdef.typ in [stringdef,arraydef]) then
                   include(varstateflags,vsf_must_be_valid)
                 else if (newstate = vs_written) then
                   exclude(varstateflags,vsf_must_be_valid);
                 p:=tunarynode(p).left;
               end;
             { do not parse calln }
             calln :
               break;
             loadn :
               begin
                 if (tloadnode(p).symtableentry.typ in [localvarsym,paravarsym,staticvarsym]) then
                  begin
                    hsym:=tabstractvarsym(tloadnode(p).symtableentry);
                    if (vsf_must_be_valid in varstateflags) and
                       (hsym.varstate in [vs_declared,vs_read_not_warned,vs_referred_not_inited]) then
                      begin
                        { Give warning/note for uninitialized locals }
                        if assigned(hsym.owner) and
                          not(cs_opt_nodedfa in current_settings.optimizerswitches) and
                           not(vo_is_external in hsym.varoptions) and
                           (hsym.owner.symtabletype in [parasymtable,localsymtable,staticsymtable]) and
                           ((hsym.owner=current_procinfo.procdef.localst) or
                            (hsym.owner=current_procinfo.procdef.parast)) then
                          begin
                            if (vo_is_funcret in hsym.varoptions) then
                              begin
                                if (vsf_use_hints in varstateflags) then
                                  CGMessagePos(p.fileinfo,sym_h_function_result_uninitialized)
                                else
                                  CGMessagePos(p.fileinfo,sym_w_function_result_uninitialized)
                              end
                            else
                              begin
                                if tloadnode(p).symtable.symtabletype=localsymtable then
                                  begin
                                    if (vsf_use_hints in varstateflags) then
                                      CGMessagePos1(p.fileinfo,sym_h_uninitialized_local_variable,hsym.realname)
                                    else
                                      CGMessagePos1(p.fileinfo,sym_w_uninitialized_local_variable,hsym.realname);
                                  end
                                else
                                  begin
                                    if (vsf_use_hints in varstateflags) then
                                      CGMessagePos1(p.fileinfo,sym_h_uninitialized_variable,hsym.realname)
                                    else
                                      CGMessagePos1(p.fileinfo,sym_w_uninitialized_variable,hsym.realname);
                                  end;
                              end;
                          end
                        else if (newstate = vs_read) then
                          newstate := vs_read_not_warned;
                      end;
                    hsym.varstate := vstrans[hsym.varstate,newstate];
                  end;
                 break;
               end;
             callparan :
               internalerror(200310081);
             else
               break;
           end;{case }
         end;
      end;


    procedure set_unique(p : tnode);
      begin
        while assigned(p) do
         begin
           case p.nodetype of
             vecn:
               begin
                 include(p.flags,nf_callunique);
                 break;
               end;
             typeconvn,
             subscriptn,
             derefn:
               p:=tunarynode(p).left;
             else
               break;
           end;
         end;
      end;


    function  valid_for_assign(p:tnode;opts:TValidAssigns; report_errors: boolean):boolean;
      var
        hp2,
        hp : tnode;
        gotstring,
        gotsubscript,
        gotrecord,
        gotpointer,
        gotvec,
        gotclass,
        gotdynarray,
        gotderef : boolean;
        fromdef,
        todef    : tdef;
        errmsg,
        temp     : longint;
      begin
        if valid_const in opts then
          errmsg:=type_e_variable_id_expected
        else if valid_property in opts then
          errmsg:=type_e_argument_cant_be_assigned
        else
          errmsg:=type_e_no_addr_of_constant;
        result:=false;
        gotsubscript:=false;
        gotvec:=false;
        gotderef:=false;
        gotrecord:=false;
        gotclass:=false;
        gotpointer:=false;
        gotdynarray:=false;
        gotstring:=false;
        hp:=p;
        if not(valid_void in opts) and
           is_void(hp.resultdef) then
         begin
           if report_errors then
             CGMessagePos(hp.fileinfo,errmsg);
           exit;
         end;
        while assigned(hp) do
         begin
           { property allowed? calln has a property check itself }
           if (nf_isproperty in hp.flags) then
             begin
               { check return type }
               case hp.resultdef.typ of
                 pointerdef :
                   gotpointer:=true;
                 objectdef :
                   gotclass:=is_class_or_interface(hp.resultdef);
                 recorddef :
                   gotrecord:=true;
                 classrefdef :
                   gotclass:=true;
                 stringdef :
                   gotstring:=true;
               end;
               if (valid_property in opts) then
                 begin
                   { don't allow writing to calls that will create
                     temps like calls that return a structure and we
                     are assigning to a member }
                   if (valid_const in opts) or
                      { if we got a deref, we won't modify the property itself }
                      (gotderef) or
                      { same when we got a class and subscript (= deref) }
                      (gotclass and gotsubscript) or
                      (
                       not(gotsubscript and gotrecord) and
                       not(gotstring and gotvec)
                      ) then
                     result:=true
                   else
                     if report_errors then
                       CGMessagePos(hp.fileinfo,errmsg);
                 end
               else
                 begin
                   { 1. if it returns a pointer and we've found a deref,
                     2. if it returns a class or record and a subscription or with is found
                     3. if the address is needed of a field (subscriptn, vecn) }
                   if (gotpointer and gotderef) or
                      (gotstring and gotvec) or
                      (
                       (gotclass or gotrecord) and
                       (gotsubscript)
                      ) or
                      (
                        (gotvec and gotdynarray)
                      ) or
                      (
                       (Valid_Addr in opts) and
                       (hp.nodetype in [subscriptn,vecn])
                      ) then
                     result:=true
                   else
                     if report_errors then
                       CGMessagePos(hp.fileinfo,errmsg);
                 end;
               exit;
             end;
           case hp.nodetype of
             temprefn :
               begin
                 valid_for_assign := true;
                 exit;
               end;
             derefn :
               begin
                 gotderef:=true;
                 hp:=tderefnode(hp).left;
               end;
             typeconvn :
               begin
                 { typecast sizes must match, exceptions:
                   - implicit typecast made by absolute
                   - from formaldef
                   - from void
                   - from/to open array
                   - typecast from pointer to array }
                 fromdef:=ttypeconvnode(hp).left.resultdef;
                 todef:=hp.resultdef;
                 if not((nf_absolute in ttypeconvnode(hp).flags) or
                        (fromdef.typ=formaldef) or
                        is_void(fromdef) or
                        is_open_array(fromdef) or
                        is_open_array(todef) or
                        ((fromdef.typ=pointerdef) and (todef.typ=arraydef)) or
                        ((fromdef.typ = objectdef) and (todef.typ = objectdef) and
                         (tobjectdef(fromdef).is_related(tobjectdef(todef))))) and
                    (fromdef.size<>todef.size) then
                  begin
                    { in TP it is allowed to typecast to smaller types. But the variable can't
                      be in a register }
                    if (m_tp7 in current_settings.modeswitches) or
                       (todef.size<fromdef.size) then
                      make_not_regable(hp,[ra_addr_regable])
                    else
                      if report_errors then
                        CGMessagePos2(hp.fileinfo,type_e_typecast_wrong_size_for_assignment,tostr(fromdef.size),tostr(todef.size));
                  end;

                 { don't allow assignments to typeconvs that need special code }
                 if not(gotsubscript or gotvec or gotderef) and
                    not(ttypeconvnode(hp).assign_allowed) then
                   begin
                     if report_errors then
                       CGMessagePos(hp.fileinfo,errmsg);
                     exit;
                   end;
                 case hp.resultdef.typ of
                   pointerdef :
                     gotpointer:=true;
                   objectdef :
                     gotclass:=is_class_or_interface(hp.resultdef);
                   classrefdef :
                     gotclass:=true;
                   arraydef :
                     begin
                       { pointer -> array conversion is done then we need to see it
                         as a deref, because a ^ is then not required anymore }
                       if (ttypeconvnode(hp).left.resultdef.typ=pointerdef) then
                        gotderef:=true;
                     end;
                 end;
                 hp:=ttypeconvnode(hp).left;
               end;
             vecn :
               begin
                 if { only check for first (= outermost) vec node }
                    not gotvec and
                    not(valid_packed in opts) and
                    (tvecnode(hp).left.resultdef.typ = arraydef) and
                    (ado_IsBitPacked in tarraydef(tvecnode(hp).left.resultdef).arrayoptions) and
                    ((tarraydef(tvecnode(hp).left.resultdef).elepackedbitsize mod 8 <> 0) or
                     (is_ordinal(tarraydef(tvecnode(hp).left.resultdef).elementdef) and
                      not ispowerof2(tarraydef(tvecnode(hp).left.resultdef).elepackedbitsize div 8,temp))) then
                   begin
                     if report_errors then
                       if (valid_property in opts) then
                         CGMessagePos(hp.fileinfo,parser_e_packed_element_no_loop)
                       else
                         CGMessagePos(hp.fileinfo,parser_e_packed_element_no_var_addr);
                     exit;
                   end;
                 gotvec:=true;
                 { accesses to dyn. arrays override read only access in delphi }
                 if (m_delphi in current_settings.modeswitches) and is_dynamic_array(tunarynode(hp).left.resultdef) then
                   gotdynarray:=true;
                 hp:=tunarynode(hp).left;
               end;
             blockn :
               begin
                 hp2:=tblocknode(hp).statements;
                 if assigned(hp2) then
                   begin
                     if hp2.nodetype<>statementn then
                       internalerror(2006110801);
                     while assigned(tstatementnode(hp2).next) do
                       hp2:=tstatementnode(hp2).next;
                     hp:=tstatementnode(hp2).statement;
                   end
                 else
                   begin
                     if report_errors then
                      CGMessagePos(hp.fileinfo,type_e_variable_id_expected);
                     exit;
                   end;
               end;
             asn :
               begin
                 { asn can't be assigned directly, it returns the value in a register instead
                   of reference. }
                 if not(gotsubscript or gotderef or gotvec) then
                   begin
                     if report_errors then
                       CGMessagePos(hp.fileinfo,errmsg);
                     exit;
                   end;
                 hp:=tunarynode(hp).left;
               end;
             subscriptn :
               begin
                 { only check first (= outermost) subscriptn }
                 if not gotsubscript and
                    not(valid_packed in opts) and
                    is_packed_record_or_object(tsubscriptnode(hp).left.resultdef) and
                    ((tsubscriptnode(hp).vs.fieldoffset mod 8 <> 0) or
                     (is_ordinal(tsubscriptnode(hp).resultdef) and
                      not ispowerof2(tsubscriptnode(hp).resultdef.packedbitsize div 8,temp)))  then
                   begin
                     if report_errors then
                       if (valid_property in opts) then
                         CGMessagePos(hp.fileinfo,parser_e_packed_element_no_loop)
                       else
                         CGMessagePos(hp.fileinfo,parser_e_packed_element_no_var_addr);
                     exit;
                   end;
                 gotsubscript:=true;
                 { loop counter? }
                 if not(Valid_Const in opts) and
                    (vo_is_loop_counter in tsubscriptnode(hp).vs.varoptions) then
                   begin
                     if report_errors then
                       CGMessage1(parser_e_illegal_assignment_to_count_var,tsubscriptnode(hp).vs.realname)
                     else
                       exit;
                   end;
                 { a class/interface access is an implicit }
                 { dereferencing                           }
                 hp:=tsubscriptnode(hp).left;
                 if is_class_or_interface(hp.resultdef) then
                   gotderef:=true;
               end;
             muln,
             divn,
             andn,
             xorn,
             orn,
             notn,
             subn,
             addn :
               begin
                 { Allow operators on a pointer, or an integer
                   and a pointer typecast and deref has been found }
                 if ((hp.resultdef.typ=pointerdef) or
                     (is_integer(hp.resultdef) and gotpointer)) and
                    gotderef then
                  result:=true
                 else
                 { Temp strings are stored in memory, for compatibility with
                   delphi only }
                   if (m_delphi in current_settings.modeswitches) and
                      ((valid_addr in opts) or
                       (valid_const in opts)) and
                      (hp.resultdef.typ=stringdef) then
                     result:=true
                 else
                  if report_errors then
                   CGMessagePos(hp.fileinfo,type_e_variable_id_expected);
                 exit;
               end;
             niln,
             pointerconstn :
               begin
                 { to support e.g. @tmypointer(0)^.data; see tests/tbs/tb0481 }
                 if gotderef then
                  result:=true
                 else
                  if report_errors then
                   CGMessagePos(hp.fileinfo,type_e_no_assign_to_addr);
                 exit;
               end;
             ordconstn,
             realconstn :
               begin
                 { these constants will be passed by value }
                 if report_errors then
                   CGMessagePos(hp.fileinfo,type_e_variable_id_expected);
                 exit;
               end;
             setconstn,
             stringconstn,
             guidconstn :
               begin
                 { these constants will be passed by reference }
                 if valid_const in opts then
                   result:=true
                 else
                   if report_errors then
                     CGMessagePos(hp.fileinfo,type_e_variable_id_expected);
                 exit;
               end;
             addrn :
               begin
                 if gotderef then
                  result:=true
                 else
                  if report_errors then
                   CGMessagePos(hp.fileinfo,type_e_no_assign_to_addr);
                 exit;
               end;
             calln :
               begin
                 { check return type }
                 case hp.resultdef.typ of
                   arraydef :
                     begin
                       { dynamic arrays are allowed when there is also a
                         vec node }
                       if is_dynamic_array(hp.resultdef) and
                          gotvec then
                        begin
                          gotderef:=true;
                          gotpointer:=true;
                        end;
                     end;
                   pointerdef :
                     gotpointer:=true;
                   objectdef :
                     gotclass:=is_class_or_interface(hp.resultdef);
                   recorddef, { handle record like class it needs a subscription }
                   classrefdef :
                     gotclass:=true;
                   stringdef :
                     gotstring:=true;
                 end;
                 { 1. if it returns a pointer and we've found a deref,
                   2. if it returns a class or record and a subscription or with is found
                   3. string is returned }
                 if (gotstring and gotvec) or
                    (gotpointer and gotderef) or
                    (gotclass and gotsubscript) then
                  result:=true
                 else
                 { Temp strings are stored in memory, for compatibility with
                   delphi only }
                   if (m_delphi in current_settings.modeswitches) and
                      (valid_addr in opts) and
                      (hp.resultdef.typ=stringdef) then
                     result:=true
                 else
                   if ([valid_const,valid_addr] * opts = [valid_const]) then
                     result:=true
                 else
                  if report_errors then
                   CGMessagePos(hp.fileinfo,errmsg);
                 exit;
               end;
             inlinen :
               begin
                 if ((valid_const in opts) and
                    (tinlinenode(hp).inlinenumber in [in_typeof_x]))
{$ifdef SUPPORT_UNALIGNED}
                    or (tinlinenode(hp).inlinenumber in [in_unaligned_x])
{$endif SUPPORT_UNALIGNED}
                    then
                   result:=true
                 else
                   if report_errors then
                    CGMessagePos(hp.fileinfo,type_e_variable_id_expected);
                 exit;
               end;
             dataconstn:
               begin
                 { only created internally, so no additional checks necessary }
                 result:=true;
                 exit;
               end;
             loadn :
               begin
                 case tloadnode(hp).symtableentry.typ of
                   absolutevarsym,
                   staticvarsym,
                   localvarsym,
                   paravarsym :
                     begin
                       { loop counter? }
                       if not(Valid_Const in opts) and
                          not gotderef and
                          (vo_is_loop_counter in tabstractvarsym(tloadnode(hp).symtableentry).varoptions) then
                         if report_errors then
                          CGMessage1(parser_e_illegal_assignment_to_count_var,tloadnode(hp).symtableentry.realname)
                         else
                          exit;
                       { read-only variable? }
                       if (tabstractvarsym(tloadnode(hp).symtableentry).varspez=vs_const) then
                        begin
                          { allow p^:= constructions with p is const parameter }
                          if gotderef or gotdynarray or (Valid_Const in opts) or
                            (nf_isinternal_ignoreconst in tloadnode(hp).flags) then
                            result:=true
                          else
                            if report_errors then
                              CGMessagePos(tloadnode(hp).fileinfo,type_e_no_assign_to_const);
                          exit;
                        end;
                       result:=true;
                       exit;
                     end;
                   procsym :
                     begin
                       if (Valid_Const in opts) then
                         result:=true
                       else
                         if report_errors then
                          CGMessagePos(hp.fileinfo,type_e_variable_id_expected);
                       exit;
                     end;
                   labelsym :
                     begin
                       if (Valid_Addr in opts) then
                         result:=true
                       else
                         if report_errors then
                          CGMessagePos(hp.fileinfo,type_e_variable_id_expected);
                       exit;
                     end;
                   constsym:
                     begin
                       if (tconstsym(tloadnode(hp).symtableentry).consttyp=constresourcestring) and
                         (valid_addr in opts) then
                         result:=true
                       else
                         if report_errors then
                          CGMessagePos(hp.fileinfo,type_e_variable_id_expected);
                       exit;
                     end;
                   else
                     begin
                       if report_errors then
                        CGMessagePos(hp.fileinfo,type_e_variable_id_expected);
                       exit;
                     end;
                 end;
               end;
             else
               begin
                 if report_errors then
                  CGMessagePos(hp.fileinfo,type_e_variable_id_expected);
                 exit;
               end;
            end;
         end;
      end;


    function  valid_for_var(p:tnode; report_errors: boolean):boolean;
      begin
        valid_for_var:=valid_for_assign(p,[],report_errors);
      end;


    function  valid_for_formal_var(p : tnode; report_errors: boolean) : boolean;
      begin
        valid_for_formal_var:=valid_for_assign(p,[valid_void],report_errors);
      end;


    function  valid_for_formal_const(p : tnode; report_errors: boolean) : boolean;
      begin
        valid_for_formal_const:=(p.resultdef.typ=formaldef) or
          valid_for_assign(p,[valid_void,valid_const,valid_property],report_errors);
      end;


    function  valid_for_assignment(p:tnode; report_errors: boolean):boolean;
      begin
        valid_for_assignment:=valid_for_assign(p,[valid_property,valid_packed],report_errors);
      end;


    function  valid_for_loopvar(p:tnode; report_errors: boolean):boolean;
      begin
        valid_for_loopvar:=valid_for_assign(p,[valid_property],report_errors);
      end;


    function  valid_for_addr(p : tnode; report_errors: boolean) : boolean;
      begin
        result:=valid_for_assign(p,[valid_const,valid_addr,valid_void],report_errors);
      end;


    procedure var_para_allowed(var eq:tequaltype;def_from,def_to:Tdef; fromnode: tnode);
      begin
        { Note: eq must be already valid, it will only be updated! }
        case def_to.typ of
          formaldef :
            begin
              { all types can be passed to a formaldef,
                but it is not the prefered way }
              if not is_constnode(fromnode) then
                eq:=te_convert_l2
              else
                eq:=te_incompatible;
            end;
          orddef :
            begin
              { allows conversion from word to integer and
                byte to shortint, but only for TP7 compatibility }
              if (m_tp7 in current_settings.modeswitches) and
                 (def_from.typ=orddef) and
                 (def_from.size=def_to.size) then
                eq:=te_convert_l1;
            end;
          arraydef :
            begin
              if is_open_array(def_to) then
                begin
                  if is_dynamic_array(def_from) and
                     equal_defs(tarraydef(def_from).elementdef,tarraydef(def_to).elementdef) then
                    eq:=te_convert_l2
                  else
                    if equal_defs(def_from,tarraydef(def_to).elementdef) then
                      eq:=te_convert_l2;
                end;
            end;
          pointerdef :
            begin
              { an implicit pointer conversion is allowed }
              if (def_from.typ=pointerdef) then
                eq:=te_convert_l1;
            end;
          stringdef :
            begin
              { all shortstrings are allowed, size is not important }
              if is_shortstring(def_from) and
                 is_shortstring(def_to) then
                eq:=te_equal;
            end;
          objectdef :
            begin
              { child objects can be also passed }
              { in non-delphi mode, otherwise    }
              { they must match exactly, except  }
              { if they are objects              }
              if (def_from.typ=objectdef) and
                 (
                  not(m_delphi in current_settings.modeswitches) or
                  (
                   (tobjectdef(def_from).objecttype=odt_object) and
                   (tobjectdef(def_to).objecttype=odt_object)
                  )
                 ) and
                 (tobjectdef(def_from).is_related(tobjectdef(def_to))) then
                eq:=te_convert_l1;
            end;
          filedef :
            begin
              { an implicit file conversion is also allowed }
              { from a typed file to an untyped one           }
              if (def_from.typ=filedef) and
                 (tfiledef(def_from).filetyp = ft_typed) and
                 (tfiledef(def_to).filetyp = ft_untyped) then
                eq:=te_convert_l1;
            end;
        end;
      end;


    procedure para_allowed(var eq:tequaltype;p:tcallparanode;def_to:tdef);
      begin
        { Note: eq must be already valid, it will only be updated! }
        case def_to.typ of
          formaldef :
            begin
              { all types can be passed to a formaldef }
              eq:=te_equal;
            end;
          stringdef :
            begin
              { to support ansi/long/wide strings in a proper way }
              { string and string[10] are assumed as equal }
              { when searching the correct overloaded procedure   }
              if (p.resultdef.typ=stringdef) and
                 (tstringdef(def_to).stringtype=tstringdef(p.resultdef).stringtype) then
                eq:=te_equal
              else
              { Passing a constant char to ansistring or shortstring or
                a widechar to widestring then handle it as equal. }
               if (p.left.nodetype=ordconstn) and
                  (
                   is_char(p.resultdef) and
                   (is_shortstring(def_to) or is_ansistring(def_to))
                  ) or
                  (
                   is_widechar(p.resultdef) and
                   is_widestring(def_to)
                  ) then
                eq:=te_equal
            end;
          setdef :
            begin
              { set can also be a not yet converted array constructor }
              if (p.resultdef.typ=arraydef) and
                 is_array_constructor(p.resultdef) and
                 not is_variant_array(p.resultdef) then
                eq:=te_equal;
            end;
          procvardef :
            begin
              { in tp7 mode proc -> procvar is allowed }
              if ((m_tp_procvar in current_settings.modeswitches) or
                  (m_mac_procvar in current_settings.modeswitches)) and
                 (p.left.nodetype=calln) and
                 (proc_to_procvar_equal(tprocdef(tcallnode(p.left).procdefinition),tprocvardef(def_to))>=te_equal) then
                eq:=te_equal
              else
                if (m_mac_procvar in current_settings.modeswitches) and
                   is_procvar_load(p.left) then
                  eq:=te_convert_l2;
            end;
        end;
      end;


    function allowenumop(nt:tnodetype):boolean;
      begin
        result:=(nt in [equaln,unequaln,ltn,lten,gtn,gten]) or
                ((cs_allow_enum_calc in current_settings.localswitches) and
                 (nt in [addn,subn]));
      end;


{****************************************************************************
                           TCallCandidates
****************************************************************************}

    constructor tcallcandidates.create(sym:tprocsym;st:TSymtable;ppn:tnode;isprop,ignorevis : boolean);
      var
        j          : integer;
        pd         : tprocdef;
        hp         : pcandidate;
        found,
        has_overload_directive : boolean;
        topclassh  : tobjectdef;
        srsymtable : TSymtable;
        srprocsym  : tprocsym;
        pt         : tcallparanode;
        checkstack : psymtablestackitem;
        hashedid   : THashedIDString;
      begin
        if not assigned(sym) then
          internalerror(200411015);

        FProcSym:=sym;
        FProcs:=nil;
        FProccnt:=0;
        FProcvisiblecnt:=0;
        FParanode:=ppn;
        FAllowVariant:=true;

        { determine length of parameter list }
        pt:=tcallparanode(ppn);
        FParalength:=0;
        while assigned(pt) do
         begin
           inc(FParalength);
           pt:=tcallparanode(pt.right);
         end;

        { when the definition has overload directive set, we search for
          overloaded definitions in the class, this only needs to be done once
          for class entries as the tree keeps always the same }
        if (not sym.overloadchecked) and
           (sym.owner.symtabletype=ObjectSymtable) and
           (po_overload in tprocdef(sym.ProcdefList[0]).procoptions) then
         search_class_overloads(sym);

        { when the class passed is defined in this unit we
          need to use the scope of that class. This is a trick
          that can be used to access protected members in other
          units. At least kylix supports it this way (PFV) }
        if assigned(st) and
           (
            (st.symtabletype=ObjectSymtable) or
            ((st.symtabletype=withsymtable) and
             (st.defowner.typ=objectdef))
           ) and
           (st.defowner.owner.symtabletype in [globalsymtable,staticsymtable]) and
           st.defowner.owner.iscurrentunit then
          topclassh:=tobjectdef(st.defowner)
        else
          begin
            if assigned(current_procinfo) then
              topclassh:=current_procinfo.procdef._class
            else
              topclassh:=nil;
          end;

        { link all procedures which have the same # of parameters }
        for j:=0 to sym.ProcdefList.Count-1 do
          begin
            pd:=tprocdef(sym.ProcdefList[j]);
            { Is the procdef visible? This needs to be checked on
              procdef level since a symbol can contain both private and
              public declarations. But the check should not be done
              when the callnode is generated by a property

              inherited overrides invisible anonymous inherited (FK) }

            if isprop or ignorevis or
               (pd.owner.symtabletype<>ObjectSymtable) or
               pd.is_visible_for_object(topclassh,nil) then
             begin
               { we have at least one procedure that is visible }
               inc(FProcvisiblecnt);
               { only when the # of parameter are supported by the
                 procedure }
               if (FParalength>=pd.minparacount) and
                  ((po_varargs in pd.procoptions) or { varargs }
                   (FParalength<=pd.maxparacount)) then
                 proc_add(sym,pd);
             end;
          end;

        { remember if the procedure is declared with the overload directive,
          it's information is still needed also after all procs are removed }
        has_overload_directive:=(po_overload in tprocdef(sym.ProcdefList[0]).procoptions);

        { when the definition has overload directive set, we search for
          overloaded definitions in the symtablestack. The found
          entries are only added to the procs list and not the procsym, because
          the list can change in every situation }
        if has_overload_directive and
           (sym.owner.symtabletype<>ObjectSymtable) then
          begin
            srsymtable:=sym.owner;
            checkstack:=symtablestack.stack;
            while assigned(checkstack) and
                  (checkstack^.symtable<>srsymtable) do
              checkstack:=checkstack^.next;
            { we've already processed the current symtable, start with
              the next symtable in the stack }
            if assigned(checkstack) then
              checkstack:=checkstack^.next;
            hashedid.id:=sym.name;
            while assigned(checkstack) do
             begin
               srsymtable:=checkstack^.symtable;
               if srsymtable.symtabletype in [localsymtable,staticsymtable,globalsymtable] then
                begin
                  srprocsym:=tprocsym(srsymtable.FindWithHash(hashedid));
                  if assigned(srprocsym) and
                     (srprocsym.typ=procsym) then
                   begin
                     { if this visible procedure doesn't have overload we can stop
                       searching }
                     if not(po_overload in tprocdef(srprocsym.ProcdefList[0]).procoptions) and
                        tprocdef(srprocsym.ProcdefList[0]).is_visible_for_object(topclassh,nil) then
                      break;
                     { process all overloaded definitions }
                     for j:=0 to srprocsym.ProcdefList.Count-1 do
                      begin
                        pd:=tprocdef(srprocsym.ProcdefList[j]);
                        { only visible procedures need to be added }
                        if pd.is_visible_for_object(topclassh,nil) then
                          begin
                            { only when the # of parameter are supported by the
                              procedure }
                            if (FParalength>=pd.minparacount) and
                               ((po_varargs in pd.procoptions) or { varargs }
                               (FParalength<=pd.maxparacount)) then
                             begin
                               found:=false;
                               hp:=FProcs;
                               while assigned(hp) do
                                begin
                                  { Only compare visible parameters for the user }
                                  if compare_paras(hp^.data.paras,pd.paras,cp_value_equal_const,[cpo_ignorehidden])>=te_equal then
                                   begin
                                     found:=true;
                                     break;
                                   end;
                                  hp:=hp^.next;
                                end;
                               if not found then
                                 proc_add(srprocsym,pd);
                             end;
                         end;
                      end;
                   end;
                end;
               checkstack:=checkstack^.next;
             end;
          end;
      end;


    constructor tcallcandidates.create_operator(op:ttoken;ppn:tnode);
      var
        j          : integer;
        pd         : tprocdef;
        hp         : pcandidate;
        found      : boolean;
        srsymtable : TSymtable;
        srprocsym  : tprocsym;
        pt         : tcallparanode;
        checkstack : psymtablestackitem;
        hashedid   : THashedIDString;
      begin
        FProcSym:=nil;
        FProcs:=nil;
        FProccnt:=0;
        FProcvisiblecnt:=0;
        FParanode:=ppn;
        FAllowVariant:=false;

        { determine length of parameter list }
        pt:=tcallparanode(ppn);
        FParalength:=0;
        while assigned(pt) do
         begin
           if pt.resultdef.typ=variantdef then
             FAllowVariant:=true;
           inc(FParalength);
           pt:=tcallparanode(pt.right);
         end;

        { we search all overloaded operator definitions in the symtablestack. The found
          entries are only added to the procs list and not the procsym, because
          the list can change in every situation }
        hashedid.id:=overloaded_names[op];
        checkstack:=symtablestack.stack;
        while assigned(checkstack) do
          begin
            srsymtable:=checkstack^.symtable;
            if srsymtable.symtabletype in [localsymtable,staticsymtable,globalsymtable] then
              begin
                srprocsym:=tprocsym(srsymtable.FindWithHash(hashedid));
                if assigned(srprocsym) and
                   (srprocsym.typ=procsym) then
                  begin
                    { Store first procsym found }
                    if not assigned(FProcsym) then
                      FProcsym:=srprocsym;

                    { process all overloaded definitions }
                    for j:=0 to srprocsym.ProcdefList.Count-1 do
                      begin
                        pd:=tprocdef(srprocsym.ProcdefList[j]);
                        { only when the # of parameter are supported by the
                          procedure }
                        if (FParalength>=pd.minparacount) and
                           (FParalength<=pd.maxparacount) then
                          begin
                            found:=false;
                            hp:=FProcs;
                            while assigned(hp) do
                              begin
                                { Only compare visible parameters for the user }
                                if compare_paras(hp^.data.paras,pd.paras,cp_value_equal_const,[cpo_ignorehidden])>=te_equal then
                                  begin
                                    found:=true;
                                    break;
                                  end;
                                hp:=hp^.next;
                              end;
                            if not found then
                              proc_add(srprocsym,pd);
                          end;
                      end;
                  end;
              end;
            checkstack:=checkstack^.next;
          end;
      end;


    destructor tcallcandidates.destroy;
      var
        hpnext,
        hp : pcandidate;
      begin
        hp:=FProcs;
        while assigned(hp) do
         begin
           hpnext:=hp^.next;
           dispose(hp);
           hp:=hpnext;
         end;
      end;


    function tcallcandidates.proc_add(ps:tprocsym;pd:tprocdef):pcandidate;
      var
        defaultparacnt : integer;
      begin
        { generate new candidate entry }
        new(result);
        fillchar(result^,sizeof(tcandidate),0);
        result^.data:=pd;
        result^.next:=FProcs;
        FProcs:=result;
        inc(FProccnt);
        { Find last parameter, skip all default parameters
          that are not passed. Ignore this skipping for varargs }
        result^.firstparaidx:=pd.paras.count-1;
        if not(po_varargs in pd.procoptions) then
         begin
           { ignore hidden parameters }
           while (result^.firstparaidx>=0) and (vo_is_hidden_para in tparavarsym(pd.paras[result^.firstparaidx]).varoptions) do
             dec(result^.firstparaidx);
           defaultparacnt:=pd.maxparacount-FParalength;
           if defaultparacnt>0 then
             begin
               if defaultparacnt>result^.firstparaidx+1 then
                 internalerror(200401141);
               dec(result^.firstparaidx,defaultparacnt);
             end;
         end;
        { Give a small penalty for overloaded methods not in
          defined the current class/unit }
        if ps.owner<>pd.owner then
          result^.ordinal_distance:=result^.ordinal_distance+1.0;
      end;


    procedure tcallcandidates.list(all:boolean);
      var
        hp : pcandidate;
      begin
        hp:=FProcs;
        while assigned(hp) do
         begin
           if all or
              (not hp^.invalid) then
             MessagePos1(hp^.data.fileinfo,sym_h_param_list,hp^.data.fullprocname(false));
           hp:=hp^.next;
         end;
      end;


{$ifdef EXTDEBUG}
    procedure tcallcandidates.dump_info(lvl:longint);

        function ParaTreeStr(p:tcallparanode):string;
        begin
          result:='';
          while assigned(p) do
           begin
             if result<>'' then
              result:=','+result;
             result:=p.resultdef.typename+result;
             p:=tcallparanode(p.right);
           end;
        end;

      var
        hp : pcandidate;
        i  : integer;
        currpara : tparavarsym;
      begin
        if not CheckVerbosity(lvl) then
         exit;
        Comment(lvl+V_LineInfo,'Overloaded callnode: '+FProcSym.name+'('+ParaTreeStr(tcallparanode(FParaNode))+')');
        hp:=FProcs;
        while assigned(hp) do
         begin
           Comment(lvl,'  '+hp^.data.fullprocname(false));
           if (hp^.invalid) then
            Comment(lvl,'   invalid')
           else
            begin
              Comment(lvl,'   ex: '+tostr(hp^.exact_count)+
                          ' eq: '+tostr(hp^.equal_count)+
                          ' l1: '+tostr(hp^.cl1_count)+
                          ' l2: '+tostr(hp^.cl2_count)+
                          ' l3: '+tostr(hp^.cl3_count)+
                          ' l4: '+tostr(hp^.cl4_count)+
                          ' l5: '+tostr(hp^.cl5_count)+
                          ' oper: '+tostr(hp^.coper_count)+
                          ' ord: '+realtostr(hp^.ordinal_distance));
              { Print parameters in left-right order }
              for i:=0 to hp^.data.paras.count-1 do
               begin
                 currpara:=tparavarsym(hp^.data.paras[i]);
                 if not(vo_is_hidden_para in currpara.varoptions) then
                   Comment(lvl,'    - '+currpara.vardef.typename+' : '+EqualTypeName[currpara.eqval]);
               end;
            end;
           hp:=hp^.next;
         end;
      end;
{$endif EXTDEBUG}


    procedure tcallcandidates.get_information;
      var
        hp       : pcandidate;
        currpara : tparavarsym;
        paraidx  : integer;
        currparanr : byte;
        rfh,rth  : double;
        objdef   : tobjectdef;
        def_from,
        def_to   : tdef;
        currpt,
        pt       : tcallparanode;
        eq       : tequaltype;
        convtype : tconverttype;
        pdtemp,
        pdoper   : tprocdef;
        releasecurrpt : boolean;
        cdoptions : tcompare_defs_options;
        n : tnode;

    {$ifopt r+}{$define ena_r}{$r-}{$endif}
    {$ifopt q+}{$define ena_q}{$q-}{$endif}
      const
        inf=1.0/0.0;
    {$ifdef ena_r}{$r+}{$endif}
    {$ifdef ena_q}{$q+}{$endif}

      begin
        cdoptions:=[cdo_check_operator];
        if FAllowVariant then
          include(cdoptions,cdo_allow_variant);
        { process all procs }
        hp:=FProcs;
        while assigned(hp) do
         begin
           { We compare parameters in reverse order (right to left),
             the firstpara is already pointing to the last parameter
             were we need to start comparing }
           currparanr:=FParalength;
           paraidx:=hp^.firstparaidx;
           while (paraidx>=0) and (vo_is_hidden_para in tparavarsym(hp^.data.paras[paraidx]).varoptions) do
             dec(paraidx);
           pt:=tcallparanode(FParaNode);
           while assigned(pt) and (paraidx>=0) do
            begin
              currpara:=tparavarsym(hp^.data.paras[paraidx]);
              { currpt can be changed from loadn to calln when a procvar
                is passed. This is to prevent that the change is permanent }
              currpt:=pt;
              releasecurrpt:=false;
              { retrieve current parameter definitions to compares }
              eq:=te_incompatible;
              def_from:=currpt.resultdef;
              def_to:=currpara.vardef;
              if not(assigned(def_from)) then
               internalerror(200212091);
              if not(
                     assigned(def_to) or
                     ((po_varargs in hp^.data.procoptions) and
                      (currparanr>hp^.data.minparacount))
                    ) then
               internalerror(200212092);

              { Convert tp procvars when not expecting a procvar }
              if (def_to.typ<>procvardef) and
                 (currpt.left.resultdef.typ=procvardef) and
                 { Only convert to call when there is no overload or the return type
                   is equal to the expected type. }
                 (
                  (count=1) or
                  equal_defs(tprocvardef(currpt.left.resultdef).returndef,def_to)
                 ) then
                begin
                  releasecurrpt:=true;
                  currpt:=tcallparanode(pt.getcopy);
                  if maybe_call_procvar(currpt.left,true) then
                    begin
                      currpt.resultdef:=currpt.left.resultdef;
                      def_from:=currpt.left.resultdef;
                    end;
                end;

             { If we expect a procvar and the left is loadnode that
               returns a procdef we need to find the correct overloaded
               procdef that matches the expected procvar. The loadnode
               temporary returned the first procdef (PFV) }
             if (def_to.typ=procvardef) and
                (currpt.left.nodetype=loadn) and
                (currpt.left.resultdef.typ=procdef) then
               begin
                 pdtemp:=tprocsym(Tloadnode(currpt.left).symtableentry).Find_procdef_byprocvardef(Tprocvardef(def_to));
                 if assigned(pdtemp) then
                   begin
                     tloadnode(currpt.left).setprocdef(pdtemp);
                     currpt.resultdef:=currpt.left.resultdef;
                     def_from:=currpt.left.resultdef;
                   end;
               end;

              { varargs are always equal, but not exact }
              if (po_varargs in hp^.data.procoptions) and
                 (currparanr>hp^.data.minparacount) and
                 not is_array_of_const(def_from) and
                 not is_array_constructor(def_from) then
                eq:=te_equal
              else
              { same definition -> exact }
               if (def_from=def_to) then
                 eq:=te_exact
              else
              { for value and const parameters check if a integer is constant or
                included in other integer -> equal and calc ordinal_distance }
               if not(currpara.varspez in [vs_var,vs_out]) and
                  is_integer(def_from) and
                  is_integer(def_to) and
                  is_in_limit(def_from,def_to) then
                 begin
                   eq:=te_equal;
                   hp^.ordinal_distance:=hp^.ordinal_distance+
                     abs(bestreal(torddef(def_from).low)-bestreal(torddef(def_to).low));
                   rth:=bestreal(torddef(def_to).high);
                   rfh:=bestreal(torddef(def_from).high);
                   hp^.ordinal_distance:=hp^.ordinal_distance+abs(rth-rfh);
                   { Give wrong sign a small penalty, this is need to get a diffrence
                     from word->[longword,longint] }
                   if is_signed(def_from)<>is_signed(def_to) then
                   {$ifopt r+}{$define ena_rq}{$q-}{$r-}{$endif}
                     hp^.ordinal_distance:=nextafter(hp^.ordinal_distance,inf);
                   {$ifdef ena_rq}{$r+}{$q+}{$endif}
                 end
              else
              { for value and const parameters check precision of real, give
                penalty for loosing of precision. var and out parameters must match exactly }
               if not(currpara.varspez in [vs_var,vs_out]) and
                  is_real(def_from) and
                  is_real(def_to) then
                 begin
                   eq:=te_equal;
                   if is_extended(def_to) then
                     rth:=4
                   else
                     if is_double (def_to) then
                       rth:=2
                   else
                     rth:=1;
                   if is_extended(def_from) then
                     rfh:=4
                   else
                     if is_double (def_from) then
                       rfh:=2
                   else
                     rfh:=1;
                   { penalty for shrinking of precision }
                   if rth<rfh then
                     rfh:=(rfh-rth)*16
                   else
                     rfh:=rth-rfh;
                   hp^.ordinal_distance:=hp^.ordinal_distance+rfh;
                 end
              else
              { related object parameters also need to determine the distance between the current
                object and the object we are comparing with. var and out parameters must match exactly }
               if not(currpara.varspez in [vs_var,vs_out]) and
                  (def_from.typ=objectdef) and
                  (def_to.typ=objectdef) and
                  (tobjectdef(def_from).objecttype=tobjectdef(def_to).objecttype) and
                  tobjectdef(def_from).is_related(tobjectdef(def_to)) then
                 begin
                   eq:=te_convert_l1;
                   objdef:=tobjectdef(def_from);
                   while assigned(objdef) do
                     begin
                       if objdef=def_to then
                         break;
                       hp^.ordinal_distance:=hp^.ordinal_distance+1;
                       objdef:=objdef.childof;
                     end;
                 end
               { compare_defs_ext compares sets and array constructors very poorly because
                 it has too little information. So we do explicitly a detailed comparisation,
                 see also bug #11288 (FK)
               }
               else if (def_to.typ=setdef) and is_array_constructor(currpt.left.resultdef) then
                 begin
                   n:=currpt.left.getcopy;
                   arrayconstructor_to_set(n);
                   eq:=compare_defs_ext(n.resultdef,def_to,n.nodetype,convtype,pdoper,cdoptions);
                   n.free;
                 end
              else
              { generic type comparision }
               begin
                 eq:=compare_defs_ext(def_from,def_to,currpt.left.nodetype,convtype,pdoper,cdoptions);

                 { when the types are not equal we need to check
                   some special case for parameter passing }
                 if (eq<te_equal) then
                  begin
                    if currpara.varspez in [vs_var,vs_out] then
                      begin
                        { para requires an equal type so the previous found
                          match was not good enough, reset to incompatible }
                        eq:=te_incompatible;
                        { var_para_allowed will return te_equal and te_convert_l1 to
                          make a difference for best matching }
                        var_para_allowed(eq,currpt.resultdef,currpara.vardef,currpt.left)
                      end
                    else
                      para_allowed(eq,currpt,def_to);
                  end;
               end;

              { when a procvar was changed to a call an exact much is
                downgraded to equal. This way an overload call with the
                procvar is choosen. See tb0471 (PFV) }
              if (pt<>currpt) and (eq=te_exact) then
                eq:=te_equal;

              { increase correct counter }
              case eq of
                te_exact :
                  inc(hp^.exact_count);
                te_equal :
                  inc(hp^.equal_count);
                te_convert_l1 :
                  inc(hp^.cl1_count);
                te_convert_l2 :
                  inc(hp^.cl2_count);
                te_convert_l3 :
                  inc(hp^.cl3_count);
                te_convert_l4 :
                  inc(hp^.cl4_count);
                te_convert_l5 :
                  inc(hp^.cl5_count);
                te_convert_operator :
                  inc(hp^.coper_count);
                te_incompatible :
                  hp^.invalid:=true;
                else
                  internalerror(200212072);
              end;

              { stop checking when an incompatible parameter is found }
              if hp^.invalid then
               begin
                 { store the current parameter info for
                   a nice error message when no procedure is found }
                 hp^.wrongparaidx:=paraidx;
                 hp^.wrongparanr:=currparanr;
                 break;
               end;

{$ifdef EXTDEBUG}
              { store equal in node tree for dump }
              currpara.eqval:=eq;
{$endif EXTDEBUG}

              { maybe release temp currpt }
              if releasecurrpt then
                currpt.free;

              { next parameter in the call tree }
              pt:=tcallparanode(pt.right);

              { next parameter for definition, only goto next para
                if we're out of the varargs }
              if not(po_varargs in hp^.data.procoptions) or
                 (currparanr<=hp^.data.maxparacount) then
               begin
                 { Ignore vs_hidden parameters }
                 repeat
                   dec(paraidx);
                 until (paraidx<0) or not(vo_is_hidden_para in tparavarsym(hp^.data.paras[paraidx]).varoptions);
               end;
              dec(currparanr);
            end;
           if not(hp^.invalid) and
              (assigned(pt) or (paraidx>=0) or (currparanr<>0)) then
             internalerror(200212141);
           { next candidate }
           hp:=hp^.next;
         end;
      end;


    function get_variantequaltype(def: tdef): tvariantequaltype;
      const
        variantorddef_cl: array[tordtype] of tvariantequaltype =
          (tve_incompatible,tve_byte,tve_word,tve_cardinal,tve_chari64,
           tve_shortint,tve_smallint,tve_longint,tve_chari64,
           tve_boolformal,tve_boolformal,tve_boolformal,tve_boolformal,tve_boolformal,
           tve_chari64,tve_chari64,tve_dblcurrency);
{ TODO: fixme for 128 bit floats }
        variantfloatdef_cl: array[tfloattype] of tvariantequaltype =
          (tve_single,tve_dblcurrency,tve_extended,
           tve_dblcurrency,tve_dblcurrency,tve_extended);
        variantstringdef_cl: array[tstringtype] of tvariantequaltype =
          (tve_sstring,tve_astring,tve_astring,tve_wstring,tve_unicodestring);
      begin
        case def.typ of
          orddef:
            begin
              result:=variantorddef_cl[torddef(def).ordtype];
            end;
          floatdef:
            begin
              result:=variantfloatdef_cl[tfloatdef(def).floattype];
            end;
          stringdef:
            begin
              result:=variantstringdef_cl[tstringdef(def).stringtype];
            end;
          formaldef:
            begin
              result:=tve_boolformal;
            end;
          else
            begin
              result:=tve_incompatible;
            end;
        end
      end;


    function is_better_candidate(currpd,bestpd:pcandidate):integer;
      var
        res : integer;
      begin
        {
          Return values:
            > 0 when currpd is better than bestpd
            < 0 when bestpd is better than currpd
            = 0 when both are equal

          To choose the best candidate we use the following order:
          - Incompatible flag
          - (Smaller) Number of convert operator parameters.
          - (Smaller) Number of convertlevel 2 parameters.
          - (Smaller) Number of convertlevel 1 parameters.
          - (Bigger) Number of exact parameters.
          - (Smaller) Number of equal parameters.
          - (Smaller) Total of ordinal distance. For example, the distance of a word
            to a byte is 65535-255=65280.
        }
        if bestpd^.invalid then
         begin
           if currpd^.invalid then
            res:=0
           else
            res:=1;
         end
        else
         if currpd^.invalid then
          res:=-1
        else
         begin
           { less operator parameters? }
           res:=(bestpd^.coper_count-currpd^.coper_count);
           if (res=0) then
            begin
             { less cl5 parameters? }
             res:=(bestpd^.cl5_count-currpd^.cl5_count);
             if (res=0) then
              begin
               { less cl4 parameters? }
               res:=(bestpd^.cl4_count-currpd^.cl4_count);
               if (res=0) then
                begin
                 { less cl3 parameters? }
                 res:=(bestpd^.cl3_count-currpd^.cl3_count);
                 if (res=0) then
                  begin
                    { less cl2 parameters? }
                    res:=(bestpd^.cl2_count-currpd^.cl2_count);
                    if (res=0) then
                     begin
                       { less cl1 parameters? }
                       res:=(bestpd^.cl1_count-currpd^.cl1_count);
                       if (res=0) then
                        begin
                          { more exact parameters? }
                          res:=(currpd^.exact_count-bestpd^.exact_count);
                          if (res=0) then
                           begin
                             { less equal parameters? }
                             res:=(bestpd^.equal_count-currpd^.equal_count);
                             if (res=0) then
                              begin
                                { smaller ordinal distance? }
                                if (currpd^.ordinal_distance<bestpd^.ordinal_distance) then
                                 res:=1
                                else
                                 if (currpd^.ordinal_distance>bestpd^.ordinal_distance) then
                                  res:=-1
                                else
                                 res:=0;
                              end;
                           end;
                        end;
                     end;
                  end;
                end;
              end;
            end;
         end;
        is_better_candidate:=res;
      end;


{ Delphi precedence rules extracted from test programs. Only valid if passing
  a variant parameter to overloaded procedures expecting exactly one parameter.

  single > (char, currency, int64, shortstring, ansistring, widestring, extended, double)
  double/currency > (char, int64, shortstring, ansistring, widestring, extended)
  extended > (char, int64, shortstring, ansistring, widestring)
  longint/cardinal > (int64, shortstring, ansistring, widestring, extended, double, single, char, currency)
  smallint > (longint, int64, shortstring, ansistring, widestring, extended, double single, char, currency);
  word > (longint, cardinal, int64, shortstring, ansistring, widestring, extended, double single, char, currency);
  shortint > (longint, smallint, int64, shortstring, ansistring, widestring, extended, double, single, char, currency)
  byte > (longint, cardinal, word, smallint, int64, shortstring, ansistring, widestring, extended, double, single, char, currency);
  boolean/formal > (char, int64, shortstring, ansistring, widestring)
  shortstring > (char, int64, ansistring, widestring)
  ansistring > (char, int64, widestring)
  widestring > (char, int64)

  Relations not mentioned mean that they conflict: no decision possible }

    function is_better_candidate_single_variant(currpd,bestpd:pcandidate):integer;

      function calculate_relation(const currvcl, bestvcl, testvcl:
          tvariantequaltype; const conflictvcls: tvariantequaltypes):integer;
        begin
          { if (bestvcl=conflictvcl) or
               (currvcl=conflictvcl) then
              result:=0
            else if (bestvcl=testvcl) then
              result:=-1
            else result:=1 }
          result:=1-2*ord(bestvcl=testvcl)+
                  ord(currvcl in conflictvcls)-ord(bestvcl in conflictvcls);
        end;


        function getfirstrealparaidx(pd: pcandidate): integer;
          begin
            { can be different for currpd and bestpd in case of overloaded }
            { functions, e.g. lowercase():char and lowercase():shortstring }
            { (depending on the calling convention and parameter order)    }
            result:=pd^.firstparaidx;
            while (result>=0) and (vo_is_hidden_para in tparavarsym(pd^.data.paras[result]).varoptions) do
              dec(result);
            if (vo_is_hidden_para in tparavarsym(pd^.data.paras[result]).varoptions) then
              internalerror(2006122803);
          end;

      var
        currpara, bestpara: tparavarsym;
        currvcl, bestvcl: tvariantequaltype;
      begin
        {
          Return values:
            > 0 when currpd is better than bestpd
            < 0 when bestpd is better than currpd
            = 0 when both are equal
        }
        currpara:=tparavarsym(currpd^.data.paras[getfirstrealparaidx(currpd)]);
        bestpara:=tparavarsym(bestpd^.data.paras[getfirstrealparaidx(bestpd)]);

        { if one of the parameters is a regular variant, fall back to the }
        { default algorithm                                               }
        if (currpara.vardef.typ = variantdef) or
           (bestpara.vardef.typ = variantdef) then
          begin
            result:=is_better_candidate(currpd,bestpd);
            exit;
          end;

        currvcl:=get_variantequaltype(currpara.vardef);
        bestvcl:=get_variantequaltype(bestpara.vardef);

        { sanity check }
        result:=-5;

        { if both are the same, there is a conflict }
        if (currvcl=bestvcl) then
          result:=0
        { if one of the two cannot be used as variant, the other is better }
        else if (bestvcl=tve_incompatible) then
          result:=1
        else if (currvcl=tve_incompatible) then
          result:=-1
        { boolean and formal are better than chari64str, but conflict with }
        { everything else                                                  }
        else if (currvcl=tve_boolformal) or
                (bestvcl=tve_boolformal) then
          if (currvcl=tve_boolformal) then
            result:=ord(bestvcl in [tve_chari64,tve_sstring,tve_astring,tve_wstring])
          else
            result:=-ord(currvcl in [tve_chari64,tve_sstring,tve_astring,tve_wstring])
        { byte is better than everything else (we assume both aren't byte, }
        { since there's only one parameter and that one can't be the same) }
        else if (currvcl=tve_byte) or
                (bestvcl=tve_byte) then
          result:=calculate_relation(currvcl,bestvcl,tve_byte,[tve_shortint])
        { shortint conflicts with word and cardinal, but is better than    }
        { everything else but byte (which has already been handled)        }
        else if (currvcl=tve_shortint) or
                (bestvcl=tve_shortint) then
          result:=calculate_relation(currvcl,bestvcl,tve_shortint,[tve_word, tve_cardinal])
        { word conflicts with smallint, but is better than everything else }
        { but shortint and byte (which has already been handled)           }
        else if (currvcl=tve_word) or
                (bestvcl=tve_word) then
          result:=calculate_relation(currvcl,bestvcl,tve_word,[tve_smallint])
        { smallint conflicts with cardinal, but is better than everything  }
        { which has not yet been tested                                    }
        else if (currvcl=tve_smallint) or
                (bestvcl=tve_smallint) then
          result:=calculate_relation(currvcl,bestvcl,tve_smallint,[tve_cardinal])
        { cardinal conflicts with each longint and is better than everything }
        { which has not yet been tested                                      }
        else if (currvcl=tve_cardinal) or
                (bestvcl=tve_cardinal) then
          result:=calculate_relation(currvcl,bestvcl,tve_cardinal,[tve_longint])
        { longint is better than everything which has not yet been tested }
        else if (currvcl=tve_longint) or
                (bestvcl=tve_longint) then
          { if bestvcl=tve_longint then
              result:=-1
            else
              result:=1 }
          result:=1-2*ord(bestvcl=tve_longint)
        { single is better than everything left }
        else if (currvcl=tve_single) or
                (bestvcl=tve_single) then
          result:=1-2*ord(bestvcl=tve_single)
        { double/comp/currency are better than everything left, and conflict }
        { with each other (but that's already tested)                        }
        else if (currvcl=tve_dblcurrency) or
                (bestvcl=tve_dblcurrency) then
          result:=1-2*ord(bestvcl=tve_dblcurrency)
        { extended is better than everything left }
        else if (currvcl=tve_extended) or
                (bestvcl=tve_extended) then
          result:=1-2*ord(bestvcl=tve_extended)
        { shortstring is better than everything left }
        else if (currvcl=tve_sstring) or
                (bestvcl=tve_sstring) then
          result:=1-2*ord(bestvcl=tve_sstring)
        { ansistring is better than everything left }
        else if (currvcl=tve_astring) or
                (bestvcl=tve_astring) then
          result:=1-2*ord(bestvcl=tve_astring)
        { widestring is better than everything left }
        else if (currvcl=tve_wstring) or
                (bestvcl=tve_wstring) then
          result:=1-2*ord(bestvcl=tve_wstring);

        { all possibilities should have been checked now }
        if (result=-5) then
          internalerror(2006122805);
      end;


    function tcallcandidates.choose_best(var bestpd:tabstractprocdef; singlevariant: boolean):integer;
      var
        besthpstart,
        hp            : pcandidate;
        cntpd,
        res           : integer;
      begin
        {
          Returns the number of candidates left and the
          first candidate is returned in pdbest
        }
        { Setup the first procdef as best, only count it as a result
          when it is valid }
        bestpd:=FProcs^.data;
        if FProcs^.invalid then
         cntpd:=0
        else
         cntpd:=1;
        if assigned(FProcs^.next) then
         begin
           besthpstart:=FProcs;
           hp:=FProcs^.next;
           while assigned(hp) do
            begin
              if not singlevariant then
                res:=is_better_candidate(hp,besthpstart)
              else
                res:=is_better_candidate_single_variant(hp,besthpstart);
              if (res>0) then
               begin
                 { hp is better, flag all procs to be incompatible }
                 while (besthpstart<>hp) do
                  begin
                    besthpstart^.invalid:=true;
                    besthpstart:=besthpstart^.next;
                  end;
                 { besthpstart is already set to hp }
                 bestpd:=besthpstart^.data;
                 cntpd:=1;
               end
              else
               if (res<0) then
                begin
                  { besthpstart is better, flag current hp to be incompatible }
                  hp^.invalid:=true;
                end
              else
               begin
                 { res=0, both are valid }
                 if not hp^.invalid then
                   inc(cntpd);
               end;
              hp:=hp^.next;
            end;
         end;

        result:=cntpd;
      end;


    procedure tcallcandidates.find_wrong_para;
      var
        currparanr : smallint;
        hp : pcandidate;
        pt : tcallparanode;
        wrongpara : tparavarsym;
      begin
        { Only process the first overloaded procdef }
        hp:=FProcs;
        { Find callparanode corresponding to the argument }
        pt:=tcallparanode(FParanode);
        currparanr:=FParalength;
        while assigned(pt) and
              (currparanr>hp^.wrongparanr) do
         begin
           pt:=tcallparanode(pt.right);
           dec(currparanr);
         end;
        if (currparanr<>hp^.wrongparanr) or
           not assigned(pt) then
          internalerror(200212094);
        { Show error message, when it was a var or out parameter
          guess that it is a missing typeconv }
        wrongpara:=tparavarsym(hp^.data.paras[hp^.wrongparaidx]);
        if wrongpara.varspez in [vs_var,vs_out] then
          begin
            { Maybe passing the correct type but passing a const to var parameter }
            if (compare_defs(pt.resultdef,wrongpara.vardef,pt.nodetype)<>te_incompatible) and
               not valid_for_var(pt.left,true) then
              CGMessagePos(pt.left.fileinfo,type_e_variable_id_expected)
            else
              CGMessagePos3(pt.left.fileinfo,parser_e_call_by_ref_without_typeconv,tostr(hp^.wrongparanr),
                FullTypeName(pt.left.resultdef,wrongpara.vardef),
                FullTypeName(wrongpara.vardef,pt.left.resultdef))
          end
        else
          CGMessagePos3(pt.left.fileinfo,type_e_wrong_parameter_type,tostr(hp^.wrongparanr),
            FullTypeName(pt.left.resultdef,wrongpara.vardef),
            FullTypeName(wrongpara.vardef,pt.left.resultdef));
      end;


    procedure check_hints(const srsym: tsym; const symoptions: tsymoptions);
      begin
        if not assigned(srsym) then
          internalerror(200602051);
        if sp_hint_deprecated in symoptions then
          Message1(sym_w_deprecated_symbol,srsym.realname);
        if sp_hint_experimental in symoptions then
          Message1(sym_w_experimental_symbol,srsym.realname);
        if sp_hint_platform in symoptions then
          Message1(sym_w_non_portable_symbol,srsym.realname);
        if sp_hint_unimplemented in symoptions then
          Message1(sym_w_non_implemented_symbol,srsym.realname);
      end;


    procedure check_ranges(const location: tfileposinfo; source: tnode; destdef: tdef);
      begin
        if not(cs_check_ordinal_size in current_settings.localswitches) then
          exit;
        { check if the assignment may cause a range check error }
        { if its not explicit, and only if the values are       }
        { ordinals, enumdef and floatdef                        }
        if assigned(destdef) and
          (destdef.typ in [enumdef,orddef,floatdef]) and
          not is_boolean(destdef) and
          assigned(source.resultdef) and
          (source.resultdef.typ in [enumdef,orddef,floatdef]) and
          not is_boolean(source.resultdef) and
          not is_constrealnode(source) then
         begin
           if (destdef.size < source.resultdef.size) then
             begin
               if (cs_check_range in current_settings.localswitches) then
                 MessagePos(location,type_w_smaller_possible_range_check)
               else
                 MessagePos(location,type_h_smaller_possible_range_check);
             end;
         end;
      end;


end.
