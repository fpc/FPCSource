{
    $Id$
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
      node,
      symconst,symtype,symdef,symsym,symbase;

    type
      Ttok2nodeRec=record
        tok : ttoken;
        nod : tnodetype;
        op_overloading_supported : boolean;
      end;

      pcandidate = ^tcandidate;
      tcandidate = record
         next        : pcandidate;
         data        : tprocdef;
         wrongpara,
         firstpara   : tparaitem;
         exact_count,
         equal_count,
         cl1_count,
         cl2_count,
         cl3_count,
         coper_count : integer; { should be signed }
         ordinal_distance : bestreal;
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
        function proc_add(pd:tprocdef):pcandidate;
      public
        constructor create(sym:tprocsym;st:tsymtable;ppn:tnode;isprop:boolean);
        constructor create_operator(op:ttoken;ppn:tnode);
        destructor destroy;override;
        procedure list(all:boolean);
{$ifdef EXTDEBUG}
        procedure dump_info(lvl:longint);
{$endif EXTDEBUG}
        procedure get_information;
        function  choose_best(var bestpd:tabstractprocdef):integer;
        procedure find_wrong_para;
        property  Count:integer read FProcCnt;
        property  VisibleCount:integer read FProcVisibleCnt;
      end;

    const
      tok2nodes=25;
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
        (tok:_CARET   ;nod:caretn;op_overloading_supported:false),    { binary overloading NOT supported }
        (tok:_UNEQUAL ;nod:unequaln;op_overloading_supported:false)   { binary overloading NOT supported  overload = instead }
      );
    const
    { firstcallparan without varspez we don't count the ref }
{$ifdef extdebug}
       count_ref : boolean = true;
{$endif def extdebug}
       allow_array_constructor : boolean = false;

    function node2opstr(nt:tnodetype):string;

    { check operator args and result type }
    function isbinaryoperatoroverloadable(treetyp:tnodetype;ld:tdef;lt:tnodetype;rd:tdef;rt:tnodetype) : boolean;
    function isoperatoracceptable(pf : tprocdef; optoken : ttoken) : boolean;
    function isunaryoverloaded(var t : tnode) : boolean;
    function isbinaryoverloaded(var t : tnode) : boolean;

    { Register Allocation }
    procedure make_not_regable(p : tnode);
    procedure calcregisters(p : tbinarynode;r32,fpu,mmx : word);

    { subroutine handling }
    function  is_procsym_load(p:tnode):boolean;
    procedure test_local_to_procvar(from_def:tprocvardef;to_def:tdef);

    { sets varsym varstate field correctly }
    procedure set_varstate(p:tnode;newstate:tvarstate;must_be_valid:boolean);

    { sets the callunique flag, if the node is a vecn, }
    { takes care of type casts etc.                 }
    procedure set_unique(p : tnode);

    function  valid_for_formal_var(p : tnode) : boolean;
    function  valid_for_formal_const(p : tnode) : boolean;
    function  valid_for_var(p:tnode):boolean;
    function  valid_for_assignment(p:tnode):boolean;


implementation

    uses
       globtype,systems,
       cutils,verbose,globals,
       symtable,
       defutil,defcmp,
       pass_1,nbas,ncnv,nld,nmem,ncal,nmat,nutils,
       cgbase,procinfo
       ;

    type
      TValidAssign=(Valid_Property,Valid_Void);
      TValidAssigns=set of TValidAssign;


    function node2opstr(nt:tnodetype):string;
      var
        i : integer;
      begin
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
          case ld.deftype of
            formaldef,
            recorddef,
            variantdef :
              begin
                allowed:=true;
              end;
            procvardef :
              begin
                if (rd.deftype in [pointerdef,procdef,procvardef]) then
                 begin
                   allowed:=false;
                   exit;
                 end;
                allowed:=true;
              end;
            pointerdef :
              begin
                if ((rd.deftype in [orddef,pointerdef,classrefdef,procvardef]) or
                    is_class_or_interface(rd)) then
                 begin
                   allowed:=false;
                   exit;
                 end;

                { don't allow pchar+string }
                if is_pchar(ld) and
                   (is_char(rd) or
                    is_widechar(rd) or
                    is_pchar(rd) or
                    is_pwidechar(rd) or
                    is_integer(rd) or
                    (rd.deftype=stringdef) or
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
                { not mmx }
                if (cs_mmx in aktlocalswitches) and
                   is_mmx_able_array(ld) then
                 begin
                   allowed:=false;
                   exit;
                 end;
                { not chararray+[(wide)char,(wide)string,(wide)chararray] }
                if is_chararray(ld) and
                   (is_char(rd) or
                    is_widechar(rd) or
                    is_pchar(rd) or
                    is_pwidechar(rd) or
                    is_integer(rd) or
                    (rd.deftype=stringdef) or
                    is_chararray(rd) or
                    is_widechararray(rd) or
                    (rt=niln)) then
                 begin
                   allowed:=false;
                   exit;
                 end;
                { dynamic array compare with niln }
                if is_dynamic_array(ld) and
                   (rt=niln) and
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
                if ((rd.deftype=stringdef) or
                    is_char(rd) or
                    is_widechar(rd) or
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
              if is_integer(ld) or
                 (ld.deftype=floatdef) then
                exit;

{$ifdef SUPPORT_MMX}
              if (cs_mmx in aktlocalswitches) and
                 is_mmx_able_array(ld) then
                exit;
{$endif SUPPORT_MMX}

              result:=true;
            end;

          notn :
            begin
              if is_integer(ld) or
                 is_boolean(ld) then
                exit;

{$ifdef SUPPORT_MMX}
              if (cs_mmx in aktlocalswitches) and
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
      begin
        result:=false;
        case pf.parast.symindex.count of
          1 : begin
                ld:=tvarsym(pf.parast.symindex.first).vartype.def;
                { assignment is a special case }
                if optoken=_ASSIGNMENT then
                  begin
                    eq:=compare_defs_ext(ld,pf.rettype.def,nothingn,conv,pd,[cdo_explicit]);
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
                      ld:=tvarsym(pf.parast.symindex.first).vartype.def;
                      rd:=tvarsym(pf.parast.symindex.first.indexnext).vartype.def;
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
        ld:=tunarynode(t).left.resulttype.def;
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
        cand_cnt:=candidates.choose_best(operpd);

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
            CGMessage(cg_e_cant_choose_overload_function);
{$ifdef EXTDEBUG}
            candidates.dump_info(V_Hint);
{$else EXTDEBUG}
            candidates.list(false);
{$endif EXTDEBUG}
            { we'll just use the first candidate to make the
              call }
          end;
        candidates.free;

        inc(operpd.procsym.refs);

        { the nil as symtable signs firstcalln that this is
          an overloaded operator }
        t:=ccallnode.create(ppn,Tprocsym(operpd.procsym),nil,nil);

        { we already know the procdef to use, so it can
          skip the overload choosing in callnode.det_resulttype }
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
        ld:=tbinarynode(t).left.resulttype.def;
        rd:=tbinarynode(t).right.resulttype.def;
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
        cand_cnt:=candidates.choose_best(operpd);

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
            CGMessage(cg_e_cant_choose_overload_function);
{$ifdef EXTDEBUG}
            candidates.dump_info(V_Hint);
{$else EXTDEBUG}
            candidates.list(false);
{$endif EXTDEBUG}
            { we'll just use the first candidate to make the
              call }
          end;
        candidates.free;

        inc(operpd.procsym.refs);

        { the nil as symtable signs firstcalln that this is
          an overloaded operator }
        ht:=ccallnode.create(ppn,Tprocsym(operpd.procsym),nil,nil);

        { we already know the procdef to use, so it can
          skip the overload choosing in callnode.det_resulttype }
        tcallnode(ht).procdefinition:=operpd;

        if t.nodetype=unequaln then
          ht:=cnotnode.create(ht);
        t:=ht;
      end;


{****************************************************************************
                          Register Calculation
****************************************************************************}

    { marks an lvalue as "unregable" }
    procedure make_not_regable(p : tnode);
      begin
         case p.nodetype of
            typeconvn :
              make_not_regable(ttypeconvnode(p).left);
            loadn :
              if tloadnode(p).symtableentry.typ=varsym then
                tvarsym(tloadnode(p).symtableentry).varoptions:=tvarsym(tloadnode(p).symtableentry).varoptions-[vo_regable,vo_fpuregable];
         end;
      end;


    { calculates the needed registers for a binary operator }
    procedure calcregisters(p : tbinarynode;r32,fpu,mmx : word);

      begin
         p.left_right_max;

      { Only when the difference between the left and right registers < the
        wanted registers allocate the amount of registers }

        if assigned(p.left) then
         begin
           if assigned(p.right) then
            begin
              { the location must be already filled in because we need it to }
              { calculate the necessary number of registers (JM)             }
              if p.expectloc = LOC_INVALID then
                internalerror(200110101);

              if (abs(p.left.registersint-p.right.registersint)<r32) or
                 ((p.expectloc = LOC_FPUREGISTER) and
                  (p.right.registersfpu <= p.left.registersfpu) and
                  ((p.right.registersfpu <> 0) or (p.left.registersfpu <> 0)) and
                  (p.left.registersint   < p.right.registersint)) then
                inc(p.registersint,r32);
              if (abs(p.left.registersfpu-p.right.registersfpu)<fpu) then
               inc(p.registersfpu,fpu);
{$ifdef SUPPORT_MMX}
              if (abs(p.left.registersmmx-p.right.registersmmx)<mmx) then
               inc(p.registersmmx,mmx);
{$endif SUPPORT_MMX}
              { the following is a little bit guessing but I think }
              { it's the only way to solve same internalerrors:    }
              { if the left and right node both uses registers     }
              { and return a mem location, but the current node    }
              { doesn't use an integer register we get probably    }
              { trouble when restoring a node                      }
              if (p.left.registersint=p.right.registersint) and
                 (p.registersint=p.left.registersint) and
                 (p.registersint>0) and
                (p.left.expectloc in [LOC_REFERENCE,LOC_CREFERENCE]) and
                (p.right.expectloc in [LOC_REFERENCE,LOC_CREFERENCE]) then
                inc(p.registersint);
            end
           else
            begin
              if (p.left.registersint<r32) then
               inc(p.registersint,r32);
              if (p.left.registersfpu<fpu) then
               inc(p.registersfpu,fpu);
{$ifdef SUPPORT_MMX}
              if (p.left.registersmmx<mmx) then
               inc(p.registersmmx,mmx);
{$endif SUPPORT_MMX}
            end;
         end;

         { error CGMessage, if more than 8 floating point }
         { registers are needed                         }
         { if p.registersfpu>maxfpuregs then
          CGMessage(cg_e_too_complex_expr); now pushed if needed PM }
      end;


{****************************************************************************
                          Subroutine Handling
****************************************************************************}

    function is_procsym_load(p:tnode):boolean;
      begin
         { ignore vecn,subscriptn }
         repeat
           case p.nodetype of
             vecn :
               p:=tvecnode(p).left;
             subscriptn :
               p:=tsubscriptnode(p).left;
             else
               break;
           end;
         until false;
         is_procsym_load:=((p.nodetype=loadn) and (tloadnode(p).symtableentry.typ=procsym)) or
                          ((p.nodetype=addrn) and (taddrnode(p).left.nodetype=loadn)
                          and (tloadnode(taddrnode(p).left).symtableentry.typ=procsym)) ;
      end;


    { local routines can't be assigned to procvars }
    procedure test_local_to_procvar(from_def:tprocvardef;to_def:tdef);
      begin
         if (from_def.parast.symtablelevel>normal_function_level) and
            (to_def.deftype=procvardef) then
           CGMessage(type_e_cannot_local_proc_to_procvar);
      end;


    procedure set_varstate(p:tnode;newstate:tvarstate;must_be_valid:boolean);
      var
        hsym : tvarsym;
      begin
        while assigned(p) do
         begin
           case p.nodetype of
             typeconvn :
               begin
                 case ttypeconvnode(p).convtype of
                   tc_cchar_2_pchar,
                   tc_cstring_2_pchar,
                   tc_array_2_pointer :
                     must_be_valid:=false;
                   tc_pchar_2_string,
                   tc_pointer_2_array :
                     must_be_valid:=true;
                 end;
                 p:=tunarynode(p).left;
               end;
             subscriptn :
               p:=tunarynode(p).left;
             vecn:
               begin
                 set_varstate(tbinarynode(p).right,vs_used,true);
                 if not(tunarynode(p).left.resulttype.def.deftype in [stringdef,arraydef]) then
                   must_be_valid:=true;
                 p:=tunarynode(p).left;
               end;
             { do not parse calln }
             calln :
               break;
             loadn :
               begin
                 if (tloadnode(p).symtableentry.typ=varsym) then
                  begin
                    hsym:=tvarsym(tloadnode(p).symtableentry);
                    if must_be_valid and (hsym.varstate=vs_declared) then
                      begin
                        { Give warning/note for uninitialized locals }
                        if assigned(hsym.owner) and
                           not(vo_is_external in hsym.varoptions) and
                           (hsym.owner.symtabletype in [localsymtable,staticsymtable]) and
                           (hsym.owner=current_procinfo.procdef.localst) then
                          begin
                            if (vo_is_funcret in hsym.varoptions) then
                               CGMessage(sym_w_function_result_not_set)
                            else
                             if tloadnode(p).symtable.symtabletype=localsymtable then
                               CGMessage1(sym_n_uninitialized_local_variable,hsym.realname)
                            else
                              CGMessage1(sym_n_uninitialized_variable,hsym.realname);
                          end;
                      end;
                    { don't override vs_used with vs_assigned }
                    if hsym.varstate<>vs_used then
                      hsym.varstate:=newstate;
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


    function  valid_for_assign(p:tnode;opts:TValidAssigns):boolean;
      var
        hp : tnode;
        gotwith,
        gotsubscript,
        gotpointer,
        gotvec,
        gotclass,
        gotderef : boolean;
        fromdef,
        todef    : tdef;
      begin
        valid_for_assign:=false;
        gotsubscript:=false;
        gotvec:=false;
        gotderef:=false;
        gotclass:=false;
        gotpointer:=false;
        gotwith:=false;
        hp:=p;
        if not(valid_void in opts) and
           is_void(hp.resulttype.def) then
         begin
           CGMessagePos(hp.fileinfo,type_e_argument_cant_be_assigned);
           exit;
         end;
        while assigned(hp) do
         begin
           { property allowed? calln has a property check itself }
           if (nf_isproperty in hp.flags) then
            begin
              if (valid_property in opts) then
               valid_for_assign:=true
              else
               begin
                 { check return type }
                 case hp.resulttype.def.deftype of
                   pointerdef :
                     gotpointer:=true;
                   objectdef :
                     gotclass:=is_class_or_interface(hp.resulttype.def);
                   recorddef, { handle record like class it needs a subscription }
                   classrefdef :
                     gotclass:=true;
                 end;
                 { 1. if it returns a pointer and we've found a deref,
                   2. if it returns a class or record and a subscription or with is found }
                 if (gotpointer and gotderef) or
                    (gotclass and (gotsubscript or gotwith)) then
                   valid_for_assign:=true
                 else
                   CGMessagePos(hp.fileinfo,type_e_argument_cant_be_assigned);
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
                 fromdef:=ttypeconvnode(hp).left.resulttype.def;
                 todef:=hp.resulttype.def;
                 if not((nf_absolute in ttypeconvnode(hp).flags) or
                        (fromdef.deftype=formaldef) or
                        is_void(fromdef) or
                        is_open_array(fromdef) or
                        is_open_array(todef) or
                        ((fromdef.deftype=pointerdef) and (todef.deftype=arraydef)) or
                        ((fromdef.deftype = objectdef) and (todef.deftype = objectdef) and
                         (tobjectdef(fromdef).is_related(tobjectdef(todef))))) and
                    (fromdef.size<>todef.size) then
                  begin
                    { in TP it is allowed to typecast to smaller types }
                    if not(m_tp7 in aktmodeswitches) or
                       (todef.size>fromdef.size) then
                     CGMessagePos2(hp.fileinfo,type_e_typecast_wrong_size_for_assignment,tostr(fromdef.size),tostr(todef.size));
                  end;
                 case hp.resulttype.def.deftype of
                   pointerdef :
                     gotpointer:=true;
                   objectdef :
                     gotclass:=is_class_or_interface(hp.resulttype.def);
                   classrefdef :
                     gotclass:=true;
                   arraydef :
                     begin
                       { pointer -> array conversion is done then we need to see it
                         as a deref, because a ^ is then not required anymore }
                       if (ttypeconvnode(hp).left.resulttype.def.deftype=pointerdef) then
                        gotderef:=true;
                     end;
                 end;
                 hp:=ttypeconvnode(hp).left;
               end;
             vecn :
               begin
                 gotvec:=true;
                 hp:=tunarynode(hp).left;
               end;
             asn :
               begin
                 { asn can't be assigned directly, it returns the value in a register instead
                   of reference. }
                 if not(gotsubscript or gotderef or gotvec) then
                   begin
                     CGMessagePos(hp.fileinfo,type_e_argument_cant_be_assigned);
                     exit;
                   end;
                 hp:=tunarynode(hp).left;
               end;
             subscriptn :
               begin
                 gotsubscript:=true;
                 { a class/interface access is an implicit }
                 { dereferencing                           }
                 hp:=tsubscriptnode(hp).left;
                 if is_class_or_interface(hp.resulttype.def) then
                   gotderef:=true;
               end;
             subn,
             addn :
               begin
                 { Allow add/sub operators on a pointer, or an integer
                   and a pointer typecast and deref has been found }
                 if ((hp.resulttype.def.deftype=pointerdef) or
                     (is_integer(hp.resulttype.def) and gotpointer)) and
                    gotderef then
                  valid_for_assign:=true
                 else
                  CGMessagePos(hp.fileinfo,type_e_variable_id_expected);
                 exit;
               end;
             addrn :
               begin
                 if gotderef or
                    (nf_procvarload in hp.flags) then
                  valid_for_assign:=true
                 else
                  CGMessagePos(hp.fileinfo,type_e_no_assign_to_addr);
                 exit;
               end;
             calln :
               begin
                 { check return type }
                 case hp.resulttype.def.deftype of
                   arraydef :
                     begin
                       { dynamic arrays are allowed when there is also a
                         vec node }
                       if is_dynamic_array(hp.resulttype.def) and
                          gotvec then
                        begin
                          gotderef:=true;
                          gotpointer:=true;
                        end;
                     end;
                   pointerdef :
                     gotpointer:=true;
                   objectdef :
                     gotclass:=is_class_or_interface(hp.resulttype.def);
                   recorddef, { handle record like class it needs a subscription }
                   classrefdef :
                     gotclass:=true;
                 end;
                 { 1. if it returns a pointer and we've found a deref,
                   2. if it returns a class or record and a subscription or with is found }
                 if (gotpointer and gotderef) or
                    (gotclass and (gotsubscript or gotwith)) then
                  valid_for_assign:=true
                 else
                  CGMessagePos(hp.fileinfo,type_e_argument_cant_be_assigned);
                 exit;
               end;
             loadn :
               begin
                 case tloadnode(hp).symtableentry.typ of
                   absolutesym,
                   varsym :
                     begin
                       if (tvarsym(tloadnode(hp).symtableentry).varspez=vs_const) then
                        begin
                          { allow p^:= constructions with p is const parameter }
                          if gotderef then
                           valid_for_assign:=true
                          else
                           CGMessagePos(tloadnode(hp).fileinfo,type_e_no_assign_to_const);
                          exit;
                        end;
                       { Are we at a with symtable, then we need to process the
                         withrefnode also to check for maybe a const load }
                       if (tloadnode(hp).symtable.symtabletype=withsymtable) then
                        begin
                          { continue with processing the withref node }
                          hp:=tnode(twithsymtable(tloadnode(hp).symtable).withrefnode);
                          gotwith:=true;
                        end
                       else
                        begin
                          valid_for_assign:=true;
                          exit;
                        end;
                     end;
                   typedconstsym :
                     begin
                       if ttypedconstsym(tloadnode(hp).symtableentry).is_writable then
                        valid_for_assign:=true
                       else
                        CGMessagePos(hp.fileinfo,type_e_no_assign_to_const);
                       exit;
                     end;
                   else
                     begin
                       CGMessagePos(hp.fileinfo,type_e_variable_id_expected);
                       exit;
                     end;
                 end;
               end;
             else
               begin
                 CGMessagePos(hp.fileinfo,type_e_variable_id_expected);
                 exit;
               end;
            end;
         end;
      end;


    function  valid_for_var(p:tnode):boolean;
      begin
        valid_for_var:=valid_for_assign(p,[]);
      end;


    function  valid_for_formal_var(p : tnode) : boolean;
      begin
        valid_for_formal_var:=valid_for_assign(p,[valid_void]);
      end;


    function  valid_for_formal_const(p : tnode) : boolean;
      var
        v : boolean;
      begin
        { p must have been firstpass'd before }
        { accept about anything but not a statement ! }
        case p.nodetype of
          calln,
          statementn,
          addrn :
           begin
             { addrn is not allowed as this generate a constant value,
               but a tp procvar are allowed (PFV) }
             if nf_procvarload in p.flags then
              v:=true
             else
              v:=false;
           end;
          else
            v:=true;
        end;
        valid_for_formal_const:=v;
      end;


    function  valid_for_assignment(p:tnode):boolean;
      begin
        valid_for_assignment:=valid_for_assign(p,[valid_property]);
      end;


    procedure var_para_allowed(var eq:tequaltype;def_from,def_to:Tdef);
      begin
        { Note: eq must be already valid, it will only be updated! }
        case def_to.deftype of
          formaldef :
            begin
              { all types can be passed to a formaldef }
              eq:=te_equal;
            end;
          orddef :
            begin
              { allows conversion from word to integer and
                byte to shortint, but only for TP7 compatibility }
              if (m_tp7 in aktmodeswitches) and
                 (def_from.deftype=orddef) and
                 (def_from.size=def_to.size) then
                eq:=te_convert_l1;
            end;
          arraydef :
            begin
              if is_open_array(def_to) and
                 is_dynamic_array(def_from) and
                equal_defs(tarraydef(def_from).elementtype.def,tarraydef(def_to).elementtype.def) then
                eq:=te_convert_l2;
            end;
          pointerdef :
            begin
              { an implicit pointer conversion is allowed }
              if (def_from.deftype=pointerdef) then
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
              if (def_from.deftype=objectdef) and
                 (
                  not(m_delphi in aktmodeswitches) or
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
              if (def_from.deftype=filedef) and
                 (tfiledef(def_from).filetyp = ft_typed) and
                 (tfiledef(def_to).filetyp = ft_untyped) then
                eq:=te_convert_l1;
            end;
        end;
      end;


    procedure para_allowed(var eq:tequaltype;p:tcallparanode;def_to:tdef);
      begin
        { Note: eq must be already valid, it will only be updated! }
        case def_to.deftype of
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
              if (p.resulttype.def.deftype=stringdef) and
                 (tstringdef(def_to).string_typ=tstringdef(p.resulttype.def).string_typ) then
                eq:=te_equal
              else
              { Passing a constant char to ansistring or shortstring or
                a widechar to widestring then handle it as equal. }
               if (p.left.nodetype=ordconstn) and
                  (
                   is_char(p.resulttype.def) and
                   (is_shortstring(def_to) or is_ansistring(def_to))
                  ) or
                  (
                   is_widechar(p.resulttype.def) and
                   is_widestring(def_to)
                  ) then
                eq:=te_equal
            end;
          setdef :
            begin
              { set can also be a not yet converted array constructor }
              if (p.resulttype.def.deftype=arraydef) and
                 (tarraydef(p.resulttype.def).IsConstructor) and
                 not(tarraydef(p.resulttype.def).IsVariant) then
                eq:=te_equal;
            end;
          procvardef :
            begin
              { in tp7 mode proc -> procvar is allowed }
              if (m_tp_procvar in aktmodeswitches) and
                 (p.left.nodetype=calln) and
                 (proc_to_procvar_equal(tprocdef(tcallnode(p.left).procdefinition),tprocvardef(def_to),true)>=te_equal) then
               eq:=te_equal;
            end;
        end;
      end;



{****************************************************************************
                           TCallCandidates
****************************************************************************}

    constructor tcallcandidates.create(sym:tprocsym;st:tsymtable;ppn:tnode;isprop:boolean);
      var
        j          : integer;
        pd         : tprocdef;
        hp         : pcandidate;
        found,
        has_overload_directive : boolean;
        topclassh  : tobjectdef;
        srsymtable : tsymtable;
        srprocsym  : tprocsym;
        pt         : tcallparanode;

      begin
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
           (sym.owner.symtabletype=objectsymtable) and
           (po_overload in sym.first_procdef.procoptions) then
         search_class_overloads(sym);

        { when the class passed is defined in this unit we
          need to use the scope of that class. This is a trick
          that can be used to access protected members in other
          units. At least kylix supports it this way (PFV) }
        if assigned(st) and
           (st.symtabletype=objectsymtable) and
           (st.defowner.owner.symtabletype in [globalsymtable,staticsymtable]) and
           (st.defowner.owner.unitid=0) then
          topclassh:=tobjectdef(st.defowner)
        else
          begin
            if assigned(current_procinfo) then
              topclassh:=current_procinfo.procdef._class
            else
              topclassh:=nil;
          end;

        { link all procedures which have the same # of parameters }
        for j:=1 to sym.procdef_count do
          begin
            pd:=sym.procdef[j];
            { Is the procdef visible? This needs to be checked on
              procdef level since a symbol can contain both private and
              public declarations. But the check should not be done
              when the callnode is generated by a property }
            if isprop or
               (pd.owner.symtabletype<>objectsymtable) or
               pd.is_visible_for_object(topclassh) then
             begin
               { we have at least one procedure that is visible }
               inc(FProcvisiblecnt);
               { only when the # of parameter are supported by the
                 procedure }
               if (FParalength>=pd.minparacount) and
                  ((po_varargs in pd.procoptions) or { varargs }
                   (FParalength<=pd.maxparacount)) then
                 proc_add(pd);
             end;
          end;

        { remember if the procedure is declared with the overload directive,
          it's information is still needed also after all procs are removed }
        has_overload_directive:=(po_overload in sym.first_procdef.procoptions);

        { when the definition has overload directive set, we search for
          overloaded definitions in the symtablestack. The found
          entries are only added to the procs list and not the procsym, because
          the list can change in every situation }
        if has_overload_directive and
           (sym.owner.symtabletype<>objectsymtable) then
          begin
            srsymtable:=sym.owner.next;
            while assigned(srsymtable) do
             begin
               if srsymtable.symtabletype in [localsymtable,staticsymtable,globalsymtable] then
                begin
                  srprocsym:=tprocsym(srsymtable.speedsearch(sym.name,sym.speedvalue));
                  { process only visible procsyms }
                  if assigned(srprocsym) and
                     (srprocsym.typ=procsym) and
                     srprocsym.is_visible_for_object(topclassh) then
                   begin
                     { if this procedure doesn't have overload we can stop
                       searching }
                     if not(po_overload in srprocsym.first_procdef.procoptions) then
                      break;
                     { process all overloaded definitions }
                     for j:=1 to srprocsym.procdef_count do
                      begin
                        pd:=srprocsym.procdef[j];
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
                              if compare_paras(hp^.data.para,pd.para,cp_value_equal_const,[cpo_ignorehidden])>=te_equal then
                               begin
                                 found:=true;
                                 break;
                               end;
                              hp:=hp^.next;
                            end;
                           if not found then
                             proc_add(pd);
                         end;
                      end;
                   end;
                end;
               srsymtable:=srsymtable.next;
             end;
          end;
      end;


    constructor tcallcandidates.create_operator(op:ttoken;ppn:tnode);
      var
        j          : integer;
        pd         : tprocdef;
        hp         : pcandidate;
        found      : boolean;
        srsymtable : tsymtable;
        srprocsym  : tprocsym;
        pt         : tcallparanode;
        sv         : cardinal;
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
           if pt.resulttype.def.deftype=variantdef then
             FAllowVariant:=true;
           inc(FParalength);
           pt:=tcallparanode(pt.right);
         end;

        { we search all overloaded operator definitions in the symtablestack. The found
          entries are only added to the procs list and not the procsym, because
          the list can change in every situation }
        sv:=getspeedvalue(overloaded_names[op]);
        srsymtable:=symtablestack;
        while assigned(srsymtable) do
          begin
            if srsymtable.symtabletype in [localsymtable,staticsymtable,globalsymtable] then
              begin
                srprocsym:=tprocsym(srsymtable.speedsearch(overloaded_names[op],sv));
                if assigned(srprocsym) and
                   (srprocsym.typ=procsym) then
                  begin
                    { Store first procsym found }
                    if not assigned(FProcsym) then
                      FProcsym:=srprocsym;

                    { process all overloaded definitions }
                    for j:=1 to srprocsym.procdef_count do
                      begin
                        pd:=srprocsym.procdef[j];
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
                                if compare_paras(hp^.data.para,pd.para,cp_value_equal_const,[cpo_ignorehidden])>=te_equal then
                                  begin
                                    found:=true;
                                    break;
                                  end;
                                hp:=hp^.next;
                              end;
                            if not found then
                              proc_add(pd);
                          end;
                      end;
                  end;
              end;
            srsymtable:=srsymtable.next;
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


    function tcallcandidates.proc_add(pd:tprocdef):pcandidate;
      var
        i : integer;
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
        result^.firstpara:=tparaitem(pd.Para.last);
        if not(po_varargs in pd.procoptions) then
         begin
           { ignore hidden parameters }
           while assigned(result^.firstpara) and (result^.firstpara.is_hidden) do
             result^.firstpara:=tparaitem(result^.firstpara.previous);
           for i:=1 to pd.maxparacount-FParalength do
             begin
               if not assigned(result^.firstpara) then
                 internalerror(200401141);
               result^.firstpara:=tparaitem(result^.firstPara.previous);
             end;
         end;
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
              result:=result+',';
             result:=result+p.resulttype.def.typename;
             p:=tcallparanode(p.right);
           end;
        end;

      var
        hp : pcandidate;
        currpara : tparaitem;
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
                          ' oper: '+tostr(hp^.coper_count)+
                          ' ord: '+realtostr(hp^.exact_count));
              { Print parameters in left-right order }
              currpara:=hp^.firstpara;
              if assigned(currpara) then
               begin
                 while assigned(currpara.next) do
                  currpara:=tparaitem(currpara.next);
               end;
              while assigned(currpara) do
               begin
                 if (not currpara.is_hidden) then
                   Comment(lvl,'    - '+currpara.paratype.def.typename+' : '+EqualTypeName[currpara.eqval]);
                 currpara:=tparaitem(currpara.previous);
               end;
            end;
           hp:=hp^.next;
         end;
      end;
{$endif EXTDEBUG}


    procedure tcallcandidates.get_information;
      var
        hp       : pcandidate;
        currpara : tparaitem;
        currparanr : byte;
        def_from,
        def_to   : tdef;
        currpt,
        pt       : tcallparanode;
        eq       : tequaltype;
        convtype : tconverttype;
        pdoper   : tprocdef;
        releasecurrpt : boolean;
        cdoptions : tcompare_defs_options;
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
           currpara:=hp^.firstpara;
           while assigned(currpara) and (currpara.is_hidden) do
             currpara:=tparaitem(currpara.previous);
           pt:=tcallparanode(FParaNode);
           while assigned(pt) and assigned(currpara) do
            begin
              { currpt can be changed from loadn to calln when a procvar
                is passed. This is to prevent that the change is permanent }
              currpt:=pt;
              releasecurrpt:=false;
              { retrieve current parameter definitions to compares }
              eq:=te_incompatible;
              def_from:=currpt.resulttype.def;
              def_to:=currpara.paratype.def;
              if not(assigned(def_from)) then
               internalerror(200212091);
              if not(
                     assigned(def_to) or
                     ((po_varargs in hp^.data.procoptions) and
                      (currparanr>hp^.data.minparacount))
                    ) then
               internalerror(200212092);

              { Convert tp procvars when not expecting a procvar }
              if (def_to.deftype<>procvardef) and
                 (currpt.left.resulttype.def.deftype=procvardef) then
                begin
                  releasecurrpt:=true;
                  currpt:=tcallparanode(pt.getcopy);
                  if maybe_call_procvar(currpt.left,true) then
                    begin
                      currpt.resulttype:=currpt.left.resulttype;
                      def_from:=currpt.left.resulttype.def;
                    end;
                end;

              { varargs are always equal, but not exact }
              if (po_varargs in hp^.data.procoptions) and
                 (currparanr>hp^.data.minparacount) then
               begin
                 eq:=te_equal;
               end
              else
              { same definition -> exact }
               if (def_from=def_to) then
                begin
                  eq:=te_exact;
                end
              else
              { for value and const parameters check if a integer is constant or
                included in other integer -> equal and calc ordinal_distance }
               if not(currpara.paratyp in [vs_var,vs_out]) and
                  is_integer(def_from) and
                  is_integer(def_to) and
                  is_in_limit(def_from,def_to) then
                 begin
                   eq:=te_equal;
                   hp^.ordinal_distance:=hp^.ordinal_distance+
                     abs(bestreal(torddef(def_from).low)-bestreal(torddef(def_to).low));
                   hp^.ordinal_distance:=hp^.ordinal_distance+
                     abs(bestreal(torddef(def_to).high)-bestreal(torddef(def_from).high));
                   { Give wrong sign a small penalty, this is need to get a diffrence
                     from word->[longword,longint] }
                   if is_signed(def_from)<>is_signed(def_to) then
                     hp^.ordinal_distance:=hp^.ordinal_distance+1.0;
                 end
              else
              { generic type comparision }
               begin
                 eq:=compare_defs_ext(def_from,def_to,currpt.left.nodetype,convtype,pdoper,cdoptions);

                 { when the types are not equal we need to check
                   some special case for parameter passing }
                 if (eq<te_equal) then
                  begin
                    if currpara.paratyp in [vs_var,vs_out] then
                      begin
                        { para requires an equal type so the previous found
                          match was not good enough, reset to incompatible }
                        eq:=te_incompatible;
                        { var_para_allowed will return te_equal and te_convert_l1 to
                          make a difference for best matching }
                        var_para_allowed(eq,currpt.resulttype.def,currpara.paratype.def)
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
                 hp^.wrongpara:=currpara;
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
                   currpara:=tparaitem(currpara.previous);
                 until (not assigned(currpara)) or (not currpara.is_hidden);
               end;
              dec(currparanr);
            end;
           if not(hp^.invalid) and
              (assigned(pt) or assigned(currpara) or (currparanr<>0)) then
             internalerror(200212141);
           { next candidate }
           hp:=hp^.next;
         end;
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
        is_better_candidate:=res;
      end;


    function tcallcandidates.choose_best(var bestpd:tabstractprocdef):integer;
      var
        besthpstart,
        hp       : pcandidate;
        cntpd,
        res      : integer;
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
              res:=is_better_candidate(hp,besthpstart);
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
        if hp^.wrongpara.paratyp in [vs_var,vs_out] then
          CGMessagePos2(pt.fileinfo,parser_e_call_by_ref_without_typeconv,
            pt.resulttype.def.typename,hp^.wrongpara.paratype.def.typename)
        else
          CGMessagePos3(pt.fileinfo,type_e_wrong_parameter_type,
            tostr(hp^.wrongparanr),pt.resulttype.def.typename,hp^.wrongpara.paratype.def.typename);
      end;


end.
{
  $Log$
  Revision 1.83  2004-03-18 16:19:03  peter
    * fixed operator overload allowing for pointer-string
    * replaced some type_e_mismatch with more informational messages

  Revision 1.82  2004/02/26 16:11:09  peter
    * return cnothingn and give error when the operator is not overloaded

  Revision 1.81  2004/02/24 16:12:39  peter
    * operator overload chooses rewrite
    * overload choosing is now generic and moved to htypechk

  Revision 1.80  2004/02/20 21:55:19  peter
    * widestring conversions added to allowed operator check

  Revision 1.79  2004/02/13 15:42:21  peter
    * compare_defs_ext has now a options argument
    * fixes for variants

  Revision 1.78  2004/02/12 15:54:03  peter
    * make extcycle is working again

  Revision 1.77  2004/02/04 22:15:15  daniel
    * Rtti generation moved to ncgutil
    * Assmtai usage of symsym removed
    * operator overloading cleanup up

  Revision 1.76  2004/02/03 22:32:53  peter
    * renamed xNNbittype to xNNinttype
    * renamed registers32 to registersint
    * replace some s32bit,u32bit with torddef([su]inttype).def.typ

  Revision 1.75  2003/11/12 15:48:27  peter
    * fix set_varstate in for loops
    * fix set_varstate from case statements

  Revision 1.74  2003/10/30 19:20:05  peter
    * fix IE when passing array to open array

  Revision 1.73  2003/10/30 17:42:48  peter
    * also check for uninited vars in staticsymtable

  Revision 1.72  2003/10/28 15:36:01  peter
    * absolute to object field supported, fixes tb0458

  Revision 1.71  2003/10/21 18:16:13  peter
    * IncompatibleTypes() added that will include unit names when
      the typenames are the same

  Revision 1.70  2003/10/20 19:29:12  peter
    * fix check for typecasting wrong sizes in assignment left

  Revision 1.69  2003/10/08 19:19:45  peter
    * set_varstate cleanup

  Revision 1.68  2003/10/05 21:21:52  peter
    * c style array of const generates callparanodes
    * varargs paraloc fixes

  Revision 1.67  2003/10/01 20:34:48  peter
    * procinfo unit contains tprocinfo
    * cginfo renamed to cgbase
    * moved cgmessage to verbose
    * fixed ppc and sparc compiles

  Revision 1.66  2003/08/23 18:52:18  peter
    * don't check size for open array in valid_for_assign

  Revision 1.65  2003/07/08 15:20:56  peter
    * don't allow add/assignments for formaldef
    * formaldef size changed to 0

  Revision 1.64  2003/06/13 21:19:30  peter
    * current_procdef removed, use current_procinfo.procdef instead

  Revision 1.63  2003/05/09 17:47:02  peter
    * self moved to hidden parameter
    * removed hdisposen,hnewn,selfn

  Revision 1.62  2003/04/27 11:21:32  peter
    * aktprocdef renamed to current_procinfo.procdef
    * procinfo renamed to current_procinfo
    * procinfo will now be stored in current_module so it can be
      cleaned up properly
    * gen_main_procsym changed to create_main_proc and release_main_proc
      to also generate a tprocinfo structure
    * fixed unit implicit initfinal

  Revision 1.61  2003/04/27 07:29:50  peter
    * current_procinfo.procdef cleanup, current_procdef is now always nil when parsing
      a new procdef declaration
    * aktprocsym removed
    * lexlevel removed, use symtable.symtablelevel instead
    * implicit init/final code uses the normal genentry/genexit
    * funcret state checking updated for new funcret handling

  Revision 1.60  2003/04/25 20:59:33  peter
    * removed funcretn,funcretsym, function result is now in varsym
      and aliases for result and function name are added using absolutesym
    * vs_hidden parameter for funcret passed in parameter
    * vs_hidden fixes
    * writenode changed to printnode and released from extdebug
    * -vp option added to generate a tree.log with the nodetree
    * nicer printnode for statements, callnode

  Revision 1.59  2003/04/22 23:50:22  peter
    * firstpass uses expectloc
    * checks if there are differences between the expectloc and
      location.loc from secondpass in EXTDEBUG

  Revision 1.58  2003/01/03 17:17:26  peter
    * use compare_def_ext to test if assignn operator is allowed

  Revision 1.57  2003/01/02 22:21:19  peter
    * fixed previous operator change

  Revision 1.56  2003/01/02 19:50:21  peter
    * fixed operator checking for objects
    * made binary operator checking simpeler

  Revision 1.55  2002/12/27 18:06:32  peter
    * fix overload error for dynarr:=nil

  Revision 1.54  2002/12/22 16:34:49  peter
    * proc-procvar crash fixed (tw2277)

  Revision 1.53  2002/12/11 22:39:24  peter
    * better error message when no operator is found for equal

  Revision 1.52  2002/11/27 22:11:59  peter
    * rewrote isbinaryoverloadable to use a case. it's now much easier
      to understand what is happening

  Revision 1.51  2002/11/25 17:43:17  peter
    * splitted defbase in defutil,symutil,defcmp
    * merged isconvertable and is_equal into compare_defs(_ext)
    * made operator search faster by walking the list only once

  Revision 1.50  2002/10/07 20:12:08  peter
    * ugly hack to fix tb0411

  Revision 1.49  2002/10/05 00:47:03  peter
    * support dynamicarray<>nil

  Revision 1.48  2002/10/04 21:13:59  peter
    * ignore vecn,subscriptn when checking for a procvar loadn

  Revision 1.47  2002/09/16 18:09:34  peter
    * set_funcret_valid fixed when result was already used in a nested
      procedure

  Revision 1.46  2002/07/20 11:57:53  florian
    * types.pas renamed to defbase.pas because D6 contains a types
      unit so this would conflicts if D6 programms are compiled
    + Willamette/SSE2 instructions to assembler added

  Revision 1.45  2002/05/18 13:34:08  peter
    * readded missing revisions

  Revision 1.44  2002/05/16 19:46:37  carl
  + defines.inc -> fpcdefs.inc to avoid conflicts if compiling by hand
  + try to fix temp allocation (still in ifdef)
  + generic constructor calls
  + start of tassembler / tmodulebase class cleanup

  Revision 1.42  2002/04/02 17:11:28  peter
    * tlocation,treference update
    * LOC_CONSTANT added for better constant handling
    * secondadd splitted in multiple routines
    * location_force_reg added for loading a location to a register
      of a specified size
    * secondassignment parses now first the right and then the left node
      (this is compatible with Kylix). This saves a lot of push/pop especially
      with string operations
    * adapted some routines to use the new cg methods

  Revision 1.41  2002/01/16 09:33:46  jonas
    * no longer allow assignments to pointer expressions (unless there's a
      deref), reported by John Lee

}
