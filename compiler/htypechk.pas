{
    $Id: htypechk.pas,v 1.124 2005/04/25 08:59:07 peter Exp $
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
         next         : pcandidate;
         data         : tprocdef;
         wrongparaidx,
         firstparaidx : integer;
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
        constructor create(sym:tprocsym;st:tsymtable;ppn:tnode;isprop,ignorevis : boolean);
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

    function  valid_for_formal_var(p : tnode) : boolean;
    function  valid_for_formal_const(p : tnode) : boolean;
    function  valid_for_var(p:tnode):boolean;
    function  valid_for_assignment(p:tnode):boolean;
    function  valid_for_addr(p : tnode) : boolean;


implementation

    uses
       globtype,systems,
       cutils,verbose,globals,
       symtable,
       defutil,defcmp,
       nbas,ncnv,nld,nmem,ncal,nmat,ninl,nutils,
       cgbase,procinfo
       ;

    type
      TValidAssign=(Valid_Property,Valid_Void,Valid_Const,Valid_Addr);
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
                if ((rd.deftype in [orddef,enumdef,pointerdef,classrefdef,procvardef]) or
                    is_class_or_interface(rd)) then
                 begin
                   allowed:=false;
                   exit;
                 end;

                { don't allow pchar+string }
                if (is_pchar(ld) or is_pwidechar(ld)) and
                   ((rd.deftype=stringdef) or
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
                { not mmx }
                if (cs_mmx in aktlocalswitches) and
                   is_mmx_able_array(ld) then
                 begin
                   allowed:=false;
                   exit;
                 end;
                { not chararray+[(wide)char,(wide)string,(wide)chararray] }
                if (is_chararray(ld) or is_widechararray(ld) or
                    is_open_chararray(ld) or is_open_widechararray(ld))
                   and
                   ((rd.deftype in [stringdef,orddef,enumdef]) or
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
                if (rd.deftype in [orddef,enumdef,stringdef]) or
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
              if (ld.deftype in [orddef,enumdef,floatdef]) then
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
              if (ld.deftype in [orddef,enumdef,floatdef]) then
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
                ld:=tparavarsym(pf.parast.symindex.first).vartype.def;
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
                      ld:=tparavarsym(pf.parast.symindex.first).vartype.def;
                      rd:=tparavarsym(pf.parast.symindex.first.indexnext).vartype.def;
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

        inc(operpd.procsym.refs);

        { the nil as symtable signs firstcalln that this is
          an overloaded operator }
        t:=ccallnode.create(ppn,Tprocsym(operpd.procsym),nil,nil,[]);

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

        inc(operpd.procsym.refs);

        { the nil as symtable signs firstcalln that this is
          an overloaded operator }
        ht:=ccallnode.create(ppn,Tprocsym(operpd.procsym),nil,nil,[]);

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
              if tloadnode(p).symtableentry.typ in [globalvarsym,localvarsym,paravarsym] then
                tabstractvarsym(tloadnode(p).symtableentry).varregable:=vr_none;
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
      end;


{****************************************************************************
                          Subroutine Handling
****************************************************************************}

    function is_procvar_load(p:tnode):boolean;
      begin
        result:=false;
        { remove voidpointer typecast for tp procvars }
        if (m_tp_procvar in aktmodeswitches) and
           (p.nodetype=typeconvn) and
           is_voidpointer(p.resulttype.def) then
          p:=tunarynode(p).left;
        result:=(p.nodetype=typeconvn) and
                (ttypeconvnode(p).convtype=tc_proc_2_procvar);
      end;


    { local routines can't be assigned to procvars }
    procedure test_local_to_procvar(from_def:tprocvardef;to_def:tdef);
      begin
         if (from_def.parast.symtablelevel>normal_function_level) and
            (to_def.deftype=procvardef) then
           CGMessage(type_e_cannot_local_proc_to_procvar);
      end;


    procedure set_varstate(p:tnode;newstate:tvarstate;varstateflags:tvarstateflags);
      var
        hsym : tabstractvarsym;
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
                     exclude(varstateflags,vsf_must_be_valid);
                   tc_pchar_2_string,
                   tc_pointer_2_array :
                     include(varstateflags,vsf_must_be_valid);
                 end;
                 p:=tunarynode(p).left;
               end;
             subscriptn :
               p:=tunarynode(p).left;
             vecn:
               begin
                 set_varstate(tbinarynode(p).right,vs_used,[vsf_must_be_valid]);
                 if not(tunarynode(p).left.resulttype.def.deftype in [stringdef,arraydef]) then
                   include(varstateflags,vsf_must_be_valid);
                 p:=tunarynode(p).left;
               end;
             { do not parse calln }
             calln :
               break;
             loadn :
               begin
                 if (tloadnode(p).symtableentry.typ in [localvarsym,paravarsym,globalvarsym]) then
                  begin
                    hsym:=tabstractvarsym(tloadnode(p).symtableentry);
                    if (vsf_must_be_valid in varstateflags) and (hsym.varstate=vs_declared) then
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
                              begin
                                if tloadnode(p).symtable.symtabletype=localsymtable then
                                  begin
                                    if (vsf_use_hints in varstateflags) then
                                      CGMessage1(sym_h_uninitialized_local_variable,hsym.realname)
                                    else
                                      CGMessage1(sym_w_uninitialized_local_variable,hsym.realname);
                                  end
                                else
                                  begin
                                    if (vsf_use_hints in varstateflags) then
                                      CGMessage1(sym_h_uninitialized_variable,hsym.realname)
                                    else
                                      CGMessage1(sym_w_uninitialized_variable,hsym.realname);
                                  end;
                              end;
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
        gotstring,
        gotwith,
        gotsubscript,
        gotpointer,
        gotvec,
        gotclass,
        gotdynarray,
        gotderef : boolean;
        fromdef,
        todef    : tdef;
        errmsg   : longint;
      begin
        if valid_const in opts then
          errmsg:=type_e_variable_id_expected
        else
          errmsg:=type_e_argument_cant_be_assigned;
        result:=false;
        gotsubscript:=false;
        gotvec:=false;
        gotderef:=false;
        gotclass:=false;
        gotpointer:=false;
        gotwith:=false;
        gotdynarray:=false;
        gotstring:=false;
        hp:=p;
        if not(valid_void in opts) and
           is_void(hp.resulttype.def) then
         begin
           CGMessagePos(hp.fileinfo,errmsg);
           exit;
         end;
        while assigned(hp) do
         begin
           { property allowed? calln has a property check itself }
           if (nf_isproperty in hp.flags) then
            begin
              if (hp.nodetype<>calln) or
                 (valid_property in opts) then
               result:=true
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
                   stringdef :
                     gotstring:=true;
                 end;
                 { 1. if it returns a pointer and we've found a deref,
                   2. if it returns a class or record and a subscription or with is found
                   3. if the address is needed of a field (subscriptn) }
                 if (gotpointer and gotderef) or
                    (gotstring and gotvec) or
                    (
                     gotclass and
                     (gotsubscript or gotwith)
                    ) or
                    (
                      (gotvec and gotdynarray)
                    ) or
                    (
                     (Valid_Addr in opts) and
                     (hp.nodetype=subscriptn)
                    ) then
                   result:=true
                 else
                   CGMessagePos(hp.fileinfo,errmsg);
               end;
              exit;
            end;
           if (Valid_Const in opts) and is_constnode(hp) then
             begin
               result:=true;
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
                    { in TP it is allowed to typecast to smaller types. But the variable can't
                      be in a register }
                    if (m_tp7 in aktmodeswitches) or
                       (todef.size<fromdef.size) then
                      make_not_regable(hp)
                    else
                      CGMessagePos2(hp.fileinfo,type_e_typecast_wrong_size_for_assignment,tostr(fromdef.size),tostr(todef.size));
                  end;
                 { don't allow assignments to typeconvs that need special code }
                 if not(gotsubscript or gotvec or gotderef) and
                    not(ttypeconvnode(hp).assign_allowed) then
                   begin
                     CGMessagePos(hp.fileinfo,errmsg);
                     exit;
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
                 { accesses to dyn. arrays override read only access in delphi }
                 if (m_delphi in aktmodeswitches) and is_dynamic_array(tunarynode(hp).left.resulttype.def) then
                   gotdynarray:=true;
                 hp:=tunarynode(hp).left;
               end;
             asn :
               begin
                 { asn can't be assigned directly, it returns the value in a register instead
                   of reference. }
                 if not(gotsubscript or gotderef or gotvec) then
                   begin
                     CGMessagePos(hp.fileinfo,errmsg);
                     exit;
                   end;
                 hp:=tunarynode(hp).left;
               end;
             subscriptn :
               begin
                 gotsubscript:=true;
                 { loop counter? }
                 if not(Valid_Const in opts) and
                    (vo_is_loop_counter in tsubscriptnode(hp).vs.varoptions) then
                   CGMessage1(parser_e_illegal_assignment_to_count_var,tsubscriptnode(hp).vs.realname);
                 { a class/interface access is an implicit }
                 { dereferencing                           }
                 hp:=tsubscriptnode(hp).left;
                 if is_class_or_interface(hp.resulttype.def) then
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
                 if ((hp.resulttype.def.deftype=pointerdef) or
                     (is_integer(hp.resulttype.def) and gotpointer)) and
                    gotderef then
                  result:=true
                 else
                 { Temp strings are stored in memory, for compatibility with
                   delphi only }
                   if (m_delphi in aktmodeswitches) and
                      (valid_addr in opts) and
                      (hp.resulttype.def.deftype=stringdef) then
                     result:=true
                 else
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
                  CGMessagePos(hp.fileinfo,type_e_no_assign_to_addr);
                 exit;
               end;
             addrn :
               begin
                 if gotderef then
                  result:=true
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
                   stringdef :
                     gotstring:=true;
                 end;
                 { 1. if it returns a pointer and we've found a deref,
                   2. if it returns a class or record and a subscription or with is found
                   3. string is returned }
                 if (gotstring and gotvec) or
                    (gotpointer and gotderef) or
                    (gotclass and (gotsubscript or gotwith)) then
                  result:=true
                 else
                 { Temp strings are stored in memory, for compatibility with
                   delphi only }
                   if (m_delphi in aktmodeswitches) and
                      (valid_addr in opts) and
                      (hp.resulttype.def.deftype=stringdef) then
                     result:=true
                 else
                  CGMessagePos(hp.fileinfo,errmsg);
                 exit;
               end;
             inlinen :
               begin
                 if (valid_const in opts) and
                    (tinlinenode(hp).inlinenumber in [in_typeof_x]) then
                   result:=true
                 else
                   CGMessagePos(hp.fileinfo,type_e_variable_id_expected);
                 exit;
               end;
             loadn :
               begin
                 case tloadnode(hp).symtableentry.typ of
                   absolutevarsym,
                   globalvarsym,
                   localvarsym,
                   paravarsym :
                     begin
                       { loop counter? }
                       if not(Valid_Const in opts) and
                          (vo_is_loop_counter in tabstractvarsym(tloadnode(hp).symtableentry).varoptions) then
                         CGMessage1(parser_e_illegal_assignment_to_count_var,tloadnode(hp).symtableentry.realname);
                       { derefed pointer }
                       if (tabstractvarsym(tloadnode(hp).symtableentry).varspez=vs_const) then
                        begin
                          { allow p^:= constructions with p is const parameter }
                          if gotderef or gotdynarray or (Valid_Const in opts) then
                           result:=true
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
                          result:=true;
                          exit;
                        end;
                     end;
                   typedconstsym :
                     begin
                       if ttypedconstsym(tloadnode(hp).symtableentry).is_writable or
                          (valid_addr in opts) then
                        result:=true
                       else
                        CGMessagePos(hp.fileinfo,type_e_no_assign_to_const);
                       exit;
                     end;
                   procsym :
                     begin
                       if (Valid_Const in opts) then
                         result:=true
                       else
                         CGMessagePos(hp.fileinfo,type_e_variable_id_expected);
                       exit;
                     end;
                   labelsym :
                     begin
                       if (Valid_Addr in opts) then
                         result:=true
                       else
                         CGMessagePos(hp.fileinfo,type_e_variable_id_expected);
                       exit;
                     end;
                   constsym:
                     begin
                       if (tconstsym(tloadnode(hp).symtableentry).consttyp=constresourcestring) and
                         (valid_addr in opts) then
                         result:=true
                       else
                         CGMessagePos(hp.fileinfo,type_e_variable_id_expected);
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
      begin
        valid_for_formal_const:=(p.resulttype.def.deftype=formaldef) or
          valid_for_assign(p,[valid_void,valid_const]);
      end;


    function  valid_for_assignment(p:tnode):boolean;
      begin
        valid_for_assignment:=valid_for_assign(p,[valid_property]);
      end;


    function  valid_for_addr(p : tnode) : boolean;
      begin
        result:=valid_for_assign(p,[valid_const,valid_addr,valid_void]);
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

    constructor tcallcandidates.create(sym:tprocsym;st:tsymtable;ppn:tnode;isprop,ignorevis : boolean);
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
           (sym.owner.symtabletype=objectsymtable) and
           (po_overload in sym.first_procdef.procoptions) then
         search_class_overloads(sym);

        { when the class passed is defined in this unit we
          need to use the scope of that class. This is a trick
          that can be used to access protected members in other
          units. At least kylix supports it this way (PFV) }
        if assigned(st) and
           (
            (st.symtabletype=objectsymtable) or
            ((st.symtabletype=withsymtable) and
             (st.defowner.deftype=objectdef))
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
        for j:=1 to sym.procdef_count do
          begin
            pd:=sym.procdef[j];
            { Is the procdef visible? This needs to be checked on
              procdef level since a symbol can contain both private and
              public declarations. But the check should not be done
              when the callnode is generated by a property

              inherited overrides invisible anonymous inherited (FK) }

            if isprop or ignorevis or
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
                  if assigned(srprocsym) and
                     (srprocsym.typ=procsym) then
                   begin
                     { if this visible procedure doesn't have overload we can stop
                       searching }
                     if not(po_overload in srprocsym.first_procdef.procoptions) and
                        srprocsym.first_procdef.is_visible_for_object(topclassh) then
                      break;
                     { process all overloaded definitions }
                     for j:=1 to srprocsym.procdef_count do
                      begin
                        pd:=srprocsym.procdef[j];
                        { only visible procedures need to be added }
                        if pd.is_visible_for_object(topclassh) then
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
                                 proc_add(pd);
                             end;
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
                                if compare_paras(hp^.data.paras,pd.paras,cp_value_equal_const,[cpo_ignorehidden])>=te_equal then
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
                          ' oper: '+tostr(hp^.coper_count)+
                          ' ord: '+realtostr(hp^.ordinal_distance));
              { Print parameters in left-right order }
              for i:=0 to hp^.data.paras.count-1 do
               begin
                 currpara:=tparavarsym(hp^.data.paras[i]);
                 if (vo_is_hidden_para in currpara.varoptions) then
                   Comment(lvl,'    - '+currpara.vartype.def.typename+' : '+EqualTypeName[currpara.eqval]);
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
        rfh,rth  : bestreal;
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
              def_from:=currpt.resulttype.def;
              def_to:=currpara.vartype.def;
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
               if not(currpara.varspez in [vs_var,vs_out]) and
                  is_integer(def_from) and
                  is_integer(def_to) and
                  is_in_limit(def_from,def_to) then
                 begin
                   eq:=te_equal;
                   hp^.ordinal_distance:=hp^.ordinal_distance+
                     abs(bestreal(torddef(def_from).low)-bestreal(torddef(def_to).low));
                   if (torddef(def_to).typ=u64bit) then
                     rth:=bestreal(qword(torddef(def_to).high))
                   else
                     rth:=bestreal(torddef(def_to).high);
                   if (torddef(def_from).typ=u64bit) then
                     rfh:=bestreal(qword(torddef(def_from).high))
                   else
                     rfh:=bestreal(torddef(def_from).high);
                   hp^.ordinal_distance:=hp^.ordinal_distance+abs(rth-rfh);
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
                    if currpara.varspez in [vs_var,vs_out] then
                      begin
                        { para requires an equal type so the previous found
                          match was not good enough, reset to incompatible }
                        eq:=te_incompatible;
                        { var_para_allowed will return te_equal and te_convert_l1 to
                          make a difference for best matching }
                        var_para_allowed(eq,currpt.resulttype.def,currpara.vartype.def)
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
            if (compare_defs(pt.resulttype.def,wrongpara.vartype.def,pt.nodetype)<>te_incompatible) and
               not valid_for_var(pt.left) then
              CGMessagePos(pt.left.fileinfo,type_e_variable_id_expected)
            else
              CGMessagePos2(pt.left.fileinfo,parser_e_call_by_ref_without_typeconv,
                FullTypeName(pt.left.resulttype.def,wrongpara.vartype.def),
                FullTypeName(wrongpara.vartype.def,pt.left.resulttype.def))
          end
        else
          CGMessagePos3(pt.left.fileinfo,type_e_wrong_parameter_type,tostr(hp^.wrongparanr),
            FullTypeName(pt.left.resulttype.def,wrongpara.vartype.def),
            FullTypeName(wrongpara.vartype.def,pt.left.resulttype.def));
      end;


end.
{
  $Log: htypechk.pas,v $
  Revision 1.124  2005/04/25 08:59:07  peter
  allow gettting the address of read-only typedconsts

  Revision 1.123  2005/04/08 15:18:32  peter
  support string[index] for const/var assignment

  Revision 1.122  2005/04/01 07:12:29  marco
   * from peter for bug 3862

  Revision 1.121  2005/03/28 15:04:40  peter
  valid_property is only used for calln. Fields are always allowed

  Revision 1.120  2005/03/25 22:20:18  peter
    * add hint when passing an uninitialized variable to a var parameter

  Revision 1.119  2005/03/10 00:15:20  peter
  don't allow overloading orddef,enumdef.floatdef for unary operators

  Revision 1.118  2005/02/20 13:12:22  peter
    * allow assignment to elements of constant dyn array in delphi mode

  Revision 1.117  2005/02/14 17:13:06  peter
    * truncate log

  Revision 1.116  2005/02/14 16:45:00  peter
    * allow more operations on integers with a typecast to pointer

  Revision 1.115  2005/02/13 20:33:57  peter
    * allow nil^ passed to var parameter

  Revision 1.114  2005/02/02 22:16:39  florian
    * delphi assumes dyn. array access make expressions l-values because it's internally a pointer

  Revision 1.113  2005/02/01 22:50:50  florian
    * inherited; works now in delphi mode for private methods; looks like a delphi bug

  Revision 1.112  2005/01/25 18:49:45  peter
    * fix overload choosing with an qword overload
    * allow to get the address of string temps in delphi mode

  Revision 1.111  2005/01/19 23:23:12  florian
    * taking the address of a resourcestring is allowed now

  Revision 1.110  2005/01/19 22:19:41  peter
    * unit mapping rewrite
    * new derefmap added

  Revision 1.109  2005/01/19 20:53:27  florian
    * tmypointer(12435)^ is an l-value

  Revision 1.108  2005/01/10 22:10:26  peter
    * widestring patches from Alexey Barkovoy

  Revision 1.107  2005/01/07 16:22:47  peter
    * handle string-open array of (wide)char without variants

}
