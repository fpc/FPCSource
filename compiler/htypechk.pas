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
      tokens,
      node,
      symtype,symdef;

    type
      Ttok2nodeRec=record
        tok : ttoken;
        nod : tnodetype;
        op_overloading_supported : boolean;
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
       get_para_resulttype : boolean = false;
       allow_array_constructor : boolean = false;

    { is overloading of this operator allowed for this
      binary operator }
    function isbinaryoperatoroverloadable(treetyp:tnodetype;ld:tdef;lt:tnodetype;rd:tdef;rt:tnodetype) : boolean;

    { is overloading of this operator allowed for this
      unary operator }
    function isunaryoperatoroverloadable(rd,dd : tdef; treetyp : tnodetype) : boolean;

    { check operator args and result type }
    function isoperatoracceptable(pf : tprocdef; optoken : ttoken) : boolean;
    function isbinaryoverloaded(var t : tnode) : boolean;

    { Register Allocation }
    procedure make_not_regable(p : tnode);
    procedure calcregisters(p : tbinarynode;r32,fpu,mmx : word);

    { subroutine handling }
    function  is_procsym_load(p:tnode):boolean;
    procedure test_local_to_procvar(from_def:tprocvardef;to_def:tdef);

    {
    type
    tvarstaterequire = (vsr_can_be_undefined,vsr_must_be_valid,
      vsr_is_used_after,vsr_must_be_valid_and_is_used_after); }

    { sets varsym varstate field correctly }
    procedure unset_varstate(p : tnode);
    procedure set_varstate(p : tnode;must_be_valid : boolean);

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
       symconst,symsym,symtable,
       defutil,defcmp,cpubase,
       ncnv,nld,
       nmem,ncal,nmat,
       cginfo,cgbase
       ;

    type
      TValidAssign=(Valid_Property,Valid_Void);
      TValidAssigns=set of TValidAssign;


    function isbinaryoperatoroverloadable(treetyp:tnodetype;ld:tdef;lt:tnodetype;rd:tdef;rt:tnodetype) : boolean;

        function internal_check(treetyp:tnodetype;ld:tdef;lt:tnodetype;rd:tdef;rt:tnodetype;var allowed:boolean):boolean;
        begin
          internal_check:=true;
          case ld.deftype of
            recorddef,
            variantdef :
              begin
                allowed:=true;
              end;
            procvardef :
              begin
                if (rd.deftype in [pointerdef,procdef,procvardef]) and
                   (treetyp in [equaln,unequaln]) then
                 begin
                   allowed:=false;
                   exit;
                 end;
                allowed:=true;
              end;
            pointerdef :
              begin
                if ((rd.deftype in [pointerdef,classrefdef,procvardef]) or
                    is_class_or_interface(rd)) and
                   (treetyp in [equaln,unequaln,gtn,gten,ltn,lten,addn,subn]) then
                 begin
                   allowed:=false;
                   exit;
                 end;

                { don't allow operations on pointer/integer }
                if is_integer(rd) then
                 begin
                   allowed:=false;
                   exit;
                 end;

                { don't allow pchar+string }
                if is_pchar(ld) and
                   (treetyp in [addn,equaln,unequaln,gtn,gten,ltn,lten]) and
                   (is_chararray(rd) or
                    is_char(rd) or
                    (rd.deftype=stringdef)) then
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
                { not chararray+[char,string,chararray] }
                if is_chararray(ld) and
                   (treetyp in [addn,equaln,unequaln,gtn,gten,ltn,lten]) and
                   (is_char(rd) or
                    is_pchar(rd) or
                    is_integer(rd) or
                    (rd.deftype=stringdef) or
                    is_chararray(rd) or
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
                    is_pchar(rd) or
                    is_chararray(rd)) and
                   (treetyp in [addn,equaln,unequaln,gtn,gten,ltn,lten]) then
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


    function isunaryoperatoroverloadable(rd,dd : tdef; treetyp : tnodetype) : boolean;
      var
        eq : tequaltype;
        conv : tconverttype;
        pd : tprocdef;
      begin
        isunaryoperatoroverloadable:=false;
        case treetyp of
          assignn :
            begin
              eq:=compare_defs_ext(rd,dd,nothingn,true,false,conv,pd);
              if eq<>te_incompatible then
               begin
                 isunaryoperatoroverloadable:=false;
                 exit;
               end;
              isunaryoperatoroverloadable:=true;
            end;

          subn :
            begin
              if is_integer(rd) or
                 (rd.deftype=floatdef) then
               begin
                 isunaryoperatoroverloadable:=false;
                 exit;
               end;

{$ifdef SUPPORT_MMX}
              if (cs_mmx in aktlocalswitches) and
                 is_mmx_able_array(rd) then
               begin
                 isunaryoperatoroverloadable:=false;
                 exit;
               end;
{$endif SUPPORT_MMX}
              isunaryoperatoroverloadable:=true;
            end;

          notn :
            begin
              if is_integer(rd) or
                 is_boolean(rd) then
               begin
                 isunaryoperatoroverloadable:=false;
                 exit;
               end;

{$ifdef SUPPORT_MMX}
              if (cs_mmx in aktlocalswitches) and
                 is_mmx_able_array(rd) then
               begin
                 isunaryoperatoroverloadable:=false;
                 exit;
               end;
{$endif SUPPORT_MMX}
              isunaryoperatoroverloadable:=true;
            end;
        end;
      end;


    function isoperatoracceptable(pf : tprocdef; optoken : ttoken) : boolean;
      var
        ld,rd,dd : tdef;
        i : longint;
      begin
        case pf.parast.symindex.count of
          2 : begin
                isoperatoracceptable:=false;
                for i:=1 to tok2nodes do
                  if tok2node[i].tok=optoken then
                    begin
                      ld:=tvarsym(pf.parast.symindex.first).vartype.def;
                      rd:=tvarsym(pf.parast.symindex.first.indexnext).vartype.def;
                      dd:=pf.rettype.def;
                      isoperatoracceptable:=
                        tok2node[i].op_overloading_supported and
                        isbinaryoperatoroverloadable(tok2node[i].nod,ld,nothingn,rd,nothingn);
                      break;
                    end;
              end;
          1 : begin
                rd:=tvarsym(pf.parast.symindex.first).vartype.def;
                dd:=pf.rettype.def;
                for i:=1 to tok2nodes do
                  if tok2node[i].tok=optoken then
                    begin
                      isoperatoracceptable:=
                        tok2node[i].op_overloading_supported and
                        isunaryoperatoroverloadable(rd,dd,tok2node[i].nod);
                      break;
                    end;
              end;
          else
            isoperatoracceptable:=false;
          end;
      end;


    function isbinaryoverloaded(var t : tnode) : boolean;

     var
         rd,ld   : tdef;
         optoken : ttoken;
         operpd  : tprocdef;
         ht      : tnode;
      begin
        isbinaryoverloaded:=false;
        operpd:=nil;
        { load easier access variables }
        ld:=tbinarynode(t).left.resulttype.def;
        rd:=tbinarynode(t).right.resulttype.def;
        if isbinaryoperatoroverloadable(t.nodetype,ld,tbinarynode(t).left.nodetype,rd,tbinarynode(t).right.nodetype) then
          begin
             isbinaryoverloaded:=true;
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
                  optoken:=tokens._lt;
                gtn:
                  optoken:=tokens._gt;
                lten:
                  optoken:=_lte;
                gten:
                  optoken:=_gte;
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
                  exit;
             end;
             { check if the operator contains overloaded procdefs }
             if overloaded_operators[optoken]=nil then
               begin
                  CGMessage(parser_e_operator_not_overloaded);
                  isbinaryoverloaded:=false;
                  exit;
               end;

             { Check if the assignment is available, if not then
               give a message that the types are not compatible }
             if optoken in [_EQUAL] then
              begin
                operpd:=overloaded_operators[optoken].search_procdef_binary_operator(ld,rd);
                if not assigned(operpd) then
                 begin
                   CGMessage2(type_e_incompatible_types,ld.typename,rd.typename);
                   isbinaryoverloaded:=false;
                   exit;
                 end;
               end;

             { the nil as symtable signs firstcalln that this is
               an overloaded operator }
             ht:=ccallnode.create(nil,overloaded_operators[optoken],nil,nil);
             inc(tcallnode(ht).symtableprocentry.refs);
             { we already know the procdef to use for equal, so it can
               skip the overload choosing in callnode.det_resulttype }
             if assigned(operpd) then
               tcallnode(ht).procdefinition:=operpd;
             { we need copies, because the originals will be destroyed when we give a }
             { changed node back to firstpass! (JM)                                   }
             if assigned(tbinarynode(t).left) then
               if assigned(tbinarynode(t).right) then
                 tcallnode(ht).left :=
                   ccallparanode.create(tbinarynode(t).right.getcopy,
                                        ccallparanode.create(tbinarynode(t).left.getcopy,nil))
               else
                 tcallnode(ht).left :=
                   ccallparanode.create(nil,
                                        ccallparanode.create(tbinarynode(t).left.getcopy,nil))
             else if assigned(tbinarynode(t).right) then
                 tcallnode(ht).left :=
                    ccallparanode.create(tbinarynode(t).right.getcopy,
                                         ccallparanode.create(nil,nil));
             if t.nodetype=unequaln then
               ht:=cnotnode.create(ht);
             t:=ht;
          end;
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

              if (abs(p.left.registers32-p.right.registers32)<r32) or
                 ((p.expectloc = LOC_FPUREGISTER) and
                  (p.right.registersfpu <= p.left.registersfpu) and
                  ((p.right.registersfpu <> 0) or (p.left.registersfpu <> 0)) and
                  (p.left.registers32   < p.right.registers32)) then
                inc(p.registers32,r32);
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
              if (p.left.registers32=p.right.registers32) and
                 (p.registers32=p.left.registers32) and
                 (p.registers32>0) and
                (p.left.expectloc in [LOC_REFERENCE,LOC_CREFERENCE]) and
                (p.right.expectloc in [LOC_REFERENCE,LOC_CREFERENCE]) then
                inc(p.registers32);
            end
           else
            begin
              if (p.left.registers32<r32) then
               inc(p.registers32,r32);
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


    procedure set_varstate(p : tnode;must_be_valid : boolean);
      var
        hsym : tvarsym;
      begin
        while assigned(p) do
         begin
           if (nf_varstateset in p.flags) then
            exit;
           include(p.flags,nf_varstateset);
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
                 set_varstate(tbinarynode(p).right,true);
                 if not(tunarynode(p).left.resulttype.def.deftype in [stringdef,arraydef]) then
                  must_be_valid:=true;
                 p:=tunarynode(p).left;
               end;
             { do not parse calln }
             calln :
               break;
             callparan :
               begin
                 set_varstate(tbinarynode(p).right,must_be_valid);
                 p:=tunarynode(p).left;
               end;
             loadn :
               begin
                 if (tloadnode(p).symtableentry.typ=varsym) then
                  begin
                    hsym:=tvarsym(tloadnode(p).symtableentry);
                    if must_be_valid and (nf_first_use in p.flags) then
                     begin
                       if (hsym.varstate=vs_declared_and_first_found) or
                          (hsym.varstate=vs_set_but_first_not_passed) then
                        begin
                          if (assigned(hsym.owner) and
                              assigned(aktprocdef) and
                              (hsym.owner=aktprocdef.localst)) then
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
                     end;
                    if (nf_first_use in p.flags) then
                     begin
                       if hsym.varstate=vs_declared_and_first_found then
                        begin
                          { this can only happen at left of an assignment, no ? PM }
                          if (parsing_para_level=0) and not must_be_valid then
                           hsym.varstate:=vs_assigned
                          else
                           hsym.varstate:=vs_used;
                        end
                       else
                        if hsym.varstate=vs_set_but_first_not_passed then
                         hsym.varstate:=vs_used;
                       exclude(p.flags,nf_first_use);
                     end
                    else
                      begin
                        if (hsym.varstate=vs_assigned) and
                           (must_be_valid or (parsing_para_level>0) or
                            (p.resulttype.def.deftype=procvardef)) then
                          hsym.varstate:=vs_used;
                        if (hsym.varstate=vs_declared_and_first_found) and
                           (must_be_valid or (parsing_para_level>0) or
                           (p.resulttype.def.deftype=procvardef)) then
                          hsym.varstate:=vs_set_but_first_not_passed;
                      end;
                  end;
                 break;
               end;
             else
               break;
           end;{case }
         end;
      end;


    procedure unset_varstate(p : tnode);
      begin
        while assigned(p) do
         begin
           exclude(p.flags,nf_varstateset);
           case p.nodetype of
             typeconvn,
             subscriptn,
             vecn :
               p:=tunarynode(p).left;
             else
               break;
           end;
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
                   - from formaldef
                   - from void
                   - typecast from pointer to array }
                 fromdef:=ttypeconvnode(hp).left.resulttype.def;
                 todef:=hp.resulttype.def;
                 if not((fromdef.deftype=formaldef) or
                        is_void(fromdef) or
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
               hp:=tunarynode(hp).left;
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
             selfn :
               begin
                 valid_for_assign:=true;
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
                          { set the assigned flag for varsyms }
                          if (tvarsym(tloadnode(hp).symtableentry).varstate=vs_declared) then
                           tvarsym(tloadnode(hp).symtableentry).varstate:=vs_assigned;
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

end.
{
  $Log$
  Revision 1.61  2003-04-27 07:29:50  peter
    * aktprocdef cleanup, aktprocdef is now always nil when parsing
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
