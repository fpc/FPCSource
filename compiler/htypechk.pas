{
    $Id$
    Copyright (c) 1998-2000 by Florian Klaempfl

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

{$i defines.inc}

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
    function isbinaryoperatoroverloadable(ld, rd,dd : tdef;
             treetyp : tnodetype) : boolean;

    { is overloading of this operator allowed for this
      unary operator }
    function isunaryoperatoroverloadable(rd,dd : tdef;
             treetyp : tnodetype) : boolean;

    { check operator args and result type }
    function isoperatoracceptable(pf : tprocdef; optoken : ttoken) : boolean;
    function isbinaryoverloaded(var t : tnode) : boolean;

    { Register Allocation }
    procedure make_not_regable(p : tnode);
    procedure calcregisters(p : tbinarynode;r32,fpu,mmx : word);

    { subroutine handling }
    function  is_procsym_load(p:tnode):boolean;
    function  is_procsym_call(p:tnode):boolean;
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

    { sets funcret_is_valid to true, if p contains a funcref node }
    procedure set_funcret_is_valid(p : tnode);

    function  valid_for_formal_var(p : tnode) : boolean;
    function  valid_for_formal_const(p : tnode) : boolean;
    function  valid_for_var(p:tnode):boolean;
    function  valid_for_assignment(p:tnode):boolean;


implementation

    uses
       globtype,systems,
       cutils,verbose,globals,
       symconst,symsym,symtable,
       types,cpubase,
       ncnv,nld,
       nmem,ncal,nmat,
       cgbase
       ;

    type
      TValidAssign=(Valid_Property,Valid_Void);
      TValidAssigns=set of TValidAssign;


    { ld is the left type definition
      rd the right type definition
      dd the result type definition  or voiddef if unkown }
    function isbinaryoperatoroverloadable(ld, rd, dd : tdef;
             treetyp : tnodetype) : boolean;
      begin
        isbinaryoperatoroverloadable:=
           (treetyp=starstarn) or
           (ld.deftype=recorddef) or
           (rd.deftype=recorddef) or
           (ld.deftype=variantdef) or
           (rd.deftype=variantdef) or
           ((rd.deftype=pointerdef) and
            not(is_pchar(rd) and
                (is_chararray(ld) or
                 (ld.deftype=stringdef) or
                 (treetyp=addn))) and
            (not(ld.deftype in [pointerdef,objectdef,classrefdef,procvardef]) or
             not (treetyp in [equaln,unequaln,gtn,gten,ltn,lten,subn])
            ) and
            (not is_integer(ld) or not (treetyp in [addn,subn]))
           ) or
           ((ld.deftype=pointerdef) and
            not(is_pchar(ld) and
                (is_chararray(rd) or
                 (rd.deftype=stringdef) or
                 (treetyp=addn))) and
            (not(rd.deftype in [stringdef,pointerdef,objectdef,classrefdef,procvardef]) and
             ((not is_integer(rd) and (rd.deftype<>objectdef)
               and (rd.deftype<>classrefdef)) or
              not (treetyp in [equaln,unequaln,gtn,gten,ltn,lten,addn,subn])
             )
            )
           ) or
           { array def, but not mmx or chararray+[char,string,chararray] }
           ((ld.deftype=arraydef) and
            not((cs_mmx in aktlocalswitches) and
                is_mmx_able_array(ld)) and
            not(is_chararray(ld) and
                (is_char(rd) or
                 is_pchar(rd) or
                 { char array + int = pchar + int, fix for web bug 1377 (JM) }
                 is_integer(rd) or
                 (rd.deftype=stringdef) or
                 is_chararray(rd)))
           ) or
           ((rd.deftype=arraydef) and
            not((cs_mmx in aktlocalswitches) and
                is_mmx_able_array(rd)) and
            not(is_chararray(rd) and
                (is_char(ld) or
                 is_pchar(ld) or
                 (ld.deftype=stringdef) or
                 is_chararray(ld)))
           ) or
           { <> and = are defined for classes }
           (
            (ld.deftype=objectdef) and
            not((treetyp in [equaln,unequaln]) and is_class_or_interface(ld))
           ) or
           (
            (rd.deftype=objectdef) and
            not((treetyp in [equaln,unequaln]) and is_class_or_interface(rd))
           )
           or
           { allow other operators that + on strings }
           (
            (is_char(rd) or
             is_pchar(rd) or
             (rd.deftype=stringdef) or
             is_chararray(rd) or
             is_char(ld) or
             is_pchar(ld) or
             (ld.deftype=stringdef) or
             is_chararray(ld)
             ) and
             not(treetyp in [addn,equaln,unequaln,gtn,gten,ltn,lten]) and
             not(is_pchar(ld) and
                 (is_integer(rd) or (rd.deftype=pointerdef)) and
                 (treetyp=subn)
                )
            );
      end;


    function isunaryoperatoroverloadable(rd,dd : tdef;
             treetyp : tnodetype) : boolean;
      begin
        isunaryoperatoroverloadable:=false;
        { what assignment overloading should be allowed ?? }
        if (treetyp=assignn) then
          begin
            isunaryoperatoroverloadable:=true;
             { this already get tbs0261 to fail
             isunaryoperatoroverloadable:=not is_equal(rd,dd); PM }
          end
        { should we force that rd and dd are equal ?? }
        else if (treetyp=subn { unaryminusn }) then
          begin
            isunaryoperatoroverloadable:=
              not is_integer(rd) and not (rd.deftype=floatdef)
{$ifdef SUPPORT_MMX}
              and not ((cs_mmx in aktlocalswitches) and
              is_mmx_able_array(rd))
{$endif SUPPORT_MMX}
              ;
          end
        else if (treetyp=notn) then
          begin
            isunaryoperatoroverloadable:=not is_integer(rd) and not is_boolean(rd)
{$ifdef SUPPORT_MMX}
              and not ((cs_mmx in aktlocalswitches) and
              is_mmx_able_array(rd))
{$endif SUPPORT_MMX}
              ;
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
                        isbinaryoperatoroverloadable(ld,rd,dd,tok2node[i].nod);
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
         ht      : tnode;
      begin
        isbinaryoverloaded:=false;
        { overloaded operator ? }
        { load easier access variables }
        rd:=tbinarynode(t).right.resulttype.def;
        ld:=tbinarynode(t).left.resulttype.def;
        if isbinaryoperatoroverloadable(ld,rd,voidtype.def,t.nodetype) then
          begin
             isbinaryoverloaded:=true;
             {!!!!!!!!! handle paras }
             case t.nodetype of
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
                equaln,unequaln :
                  optoken:=_EQUAL;
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
             { the nil as symtable signs firstcalln that this is
               an overloaded operator }
             ht:=ccallnode.create(nil,overloaded_operators[optoken],nil,nil);
             { we have to convert p^.left and p^.right into
              callparanodes }
             if tcallnode(ht).symtableprocentry=nil then
               begin
                  CGMessage(parser_e_operator_not_overloaded);
                  ht.free;
                  isbinaryoverloaded:=false;
                  exit;
               end;
             inc(tcallnode(ht).symtableprocentry.refs);
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
              if p.location.loc = LOC_INVALID then
                internalerror(200110101);

              if (abs(p.left.registers32-p.right.registers32)<r32) or
                 ((p.location.loc = LOC_FPU) and
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
                (p.left.location.loc in [LOC_REFERENCE,LOC_MEM]) and
                (p.right.location.loc in [LOC_REFERENCE,LOC_MEM]) then
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
         is_procsym_load:=((p.nodetype=loadn) and (tloadnode(p).symtableentry.typ=procsym)) or
                          ((p.nodetype=addrn) and (taddrnode(p).left.nodetype=loadn)
                          and (tloadnode(taddrnode(p).left).symtableentry.typ=procsym)) ;
      end;


   { change a proc call to a procload for assignment to a procvar }
   { this can only happen for proc/function without arguments }
    function is_procsym_call(p:tnode):boolean;
      begin
        is_procsym_call:=(p.nodetype=calln) and (tcallnode(p).left=nil) and
             (((tcallnode(p).symtableprocentry.typ=procsym) and (tcallnode(p).right=nil)) or
             (assigned(tcallnode(p).right) and (tcallnode(tcallnode(p).right).symtableprocentry.typ=varsym)));
      end;


    { local routines can't be assigned to procvars }
    procedure test_local_to_procvar(from_def:tprocvardef;to_def:tdef);
      begin
         if (from_def.symtablelevel>1) and (to_def.deftype=procvardef) then
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
                    if must_be_valid and (nf_first in p.flags) then
                     begin
                       if (hsym.varstate=vs_declared_and_first_found) or
                          (hsym.varstate=vs_set_but_first_not_passed) then
                        begin
                          if (assigned(hsym.owner) and
                             assigned(aktprocsym) and
                             (hsym.owner = aktprocdef.localst)) then
                           begin
                             if tloadnode(p).symtable.symtabletype=localsymtable then
                              CGMessage1(sym_n_uninitialized_local_variable,hsym.realname)
                             else
                              CGMessage1(sym_n_uninitialized_variable,hsym.realname);
                           end;
                        end;
                     end;
                    if (nf_first in p.flags) then
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
                       exclude(p.flags,nf_first);
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
             funcretn:
               begin
                 { no claim if setting higher return value_str }
                 if must_be_valid and
                    (lexlevel=tfuncretnode(p).funcretsym.owner.symtablelevel) and
                    ((tfuncretnode(p).funcretsym.funcretstate=vs_declared) or
                    ((nf_is_first_funcret in p.flags) and
                     (tfuncretnode(p).funcretsym.funcretstate=vs_declared_and_first_found))) then
                   begin
                     CGMessage(sym_w_function_result_not_set);
                     { avoid multiple warnings }
                     tfuncretnode(p).funcretsym.funcretstate:=vs_assigned;
                   end;
                 if (nf_is_first_funcret in p.flags) and not must_be_valid then
                   tfuncretnode(p).funcretsym.funcretstate:=vs_assigned;
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


    procedure set_funcret_is_valid(p:tnode);
      begin
        while assigned(p) do
         begin
           case p.nodetype of
             funcretn:
               begin
                 if (nf_is_first_funcret in p.flags) then
                   tfuncretnode(p).funcretsym.funcretstate:=vs_assigned;
                 break;
               end;
             vecn,
             {derefn,}
             typeconvn,
             subscriptn:
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
        gotclass,
        gotderef : boolean;
        fromdef,
        todef    : tdef;
      begin
        valid_for_assign:=false;
        gotsubscript:=false;
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
             vecn,
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
             selfn,
             funcretn :
               begin
                 valid_for_assign:=true;
                 exit;
               end;
             calln :
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
                   funcretsym :
                     begin
                       valid_for_assign:=true;
                       exit;
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
  Revision 1.41  2002-01-16 09:33:46  jonas
    * no longer allow assignments to pointer expressions (unless there's a
      deref), reported by John Lee

  Revision 1.40  2001/12/31 16:59:41  peter
    * protected/private symbols parsing fixed

  Revision 1.39  2001/11/08 21:55:36  marco
   * Fix from Peter. Fixes a hang when ptop's upperstr procedure is converted
     to ansistrings

  Revision 1.38  2001/11/02 22:58:01  peter
    * procsym definition rewrite

  Revision 1.37  2001/10/20 20:30:21  peter
    * read only typed const support, switch $J-

  Revision 1.36  2001/10/12 13:51:51  jonas
    * fixed internalerror(10) due to previous fpu overflow fixes ("merged")
    * fixed bug in n386add (introduced after compilerproc changes for string
      operations) where calcregisters wasn't called for shortstring addnodes
    * NOTE: from now on, the location of a binary node must now always be set
       before you call calcregisters() for it

  Revision 1.35  2001/09/17 21:29:11  peter
    * merged netbsd, fpu-overflow from fixes branch

  Revision 1.34  2001/09/07 07:46:17  jonas
    * allow typecasting from child object types to parent object types (with
      different sizes)

  Revision 1.33  2001/09/02 21:13:31  peter
    * check for size differences in typecasts when assigning

  Revision 1.32  2001/08/26 13:36:37  florian
    * some cg reorganisation
    * some PPC updates

  Revision 1.31  2001/08/23 14:28:35  jonas
    + tempcreate/ref/delete nodes (allows the use of temps in the
      resulttype and first pass)
    * made handling of read(ln)/write(ln) processor independent
    * moved processor independent handling for str and reset/rewrite-typed
      from firstpass to resulttype pass
    * changed names of helpers in text.inc to be generic for use as
      compilerprocs + added "iocheck" directive for most of them
    * reading of ordinals is done by procedures instead of functions
      because otherwise FPC_IOCHECK overwrote the result before it could
      be stored elsewhere (range checking still works)
    * compilerprocs can now be used in the system unit before they are
      implemented
    * added note to errore.msg that booleans can't be read using read/readln

  Revision 1.30  2001/08/06 21:40:46  peter
    * funcret moved from tprocinfo to tprocdef

  Revision 1.29  2001/06/04 18:04:36  peter
    * fixes to valid_for_assign for properties

  Revision 1.28  2001/06/04 11:48:02  peter
    * better const to var checking

  Revision 1.27  2001/05/18 22:57:08  peter
    * replace constant by cpu dependent value (merged)

  Revision 1.26  2001/05/08 08:52:05  jonas
    * fix from Peter to avoid excessive number of warnings

  Revision 1.25  2001/04/22 22:46:49  florian
    * more variant support

  Revision 1.24  2001/04/13 01:22:08  peter
    * symtable change to classes
    * range check generation and errors fixed, make cycle DEBUG=1 works
    * memory leaks fixed

  Revision 1.23  2001/04/02 21:20:29  peter
    * resulttype rewrite

  Revision 1.22  2001/02/20 21:46:26  peter
    * don't allow assign to void type (merged)

  Revision 1.21  2001/02/04 11:12:17  jonas
    * fixed web bug 1377 & const pointer arithmtic

  Revision 1.20  2000/12/09 13:04:05  florian
    * web bug 1207 fixed: field and properties of const classes can be
      changed

  Revision 1.19  2000/11/29 00:30:31  florian
    * unused units removed from uses clause
    * some changes for widestrings

  Revision 1.18  2000/11/28 17:14:33  jonas
    * fixed crash when trying to use an overloaded operator which is nowhere
      defined

  Revision 1.17  2000/11/28 14:04:03  jonas
    * fixed operator overloading problems

  Revision 1.16  2000/11/13 11:30:54  florian
    * some bugs with interfaces and NIL fixed

  Revision 1.15  2000/11/12 22:20:37  peter
    * create generic toutputsection for binary writers

  Revision 1.14  2000/11/04 14:25:19  florian
    + merged Attila's changes for interfaces, not tested yet

  Revision 1.13  2000/10/31 22:02:47  peter
    * symtable splitted, no real code changes

  Revision 1.12  2000/10/14 10:14:47  peter
    * moehrendorf oct 2000 rewrite

  Revision 1.11  2000/10/01 19:48:23  peter
    * lot of compile updates for cg11

  Revision 1.10  2000/09/29 15:45:23  florian
    * make cycle fixed

  Revision 1.9  2000/09/28 19:49:51  florian
  *** empty log message ***

  Revision 1.8  2000/09/27 18:14:31  florian
    * fixed a lot of syntax errors in the n*.pas stuff

  Revision 1.7  2000/09/26 20:06:13  florian
    * hmm, still a lot of work to get things compilable

  Revision 1.6  2000/09/24 15:06:17  peter
    * use defines.inc

  Revision 1.5  2000/08/27 16:11:51  peter
    * moved some util functions from globals,cobjects to cutils
    * splitted files into finput,fmodule

  Revision 1.4  2000/08/16 18:33:53  peter
    * splitted namedobjectitem.next into indexnext and listnext so it
      can be used in both lists
    * don't allow "word = word" type definitions (merged)

  Revision 1.3  2000/08/07 11:31:04  jonas
    * fixed bug in type conversions between enum subranges (it didn't take
      the packenum directive into account)
    + define PACKENUMFIXED symbol in options.pas
     (merged from fixes branch)

  Revision 1.2  2000/07/13 11:32:41  michael
  + removed logs
}
