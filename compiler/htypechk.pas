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
{$ifdef cg11}
      node,
{$else cg11}
      tree,
{$endif cg11}
      symtable;

    type
{$ifdef cg11}
      Ttok2nodeRec=record
        tok : ttoken;
        nod : tnodetype;
        op_overloading_supported : boolean;
{$else cg11}
      Ttok2nodeRec=record
        tok : ttoken;
        nod : ttreetyp;
        op_overloading_supported : boolean;
{$endif cg11}
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

{$ifdef cg11}
    { if someones has a lot of time he might change this into methods }
    { tnode and its decendends                                        }
    { Conversion }
    function isconvertable(def_from,def_to : pdef;
             var doconv : tconverttype;fromtreetype : tnodetype;
             explicit : boolean) : byte;
    { is overloading of this operator allowed for this
      binary operator }
    function isbinaryoperatoroverloadable(ld, rd,dd : pdef;
             treetyp : tnodetype) : boolean;

    { is overloading of this operator allowed for this
      unary operator }
    function isunaryoperatoroverloadable(rd,dd : pdef;
             treetyp : tnodetype) : boolean;

    { check operator args and result type }
    function isoperatoracceptable(pf : pprocdef; optoken : ttoken) : boolean;

    { Register Allocation }
    procedure make_not_regable(p : tnode);
    procedure calcregisters(p : tbinarynode;r32,fpu,mmx : word);

    { subroutine handling }
    procedure test_protected_sym(sym : psym);
    procedure test_protected(p : tnode);
    function  valid_for_formal_var(p : tnode) : boolean;
    function  valid_for_formal_const(p : tnode) : boolean;
    function  is_procsym_load(p:tnode):boolean;
    function  is_procsym_call(p:tnode):boolean;
    function  assignment_overloaded(from_def,to_def : pdef) : pprocdef;
    procedure test_local_to_procvar(from_def:pprocvardef;to_def:pdef);
    function  valid_for_assign(p:tnode;allowprop:boolean):boolean;
{$else cg11}
    { Conversion }
    function isconvertable(def_from,def_to : pdef;
             var doconv : tconverttype;fromtreetype : ttreetyp;
             explicit : boolean) : byte;
    { is overloading of this operator allowed for this
      binary operator }
    function isbinaryoperatoroverloadable(ld, rd,dd : pdef;
             treetyp : ttreetyp) : boolean;

    { is overloading of this operator allowed for this
      unary operator }
    function isunaryoperatoroverloadable(rd,dd : pdef;
             treetyp : ttreetyp) : boolean;

    { check operator args and result type }
    function isoperatoracceptable(pf : pprocdef; optoken : ttoken) : boolean;

    { Register Allocation }
    procedure make_not_regable(p : ptree);
    procedure left_right_max(p : ptree);
    procedure calcregisters(p : ptree;r32,fpu,mmx : word);

    { subroutine handling }
    procedure test_protected_sym(sym : psym);
    procedure test_protected(p : ptree);
    function  valid_for_formal_var(p : ptree) : boolean;
    function  valid_for_formal_const(p : ptree) : boolean;
    function  is_procsym_load(p:Ptree):boolean;
    function  is_procsym_call(p:Ptree):boolean;
    function  assignment_overloaded(from_def,to_def : pdef) : pprocdef;
    procedure test_local_to_procvar(from_def:pprocvardef;to_def:pdef);
    function  valid_for_assign(p:ptree;allowprop:boolean):boolean;
{$endif cg11}

implementation

    uses
       globtype,systems,
       cutils,cobjects,verbose,globals,
       symconst,
       types,pass_1,cpubase,ncnv,nld,
       nmem,ncal,
{$ifdef newcg}
       cgbase
{$else}
       hcodegen
{$endif}
       ;

{$ifdef cg11}
{****************************************************************************
                             Convert
****************************************************************************}

    { Returns:
       0 - Not convertable
       1 - Convertable
       2 - Convertable, but not first choice }
    function isconvertable(def_from,def_to : pdef;
             var doconv : tconverttype;fromtreetype : tnodetype;
             explicit : boolean) : byte;

      { Tbasetype:  uauto,uvoid,uchar,
                    u8bit,u16bit,u32bit,
                    s8bit,s16bit,s32,
                    bool8bit,bool16bit,bool32bit,
                    u64bit,s64bitint }
      type
        tbasedef=(bvoid,bchar,bint,bbool);
      const
        basedeftbl:array[tbasetype] of tbasedef =
          (bvoid,bvoid,bchar,
           bint,bint,bint,
           bint,bint,bint,
           bbool,bbool,bbool,bint,bint,bchar);

        basedefconverts : array[tbasedef,tbasedef] of tconverttype =
         ((tc_not_possible,tc_not_possible,tc_not_possible,tc_not_possible),
          (tc_not_possible,tc_equal,tc_not_possible,tc_not_possible),
          (tc_not_possible,tc_not_possible,tc_int_2_int,tc_int_2_bool),
          (tc_not_possible,tc_not_possible,tc_bool_2_int,tc_bool_2_bool));

      var
         b : byte;
         hd1,hd2 : pdef;
         hct : tconverttype;
      begin
       { safety check }
         if not(assigned(def_from) and assigned(def_to)) then
          begin
            isconvertable:=0;
            exit;
          end;

       { tp7 procvar def support, in tp7 a procvar is always called, if the
         procvar is passed explicit a addrn would be there }
         if (m_tp_procvar in aktmodeswitches) and
            (def_from^.deftype=procvardef) and
            (fromtreetype=loadn) then
          begin
            def_from:=pprocvardef(def_from)^.rettype.def;
          end;

       { we walk the wanted (def_to) types and check then the def_from
         types if there is a conversion possible }
         b:=0;
         case def_to^.deftype of
           orddef :
             begin
               case def_from^.deftype of
                 orddef :
                   begin
                     doconv:=basedefconverts[basedeftbl[porddef(def_from)^.typ],basedeftbl[porddef(def_to)^.typ]];
                     b:=1;
                     if (doconv=tc_not_possible) or
                        ((doconv=tc_int_2_bool) and
                         (not explicit) and
                         (not is_boolean(def_from))) or
                        ((doconv=tc_bool_2_int) and
                         (not explicit) and
                         (not is_boolean(def_to))) then
                       b:=0;
                   end;
                 enumdef :
                   begin
                     { needed for char(enum) }
                     if explicit then
                      begin
                        doconv:=tc_int_2_int;
                        b:=1;
                      end;
                   end;
               end;
             end;

          stringdef :
             begin
               case def_from^.deftype of
                 stringdef :
                   begin
                     doconv:=tc_string_2_string;
                     b:=1;
                   end;
                 orddef :
                   begin
                   { char to string}
                     if is_char(def_from) then
                      begin
                        doconv:=tc_char_2_string;
                        b:=1;
                      end;
                   end;
                 arraydef :
                   begin
                   { array of char to string, the length check is done by the firstpass of this node }
                     if is_chararray(def_from) then
                      begin
                        doconv:=tc_chararray_2_string;
                        if (not(cs_ansistrings in aktlocalswitches) and
                            is_shortstring(def_to)) or
                           ((cs_ansistrings in aktlocalswitches) and
                            is_ansistring(def_to)) then
                         b:=1
                        else
                         b:=2;
                      end;
                   end;
                 pointerdef :
                   begin
                   { pchar can be assigned to short/ansistrings,
                     but not in tp7 compatible mode }
                     if is_pchar(def_from) and not(m_tp7 in aktmodeswitches) then
                      begin
                        doconv:=tc_pchar_2_string;
                        b:=1;
                      end;
                   end;
               end;
             end;

           floatdef :
             begin
               case def_from^.deftype of
                 orddef :
                   begin { ordinal to real }
                     if is_integer(def_from) then
                       begin
                          if pfloatdef(def_to)^.typ=f32bit then
                            doconv:=tc_int_2_fix
                          else
                            doconv:=tc_int_2_real;
                          b:=1;
                       end;
                   end;
                 floatdef :
                   begin { 2 float types ? }
                     if pfloatdef(def_from)^.typ=pfloatdef(def_to)^.typ then
                       doconv:=tc_equal
                     else
                       begin
                          if pfloatdef(def_from)^.typ=f32bit then
                            doconv:=tc_fix_2_real
                          else
                            if pfloatdef(def_to)^.typ=f32bit then
                              doconv:=tc_real_2_fix
                            else
                              doconv:=tc_real_2_real;
                       end;
                     b:=1;
                   end;
               end;
             end;

           enumdef :
             begin
               if (def_from^.deftype=enumdef) then
                begin
                  hd1:=def_from;
                  while assigned(penumdef(hd1)^.basedef) do
                   hd1:=penumdef(hd1)^.basedef;
                  hd2:=def_to;
                  while assigned(penumdef(hd2)^.basedef) do
                    hd2:=penumdef(hd2)^.basedef;
                  if (hd1=hd2) then
                    begin
                       b:=1;
                       { because of packenum they can have different sizes! (JM) }
                       doconv:=tc_int_2_int;
                    end;
                end;
             end;

           arraydef :
             begin
             { open array is also compatible with a single element of its base type }
               if is_open_array(def_to) and
                  is_equal(parraydef(def_to)^.elementtype.def,def_from) then
                begin
                  doconv:=tc_equal;
                  b:=1;
                end
               else
                begin
                  case def_from^.deftype of
                    arraydef :
                      begin
                        { array constructor -> open array }
                        if is_open_array(def_to) and
                           is_array_constructor(def_from) then
                         begin
                           if is_void(parraydef(def_from)^.elementtype.def) or
                              is_equal(parraydef(def_to)^.elementtype.def,parraydef(def_from)^.elementtype.def) then
                            begin
                              doconv:=tc_equal;
                              b:=1;
                            end
                           else
                            if isconvertable(parraydef(def_from)^.elementtype.def,
                                             parraydef(def_to)^.elementtype.def,hct,arrayconstructn,false)<>0 then
                             begin
                               doconv:=hct;
                               b:=2;
                             end;
                         end;
                      end;
                    pointerdef :
                      begin
                        if is_zero_based_array(def_to) and
                           is_equal(ppointerdef(def_from)^.pointertype.def,parraydef(def_to)^.elementtype.def) then
                         begin
                           doconv:=tc_pointer_2_array;
                           b:=1;
                         end;
                      end;
                    stringdef :
                      begin
                        { string to array of char}
                        if (not(is_special_array(def_to)) or is_open_array(def_to)) and
                          is_equal(parraydef(def_to)^.elementtype.def,cchardef) then
                         begin
                           doconv:=tc_string_2_chararray;
                           b:=1;
                         end;
                      end;
                  end;
                end;
             end;

           pointerdef :
             begin
               case def_from^.deftype of
                 stringdef :
                   begin
                     { string constant (which can be part of array constructor)
                       to zero terminated string constant }
                     if (fromtreetype in [arrayconstructn,stringconstn]) and
                        is_pchar(def_to) then
                      begin
                        doconv:=tc_cstring_2_pchar;
                        b:=1;
                      end;
                   end;
                 orddef :
                   begin
                     { char constant to zero terminated string constant }
                     if (fromtreetype=ordconstn) then
                      begin
                        if is_equal(def_from,cchardef) and
                           is_pchar(def_to) then
                         begin
                           doconv:=tc_cchar_2_pchar;
                           b:=1;
                         end
                        else
                         if is_integer(def_from) then
                          begin
                            doconv:=tc_cord_2_pointer;
                            b:=1;
                          end;
                      end;
                   end;
                 arraydef :
                   begin
                     { chararray to pointer }
                     if is_zero_based_array(def_from) and
                        is_equal(parraydef(def_from)^.elementtype.def,ppointerdef(def_to)^.pointertype.def) then
                      begin
                        doconv:=tc_array_2_pointer;
                        b:=1;
                      end;
                   end;
                 pointerdef :
                   begin
                     { child class pointer can be assigned to anchestor pointers }
                     if (
                         (ppointerdef(def_from)^.pointertype.def^.deftype=objectdef) and
                         (ppointerdef(def_to)^.pointertype.def^.deftype=objectdef) and
                         pobjectdef(ppointerdef(def_from)^.pointertype.def)^.is_related(
                           pobjectdef(ppointerdef(def_to)^.pointertype.def))
                        ) or
                        { all pointers can be assigned to void-pointer }
                        is_equal(ppointerdef(def_to)^.pointertype.def,voiddef) or
                        { in my opnion, is this not clean pascal }
                        { well, but it's handy to use, it isn't ? (FK) }
                        is_equal(ppointerdef(def_from)^.pointertype.def,voiddef) then
                       begin
                         doconv:=tc_equal;
                         b:=1;
                       end;
                   end;
                 procvardef :
                   begin
                     { procedure variable can be assigned to an void pointer }
                     { Not anymore. Use the @ operator now.}
                     if not(m_tp_procvar in aktmodeswitches) and
                        (ppointerdef(def_to)^.pointertype.def^.deftype=orddef) and
                        (porddef(ppointerdef(def_to)^.pointertype.def)^.typ=uvoid) then
                      begin
                        doconv:=tc_equal;
                        b:=1;
                      end;
                   end;
                 classrefdef,
                 objectdef :
                   begin
                     { class types and class reference type
                       can be assigned to void pointers      }
                     if (
                         ((def_from^.deftype=objectdef) and pobjectdef(def_from)^.is_class) or
                         (def_from^.deftype=classrefdef)
                        ) and
                        (ppointerdef(def_to)^.pointertype.def^.deftype=orddef) and
                        (porddef(ppointerdef(def_to)^.pointertype.def)^.typ=uvoid) then
                       begin
                         doconv:=tc_equal;
                         b:=1;
                       end;
                   end;
               end;
             end;

           setdef :
             begin
               { automatic arrayconstructor -> set conversion }
               if is_array_constructor(def_from) then
                begin
                  doconv:=tc_arrayconstructor_2_set;
                  b:=1;
                end;
             end;

           procvardef :
             begin
               { proc -> procvar }
               if (def_from^.deftype=procdef) then
                begin
                  doconv:=tc_proc_2_procvar;
                  if proc_to_procvar_equal(pprocdef(def_from),pprocvardef(def_to)) then
                   b:=1;
                end
               else
                { for example delphi allows the assignement from pointers }
                { to procedure variables                                  }
                if (m_pointer_2_procedure in aktmodeswitches) and
                  (def_from^.deftype=pointerdef) and
                  (ppointerdef(def_from)^.pointertype.def^.deftype=orddef) and
                  (porddef(ppointerdef(def_from)^.pointertype.def)^.typ=uvoid) then
                begin
                   doconv:=tc_equal;
                   b:=1;
                end
               else
               { nil is compatible with procvars }
                if (fromtreetype=niln) then
                 begin
                   doconv:=tc_equal;
                   b:=1;
                 end;
             end;

           objectdef :
             begin
               { object pascal objects }
               if (def_from^.deftype=objectdef) {and
                  pobjectdef(def_from)^.isclass and pobjectdef(def_to)^.isclass }then
                begin
                  doconv:=tc_equal;
                  if pobjectdef(def_from)^.is_related(pobjectdef(def_to)) then
                   b:=1;
                end
               else
               { Class specific }
                if (pobjectdef(def_to)^.is_class) then
                 begin
                   { void pointer also for delphi mode }
                   if (m_delphi in aktmodeswitches) and
                      is_voidpointer(def_from) then
                    begin
                      doconv:=tc_equal;
                      b:=1;
                    end
                   else
                   { nil is compatible with class instances }
                    if (fromtreetype=niln) and (pobjectdef(def_to)^.is_class) then
                     begin
                       doconv:=tc_equal;
                       b:=1;
                     end;
                 end;
             end;

           classrefdef :
             begin
               { class reference types }
               if (def_from^.deftype=classrefdef) then
                begin
                  doconv:=tc_equal;
                  if pobjectdef(pclassrefdef(def_from)^.pointertype.def)^.is_related(
                       pobjectdef(pclassrefdef(def_to)^.pointertype.def)) then
                   b:=1;
                end
               else
                { nil is compatible with class references }
                if (fromtreetype=niln) then
                 begin
                   doconv:=tc_equal;
                   b:=1;
                 end;
             end;

           filedef :
             begin
               { typed files are all equal to the abstract file type
               name TYPEDFILE in system.pp in is_equal in types.pas
               the problem is that it sholud be also compatible to FILE
               but this would leed to a problem for ASSIGN RESET and REWRITE
               when trying to find the good overloaded function !!
               so all file function are doubled in system.pp
               this is not very beautiful !!}
               if (def_from^.deftype=filedef) and
                  (
                   (
                    (pfiledef(def_from)^.filetyp = ft_typed) and
                    (pfiledef(def_to)^.filetyp = ft_typed) and
                    (
                     (pfiledef(def_from)^.typedfiletype.def = pdef(voiddef)) or
                     (pfiledef(def_to)^.typedfiletype.def = pdef(voiddef))
                    )
                   ) or
                   (
                    (
                     (pfiledef(def_from)^.filetyp = ft_untyped) and
                     (pfiledef(def_to)^.filetyp = ft_typed)
                    ) or
                    (
                     (pfiledef(def_from)^.filetyp = ft_typed) and
                     (pfiledef(def_to)^.filetyp = ft_untyped)
                    )
                   )
                  ) then
                 begin
                    doconv:=tc_equal;
                    b:=1;
                 end
             end;

           else
             begin
             { assignment overwritten ?? }
               if assignment_overloaded(def_from,def_to)<>nil then
                b:=2;
             end;
         end;
        isconvertable:=b;
      end;

    { ld is the left type definition
      rd the right type definition
      dd the result type definition  or voiddef if unkown }
    function isbinaryoperatoroverloadable(ld, rd, dd : pdef;
             treetyp : tnodetype) : boolean;
      begin
        isbinaryoperatoroverloadable:=
           (treetyp=starstarn) or
           (ld^.deftype=recorddef) or
           (rd^.deftype=recorddef) or
           ((rd^.deftype=pointerdef) and
            not(is_pchar(rd) and
                (is_chararray(ld) or
                 (ld^.deftype=stringdef) or
                 (treetyp=addn))) and
            (not(ld^.deftype in [pointerdef,objectdef,classrefdef,procvardef]) or
             not (treetyp in [equaln,unequaln,gtn,gten,ltn,lten,subn])
            ) and
            (not is_integer(ld) or not (treetyp in [addn,subn]))
           ) or
           ((ld^.deftype=pointerdef) and
            not(is_pchar(ld) and
                (is_chararray(rd) or
                 (rd^.deftype=stringdef) or
                 (treetyp=addn))) and
            (not(rd^.deftype in [stringdef,pointerdef,objectdef,classrefdef,procvardef]) and
             ((not is_integer(rd) and (rd^.deftype<>objectdef)
               and (rd^.deftype<>classrefdef)) or
              not (treetyp in [equaln,unequaln,gtn,gten,ltn,lten,addn,subn])
             )
            )
           ) or
           { array def, but not mmx or chararray+[char,string,chararray] }
           ((ld^.deftype=arraydef) and
            not((cs_mmx in aktlocalswitches) and
                is_mmx_able_array(ld)) and
            not(is_chararray(ld) and
                (is_char(rd) or
                is_pchar(rd) or
                 (rd^.deftype=stringdef) or
                 is_chararray(rd)))
           ) or
           ((rd^.deftype=arraydef) and
            not((cs_mmx in aktlocalswitches) and
                is_mmx_able_array(rd)) and
            not(is_chararray(rd) and
                (is_char(ld) or
                 is_pchar(ld) or
                 (ld^.deftype=stringdef) or
                 is_chararray(ld)))
           ) or
           { <> and = are defined for classes }
           ((ld^.deftype=objectdef) and
            (not(pobjectdef(ld)^.is_class) or
             not(treetyp in [equaln,unequaln])
            )
           ) or
           ((rd^.deftype=objectdef) and
            (not(pobjectdef(rd)^.is_class) or
             not(treetyp in [equaln,unequaln])
            )
             or
           { allow other operators that + on strings }
           (
            (is_char(rd) or
             is_pchar(rd) or
             (rd^.deftype=stringdef) or
             is_chararray(rd) or
             is_char(ld) or
             is_pchar(ld) or
             (ld^.deftype=stringdef) or
             is_chararray(ld)
             ) and
             not(treetyp in [addn,equaln,unequaln,gtn,gten,ltn,lten]) and
             not(is_pchar(ld) and
                 (is_integer(rd) or (rd^.deftype=pointerdef)) and
                 (treetyp=subn)
                )
            )
           );
      end;


    function isunaryoperatoroverloadable(rd,dd : pdef;
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
              not is_integer(rd) and not (rd^.deftype=floatdef)
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

    function isoperatoracceptable(pf : pprocdef; optoken : ttoken) : boolean;
      var
        ld,rd,dd : pdef;
        i : longint;
      begin
        case pf^.parast^.symindex^.count of
          2 : begin
                isoperatoracceptable:=false;
                for i:=1 to tok2nodes do
                  if tok2node[i].tok=optoken then
                    begin
                      ld:=pvarsym(pf^.parast^.symindex^.first)^.vartype.def;
                      rd:=pvarsym(pf^.parast^.symindex^.first^.indexnext)^.vartype.def;
                      dd:=pf^.rettype.def;
                      isoperatoracceptable:=
                        tok2node[i].op_overloading_supported and
                        isbinaryoperatoroverloadable(ld,rd,dd,tok2node[i].nod);
                      break;
                    end;
              end;
          1 : begin
                rd:=pvarsym(pf^.parast^.symindex^.first)^.vartype.def;
                dd:=pf^.rettype.def;
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
              if tloadnode(p).symtableentry^.typ=varsym then
                pvarsym(tloadnode(p).symtableentry)^.varoptions:=pvarsym(tloadnode(p).symtableentry)^.varoptions-[vo_regable,vo_fpuregable];
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
              if (abs(p.left.registers32-p.right.registers32)<r32) then
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
         if p.registersfpu>8 then
          CGMessage(cg_e_too_complex_expr);
      end;

{****************************************************************************
                          Subroutine Handling
****************************************************************************}

{ protected field handling
  protected field can not appear in
  var parameters of function !!
  this can only be done after we have determined the
  overloaded function
  this is the reason why it is not in the parser, PM }

    procedure test_protected_sym(sym : psym);
      begin
         if (sp_protected in sym^.symoptions) and
            ((sym^.owner^.symtabletype=unitsymtable) or
             ((sym^.owner^.symtabletype=objectsymtable) and
             (pobjectdef(sym^.owner^.defowner)^.owner^.symtabletype=unitsymtable))
            ) then
          CGMessage(parser_e_cant_access_protected_member);
      end;


    procedure test_protected(p : tnode);
      begin
        case p.nodetype of
         loadn : test_protected_sym(tloadnode(p).symtableentry);
     typeconvn : test_protected(ttypeconvnode(p).left);
        derefn : test_protected(tderefnode(p).left);
    subscriptn : begin
                 { test_protected(p.left);
                   Is a field of a protected var
                   also protected ???  PM }
                   test_protected_sym(tsubscriptnode(p).vs);
                 end;
        end;
      end;

   function  valid_for_formal_var(p : tnode) : boolean;
     var
        v : boolean;
     begin
        case p.nodetype of
         loadn :
           v:=(tloadnode(p).symtableentry^.typ in [typedconstsym,varsym]);
         typeconvn :
           v:=valid_for_formal_var(ttypeconvnode(p).left);
         derefn,
         subscriptn,
         vecn,
         funcretn,
         selfn :
           v:=true;
         calln : { procvars are callnodes first }
           v:=assigned(tcallnode(p).right) and not assigned(tcallnode(p).left);
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
           v:=false;
        end;
        valid_for_formal_var:=v;
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

    function is_procsym_load(p:tnode):boolean;
      begin
         is_procsym_load:=((p.nodetype=loadn) and (tloadnode(p).symtableentry^.typ=procsym)) or
                          ((p.nodetype=addrn) and (taddrnode(p).left.nodetype=loadn)
                          and (tloadnode(taddrnode(p).left).symtableentry^.typ=procsym)) ;
      end;

   { change a proc call to a procload for assignment to a procvar }
   { this can only happen for proc/function without arguments }
    function is_procsym_call(p:tnode):boolean;
      begin
        is_procsym_call:=(p.nodetype=calln) and (tcallnode(p).left=nil) and
             (((tcallnode(p).symtableprocentry^.typ=procsym) and (tcallnode(p).right=nil)) or
             (assigned(tcallnode(p).right) and (tcallnode(tcallnode(p).right).symtableprocentry^.typ=varsym)));
      end;


    function assignment_overloaded(from_def,to_def : pdef) : pprocdef;
       var
          passproc : pprocdef;
          convtyp : tconverttype;
       begin
          assignment_overloaded:=nil;
          if assigned(overloaded_operators[_assignment]) then
            passproc:=overloaded_operators[_assignment]^.definition
          else
            exit;
          while passproc<>nil do
            begin
              if is_equal(passproc^.rettype.def,to_def) and
                 (is_equal(pparaitem(passproc^.para^.first)^.paratype.def,from_def) or
                 (isconvertable(from_def,pparaitem(passproc^.para^.first)^.paratype.def,convtyp,ordconstn,false)=1)) then
                begin
                   assignment_overloaded:=passproc;
                   break;
                end;
              passproc:=passproc^.nextoverloaded;
            end;
       end;


    { local routines can't be assigned to procvars }
    procedure test_local_to_procvar(from_def:pprocvardef;to_def:pdef);
      begin
         if (from_def^.symtablelevel>1) and (to_def^.deftype=procvardef) then
           CGMessage(type_e_cannot_local_proc_to_procvar);
      end;


    function valid_for_assign(p:tnode;allowprop:boolean):boolean;
      var
        hp : tnode;
        gotwith,
        gotsubscript,
        gotpointer,
        gotclass,
        gotderef : boolean;
      begin
        valid_for_assign:=false;
        gotsubscript:=false;
        gotderef:=false;
        gotclass:=false;
        gotpointer:=false;
        gotwith:=false;
        hp:=p;
        while assigned(hp) do
         begin
           { property allowed? calln has a property check itself }
           if (not allowprop) and
              (nf_isproperty in hp.flags) and
              (hp.nodetype<>calln) then
            begin
              CGMessagePos(hp.fileinfo,type_e_argument_cant_be_assigned);
              exit;
            end;
           case hp.nodetype of
             derefn :
               begin
                 gotderef:=true;
                 hp:=tderefnode(hp).left;
               end;
             typeconvn :
               begin
                 case hp.resulttype^.deftype of
                   pointerdef :
                     gotpointer:=true;
                   objectdef :
                     gotclass:=pobjectdef(hp.resulttype)^.is_class;
                   classrefdef :
                     gotclass:=true;
                   arraydef :
                     begin
                       { pointer -> array conversion is done then we need to see it
                         as a deref, because a ^ is then not required anymore }
                       if (ttypeconvnode(hp).left.resulttype^.deftype=pointerdef) then
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
                 hp:=tsubscriptnode(hp).left;
               end;
             subn,
             addn :
               begin
                 { Allow add/sub operators on a pointer, or an integer
                   and a pointer typecast and deref has been found }
                 if (hp.resulttype^.deftype=pointerdef) or
                    (is_integer(hp.resulttype) and gotpointer and gotderef) then
                  valid_for_assign:=true
                 else
                  CGMessagePos(hp.fileinfo,type_e_variable_id_expected);
                 exit;
               end;
             addrn :
               begin
                 if not(gotderef) and
                    not(nf_procvarload in hp.flags) then
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
                 case hp.resulttype^.deftype of
                   pointerdef :
                     gotpointer:=true;
                   objectdef :
                     gotclass:=pobjectdef(hp.resulttype)^.is_class;
                   recorddef, { handle record like class it needs a subscription }
                   classrefdef :
                     gotclass:=true;
                 end;
                 { 1. if it returns a pointer and we've found a deref,
                   2. if it returns a class or record and a subscription or with is found,
                   3. property is allowed }
                 if (gotpointer and gotderef) or
                    (gotclass and (gotsubscript or gotwith)) or
                    ((nf_isproperty in hp.flags) and allowprop) then
                  valid_for_assign:=true
                 else
                  CGMessagePos(hp.fileinfo,type_e_argument_cant_be_assigned);
                 exit;
               end;
             loadn :
               begin
                 case tloadnode(hp).symtableentry^.typ of
                   absolutesym,
                   varsym :
                     begin
                       if (pvarsym(tloadnode(hp).symtableentry)^.varspez=vs_const) then
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
                       if (tloadnode(hp).symtable^.symtabletype=withsymtable) then
                        begin
                          { continue with processing the withref node }
                          hp:=tnode(pwithsymtable(tloadnode(hp).symtable)^.withrefnode);
                          gotwith:=true;
                        end
                       else
                        begin
                          { set the assigned flag for varsyms }
                          if (pvarsym(tloadnode(hp).symtableentry)^.varstate=vs_declared) then
                           pvarsym(tloadnode(hp).symtableentry)^.varstate:=vs_assigned;
                          valid_for_assign:=true;
                          exit;
                        end;
                     end;
                   funcretsym,
                   typedconstsym :
                     begin
                       valid_for_assign:=true;
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

{$else cg11}
{****************************************************************************
                             Convert
****************************************************************************}

    { Returns:
       0 - Not convertable
       1 - Convertable
       2 - Convertable, but not first choice }
    function isconvertable(def_from,def_to : pdef;
             var doconv : tconverttype;fromtreetype : ttreetyp;
             explicit : boolean) : byte;

      { Tbasetype:  uauto,uvoid,uchar,
                    u8bit,u16bit,u32bit,
                    s8bit,s16bit,s32,
                    bool8bit,bool16bit,bool32bit,
                    u64bit,s64bitint }
      type
        tbasedef=(bvoid,bchar,bint,bbool);
      const
        basedeftbl:array[tbasetype] of tbasedef =
          (bvoid,bvoid,bchar,
           bint,bint,bint,
           bint,bint,bint,
           bbool,bbool,bbool,bint,bint,bchar);

        basedefconverts : array[tbasedef,tbasedef] of tconverttype =
         ((tc_not_possible,tc_not_possible,tc_not_possible,tc_not_possible),
          (tc_not_possible,tc_equal,tc_not_possible,tc_not_possible),
          (tc_not_possible,tc_not_possible,tc_int_2_int,tc_int_2_bool),
          (tc_not_possible,tc_not_possible,tc_bool_2_int,tc_bool_2_bool));

      var
         b : byte;
         hd1,hd2 : pdef;
         hct : tconverttype;
      begin
       { safety check }
         if not(assigned(def_from) and assigned(def_to)) then
          begin
            isconvertable:=0;
            exit;
          end;

       { tp7 procvar def support, in tp7 a procvar is always called, if the
         procvar is passed explicit a addrn would be there }
         if (m_tp_procvar in aktmodeswitches) and
            (def_from^.deftype=procvardef) and
            (fromtreetype=loadn) then
          begin
            def_from:=pprocvardef(def_from)^.rettype.def;
          end;

       { we walk the wanted (def_to) types and check then the def_from
         types if there is a conversion possible }
         b:=0;
         case def_to^.deftype of
           orddef :
             begin
               case def_from^.deftype of
                 orddef :
                   begin
                     doconv:=basedefconverts[basedeftbl[porddef(def_from)^.typ],basedeftbl[porddef(def_to)^.typ]];
                     b:=1;
                     if (doconv=tc_not_possible) or
                        ((doconv=tc_int_2_bool) and
                         (not explicit) and
                         (not is_boolean(def_from))) or
                        ((doconv=tc_bool_2_int) and
                         (not explicit) and
                         (not is_boolean(def_to))) then
                       b:=0;
                   end;
                 enumdef :
                   begin
                     { needed for char(enum) }
                     if explicit then
                      begin
                        doconv:=tc_int_2_int;
                        b:=1;
                      end;
                   end;
               end;
             end;

          stringdef :
             begin
               case def_from^.deftype of
                 stringdef :
                   begin
                     doconv:=tc_string_2_string;
                     b:=1;
                   end;
                 orddef :
                   begin
                   { char to string}
                     if is_char(def_from) then
                      begin
                        doconv:=tc_char_2_string;
                        b:=1;
                      end;
                   end;
                 arraydef :
                   begin
                   { array of char to string, the length check is done by the firstpass of this node }
                     if is_chararray(def_from) then
                      begin
                        doconv:=tc_chararray_2_string;
                        if (not(cs_ansistrings in aktlocalswitches) and
                            is_shortstring(def_to)) or
                           ((cs_ansistrings in aktlocalswitches) and
                            is_ansistring(def_to)) then
                         b:=1
                        else
                         b:=2;
                      end;
                   end;
                 pointerdef :
                   begin
                   { pchar can be assigned to short/ansistrings,
                     but not in tp7 compatible mode }
                     if is_pchar(def_from) and not(m_tp7 in aktmodeswitches) then
                      begin
                        doconv:=tc_pchar_2_string;
                        b:=1;
                      end;
                   end;
               end;
             end;

           floatdef :
             begin
               case def_from^.deftype of
                 orddef :
                   begin { ordinal to real }
                     if is_integer(def_from) then
                       begin
                          if pfloatdef(def_to)^.typ=f32bit then
                            doconv:=tc_int_2_fix
                          else
                            doconv:=tc_int_2_real;
                          b:=1;
                       end;
                   end;
                 floatdef :
                   begin { 2 float types ? }
                     if pfloatdef(def_from)^.typ=pfloatdef(def_to)^.typ then
                       doconv:=tc_equal
                     else
                       begin
                          if pfloatdef(def_from)^.typ=f32bit then
                            doconv:=tc_fix_2_real
                          else
                            if pfloatdef(def_to)^.typ=f32bit then
                              doconv:=tc_real_2_fix
                            else
                              doconv:=tc_real_2_real;
                       end;
                     b:=1;
                   end;
               end;
             end;

           enumdef :
             begin
               if (def_from^.deftype=enumdef) then
                begin
                  hd1:=def_from;
                  while assigned(penumdef(hd1)^.basedef) do
                   hd1:=penumdef(hd1)^.basedef;
                  hd2:=def_to;
                  while assigned(penumdef(hd2)^.basedef) do
                    hd2:=penumdef(hd2)^.basedef;
                  if (hd1=hd2) then
                    begin
                       b:=1;
                       { because of packenum they can have different sizes! (JM) }
                       doconv:=tc_int_2_int;
                    end;
                end;
             end;

           arraydef :
             begin
             { open array is also compatible with a single element of its base type }
               if is_open_array(def_to) and
                  is_equal(parraydef(def_to)^.elementtype.def,def_from) then
                begin
                  doconv:=tc_equal;
                  b:=1;
                end
               else
                begin
                  case def_from^.deftype of
                    arraydef :
                      begin
                        { array constructor -> open array }
                        if is_open_array(def_to) and
                           is_array_constructor(def_from) then
                         begin
                           if is_void(parraydef(def_from)^.elementtype.def) or
                              is_equal(parraydef(def_to)^.elementtype.def,parraydef(def_from)^.elementtype.def) then
                            begin
                              doconv:=tc_equal;
                              b:=1;
                            end
                           else
                            if isconvertable(parraydef(def_from)^.elementtype.def,
                                             parraydef(def_to)^.elementtype.def,hct,arrayconstructn,false)<>0 then
                             begin
                               doconv:=hct;
                               b:=2;
                             end;
                         end;
                      end;
                    pointerdef :
                      begin
                        if is_zero_based_array(def_to) and
                           is_equal(ppointerdef(def_from)^.pointertype.def,parraydef(def_to)^.elementtype.def) then
                         begin
                           doconv:=tc_pointer_2_array;
                           b:=1;
                         end;
                      end;
                    stringdef :
                      begin
                        { string to array of char}
                        if (not(is_special_array(def_to)) or is_open_array(def_to)) and
                          is_equal(parraydef(def_to)^.elementtype.def,cchardef) then
                         begin
                           doconv:=tc_string_2_chararray;
                           b:=1;
                         end;
                      end;
                  end;
                end;
             end;

           pointerdef :
             begin
               case def_from^.deftype of
                 stringdef :
                   begin
                     { string constant (which can be part of array constructor)
                       to zero terminated string constant }
                     if (fromtreetype in [arrayconstructn,stringconstn]) and
                        is_pchar(def_to) then
                      begin
                        doconv:=tc_cstring_2_pchar;
                        b:=1;
                      end;
                   end;
                 orddef :
                   begin
                     { char constant to zero terminated string constant }
                     if (fromtreetype=ordconstn) then
                      begin
                        if is_equal(def_from,cchardef) and
                           is_pchar(def_to) then
                         begin
                           doconv:=tc_cchar_2_pchar;
                           b:=1;
                         end
                        else
                         if is_integer(def_from) then
                          begin
                            doconv:=tc_cord_2_pointer;
                            b:=1;
                          end;
                      end;
                   end;
                 arraydef :
                   begin
                     { chararray to pointer }
                     if is_zero_based_array(def_from) and
                        is_equal(parraydef(def_from)^.elementtype.def,ppointerdef(def_to)^.pointertype.def) then
                      begin
                        doconv:=tc_array_2_pointer;
                        b:=1;
                      end;
                   end;
                 pointerdef :
                   begin
                     { child class pointer can be assigned to anchestor pointers }
                     if (
                         (ppointerdef(def_from)^.pointertype.def^.deftype=objectdef) and
                         (ppointerdef(def_to)^.pointertype.def^.deftype=objectdef) and
                         pobjectdef(ppointerdef(def_from)^.pointertype.def)^.is_related(
                           pobjectdef(ppointerdef(def_to)^.pointertype.def))
                        ) or
                        { all pointers can be assigned to void-pointer }
                        is_equal(ppointerdef(def_to)^.pointertype.def,voiddef) or
                        { in my opnion, is this not clean pascal }
                        { well, but it's handy to use, it isn't ? (FK) }
                        is_equal(ppointerdef(def_from)^.pointertype.def,voiddef) then
                       begin
                         doconv:=tc_equal;
                         b:=1;
                       end;
                   end;
                 procvardef :
                   begin
                     { procedure variable can be assigned to an void pointer }
                     { Not anymore. Use the @ operator now.}
                     if not(m_tp_procvar in aktmodeswitches) and
                        (ppointerdef(def_to)^.pointertype.def^.deftype=orddef) and
                        (porddef(ppointerdef(def_to)^.pointertype.def)^.typ=uvoid) then
                      begin
                        doconv:=tc_equal;
                        b:=1;
                      end;
                   end;
                 classrefdef,
                 objectdef :
                   begin
                     { class types and class reference type
                       can be assigned to void pointers      }
                     if (
                         ((def_from^.deftype=objectdef) and pobjectdef(def_from)^.is_class) or
                         (def_from^.deftype=classrefdef)
                        ) and
                        (ppointerdef(def_to)^.pointertype.def^.deftype=orddef) and
                        (porddef(ppointerdef(def_to)^.pointertype.def)^.typ=uvoid) then
                       begin
                         doconv:=tc_equal;
                         b:=1;
                       end;
                   end;
               end;
             end;

           setdef :
             begin
               { automatic arrayconstructor -> set conversion }
               if is_array_constructor(def_from) then
                begin
                  doconv:=tc_arrayconstructor_2_set;
                  b:=1;
                end;
             end;

           procvardef :
             begin
               { proc -> procvar }
               if (def_from^.deftype=procdef) then
                begin
                  doconv:=tc_proc_2_procvar;
                  if proc_to_procvar_equal(pprocdef(def_from),pprocvardef(def_to)) then
                   b:=1;
                end
               else
                { for example delphi allows the assignement from pointers }
                { to procedure variables                                  }
                if (m_pointer_2_procedure in aktmodeswitches) and
                  (def_from^.deftype=pointerdef) and
                  (ppointerdef(def_from)^.pointertype.def^.deftype=orddef) and
                  (porddef(ppointerdef(def_from)^.pointertype.def)^.typ=uvoid) then
                begin
                   doconv:=tc_equal;
                   b:=1;
                end
               else
               { nil is compatible with procvars }
                if (fromtreetype=niln) then
                 begin
                   doconv:=tc_equal;
                   b:=1;
                 end;
             end;

           objectdef :
             begin
               { object pascal objects }
               if (def_from^.deftype=objectdef) {and
                  pobjectdef(def_from)^.isclass and pobjectdef(def_to)^.isclass }then
                begin
                  doconv:=tc_equal;
                  if pobjectdef(def_from)^.is_related(pobjectdef(def_to)) then
                   b:=1;
                end
               else
               { Class specific }
                if (pobjectdef(def_to)^.is_class) then
                 begin
                   { void pointer also for delphi mode }
                   if (m_delphi in aktmodeswitches) and
                      is_voidpointer(def_from) then
                    begin
                      doconv:=tc_equal;
                      b:=1;
                    end
                   else
                   { nil is compatible with class instances }
                    if (fromtreetype=niln) and (pobjectdef(def_to)^.is_class) then
                     begin
                       doconv:=tc_equal;
                       b:=1;
                     end;
                 end;
             end;

           classrefdef :
             begin
               { class reference types }
               if (def_from^.deftype=classrefdef) then
                begin
                  doconv:=tc_equal;
                  if pobjectdef(pclassrefdef(def_from)^.pointertype.def)^.is_related(
                       pobjectdef(pclassrefdef(def_to)^.pointertype.def)) then
                   b:=1;
                end
               else
                { nil is compatible with class references }
                if (fromtreetype=niln) then
                 begin
                   doconv:=tc_equal;
                   b:=1;
                 end;
             end;

           filedef :
             begin
               { typed files are all equal to the abstract file type
               name TYPEDFILE in system.pp in is_equal in types.pas
               the problem is that it sholud be also compatible to FILE
               but this would leed to a problem for ASSIGN RESET and REWRITE
               when trying to find the good overloaded function !!
               so all file function are doubled in system.pp
               this is not very beautiful !!}
               if (def_from^.deftype=filedef) and
                  (
                   (
                    (pfiledef(def_from)^.filetyp = ft_typed) and
                    (pfiledef(def_to)^.filetyp = ft_typed) and
                    (
                     (pfiledef(def_from)^.typedfiletype.def = pdef(voiddef)) or
                     (pfiledef(def_to)^.typedfiletype.def = pdef(voiddef))
                    )
                   ) or
                   (
                    (
                     (pfiledef(def_from)^.filetyp = ft_untyped) and
                     (pfiledef(def_to)^.filetyp = ft_typed)
                    ) or
                    (
                     (pfiledef(def_from)^.filetyp = ft_typed) and
                     (pfiledef(def_to)^.filetyp = ft_untyped)
                    )
                   )
                  ) then
                 begin
                    doconv:=tc_equal;
                    b:=1;
                 end
             end;

           else
             begin
             { assignment overwritten ?? }
               if assignment_overloaded(def_from,def_to)<>nil then
                b:=2;
             end;
         end;
        isconvertable:=b;
      end;

    { ld is the left type definition
      rd the right type definition
      dd the result type definition  or voiddef if unkown }
    function isbinaryoperatoroverloadable(ld, rd, dd : pdef;
             treetyp : ttreetyp) : boolean;
      begin
        isbinaryoperatoroverloadable:=
           (treetyp=starstarn) or
           (ld^.deftype=recorddef) or
           (rd^.deftype=recorddef) or
           ((rd^.deftype=pointerdef) and
            not(is_pchar(rd) and
                (is_chararray(ld) or
                 (ld^.deftype=stringdef) or
                 (treetyp=addn))) and
            (not(ld^.deftype in [pointerdef,objectdef,classrefdef,procvardef]) or
             not (treetyp in [equaln,unequaln,gtn,gten,ltn,lten,subn])
            ) and
            (not is_integer(ld) or not (treetyp in [addn,subn]))
           ) or
           ((ld^.deftype=pointerdef) and
            not(is_pchar(ld) and
                (is_chararray(rd) or
                 (rd^.deftype=stringdef) or
                 (treetyp=addn))) and
            (not(rd^.deftype in [stringdef,pointerdef,objectdef,classrefdef,procvardef]) and
             ((not is_integer(rd) and (rd^.deftype<>objectdef)
               and (rd^.deftype<>classrefdef)) or
              not (treetyp in [equaln,unequaln,gtn,gten,ltn,lten,addn,subn])
             )
            )
           ) or
           { array def, but not mmx or chararray+[char,string,chararray] }
           ((ld^.deftype=arraydef) and
            not((cs_mmx in aktlocalswitches) and
                is_mmx_able_array(ld)) and
            not(is_chararray(ld) and
                (is_char(rd) or
                is_pchar(rd) or
                 (rd^.deftype=stringdef) or
                 is_chararray(rd)))
           ) or
           ((rd^.deftype=arraydef) and
            not((cs_mmx in aktlocalswitches) and
                is_mmx_able_array(rd)) and
            not(is_chararray(rd) and
                (is_char(ld) or
                 is_pchar(ld) or
                 (ld^.deftype=stringdef) or
                 is_chararray(ld)))
           ) or
           { <> and = are defined for classes }
           ((ld^.deftype=objectdef) and
            (not(pobjectdef(ld)^.is_class) or
             not(treetyp in [equaln,unequaln])
            )
           ) or
           ((rd^.deftype=objectdef) and
            (not(pobjectdef(rd)^.is_class) or
             not(treetyp in [equaln,unequaln])
            )
             or
           { allow other operators that + on strings }
           (
            (is_char(rd) or
             is_pchar(rd) or
             (rd^.deftype=stringdef) or
             is_chararray(rd) or
             is_char(ld) or
             is_pchar(ld) or
             (ld^.deftype=stringdef) or
             is_chararray(ld)
             ) and
             not(treetyp in [addn,equaln,unequaln,gtn,gten,ltn,lten]) and
             not(is_pchar(ld) and
                 (is_integer(rd) or (rd^.deftype=pointerdef)) and
                 (treetyp=subn)
                )
            )
           );
      end;


    function isunaryoperatoroverloadable(rd,dd : pdef;
             treetyp : ttreetyp) : boolean;
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
              not is_integer(rd) and not (rd^.deftype=floatdef)
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

    function isoperatoracceptable(pf : pprocdef; optoken : ttoken) : boolean;
      var
        ld,rd,dd : pdef;
        i : longint;
      begin
        case pf^.parast^.symindex^.count of
          2 : begin
                isoperatoracceptable:=false;
                for i:=1 to tok2nodes do
                  if tok2node[i].tok=optoken then
                    begin
                      ld:=pvarsym(pf^.parast^.symindex^.first)^.vartype.def;
                      rd:=pvarsym(pf^.parast^.symindex^.first^.indexnext)^.vartype.def;
                      dd:=pf^.rettype.def;
                      isoperatoracceptable:=
                        tok2node[i].op_overloading_supported and
                        isbinaryoperatoroverloadable(ld,rd,dd,tok2node[i].nod);
                      break;
                    end;
              end;
          1 : begin
                rd:=pvarsym(pf^.parast^.symindex^.first)^.vartype.def;
                dd:=pf^.rettype.def;
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

{****************************************************************************
                          Register Calculation
****************************************************************************}

    { marks an lvalue as "unregable" }
    procedure make_not_regable(p : ptree);
      begin
         case p^.treetype of
            typeconvn :
              make_not_regable(p^.left);
            loadn :
              if p^.symtableentry^.typ=varsym then
                pvarsym(p^.symtableentry)^.varoptions:=pvarsym(p^.symtableentry)^.varoptions-[vo_regable,vo_fpuregable];
         end;
      end;


    procedure left_right_max(p : ptree);
      begin
        if assigned(p^.left) then
         begin
           if assigned(p^.right) then
            begin
              p^.registers32:=max(p^.left^.registers32,p^.right^.registers32);
              p^.registersfpu:=max(p^.left^.registersfpu,p^.right^.registersfpu);
{$ifdef SUPPORT_MMX}
              p^.registersmmx:=max(p^.left^.registersmmx,p^.right^.registersmmx);
{$endif SUPPORT_MMX}
            end
           else
            begin
              p^.registers32:=p^.left^.registers32;
              p^.registersfpu:=p^.left^.registersfpu;
{$ifdef SUPPORT_MMX}
              p^.registersmmx:=p^.left^.registersmmx;
{$endif SUPPORT_MMX}
            end;
         end;
      end;

    { calculates the needed registers for a binary operator }
    procedure calcregisters(p : ptree;r32,fpu,mmx : word);

      begin
         left_right_max(p);

      { Only when the difference between the left and right registers < the
        wanted registers allocate the amount of registers }

        if assigned(p^.left) then
         begin
           if assigned(p^.right) then
            begin
              if (abs(p^.left^.registers32-p^.right^.registers32)<r32) then
               inc(p^.registers32,r32);
              if (abs(p^.left^.registersfpu-p^.right^.registersfpu)<fpu) then
               inc(p^.registersfpu,fpu);
{$ifdef SUPPORT_MMX}
              if (abs(p^.left^.registersmmx-p^.right^.registersmmx)<mmx) then
               inc(p^.registersmmx,mmx);
{$endif SUPPORT_MMX}
              { the following is a little bit guessing but I think }
              { it's the only way to solve same internalerrors:    }
              { if the left and right node both uses registers     }
              { and return a mem location, but the current node    }
              { doesn't use an integer register we get probably    }
              { trouble when restoring a node                      }
              if (p^.left^.registers32=p^.right^.registers32) and
                 (p^.registers32=p^.left^.registers32) and
                 (p^.registers32>0) and
                (p^.left^.location.loc in [LOC_REFERENCE,LOC_MEM]) and
                (p^.right^.location.loc in [LOC_REFERENCE,LOC_MEM]) then
                inc(p^.registers32);
            end
           else
            begin
              if (p^.left^.registers32<r32) then
               inc(p^.registers32,r32);
              if (p^.left^.registersfpu<fpu) then
               inc(p^.registersfpu,fpu);
{$ifdef SUPPORT_MMX}
              if (p^.left^.registersmmx<mmx) then
               inc(p^.registersmmx,mmx);
{$endif SUPPORT_MMX}
            end;
         end;

         { error CGMessage, if more than 8 floating point }
         { registers are needed                         }
         if p^.registersfpu>8 then
          CGMessage(cg_e_too_complex_expr);
      end;

{****************************************************************************
                          Subroutine Handling
****************************************************************************}

{ protected field handling
  protected field can not appear in
  var parameters of function !!
  this can only be done after we have determined the
  overloaded function
  this is the reason why it is not in the parser, PM }

    procedure test_protected_sym(sym : psym);
      begin
         if (sp_protected in sym^.symoptions) and
            ((sym^.owner^.symtabletype=unitsymtable) or
             ((sym^.owner^.symtabletype=objectsymtable) and
             (pobjectdef(sym^.owner^.defowner)^.owner^.symtabletype=unitsymtable))
            ) then
          CGMessage(parser_e_cant_access_protected_member);
      end;


    procedure test_protected(p : ptree);
      begin
        case p^.treetype of
         loadn : test_protected_sym(p^.symtableentry);
     typeconvn : test_protected(p^.left);
        derefn : test_protected(p^.left);
    subscriptn : begin
                 { test_protected(p^.left);
                   Is a field of a protected var
                   also protected ???  PM }
                   test_protected_sym(p^.vs);
                 end;
        end;
      end;

   function  valid_for_formal_var(p : ptree) : boolean;
     var
        v : boolean;
     begin
        case p^.treetype of
         loadn :
           v:=(p^.symtableentry^.typ in [typedconstsym,varsym]);
         typeconvn :
           v:=valid_for_formal_var(p^.left);
         derefn,
         subscriptn,
         vecn,
         funcretn,
         selfn :
           v:=true;
         calln : { procvars are callnodes first }
           v:=assigned(p^.right) and not assigned(p^.left);
         addrn :
           begin
             { addrn is not allowed as this generate a constant value,
               but a tp procvar are allowed (PFV) }
             if p^.procvarload then
              v:=true
             else
              v:=false;
           end;
         else
           v:=false;
        end;
        valid_for_formal_var:=v;
     end;

   function  valid_for_formal_const(p : ptree) : boolean;
     var
        v : boolean;
     begin
        { p must have been firstpass'd before }
        { accept about anything but not a statement ! }
        case p^.treetype of
          calln,
          statementn,
          addrn :
           begin
             { addrn is not allowed as this generate a constant value,
               but a tp procvar are allowed (PFV) }
             if p^.procvarload then
              v:=true
             else
              v:=false;
           end;
          else
            v:=true;
        end;
        valid_for_formal_const:=v;
     end;

    function is_procsym_load(p:Ptree):boolean;
      begin
         is_procsym_load:=((p^.treetype=loadn) and (p^.symtableentry^.typ=procsym)) or
                          ((p^.treetype=addrn) and (p^.left^.treetype=loadn)
                          and (p^.left^.symtableentry^.typ=procsym)) ;
      end;

   { change a proc call to a procload for assignment to a procvar }
   { this can only happen for proc/function without arguments }
    function is_procsym_call(p:Ptree):boolean;
      begin
        is_procsym_call:=(p^.treetype=calln) and (p^.left=nil) and
             (((p^.symtableprocentry^.typ=procsym) and (p^.right=nil)) or
             ((p^.right<>nil) and (p^.right^.symtableprocentry^.typ=varsym)));
      end;


    function assignment_overloaded(from_def,to_def : pdef) : pprocdef;
       var
          passproc : pprocdef;
          convtyp : tconverttype;
       begin
          assignment_overloaded:=nil;
          if assigned(overloaded_operators[_assignment]) then
            passproc:=overloaded_operators[_assignment]^.definition
          else
            exit;
          while passproc<>nil do
            begin
              if is_equal(passproc^.rettype.def,to_def) and
                 (is_equal(pparaitem(passproc^.para^.first)^.paratype.def,from_def) or
                 (isconvertable(from_def,pparaitem(passproc^.para^.first)^.paratype.def,convtyp,ordconstn,false)=1)) then
                begin
                   assignment_overloaded:=passproc;
                   break;
                end;
              passproc:=passproc^.nextoverloaded;
            end;
       end;


    { local routines can't be assigned to procvars }
    procedure test_local_to_procvar(from_def:pprocvardef;to_def:pdef);
      begin
         if (from_def^.symtablelevel>1) and (to_def^.deftype=procvardef) then
           CGMessage(type_e_cannot_local_proc_to_procvar);
      end;


    function valid_for_assign(p:ptree;allowprop:boolean):boolean;
      var
        hp : ptree;
        gotwith,
        gotsubscript,
        gotpointer,
        gotclass,
        gotderef : boolean;
      begin
        valid_for_assign:=false;
        gotsubscript:=false;
        gotderef:=false;
        gotclass:=false;
        gotpointer:=false;
        gotwith:=false;
        hp:=p;
        while assigned(hp) do
         begin
           { property allowed? calln has a property check itself }
           if (not allowprop) and
              (hp^.isproperty) and
              (hp^.treetype<>calln) then
            begin
              CGMessagePos(hp^.fileinfo,type_e_argument_cant_be_assigned);
              exit;
            end;
           case hp^.treetype of
             derefn :
               begin
                 gotderef:=true;
                 hp:=hp^.left;
               end;
             typeconvn :
               begin
                 case hp^.resulttype^.deftype of
                   pointerdef :
                     gotpointer:=true;
                   objectdef :
                     gotclass:=pobjectdef(hp^.resulttype)^.is_class;
                   classrefdef :
                     gotclass:=true;
                   arraydef :
                     begin
                       { pointer -> array conversion is done then we need to see it
                         as a deref, because a ^ is then not required anymore }
                       if (hp^.left^.resulttype^.deftype=pointerdef) then
                        gotderef:=true;
                     end;
                 end;
                 hp:=hp^.left;
               end;
             vecn,
             asn :
               hp:=hp^.left;
             subscriptn :
               begin
                 gotsubscript:=true;
                 hp:=hp^.left;
               end;
             subn,
             addn :
               begin
                 { Allow add/sub operators on a pointer, or an integer
                   and a pointer typecast and deref has been found }
                 if (hp^.resulttype^.deftype=pointerdef) or
                    (is_integer(hp^.resulttype) and gotpointer and gotderef) then
                  valid_for_assign:=true
                 else
                  CGMessagePos(hp^.fileinfo,type_e_variable_id_expected);
                 exit;
               end;
             addrn :
               begin
                 if not(gotderef) and
                    not(hp^.procvarload) then
                  CGMessagePos(hp^.fileinfo,type_e_no_assign_to_addr);
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
                 case hp^.resulttype^.deftype of
                   pointerdef :
                     gotpointer:=true;
                   objectdef :
                     gotclass:=pobjectdef(hp^.resulttype)^.is_class;
                   recorddef, { handle record like class it needs a subscription }
                   classrefdef :
                     gotclass:=true;
                 end;
                 { 1. if it returns a pointer and we've found a deref,
                   2. if it returns a class or record and a subscription or with is found,
                   3. property is allowed }
                 if (gotpointer and gotderef) or
                    (gotclass and (gotsubscript or gotwith)) or
                    (hp^.isproperty and allowprop) then
                  valid_for_assign:=true
                 else
                  CGMessagePos(hp^.fileinfo,type_e_argument_cant_be_assigned);
                 exit;
               end;
             loadn :
               begin
                 case hp^.symtableentry^.typ of
                   absolutesym,
                   varsym :
                     begin
                       if (pvarsym(hp^.symtableentry)^.varspez=vs_const) then
                        begin
                          { allow p^:= constructions with p is const parameter }
                          if gotderef then
                           valid_for_assign:=true
                          else
                           CGMessagePos(hp^.fileinfo,type_e_no_assign_to_const);
                          exit;
                        end;
                       { Are we at a with symtable, then we need to process the
                         withrefnode also to check for maybe a const load }
                       if (hp^.symtable^.symtabletype=withsymtable) then
                        begin
                          { continue with processing the withref node }
                          hp:=ptree(pwithsymtable(hp^.symtable)^.withrefnode);
                          gotwith:=true;
                        end
                       else
                        begin
                          { set the assigned flag for varsyms }
                          if (pvarsym(hp^.symtableentry)^.varstate=vs_declared) then
                           pvarsym(hp^.symtableentry)^.varstate:=vs_assigned;
                          valid_for_assign:=true;
                          exit;
                        end;
                     end;
                   funcretsym,
                   typedconstsym :
                     begin
                       valid_for_assign:=true;
                       exit;
                     end;
                 end;
               end;
             else
               begin
                 CGMessagePos(hp^.fileinfo,type_e_variable_id_expected);
                 exit;
               end;
            end;
         end;
      end;

{$endif cg11}
end.
{
  $Log$
  Revision 1.9  2000-09-28 19:49:51  florian
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