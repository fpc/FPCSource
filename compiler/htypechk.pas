{
    $Id$
    Copyright (c) 1996-98 by Florian Klaempfl

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
interface

    uses
      tree,symtable;

    const
    { firstcallparan without varspez we don't count the ref }
       count_ref : boolean = true;
       get_para_resulttype : boolean = false;
       allow_array_constructor : boolean = false;


    { Conversion }
    function isconvertable(def_from,def_to : pdef;
             var doconv : tconverttype;fromtreetype : ttreetyp;
             explicit : boolean) : byte;

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


implementation

    uses
       globtype,systems,tokens,
       cobjects,verbose,globals,
       types,
       hcodegen;

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
           bbool,bbool,bbool,bint,bint);

        basedefconverts : array[tbasedef,tbasedef] of tconverttype =
         ((tc_not_possible,tc_not_possible,tc_not_possible,tc_not_possible),
          (tc_not_possible,tc_equal,tc_not_possible,tc_not_possible),
          (tc_not_possible,tc_not_possible,tc_int_2_int,tc_int_2_bool),
          (tc_not_possible,tc_not_possible,tc_bool_2_int,tc_bool_2_bool));

      var
         b : byte;
         hd1,hd2 : pdef;
      begin
       { safety check }
         if not(assigned(def_from) and assigned(def_to)) then
          begin
            isconvertable:=0;
            exit;
          end;

         b:=0;
       { we walk the wanted (def_to) types and check then the def_from
         types if there is a conversion possible }
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
{                 enumdef :
                   begin
                     doconv:=tc_int_2_int;
                     b:=1;
                   end;}
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
                   { pchar can be assigned to short/ansistrings }
                     if is_pchar(def_from) and not(m_tp in aktmodeswitches) then
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
                  if assigned(penumdef(def_from)^.basedef) then
                   hd1:=penumdef(def_from)^.basedef
                  else
                   hd1:=def_from;
                  if assigned(penumdef(def_to)^.basedef) then
                   hd2:=penumdef(def_to)^.basedef
                  else
                   hd2:=def_to;
                  if (hd1=hd2) then
                   b:=1;
                end;
             end;

           arraydef :
             begin
             { open array is also compatible with a single element of its base type }
               if is_open_array(def_to) and
                  is_equal(parraydef(def_to)^.definition,def_from) then
                begin
                  doconv:=tc_equal;
                  b:=1;
                end
               else
                begin
                  case def_from^.deftype of
                    pointerdef :
                      begin
                        if is_zero_based_array(def_to) and
                           is_equal(ppointerdef(def_from)^.definition,parraydef(def_to)^.definition) then
                         begin
                           doconv:=tc_pointer_2_array;
                           b:=1;
                         end;
                      end;
                    stringdef :
                      begin
                        { string to array of char}
                        if (not(is_special_array(def_to)) or is_open_array(def_to)) and
                          is_equal(parraydef(def_to)^.definition,cchardef) then
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
                     { string constant to zero terminated string constant }
                     if (fromtreetype=stringconstn) and
                        is_pchar(def_to) then
                      begin
                        doconv:=tc_cstring_2_pchar;
                        b:=1;
                      end;
                   end;
                 orddef :
                   begin
                     { char constant to zero terminated string constant }
                     if (fromtreetype=ordconstn) and is_equal(def_from,cchardef) and
                        is_pchar(def_to) then
                      begin
                        doconv:=tc_cchar_2_pchar;
                        b:=1;
                      end;
                   end;
                 arraydef :
                   begin
                     { chararray to pointer }
                     if is_zero_based_array(def_from) and
                        is_equal(parraydef(def_from)^.definition,ppointerdef(def_to)^.definition) then
                      begin
                        doconv:=tc_array_2_pointer;
                        b:=1;
                      end;
                   end;
                 pointerdef :
                   begin
                     { child class pointer can be assigned to anchestor pointers }
                     if (
                         (ppointerdef(def_from)^.definition^.deftype=objectdef) and
                         (ppointerdef(def_to)^.definition^.deftype=objectdef) and
                         pobjectdef(ppointerdef(def_from)^.definition)^.isrelated(
                           pobjectdef(ppointerdef(def_to)^.definition))
                        ) or
                        { all pointers can be assigned to void-pointer }
                        is_equal(ppointerdef(def_to)^.definition,voiddef) or
                        { in my opnion, is this not clean pascal }
                        { well, but it's handy to use, it isn't ? (FK) }
                        is_equal(ppointerdef(def_from)^.definition,voiddef) then
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
                        (ppointerdef(def_to)^.definition^.deftype=orddef) and
                        (porddef(ppointerdef(def_to)^.definition)^.typ=uvoid) then
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
                         ((def_from^.deftype=objectdef) and pobjectdef(def_from)^.isclass) or
                         (def_from^.deftype=classrefdef)
                        ) and
                        (ppointerdef(def_to)^.definition^.deftype=orddef) and
                        (porddef(ppointerdef(def_to)^.definition)^.typ=uvoid) then
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
                  (ppointerdef(def_from)^.definition^.deftype=orddef) and
                  (porddef(ppointerdef(def_from)^.definition)^.typ=uvoid) then
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
                  if pobjectdef(def_from)^.isrelated(pobjectdef(def_to)) then
                   b:=1;
                end
               else
                { nil is compatible with class instances }
                if (fromtreetype=niln) and (pobjectdef(def_to)^.isclass) then
                 begin
                   doconv:=tc_equal;
                   b:=1;
                 end;
             end;

           classrefdef :
             begin
               { class reference types }
               if (def_from^.deftype=classrefdef) then
                begin
                  doconv:=tc_equal;
                  if pobjectdef(pclassrefdef(def_from)^.definition)^.isrelated(
                       pobjectdef(pclassrefdef(def_to)^.definition)) then
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
                    (pfiledef(def_from)^.filetype = ft_typed) and
                    (pfiledef(def_to)^.filetype = ft_typed) and
                    (
                     (pfiledef(def_from)^.typed_as = pdef(voiddef)) or
                     (pfiledef(def_to)^.typed_as = pdef(voiddef))
                    )
                   ) or
                   (
                    (
                     (pfiledef(def_from)^.filetype = ft_untyped) and
                     (pfiledef(def_to)^.filetype = ft_typed)
                    ) or
                    (
                     (pfiledef(def_from)^.filetype = ft_typed) and
                     (pfiledef(def_to)^.filetype = ft_untyped)
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
               if assigned(assignment_overloaded(def_from,def_to)) then
                b:=2;
             end;
         end;
        isconvertable:=b;
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
                pvarsym(p^.symtableentry)^.var_options :=
                  pvarsym(p^.symtableentry)^.var_options and not vo_regable;
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
         if ((sym^.properties and sp_protected)<>0) and
           ((sym^.owner^.symtabletype=unitsymtable) or
            ((sym^.owner^.symtabletype=objectsymtable) and
           (pobjectdef(sym^.owner^.defowner)^.owner^.symtabletype=unitsymtable))) then
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
         loadn : v:=(p^.symtableentry^.typ in [typedconstsym,varsym]);
     typeconvn : v:=valid_for_formal_var(p^.left);
         typen : v:=false;
     derefn,subscriptn,vecn,
     funcretn,selfn : v:=true;
        { procvars are callnodes first }
         calln : v:=assigned(p^.right) and not assigned(p^.left);
        { should this depend on mode ? }
         addrn : v:=true;
        { no other node accepted (PM) }
        else v:=false;
        end;
        valid_for_formal_var:=v;
     end;

   function  valid_for_formal_const(p : ptree) : boolean;
     var
        v : boolean;
     begin
        { p must have been firstpass'd before }
        { accept about anything but not a statement ! }
        v:=true;
        if (p^.treetype in [calln,statementn]) then
      {  if not assigned(p^.resulttype) or (p^.resulttype=pdef(voiddef)) then }
          v:=false;
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
          if assigned(overloaded_operators[assignment]) then
            passproc:=overloaded_operators[assignment]^.definition
          else
            exit;
          while passproc<>nil do
            begin
              if is_equal(passproc^.retdef,to_def) and
                 (is_equal(passproc^.para1^.data,from_def) or
                 (isconvertable(from_def,passproc^.para1^.data,convtyp,ordconstn,false)=1)) then
                begin
                   assignment_overloaded:=passproc;
                   break;
                end;
              passproc:=passproc^.nextoverloaded;
            end;
       end;

end.
{
  $Log$
  Revision 1.29  1999-06-18 11:02:51  daniel
  - Enumerations no longer compatible with integer types.

  Revision 1.28  1999/06/17 13:19:51  pierre
   * merged from 0_99_12 branch

  Revision 1.27.2.1  1999/06/17 12:51:42  pierre
   * changed is_assignment_overloaded into
      function assignment_overloaded : pprocdef
      to allow overloading of assignment with only different result type

  Revision 1.27  1999/06/01 19:27:47  peter
    * better checks for procvar and methodpointer

  Revision 1.26  1999/05/20 14:58:26  peter
    * fixed arrayconstruct->set conversion which didn't work for enum sets

  Revision 1.25  1999/05/19 20:40:12  florian
    * fixed a couple of array related bugs:
      - var a : array[0..1] of char;   p : pchar;  p:=a+123; works now
      - open arrays with an odd size doesn't work: movsb wasn't generated
      - introduced some new array type helper routines (is_special_array) etc.
      - made the array type checking in isconvertable more strict, often
        open array can be used where is wasn't allowed etc...

  Revision 1.24  1999/05/06 10:10:02  peter
    * overloaded conversion has lower priority

  Revision 1.23  1999/04/26 09:30:47  peter
    * small tp7 fix
    * fix void pointer with formaldef

  Revision 1.22  1999/04/21 22:00:01  pierre
    + valid_for_formal_var and valid_for_formal_const added

  Revision 1.21  1999/04/21 16:31:40  pierre
  ra386att.pas : problem with commit -m !

  Revision 1.20  1999/04/15 08:56:27  peter
    * fixed bool-bool conversion

  Revision 1.19  1999/03/24 23:17:02  peter
    * fixed bugs 212,222,225,227,229,231,233

  Revision 1.18  1999/03/06 17:25:19  peter
    * moved comp<->real warning so it doesn't occure everytime that
      isconvertable is called with

  Revision 1.17  1999/03/02 18:24:20  peter
    * fixed overloading of array of char

  Revision 1.16  1999/01/27 13:53:27  pierre
  htypechk.pas

  Revision 1.15  1999/01/27 13:12:10  pierre
   * bool to int must be explicit

  Revision 1.14  1999/01/19 15:55:32  pierre
   * fix for boolean to comp conversion (now disabled)

  Revision 1.13  1998/12/15 17:11:37  peter
    * string:=pchar not allowed in tp mode

  Revision 1.12  1998/12/11 00:03:18  peter
    + globtype,tokens,version unit splitted from globals

  Revision 1.11  1998/12/10 09:47:21  florian
    + basic operations with int64/qord (compiler with -dint64)
    + rtti of enumerations extended: names are now written

  Revision 1.10  1998/11/29 12:40:23  peter
    * newcnv -> not oldcnv

  Revision 1.9  1998/11/26 13:10:42  peter
    * new int - int conversion -dNEWCNV
    * some function renamings

  Revision 1.8  1998/11/17 00:36:42  peter
    * more ansistring fixes

  Revision 1.7  1998/10/14 13:33:24  peter
    * fixed small typo

  Revision 1.6  1998/10/14 12:53:38  peter
    * fixed small tp7 things
    * boolean:=longbool and longbool fixed

  Revision 1.5  1998/10/12 09:49:58  florian
    + support of <procedure var type>:=<pointer> in delphi mode added

  Revision 1.4  1998/09/30 16:42:52  peter
    * fixed bool-bool cnv

  Revision 1.3  1998/09/24 23:49:05  peter
    + aktmodeswitches

  Revision 1.2  1998/09/24 09:02:14  peter
    * rewritten isconvertable to use case
    * array of .. and single variable are compatible

  Revision 1.1  1998/09/23 20:42:22  peter
    * splitted pass_1

}
