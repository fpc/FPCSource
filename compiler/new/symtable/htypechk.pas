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

{The isconvertable can better be handled inside the symtable, this
 would result is much better maintenance possibilities.}

interface

    uses
      tree,symtable,defs,symbols;

    const
    { firstcallparan without varspez we don't count the ref }
{$ifdef extdebug}
       count_ref : boolean = true;
{$endif def extdebug}
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
(*  procedure test_protected_sym(sym : psym);
    procedure test_protected(p : ptree);*)
    function  valid_for_formal_var(p : ptree) : boolean;
    function  valid_for_formal_const(p : ptree) : boolean;
    function  is_procsym_load(p:Ptree):boolean;
    function  is_procsym_call(p:Ptree):boolean;
    function  assignment_overloaded(from_def,to_def : pdef) : pprocdef;
    procedure test_local_to_procvar(from_def:pprocvardef;to_def:pdef);
    function  valid_for_assign(p:ptree;allowprop:boolean):boolean;


implementation

    uses
       globtype,systems,tokens,
       cobjects,verbose,globals,
       types,pass_1,cpubase,symtablt,
{$ifdef newcg}
       cgbase
{$else}
       hcodegen
{$endif}
       ;

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
           bbool,bbool,bbool,bint,bint,bint,bchar);

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
    {!!!! This code should never be called with nil parameters. If you really
     want to check this, make it an internalerror instead of an exit!! (DM)
        if not(assigned(def_from) and assigned(def_to)) then
          begin
            isconvertable:=0;
            exit;
          end;}

       { tp7 procvar def support, in tp7 a procvar is always called, if the
         procvar is passed explicit a addrn would be there }
         if (m_tp_procvar in aktmodeswitches) and
            (def_from^.is_object(typeof(Tprocvardef))) and
            (fromtreetype=loadn) then
          begin
            def_from:=pprocvardef(def_from)^.retdef;
          end;

       { we walk the wanted (def_to) types and check then the def_from
         types if there is a conversion possible }
         b:=0;
         if def_to^.is_object(typeof(Torddef)) then
            begin
              if def_from^.is_object(typeof(Torddef)) then
                 begin
                   doconv:=basedefconverts[basedeftbl[Tbasetype(porddef(def_from)^.typ)],basedeftbl[porddef(def_to)^.typ]];
                   b:=1;
                   if (doconv=tc_not_possible) or
                      ((doconv=tc_int_2_bool) and
                       (not explicit) and
                       (not is_boolean(def_from))) or
                      ((doconv=tc_bool_2_int) and
                       (not explicit) and
                       (not is_boolean(def_to))) then
                     b:=0;
                 end
              else if def_from^.is_object(typeof(Tenumdef)) then
                 begin
                   { needed for char(enum) }
                   if explicit then
                    begin
                      doconv:=tc_int_2_int;
                      b:=1;
                    end;
                 end;
            end
         else if def_to^.is_object(typeof(Tstringdef)) then
             begin
               if def_from^.is_object(typeof(Tstringdef)) then
                   begin
                     doconv:=tc_string_2_string;
                     b:=1;
                   end
               else if def_from^.is_object(typeof(Torddef)) then
                   begin
                   { char to string}
                     if is_char(def_from) then
                      begin
                        doconv:=tc_char_2_string;
                        b:=1;
                      end;
                   end
               else if def_from^.is_object(typeof(Tarraydef)) then
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
                   end
               else if def_from^.is_object(typeof(Tpointerdef)) then
                   begin
                   { pchar can be assigned to short/ansistrings }
                     if is_pchar(def_from) and not(m_tp in aktmodeswitches) then
                      begin
                        doconv:=tc_pchar_2_string;
                        b:=1;
                      end;
                   end;
             end
         else if def_to^.is_object(typeof(Tfloatdef)) then
             begin
               if def_from^.is_object(typeof(Torddef)) then
                   begin { ordinal to real }
                     if is_integer(def_from) then
                       begin
                          if pfloatdef(def_to)^.typ=f32bit then
                            doconv:=tc_int_2_fix
                          else
                            doconv:=tc_int_2_real;
                          b:=1;
                       end;
                   end
               else if def_from^.is_object(typeof(Tfloatdef)) then
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
             end
         else if def_to^.is_object(typeof(Tenumdef)) then
             begin
               if def_from^.is_object(typeof(Tenumdef)) then
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
             end
         else if def_to^.is_object(typeof(Tarraydef)) then
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
                  if def_from^.is_object(typeof(Tarraydef)) then
                      begin
                        { array constructor -> open array }
                        if is_open_array(def_to) and
                           is_array_constructor(def_from) then
                         begin
                           if is_void(parraydef(def_from)^.definition) or
                              is_equal(parraydef(def_to)^.definition,parraydef(def_from)^.definition) then
                            begin
                              doconv:=tc_equal;
                              b:=1;
                            end
                           else
                            if isconvertable(parraydef(def_to)^.definition,
                                             parraydef(def_from)^.definition,hct,nothingn,false)<>0 then
                             begin
                               doconv:=hct;
                               b:=2;
                             end;
                         end;
                      end
                  else if def_from^.is_object(typeof(Tpointerdef)) then
                      begin
                        if is_zero_based_array(def_to) and
                           is_equal(ppointerdef(def_from)^.definition,parraydef(def_to)^.definition) then
                         begin
                           doconv:=tc_pointer_2_array;
                           b:=1;
                         end;
                      end
                  else if def_from^.is_object(typeof(Tstringdef)) then
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
             end
         else if def_to^.is_object(typeof(Tpointerdef)) then
             begin
               if def_from^.is_object(typeof(Tstringdef)) then
                   begin
                     { string constant to zero terminated string constant }
                     if (fromtreetype=stringconstn) and
                        is_pchar(def_to) then
                      begin
                        doconv:=tc_cstring_2_pchar;
                        b:=1;
                      end;
                   end
               else if def_from^.is_object(typeof(Torddef)) then
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
                   end
               else if def_from^.is_object(typeof(Tarraydef)) then
                   begin
                     { chararray to pointer }
                     if is_zero_based_array(def_from) and
                        is_equal(parraydef(def_from)^.definition,ppointerdef(def_to)^.definition) then
                      begin
                        doconv:=tc_array_2_pointer;
                        b:=1;
                      end;
                   end
               else if def_from^.is_object(typeof(Tpointerdef)) then
                   begin
                     { child class pointer can be assigned to anchestor pointers }
                     if (
                         (Ppointerdef(def_from)^.definition^.is_object(typeof(Tobjectdef))) and
                         (Ppointerdef(def_to)^.definition^.is_object(typeof(Tobjectdef))) and
                         pobjectdef(ppointerdef(def_from)^.definition)^.is_related(
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
                   end
               else if def_from^.is_object(typeof(Tprocvardef)) then
                   begin
                     { procedure variable can be assigned to an void pointer }
                     { Not anymore. Use the @ operator now.}
                     if not(m_tp_procvar in aktmodeswitches) and
                        (typeof((Ppointerdef(def_to)^.definition^))=typeof(Torddef)) and
                        (porddef(ppointerdef(def_to)^.definition)^.typ=uvoid) then
                      begin
                        doconv:=tc_equal;
                        b:=1;
                      end;
                   end
               else if def_from^.is_object(typeof(Tclassrefdef)) or
                def_from^.is_object(typeof(Tobjectdef)) then
                   begin
                     { class types and class reference type
                       can be assigned to void pointers      }
                     if (
                         (def_from^.is_object(typeof(Tobjectdef)) and
                         (oo_is_class in pobjectdef(def_from)^.options))) or
                         (def_from^.is_object(typeof(Tclassrefdef))
                        ) and
                         ppointerdef(def_to)^.definition^.is_object(typeof(Torddef)) and
                        (porddef(ppointerdef(def_to)^.definition)^.typ=uvoid) then
                       begin
                         doconv:=tc_equal;
                         b:=1;
                       end;
                   end;
             end
         else if def_to^.is_object(typeof(Tsetdef)) then
             begin
               { automatic arrayconstructor -> set conversion }
               if is_array_constructor(def_from) then
                begin
                  doconv:=tc_arrayconstructor_2_set;
                  b:=1;
                end;
             end
         else if def_to^.is_object(typeof(Tprocvardef)) then
             begin
               { proc -> procvar }
               if def_from^.is_object(typeof(Tprocdef)) then
                begin
                  doconv:=tc_proc_2_procvar;
                  if proc_to_procvar_equal(pprocdef(def_from),pprocvardef(def_to)) then
                   b:=1;
                end
               else
                { for example delphi allows the assignement from pointers }
                { to procedure variables                                  }
                if (m_pointer_2_procedure in aktmodeswitches) and
                  def_from^.is_object(typeof(Tpointerdef)) and
                  ppointerdef(def_from)^.definition^.is_object(typeof(Torddef)) and
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
             end
         else if def_to^.is_object(typeof(Tobjectdef)) then
             begin
               { object pascal objects }
               if def_from^.is_object(typeof(Tobjectdef)) then
                begin
                  doconv:=tc_equal;
                  if pobjectdef(def_from)^.is_related(pobjectdef(def_to)) then
                   b:=1;
                end
               else
               { Class specific }
                if (oo_is_class in pobjectdef(def_to)^.options) then
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
                    if (fromtreetype=niln) and (oo_is_class in pobjectdef(def_to)^.options) then
                     begin
                       doconv:=tc_equal;
                       b:=1;
                     end;
                 end;
             end
         else if def_to^.is_object(typeof(Tclassrefdef)) then
             begin
               { class reference types }
               if def_from^.is_object(typeof(Tclassrefdef)) then
                begin
                  doconv:=tc_equal;
                  if pobjectdef(pclassrefdef(def_from)^.definition)^.is_related(
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
             end
         else if def_to^.is_object(typeof(Tfiledef)) then
             begin
               { typed files are all equal to the abstract file type
               name TYPEDFILE in system.pp in is_equal in types.pas
               the problem is that it sholud be also compatible to FILE
               but this would leed to a problem for ASSIGN RESET and REWRITE
               when trying to find the good overloaded function !!
               so all file function are doubled in system.pp
               this is not very beautiful !!}
               if (typeof(def_from^)=typeof(Tfiledef)) and
                  (
                   (
                    (pfiledef(def_from)^.filetype = ft_typed) and
                    (pfiledef(def_to)^.filetype = ft_typed) and
                    (
                     (pfiledef(def_from)^.definition=pdef(voiddef)) or
                     (pfiledef(def_to)^.definition=pdef(voiddef))
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
             end

           else
             begin
             { assignment overwritten ?? }
               if assignment_overloaded(def_from,def_to)<>nil then
                b:=2;
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
              if typeof(p^.symtableentry^)=typeof(Tvarsym) then
                pvarsym(p^.symtableentry)^.properties:=
                 pvarsym(p^.symtableentry)^.properties-[vo_regable,vo_fpuregable];
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

(*  procedure test_protected_sym(sym : Pprocsym);
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
      end;*)

   function  valid_for_formal_var(p : ptree) : boolean;
     var
        v : boolean;
     begin
        case p^.treetype of
         loadn : v:=(typeof(p^.symtableentry^)=typeof(Ttypedconstsym)) or
                      (typeof(p^.symtableentry^)=typeof(Tvarsym));
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
         is_procsym_load:=((p^.treetype=loadn) and (typeof(p^.symtableentry^)=typeof(Tprocsym)) or
                          ((p^.treetype=addrn) and (p^.left^.treetype=loadn)
                          and (typeof(p^.left^.symtableentry^)=typeof(Tprocsym))));
      end;

   { change a proc call to a procload for assignment to a procvar }
   { this can only happen for proc/function without arguments }
    function is_procsym_call(p:Ptree):boolean;
      begin
        is_procsym_call:=(p^.treetype=calln) and (p^.left=nil) and
             (((typeof(p^.symtableprocentry^)=typeof(Tprocsym)) and (p^.right=nil)) or
             ((p^.right<>nil) and (typeof(p^.right^.symtableprocentry^)=typeof(Tvarsym))));
      end;


    function assignment_overloaded(from_def,to_def : pdef) : pprocdef;

        function matches(item:pointer):boolean;{$IFDEF TP}far;{$ENDIF TP}

        var first_param_def:Pdef;
            convtyp:Tconverttype;

        begin
            {The right assignment overload had been found when:
             - The retdef of item equals the to_def.
             - The definition of the first parameter equals the from_def
               or it can be converted to from_def.}
            first_param_def:=Pparamsym(Pparameter(Pprocdef(item)^.
             parameters^.at(0))^.data)^.definition;
            if is_equal(Pprocdef(item)^.retdef,to_def) and
             (is_equal(first_param_def,from_def) or
             (isconvertable(from_def,first_param_def,
              convtyp,ordconstn,false)=1)) then
                matches:=true;
        end;

    var passproc:Pprocdef;

    begin
        assignment_overloaded:=nil;
        if overloaded_operators[_assignment]<>nil then
            assignment_overloaded:=overloaded_operators[_assignment]^.
             firstthat(@matches);
    end;


    { local routines can't be assigned to procvars }
    procedure test_local_to_procvar(from_def:pprocvardef;to_def:pdef);

    begin
        if (typeof(from_def^.owner^)=typeof(Tprocsymtable)) and
         (typeof(to_def^)=typeof(Tprocvardef)) then
            CGMessage(type_e_cannot_local_proc_to_procvar);
    end;


    function valid_for_assign(p:ptree;allowprop:boolean):boolean;
      var
        hp : ptree;
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
                    if typeof(hp^.resulttype^)=typeof(Tpointerdef) then
                        gotpointer:=true
                    else if typeof(hp^.resulttype^)=typeof(Tobjectdef) then
                        gotclass:=oo_is_class in Pobjectdef(hp^.resulttype)^.options
                    else if typeof(hp^.resulttype^)=typeof(Tclassrefdef) then
                        gotclass:=true
                    else if (typeof(hp^.resulttype^)=typeof(Tarraydef)) and
                     (typeof(hp^.left^.resulttype^)=typeof(Tpointerdef)) then
                        gotderef:=true;
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
                 if (typeof(hp^.resulttype^)=typeof(Tpointerdef)) or
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
                    if typeof(hp^.resulttype^)=typeof(Tpointerdef) then
                        gotpointer:=true
                    else if typeof(hp^.resulttype^)=typeof(Tobjectdef) then
                        gotclass:=oo_is_class in Pobjectdef(hp^.resulttype)^.options
                    else if typeof(hp^.resulttype^)=typeof(Tclassrefdef) then
                        gotclass:=true;

                    { 1. if it returns a pointer and we've found a deref,
                      2. if it returns a class and a subscription is found,
                      3. property is allowed }
                    if (gotpointer and gotderef) or
                     (gotclass and gotsubscript) or
                     (hp^.isproperty and allowprop) then
                        valid_for_assign:=true
                    else
                        CGMessagePos(hp^.fileinfo,type_e_argument_cant_be_assigned);
                    exit;
               end;
             loadn :
               begin
                 if (typeof(hp^.symtableentry^)=typeof(Tabsolutesym)) or
                  (typeof(hp^.symtableentry^)=typeof(Tparamsym)) or
                  (typeof(hp^.symtableentry^)=typeof(Tvarsym)) then
                     begin
                       if (typeof(hp^.symtableentry^)=typeof(Tparamsym)) and
                        (Pparamsym(hp^.symtableentry)^.varspez=vs_const) then
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
                       if typeof(hp^.symtable^)=typeof(Twithsymtable) then
                        begin
                          { continue with processing the withref node }
                          hp:=ptree(pwithsymtable(hp^.symtable)^.withrefnode);
                        end
                       else
                        begin
                          { set the assigned flag for varsyms }
                          if (pvarsym(hp^.symtableentry)^.state=vs_declared) then
                           pvarsym(hp^.symtableentry)^.state:=vs_assigned;
                          valid_for_assign:=true;
                          exit;
                        end;
                     end;
                 if (typeof(hp^.symtableentry^)=typeof(Tfuncretsym)) or
                  (typeof(hp^.symtableentry^)=typeof(Ttypedconstsym)) then
                     begin
                       valid_for_assign:=true;
                       exit;
                     end;
               end;
             else
                 CGMessagePos(hp^.fileinfo,type_e_variable_id_expected);
                 exit;
            end;
         end;
      end;

end.
{
  $Log$
  Revision 1.2  2000-03-11 21:11:25  daniel
    * Ported hcgdata to new symtable.
    * Alignment code changed as suggested by Peter
    + Usage of my is operator replacement, is_object

  Revision 1.1  2000/02/28 17:23:58  daniel
  * Current work of symtable integration committed. The symtable can be
    activated by defining 'newst', but doesn't compile yet. Changes in type
    checking and oop are completed. What is left is to write a new
    symtablestack and adapt the parser to use it.

  Revision 1.59  2000/02/18 16:13:29  florian
    * optimized ansistring compare with ''
    * fixed 852

  Revision 1.58  2000/02/09 13:22:53  peter
    * log truncated

  Revision 1.57  2000/02/05 12:11:50  peter
    * property check for assigning fixed for calln

  Revision 1.56  2000/02/01 09:41:27  peter
    * allow class -> voidpointer for delphi mode

  Revision 1.55  2000/01/07 01:14:27  peter
    * updated copyright to 2000

  Revision 1.54  1999/12/31 14:26:27  peter
    * fixed crash with empty array constructors

  Revision 1.53  1999/12/18 14:55:21  florian
    * very basic widestring support

  Revision 1.52  1999/12/16 19:12:04  peter
    * allow constant pointer^ also for assignment

  Revision 1.51  1999/12/09 09:35:54  peter
    * allow assigning to self

  Revision 1.50  1999/11/30 10:40:43  peter
    + ttype, tsymlist

  Revision 1.49  1999/11/18 15:34:45  pierre
    * Notes/Hints for local syms changed to
      Set_varstate function

  Revision 1.48  1999/11/09 14:47:03  peter
    * pointer->array is allowed for all pointer types in FPC, fixed assign
      check for it.

  Revision 1.47  1999/11/09 13:29:33  peter
    * valid_for_assign allow properties with calln

  Revision 1.46  1999/11/08 22:45:33  peter
    * allow typecasting to integer within pointer typecast+deref

  Revision 1.45  1999/11/06 14:34:21  peter
    * truncated log to 20 revs

  Revision 1.44  1999/11/04 23:11:21  peter
    * fixed pchar and deref detection for assigning

  Revision 1.43  1999/10/27 16:04:45  peter
    * valid_for_assign support for calln,asn

  Revision 1.42  1999/10/26 12:30:41  peter
    * const parameter is now checked
    * better and generic check if a node can be used for assigning
    * export fixes
    * procvar equal works now (it never had worked at least from 0.99.8)
    * defcoll changed to linkedlist with pparaitem so it can easily be
      walked both directions

  Revision 1.41  1999/10/14 14:57:52  florian
    - removed the hcodegen use in the new cg, use cgbase instead

  Revision 1.40  1999/09/26 21:30:15  peter
    + constant pointer support which can happend with typecasting like
      const p=pointer(1)
    * better procvar parsing in typed consts

  Revision 1.39  1999/09/17 17:14:04  peter
    * @procvar fixes for tp mode
    * @<id>:= gives now an error

  Revision 1.38  1999/08/17 13:26:07  peter
    * arrayconstructor -> arrayofconst fixed when arraycosntructor was not
      variant.

}