{
    $Id$
    Copyright (c) 1996-98 by Florian Klaempfl

    This unit implements the first pass of the code generator

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

{$ifdef tp}
  {$F+}
{$endif tp}
unit pass_1;

  interface

    uses tree;

    procedure firstpass(var p : ptree);
    function  do_firstpass(var p : ptree) : boolean;


  implementation

     uses
        cobjects,verbose,comphook,systems,globals,
        aasm,symtable,types,strings,hcodegen,files
{$ifdef i386}
        ,i386
        ,tgeni386
{$endif}
{$ifdef m68k}
        ,m68k
        ,tgen68k
{$endif}
{$ifdef UseBrowser}
        ,browser
{$endif UseBrowser}
        ;

    { firstcallparan without varspez
      we don't count the ref }
    const
       count_ref : boolean = true;


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

         { error message, if more than 8 floating point }
         { registers are needed                         }
         if p^.registersfpu>8 then
          Message(cg_e_too_complex_expr);
      end;


    function both_rm(p : ptree) : boolean;
        begin
           both_rm:=(p^.left^.location.loc in [LOC_MEM,LOC_REFERENCE]) and
                    (p^.right^.location.loc in [LOC_MEM,LOC_REFERENCE]);
        end;


    function is_assignment_overloaded(from_def,to_def : pdef) : boolean;forward;


    function isconvertable(def_from,def_to : pdef;
             var doconv : tconverttype;fromtreetype : ttreetyp;
             explicit : boolean) : boolean;
      const
      { Tbasetype:  uauto,uvoid,uchar,
                    u8bit,u16bit,u32bit,
                    s8bit,s16bit,s32,
                    bool8bit,bool16bit,boot32bit }
         basedefconverts : array[tbasetype,tbasetype] of tconverttype =
           {uauto}
           ((tc_not_possible,tc_not_possible,tc_not_possible,
             tc_not_possible,tc_not_possible,tc_not_possible,
             tc_not_possible,tc_not_possible,tc_not_possible,
             tc_not_possible,tc_not_possible,tc_not_possible),
           {uvoid}
            (tc_not_possible,tc_not_possible,tc_not_possible,
             tc_not_possible,tc_not_possible,tc_not_possible,
             tc_not_possible,tc_not_possible,tc_not_possible,
             tc_not_possible,tc_not_possible,tc_not_possible),
           {uchar}
            (tc_not_possible,tc_not_possible,tc_only_rangechecks32bit,
             tc_not_possible,tc_not_possible,tc_not_possible,
             tc_not_possible,tc_not_possible,tc_not_possible,
             tc_not_possible,tc_not_possible,tc_not_possible),
           {u8bit}
            (tc_not_possible,tc_not_possible,tc_not_possible,
             tc_only_rangechecks32bit,tc_u8bit_2_u16bit,tc_u8bit_2_u32bit,
             tc_only_rangechecks32bit,tc_u8bit_2_s16bit,tc_u8bit_2_s32bit,
             tc_int_2_bool,tc_int_2_bool,tc_int_2_bool),
           {u16bit}
            (tc_not_possible,tc_not_possible,tc_not_possible,
             tc_u16bit_2_u8bit,tc_only_rangechecks32bit,tc_u16bit_2_u32bit,
             tc_u16bit_2_s8bit,tc_only_rangechecks32bit,tc_u16bit_2_s32bit,
             tc_int_2_bool,tc_int_2_bool,tc_int_2_bool),
           {u32bit}
            (tc_not_possible,tc_not_possible,tc_not_possible,
             tc_u32bit_2_u8bit,tc_u32bit_2_u16bit,tc_only_rangechecks32bit,
             tc_u32bit_2_s8bit,tc_u32bit_2_s16bit,tc_only_rangechecks32bit,
             tc_int_2_bool,tc_int_2_bool,tc_int_2_bool),
           {s8bit}
            (tc_not_possible,tc_not_possible,tc_not_possible,
             tc_only_rangechecks32bit,tc_s8bit_2_u16bit,tc_s8bit_2_u32bit,
             tc_only_rangechecks32bit,tc_s8bit_2_s16bit,tc_s8bit_2_s32bit,
             tc_int_2_bool,tc_int_2_bool,tc_int_2_bool),
           {s16bit}
            (tc_not_possible,tc_not_possible,tc_not_possible,
             tc_s16bit_2_u8bit,tc_only_rangechecks32bit,tc_s16bit_2_u32bit,
             tc_s16bit_2_s8bit,tc_only_rangechecks32bit,tc_s16bit_2_s32bit,
             tc_int_2_bool,tc_int_2_bool,tc_int_2_bool),
           {s32bit}
            (tc_not_possible,tc_not_possible,tc_not_possible,
             tc_s32bit_2_u8bit,tc_s32bit_2_u16bit,tc_only_rangechecks32bit,
             tc_s32bit_2_s8bit,tc_s32bit_2_s16bit,tc_only_rangechecks32bit,
             tc_int_2_bool,tc_int_2_bool,tc_int_2_bool),
           {bool8bit}
            (tc_not_possible,tc_not_possible,tc_not_possible,
             tc_bool_2_int,tc_bool_2_int,tc_bool_2_int,
             tc_bool_2_int,tc_bool_2_int,tc_bool_2_int,
             tc_only_rangechecks32bit,tc_bool_2_int,tc_bool_2_int),
           {bool16bit}
            (tc_not_possible,tc_not_possible,tc_not_possible,
             tc_bool_2_int,tc_bool_2_int,tc_bool_2_int,
             tc_bool_2_int,tc_bool_2_int,tc_bool_2_int,
             tc_bool_2_int,tc_only_rangechecks32bit,tc_bool_2_int),
           {bool32bit}
            (tc_not_possible,tc_not_possible,tc_not_possible,
             tc_bool_2_int,tc_bool_2_int,tc_bool_2_int,
             tc_bool_2_int,tc_bool_2_int,tc_bool_2_int,
             tc_bool_2_int,tc_bool_2_int,tc_only_rangechecks32bit));

      var
         b : boolean;
         hd1,hd2 : pdef;
      begin
         b:=false;
         if (not assigned(def_from)) or (not assigned(def_to)) then
          begin
            isconvertable:=false;
            exit;
          end;

        { handle ord to ord first }
         if (def_from^.deftype=orddef) and (def_to^.deftype=orddef) then
           begin
              doconv:=basedefconverts[porddef(def_from)^.typ,porddef(def_to)^.typ];
            { Don't allow automatic int->bool.
              Very Bad Hack !!!! (PFV) }
              if (doconv=tc_int_2_bool) and (not explicit) then
               b:=false
              else
               if doconv<>tc_not_possible then
                 b:=true;
           end
         else

          if (def_from^.deftype=orddef) and (def_to^.deftype=floatdef) then
           begin
              if pfloatdef(def_to)^.typ=f32bit then
                doconv:=tc_int_2_fix
              else
                doconv:=tc_int_2_real;
              b:=true;
           end
         else

         { 2 float types ? }
          if (def_from^.deftype=floatdef) and (def_to^.deftype=floatdef) then
           begin
              if pfloatdef(def_from)^.typ=pfloatdef(def_to)^.typ then
                doconv:=tc_equal
              else
                begin
                   if pfloatdef(def_from)^.typ=f32bit then
                     doconv:=tc_fix_2_real
                   else if pfloatdef(def_to)^.typ=f32bit then
                     doconv:=tc_real_2_fix
                   else
                     doconv:=tc_real_2_real;
                   { comp isn't a floating type }
{$ifdef i386}
                   if (pfloatdef(def_to)^.typ=s64bit) and
                      (pfloatdef(def_from)^.typ<>s64bit)  and
                      not (explicit) then
                     Message(type_w_convert_real_2_comp);
{$endif}
                end;
              b:=true;
           end
         else

         { enum to enum }
          if (def_from^.deftype=enumdef) and (def_to^.deftype=enumdef) then
           begin
             if assigned(penumdef(def_from)^.basedef) then
              hd1:=penumdef(def_from)^.basedef
             else
              hd1:=def_from;
             if assigned(penumdef(def_to)^.basedef) then
              hd2:=penumdef(def_to)^.basedef
             else
              hd2:=def_to;
             b:=(hd1=hd2);
           end
         else

         { assignment overwritten ?? }
          if is_assignment_overloaded(def_from,def_to) then
           b:=true
         else

          if (def_from^.deftype=pointerdef) and (def_to^.deftype=arraydef) and
             (parraydef(def_to)^.lowrange=0) and
             is_equal(ppointerdef(def_from)^.definition,parraydef(def_to)^.definition) then
           begin
              doconv:=tc_pointer_to_array;
              b:=true;
           end
         else

          if (def_from^.deftype=arraydef) and (def_to^.deftype=pointerdef) and
             (parraydef(def_from)^.lowrange=0) and
             is_equal(parraydef(def_from)^.definition,ppointerdef(def_to)^.definition) then
           begin
              doconv:=tc_array_to_pointer;
              b:=true;
           end
         else

         { typed files are all equal to the abstract file type
         name TYPEDFILE in system.pp in is_equal in types.pas
         the problem is that it sholud be also compatible to FILE
         but this would leed to a problem for ASSIGN RESET and REWRITE
         when trying to find the good overloaded function !!
         so all file function are doubled in system.pp
         this is not very beautiful !!}
          if (def_from^.deftype=filedef) and (def_to^.deftype=filedef) and
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
              b:=true;
           end
         else

         { object pascal objects }
          if (def_from^.deftype=objectdef) and (def_to^.deftype=objectdef) {and
           pobjectdef(def_from)^.isclass and pobjectdef(def_to)^.isclass }then
           begin
              doconv:=tc_equal;
              b:=pobjectdef(def_from)^.isrelated(
                pobjectdef(def_to));
           end
         else
          { class types and class reference type
            can be assigned to void pointers      }
          if (((def_from^.deftype=objectdef) and
               pobjectdef(def_from)^.isclass) or
               (def_from^.deftype=classrefdef)
             ) and
             (def_to^.deftype=pointerdef) and
             (ppointerdef(def_to)^.definition^.deftype=orddef) and
             (porddef(ppointerdef(def_to)^.definition)^.typ=uvoid) then

           begin
              doconv:=tc_equal;
              b:=true;
           end
         else

         { class reference types }
          if (def_from^.deftype=classrefdef) and (def_from^.deftype=classrefdef) then
           begin
              doconv:=tc_equal;
              b:=pobjectdef(pclassrefdef(def_from)^.definition)^.isrelated(
                pobjectdef(pclassrefdef(def_to)^.definition));
           end
         else

          if (def_from^.deftype=pointerdef) and (def_to^.deftype=pointerdef) then
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
                  b:=true;
               end
           end
         else

          if (def_from^.deftype=stringdef) and (def_to^.deftype=stringdef) then
           begin
             doconv:=tc_string_to_string;
             b:=true;
           end
         else

         { char to string}
          if is_equal(def_from,cchardef) and (def_to^.deftype=stringdef) then
           begin
             doconv:=tc_char_to_string;
             b:=true;
           end
         else

         { string constant to zero terminated string constant }
          if (fromtreetype=stringconstn) and
             ((def_to^.deftype=pointerdef) and is_equal(Ppointerdef(def_to)^.definition,cchardef)) then
           begin
             doconv:=tc_cstring_charpointer;
             b:=true;
           end
         else

         { array of char to string, the length check is done by the firstpass of this node }
          if (def_from^.deftype=stringdef) and
             ((def_to^.deftype=arraydef) and is_equal(parraydef(def_to)^.definition,cchardef)) then
           begin
             doconv:=tc_string_chararray;
             b:=true;
           end
         else

         { string to array of char, the length check is done by the firstpass of this node }
          if ((def_from^.deftype=arraydef) and is_equal(parraydef(def_from)^.definition,cchardef)) and
              (def_to^.deftype=stringdef) then
           begin
             doconv:=tc_chararray_2_string;
             b:=true;
           end
         else

           if (fromtreetype=ordconstn) and is_equal(def_from,cchardef) then
             begin
                if (def_to^.deftype=pointerdef) and
                  is_equal(ppointerdef(def_to)^.definition,cchardef) then
                  begin
                     doconv:=tc_cchar_charpointer;
                     b:=true;
                  end;
             end
         else

           if (def_to^.deftype=procvardef) and (def_from^.deftype=procdef) then
             begin
                def_from^.deftype:=procvardef;
                doconv:=tc_proc2procvar;
                b:=is_equal(def_from,def_to);
                def_from^.deftype:=procdef;
             end
         else

           { nil is compatible with class instances }
           if (fromtreetype=niln) and (def_to^.deftype=objectdef)
             and (pobjectdef(def_to)^.isclass) then
             begin
                doconv:=tc_equal;
                b:=true;
             end
         else

           { nil is compatible with class references }
           if (fromtreetype=niln) and (def_to^.deftype=classrefdef) then
             begin
                doconv:=tc_equal;
                b:=true;
             end
         else

           { nil is compatible with procvars }
           if (fromtreetype=niln) and (def_to^.deftype=procvardef) then
             begin
                doconv:=tc_equal;
                b:=true;
             end
         else

           { nil is compatible with ansi- and wide strings }
           if (fromtreetype=niln) and (def_to^.deftype=stringdef)
             and (pstringdef(def_to)^.string_typ in [st_ansistring,st_widestring]) then
             begin
                doconv:=tc_equal;
                b:=true;
             end
         else
           { ansi- and wide strings can be assigned to void pointers }
           if (def_from^.deftype=stringdef) and
             (pstringdef(def_from)^.string_typ in [st_ansistring,st_widestring]) and
             (def_to^.deftype=pointerdef) and
             (ppointerdef(def_to)^.definition^.deftype=orddef) and
             (porddef(ppointerdef(def_to)^.definition)^.typ=uvoid) then
             begin
                doconv:=tc_equal;
                b:=true;
             end
         else
           { ansistrings can be assigned to pchar }
           if is_ansistring(def_from) and
             (def_to^.deftype=pointerdef) and
             (ppointerdef(def_to)^.definition^.deftype=orddef) and
             (porddef(ppointerdef(def_to)^.definition)^.typ=uchar) then
             begin
                doconv:=tc_ansistring_2_pchar;
                b:=true;
             end
         else
           { pchar can be assigned to ansistrings }
           if ((def_from^.deftype=pointerdef) and
             (ppointerdef(def_from)^.definition^.deftype=orddef) and
             (porddef(ppointerdef(def_from)^.definition)^.typ=uchar)) and
             is_ansistring(def_to) then
             begin
                doconv:=tc_pchar_2_ansistring;
                b:=true;
             end
         else

         { procedure variable can be assigned to an void pointer }
         { Not anymore. Use the @ operator now.}
           if not (cs_tp_compatible in aktmoduleswitches) then
             begin
                if (def_from^.deftype=procvardef) and
                  (def_to^.deftype=pointerdef) and
                  (ppointerdef(def_to)^.definition^.deftype=orddef) and
                  (porddef(ppointerdef(def_to)^.definition)^.typ=uvoid) then
                  begin
                     doconv:=tc_equal;
                     b:=true;
                  end;
             end;

         isconvertable:=b;
      end;


    procedure firsterror(var p : ptree);

      begin
         p^.error:=true;
         codegenerror:=true;
         p^.resulttype:=generrordef;
      end;

    procedure firstload(var p : ptree);

      begin
         p^.location.loc:=LOC_REFERENCE;
         p^.registers32:=0;
         p^.registersfpu:=0;

{$ifdef SUPPORT_MMX}
         p^.registersmmx:=0;
{$endif SUPPORT_MMX}
         clear_reference(p^.location.reference);
         if p^.symtableentry^.typ=funcretsym then
           begin
              putnode(p);
              p:=genzeronode(funcretn);
              p^.funcretprocinfo:=pprocinfo(pfuncretsym(p^.symtableentry)^.funcretprocinfo);
              p^.retdef:=pfuncretsym(p^.symtableentry)^.funcretdef;
              firstpass(p);
              exit;
           end;
         if p^.symtableentry^.typ=absolutesym then
           begin
              p^.resulttype:=pabsolutesym(p^.symtableentry)^.definition;
              if pabsolutesym(p^.symtableentry)^.abstyp=tovar then
                p^.symtableentry:=pabsolutesym(p^.symtableentry)^.ref;
              p^.symtable:=p^.symtableentry^.owner;
              p^.is_absolute:=true;
           end;
         case p^.symtableentry^.typ of
            absolutesym :;
            varsym :
                begin
                   if not(p^.is_absolute) and (p^.resulttype=nil) then
                     p^.resulttype:=pvarsym(p^.symtableentry)^.definition;
                   if ((p^.symtable^.symtabletype=parasymtable) or
                       (p^.symtable^.symtabletype=localsymtable)) and
                      (lexlevel>p^.symtable^.symtablelevel) then
                     begin
                        { sollte sich die Variable in einem anderen Stackframe       }
                        { befinden, so brauchen wir ein Register zum Dereferenceieren }
                        if (p^.symtable^.symtablelevel)>0 then
                          begin
                             p^.registers32:=1;
                             { further, the variable can't be put into a register }
                             pvarsym(p^.symtableentry)^.var_options:=
                               pvarsym(p^.symtableentry)^.var_options and not vo_regable;
                          end;
                     end;
                   if (pvarsym(p^.symtableentry)^.varspez=vs_const) then
                     p^.location.loc:=LOC_MEM;
                   { we need a register for call by reference parameters }
                   if (pvarsym(p^.symtableentry)^.varspez=vs_var) or
                      ((pvarsym(p^.symtableentry)^.varspez=vs_const) and
                      dont_copy_const_param(pvarsym(p^.symtableentry)^.definition)
                      ) or
                      { call by value open arrays are also indirect addressed }
                      is_open_array(pvarsym(p^.symtableentry)^.definition) then
                     p^.registers32:=1;
                   if p^.symtable^.symtabletype=withsymtable then
                     inc(p^.registers32);

                   { a class variable is a pointer !!!
                     yes, but we have to resolve the reference in an
                     appropriate tree node (FK)

                   if (pvarsym(p^.symtableentry)^.definition^.deftype=objectdef) and
                      ((pobjectdef(pvarsym(p^.symtableentry)^.definition)^.options and oois_class)<>0) then
                     p^.registers32:=1;
                   }

                   { count variable references }

                   if must_be_valid and p^.is_first then
                     begin
                     if pvarsym(p^.symtableentry)^.is_valid=2 then
                       if (assigned(pvarsym(p^.symtableentry)^.owner) and assigned(aktprocsym)
                       and (pvarsym(p^.symtableentry)^.owner = aktprocsym^.definition^.localst)) then
                       Message1(sym_n_uninitialized_local_variable,pvarsym(p^.symtableentry)^.name);
                     end;
                   if count_ref then
                     begin
                        if (p^.is_first) then
                          begin
                             if (pvarsym(p^.symtableentry)^.is_valid=2) then
                               pvarsym(p^.symtableentry)^.is_valid:=1;
                              p^.is_first:=false;
                           end;
                     end;
                     { this will create problem with local var set by
                     under_procedures
                     if (assigned(pvarsym(p^.symtableentry)^.owner) and assigned(aktprocsym)
                       and ((pvarsym(p^.symtableentry)^.owner = aktprocsym^.definition^.localst)
                       or (pvarsym(p^.symtableentry)^.owner = aktprocsym^.definition^.localst))) then }
                   if t_times<1 then
                     inc(pvarsym(p^.symtableentry)^.refs)
                   else
                     inc(pvarsym(p^.symtableentry)^.refs,t_times);
                end;
            typedconstsym :
              if not p^.is_absolute then
                     p^.resulttype:=ptypedconstsym(p^.symtableentry)^.definition;
            procsym :
                begin
                   if assigned(pprocsym(p^.symtableentry)^.definition^.nextoverloaded) then
                     Message(parser_e_no_overloaded_procvars);
                   p^.resulttype:=pprocsym(p^.symtableentry)^.definition;
                end;
            else internalerror(3);
         end;
      end;


    procedure firstadd(var p : ptree);

      procedure make_bool_equal_size(var p:ptree);
      begin
        if porddef(p^.left^.resulttype)^.typ>porddef(p^.right^.resulttype)^.typ then
         begin
           p^.right:=gentypeconvnode(p^.right,porddef(p^.left^.resulttype));
           p^.right^.convtyp:=tc_bool_2_int;
           p^.right^.explizit:=true;
           firstpass(p^.right);
         end
        else
         if porddef(p^.left^.resulttype)^.typ<porddef(p^.right^.resulttype)^.typ then
          begin
            p^.left:=gentypeconvnode(p^.left,porddef(p^.right^.resulttype));
            p^.left^.convtyp:=tc_bool_2_int;
            p^.left^.explizit:=true;
            firstpass(p^.left);
          end;
      end;

      var
         t       : ptree;
         lt,rt   : ttreetyp;
         rv,lv   : longint;
         rvd,lvd : bestreal;
         rd,ld   : pdef;
         tempdef : pdef;
         concatstrings : boolean;

         { to evalute const sets }
         resultset : pconstset;
         i : longint;
         b : boolean;
         convdone : boolean;
{$ifndef UseAnsiString}
         s1,s2:^string;
{$else UseAnsiString}
         s1,s2 : pchar;
         l1,l2 : longint;
{$endif UseAnsiString}

         { this totally forgets to set the pi_do_call flag !! }
      label
         no_overload;

      begin
         { first do the two subtrees }
         firstpass(p^.left);
         firstpass(p^.right);
         lt:=p^.left^.treetype;
         rt:=p^.right^.treetype;
         rd:=p^.right^.resulttype;
         ld:=p^.left^.resulttype;
         convdone:=false;

         if codegenerror then
           exit;

         { overloaded operator ? }
         if (p^.treetype=starstarn) or
            (ld^.deftype=recorddef) or
            { <> and = are defined for classes }
            ((ld^.deftype=objectdef) and
             (not(pobjectdef(ld)^.isclass) or
              not(p^.treetype in [equaln,unequaln])
             )
            ) or
            (rd^.deftype=recorddef) or
            { <> and = are defined for classes }
            ((rd^.deftype=objectdef) and
             (not(pobjectdef(rd)^.isclass) or
              not(p^.treetype in [equaln,unequaln])
             )
            ) then
           begin
              {!!!!!!!!! handle paras }
              case p^.treetype of
                 { the nil as symtable signs firstcalln that this is
                   an overloaded operator }
                 addn:
                   t:=gencallnode(overloaded_operators[plus],nil);
                 subn:
                   t:=gencallnode(overloaded_operators[minus],nil);
                 muln:
                   t:=gencallnode(overloaded_operators[star],nil);
                 starstarn:
                   t:=gencallnode(overloaded_operators[starstar],nil);
                 slashn:
                   t:=gencallnode(overloaded_operators[slash],nil);
                 ltn:
                   t:=gencallnode(overloaded_operators[globals.lt],nil);
                 gtn:
                   t:=gencallnode(overloaded_operators[gt],nil);
                 lten:
                   t:=gencallnode(overloaded_operators[lte],nil);
                 gten:
                   t:=gencallnode(overloaded_operators[gte],nil);
                 equaln,unequaln :
                   t:=gencallnode(overloaded_operators[equal],nil);
                 else goto no_overload;
              end;
              { we have to convert p^.left and p^.right into
               callparanodes }
              t^.left:=gencallparanode(p^.left,nil);
              t^.left:=gencallparanode(p^.right,t^.left);
              if t^.symtableprocentry=nil then
               Message(parser_e_operator_not_overloaded);
              if p^.treetype=unequaln then
               t:=gensinglenode(notn,t);
              firstpass(t);
              putnode(p);
              p:=t;
              exit;
           end;
         no_overload:
         { compact consts }

         { convert int consts to real consts, if the }
         { other operand is a real const             }
         if (rt=realconstn) and is_constintnode(p^.left) then
           begin
              t:=genrealconstnode(p^.left^.value);
              disposetree(p^.left);
              p^.left:=t;
              lt:=realconstn;
           end;
         if (lt=realconstn) and is_constintnode(p^.right) then
           begin
              t:=genrealconstnode(p^.right^.value);
              disposetree(p^.right);
              p^.right:=t;
              rt:=realconstn;
           end;

       { both are int constants ? }
         if is_constintnode(p^.left) and is_constintnode(p^.right) then
           begin
              lv:=p^.left^.value;
              rv:=p^.right^.value;
              case p^.treetype of
                addn : t:=genordinalconstnode(lv+rv,s32bitdef);
                subn : t:=genordinalconstnode(lv-rv,s32bitdef);
                muln : t:=genordinalconstnode(lv*rv,s32bitdef);
                xorn : t:=genordinalconstnode(lv xor rv,s32bitdef);
                 orn : t:=genordinalconstnode(lv or rv,s32bitdef);
                andn : t:=genordinalconstnode(lv and rv,s32bitdef);
                 ltn : t:=genordinalconstnode(ord(lv<rv),booldef);
                lten : t:=genordinalconstnode(ord(lv<=rv),booldef);
                 gtn : t:=genordinalconstnode(ord(lv>rv),booldef);
                gten : t:=genordinalconstnode(ord(lv>=rv),booldef);
              equaln : t:=genordinalconstnode(ord(lv=rv),booldef);
            unequaln : t:=genordinalconstnode(ord(lv<>rv),booldef);
              slashn : begin
                       { int/int becomes a real }
                         t:=genrealconstnode(int(lv)/int(rv));
                         firstpass(t);
                       end;
              else
                Message(type_e_mismatch);
              end;
              disposetree(p);
              firstpass(t);
              p:=t;
              exit;
           end;

       { both real constants ? }
         if (lt=realconstn) and (rt=realconstn) then
           begin
              lvd:=p^.left^.value_real;
              rvd:=p^.right^.value_real;
              case p^.treetype of
                 addn : t:=genrealconstnode(lvd+rvd);
                 subn : t:=genrealconstnode(lvd-rvd);
                 muln : t:=genrealconstnode(lvd*rvd);
               caretn : t:=genrealconstnode(exp(ln(lvd)*rvd));
               slashn : t:=genrealconstnode(lvd/rvd);
                  ltn : t:=genordinalconstnode(ord(lvd<rvd),booldef);
                 lten : t:=genordinalconstnode(ord(lvd<=rvd),booldef);
                  gtn : t:=genordinalconstnode(ord(lvd>rvd),booldef);
                 gten : t:=genordinalconstnode(ord(lvd>=rvd),booldef);
               equaln : t:=genordinalconstnode(ord(lvd=rvd),booldef);
             unequaln : t:=genordinalconstnode(ord(lvd<>rvd),booldef);
              else
                Message(type_e_mismatch);
              end;
              disposetree(p);
              p:=t;
              firstpass(p);
              exit;
           end;

       { concating strings ? }
         concatstrings:=false;
{$ifdef UseAnsiString}
         s1:=nil;
         s2:=nil;
{$else UseAnsiString}
         new(s1);
         new(s2);
{$endif UseAnsiString}
         if (lt=ordconstn) and (rt=ordconstn) and
            (ld^.deftype=orddef) and (porddef(ld)^.typ=uchar) and
            (rd^.deftype=orddef) and (porddef(rd)^.typ=uchar) then
           begin
{$ifdef UseAnsiString}
              s1:=strpnew(char(byte(p^.left^.value)));
              s2:=strpnew(char(byte(p^.right^.value)));
              l1:=1;l2:=1;
{$else UseAnsiString}
              s1^:=char(byte(p^.left^.value));
              s2^:=char(byte(p^.right^.value));
{$endif UseAnsiString}
              concatstrings:=true;
           end
         else
           if (lt=stringconstn) and (rt=ordconstn) and
              (rd^.deftype=orddef) and (porddef(rd)^.typ=uchar) then
           begin
{$ifdef UseAnsiString}
              { here there is allways the damn #0 problem !! }
              s1:=getpcharcopy(p^.left);
              l1:=p^.left^.length;
              s2:=strpnew(char(byte(p^.right^.value)));
              l2:=1;
{$else UseAnsiString}
              s1^:=p^.left^.value_str^;
              s2^:=char(byte(p^.right^.value));
{$endif UseAnsiString}
              concatstrings:=true;
           end
         else if (lt=ordconstn) and (rt=stringconstn) and
           (ld^.deftype=orddef) and
           (porddef(ld)^.typ=uchar) then
           begin
{$ifdef UseAnsiString}
              { here there is allways the damn #0 problem !! }
              s1:=strpnew(char(byte(p^.left^.value)));
              l1:=1;
              s2:=getpcharcopy(p^.right);
              l2:=p^.right^.length;
{$else UseAnsiString}
              s1^:=char(byte(p^.left^.value));
              s2^:=p^.right^.value_str^;
{$endif UseAnsiString}
              concatstrings:=true;
           end
         else if (lt=stringconstn) and (rt=stringconstn) then
           begin
{$ifdef UseAnsiString}
              s1:=getpcharcopy(p^.left);
              l1:=p^.left^.length;
              s2:=getpcharcopy(p^.right);
              l2:=p^.right^.length;
{$else UseAnsiString}
              s1^:=p^.left^.value_str^;
              s2^:=p^.right^.value_str^;
{$endif UseAnsiString}
              concatstrings:=true;
           end;

         { I will need to translate all this to ansistrings !!! }
         if concatstrings then
           begin
              case p^.treetype of
{$ifndef UseAnsiString}
                 addn : t:=genstringconstnode(s1^+s2^);
                 ltn : t:=genordinalconstnode(byte(s1^<s2^),booldef);
                 lten : t:=genordinalconstnode(byte(s1^<=s2^),booldef);
                 gtn : t:=genordinalconstnode(byte(s1^>s2^),booldef);
                 gten : t:=genordinalconstnode(byte(s1^>=s2^),booldef);
                 equaln : t:=genordinalconstnode(byte(s1^=s2^),booldef);
                 unequaln : t:=genordinalconstnode(byte(s1^<>s2^),booldef);
{$else UseAnsiString}
                 addn : t:=genpcharconstnode(
                             concatansistrings(s1,s2,l1,l2),l1+l2);
                 ltn : t:=genordinalconstnode(
                           byte(compareansistrings(s1,s2,l1,l2)<0),booldef);
                 lten : t:=genordinalconstnode(
                            byte(compareansistrings(s1,s2,l1,l2)<=0),booldef);
                 gtn : t:=genordinalconstnode(
                            byte(compareansistrings(s1,s2,l1,l2)>0),booldef);
                 gten : t:=genordinalconstnode(
                             byte(compareansistrings(s1,s2,l1,l2)>=0),booldef);
                 equaln : t:=genordinalconstnode(
                               byte(compareansistrings(s1,s2,l1,l2)=0),booldef);
                 unequaln : t:=genordinalconstnode(
                                 byte(compareansistrings(s1,s2,l1,l2)<>0),booldef);
{$endif UseAnsiString}
              end;
{$ifdef UseAnsiString}
              ansistringdispose(s1,l1);
              ansistringdispose(s2,l2);
{$else UseAnsiString}
              dispose(s1);
              dispose(s2);
{$endif UseAnsiString}
              disposetree(p);
              firstpass(t);
              p:=t;
              exit;
           end;
{$ifdef UseAnsiString}
         ansistringdispose(s1,l1);
         ansistringdispose(s2,l2);
{$else UseAnsiString}
         dispose(s1);
         dispose(s2);
{$endif UseAnsiString}

       { if both are orddefs then check sub types }
         if (ld^.deftype=orddef) and (rd^.deftype=orddef) then
           begin
           { 2 booleans ? }
             if (porddef(ld)^.typ in [bool8bit,bool16bit,bool32bit]) and
                (porddef(rd)^.typ in [bool8bit,bool16bit,bool32bit]) then
              begin
                case p^.treetype of
             andn,orn : begin
                          calcregisters(p,0,0,0);
                          p^.location.loc:=LOC_JUMP;
                        end;
             unequaln,
          equaln,xorn : begin
                          { this forces a better code generation (TEST }
                          { instead of CMP)                            }
                          if p^.treetype<>xorn then
                            begin
                               if (p^.left^.treetype=ordconstn) and
                                 (p^.left^.value<>0) then
                                 begin
                                    p^.left^.value:=0;
                                    if p^.treetype=equaln then
                                      p^.treetype:=unequaln
                                    else
                                      p^.treetype:=equaln;
                                 end;
                               if (p^.right^.treetype=ordconstn) and
                                 (p^.right^.value<>0) then
                                 begin
                                    p^.right^.value:=0;
                                    if p^.treetype=equaln then
                                      p^.treetype:=unequaln
                                    else
                                      p^.treetype:=equaln;
                                 end;
                            end;

                          make_bool_equal_size(p);
                          calcregisters(p,1,0,0);
                        end
                else
                  Message(type_e_mismatch);
                end;
                convdone:=true;
              end
             else
             { Both are chars? only convert to strings for addn }
              if (porddef(rd)^.typ=uchar) and (porddef(ld)^.typ=uchar) then
               begin
                 if p^.treetype=addn then
                   begin
                      p^.left:=gentypeconvnode(p^.left,cstringdef);
                      firstpass(p^.left);
                      p^.right:=gentypeconvnode(p^.right,cstringdef);
                      firstpass(p^.right);
                      { here we call STRCOPY }
                      procinfo.flags:=procinfo.flags or pi_do_call;
                      calcregisters(p,0,0,0);
                      p^.location.loc:=LOC_MEM;
                   end
                 else
                  calcregisters(p,1,0,0);
                 convdone:=true;
               end;
           end
         else

         { is one of the sides a string ? }
           if (ld^.deftype=stringdef) or (rd^.deftype=stringdef) then
            begin
            { convert other side to a string, if not both site are strings,
              the typeconv will put give an error if it's not possible }
              if not((rd^.deftype=stringdef) and (ld^.deftype=stringdef)) then
               begin
                 if ld^.deftype=stringdef then
                  p^.right:=gentypeconvnode(p^.right,cstringdef)
                 else
                  p^.left:=gentypeconvnode(p^.left,cstringdef);
                 firstpass(p^.left);
                 firstpass(p^.right);
               end;
            { here we call STRCONCAT or STRCMP or STRCOPY }
              procinfo.flags:=procinfo.flags or pi_do_call;
              calcregisters(p,0,0,0);
              p^.location.loc:=LOC_MEM;
              convdone:=true;
           end
         else

         { left side a setdef ? }
           if (ld^.deftype=setdef) then
             begin
             { right site must also be a setdef, unless addn is used }
                if not(p^.treetype in [subn,symdifn,addn,muln,equaln,unequaln]) or
                   ((rd^.deftype<>setdef) and (p^.treetype<>addn)) then
                  Message(type_e_mismatch);

                if ((rd^.deftype=setdef) and not(is_equal(rd,ld))) and
                   not((rt=setelementn) and is_equal(psetdef(ld)^.setof,rd)) then
                  Message(type_e_set_element_are_not_comp);

                { ranges require normsets }
                if (psetdef(ld)^.settype=smallset) and
                   (rt=setelementn) and
                   assigned(p^.right^.right) then
                 begin
                   { generate a temporary normset def }
                   tempdef:=new(psetdef,init(psetdef(ld)^.setof,255));
                   p^.left:=gentypeconvnode(p^.left,tempdef);
                   firstpass(p^.left);
                   dispose(tempdef,done);
                   ld:=p^.left^.resulttype;
                 end;

                { if the destination is not a smallset then insert a typeconv
                  which loads a smallset into a normal set }
                if (psetdef(ld)^.settype<>smallset) and
                   (psetdef(rd)^.settype=smallset) then
                 begin
                   p^.right:=gentypeconvnode(p^.right,psetdef(p^.left^.resulttype));
                   firstpass(p^.right);
                 end;

                { do constant evalution }
                if (p^.right^.treetype=setconstn) and
                   (p^.left^.treetype=setconstn) then
                  begin
                     new(resultset);
                     case p^.treetype of
                        addn : begin
                                  for i:=0 to 31 do
                                    resultset^[i]:=
                                      p^.right^.value_set^[i] or p^.left^.value_set^[i];
                                  t:=gensetconstnode(resultset,psetdef(ld));
                               end;
                        muln : begin
                                  for i:=0 to 31 do
                                    resultset^[i]:=
                                      p^.right^.value_set^[i] and p^.left^.value_set^[i];
                                  t:=gensetconstnode(resultset,psetdef(ld));
                               end;
                        subn : begin
                                  for i:=0 to 31 do
                                    resultset^[i]:=
                                      p^.left^.value_set^[i] and not(p^.right^.value_set^[i]);
                                  t:=gensetconstnode(resultset,psetdef(ld));
                               end;
                     symdifn : begin
                                  for i:=0 to 31 do
                                    resultset^[i]:=
                                      p^.left^.value_set^[i] xor p^.right^.value_set^[i];
                                  t:=gensetconstnode(resultset,psetdef(ld));
                               end;
                    unequaln : begin
                                 b:=true;
                                 for i:=0 to 31 do
                                  if p^.right^.value_set^[i]=p^.left^.value_set^[i] then
                                   begin
                                     b:=false;
                                     break;
                                   end;
                                 t:=genordinalconstnode(ord(b),booldef);
                               end;
                      equaln : begin
                                 b:=true;
                                 for i:=0 to 31 do
                                  if p^.right^.value_set^[i]<>p^.left^.value_set^[i] then
                                   begin
                                     b:=false;
                                     break;
                                   end;
                                 t:=genordinalconstnode(ord(b),booldef);
                               end;
                     end;
                     dispose(resultset);
                     disposetree(p);
                     p:=t;
                     firstpass(p);
                     exit;
                  end
                else
                 if psetdef(ld)^.settype=smallset then
                  begin
                     calcregisters(p,1,0,0);
                     p^.location.loc:=LOC_REGISTER;
                  end
                 else
                  begin
                     calcregisters(p,0,0,0);
                     { here we call SET... }
                     procinfo.flags:=procinfo.flags or pi_do_call;
                     p^.location.loc:=LOC_MEM;
                  end;
              convdone:=true;
            end
         else

         { is one a real float ? }
           if (rd^.deftype=floatdef) or (ld^.deftype=floatdef) then
            begin
            { if one is a fixed, then convert to f32bit }
              if ((rd^.deftype=floatdef) and (pfloatdef(rd)^.typ=f32bit)) or
                 ((ld^.deftype=floatdef) and (pfloatdef(ld)^.typ=f32bit)) then
               begin
                 if not(porddef(rd)^.typ in [u8bit,s8bit,u16bit,s16bit,s32bit,u32bit]) or (p^.treetype<>muln) then
                   p^.right:=gentypeconvnode(p^.right,s32fixeddef);
                 if not(porddef(rd)^.typ in [u8bit,s8bit,u16bit,s16bit,s32bit,u32bit]) or (p^.treetype<>muln) then
                   p^.left:=gentypeconvnode(p^.left,s32fixeddef);
                 firstpass(p^.left);
                 firstpass(p^.right);
                 calcregisters(p,1,0,0);
                 p^.location.loc:=LOC_REGISTER;
               end
              else
              { convert both to c64float }
                begin
                  p^.right:=gentypeconvnode(p^.right,c64floatdef);
                  p^.left:=gentypeconvnode(p^.left,c64floatdef);
                  firstpass(p^.left);
                  firstpass(p^.right);
                  calcregisters(p,1,1,0);
                  p^.location.loc:=LOC_FPU;
                end;
              convdone:=true;
            end
         else

         { pointer comperation and subtraction }
           if (rd^.deftype=pointerdef) and (ld^.deftype=pointerdef) then
            begin
              p^.location.loc:=LOC_REGISTER;
              p^.right:=gentypeconvnode(p^.right,ld);
              firstpass(p^.right);
              calcregisters(p,1,0,0);
              case p^.treetype of
                 equaln,unequaln : ;
                 ltn,lten,gtn,gten:
                   begin
                      if not(cs_extsyntax in aktmoduleswitches) then
                        Message(type_e_mismatch);
                   end;
                 subn:
                   begin
                      if not(cs_extsyntax in aktmoduleswitches) then
                        Message(type_e_mismatch);
                      p^.resulttype:=s32bitdef;
                      exit;
                   end;
                 else Message(type_e_mismatch);
              end;
              convdone:=true;
           end
         else

           if (rd^.deftype=objectdef) and (ld^.deftype=objectdef) and
              pobjectdef(rd)^.isclass and pobjectdef(ld)^.isclass then
            begin
              p^.location.loc:=LOC_REGISTER;
              if pobjectdef(rd)^.isrelated(pobjectdef(ld)) then
                p^.right:=gentypeconvnode(p^.right,ld)
              else
                p^.left:=gentypeconvnode(p^.left,rd);
              firstpass(p^.right);
              firstpass(p^.left);
              calcregisters(p,1,0,0);
              case p^.treetype of
                 equaln,unequaln : ;
                 else Message(type_e_mismatch);
              end;
              convdone:=true;
            end
         else

           if (rd^.deftype=classrefdef) and (ld^.deftype=classrefdef) then
            begin
              p^.location.loc:=LOC_REGISTER;
              if pobjectdef(pclassrefdef(rd)^.definition)^.isrelated(pobjectdef(
                pclassrefdef(ld)^.definition)) then
                p^.right:=gentypeconvnode(p^.right,ld)
              else
                p^.left:=gentypeconvnode(p^.left,rd);
              firstpass(p^.right);
              firstpass(p^.left);
              calcregisters(p,1,0,0);
              case p^.treetype of
                 equaln,unequaln : ;
                 else Message(type_e_mismatch);
              end;
              convdone:=true;
           end
         else

         { allows comperasion with nil pointer }
           if (rd^.deftype=objectdef) and
              pobjectdef(rd)^.isclass then
            begin
              p^.location.loc:=LOC_REGISTER;
              p^.left:=gentypeconvnode(p^.left,rd);
              firstpass(p^.left);
              calcregisters(p,1,0,0);
              case p^.treetype of
                 equaln,unequaln : ;
                 else Message(type_e_mismatch);
              end;
              convdone:=true;
            end
         else

           if (ld^.deftype=objectdef) and
              pobjectdef(ld)^.isclass then
            begin
              p^.location.loc:=LOC_REGISTER;
              p^.right:=gentypeconvnode(p^.right,ld);
              firstpass(p^.right);
              calcregisters(p,1,0,0);
              case p^.treetype of
                 equaln,unequaln : ;
                 else Message(type_e_mismatch);
              end;
              convdone:=true;
            end
         else

           if (rd^.deftype=classrefdef) then
            begin
              p^.left:=gentypeconvnode(p^.left,rd);
              firstpass(p^.left);
              calcregisters(p,1,0,0);
              case p^.treetype of
                 equaln,unequaln : ;
                 else Message(type_e_mismatch);
              end;
              convdone:=true;
            end
         else

           if (ld^.deftype=classrefdef) then
            begin
              p^.right:=gentypeconvnode(p^.right,ld);
              firstpass(p^.right);
              calcregisters(p,1,0,0);
              case p^.treetype of
                equaln,unequaln : ;
              else
                Message(type_e_mismatch);
              end;
              convdone:=true;
           end
         else

           if (rd^.deftype=pointerdef) then
            begin
              p^.location.loc:=LOC_REGISTER;
              p^.left:=gentypeconvnode(p^.left,s32bitdef);
              firstpass(p^.left);
              calcregisters(p,1,0,0);
              if p^.treetype=addn then
                begin
                  if not(cs_extsyntax in aktmoduleswitches) then
                    Message(type_e_mismatch);
                end
              else
                Message(type_e_mismatch);
              convdone:=true;
            end
         else

           if (ld^.deftype=pointerdef) then
            begin
              p^.location.loc:=LOC_REGISTER;
              p^.right:=gentypeconvnode(p^.right,s32bitdef);
              firstpass(p^.right);
              calcregisters(p,1,0,0);
              case p^.treetype of
                addn,subn : if not(cs_extsyntax in aktmoduleswitches) then
                              Message(type_e_mismatch);
              else
                Message(type_e_mismatch);
              end;
              convdone:=true;
           end
         else

           if (rd^.deftype=procvardef) and (ld^.deftype=procvardef) and is_equal(rd,ld) then
            begin
              calcregisters(p,1,0,0);
              p^.location.loc:=LOC_REGISTER;
              case p^.treetype of
                 equaln,unequaln : ;
              else
                Message(type_e_mismatch);
              end;
              convdone:=true;
            end
         else

{$ifdef SUPPORT_MMX}
           if (cs_mmx in aktlocalswitches) and is_mmx_able_array(ld) and
             is_mmx_able_array(rd) and is_equal(ld,rd) then
            begin
              firstpass(p^.right);
              firstpass(p^.left);
              case p^.treetype of
                addn,subn,xorn,orn,andn:
                  ;
                { mul is a little bit restricted }
                muln:
                  if not(mmx_type(p^.left^.resulttype) in
                    [mmxu16bit,mmxs16bit,mmxfixed16]) then
                    Message(type_e_mismatch);
                else
                  Message(type_e_mismatch);
              end;
              p^.location.loc:=LOC_MMXREGISTER;
              calcregisters(p,0,0,1);
              convdone:=true;
            end
          else
{$endif SUPPORT_MMX}

           if (ld^.deftype=enumdef) and (rd^.deftype=enumdef) and (is_equal(ld,rd)) then
            begin
              calcregisters(p,1,0,0);
              case p^.treetype of
                 equaln,unequaln,
                 ltn,lten,gtn,gten : ;
                 else Message(type_e_mismatch);
              end;
              convdone:=true;
            end;

         { the general solution is to convert to 32 bit int }
         if not convdone then
           begin
              { but an int/int gives real/real! }
              if p^.treetype=slashn then
                begin
                   Message(type_w_int_slash_int);
                   Message(type_h_use_div_for_int);
                   p^.right:=gentypeconvnode(p^.right,c64floatdef);
                   p^.left:=gentypeconvnode(p^.left,c64floatdef);
                   firstpass(p^.left);
                   firstpass(p^.right);
                   { maybe we need an integer register to save }
                   { a reference                               }
                   if ((p^.left^.location.loc<>LOC_FPU) or
                       (p^.right^.location.loc<>LOC_FPU)) and
                       (p^.left^.registers32=p^.right^.registers32) then
                     calcregisters(p,1,1,0)
                   else
                     calcregisters(p,0,1,0);
                   p^.location.loc:=LOC_FPU;
                end
              else
                begin
                   p^.right:=gentypeconvnode(p^.right,s32bitdef);
                   p^.left:=gentypeconvnode(p^.left,s32bitdef);
                   firstpass(p^.left);
                   firstpass(p^.right);
                   calcregisters(p,1,0,0);
                   p^.location.loc:=LOC_REGISTER;
                end;
           end;

         if codegenerror then
           exit;

         { determines result type for comparions }
         { here the is a problem with multiple passes }
         { example length(s)+1 gets internal 'longint' type first }
         { if it is a arg it is converted to 'LONGINT' }
         { but a second first pass will reset this to 'longint' }
         case p^.treetype of
            ltn,lten,gtn,gten,equaln,unequaln:
              begin
                 if not assigned(p^.resulttype) then
                   p^.resulttype:=booldef;
                 p^.location.loc:=LOC_FLAGS;
              end;
            xorn:
              begin
                if not assigned(p^.resulttype) then
                  p^.resulttype:=p^.left^.resulttype;
                 p^.location.loc:=LOC_REGISTER;
              end;
            addn:
              begin
                 { the result of a string addition is a string of length 255 }
                 if (p^.left^.resulttype^.deftype=stringdef) or
                    (p^.right^.resulttype^.deftype=stringdef) then
                   begin
{$ifndef UseAnsiString}
                      if not assigned(p^.resulttype) then
                        p^.resulttype:=cstringdef
{$else UseAnsiString}
                      if is_ansistring(p^.left^.resulttype) or
                         is_ansistring(p^.right^.resulttype) then
                        p^.resulttype:=cansistringdef
                      else
                        p^.resulttype:=cstringdef;
{$endif UseAnsiString}
                   end
                 else
                   if not assigned(p^.resulttype) then
                     p^.resulttype:=p^.left^.resulttype;
              end;
            else if not assigned(p^.resulttype) then
              p^.resulttype:=p^.left^.resulttype;
         end;
      end;

    procedure firstmoddiv(var p : ptree);

      var
         t : ptree;
         {power : longint; }

      begin
         firstpass(p^.left);
         firstpass(p^.right);

         if codegenerror then
           exit;

         if is_constintnode(p^.left) and is_constintnode(p^.right) then
           begin
              case p^.treetype of
                 modn : t:=genordinalconstnode(p^.left^.value mod p^.right^.value,s32bitdef);
                 divn : t:=genordinalconstnode(p^.left^.value div p^.right^.value,s32bitdef);
              end;
              disposetree(p);
              firstpass(t);
              p:=t;
              exit;
           end;
         { !!!!!! u32bit }
         p^.right:=gentypeconvnode(p^.right,s32bitdef);
         p^.left:=gentypeconvnode(p^.left,s32bitdef);
         firstpass(p^.left);
         firstpass(p^.right);

         if codegenerror then
           exit;

         left_right_max(p);
         if p^.left^.registers32<=p^.right^.registers32 then
           inc(p^.registers32);

         p^.resulttype:=s32bitdef;
         p^.location.loc:=LOC_REGISTER;
      end;

    procedure firstshlshr(var p : ptree);

      var
         t : ptree;

      begin
         firstpass(p^.left);
         firstpass(p^.right);

         if codegenerror then
           exit;

         if is_constintnode(p^.left) and is_constintnode(p^.right) then
           begin
              case p^.treetype of
                 shrn : t:=genordinalconstnode(p^.left^.value shr p^.right^.value,s32bitdef);
                 shln : t:=genordinalconstnode(p^.left^.value shl p^.right^.value,s32bitdef);
              end;
              disposetree(p);
              firstpass(t);
              p:=t;
              exit;
           end;
         p^.right:=gentypeconvnode(p^.right,s32bitdef);
         p^.left:=gentypeconvnode(p^.left,s32bitdef);
         firstpass(p^.left);
         firstpass(p^.right);

         if codegenerror then
           exit;

         calcregisters(p,2,0,0);
         {
         p^.registers32:=p^.left^.registers32;

         if p^.registers32<p^.right^.registers32 then
           p^.registers32:=p^.right^.registers32;
         if p^.registers32<1 then p^.registers32:=1;
         }
         p^.resulttype:=s32bitdef;
         p^.location.loc:=LOC_REGISTER;
      end;

    procedure firstrealconst(var p : ptree);

      begin
         p^.location.loc:=LOC_MEM;
      end;

    procedure firstfixconst(var p : ptree);

      begin
         p^.location.loc:=LOC_MEM;
      end;

    procedure firstordconst(var p : ptree);

      begin
         p^.location.loc:=LOC_MEM;
      end;

    procedure firstniln(var p : ptree);

      begin
         p^.resulttype:=voidpointerdef;
         p^.location.loc:=LOC_MEM;
      end;

    procedure firststringconst(var p : ptree);

      begin
         {why this !!! lost of dummy type definitions
         one per const string !!!
         p^.resulttype:=new(pstringdef,init(length(p^.value_str^)));}
         if cs_ansistrings in aktlocalswitches then
           p^.resulttype:=cansistringdef
         else
           p^.resulttype:=cstringdef;
         p^.location.loc:=LOC_MEM;
      end;

    procedure firstumminus(var p : ptree);

      var
         t : ptree;
         minusdef : pprocdef;

      begin
         firstpass(p^.left);
         p^.registers32:=p^.left^.registers32;
         p^.registersfpu:=p^.left^.registersfpu;
{$ifdef SUPPORT_MMX}
         p^.registersmmx:=p^.left^.registersmmx;
{$endif SUPPORT_MMX}
         p^.resulttype:=p^.left^.resulttype;
         if codegenerror then
           exit;
         if is_constintnode(p^.left) then
           begin
              t:=genordinalconstnode(-p^.left^.value,s32bitdef);
              disposetree(p);
              firstpass(t);
              p:=t;
              exit;
           end;
           { nasm can not cope with negativ reals !! }
         if is_constrealnode(p^.left)
{$ifdef i386}
         and not(aktoutputformat in [as_nasmcoff,as_nasmelf,as_nasmobj])
{$endif}
           then
           begin
              t:=genrealconstnode(-p^.left^.value_real);
              disposetree(p);
              firstpass(t);
              p:=t;
              exit;
           end;
         if (p^.left^.resulttype^.deftype=floatdef) then
           begin
              if pfloatdef(p^.left^.resulttype)^.typ=f32bit then
                begin
                   if (p^.left^.location.loc<>LOC_REGISTER) and
                     (p^.registers32<1) then
                     p^.registers32:=1;
                   p^.location.loc:=LOC_REGISTER;
                end
              else
                p^.location.loc:=LOC_FPU;
           end
{$ifdef SUPPORT_MMX}
         else if (cs_mmx in aktlocalswitches) and
           is_mmx_able_array(p^.left^.resulttype) then
             begin
               if (p^.left^.location.loc<>LOC_MMXREGISTER) and
                 (p^.registersmmx<1) then
                 p^.registersmmx:=1;
               { if saturation is on, p^.left^.resulttype isn't
                 "mmx able" (FK)
               if (cs_mmx_saturation in aktlocalswitches^) and
                 (porddef(parraydef(p^.resulttype)^.definition)^.typ in
                 [s32bit,u32bit]) then
                 Message(type_e_mismatch);
               }
             end
{$endif SUPPORT_MMX}
         else if (p^.left^.resulttype^.deftype=orddef) then
           begin
              p^.left:=gentypeconvnode(p^.left,s32bitdef);
              firstpass(p^.left);
              p^.registersfpu:=p^.left^.registersfpu;
{$ifdef SUPPORT_MMX}
              p^.registersmmx:=p^.left^.registersmmx;
{$endif SUPPORT_MMX}
              p^.registers32:=p^.left^.registers32;
              if codegenerror then
                exit;
              if (p^.left^.location.loc<>LOC_REGISTER) and
                (p^.registers32<1) then
              p^.registers32:=1;
              p^.location.loc:=LOC_REGISTER;
              p^.resulttype:=p^.left^.resulttype;
           end
         else
           begin
              if assigned(overloaded_operators[minus]) then
                minusdef:=overloaded_operators[minus]^.definition
              else
                minusdef:=nil;
              while assigned(minusdef) do
                begin
                   if (minusdef^.para1^.data=p^.left^.resulttype) and
                     (minusdef^.para1^.next=nil) then
                     begin
                        t:=gencallnode(overloaded_operators[minus],nil);
                        t^.left:=gencallparanode(p^.left,nil);
                        putnode(p);
                        p:=t;
                        firstpass(p);
                        exit;
                     end;
                   minusdef:=minusdef^.nextoverloaded;
                end;
              Message(type_e_mismatch);
           end;
      end;

    procedure firstaddr(var p : ptree);

      var
         hp  : ptree;
         hp2 : pdefcoll;
         store_valid : boolean;
         hp3 : pabstractprocdef;

      begin
         make_not_regable(p^.left);
         if not(assigned(p^.resulttype)) then
           begin
              if p^.left^.treetype=calln then
                begin
                   hp:=genloadnode(pvarsym(p^.left^.symtableprocentry),p^.left^.symtableproc);
                   { result is a procedure variable }
                   { No, to be TP compatible, you must return a pointer to
                     the procedure that is stored in the procvar.}
                   if not(cs_tp_compatible in aktmoduleswitches) then
                     begin
                        p^.resulttype:=new(pprocvardef,init);

                     { it could also be a procvar, not only pprocsym ! }
                        if p^.left^.symtableprocentry^.typ=varsym then
                         hp3:=pabstractprocdef(pvarsym(p^.left^.symtableprocentry)^.definition)
                        else
                         hp3:=pabstractprocdef(pprocsym(p^.left^.symtableprocentry)^.definition);

                        pprocvardef(p^.resulttype)^.options:=hp3^.options;
                        pprocvardef(p^.resulttype)^.retdef:=hp3^.retdef;

                        hp2:=hp3^.para1;
                        while assigned(hp2) do
                          begin
                             pprocvardef(p^.resulttype)^.concatdef(hp2^.data,hp2^.paratyp);
                             hp2:=hp2^.next;
                          end;
                     end
                   else
                     p^.resulttype:=voidpointerdef;

                   disposetree(p^.left);
                   p^.left:=hp;
                end
              else
                begin
                  if not(cs_typed_addresses in aktlocalswitches) then
                    p^.resulttype:=voidpointerdef
                  else p^.resulttype:=new(ppointerdef,init(p^.left^.resulttype));
                end;
           end;
         store_valid:=must_be_valid;
         must_be_valid:=false;
         firstpass(p^.left);
         must_be_valid:=store_valid;
         if codegenerror then
           exit;

         { we should allow loc_mem for @string }
         if (p^.left^.location.loc<>LOC_REFERENCE) and
            (p^.left^.location.loc<>LOC_MEM) then
           Message(cg_e_illegal_expression);

         p^.registers32:=p^.left^.registers32;
         p^.registersfpu:=p^.left^.registersfpu;
{$ifdef SUPPORT_MMX}
         p^.registersmmx:=p^.left^.registersmmx;
{$endif SUPPORT_MMX}
         if p^.registers32<1 then
           p^.registers32:=1;
         p^.location.loc:=LOC_REGISTER;
      end;

    procedure firstdoubleaddr(var p : ptree);

      begin
         make_not_regable(p^.left);
         firstpass(p^.left);
         if p^.resulttype=nil then
                p^.resulttype:=voidpointerdef;
         if (p^.left^.resulttype^.deftype)<>procvardef then
                Message(cg_e_illegal_expression);

         if codegenerror then
           exit;

         if (p^.left^.location.loc<>LOC_REFERENCE) then
           Message(cg_e_illegal_expression);

         p^.registers32:=p^.left^.registers32;
         p^.registersfpu:=p^.left^.registersfpu;
{$ifdef SUPPORT_MMX}
         p^.registersmmx:=p^.left^.registersmmx;
{$endif SUPPORT_MMX}
         if p^.registers32<1 then
           p^.registers32:=1;
         p^.location.loc:=LOC_REGISTER;
      end;

    procedure firstnot(var p : ptree);

      var
         t : ptree;

      begin
         firstpass(p^.left);

         if codegenerror then
           exit;

         if (p^.left^.treetype=ordconstn) then
           begin
              t:=genordinalconstnode(not(p^.left^.value),p^.left^.resulttype);
              disposetree(p);
              firstpass(t);
              p:=t;
              exit;
           end;
         p^.resulttype:=p^.left^.resulttype;
         p^.location.loc:=p^.left^.location.loc;
{$ifdef SUPPORT_MMX}
         p^.registersmmx:=p^.left^.registersmmx;
{$endif SUPPORT_MMX}
         if is_equal(p^.resulttype,booldef) then
           begin
              p^.registers32:=p^.left^.registers32;
              if ((p^.location.loc=LOC_REFERENCE) or
                (p^.location.loc=LOC_CREGISTER)) and
                (p^.registers32<1) then
                p^.registers32:=1;
           end
         else
{$ifdef SUPPORT_MMX}
           if (cs_mmx in aktlocalswitches) and
             is_mmx_able_array(p^.left^.resulttype) then
             begin
               if (p^.left^.location.loc<>LOC_MMXREGISTER) and
                 (p^.registersmmx<1) then
                 p^.registersmmx:=1;
             end
         else
{$endif SUPPORT_MMX}
           begin
              p^.left:=gentypeconvnode(p^.left,s32bitdef);
              firstpass(p^.left);

              if codegenerror then
                exit;

              p^.resulttype:=p^.left^.resulttype;
              p^.registers32:=p^.left^.registers32;
{$ifdef SUPPORT_MMX}
              p^.registersmmx:=p^.left^.registersmmx;
{$endif SUPPORT_MMX}

              if (p^.left^.location.loc<>LOC_REGISTER) and
                (p^.registers32<1) then
                p^.registers32:=1;
              p^.location.loc:=LOC_REGISTER;
           end;
         p^.registersfpu:=p^.left^.registersfpu;
      end;

    procedure firstnothing(var p : ptree);

      begin
         p^.resulttype:=voiddef;
      end;

    procedure firstassignment(var p : ptree);

      var
         store_valid : boolean;
         hp : ptree;

      begin
         store_valid:=must_be_valid;
         must_be_valid:=false;

         firstpass(p^.left);
         if codegenerror then
           exit;

         { assignements to open arrays aren't allowed }
         if is_open_array(p^.left^.resulttype) then
           Message(type_e_mismatch);
         { test if we can avoid copying string to temp
           as in s:=s+...; (PM) }
{$ifdef dummyi386}
         if ((p^.right^.treetype=addn) or (p^.right^.treetype=subn)) and
            equal_trees(p^.left,p^.right^.left) and
            (ret_in_acc(p^.left^.resulttype)) and
            (not cs_rangechecking in aktmoduleswitches^) then
           begin
              disposetree(p^.right^.left);
              hp:=p^.right;
              p^.right:=p^.right^.right;
              if hp^.treetype=addn then
                p^.assigntyp:=at_plus
              else
                p^.assigntyp:=at_minus;
              putnode(hp);
           end;
         if p^.assigntyp<>at_normal then
           begin
              { for fpu type there is no faster way }
              if is_fpu(p^.left^.resulttype) then
                case p^.assigntyp of
                  at_plus  : p^.right:=gennode(addn,getcopy(p^.left),p^.right);
                  at_minus : p^.right:=gennode(subn,getcopy(p^.left),p^.right);
                  at_star  : p^.right:=gennode(muln,getcopy(p^.left),p^.right);
                  at_slash : p^.right:=gennode(slashn,getcopy(p^.left),p^.right);
                  end;
           end;
{$endif i386}
         must_be_valid:=true;
         firstpass(p^.right);
         must_be_valid:=store_valid;
         if codegenerror then
           exit;

         { some string functions don't need conversion, so treat them separatly }

         if is_shortstring(p^.left^.resulttype) and (assigned(p^.right^.resulttype)) then
          begin
            if not ((p^.right^.resulttype^.deftype=stringdef) or
                    ((p^.right^.resulttype^.deftype=orddef) and (porddef(p^.right^.resulttype)^.typ=uchar))) then
             begin
               p^.right:=gentypeconvnode(p^.right,p^.left^.resulttype);
               firstpass(p^.right);
               if codegenerror then
                exit;
             end;
            { we call STRCOPY }
            procinfo.flags:=procinfo.flags or pi_do_call;
            hp:=p^.right;
            { test for s:=s+anything ... }
            { the problem is for
              s:=s+s+s;
              this is broken here !! }
            { while hp^.treetype=addn do hp:=hp^.left;
            if equal_trees(p^.left,hp) then
              begin
                p^.concat_string:=true;
                hp:=p^.right;
                while hp^.treetype=addn do
                  begin
                    hp^.use_strconcat:=true;
                    hp:=hp^.left;
                  end;
              end; }
          end
         else
          begin
            if (p^.right^.treetype=realconstn) then
              begin
                 if p^.left^.resulttype^.deftype=floatdef then
                   begin
                      case pfloatdef(p^.left^.resulttype)^.typ of
                        s32real : p^.right^.realtyp:=ait_real_32bit;
                        s64real : p^.right^.realtyp:=ait_real_64bit;
                        s80real : p^.right^.realtyp:=ait_real_extended;
                        { what about f32bit and s64bit }
                      else
                        begin
                           p^.right:=gentypeconvnode(p^.right,p^.left^.resulttype);

                           { nochmal firstpass wegen der Typkonvertierung aufrufen }
                           firstpass(p^.right);

                           if codegenerror then
                             exit;
                        end;
                      end;
                   end;
               end
             else
               begin
                 p^.right:=gentypeconvnode(p^.right,p^.left^.resulttype);
                 firstpass(p^.right);
                 if codegenerror then
                  exit;
               end;
          end;

         p^.resulttype:=voiddef;
         {
           p^.registers32:=max(p^.left^.registers32,p^.right^.registers32);
           p^.registersfpu:=max(p^.left^.registersfpu,p^.right^.registersfpu);
         }
         p^.registers32:=p^.left^.registers32+p^.right^.registers32;
         p^.registersfpu:=max(p^.left^.registersfpu,p^.right^.registersfpu);
{$ifdef SUPPORT_MMX}
         p^.registersmmx:=max(p^.left^.registersmmx,p^.right^.registersmmx);
{$endif SUPPORT_MMX}
      end;

    procedure firstlr(var p : ptree);

      begin
         firstpass(p^.left);
         firstpass(p^.right);
      end;

    procedure firstderef(var p : ptree);

      begin
         firstpass(p^.left);
         if codegenerror then
           begin
             p^.resulttype:=generrordef;
             exit;
           end;

         p^.registers32:=max(p^.left^.registers32,1);
         p^.registersfpu:=p^.left^.registersfpu;
{$ifdef SUPPORT_MMX}
         p^.registersmmx:=p^.left^.registersmmx;
{$endif SUPPORT_MMX}

         if p^.left^.resulttype^.deftype<>pointerdef then
          Message(cg_e_invalid_qualifier);

         p^.resulttype:=ppointerdef(p^.left^.resulttype)^.definition;
         p^.location.loc:=LOC_REFERENCE;
      end;

    procedure firstrange(var p : ptree);

      var
         ct : tconverttype;

      begin
         firstpass(p^.left);
         firstpass(p^.right);
         if codegenerror then
           exit;
         { both types must be compatible }
         if not(is_equal(p^.left^.resulttype,p^.right^.resulttype)) and
            not(isconvertable(p^.left^.resulttype,p^.right^.resulttype,ct,ordconstn,false)) then
           Message(type_e_mismatch);
         { Check if only when its a constant set }
         if (p^.left^.treetype=ordconstn) and (p^.right^.treetype=ordconstn) then
          begin
          { upper limit must be greater or equal than lower limit }
          { not if u32bit }
            if (p^.left^.value>p^.right^.value) and
               (( p^.left^.value<0) or (p^.right^.value>=0)) then
              Message(cg_e_upper_lower_than_lower);
          end;
        left_right_max(p);
        p^.resulttype:=p^.left^.resulttype;
        set_location(p^.location,p^.left^.location);
      end;


    procedure firstvecn(var p : ptree);

      var
         harr : pdef;
         ct : tconverttype;


      begin
         firstpass(p^.left);
         firstpass(p^.right);
         if codegenerror then
           exit;

         { range check only for arrays }
         if (p^.left^.resulttype^.deftype=arraydef) then
           begin
              if not(isconvertable(p^.right^.resulttype,
                parraydef(p^.left^.resulttype)^.rangedef,
                ct,ordconstn,false)) and
              not(is_equal(p^.right^.resulttype,
                parraydef(p^.left^.resulttype)^.rangedef)) then
                Message(type_e_mismatch);
           end;
         { Never convert a boolean or a char !}
         { maybe type conversion }
         if (p^.right^.resulttype^.deftype<>enumdef) and
          not ((p^.right^.resulttype^.deftype=orddef) and
          (Porddef(p^.right^.resulttype)^.typ in [bool8bit,bool16bit,bool32bit,uchar])) then
                begin
                        p^.right:=gentypeconvnode(p^.right,s32bitdef);
                        { once more firstpass }
                        {?? It's better to only firstpass when the tree has
                         changed, isn't it ?}
                        firstpass(p^.right);
                end;
         if codegenerror then
           exit;

         { determine return type }
         if not assigned(p^.resulttype) then
           if p^.left^.resulttype^.deftype=arraydef then
             p^.resulttype:=parraydef(p^.left^.resulttype)^.definition
           else if (p^.left^.resulttype^.deftype=pointerdef) then
             begin
                { convert pointer to array }
                harr:=new(parraydef,init(0,$7fffffff,s32bitdef));
                parraydef(harr)^.definition:=ppointerdef(p^.left^.resulttype)^.definition;
                p^.left:=gentypeconvnode(p^.left,harr);
                firstpass(p^.left);

                if codegenerror then
                  exit;
                p^.resulttype:=parraydef(harr)^.definition
             end
           else if p^.left^.resulttype^.deftype=stringdef then
             begin
                { indexed access to strings }
                case pstringdef(p^.left^.resulttype)^.string_typ of
                   {
                   st_widestring : p^.resulttype:=cwchardef;
                   }
                   st_ansistring : p^.resulttype:=cchardef;
                   st_longstring : p^.resulttype:=cchardef;
                   st_shortstring : p^.resulttype:=cchardef;
                end;
             end
           else
             Message(type_e_mismatch);
         { the register calculation is easy if a const index is used }
         if p^.right^.treetype=ordconstn then
           begin
              p^.registers32:=p^.left^.registers32;

              { for ansi/wide strings, we need at least one register }
              if is_ansistring(p^.left^.resulttype) or
                is_widestring(p^.left^.resulttype) then
                p^.registers32:=max(p^.registers32,1);
           end
         else
           begin
              { this rules are suboptimal, but they should give }
              { good results                                    }
              p^.registers32:=max(p^.left^.registers32,p^.right^.registers32);

              { for ansi/wide strings, we need at least one register }
              if is_ansistring(p^.left^.resulttype) or
                is_widestring(p^.left^.resulttype) then
                p^.registers32:=max(p^.registers32,1);

              { need we an extra register when doing the restore ? }
              if (p^.left^.registers32<=p^.right^.registers32) and
              { only if the node needs less than 3 registers }
              { two for the right node and one for the       }
              { left address                                 }
                (p^.registers32<3) then
                inc(p^.registers32);

              { need we an extra register for the index ? }
              if (p^.right^.location.loc<>LOC_REGISTER)
              { only if the right node doesn't need a register }
                and (p^.right^.registers32<1) then
                inc(p^.registers32);

              { not correct, but what works better ?
              if p^.left^.registers32>0 then
                p^.registers32:=max(p^.registers32,2)
              else
                 min. one register
                p^.registers32:=max(p^.registers32,1);
              }
           end;
         p^.registersfpu:=max(p^.left^.registersfpu,p^.right^.registersfpu);
{$ifdef SUPPORT_MMX}
         p^.registersmmx:=max(p^.left^.registersmmx,p^.right^.registersmmx);
{$endif SUPPORT_MMX}
         p^.location.loc:=p^.left^.location.loc;
      end;

    type
       tfirstconvproc = procedure(var p : ptree);

    procedure first_bigger_smaller(var p : ptree);

      begin
         if (p^.left^.location.loc<>LOC_REGISTER) and (p^.registers32=0) then
           p^.registers32:=1;
         p^.location.loc:=LOC_REGISTER;
      end;

    procedure first_cstring_charpointer(var p : ptree);

      begin
         p^.registers32:=1;
         p^.location.loc:=LOC_REGISTER;
      end;

    procedure first_string_chararray(var p : ptree);

      begin
         p^.registers32:=1;
         p^.location.loc:=LOC_REGISTER;
      end;

    procedure first_string_string(var p : ptree);

      begin
         if pstringdef(p^.resulttype)^.string_typ<>
            pstringdef(p^.left^.resulttype)^.string_typ then
           begin
              if p^.left^.treetype=stringconstn then
                p^.left^.stringtype:=pstringdef(p^.resulttype)^.string_typ
              else
                procinfo.flags:=procinfo.flags or pi_do_call;
           end;
         { for simplicity lets first keep all ansistrings
           as LOC_MEM, could also become LOC_REGISTER }
         p^.location.loc:=LOC_MEM;
      end;

    procedure first_char_to_string(var p : ptree);

      var
         hp : ptree;

      begin
         if p^.left^.treetype=ordconstn then
           begin
              hp:=genstringconstnode(chr(p^.left^.value));
              firstpass(hp);
              disposetree(p);
              p:=hp;
           end
         else
           p^.location.loc:=LOC_MEM;
      end;

    procedure first_nothing(var p : ptree);

      begin
         p^.location.loc:=LOC_MEM;
      end;

    procedure first_array_to_pointer(var p : ptree);

      begin
         if p^.registers32<1 then
           p^.registers32:=1;
         p^.location.loc:=LOC_REGISTER;
      end;

    procedure first_int_real(var p : ptree);

      var t : ptree;

      begin
         if p^.left^.treetype=ordconstn then
           begin
              { convert constants direct }
              { not because of type conversion }
              t:=genrealconstnode(p^.left^.value);
              { do a first pass here
                because firstpass of typeconv does
                not redo it for left field !! }
              firstpass(t);
              { the type can be something else than s64real !!}
              t:=gentypeconvnode(t,p^.resulttype);
              firstpass(t);
              disposetree(p);
              p:=t;
              exit;
           end
         else
           begin
              if p^.registersfpu<1 then
                p^.registersfpu:=1;
              p^.location.loc:=LOC_FPU;
           end;
      end;

    procedure first_int_fix(var p : ptree);

      begin
         if p^.left^.treetype=ordconstn then
           begin
              { convert constants direct }
              p^.treetype:=fixconstn;
              p^.value_fix:=p^.left^.value shl 16;
              p^.disposetyp:=dt_nothing;
              disposetree(p^.left);
              p^.location.loc:=LOC_MEM;
           end
         else
           begin
              if p^.registers32<1 then
                p^.registers32:=1;
                  p^.location.loc:=LOC_REGISTER;
           end;
      end;

    procedure first_real_fix(var p : ptree);

      begin
         if p^.left^.treetype=realconstn then
           begin
              { convert constants direct }
              p^.treetype:=fixconstn;
              p^.value_fix:=round(p^.left^.value_real*65536);
              p^.disposetyp:=dt_nothing;
              disposetree(p^.left);
              p^.location.loc:=LOC_MEM;
           end
         else
           begin
              { at least one fpu and int register needed }
              if p^.registers32<1 then
                p^.registers32:=1;
              if p^.registersfpu<1 then
                p^.registersfpu:=1;
              p^.location.loc:=LOC_REGISTER;
           end;
      end;

    procedure first_fix_real(var p : ptree);

      begin
         if p^.left^.treetype=fixconstn then
           begin
              { convert constants direct }
              p^.treetype:=realconstn;
              p^.value_real:=round(p^.left^.value_fix/65536.0);
              p^.disposetyp:=dt_nothing;
              disposetree(p^.left);
              p^.location.loc:=LOC_MEM;
           end
         else
           begin
              if p^.registersfpu<1 then
                p^.registersfpu:=1;
                  p^.location.loc:=LOC_FPU;
           end;
    end;

    procedure first_real_real(var p : ptree);

      begin
         if p^.registersfpu<1 then
           p^.registersfpu:=1;
         p^.location.loc:=LOC_FPU;
      end;

    procedure first_pointer_to_array(var p : ptree);

      begin
         if p^.registers32<1 then
           p^.registers32:=1;
         p^.location.loc:=LOC_REFERENCE;
      end;

    procedure first_chararray_string(var p : ptree);

      begin
         { the only important information is the location of the }
         { result                                                }
         { other stuff is done by firsttypeconv                  }
         p^.location.loc:=LOC_MEM;
      end;

    procedure first_cchar_charpointer(var p : ptree);

      begin
         p^.left:=gentypeconvnode(p^.left,cstringdef);
         { convert constant char to constant string }
         firstpass(p^.left);
         { evalute tree }
         firstpass(p);
      end;

    procedure first_locmem(var p : ptree);

      begin
         p^.location.loc:=LOC_MEM;
      end;

    procedure first_bool_int(var p : ptree);
       begin
          p^.location.loc:=LOC_REGISTER;
          { Florian I think this is overestimated
            but I still do not really understand how to get this right (PM) }
          { Hmmm, I think we need only one reg to return the result of      }
          { this node => so }
          if p^.registers32<1 then
            p^.registers32:=1;
          {  should work (FK)
          p^.registers32:=p^.left^.registers32+1;}
       end;

    procedure first_int_bool(var p : ptree);

       begin
          p^.location.loc:=LOC_REGISTER;
          { Florian I think this is overestimated
            but I still do not really understand how to get this right (PM) }
          { Hmmm, I think we need only one reg to return the result of      }
          { this node => so }
          p^.left:=gentypeconvnode(p^.left,s32bitdef);
          firstpass(p^.left);
          if p^.registers32<1 then
            p^.registers32:=1;
{          p^.resulttype:=booldef; }
          {  should work (FK)
          p^.registers32:=p^.left^.registers32+1;}
       end;

    procedure first_proc_to_procvar(var p : ptree);

      begin
         { hmmm, I'am not sure if that is necessary (FK) }
         firstpass(p^.left);
         if codegenerror then
           exit;

         if (p^.left^.location.loc<>LOC_REFERENCE) then
           Message(cg_e_illegal_expression);

         p^.registers32:=p^.left^.registers32;
         if p^.registers32<1 then
           p^.registers32:=1;
         p^.location.loc:=LOC_REGISTER;
      end;

    procedure first_load_smallset(var p : ptree);

      begin
      end;

    procedure first_pchar_to_ansistring(var p : ptree);

      begin
         p^.location.loc:=LOC_REGISTER;
         if p^.registers32<1 then
           p^.registers32:=1;
      end;

    procedure first_ansistring_to_pchar(var p : ptree);

      begin
         p^.location.loc:=LOC_REGISTER;
         if p^.registers32<1 then
           p^.registers32:=1;
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
{***}

     function is_assignment_overloaded(from_def,to_def : pdef) : boolean;
       var
          passproc : pprocdef;
          convtyp : tconverttype;
       begin
          is_assignment_overloaded:=false;
          if assigned(overloaded_operators[assignment]) then
            passproc:=overloaded_operators[assignment]^.definition
          else
            exit;
          while passproc<>nil do
            begin
              if is_equal(passproc^.retdef,to_def) and
                 isconvertable(from_def,passproc^.para1^.data,convtyp,
                   ordconstn { nur Dummy},false ) then
                begin
                   is_assignment_overloaded:=true;
                   break;
                end;
              passproc:=passproc^.nextoverloaded;
            end;
       end;

    { Attention: do *** no ***  recursive call of firstpass }
    { because the child tree is always passed               }

  procedure firsttypeconv(var p : ptree);

    var
           hp : ptree;
           aprocdef : pprocdef;
           proctype : tdeftype;

    const
       firstconvert : array[tconverttype] of
         tfirstconvproc = (first_nothing,first_nothing,
                           first_bigger_smaller,first_nothing,first_bigger_smaller,
                           first_bigger_smaller,first_bigger_smaller,
                           first_bigger_smaller,first_bigger_smaller,
                           first_bigger_smaller,first_string_string,
                           first_cstring_charpointer,first_string_chararray,
                           first_array_to_pointer,first_pointer_to_array,
                           first_char_to_string,first_bigger_smaller,
                           first_bigger_smaller,first_bigger_smaller,
                           first_bigger_smaller,first_bigger_smaller,
                           first_bigger_smaller,first_bigger_smaller,
                           first_bigger_smaller,first_bigger_smaller,
                           first_bigger_smaller,first_bigger_smaller,
                           first_bigger_smaller,first_bigger_smaller,
                           first_bigger_smaller,first_bigger_smaller,
                           first_bigger_smaller,first_bigger_smaller,
                           first_bigger_smaller,first_bigger_smaller,
                           first_bool_int,first_int_bool,
                           first_int_real,first_real_fix,
                           first_fix_real,first_int_fix,first_real_real,
                           first_locmem,first_proc_to_procvar,
                           first_cchar_charpointer,
                           first_load_smallset,
                           first_ansistring_to_pchar,
                           first_pchar_to_ansistring);

    begin
       aprocdef:=nil;
       { if explicite type conversation, then run firstpass }
       if p^.explizit then
         firstpass(p^.left);

       if codegenerror then
         begin
           p^.resulttype:=generrordef;
           exit;
         end;

       if not assigned(p^.left^.resulttype) then
        begin
          codegenerror:=true;
          internalerror(52349);
          exit;
        end;

       { load the value_str from the left part }
       p^.registers32:=p^.left^.registers32;
       p^.registersfpu:=p^.left^.registersfpu;
{$ifdef SUPPORT_MMX}
       p^.registersmmx:=p^.left^.registersmmx;
{$endif}
       set_location(p^.location,p^.left^.location);

       { remove obsolete type conversions }
       if is_equal(p^.left^.resulttype,p^.resulttype) then
         begin
         { becuase is_equal only checks the basetype for sets we need to
           check here if we are loading a smallset into a normalset }
           if (p^.resulttype^.deftype=setdef) and
              (p^.left^.resulttype^.deftype=setdef) and
              (psetdef(p^.resulttype)^.settype<>smallset) and
              (psetdef(p^.left^.resulttype)^.settype=smallset) then
            begin
            { try to define the set as a normalset if it's a constant set }
              if p^.left^.treetype=setconstn then
               begin
                 p^.resulttype:=p^.left^.resulttype;
                 psetdef(p^.resulttype)^.settype:=normset
               end
              else
               p^.convtyp:=tc_load_smallset;
              exit;
            end
           else
            begin
              hp:=p;
              p:=p^.left;
              p^.resulttype:=hp^.resulttype;
              putnode(hp);
              exit;
            end;
         end;

       if is_assignment_overloaded(p^.left^.resulttype,p^.resulttype) then
         begin
            procinfo.flags:=procinfo.flags or pi_do_call;
            hp:=gencallnode(overloaded_operators[assignment],nil);
            hp^.left:=gencallparanode(p^.left,nil);
            putnode(p);
            p:=hp;
            firstpass(p);
            exit;
         end;
       if (not(isconvertable(p^.left^.resulttype,p^.resulttype,
           p^.convtyp,p^.left^.treetype,p^.explizit))) then
         begin
           {Procedures have a resulttype of voiddef and functions of their
           own resulttype. They will therefore always be incompatible with
           a procvar. Because isconvertable cannot check for procedures we
           use an extra check for them.}
           if (cs_tp_compatible in aktmoduleswitches) and
             ((is_procsym_load(p^.left) or is_procsym_call(p^.left)) and
             (p^.resulttype^.deftype=procvardef)) then
             begin
                { just a test: p^.explizit:=false; }
                if is_procsym_call(p^.left) then
                  begin
                     if p^.left^.right=nil then
                       begin
                          p^.left^.treetype:=loadn;
                          { are at same offset so this could be spared, but
                          it more secure to do it anyway }
                          p^.left^.symtableentry:=p^.left^.symtableprocentry;
                          p^.left^.resulttype:=pprocsym(p^.left^.symtableentry)^.definition;
                          aprocdef:=pprocdef(p^.left^.resulttype);
                       end
                     else
                       begin
                          p^.left^.right^.treetype:=loadn;
                          p^.left^.right^.symtableentry:=p^.left^.right^.symtableentry;
                          P^.left^.right^.resulttype:=pvarsym(p^.left^.symtableentry)^.definition;
                          hp:=p^.left^.right;
                          putnode(p^.left);
                          p^.left:=hp;
                          { should we do that ? }
                          firstpass(p^.left);
                          if not is_equal(p^.left^.resulttype,p^.resulttype) then
                            begin
                               Message(type_e_mismatch);
                               exit;
                            end
                          else
                            begin
                               hp:=p;
                               p:=p^.left;
                               p^.resulttype:=hp^.resulttype;
                               putnode(hp);
                               exit;
                            end;
                       end;
                  end
                else
                  begin
                     if p^.left^.treetype=addrn then
                       begin
                          hp:=p^.left;
                          p^.left:=p^.left^.left;
                          putnode(p^.left);
                       end
                     else
                       aprocdef:=pprocsym(p^.left^.symtableentry)^.definition;
                  end;

                p^.convtyp:=tc_proc2procvar;
                { Now check if the procedure we are going to assign to
                  the procvar,  is compatible with the procvar's type.
                  Did the original procvar support do such a check?
                  I can't find any.}
                { answer : is_equal works for procvardefs !! }
                { but both must be procvardefs, so we cheet  little }
                if assigned(aprocdef) then
                  begin
                    proctype:=aprocdef^.deftype;
                    aprocdef^.deftype:=procvardef;

                    if not is_equal(aprocdef,p^.resulttype) then
                      begin
                        aprocdef^.deftype:=proctype;
                        Message(type_e_mismatch);
                      end;
                    aprocdef^.deftype:=proctype;
                    firstconvert[p^.convtyp](p);
                  end
                else
                  Message(type_e_mismatch);
                exit;
             end
           else
             begin
                if p^.explizit then
                  begin
                     { boolean to byte are special because the
                       location can be different }
                     if (p^.resulttype^.deftype=orddef) and
                        (porddef(p^.resulttype)^.typ=u8bit) and
                        (p^.left^.resulttype^.deftype=orddef) and
                        (porddef(p^.left^.resulttype)^.typ=bool8bit) then
                       begin
                          p^.convtyp:=tc_bool_2_int;
                          firstconvert[p^.convtyp](p);
                          exit;
                       end;
                     { normal tc_equal-Konvertierung durchfÅhren }
                     p^.convtyp:=tc_equal;
                     { wenn AufzÑhltyp nach Ordinal konvertiert werden soll }
                     { dann AufzÑhltyp=s32bit                               }
                     if (p^.left^.resulttype^.deftype=enumdef) and
                        is_ordinal(p^.resulttype) then
                       begin
                          if p^.left^.treetype=ordconstn then
                            begin
                               hp:=genordinalconstnode(p^.left^.value,p^.resulttype);
                               disposetree(p);
                               firstpass(hp);
                               p:=hp;
                               exit;
                            end
                          else
                            begin
                               if not isconvertable(s32bitdef,p^.resulttype,p^.convtyp,
                               ordconstn { nur Dummy},false ) then
                                 Message(cg_e_illegal_type_conversion);
                            end;

                       end
                     { ordinal to enumeration }
                     else
                       if (p^.resulttype^.deftype=enumdef) and
                          is_ordinal(p^.left^.resulttype) then
                         begin
                            if p^.left^.treetype=ordconstn then
                              begin
                                 hp:=genordinalconstnode(p^.left^.value,p^.resulttype);
                                 disposetree(p);
                                 firstpass(hp);
                                 p:=hp;
                                 exit;
                              end
                            else
                              begin
                                 if not isconvertable(p^.left^.resulttype,s32bitdef,p^.convtyp,
                                   ordconstn { nur Dummy},false ) then
                                   Message(cg_e_illegal_type_conversion);
                              end;
                         end
                     {Are we typecasting an ordconst to a char?}
                     else
                       if is_equal(p^.resulttype,cchardef) and
                          is_ordinal(p^.left^.resulttype) then
                         begin
                            if p^.left^.treetype=ordconstn then
                              begin
                                 hp:=genordinalconstnode(p^.left^.value,p^.resulttype);
                                 firstpass(hp);
                                 disposetree(p);
                                 p:=hp;
                                 exit;
                              end
                            else
                              begin
                                 { this is wrong because it converts to a 4 byte long var !!
                                   if not isconvertable(p^.left^.resulttype,s32bitdef,p^.convtyp,ordconstn  nur Dummy ) then }
                                 if not isconvertable(p^.left^.resulttype,u8bitdef,
                                   p^.convtyp,ordconstn { nur Dummy},false ) then
                                   Message(cg_e_illegal_type_conversion);
                              end;
                         end
                     { only if the same size or formal def }
                     { why do we allow typecasting of voiddef ?? (PM) }
                     else
                       if not(
                             (p^.left^.resulttype^.deftype=formaldef) or
                             (p^.left^.resulttype^.size=p^.resulttype^.size) or
                             (is_equal(p^.left^.resulttype,voiddef)  and
                             (p^.left^.treetype=derefn))
                             ) then
                         Message(cg_e_illegal_type_conversion);
                     { the conversion into a strutured type is only }
                     { possible, if the source is no register         }
                     if ((p^.resulttype^.deftype in [recorddef,stringdef,arraydef]) or
                         ((p^.resulttype^.deftype=objectdef) and not(pobjectdef(p^.resulttype)^.isclass))
                        ) and (p^.left^.location.loc in [LOC_REGISTER,LOC_CREGISTER]) and
                        {it also works if the assignment is overloaded }
                        not is_assignment_overloaded(p^.left^.resulttype,p^.resulttype) then
                       Message(cg_e_illegal_type_conversion);
                end
              else
                Message(type_e_mismatch);
           end
         end
       else
         begin
            { ordinal contants can be directly converted }
            if (p^.left^.treetype=ordconstn) and is_ordinal(p^.resulttype) then
              begin
                 { perform range checking }
                 if not(p^.explizit and (cs_tp_compatible in aktmoduleswitches)) then
                   testrange(p^.resulttype,p^.left^.value);
                 hp:=genordinalconstnode(p^.left^.value,p^.resulttype);
                 disposetree(p);
                 firstpass(hp);
                 p:=hp;
                 exit;
              end;
            if p^.convtyp<>tc_equal then
              firstconvert[p^.convtyp](p);
         end;
    end;

    { *************** subroutine handling **************** }

    { protected field handling
      protected field can not appear in
      var parameters of function !!
      this can only be done after we have determined the
      overloaded function
      this is the reason why it is not in the parser
       PM }

    procedure test_protected_sym(sym : psym);

      begin
         if ((sym^.properties and sp_protected)<>0) and
           ((sym^.owner^.symtabletype=unitsymtable) or
            ((sym^.owner^.symtabletype=objectsymtable) and
           (pobjectdef(sym^.owner^.defowner)^.owner^.symtabletype=unitsymtable))) then
          Message(parser_e_cant_access_protected_member);
      end;

    procedure test_protected(p : ptree);

      begin
         if p^.treetype=loadn then
           begin
              test_protected_sym(p^.symtableentry);
           end
         else if p^.treetype=typeconvn then
           begin
              test_protected(p^.left);
           end
         else if p^.treetype=derefn then
           begin
              test_protected(p^.left);
           end
         else if p^.treetype=subscriptn then
           begin
              { test_protected(p^.left);
               Is a field of a protected var
                also protected ???  PM }
              test_protected_sym(p^.vs);
           end;
      end;

    procedure firstcallparan(var p : ptree;defcoll : pdefcoll);

      var store_valid : boolean;
          convtyp     : tconverttype;
      begin
         inc(parsing_para_level);
         if assigned(p^.right) then
           begin
              if defcoll=nil then
                firstcallparan(p^.right,nil)
              else
                firstcallparan(p^.right,defcoll^.next);
              p^.registers32:=p^.right^.registers32;
              p^.registersfpu:=p^.right^.registersfpu;
{$ifdef SUPPORT_MMX}
              p^.registersmmx:=p^.right^.registersmmx;
{$endif}
           end;
         if defcoll=nil then
           begin
              { this breaks typeconversions in write !!! (PM) }
              {if not(assigned(p^.resulttype)) then }
              if not(assigned(p^.resulttype)) or
                (p^.left^.treetype=typeconvn) then
                firstpass(p^.left);
              {else
                exit; this broke the
                value of registers32 !! }

              if codegenerror then
                begin
                   dec(parsing_para_level);
                   exit;
                end;

              p^.resulttype:=p^.left^.resulttype;
           end
         { if we know the routine which is called, then the type }
         { conversions are inserted                              }
         else
           begin
               if count_ref then
                 begin
                    store_valid:=must_be_valid;
                    if (defcoll^.paratyp=vs_var) then
                      test_protected(p^.left);
                    if (defcoll^.paratyp<>vs_var) then
                      must_be_valid:=true
                    else
                      must_be_valid:=false;
                    { here we must add something for the implicit type }
                    { conversion from array of char to pchar }
                    if isconvertable(p^.left^.resulttype,defcoll^.data,convtyp,
                      p^.left^.treetype,false) then
                      if convtyp=tc_array_to_pointer then
                        must_be_valid:=false;
                    firstpass(p^.left);
                    must_be_valid:=store_valid;
                 end;
              if not(is_shortstring(p^.left^.resulttype) and
                     is_shortstring(defcoll^.data)) and
                     (defcoll^.data^.deftype<>formaldef) then
                begin
                   if (defcoll^.paratyp=vs_var) and
                   { allows conversion from word to integer and
                     byte to shortint }
                     (not(
                        (p^.left^.resulttype^.deftype=orddef) and
                        (defcoll^.data^.deftype=orddef) and
                        (p^.left^.resulttype^.size=defcoll^.data^.size)
                         ) and
                   { an implicit pointer conversion is allowed }
                     not(
                        (p^.left^.resulttype^.deftype=pointerdef) and
                        (defcoll^.data^.deftype=pointerdef)
                         ) and
                   { child classes can be also passed }
                     not(
                        (p^.left^.resulttype^.deftype=objectdef) and
                        (defcoll^.data^.deftype=objectdef) and
                        pobjectdef(p^.left^.resulttype)^.isrelated(pobjectdef(defcoll^.data))
                        ) and

                   { an implicit file conversion is also allowed }
                   { from a typed file to an untyped one           }
                     not(
                        (p^.left^.resulttype^.deftype=filedef) and
                        (defcoll^.data^.deftype=filedef) and
                        (pfiledef(defcoll^.data)^.filetype = ft_untyped) and
                        (pfiledef(p^.left^.resulttype)^.filetype = ft_typed)
                         ) and
                     not(is_equal(p^.left^.resulttype,defcoll^.data))) then
                       Message(parser_e_call_by_ref_without_typeconv);
                   { don't generate an type conversion for open arrays   }
                   { else we loss the ranges                             }
                   if not(is_open_array(defcoll^.data)) then
                     begin
                        p^.left:=gentypeconvnode(p^.left,defcoll^.data);
                        firstpass(p^.left);
                     end;
                   if codegenerror then
                     begin
                        dec(parsing_para_level);
                        exit;
                     end;
                end;
              { check var strings }
              if (cs_strict_var_strings in aktlocalswitches) and
                 is_shortstring(p^.left^.resulttype) and
                 is_shortstring(defcoll^.data) and
                 (defcoll^.paratyp=vs_var) and
                 not(is_equal(p^.left^.resulttype,defcoll^.data)) then
                 Message(type_e_strict_var_string_violation);
              { Variablen, die call by reference Åbergeben werden, }
              { kînnen nicht in ein Register kopiert werden       }
              { is this usefull here ? }
              { this was missing in formal parameter list   }
              if defcoll^.paratyp=vs_var then
                make_not_regable(p^.left);

              p^.resulttype:=defcoll^.data;
           end;
         if p^.left^.registers32>p^.registers32 then
           p^.registers32:=p^.left^.registers32;
         if p^.left^.registersfpu>p^.registersfpu then
           p^.registersfpu:=p^.left^.registersfpu;
{$ifdef SUPPORT_MMX}
         if p^.left^.registersmmx>p^.registersmmx then
           p^.registersmmx:=p^.left^.registersmmx;
{$endif SUPPORT_MMX}
         dec(parsing_para_level);
      end;

    procedure firstcalln(var p : ptree);

      type
         pprocdefcoll = ^tprocdefcoll;

         tprocdefcoll = record
            data : pprocdef;
            nextpara : pdefcoll;
            firstpara : pdefcoll;
            next : pprocdefcoll;
         end;

      var
         hp,procs,hp2 : pprocdefcoll;
         pd : pprocdef;
         actprocsym : pprocsym;
         def_from,def_to,conv_to : pdef;
         pt,inlinecode : ptree;
         exactmatch,inlined : boolean;
         paralength,l : longint;
         pdc : pdefcoll;

         { only Dummy }
         hcvt : tconverttype;
         regi : tregister;
         store_valid, old_count_ref : boolean;

      { types.is_equal can't handle a formaldef ! }
      function is_equal(def1,def2 : pdef) : boolean;

        begin
           { all types can be passed to a formaldef }
           is_equal:=(def1^.deftype=formaldef) or
             (assigned(def2) and types.is_equal(def1,def2))
           { to support ansi/long/wide strings in a proper way }
           { string and string[10] are assumed as equal        }
           { when searching the correct overloaded procedure   }
             or
             (assigned(def1) and assigned(def2) and
              (def1^.deftype=stringdef) and (def2^.deftype=stringdef) and
              (pstringdef(def1)^.string_typ=pstringdef(def2)^.string_typ)
             )
             ;
        end;

      function is_in_limit(def_from,def_to : pdef) : boolean;

        begin
           is_in_limit:=(def_from^.deftype = orddef) and
                        (def_to^.deftype = orddef) and
                        (porddef(def_from)^.low>porddef(def_to)^.low) and
                        (porddef(def_from)^.high<porddef(def_to)^.high);
        end;

      var
        is_const : boolean;
      begin
         { release registers! }
         { if procdefinition<>nil then we called firstpass already }
         { it seems to be bad because of the registers }
         { at least we can avoid the overloaded search !! }
         procs:=nil;
         { made this global for disposing !! }
         store_valid:=must_be_valid;
         must_be_valid:=false;

         inlined:=false;
         if assigned(p^.procdefinition) and
            ((p^.procdefinition^.options and poinline)<>0) then
           begin
              inlinecode:=p^.right;
              if assigned(inlinecode) then
                begin
                   inlined:=true;
                   p^.procdefinition^.options:=p^.procdefinition^.options and (not poinline);
                end;
              p^.right:=nil;
           end;
         { procedure variable ? }
         if assigned(p^.right) then
           begin
              { procedure does a call }
              procinfo.flags:=procinfo.flags or pi_do_call;

              { calc the correture value for the register }
{$ifdef i386}
              for regi:=R_EAX to R_EDI do
                inc(reg_pushes[regi],t_times*2);
{$endif}
{$ifdef m68k}
              for regi:=R_D0 to R_A6 do
                inc(reg_pushes[regi],t_times*2);
{$endif}
              { calculate the type of the parameters }
              if assigned(p^.left) then
                begin
                   old_count_ref:=count_ref;
                   count_ref:=false;
                   firstcallparan(p^.left,nil);
                   count_ref:=old_count_ref;
                   if codegenerror then
                     exit;
                end;
              firstpass(p^.right);

              { check the parameters }
              pdc:=pprocvardef(p^.right^.resulttype)^.para1;
              pt:=p^.left;
              while assigned(pdc) and assigned(pt) do
                begin
                   pt:=pt^.right;
                   pdc:=pdc^.next;
                end;
              if assigned(pt) or assigned(pdc) then
                Message(parser_e_illegal_parameter_list);
              { insert type conversions }
              if assigned(p^.left) then
                begin
                   old_count_ref:=count_ref;
                   count_ref:=true;
                   firstcallparan(p^.left,pprocvardef(p^.right^.resulttype)^.para1);
                   count_ref:=old_count_ref;
                   if codegenerror then
                     exit;
                end;
              p^.resulttype:=pprocvardef(p^.right^.resulttype)^.retdef;
              { this was missing, leads to a bug below if
                the procvar is a function }
              p^.procdefinition:=pprocdef(p^.right^.resulttype);
           end
         else
         { not a procedure variable }
           begin
              { determine the type of the parameters }
              if assigned(p^.left) then
                begin
                   old_count_ref:=count_ref;
                   count_ref:=false;
                   store_valid:=must_be_valid;
                   must_be_valid:=false;
                   firstcallparan(p^.left,nil);
                   count_ref:=old_count_ref;
                   must_be_valid:=store_valid;
                   if codegenerror then
                     exit;
                end;

              { do we know the procedure to call ? }
              if not(assigned(p^.procdefinition)) then
                begin
                   actprocsym:=pprocsym(p^.symtableprocentry);
                   { determine length of parameter list }
                   pt:=p^.left;
                   paralength:=0;
                   while assigned(pt) do
                     begin
                        inc(paralength);
                        pt:=pt^.right;
                     end;

                   { link all procedures which have the same # of parameters }
                   pd:=actprocsym^.definition;
                   while assigned(pd) do
                     begin
                        { we should also check that the overloaded function
                        has been declared in a unit that is in the uses !! }
                        { pd^.owner should be in the symtablestack !! }
                        { Laenge der deklarierten Parameterliste feststellen: }
                        { not necessary why nextprocsym field }
                        {st:=symtablestack;
                        if (pd^.owner^.symtabletype<>objectsymtable) then
                          while assigned(st) do
                            begin
                               if (st=pd^.owner) then break;
                               st:=st^.next;
                            end;
                        if assigned(st) then }
                          begin
                             pdc:=pd^.para1;
                             l:=0;
                             while assigned(pdc) do
                               begin
                                  inc(l);
                                  pdc:=pdc^.next;
                               end;
                             { only when the # of parameter are equal }
                             if l=paralength then
                               begin
                                  new(hp);
                                  hp^.data:=pd;
                                  hp^.next:=procs;
                                  hp^.nextpara:=pd^.para1;
                                  hp^.firstpara:=pd^.para1;
                                  procs:=hp;
                               end;
                          end;
                        pd:=pd^.nextoverloaded;
{$ifdef CHAINPROCSYMS}
                        if (pd=nil) and not (p^.unit_specific) then
                          begin
                             actprocsym:=actprocsym^.nextprocsym;
                             if assigned(actprocsym) then
                               pd:=actprocsym^.definition;
                          end;
{$endif CHAINPROCSYMS}
                     end;

                   { no procedures found? then there is something wrong
                     with the parameter size }
                   if not assigned(procs) and
                      ((parsing_para_level=0) or assigned(p^.left)) then
                    begin
                       Message(parser_e_wrong_parameter_size);
                       actprocsym^.write_parameter_lists;
                       exit;
                    end;

                   { now we can compare parameter after parameter }
                   pt:=p^.left;
                   while assigned(pt) do
                     begin
                        { matches a parameter of one procedure exact ? }
                        exactmatch:=false;
                        hp:=procs;
                        while assigned(hp) do
                          begin
                             if is_equal(hp^.nextpara^.data,pt^.resulttype) then
                               begin
                                  if hp^.nextpara^.data=pt^.resulttype then
                                    begin
                                       pt^.exact_match_found:=true;
                                       hp^.nextpara^.argconvtyp:=act_exact;
                                    end
                                  else
                                    hp^.nextpara^.argconvtyp:=act_equal;
                                  exactmatch:=true;
                               end
                             else
                               hp^.nextpara^.argconvtyp:=act_convertable;
                             hp:=hp^.next;
                          end;

                        { .... if yes, del all the other procedures }
                        if exactmatch then
                          begin
                             { the first .... }
                             while (assigned(procs)) and not(is_equal(procs^.nextpara^.data,pt^.resulttype)) do
                               begin
                                  hp:=procs^.next;
                                  dispose(procs);
                                  procs:=hp;
                               end;
                             { and the others }
                             hp:=procs;
                             while (assigned(hp)) and assigned(hp^.next) do
                               begin
                                  if not(is_equal(hp^.next^.nextpara^.data,pt^.resulttype)) then
                                    begin
                                       hp2:=hp^.next^.next;
                                       dispose(hp^.next);
                                       hp^.next:=hp2;
                                    end
                                  else
                                    hp:=hp^.next;
                               end;
                          end
                        { when a parameter matches exact, remove all procs
                          which need typeconvs }
                        else
                          begin
                             { the first... }
                             while (assigned(procs)) and
                               not(isconvertable(pt^.resulttype,procs^.nextpara^.data,
                                 hcvt,pt^.left^.treetype,false)) do
                               begin
                                  hp:=procs^.next;
                                  dispose(procs);
                                  procs:=hp;
                               end;
                             { and the others }
                             hp:=procs;
                             while (assigned(hp)) and assigned(hp^.next) do
                               begin
                                  if not(isconvertable(pt^.resulttype,hp^.next^.nextpara^.data,
                                    hcvt,pt^.left^.treetype,false)) then
                                    begin
                                       hp2:=hp^.next^.next;
                                       dispose(hp^.next);
                                       hp^.next:=hp2;
                                    end
                                  else
                                    hp:=hp^.next;
                               end;
                          end;
                        { update nextpara for all procedures }
                        hp:=procs;
                        while assigned(hp) do
                          begin
                             hp^.nextpara:=hp^.nextpara^.next;
                             hp:=hp^.next;
                          end;
                        { load next parameter }
                        pt:=pt^.right;
                     end;

                   if not assigned(procs) then
                    begin
                      { there is an error, must be wrong type, because
                        wrong size is already checked (PFV) }
                      if (parsing_para_level=0) or (p^.left<>nil) then
                       begin
                          Message(parser_e_wrong_parameter_type);
                          actprocsym^.write_parameter_lists;
                          exit;
                       end
                      else
                       begin
                         { try to convert to procvar }
                         p^.treetype:=loadn;
                         p^.resulttype:=pprocsym(p^.symtableprocentry)^.definition;
                         p^.symtableentry:=p^.symtableprocentry;
                         p^.is_first:=false;
                         p^.disposetyp:=dt_nothing;
                         firstpass(p);
                         exit;
                       end;
                     end;

                   { if there are several choices left then for orddef }
                   { if a type is totally included in the other        }
                   { we don't fear an overflow ,                       }
                   { so we can do as if it is an exact match           }
                   { this will convert integer to longint              }
                   { rather than to words                              }
                   { conversion of byte to integer or longint          }
                   {would still not be solved                          }
                   if assigned(procs^.next) then
                     begin
                        hp:=procs;
                        while assigned(hp) do
                          begin
                            hp^.nextpara:=hp^.firstpara;
                            hp:=hp^.next;
                          end;
                        pt:=p^.left;
                        while assigned(pt) do
                          begin
                             { matches a parameter of one procedure exact ? }
                             exactmatch:=false;
                             def_from:=pt^.resulttype;
                             hp:=procs;
                             while assigned(hp) do
                               begin
                                  if not is_equal(hp^.nextpara^.data,pt^.resulttype) then
                                    begin
                                       def_to:=hp^.nextpara^.data;
                                       if ((def_from^.deftype=orddef) and (def_to^.deftype=orddef)) and
                                         (is_in_limit(def_from,def_to) or
                                         ((hp^.nextpara^.paratyp=vs_var) and
                                         (def_from^.size=def_to^.size))) then
                                         begin
                                            exactmatch:=true;
                                            conv_to:=def_to;
                                         end;
                                    end;
                                  hp:=hp^.next;
                               end;

                             { .... if yes, del all the other procedures }
                             if exactmatch then
                               begin
                                  { the first .... }
                                  while (assigned(procs)) and not(is_in_limit(def_from,procs^.nextpara^.data)) do
                                    begin
                                       hp:=procs^.next;
                                       dispose(procs);
                                       procs:=hp;
                                    end;
                                  { and the others }
                                  hp:=procs;
                                  while (assigned(hp)) and assigned(hp^.next) do
                                    begin
                                       if not(is_in_limit(def_from,hp^.next^.nextpara^.data)) then
                                         begin
                                            hp2:=hp^.next^.next;
                                            dispose(hp^.next);
                                            hp^.next:=hp2;
                                         end
                                       else
                                         begin
                                           def_to:=hp^.next^.nextpara^.data;
                                           if (conv_to^.size>def_to^.size) or
                                              ((porddef(conv_to)^.low<porddef(def_to)^.low) and
                                              (porddef(conv_to)^.high>porddef(def_to)^.high)) then
                                             begin
                                                hp2:=procs;
                                                procs:=hp;
                                                conv_to:=def_to;
                                                dispose(hp2);
                                             end
                                           else
                                             hp:=hp^.next;
                                         end;
                                    end;
                               end;
                             { update nextpara for all procedures }
                             hp:=procs;
                             while assigned(hp) do
                               begin
                                  hp^.nextpara:=hp^.nextpara^.next;
                                  hp:=hp^.next;
                               end;
                             pt:=pt^.right;
                          end;
                     end;
                   { let's try to eliminate equal is exact is there }
                   {if assigned(procs^.next) then
                     begin
                        pt:=p^.left;
                        while assigned(pt) do
                          begin
                             if pt^.exact_match_found then
                               begin
                                  hp:=procs;
                                  while (assigned(procs)) and (procs^.nextpara^.data<>pt^.resulttype) do
                                    begin
                                       hp:=procs^.next;
                                       dispose(procs);
                                       procs:=hp;
                                    end;
                               end;
                             pt:=pt^.right;
                          end;
                     end; }

{$ifndef CHAINPROCSYMS}
                   if assigned(procs^.next) then
                     begin
                        Message(cg_e_cant_choose_overload_function);
                        actprocsym^.write_parameter_lists;
                     end;
{$else CHAINPROCSYMS}
                   if assigned(procs^.next) then
                     { if the last retained is the only one }
                     { from a unit it is OK              PM  }
                     { the last is the one coming from the first symtable }
                     { as the diff defcoll are inserted in front }
                     begin
                        hp2:=procs;
                        while assigned(hp2^.next) and assigned(hp2^.next^.next) do
                          hp2:=hp2^.next;
                        if (hp2^.data^.owner<>hp2^.next^.data^.owner) then
                          begin
                             hp:=procs^.next;
                             {hp2 is the correct one }
                             hp2:=hp2^.next;
                             while hp<>hp2 do
                               begin
                                 dispose(procs);
                                 procs:=hp;
                                 hp:=procs^.next;
                               end;
                             procs:=hp2;
                          end
                        else
                           begin
                              Message(cg_e_cant_choose_overload_function);
                              actprocsym^.write_parameter_lists;
                              error(too_much_matches);
                           end;
                     end;
{$endif CHAINPROCSYMS}
     {$ifdef UseBrowser}
                   if make_ref then
                     begin
                        procs^.data^.lastref:=new(pref,init(procs^.data^.lastref,@p^.fileinfo));
                     end;
     {$endif UseBrowser}

                   p^.procdefinition:=procs^.data;
                   p^.resulttype:=procs^.data^.retdef;
                   { big error for with statements
                   p^.symtableproc:=p^.procdefinition^.owner; }
                   p^.location.loc:=LOC_MEM;
{$ifdef CHAINPROCSYMS}
                   { object with method read;
                     call to read(x) will be a usual procedure call }
                   if assigned(p^.methodpointer) and
                     (p^.procdefinition^._class=nil) then
                     begin
                        { not ok for extended }
                        case p^.methodpointer^.treetype of
                           typen,hnewn : fatalerror(no_para_match);
                        end;
                        disposetree(p^.methodpointer);
                        p^.methodpointer:=nil;
                     end;
{$endif CHAINPROCSYMS}
               end;{ end of procedure to call determination }

              is_const:=((p^.procdefinition^.options and pointernconst)<>0) and
                         (p^.left^.left^.treetype in [realconstn,ordconstn]);
              { handle predefined procedures }
              if ((p^.procdefinition^.options and pointernproc)<>0) or is_const then
                begin
                   { settextbuf needs two args }
                   if assigned(p^.left^.right) then
                     pt:=geninlinenode(pprocdef(p^.procdefinition)^.extnumber,is_const,p^.left)
                   else
                     begin
                        pt:=geninlinenode(pprocdef(p^.procdefinition)^.extnumber,is_const,p^.left^.left);
                        putnode(p^.left);
                     end;
                   putnode(p);
                   firstpass(pt);
                   p:=pt;

                   must_be_valid:=store_valid;
                   if codegenerror then
                     exit;

                   dispose(procs);
                   exit;
                end
              else
                { no intern procedure => we do a call }
              { calc the correture value for the register }
              { handle predefined procedures }
              if (p^.procdefinition^.options and poinline)<>0 then
                begin
                   if assigned(p^.methodpointer) then
                     Message(cg_e_unable_inline_object_methods);
                   if assigned(p^.right) and (p^.right^.treetype<>procinlinen) then
                     Message(cg_e_unable_inline_procvar);
                   { p^.treetype:=procinlinen; }
                   if not assigned(p^.right) then
                     begin
                        if assigned(p^.procdefinition^.code) then
                          inlinecode:=genprocinlinenode(p,ptree(p^.procdefinition^.code))
                        else
                          Message(cg_e_no_code_for_inline_stored);
                        if assigned(inlinecode) then
                          begin
                             { consider it has not inlined if called
                               again inside the args }
                             p^.procdefinition^.options:=p^.procdefinition^.options and (not poinline);
                             firstpass(inlinecode);
                             inlined:=true;
                          end;
                     end;
                end
              else
                procinfo.flags:=procinfo.flags or pi_do_call;

              { work trough all parameters to insert the type conversions }
              { !!! done now after internproc !! (PM) }
              if assigned(p^.left) then
                begin
                   old_count_ref:=count_ref;
                   count_ref:=true;
                   firstcallparan(p^.left,p^.procdefinition^.para1);
                   count_ref:=old_count_ref;
                end;
{$ifdef i386}
              for regi:=R_EAX to R_EDI do
                begin
                   if (p^.procdefinition^.usedregisters and ($80 shr word(regi)))<>0 then
                     inc(reg_pushes[regi],t_times*2);
                end;
{$endif}
{$ifdef m68k}
             for regi:=R_D0 to R_A6 do
               begin
                  if (p^.procdefinition^.usedregisters and ($800 shr word(regi)))<>0 then
                    inc(reg_pushes[regi],t_times*2);
               end;
{$endif}
           end;
         { ensure that the result type is set }
         p^.resulttype:=p^.procdefinition^.retdef;
         { get a register for the return value }
         if (p^.resulttype<>pdef(voiddef)) then
           begin
              if (p^.procdefinition^.options and poconstructor)<>0 then
                begin
                   { extra handling of classes }
                   { p^.methodpointer should be assigned! }
                   if assigned(p^.methodpointer) and assigned(p^.methodpointer^.resulttype) and
                     (p^.methodpointer^.resulttype^.deftype=classrefdef) then
                     begin
                        p^.location.loc:=LOC_REGISTER;
                        p^.registers32:=1;
                        { the result type depends on the classref }
                        p^.resulttype:=pclassrefdef(p^.methodpointer^.resulttype)^.definition;
                     end
                  { a object constructor returns the result with the flags }
                   else
                     p^.location.loc:=LOC_FLAGS;
                end
              else
                begin
{$ifdef SUPPORT_MMX}
                   if (cs_mmx in aktlocalswitches) and
                     is_mmx_able_array(p^.resulttype) then
                     begin
                        p^.location.loc:=LOC_MMXREGISTER;
                        p^.registersmmx:=1;
                     end
                   else
{$endif SUPPORT_MMX}
                   if ret_in_acc(p^.resulttype) then
                     begin
                        p^.location.loc:=LOC_REGISTER;
                        p^.registers32:=1;
                     end
                   else if (p^.resulttype^.deftype=floatdef) then
                     begin
                        p^.location.loc:=LOC_FPU;
                        p^.registersfpu:=1;
                     end
                end;
           end;

{$ifdef StoreFPULevel}
         { a fpu can be used in any procedure !! }
         p^.registersfpu:=p^.procdefinition^.fpu_used;
{$endif StoreFPULevel}
         { if this is a call to a method calc the registers }
         if (p^.methodpointer<>nil) then
           begin
              case p^.methodpointer^.treetype of
                { but only, if this is not a supporting node }
                typen,hnewn : ;
                else
                  begin
                     { R.Assign is not a constructor !!! }
                     { but for R^.Assign, R must be valid !! }
                     if ((p^.procdefinition^.options and poconstructor) <> 0) or
                        ((p^.methodpointer^.treetype=loadn) and
                        ((pobjectdef(p^.methodpointer^.resulttype)^.options and oo_hasvirtual) = 0)) then
                       must_be_valid:=false
                     else
                       must_be_valid:=true;
                     firstpass(p^.methodpointer);
                     p^.registersfpu:=max(p^.methodpointer^.registersfpu,p^.registersfpu);
                     p^.registers32:=max(p^.methodpointer^.registers32,p^.registers32);
{$ifdef SUPPORT_MMX}
                     p^.registersmmx:=max(p^.methodpointer^.registersmmx,p^.registersmmx);
{$endif SUPPORT_MMX}
                  end;
              end;
           end;

         if inlined then
           begin
              p^.right:=inlinecode;
              p^.procdefinition^.options:=p^.procdefinition^.options or  poinline;
           end;
         { determine the registers of the procedure variable }
         { is this OK for inlined procs also ?? (PM)         }
         if assigned(p^.right) then
           begin
              p^.registersfpu:=max(p^.right^.registersfpu,p^.registersfpu);
              p^.registers32:=max(p^.right^.registers32,p^.registers32);
{$ifdef SUPPORT_MMX}
              p^.registersmmx:=max(p^.right^.registersmmx,p^.registersmmx);
{$endif SUPPORT_MMX}
           end;
         { determine the registers of the procedure }
         if assigned(p^.left) then
           begin
              p^.registersfpu:=max(p^.left^.registersfpu,p^.registersfpu);
              p^.registers32:=max(p^.left^.registers32,p^.registers32);
{$ifdef SUPPORT_MMX}
              p^.registersmmx:=max(p^.left^.registersmmx,p^.registersmmx);
{$endif SUPPORT_MMX}
           end;
         if assigned(procs) then
           dispose(procs);
         must_be_valid:=store_valid;
      end;

    procedure firstfuncret(var p : ptree);

      begin
         p^.resulttype:=p^.retdef;
         p^.location.loc:=LOC_REFERENCE;
         if ret_in_param(p^.retdef) or
            (@procinfo<>pprocinfo(p^.funcretprocinfo)) then
           p^.registers32:=1;
         { no claim if setting higher return value_str }
         if must_be_valid and
            (@procinfo=pprocinfo(p^.funcretprocinfo)) and
            not procinfo.funcret_is_valid then
           Message(sym_w_function_result_not_set);
         if count_ref then
           pprocinfo(p^.funcretprocinfo)^.funcret_is_valid:=true;
      end;


    { intern inline suborutines }
    procedure firstinline(var p : ptree);

      var
         vl      : longint;
         vr      : bestreal;
         hp,hpp  : ptree;
         store_count_ref,
         isreal,
         dowrite,
         store_valid,
         file_is_typed : boolean;

      procedure do_lowhigh(adef : pdef);

        var
           v : longint;
           enum : penumsym;

        begin
           case Adef^.deftype of
             orddef:
               begin
                  if p^.inlinenumber=in_low_x then
                    v:=porddef(Adef)^.low
                  else
                    v:=porddef(Adef)^.high;
                  hp:=genordinalconstnode(v,adef);
                  firstpass(hp);
                  disposetree(p);
                  p:=hp;
               end;
             enumdef:
               begin
                  enum:=Penumdef(Adef)^.first;
                  if p^.inlinenumber=in_high_x then
                    while enum^.next<>nil do
                      enum:=enum^.next;
                  hp:=genenumnode(enum);
                  disposetree(p);
                  p:=hp;
               end
           end;
        end;

      begin
         store_valid:=must_be_valid;
         store_count_ref:=count_ref;
         count_ref:=false;
         if not (p^.inlinenumber in [in_read_x,in_readln_x,in_sizeof_x,
            in_typeof_x,in_ord_x,in_str_x_string,
            in_reset_typedfile,in_rewrite_typedfile]) then
           must_be_valid:=true
         else
           must_be_valid:=false;
         { if we handle writeln; p^.left contains no valid address }
         if assigned(p^.left) then
           begin
              if p^.left^.treetype=callparan then
                firstcallparan(p^.left,nil)
              else
                firstpass(p^.left);
              left_right_max(p);
              set_location(p^.location,p^.left^.location);
           end;
         { handle intern constant functions in separate case }
         if p^.inlineconst then
          begin
            isreal:=(p^.left^.treetype=realconstn);
            vl:=p^.left^.value;
            vr:=p^.left^.value_real;
            case p^.inlinenumber of
         in_const_trunc : begin
                            if isreal then
                             hp:=genordinalconstnode(trunc(vr),s32bitdef)
                            else
                             hp:=genordinalconstnode(trunc(vl),s32bitdef);
                          end;
         in_const_round : begin
                            if isreal then
                             hp:=genordinalconstnode(round(vr),s32bitdef)
                            else
                             hp:=genordinalconstnode(round(vl),s32bitdef);
                          end;
          in_const_frac : begin
                            if isreal then
                             hp:=genrealconstnode(frac(vr))
                            else
                             hp:=genrealconstnode(frac(vl));
                          end;
           in_const_int : begin
                            if isreal then
                             hp:=genrealconstnode(int(vr))
                            else
                             hp:=genrealconstnode(int(vl));
                          end;
           in_const_abs : begin
                            if isreal then
                             hp:=genrealconstnode(abs(vr))
                            else
                             hp:=genordinalconstnode(abs(vl),p^.left^.resulttype);
                          end;
           in_const_sqr : begin
                            if isreal then
                             hp:=genrealconstnode(sqr(vr))
                            else
                             hp:=genordinalconstnode(sqr(vl),p^.left^.resulttype);
                          end;
           in_const_odd : begin
                            if isreal then
                             Message(type_e_integer_expr_expected)
                            else
                             hp:=genordinalconstnode(byte(odd(vl)),booldef);
                          end;
     in_const_swap_word : begin
                            if isreal then
                             Message(type_e_integer_expr_expected)
                            else
                             hp:=genordinalconstnode((vl and $ff) shl 8+(vl shr 8),p^.left^.resulttype);
                          end;
     in_const_swap_long : begin
                            if isreal then
                             Message(type_e_mismatch)
                            else
                             hp:=genordinalconstnode((vl and $ffff) shl 16+(vl shr 16),p^.left^.resulttype);
                          end;
           in_const_ptr : begin
                            if isreal then
                             Message(type_e_mismatch)
                            else
                             hp:=genordinalconstnode(vl,voidpointerdef);
                          end;
            else
              internalerror(88);
            end;
            disposetree(p);
            firstpass(hp);
            p:=hp;
          end
         else
          begin
            case p^.inlinenumber of
             in_lo_long,in_hi_long,
             in_lo_word,in_hi_word:
               begin
                  if p^.registers32<1 then
                    p^.registers32:=1;
                  if p^.inlinenumber in [in_lo_word,in_hi_word] then
                    p^.resulttype:=u8bitdef
                  else
                    p^.resulttype:=u16bitdef;
                  p^.location.loc:=LOC_REGISTER;
                  if not is_integer(p^.left^.resulttype) then
                    Message(type_e_mismatch)
                  else
                    begin
                      if p^.left^.treetype=ordconstn then
                       begin
                         case p^.inlinenumber of
                          in_lo_word : hp:=genordinalconstnode(p^.left^.value and $ff,p^.left^.resulttype);
                          in_hi_word : hp:=genordinalconstnode(p^.left^.value shr 8,p^.left^.resulttype);
                          in_lo_long : hp:=genordinalconstnode(p^.left^.value and $ffff,p^.left^.resulttype);
                          in_hi_long : hp:=genordinalconstnode(p^.left^.value shr 16,p^.left^.resulttype);
                         end;
                         disposetree(p);
                         firstpass(hp);
                         p:=hp;
                       end;
                    end;
               end;
             in_sizeof_x:
               begin
                  if p^.registers32<1 then
                    p^.registers32:=1;
                  p^.resulttype:=s32bitdef;
                  p^.location.loc:=LOC_REGISTER;
               end;
             in_typeof_x:
               begin
                  if p^.registers32<1 then
                    p^.registers32:=1;
                  p^.location.loc:=LOC_REGISTER;
                  p^.resulttype:=voidpointerdef;
               end;
             in_ord_x:
               begin
                  if (p^.left^.treetype=ordconstn) then
                    begin
                       hp:=genordinalconstnode(p^.left^.value,s32bitdef);
                       disposetree(p);
                       p:=hp;
                       firstpass(p);
                    end
                  else
                    begin
                       if (p^.left^.resulttype^.deftype=orddef) then
                         if (porddef(p^.left^.resulttype)^.typ in [uchar,bool8bit]) then
                           begin
                              if porddef(p^.left^.resulttype)^.typ=bool8bit then
                                begin
                                   hp:=gentypeconvnode(p^.left,u8bitdef);
                                   putnode(p);
                                   p:=hp;
                                   p^.convtyp:=tc_bool_2_int;
                                   p^.explizit:=true;
                                   firstpass(p);
                                end
                              else
                                begin
                                   hp:=gentypeconvnode(p^.left,u8bitdef);
                                   putnode(p);
                                   p:=hp;
                                   p^.explizit:=true;
                                   firstpass(p);
                                end;
                           end
                         { can this happen ? }
                         else if (porddef(p^.left^.resulttype)^.typ=uvoid) then
                           Message(type_e_mismatch)
                         else
                           { all other orddef need no transformation }
                           begin
                              hp:=p^.left;
                              putnode(p);
                              p:=hp;
                           end
                       else if (p^.left^.resulttype^.deftype=enumdef) then
                         begin
                            hp:=gentypeconvnode(p^.left,s32bitdef);
                            putnode(p);
                            p:=hp;
                            p^.explizit:=true;
                            firstpass(p);
                         end
                       else
                         begin
                            { can anything else be ord() ?}
                            Message(type_e_mismatch);
                         end;
                    end;
               end;
             in_chr_byte:
               begin
                  hp:=gentypeconvnode(p^.left,cchardef);
                  putnode(p);
                  p:=hp;
                  p^.explizit:=true;
                  firstpass(p);
               end;
             in_length_string:
               begin
{$ifdef UseAnsiString}
                  if is_ansistring(p^.left^.resulttype) then
                    p^.resulttype:=s32bitdef
                  else
{$endif UseAnsiString}
                    p^.resulttype:=u8bitdef;
                  { wer don't need string conversations here }
                  if (p^.left^.treetype=typeconvn) and
                     (p^.left^.left^.resulttype^.deftype=stringdef) then
                    begin
                       hp:=p^.left^.left;
                       putnode(p^.left);
                       p^.left:=hp;
                    end;

                  { evalutes length of constant strings direct }
                  if (p^.left^.treetype=stringconstn) then
                    begin
{$ifdef UseAnsiString}
                       hp:=genordinalconstnode(p^.left^.length,s32bitdef);
{$else UseAnsiString}
                       hp:=genordinalconstnode(length(p^.left^.value_str^),s32bitdef);
{$endif UseAnsiString}
                       disposetree(p);
                       firstpass(hp);
                       p:=hp;
                    end;
               end;
             in_assigned_x:
               begin
                  p^.resulttype:=booldef;
                  p^.location.loc:=LOC_FLAGS;
               end;
             in_pred_x,
             in_succ_x:
               begin
                  inc(p^.registers32);
                  p^.resulttype:=p^.left^.resulttype;
                  p^.location.loc:=LOC_REGISTER;
                  if not is_ordinal(p^.resulttype) then
                    Message(type_e_ordinal_expr_expected)
                  else
                    begin
                      if (p^.resulttype^.deftype=enumdef) and
                         (penumdef(p^.resulttype)^.has_jumps) then
                        Message(type_e_succ_and_pred_enums_with_assign_not_possible)
                      else
                        if p^.left^.treetype=ordconstn then
                         begin
                           if p^.inlinenumber=in_succ_x then
                             hp:=genordinalconstnode(p^.left^.value+1,p^.left^.resulttype)
                           else
                             hp:=genordinalconstnode(p^.left^.value-1,p^.left^.resulttype);
                           disposetree(p);
                           firstpass(hp);
                           p:=hp;
                         end;
                    end;
               end;
            in_inc_x,
            in_dec_x:
              begin
                 p^.resulttype:=voiddef;
                 if assigned(p^.left) then
                   begin
                      firstcallparan(p^.left,nil);
                      if codegenerror then
                       exit;
                      { first param must be var }
                      if is_constnode(p^.left^.left) then
                        Message(type_e_variable_id_expected);
                      { check type }
                      if (p^.left^.resulttype^.deftype in [enumdef,pointerdef]) or
                         is_ordinal(p^.left^.resulttype) then
                        begin
                           { two paras ? }
                           if assigned(p^.left^.right) then
                             begin
                                { insert a type conversion         }
                                { the second param is always longint }
                                p^.left^.right^.left:=gentypeconvnode(p^.left^.right^.left,s32bitdef);
                                { check the type conversion }
                                firstpass(p^.left^.right^.left);

                                { need we an additional register ? }
                                if not(is_constintnode(p^.left^.right^.left)) and
                                  (p^.left^.right^.left^.location.loc in [LOC_MEM,LOC_REFERENCE]) and
                                  (p^.left^.right^.left^.registers32<1) then
                                  inc(p^.registers32);

                                if assigned(p^.left^.right^.right) then
                                  Message(cg_e_illegal_expression);
                             end;
                        end
                      else
                        Message(type_e_ordinal_expr_expected);
                   end
                 else
                   Message(type_e_mismatch);
              end;
             in_read_x,
             in_readln_x,
             in_write_x,
             in_writeln_x :
               begin
                  { needs a call }
                  procinfo.flags:=procinfo.flags or pi_do_call;
                  p^.resulttype:=voiddef;
                  { we must know if it is a typed file or not }
                  { but we must first do the firstpass for it }
                  file_is_typed:=false;
                  if assigned(p^.left) then
                    begin
                       firstcallparan(p^.left,nil);
                       { now we can check }
                       hp:=p^.left;
                       while assigned(hp^.right) do
                         hp:=hp^.right;
                       { if resulttype is not assigned, then automatically }
                       { file is not typed.                                }
                       if assigned(hp) and assigned(hp^.resulttype) then
                         Begin
                           if (hp^.resulttype^.deftype=filedef) and
                              (pfiledef(hp^.resulttype)^.filetype=ft_typed) then
                            begin
                              file_is_typed:=true;
                              { test the type }
                              hpp:=p^.left;
                              while (hpp<>hp) do
                               begin
                                 if not is_equal(hpp^.resulttype,pfiledef(hp^.resulttype)^.typed_as) then
                                   Message(type_e_mismatch);
                                 hpp:=hpp^.right;
                               end;
                            end;
                         end; { endif assigned(hp) }

                       { insert type conversions for write(ln) }
                       if (not file_is_typed) then
                         begin
                            dowrite:=(p^.inlinenumber in [in_write_x,in_writeln_x]);
                            hp:=p^.left;
                            while assigned(hp) do
                              begin
                                if not assigned(hp^.left^.symtableentry) or 
                                   (hp^.left^.symtableentry^.typ in [typesym]) then
                                  Message(type_e_cant_read_write_type);
                                if assigned(hp^.left^.resulttype) then
                                  begin
                                    isreal:=false;
                                    case hp^.left^.resulttype^.deftype of
                                      filedef : begin
                                                { only allowed as first parameter }
                                                  if assigned(hp^.right) then
                                                   Message(type_e_cant_read_write_type);
                                                end;
                                    stringdef : ;
                                   pointerdef : begin
                                                  if not is_equal(ppointerdef(hp^.left^.resulttype)^.definition,cchardef) then
                                                    Message(type_e_cant_read_write_type);
                                                end;
                                     floatdef : begin
                                                  isreal:=true;
                                                end;
                                       orddef : begin
                                                  case porddef(hp^.left^.resulttype)^.typ of
                                                     uchar,
                                             u32bit,s32bit : ;
                                               u8bit,s8bit,
                                             u16bit,s16bit : if dowrite then
                                                              hp^.left:=gentypeconvnode(hp^.left,s32bitdef);
                                                  bool8bit,
                                       bool16bit,bool32bit : if dowrite then
                                                              hp^.left:=gentypeconvnode(hp^.left,booldef)
                                                             else
                                                              Message(type_e_cant_read_write_type);
                                                  else
                                                    Message(type_e_cant_read_write_type);
                                                  end;
                                                end;
                                     arraydef : begin
                                                  if not((parraydef(hp^.left^.resulttype)^.lowrange=0) and
                                                         is_equal(parraydef(hp^.left^.resulttype)^.definition,cchardef)) then
                                                   begin
                                                   { but we convert only if the first index<>0,
                                                     because in this case we have a ASCIIZ string }
                                                     if dowrite and
                                                        (parraydef(hp^.left^.resulttype)^.lowrange<>0) and
                                                        (parraydef(hp^.left^.resulttype)^.definition^.deftype=orddef) and
                                                        (porddef(parraydef(hp^.left^.resulttype)^.definition)^.typ=uchar) then
                                                       hp^.left:=gentypeconvnode(hp^.left,cstringdef)
                                                     else
                                                       Message(type_e_cant_read_write_type);
                                                   end;
                                                end;
                                    else
                                      Message(type_e_cant_read_write_type);
                                    end;

                                    { some format options ? }
                                    hpp:=hp^.right;
                                    if assigned(hpp) and hpp^.is_colon_para then
                                      begin
                                        if (not is_integer(hpp^.resulttype)) then
                                          Message(type_e_integer_expr_expected)
                                        else
                                          hpp^.left:=gentypeconvnode(hpp^.left,s32bitdef);
                                        hpp:=hpp^.right;
                                        if assigned(hpp) and hpp^.is_colon_para then
                                          begin
                                            if isreal then
                                             begin
                                               if (not is_integer(hpp^.resulttype)) then
                                                 Message(type_e_integer_expr_expected)
                                               else
                                                 hpp^.left:=gentypeconvnode(hpp^.left,s32bitdef);
                                             end
                                            else
                                             Message(parser_e_illegal_colon_qualifier);
                                          end;
                                      end;

                                  end;
                                 hp:=hp^.right;
                              end;
                         end;
                       { pass all parameters again for the typeconversions }
                       if codegenerror then
                         exit;
                       must_be_valid:=true;
                       firstcallparan(p^.left,nil);
                       { calc registers }
                       left_right_max(p);
                    end;
               end;
            in_settextbuf_file_x :
              begin
                 { warning here p^.left is the callparannode
                   not the argument directly }
                 { p^.left^.left is text var }
                 { p^.left^.right^.left is the buffer var }
                 { firstcallparan(p^.left,nil);
                   already done in firstcalln }
                 { now we know the type of buffer }
                 getsymonlyin(systemunit,'SETTEXTBUF');
                 hp:=gencallnode(pprocsym(srsym),systemunit);
                 hp^.left:=gencallparanode(
                   genordinalconstnode(p^.left^.left^.resulttype^.size,s32bitdef),p^.left);
                 putnode(p);
                 p:=hp;
                 firstpass(p);
              end;
             { the firstpass of the arg has been done in firstcalln ? }
             in_reset_typedfile,in_rewrite_typedfile :
               begin
                  procinfo.flags:=procinfo.flags or pi_do_call;
                  { to be sure the right definition is loaded }
                  p^.left^.resulttype:=nil;
                  firstload(p^.left);
                  p^.resulttype:=voiddef;
               end;
             in_str_x_string :
               begin
                  procinfo.flags:=procinfo.flags or pi_do_call;
                  p^.resulttype:=voiddef;
                  if assigned(p^.left) then
                    begin
                       hp:=p^.left^.right;
                       { first pass just the string for first local use }
                       must_be_valid:=false;
                       count_ref:=true;
                       p^.left^.right:=nil;
                       firstcallparan(p^.left,nil);
                       must_be_valid:=true;
                       p^.left^.right:=hp;
                       firstcallparan(p^.left^.right,nil);
                       hp:=p^.left;
                       { valid string ? }
                       if not assigned(hp) or
                          (hp^.left^.resulttype^.deftype<>stringdef) or
                          (hp^.right=nil) or
                          (hp^.left^.location.loc<>LOC_REFERENCE) then
                         Message(cg_e_illegal_expression);
                       { !!!! check length of string }

                       while assigned(hp^.right) do
                         hp:=hp^.right;
                       { check and convert the first param }
                       if hp^.is_colon_para then
                         Message(cg_e_illegal_expression);

                       isreal:=false;
                       case hp^.resulttype^.deftype of
                        orddef : begin
                                   case porddef(hp^.left^.resulttype)^.typ of
                              u32bit,s32bit : ;
                                u8bit,s8bit,
                              u16bit,s16bit : hp^.left:=gentypeconvnode(hp^.left,s32bitdef);
                                   else
                                     Message(type_e_integer_or_real_expr_expected);
                                   end;
                                 end;
                      floatdef : begin
                                   isreal:=true;
                                 end;
                       else
                         Message(type_e_integer_or_real_expr_expected);
                       end;

                       { some format options ? }
                       hpp:=p^.left^.right;
                       if assigned(hpp) and hpp^.is_colon_para then
                         begin
                           if (not is_integer(hpp^.resulttype)) then
                             Message(type_e_integer_expr_expected)
                           else
                             hpp^.left:=gentypeconvnode(hpp^.left,s32bitdef);
                           hpp:=hpp^.right;
                           if assigned(hpp) and hpp^.is_colon_para then
                             begin
                               if isreal then
                                begin
                                  if (not is_integer(hpp^.resulttype)) then
                                    Message(type_e_integer_expr_expected)
                                  else
                                    hpp^.left:=gentypeconvnode(hpp^.left,s32bitdef);
                                end
                               else
                                Message(parser_e_illegal_colon_qualifier);
                             end;
                         end;

                       { for first local use }
                       must_be_valid:=false;
                       count_ref:=true;
                    end
                  else
                    Message(parser_e_illegal_parameter_list);
                  { pass all parameters again for the typeconversions }
                  if codegenerror then
                    exit;
                  must_be_valid:=true;
                  firstcallparan(p^.left,nil);
                  { calc registers }
                  left_right_max(p);
               end;
            in_include_x_y,
            in_exclude_x_y:
              begin
                 p^.resulttype:=voiddef;
                 if assigned(p^.left) then
                   begin
                      firstcallparan(p^.left,nil);
                      p^.registers32:=p^.left^.registers32;
                      p^.registersfpu:=p^.left^.registersfpu;
{$ifdef SUPPORT_MMX}
                      p^.registersmmx:=p^.left^.registersmmx;
{$endif SUPPORT_MMX}
                      { first param must be var }
                      if (p^.left^.left^.location.loc<>LOC_REFERENCE) and
                         (p^.left^.left^.location.loc<>LOC_CREGISTER) then
                        Message(cg_e_illegal_expression);
                      { check type }
                      if (p^.left^.resulttype^.deftype=setdef) then
                        begin
                           { two paras ? }
                           if assigned(p^.left^.right) then
                             begin
                                { insert a type conversion         }
                                { to the type of the set elements  }
                                p^.left^.right^.left:=gentypeconvnode(
                                  p^.left^.right^.left,
                                  psetdef(p^.left^.resulttype)^.setof);
                                { check the type conversion }
                                firstpass(p^.left^.right^.left);
                                { only three parameters are allowed }
                                if assigned(p^.left^.right^.right) then
                                  Message(cg_e_illegal_expression);
                             end;
                        end
                      else
                        Message(type_e_mismatch);
                   end
                 else
                   Message(type_e_mismatch);
              end;
             in_low_x,in_high_x:
               begin
                  if p^.left^.treetype in [typen,loadn] then
                    begin
                       case p^.left^.resulttype^.deftype of
                          orddef,enumdef:
                            begin
                               do_lowhigh(p^.left^.resulttype);
                               firstpass(p);
                            end;
                          setdef:
                            begin
                               do_lowhigh(Psetdef(p^.left^.resulttype)^.setof);
                               firstpass(p);
                            end;
                         arraydef:
                            begin
                              if is_open_array(p^.left^.resulttype) then
                                begin
                                   if p^.inlinenumber=in_low_x then
                                     begin
                                        hp:=genordinalconstnode(Parraydef(p^.left^.resulttype)^.lowrange,s32bitdef);
                                        disposetree(p);
                                        p:=hp;
                                        firstpass(p);
                                     end
                                   else
                                     begin
                                        p^.resulttype:=s32bitdef;
                                        p^.registers32:=max(1,
                                          p^.registers32);
                                        p^.location.loc:=LOC_REGISTER;
                                     end;
                                end
                              else
                                begin
                                   if p^.inlinenumber=in_low_x then
                                     hp:=genordinalconstnode(Parraydef(p^.left^.resulttype)^.lowrange,s32bitdef)
                                   else
                                     hp:=genordinalconstnode(Parraydef(p^.left^.resulttype)^.highrange,s32bitdef);
                                   disposetree(p);
                                   p:=hp;
                                   firstpass(p);
                                end;
                           end;
                         stringdef:
                           begin
                              if p^.inlinenumber=in_low_x then
                                hp:=genordinalconstnode(0,u8bitdef)
                              else
                                hp:=genordinalconstnode(Pstringdef(p^.left^.resulttype)^.len,u8bitdef);
                              disposetree(p);
                              p:=hp;
                              firstpass(p);
                           end;
                         else
                           Message(type_e_mismatch);
                         end;
                    end
                  else
                    Message(type_e_varid_or_typeid_expected);
               end
                 else internalerror(8);
              end;
            end;
           must_be_valid:=store_valid;
           count_ref:=store_count_ref;
       end;

    procedure firstsubscriptn(var p : ptree);

      begin
         firstpass(p^.left);
         if codegenerror then
           begin
             p^.resulttype:=generrordef;
             exit;
           end;

         p^.resulttype:=p^.vs^.definition;
         { this must be done in the parser
         if count_ref and not must_be_valid then
           if (p^.vs^.properties and sp_protected)<>0 then
             Message(parser_e_cant_write_protected_member);
         }
         p^.registers32:=p^.left^.registers32;
         p^.registersfpu:=p^.left^.registersfpu;
{$ifdef SUPPORT_MMX}
         p^.registersmmx:=p^.left^.registersmmx;
{$endif SUPPORT_MMX}
         { classes must be dereferenced implicit }
         if (p^.left^.resulttype^.deftype=objectdef) and
           pobjectdef(p^.left^.resulttype)^.isclass then
           begin
              if p^.registers32=0 then
                p^.registers32:=1;
              p^.location.loc:=LOC_REFERENCE;
           end
         else
           begin
              if (p^.left^.location.loc<>LOC_MEM) and
                (p^.left^.location.loc<>LOC_REFERENCE) then
                Message(cg_e_illegal_expression);
              set_location(p^.location,p^.left^.location);
           end;
      end;

    procedure firstselfn(var p : ptree);

      begin
         if (p^.resulttype^.deftype=classrefdef) or
           ((p^.resulttype^.deftype=objectdef)
             and pobjectdef(p^.resulttype)^.isclass
           ) then
           p^.location.loc:=LOC_REGISTER
         else
           p^.location.loc:=LOC_REFERENCE;
      end;

    procedure firsttypen(var p : ptree);

      begin
{       DM: Why not allowed? For example: low(word) results in a type
        id of word.
        error(typeid_here_not_allowed);}
      end;

    procedure firsthnewn(var p : ptree);

      begin
      end;

    procedure firsthdisposen(var p : ptree);

      begin
         firstpass(p^.left);

         if codegenerror then
           exit;

         p^.registers32:=p^.left^.registers32;
         p^.registersfpu:=p^.left^.registersfpu;
{$ifdef SUPPORT_MMX}
         p^.registersmmx:=p^.left^.registersmmx;
{$endif SUPPORT_MMX}
         if p^.registers32<1 then
           p^.registers32:=1;
         {
         if p^.left^.location.loc<>LOC_REFERENCE then
           Message(cg_e_illegal_expression);
         }
         p^.location.loc:=LOC_REFERENCE;
         p^.resulttype:=ppointerdef(p^.left^.resulttype)^.definition;
      end;

    procedure firstnewn(var p : ptree);

      begin
         { Standardeinleitung }
         firstpass(p^.left);

         if codegenerror then
           exit;
         p^.registers32:=p^.left^.registers32;
         p^.registersfpu:=p^.left^.registersfpu;
{$ifdef SUPPORT_MMX}
         p^.registersmmx:=p^.left^.registersmmx;
{$endif SUPPORT_MMX}
         { result type is already set }
         procinfo.flags:=procinfo.flags or pi_do_call;
         p^.location.loc:=LOC_REGISTER;
      end;

    procedure firstsimplenewdispose(var p : ptree);

      begin
         { this cannot be in a register !! }
         make_not_regable(p^.left);

         firstpass(p^.left);

         { check the type }
         if (p^.left^.resulttype=nil) or (p^.left^.resulttype^.deftype<>pointerdef) then
           Message(type_e_pointer_type_expected);

         if (p^.left^.location.loc<>LOC_REFERENCE) {and
            (p^.left^.location.loc<>LOC_CREGISTER)} then
           Message(cg_e_illegal_expression);

         p^.registers32:=p^.left^.registers32;
         p^.registersfpu:=p^.left^.registersfpu;
{$ifdef SUPPORT_MMX}
         p^.registersmmx:=p^.left^.registersmmx;
{$endif SUPPORT_MMX}
         p^.resulttype:=voiddef;
         procinfo.flags:=procinfo.flags or pi_do_call;
      end;


    procedure firstsetele(var p : ptree);
      begin
         firstpass(p^.left);
         if codegenerror then
          exit;

         if assigned(p^.right) then
          begin
            firstpass(p^.right);
            if codegenerror then
             exit;
          end;

         calcregisters(p,0,0,0);
         p^.resulttype:=p^.left^.resulttype;
         set_location(p^.location,p^.left^.location);
      end;


    procedure firstsetcons(var p : ptree);
      begin
         p^.location.loc:=LOC_MEM;
      end;


    procedure firstin(var p : ptree);

      begin
         p^.location.loc:=LOC_FLAGS;
         p^.resulttype:=booldef;

         firstpass(p^.right);
         if codegenerror then
           exit;

         if p^.right^.resulttype^.deftype<>setdef then
          Message(sym_e_set_expected);

         firstpass(p^.left);
         if codegenerror then
           exit;

         p^.left:=gentypeconvnode(p^.left,psetdef(p^.right^.resulttype)^.setof);

         firstpass(p^.left);
         if codegenerror then
           exit;

         left_right_max(p);
         { this is not allways true due to optimization }
         { but if we don't set this we get problems with optimizing self code }
         if psetdef(p^.right^.resulttype)^.settype<>smallset then
           procinfo.flags:=procinfo.flags or pi_do_call
         else
           begin
              { a smallset needs maybe an misc. register }
              if (p^.left^.treetype<>ordconstn) and
                not(p^.right^.location.loc in [LOC_CREGISTER,LOC_REGISTER]) and
                (p^.right^.registers32<1) then
                inc(p^.registers32);
           end;
      end;

    procedure firststatement(var p : ptree);

      begin
         { left is the next statement in the list }
         p^.resulttype:=voiddef;

         { no temps over several statements }
         cleartempgen;
         { right is the statement itself calln assignn or a complex one }
         firstpass(p^.right);
         if (not (cs_extsyntax in aktmoduleswitches)) and
            assigned(p^.right^.resulttype) and
            (p^.right^.resulttype<>pdef(voiddef)) then
           Message(cg_e_illegal_expression);
         if codegenerror then
           exit;
         p^.registers32:=p^.right^.registers32;
         p^.registersfpu:=p^.right^.registersfpu;
{$ifdef SUPPORT_MMX}
         p^.registersmmx:=p^.right^.registersmmx;
{$endif SUPPORT_MMX}
         { left is the next in the list }
         firstpass(p^.left);
         if codegenerror then
           exit;
              if p^.right^.registers32>p^.registers32 then
                p^.registers32:=p^.right^.registers32;
              if p^.right^.registersfpu>p^.registersfpu then
                p^.registersfpu:=p^.right^.registersfpu;
{$ifdef SUPPORT_MMX}
              if p^.right^.registersmmx>p^.registersmmx then
                p^.registersmmx:=p^.right^.registersmmx;
{$endif}

      end;

    procedure firstblock(var p : ptree);

      var
         hp : ptree;
         count : longint;

      begin
         count:=0;
         hp:=p^.left;
         while assigned(hp) do
           begin
              if cs_regalloc in aktglobalswitches then
                begin
                   { Codeumstellungen }

                   { Funktionsresultate an exit anhÑngen }
                   { this is wrong for string or other complex
                     result types !!! }
                   if ret_in_acc(procinfo.retdef) and
                      assigned(hp^.left) and
                      (hp^.left^.right^.treetype=exitn) and
                      (hp^.right^.treetype=assignn) and
                      (hp^.right^.left^.treetype=funcretn) then
                      begin
                         if assigned(hp^.left^.right^.left) then
                           Message(cg_n_inefficient_code)
                         else
                           begin
                              hp^.left^.right^.left:=getcopy(hp^.right^.right);
                              disposetree(hp^.right);
                              hp^.right:=nil;
                           end;
                      end
                   { warning if unreachable code occurs and elimate this }
                   else if (hp^.right^.treetype in
                     [exitn,breakn,continuen,goton]) and
                     assigned(hp^.left) and
                     (hp^.left^.treetype<>labeln) then
                     begin
                        { use correct line number }
                        aktfilepos:=hp^.left^.fileinfo;
                        disposetree(hp^.left);
                        hp^.left:=nil;
                        Message(cg_w_unreachable_code);
                        { old lines }
                        aktfilepos:=hp^.right^.fileinfo;
                     end;
                end;
              if assigned(hp^.right) then
                begin
                   cleartempgen;
                   firstpass(hp^.right);
                   if (not (cs_extsyntax in aktmoduleswitches)) and
                      assigned(hp^.right^.resulttype) and
                      (hp^.right^.resulttype<>pdef(voiddef)) then
                     Message(cg_e_illegal_expression);
                   if codegenerror then
                     exit;

                   hp^.registers32:=hp^.right^.registers32;
                   hp^.registersfpu:=hp^.right^.registersfpu;
{$ifdef SUPPORT_MMX}
                   hp^.registersmmx:=hp^.right^.registersmmx;
{$endif SUPPORT_MMX}
                end
              else
                hp^.registers32:=0;

              if hp^.registers32>p^.registers32 then
                p^.registers32:=hp^.registers32;
              if hp^.registersfpu>p^.registersfpu then
                p^.registersfpu:=hp^.registersfpu;
{$ifdef SUPPORT_MMX}
              if hp^.registersmmx>p^.registersmmx then
                p^.registersmmx:=hp^.registersmmx;
{$endif}
              inc(count);
              hp:=hp^.left;
           end;
         { p^.registers32:=round(p^.registers32/count); }
      end;

    procedure first_while_repeat(var p : ptree);

      var
         old_t_times : longint;

      begin
         old_t_times:=t_times;

         { Registergewichtung bestimmen }
         if not(cs_littlesize in aktglobalswitches ) then
           t_times:=t_times*8;

         cleartempgen;
         must_be_valid:=true;
         firstpass(p^.left);
         if codegenerror then
           exit;
         if not((p^.left^.resulttype^.deftype=orddef) and
            (porddef(p^.left^.resulttype)^.typ in [bool8bit,bool16bit,bool32bit])) then
            begin
               Message(type_e_mismatch);
               exit;
            end;

         p^.registers32:=p^.left^.registers32;
         p^.registersfpu:=p^.left^.registersfpu;
{$ifdef SUPPORT_MMX}
         p^.registersmmx:=p^.left^.registersmmx;
{$endif SUPPORT_MMX}

         { loop instruction }
         if assigned(p^.right) then
           begin
              cleartempgen;
              firstpass(p^.right);
              if codegenerror then
                exit;

              if p^.registers32<p^.right^.registers32 then
                p^.registers32:=p^.right^.registers32;
              if p^.registersfpu<p^.right^.registersfpu then
                p^.registersfpu:=p^.right^.registersfpu;
{$ifdef SUPPORT_MMX}
              if p^.registersmmx<p^.right^.registersmmx then
                p^.registersmmx:=p^.right^.registersmmx;
{$endif SUPPORT_MMX}
           end;

         t_times:=old_t_times;
      end;

    procedure firstif(var p : ptree);

      var
         old_t_times : longint;
         hp : ptree;

      begin
         old_t_times:=t_times;

         cleartempgen;
         must_be_valid:=true;
         firstpass(p^.left);
         if codegenerror then
           exit;
         if not((p^.left^.resulttype^.deftype=orddef) and
            (porddef(p^.left^.resulttype)^.typ in [bool8bit,bool16bit,bool32bit])) then
            begin
               Message(type_e_mismatch);
               exit;
            end;

         p^.registers32:=p^.left^.registers32;
         p^.registersfpu:=p^.left^.registersfpu;
{$ifdef SUPPORT_MMX}
         p^.registersmmx:=p^.left^.registersmmx;
{$endif SUPPORT_MMX}

         { determines registers weigths }
         if not(cs_littlesize in aktglobalswitches) then
           t_times:=t_times div 2;
         if t_times=0 then
           t_times:=1;

         { if path }
         if assigned(p^.right) then
           begin
              cleartempgen;
              firstpass(p^.right);
              if codegenerror then
                exit;

              if p^.registers32<p^.right^.registers32 then
                p^.registers32:=p^.right^.registers32;
              if p^.registersfpu<p^.right^.registersfpu then
                p^.registersfpu:=p^.right^.registersfpu;
{$ifdef SUPPORT_MMX}
              if p^.registersmmx<p^.right^.registersmmx then
                p^.registersmmx:=p^.right^.registersmmx;
{$endif SUPPORT_MMX}
           end;

         { else path }
         if assigned(p^.t1) then
           begin
              cleartempgen;
              firstpass(p^.t1);
              if codegenerror then
                exit;

              if p^.registers32<p^.t1^.registers32 then
                p^.registers32:=p^.t1^.registers32;
              if p^.registersfpu<p^.t1^.registersfpu then
                p^.registersfpu:=p^.t1^.registersfpu;
{$ifdef SUPPORT_MMX}
              if p^.registersmmx<p^.t1^.registersmmx then
                p^.registersmmx:=p^.t1^.registersmmx;
{$endif SUPPORT_MMX}
           end;
         if p^.left^.treetype=ordconstn then
           begin
              { optimize }
              if p^.left^.value=1 then
                begin
                   disposetree(p^.left);
                   hp:=p^.right;
                   disposetree(p^.t1);
                   { we cannot set p to nil !!! }
                   if assigned(hp) then
                     begin
                        putnode(p);
                        p:=hp;
                     end
                   else
                     begin
                        p^.left:=nil;
                        p^.t1:=nil;
                        p^.treetype:=nothingn;
                     end;
                end
              else
                begin
                   disposetree(p^.left);
                   hp:=p^.t1;
                   disposetree(p^.right);
                   { we cannot set p to nil !!! }
                   if assigned(hp) then
                     begin
                        putnode(p);
                        p:=hp;
                     end
                   else
                     begin
                        p^.left:=nil;
                        p^.right:=nil;
                        p^.treetype:=nothingn;
                     end;
                end;
           end;

         t_times:=old_t_times;
      end;

    procedure firstexitn(var p : ptree);

      begin
         if assigned(p^.left) then
           begin
              firstpass(p^.left);
              p^.registers32:=p^.left^.registers32;
              p^.registersfpu:=p^.left^.registersfpu;
{$ifdef SUPPORT_MMX}
              p^.registersmmx:=p^.left^.registersmmx;
{$endif SUPPORT_MMX}

           end;
      end;

    procedure firstfor(var p : ptree);

      var
         old_t_times : longint;

      begin
         { Registergewichtung bestimmen
           (nicht genau), }
         old_t_times:=t_times;
         if not(cs_littlesize in aktglobalswitches) then
           t_times:=t_times*8;

         cleartempgen;
         if assigned(p^.t1) then
          begin
            firstpass(p^.t1);
            if codegenerror then
             exit;
          end;


         p^.registers32:=p^.t1^.registers32;
         p^.registersfpu:=p^.t1^.registersfpu;
{$ifdef SUPPORT_MMX}
         p^.registersmmx:=p^.left^.registersmmx;
{$endif SUPPORT_MMX}

         if p^.left^.treetype<>assignn then
           Message(cg_e_illegal_expression);

         { Laufvariable retten }
         p^.t2:=getcopy(p^.left^.left);

         { Check count var }
         if (p^.t2^.treetype<>loadn) then
          Message(cg_e_illegal_count_var);

         if (not(is_ordinal(p^.t2^.resulttype))) then
          Message(type_e_ordinal_expr_expected);

         cleartempgen;
         must_be_valid:=false;
         firstpass(p^.left);
         must_be_valid:=true;
         if p^.left^.registers32>p^.registers32 then
           p^.registers32:=p^.left^.registers32;
         if p^.left^.registersfpu>p^.registersfpu then
           p^.registersfpu:=p^.left^.registersfpu;
{$ifdef SUPPORT_MMX}
         if p^.left^.registersmmx>p^.registersmmx then
           p^.registersmmx:=p^.left^.registersmmx;
{$endif SUPPORT_MMX}
         cleartempgen;
         firstpass(p^.t2);
         if p^.t2^.registers32>p^.registers32 then
           p^.registers32:=p^.t2^.registers32;
         if p^.t2^.registersfpu>p^.registersfpu then
           p^.registersfpu:=p^.t2^.registersfpu;
{$ifdef SUPPORT_MMX}
         if p^.t2^.registersmmx>p^.registersmmx then
           p^.registersmmx:=p^.t2^.registersmmx;
{$endif SUPPORT_MMX}

         cleartempgen;
         firstpass(p^.right);
         if p^.right^.treetype<>ordconstn then
           begin
              p^.right:=gentypeconvnode(p^.right,p^.t2^.resulttype);
              cleartempgen;
              firstpass(p^.right);
           end;

         if p^.right^.registers32>p^.registers32 then
           p^.registers32:=p^.right^.registers32;
         if p^.right^.registersfpu>p^.registersfpu then
           p^.registersfpu:=p^.right^.registersfpu;
{$ifdef SUPPORT_MMX}
         if p^.right^.registersmmx>p^.registersmmx then
           p^.registersmmx:=p^.right^.registersmmx;
{$endif SUPPORT_MMX}
         t_times:=old_t_times;
      end;

    procedure firstasm(var p : ptree);

      begin
         { it's a f... to determine the used registers }
         { should be done by getnode
           I think also, that all value_str should be set to their maximum (FK)
         p^.registers32:=0;
         p^.registersfpu:=0;
         p^.registersmmx:=0;
         }
         procinfo.flags:=procinfo.flags or pi_uses_asm;
      end;

    procedure firstgoto(var p : ptree);

      begin
         {
         p^.registers32:=0;
         p^.registersfpu:=0;
         }
         p^.resulttype:=voiddef;
      end;

    procedure firstlabel(var p : ptree);

      begin
         cleartempgen;
         firstpass(p^.left);
         p^.registers32:=p^.left^.registers32;
         p^.registersfpu:=p^.left^.registersfpu;
{$ifdef SUPPORT_MMX}
         p^.registersmmx:=p^.left^.registersmmx;
{$endif SUPPORT_MMX}
         p^.resulttype:=voiddef;
      end;

    procedure firstcase(var p : ptree);

      var
         old_t_times : longint;
         hp : ptree;

      begin
         { evalutes the case expression }
         cleartempgen;
         must_be_valid:=true;
         firstpass(p^.left);
         if codegenerror then
           exit;
         p^.registers32:=p^.left^.registers32;
         p^.registersfpu:=p^.left^.registersfpu;
{$ifdef SUPPORT_MMX}
         p^.registersmmx:=p^.left^.registersmmx;
{$endif SUPPORT_MMX}

         { walk through all instructions }

         {   estimates the repeat of each instruction }
         old_t_times:=t_times;
         if not(cs_littlesize in aktglobalswitches) then
           begin
              t_times:=t_times div case_count_labels(p^.nodes);
              if t_times<1 then
                t_times:=1;
           end;
         {   first case }
         hp:=p^.right;
         while assigned(hp) do
           begin
              cleartempgen;
              firstpass(hp^.right);

              { searchs max registers }
              if hp^.right^.registers32>p^.registers32 then
                p^.registers32:=hp^.right^.registers32;
              if hp^.right^.registersfpu>p^.registersfpu then
                p^.registersfpu:=hp^.right^.registersfpu;
{$ifdef SUPPORT_MMX}
              if hp^.right^.registersmmx>p^.registersmmx then
                p^.registersmmx:=hp^.right^.registersmmx;
{$endif SUPPORT_MMX}

              hp:=hp^.left;
           end;

         { may be handle else tree }
         if assigned(p^.elseblock) then
           begin
              cleartempgen;
              firstpass(p^.elseblock);
              if codegenerror then
                exit;
              if p^.registers32<p^.elseblock^.registers32 then
                p^.registers32:=p^.elseblock^.registers32;
              if p^.registersfpu<p^.elseblock^.registersfpu then
                p^.registersfpu:=p^.elseblock^.registersfpu;
{$ifdef SUPPORT_MMX}
              if p^.registersmmx<p^.elseblock^.registersmmx then
                p^.registersmmx:=p^.elseblock^.registersmmx;
{$endif SUPPORT_MMX}
           end;
         t_times:=old_t_times;

         { there is one register required for the case expression }
         if p^.registers32<1 then p^.registers32:=1;
      end;

    procedure firsttryexcept(var p : ptree);

      begin
         cleartempgen;
         firstpass(p^.left);

         { on statements }
         if assigned(p^.right) then
           begin
              cleartempgen;
              firstpass(p^.right);
              p^.registers32:=max(p^.registers32,p^.right^.registers32);
              p^.registersfpu:=max(p^.registersfpu,p^.right^.registersfpu);
{$ifdef SUPPORT_MMX}
              p^.registersmmx:=max(p^.registersmmx,p^.right^.registersmmx);
{$endif SUPPORT_MMX}
           end;
         { else block }
         if assigned(p^.t1) then
           begin
              firstpass(p^.t1);
              p^.registers32:=max(p^.registers32,p^.t1^.registers32);
              p^.registersfpu:=max(p^.registersfpu,p^.t1^.registersfpu);
{$ifdef SUPPORT_MMX}
              p^.registersmmx:=max(p^.registersmmx,p^.t1^.registersmmx);
{$endif SUPPORT_MMX}
           end;
      end;

    procedure firsttryfinally(var p : ptree);

      begin
         p^.resulttype:=voiddef;
         cleartempgen;
         must_be_valid:=true;
         firstpass(p^.left);

         cleartempgen;
         must_be_valid:=true;
         firstpass(p^.right);
         if codegenerror then
           exit;
         left_right_max(p);
      end;

    procedure firstis(var p : ptree);

      begin
         firstpass(p^.left);
         firstpass(p^.right);

         if (p^.right^.resulttype^.deftype<>classrefdef) then
           Message(type_e_mismatch);
         if codegenerror then
           exit;

         left_right_max(p);

         { left must be a class }
         if (p^.left^.resulttype^.deftype<>objectdef) or
           not(pobjectdef(p^.left^.resulttype)^.isclass) then
           Message(type_e_mismatch);

         { the operands must be related }
         if (not(pobjectdef(p^.left^.resulttype)^.isrelated(
           pobjectdef(pclassrefdef(p^.right^.resulttype)^.definition)))) and
           (not(pobjectdef(pclassrefdef(p^.right^.resulttype)^.definition)^.isrelated(
           pobjectdef(p^.left^.resulttype)))) then
           Message(type_e_mismatch);

         p^.location.loc:=LOC_FLAGS;
         p^.resulttype:=booldef;
      end;

    procedure firstas(var p : ptree);

      begin
         firstpass(p^.right);
         firstpass(p^.left);
         if (p^.right^.resulttype^.deftype<>classrefdef) then
           Message(type_e_mismatch);

         if codegenerror then
           exit;

         left_right_max(p);

         { left must be a class }
         if (p^.left^.resulttype^.deftype<>objectdef) or
           not(pobjectdef(p^.left^.resulttype)^.isclass) then
           Message(type_e_mismatch);

         { the operands must be related }
         if (not(pobjectdef(p^.left^.resulttype)^.isrelated(
           pobjectdef(pclassrefdef(p^.right^.resulttype)^.definition)))) and
           (not(pobjectdef(pclassrefdef(p^.right^.resulttype)^.definition)^.isrelated(
           pobjectdef(p^.left^.resulttype)))) then
           Message(type_e_mismatch);

         p^.location:=p^.left^.location;
         p^.resulttype:=pclassrefdef(p^.right^.resulttype)^.definition;
      end;

    procedure firstloadvmt(var p : ptree);

      begin
         { resulttype must be set !
         p^.registersfpu:=0;
         }
         p^.registers32:=1;
         p^.location.loc:=LOC_REGISTER;
      end;

    procedure firstraise(var p : ptree);

      begin
         p^.resulttype:=voiddef;
         {
         p^.registersfpu:=0;
         p^.registers32:=0;
         }
         if assigned(p^.left) then
           begin
              firstpass(p^.left);

              { this must be a _class_ }
              if (p^.left^.resulttype^.deftype<>objectdef) or
                ((pobjectdef(p^.left^.resulttype)^.options and oois_class)=0) then
                Message(type_e_mismatch);

              p^.registersfpu:=p^.left^.registersfpu;
              p^.registers32:=p^.left^.registers32;
{$ifdef SUPPORT_MMX}
              p^.registersmmx:=p^.left^.registersmmx;
{$endif SUPPORT_MMX}
              if assigned(p^.right) then
                begin
                   firstpass(p^.right);
                   p^.right:=gentypeconvnode(p^.right,s32bitdef);
                   firstpass(p^.right);
                   left_right_max(p);
                end;
           end;
      end;

    procedure firstwith(var p : ptree);

      begin
         if assigned(p^.left) and assigned(p^.right) then
            begin
               firstpass(p^.left);
               if codegenerror then
                 exit;

               firstpass(p^.right);

               if codegenerror then
                 exit;

               left_right_max(p);
               p^.resulttype:=voiddef;
            end
         else
           begin
              { optimization }
              disposetree(p);
              p:=nil;
           end;
      end;

    procedure firstonn(var p : ptree);

      begin
         { that's really an example procedure for a firstpass :) }
         cleartempgen;
         p^.resulttype:=voiddef;
         p^.registers32:=0;
         p^.registersfpu:=0;
{$ifdef SUPPORT_MMX}
         p^.registersmmx:=0;
{$endif SUPPORT_MMX}
         if assigned(p^.left) then
           begin
              firstpass(p^.left);
              p^.registers32:=p^.left^.registers32;
              p^.registersfpu:=p^.left^.registersfpu;
{$ifdef SUPPORT_MMX}
              p^.registersmmx:=p^.left^.registersmmx;
{$endif SUPPORT_MMX}
           end;

         cleartempgen;
         if assigned(p^.right) then
           begin
              firstpass(p^.right);
              p^.registers32:=max(p^.registers32,p^.right^.registers32);
              p^.registersfpu:=max(p^.registersfpu,p^.right^.registersfpu);
{$ifdef SUPPORT_MMX}
              p^.registersmmx:=max(p^.registersmmx,p^.right^.registersmmx);
{$endif SUPPORT_MMX}
           end;
      end;

    procedure firstprocinline(var p : ptree);

      begin
          {left contains the code in tree form }
          { but it has already been firstpassed }
          { so firstpass(p^.left); does not seem required }
          { might be required later if we change the arg handling !! }
      end;

    type
       firstpassproc = procedure(var p : ptree);

    procedure firstpass(var p : ptree);

(*       ttreetyp = (addn,            {Represents the + operator.}
                   muln,            {Represents the * operator.}
                   subn,            {Represents the - operator.}
                   divn,            {Represents the div operator.}
                   symdifn,         {Represents the >< operator.}
                   modn,            {Represents the mod operator.}
                   assignn,         {Represents an assignment.}
                   loadn,           {Represents the use of a variabele.}
                   rangen,          {Represents a range (i.e. 0..9).}
                   ltn,             {Represents the < operator.}
                   lten,            {Represents the <= operator.}
                   gtn,             {Represents the > operator.}
                   gten,            {Represents the >= operator.}
                   equaln,          {Represents the = operator.}
                   unequaln,        {Represents the <> operator.}
                   inn,             {Represents the in operator.}
                   orn,             {Represents the or operator.}
                   xorn,            {Represents the xor operator.}
                   shrn,            {Represents the shr operator.}
                   shln,            {Represents the shl operator.}
                   slashn,          {Represents the / operator.}
                   andn,            {Represents the and operator.}
                   subscriptn,      {??? Field in a record/object?}
                   derefn,          {Dereferences a pointer.}
                   addrn,           {Represents the @ operator.}
                   doubleaddrn,     {Represents the @@ operator.}
                   ordconstn,       {Represents an ordinal value.}
                   typeconvn,       {Represents type-conversion/typecast.}
                   calln,           {Represents a call node.}
                   callparan,       {Represents a parameter.}
                   realconstn,      {Represents a real value.}
                   fixconstn,       {Represents a fixed value.}
                   umminusn,        {Represents a sign change (i.e. -2).}
                   asmn,            {Represents an assembler node }
                   vecn,            {Represents array indexing.}
                   stringconstn,    {Represents a string constant.}
                   funcretn,        {Represents the function result var.}
                   selfn,           {Represents the self parameter.}
                   notn,            {Represents the not operator.}
                   inlinen,         {Internal procedures (i.e. writeln).}
                   niln,            {Represents the nil pointer.}
                   errorn,          {This part of the tree could not be
                                     parsed because of a compiler error.}
                   typen,           {A type name. Used for i.e. typeof(obj).}
                   hnewn,           {The new operation, constructor call.}
                   hdisposen,       {The dispose operation with destructor call.}
                   newn,            {The new operation, constructor call.}
                   simpledisposen,  {The dispose operation.}
                   setelen,         {A set element (i.e. [a,b]).}
                   setconstrn,      {A set constant (i.e. [1,2]).}
                   blockn,          {A block of statements.}
                   statementn,      {One statement in list of nodes.}
                   loopn,           { used in genloopnode, must be converted }
                   ifn,             {An if statement.}
                   breakn,          {A break statement.}
                   continuen,       {A continue statement.}
                   repeatn,         {A repeat until block.}
                   whilen,          {A while do statement.}
                   forn,            {A for loop.}
                   exitn,           {An exit statement.}
                   withn,           {A with statement.}
                   casen,           {A case statement.}
                   labeln,          {A label.}
                   goton,           {A goto statement.}
                   simplenewn,      {The new operation.}
                   tryexceptn,      {A try except block.}
                   raisen,          {A raise statement.}
                   switchesn,       {??? Currently unused...}
                   tryfinallyn,     {A try finally statement.}
                   isn,             {Represents the is operator.}
                   asn,             {Represents the as typecast.}
                   caretn,          {Represents the ^ operator.}
                   failn,           {Represents the fail statement.}
                   starstarn,       {Represents the ** operator exponentiation }
                   procinlinen,      {Procedures that can be inlined }
                   { added for optimizations where we cannot suppress }
                   nothingn,
                   loadvmtn);       {???.} *)
      const
         procedures : array[ttreetyp] of firstpassproc =
            (firstadd,firstadd,firstadd,firstmoddiv,firstadd,
             firstmoddiv,firstassignment,firstload,firstrange,
             firstadd,firstadd,firstadd,firstadd,
             firstadd,firstadd,firstin,firstadd,
             firstadd,firstshlshr,firstshlshr,firstadd,
             firstadd,firstsubscriptn,firstderef,firstaddr,firstdoubleaddr,
             firstordconst,firsttypeconv,firstcalln,firstnothing,
             firstrealconst,firstfixconst,firstumminus,firstasm,firstvecn,
             firststringconst,firstfuncret,firstselfn,
             firstnot,firstinline,firstniln,firsterror,
             firsttypen,firsthnewn,firsthdisposen,firstnewn,
             firstsimplenewdispose,firstsetele,firstsetcons,firstblock,
             firststatement,firstnothing,firstif,firstnothing,
             firstnothing,first_while_repeat,first_while_repeat,firstfor,
             firstexitn,firstwith,firstcase,firstlabel,
             firstgoto,firstsimplenewdispose,firsttryexcept,
             firstraise,firstnothing,firsttryfinally,
             firstonn,firstis,firstas,firstadd,
             firstnothing,firstadd,firstprocinline,firstnothing,firstloadvmt);

      var
         oldcodegenerror  : boolean;
         oldlocalswitches : tlocalswitches;
         oldpos           : tfileposinfo;
{$ifdef extdebug}
         str1,str2 : string;
         oldp      : ptree;
         not_first : boolean;
{$endif extdebug}

      begin
{$ifdef extdebug}
         inc(total_of_firstpass);
         if (p^.firstpasscount>0) and only_one_pass then
           exit;
{$endif extdebug}
         oldcodegenerror:=codegenerror;
         oldpos:=aktfilepos;
         oldlocalswitches:=aktlocalswitches;
{$ifdef extdebug}
         if p^.firstpasscount>0 then
           begin
              move(p^,str1[1],sizeof(ttree));
              str1[0]:=char(sizeof(ttree));
              new(oldp);
              oldp^:=p^;
              not_first:=true;
              inc(firstpass_several);
           end
         else
           not_first:=false;
{$endif extdebug}

         aktfilepos:=p^.fileinfo;
         aktlocalswitches:=p^.localswitches;
         if not p^.error then
           begin
              codegenerror:=false;
              procedures[p^.treetype](p);
              p^.error:=codegenerror;
              codegenerror:=codegenerror or oldcodegenerror;
           end
         else
           codegenerror:=true;
{$ifdef extdebug}
         if not_first then
           begin
              { dirty trick to compare two ttree's (PM) }
              move(p^,str2[1],sizeof(ttree));
              str2[0]:=char(sizeof(ttree));
              if str1<>str2 then
                begin
                   comment(v_debug,'tree changed after first counting pass '
                     +tostr(longint(p^.treetype)));
                   compare_trees(oldp,p);
                end;
              dispose(oldp);
           end;
         if count_ref then
           inc(p^.firstpasscount);
{$endif extdebug}
         aktlocalswitches:=oldlocalswitches;
         aktfilepos:=oldpos;
      end;

    function do_firstpass(var p : ptree) : boolean;

      begin
         codegenerror:=false;
         firstpass(p);
         do_firstpass:=codegenerror;
      end;

    { to be called only for a whole function }
    { to insert code at entry and exit       }

    function function_firstpass(var p : ptree) : boolean;

      begin
         codegenerror:=false;
         firstpass(p);
         function_firstpass:=codegenerror;
      end;

end.
{
  $Log$
  Revision 1.78  1998-09-08 13:36:24  peter
    + can't write type syms anymore

  Revision 1.77  1998/09/07 22:25:52  peter
    * fixed str(boolean,string) which was allowed
    * fixed write(' ':<int expression>) only constants where allowed :(

  Revision 1.76  1998/09/07 18:46:05  peter
    * update smartlinking, uses getdatalabel
    * renamed ptree.value vars to value_str,value_real,value_set

  Revision 1.75  1998/09/05 23:51:06  florian
    * possible bug with too few registers in first/secondin fixed

  Revision 1.74  1998/09/05 23:04:00  florian
    * some fixes to get -Or work:
      - inc/dec didn't take care of CREGISTER
      - register calculcation of inc/dec was wrong
      - var/const parameters get now assigned 32 bit register, but
        const parameters only if they are passed by reference !

  Revision 1.73  1998/09/05 22:29:57  florian
    + the boolean comparision a=true generates now the same code as only a,
      (a=1 was compiled to cmp 1,a now it is compiled to cmp 0,a)

  Revision 1.72  1998/09/05 22:11:01  florian
    + switch -vb
    * while/repeat loops accept now also word/longbool conditions
    * makebooltojump did an invalid ungetregister32, fixed

  Revision 1.71  1998/09/04 11:55:18  florian
    * problem with -Or fixed

  Revision 1.70  1998/09/04 08:42:00  peter
    * updated some error messages

  Revision 1.69  1998/09/01 17:39:47  peter
    + internal constant functions

  Revision 1.68  1998/09/01 09:02:52  peter
    * moved message() to hcodegen, so pass_2 also uses them

  Revision 1.67  1998/09/01 07:54:20  pierre
    * UseBrowser a little updated (might still be buggy !!)
    * bug in psub.pas in function specifier removed
    * stdcall allowed in interface and in implementation
      (FPC will not yet complain if it is missing in either part
      because stdcall is only a dummy !!)

  Revision 1.66  1998/08/31 08:52:05  peter
    * fixed error 10 with succ() and pref()

  Revision 1.65  1998/08/28 12:51:40  florian
    + ansistring to pchar type cast fixed

  Revision 1.64  1998/08/28 10:54:22  peter
    * fixed smallset generation from elements, it has never worked before!

  Revision 1.63  1998/08/24 10:05:39  florian
    + class types and class reference types are now compatible with void
      pointers
    + class can be stored now registers, even if a type conversation is applied

  Revision 1.62  1998/08/23 16:07:22  florian
    * internalerror with mod/div fixed

  Revision 1.61  1998/08/21 14:08:47  pierre
    + TEST_FUNCRET now default (old code removed)
      works also for m68k (at least compiles)

  Revision 1.60  1998/08/20 12:59:57  peter
    - removed obsolete in_*

  Revision 1.59  1998/08/20 09:26:39  pierre
    + funcret setting in underproc testing
      compile with _dTEST_FUNCRET

  Revision 1.58  1998/08/19 16:07:51  jonas
    * changed optimizer switches + cleanup of DestroyRefs in daopt386.pas

  Revision 1.57  1998/08/19 00:42:39  peter
    + subrange types for enums
    + checking for bounds type with ranges

  Revision 1.56  1998/08/18 09:24:42  pierre
    * small warning position bug fixed
    * support_mmx switches splitting was missing
    * rhide error and warning output corrected

  Revision 1.55  1998/08/14 18:18:44  peter
    + dynamic set contruction
    * smallsets are now working (always longint size)

  Revision 1.54  1998/08/13 11:00:10  peter
    * fixed procedure<>procedure construct

  Revision 1.53  1998/08/12 19:39:28  peter
    * fixed some crashes

  Revision 1.52  1998/08/10 14:50:08  peter
    + localswitches, moduleswitches, globalswitches splitting

  Revision 1.51  1998/08/10 10:18:29  peter
    + Compiler,Comphook unit which are the new interface units to the
      compiler

  Revision 1.50  1998/08/08 21:51:39  peter
    * small crash prevent is firstassignment

  Revision 1.49  1998/07/30 16:07:08  florian
    * try ... expect <statement> end; works now

  Revision 1.48  1998/07/30 13:30:35  florian
    * final implemenation of exception support, maybe it needs
      some fixes :)

  Revision 1.47  1998/07/30 11:18:17  florian
    + first implementation of try ... except on .. do end;
    * limitiation of 65535 bytes parameters for cdecl removed

  Revision 1.46  1998/07/28 21:52:52  florian
    + implementation of raise and try..finally
    + some misc. exception stuff

  Revision 1.45  1998/07/26 21:58:59  florian
   + better support for switch $H
   + index access to ansi strings added
   + assigment of data (records/arrays) containing ansi strings

  Revision 1.44  1998/07/24 22:16:59  florian
    * internal error 10 together with array access fixed. I hope
      that's the final fix.

  Revision 1.43  1998/07/20 18:40:14  florian
    * handling of ansi string constants should now work

  Revision 1.42  1998/07/20 10:23:01  florian
    * better ansi string assignement

  Revision 1.41  1998/07/18 22:54:27  florian
    * some ansi/wide/longstring support fixed:
       o parameter passing
       o returning as result from functions

  Revision 1.40  1998/07/18 17:11:09  florian
    + ansi string constants fixed
    + switch $H partial implemented

  Revision 1.39  1998/07/14 21:46:47  peter
    * updated messages file

  Revision 1.38  1998/07/14 14:46:50  peter
    * released NEWINPUT

  Revision 1.37  1998/07/07 12:31:44  peter
    * fixed string:= which allowed almost any type

  Revision 1.36  1998/07/07 11:20:00  peter
    + NEWINPUT for a better inputfile and scanner object

  Revision 1.35  1998/06/25 14:04:19  peter
    + internal inc/dec

  Revision 1.34  1998/06/25 08:48:14  florian
    * first version of rtti support

  Revision 1.33  1998/06/16 08:56:24  peter
    + targetcpu
    * cleaner pmodules for newppu

  Revision 1.32  1998/06/14 18:23:57  peter
    * fixed xor bug (from mailinglist)

  Revision 1.31  1998/06/13 00:10:09  peter
    * working browser and newppu
    * some small fixes against crashes which occured in bp7 (but not in
      fpc?!)

  Revision 1.30  1998/06/12 10:32:28  pierre
    * column problem hopefully solved
    + C vars declaration changed

  Revision 1.29  1998/06/09 16:01:44  pierre
    + added procedure directive parsing for procvars
      (accepted are popstack cdecl and pascal)
    + added C vars with the following syntax
      var C calias 'true_c_name';(can be followed by external)
      reason is that you must add the Cprefix

      which is target dependent

  Revision 1.28  1998/06/05 14:37:29  pierre
    * fixes for inline for operators
    * inline procedure more correctly restricted

  Revision 1.27  1998/06/05 00:01:06  florian
    * bugs with assigning related objects and passing objects by reference
      to a procedure

  Revision 1.26  1998/06/04 09:55:39  pierre
    * demangled name of procsym reworked to become independant
      of the mangling scheme

  Revision 1.25  1998/06/03 22:48:57  peter
    + wordbool,longbool
    * rename bis,von -> high,low
    * moved some systemunit loading/creating to psystem.pas

  Revision 1.24  1998/06/02 17:03:01  pierre
    *  with node corrected for objects
    * small bugs for SUPPORT_MMX fixed

  Revision 1.23  1998/06/01 16:50:20  peter
    + boolean -> ord conversion
    * fixed ord -> boolean conversion

  Revision 1.22  1998/05/28 17:26:49  peter
    * fixed -R switch, it didn't work after my previous akt/init patch
    * fixed bugs 110,130,136

  Revision 1.21  1998/05/25 17:11:41  pierre
    * firstpasscount bug fixed
      now all is already set correctly the first time
      under EXTDEBUG try -gp to skip all other firstpasses
      it works !!
    * small bug fixes
      - for smallsets with -dTESTSMALLSET
      - some warnings removed (by correcting code !)

  Revision 1.20  1998/05/23 01:21:17  peter
    + aktasmmode, aktoptprocessor, aktoutputformat
    + smartlink per module $SMARTLINK-/+ (like MMX) and moved to aktswitches
    + $LIBNAME to set the library name where the unit will be put in
    * splitted cgi386 a bit (codeseg to large for bp7)
    * nasm, tasm works again. nasm moved to ag386nsm.pas

  Revision 1.19  1998/05/20 09:42:34  pierre
    + UseTokenInfo now default
    * unit in interface uses and implementation uses gives error now
    * only one error for unknown symbol (uses lastsymknown boolean)
      the problem came from the label code !
    + first inlined procedures and function work
      (warning there might be allowed cases were the result is still wrong !!)
    * UseBrower updated gives a global list of all position of all used symbols
      with switch -gb

  Revision 1.18  1998/05/11 13:07:55  peter
    + $ifdef NEWPPU for the new ppuformat
    + $define GDB not longer required
    * removed all warnings and stripped some log comments
    * no findfirst/findnext anymore to remove smartlink *.o files

  Revision 1.17  1998/05/06 08:38:43  pierre
    * better position info with UseTokenInfo
      UseTokenInfo greatly simplified
    + added check for changed tree after first time firstpass
      (if we could remove all the cases were it happen
      we could skip all firstpass if firstpasscount > 1)
      Only with ExtDebug

  Revision 1.16  1998/05/01 16:38:45  florian
    * handling of private and protected fixed
    + change_keywords_to_tp implemented to remove
      keywords which aren't supported by tp
    * break and continue are now symbols of the system unit
    + widestring, longstring and ansistring type released

  Revision 1.15  1998/05/01 09:01:23  florian
    + correct semantics of private and protected
    * small fix in variable scope:
       a id can be used in a parameter list of a method, even it is used in
       an anchestor class as field id

  Revision 1.14  1998/04/30 15:59:41  pierre
    * GDB works again better :
      correct type info in one pass
    + UseTokenInfo for better source position
    * fixed one remaining bug in scanner for line counts
    * several little fixes

  Revision 1.13  1998/04/29 10:33:56  pierre
    + added some code for ansistring (not complete nor working yet)
    * corrected operator overloading
    * corrected nasm output
    + started inline procedures
    + added starstarn : use ** for exponentiation (^ gave problems)
    + started UseTokenInfo cond to get accurate positions

  Revision 1.12  1998/04/22 21:06:50  florian
    * last fixes before the release:
      - veryyyy slow firstcall fixed

  Revision 1.11  1998/04/21 10:16:48  peter
    * patches from strasbourg
    * objects is not used anymore in the fpc compiled version

  Revision 1.10  1998/04/14 23:27:03  florian
    + exclude/include with constant second parameter added

  Revision 1.9  1998/04/13 21:15:42  florian
    * error handling of pass_1 and cgi386 fixed
    * the following bugs fixed: 0117, 0118, 0119 and 0129, 0122 was already
      fixed, verified

  Revision 1.8  1998/04/13 08:42:52  florian
    * call by reference and call by value open arrays fixed

  Revision 1.7  1998/04/12 22:39:44  florian
    * problem with read access to properties solved
    * correct handling of hidding methods via virtual (COM)
    * correct result type of constructor calls (COM), the resulttype
      depends now on the type of the class reference

  Revision 1.6  1998/04/09 22:16:34  florian
    * problem with previous REGALLOC solved
    * improved property support

  Revision 1.5  1998/04/08 16:58:04  pierre
    * several bugfixes
      ADD ADC and AND are also sign extended
      nasm output OK (program still crashes at end
      and creates wrong assembler files !!)
      procsym types sym in tdef removed !!

  Revision 1.4  1998/04/07 22:45:04  florian
    * bug0092, bug0115 and bug0121 fixed
    + packed object/class/array
}
