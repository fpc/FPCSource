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

    function do_firstpass(var p : ptree) : boolean;

  implementation

     uses
        scanner,cobjects,verbose,systems,globals,aasm,symtable,
        types,strings,hcodegen,files
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

    procedure message(const t : tmsgconst);

      var
         olderrorcount : longint;

      begin
         if not(codegenerror) then
           begin
              olderrorcount:=status.errorcount;
              verbose.Message(t);
              codegenerror:=olderrorcount<>status.errorcount;
           end;
      end;

    procedure message1(const t : tmsgconst;const s : string);

      var
         olderrorcount : longint;

      begin
         if not(codegenerror) then
           begin
              olderrorcount:=status.errorcount;
              verbose.Message1(t,s);
              codegenerror:=olderrorcount<>status.errorcount;
           end;
      end;

    procedure message2(const t : tmsgconst;const s1,s2 : string);

      var
         olderrorcount : longint;

      begin
         if not(codegenerror) then
           begin
              olderrorcount:=status.errorcount;
              verbose.Message2(t,s1,s2);
              codegenerror:=olderrorcount<>status.errorcount;
           end;
      end;

    procedure message3(const t : tmsgconst;const s1,s2,s3 : string);

      var
         olderrorcount : longint;

      begin
         if not(codegenerror) then
           begin
              olderrorcount:=status.errorcount;
              verbose.Message3(t,s1,s2,s3);
              codegenerror:=olderrorcount<>status.errorcount;
           end;
      end;

    procedure firstpass(var p : ptree);forward;

    { marks an lvalue as "unregable" }
    procedure make_not_regable(p : ptree);

      begin
         case p^.treetype of
            typeconvn : make_not_regable(p^.left);
            loadn : if p^.symtableentry^.typ=varsym then
                      pvarsym(p^.symtableentry)^.regable:=false;
         end;
      end;


    procedure left_right_max(p : ptree);
      begin
         p^.registers32:=max(p^.left^.registers32,p^.right^.registers32);
         p^.registersfpu:=max(p^.left^.registersfpu,p^.right^.registersfpu);
{$ifdef SUPPORT_MMX}
         p^.registersmmx:=max(p^.left^.registersmmx,p^.right^.registersmmx);
{$endif SUPPORT_MMX}
      end;

    { calculates the needed registers for a binary operator }
    procedure calcregisters(p : ptree;r32,fpu,mmx : word);

      begin
         left_right_max(p);
         { Nur wenn links und rechts ein Unterschied < benîtige Anzahl ist, }
         { wird ein zusÑtzliches Register benîtigt, da es dann keinen       }
         { schwierigeren Ast gibt, welcher erst ausgewertet werden kann     }

         if (abs(p^.left^.registers32-p^.right^.registers32)<r32) then
           inc(p^.registers32,r32);

         if (abs(p^.left^.registersfpu-p^.right^.registersfpu)<fpu) then
           inc(p^.registersfpu,fpu);

{$ifdef SUPPORT_MMX}
         if (abs(p^.left^.registersmmx-p^.right^.registersmmx)<mmx) then
           inc(p^.registersmmx,mmx);
{$endif SUPPORT_MMX}

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

    function isconvertable(def_from,def_to : pdef;
             var doconv : tconverttype;fromtreetype : ttreetyp;
             explicit : boolean) : boolean;

      { Tbasetype:  uauto,uvoid,uchar,
                    u8bit,u16bit,u32bit,
                    s8bit,s16bit,s32,
                    bool8bit,bool16bit,boot32bit }

      const
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

      begin
         b:=false;
         if (not assigned(def_from)) or (not assigned(def_to)) then
          begin
            isconvertable:=false;
            exit;
          end;

         if (def_from^.deftype=orddef) and (def_to^.deftype=orddef) then
           begin
              doconv:=basedefconverts[porddef(def_from)^.typ,porddef(def_to)^.typ];
              if doconv<>tc_not_possible then
                b:=true;
           end
         else if (def_from^.deftype=orddef) and (def_to^.deftype=floatdef) then
           begin
              if pfloatdef(def_to)^.typ=f32bit then
                doconv:=tc_int_2_fix
              else
                doconv:=tc_int_2_real;
              b:=true;
           end
         else if (def_from^.deftype=floatdef) and (def_to^.deftype=floatdef) then
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
                     Message(parser_w_convert_real_2_comp);
{$endif}
                end;
              b:=true;
           end
         else if (def_from^.deftype=pointerdef) and (def_to^.deftype=arraydef) and
                 (parraydef(def_to)^.lowrange=0) and
                 is_equal(ppointerdef(def_from)^.definition,
                   parraydef(def_to)^.definition) then
           begin
              doconv:=tc_pointer_to_array;
              b:=true;
           end
         else if (def_from^.deftype=arraydef) and (def_to^.deftype=pointerdef) and
                (parraydef(def_from)^.lowrange=0) and
                is_equal(parraydef(def_from)^.definition,
                ppointerdef(def_to)^.definition) then
           begin
              doconv:=tc_array_to_pointer;
              b:=true;
           end
         { typed files are all equal to the abstract file type
         name TYPEDFILE in system.pp in is_equal in types.pas
         the problem is that it sholud be also compatible to FILE
         but this would leed to a problem for ASSIGN RESET and REWRITE
         when trying to find the good overloaded function !!
         so all file function are doubled in system.pp
         this is not very beautiful !!}
         else if (def_from^.deftype=filedef) and (def_to^.deftype=filedef) and
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
         { object pascal objects }
         else if (def_from^.deftype=objectdef) and (def_to^.deftype=objectdef) {and
           pobjectdef(def_from)^.isclass and pobjectdef(def_to)^.isclass }then
           begin
              doconv:=tc_equal;
              b:=pobjectdef(def_from)^.isrelated(
                pobjectdef(def_to));
           end
         { class reference types }
         else if (def_from^.deftype=classrefdef) and (def_from^.deftype=classrefdef) then
           begin
              doconv:=tc_equal;
              b:=pobjectdef(pclassrefdef(def_from)^.definition)^.isrelated(
                pobjectdef(pclassrefdef(def_to)^.definition));
           end

         else if (def_from^.deftype=pointerdef) and (def_to^.deftype=pointerdef) then
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
           if is_equal(def_from,cchardef) and
             (def_to^.deftype=stringdef) then
             begin
                doconv:=tc_char_to_string;
                b:=true;
             end
         else
           { string constant to zero terminated string constant }
           if (fromtreetype=stringconstn) and
             (
              (def_to^.deftype=pointerdef) and
              is_equal(Ppointerdef(def_to)^.definition,cchardef)
             ) then
             begin
                doconv:=tc_cstring_charpointer;
                b:=true;
             end
         else
           { array of char to string                                }
           { the length check is done by the firstpass of this node }
           if (def_from^.deftype=stringdef) and
             (
              (def_to^.deftype=arraydef) and
              is_equal(parraydef(def_to)^.definition,cchardef)
             ) then
             begin
                doconv:=tc_string_chararray;
                b:=true;
             end
         else
           { string to array of char }
           { the length check is done by the firstpass of this node }
           if (
               (def_from^.deftype=arraydef) and
               is_equal(parraydef(def_from)^.definition,cchardef)
              ) and
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
         { procedure variable can be assigned to an void pointer }
         { Not anymore. Use the @ operator now.}
         else
           if not (cs_tp_compatible in aktswitches) then
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
{$ifdef TEST_FUNCRET}
         if p^.symtableentry^.typ=funcretsym then
           begin
              putnode(p);
              p:=genzeronode(funcretn);
              p^.funcretprocinfo:=pprocinfo(pfuncretsym(p^.symtableentry)^.funcretprocinfo);
              p^.retdef:=pfuncretsym(p^.symtableentry)^.funcretdef;
              firstpass(p);
              exit;
           end;
{$endif TEST_FUNCRET}
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
                             { au·erdem kann sie nicht mehr in ein Register
                               geladen werden }
                             pvarsym(p^.symtableentry)^.regable:=false;
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
                     p^.registers32:=1;

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
         lt,rt : ttreetyp;
         t : ptree;
         rv,lv : longint;
         rvd,lvd : {double}bestreal;
         rd,ld : pdef;
         concatstrings : boolean;

         { to evalute const sets }
         resultset : pconstset;
         i : longint;
         b : boolean;
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
         if is_constintnode(p^.left) and
           (rt=realconstn) then
           begin
              t:=genrealconstnode(p^.left^.value);
              disposetree(p^.left);
              p^.left:=t;
              lt:=realconstn;
           end;
         if is_constintnode(p^.right) and
            (lt=realconstn) then
           begin
              t:=genrealconstnode(p^.right^.value);
              disposetree(p^.right);
              p^.right:=t;
              rt:=realconstn;
           end;

         if is_constintnode(p^.left) and
           is_constintnode(p^.right) then
           begin
              lv:=p^.left^.value;
              rv:=p^.right^.value;
              case p^.treetype of
                 addn:
                   t:=genordinalconstnode(lv+rv,s32bitdef);
                 subn:
                   t:=genordinalconstnode(lv-rv,s32bitdef);
                 muln:
                   t:=genordinalconstnode(lv*rv,s32bitdef);
                 xorn:
                   t:=genordinalconstnode(lv xor rv,s32bitdef);
                 orn:
                   t:=genordinalconstnode(lv or rv,s32bitdef);
                 andn:
                   t:=genordinalconstnode(lv and rv,s32bitdef);
                 ltn:
                   t:=genordinalconstnode(ord(lv<rv),booldef);
                 lten:
                   t:=genordinalconstnode(ord(lv<=rv),booldef);
                 gtn:
                   t:=genordinalconstnode(ord(lv>rv),booldef);
                 gten:
                   t:=genordinalconstnode(ord(lv>=rv),booldef);
                 equaln:
                   t:=genordinalconstnode(ord(lv=rv),booldef);
                 unequaln:
                   t:=genordinalconstnode(ord(lv<>rv),booldef);
                 slashn :
                   begin
                      { int/int becomes a real }
                      t:=genrealconstnode(int(lv)/int(rv));
                      firstpass(t);
                   end;
                 else
                   Message(sym_e_type_mismatch);
                end;
              disposetree(p);
              firstpass(t);
              p:=t;
              exit;
              end
         else
           { real constants }
           if (lt=realconstn) and (rt=realconstn) then
           begin
              lvd:=p^.left^.valued;
              rvd:=p^.right^.valued;
              case p^.treetype of
                 addn:
                   t:=genrealconstnode(lvd+rvd);
                 subn:
                   t:=genrealconstnode(lvd-rvd);
                 muln:
                   t:=genrealconstnode(lvd*rvd);
                 caretn:
                   t:=genrealconstnode(exp(ln(lvd)*rvd));
                 slashn:
                   t:=genrealconstnode(lvd/rvd);
                 ltn:
                   t:=genordinalconstnode(ord(lvd<rvd),booldef);
                 lten:
                   t:=genordinalconstnode(ord(lvd<=rvd),booldef);
                 gtn:
                   t:=genordinalconstnode(ord(lvd>rvd),booldef);
                 gten:
                   t:=genordinalconstnode(ord(lvd>=rvd),booldef);
                 equaln:
                   t:=genordinalconstnode(ord(lvd=rvd),booldef);
                 unequaln:
                   t:=genordinalconstnode(ord(lvd<>rvd),booldef);
                 else
                   Message(sym_e_type_mismatch);
              end;
              disposetree(p);
              p:=t;
              firstpass(p);
              exit;
           end;
         concatstrings:=false;
{$ifdef UseAnsiString}
         s1:=nil;
         s2:=nil;
{$else UseAnsiString}
         new(s1);
         new(s2);
{$endif UseAnsiString}
         if (lt=ordconstn) and (rt=ordconstn) and
           (ld^.deftype=orddef) and
           (porddef(ld)^.typ=uchar) and
           (rd^.deftype=orddef) and
           (porddef(rd)^.typ=uchar) then
           begin
{$ifdef UseAnsiString}
              s1:=strpnew(char(byte(p^.left^.value)));
              s2:=strpnew(char(byte(p^.right^.value)));
              l1:=1;l2:=1;
{$else UseAnsiString}
              s1^:=char(byte(p^.left^.value));
              s2^:=char(byte(p^.right^.value));
              concatstrings:=true;
{$endif UseAnsiString}
           end
         else if (lt=stringconstn) and (rt=ordconstn) and
           (rd^.deftype=orddef) and
           (porddef(rd)^.typ=uchar) then
           begin
{$ifdef UseAnsiString}
              { here there is allways the damn #0 problem !! }
              s1:=getpcharcopy(p^.left);
              l1:=p^.left^.length;
              s2:=strpnew(char(byte(p^.right^.value)));
              l2:=1;
{$else UseAnsiString}
              s1^:=p^.left^.values^;
              s2^:=char(byte(p^.right^.value));
              concatstrings:=true;
{$endif UseAnsiString}
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
              s2^:=p^.right^.values^;
              concatstrings:=true;
{$endif UseAnsiString}
           end
         else if (lt=stringconstn) and (rt=stringconstn) then
           begin
{$ifdef UseAnsiString}
              s1:=getpcharcopy(p^.left);
              l1:=p^.left^.length;
              s2:=getpcharcopy(p^.right);
              l2:=p^.right^.length;
              concatstrings:=true;
{$else UseAnsiString}
              s1^:=p^.left^.values^;
              s2^:=p^.right^.values^;
              concatstrings:=true;
{$endif UseAnsiString}
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

         { we can set this globally but it not allways true }
         { procinfo.flags:=procinfo.flags or pi_do_call;    }

         { if both are boolean: }
         if ((ld^.deftype=orddef) and
            (porddef(ld)^.typ in [bool8bit,bool16bit,bool32bit])) and
            ((rd^.deftype=orddef) and
            (porddef(rd)^.typ in [bool8bit,bool16bit,bool32bit])) then
           begin
             case p^.treetype of
             andn,orn : begin
                          calcregisters(p,0,0,0);
                          p^.location.loc:=LOC_JUMP;
                        end;
             unequaln,
          equaln,xorn : begin
                          make_bool_equal_size(p);
                          calcregisters(p,1,0,0);
                        end
             else
               Message(sym_e_type_mismatch);
             end;
           end
         { wenn beides vom Char dann keine Konvertiereung einfÅgen }
         { hîchstens es handelt sich um einen +-Operator           }
         else if ((rd^.deftype=orddef) and (porddef(rd)^.typ=uchar)) and
                 ((ld^.deftype=orddef) and (porddef(ld)^.typ=uchar)) then
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
            end
         { if string and character, then conver the character to a string }
         else if ((rd^.deftype=stringdef) and
                 ((ld^.deftype=orddef) and (porddef(ld)^.typ=uchar))) or
                 ((ld^.deftype=stringdef) and
                 ((rd^.deftype=orddef) and (porddef(rd)^.typ=uchar))) then
           begin
              if ((ld^.deftype=orddef) and (porddef(ld)^.typ=uchar)) then
                p^.left:=gentypeconvnode(p^.left,cstringdef)
              else
                p^.right:=gentypeconvnode(p^.right,cstringdef);
              firstpass(p^.left);
              firstpass(p^.right);
              { here we call STRCONCAT or STRCMP }
              procinfo.flags:=procinfo.flags or pi_do_call;
              calcregisters(p,0,0,0);
              p^.location.loc:=LOC_MEM;
           end
         else
           if ((rd^.deftype=setdef) and (ld^.deftype=setdef)) then
             begin
                case p^.treetype of
                   subn,symdifn,addn,muln,equaln,unequaln : ;
                   else Message(sym_e_type_mismatch);
                end;
                if not(is_equal(rd,ld)) then
                 Message(sym_e_set_element_are_not_comp);
                { why here its is alredy in entry of firstadd
                firstpass(p^.left);
                firstpass(p^.right); }
                { do constant evalution }
                { set constructor ? }
                if (p^.right^.treetype=setconstrn) and
                  (p^.left^.treetype=setconstrn) and
                  { and no variables ? }
                  (p^.right^.left=nil) and
                  (p^.left^.left=nil) then
                  begin
                     new(resultset);
                     case p^.treetype of
                        addn : begin
                                  for i:=0 to 31 do
                                    resultset^[i]:=
                                      p^.right^.constset^[i] or p^.left^.constset^[i];
                                  t:=gensetconstruktnode(resultset,psetdef(ld));
                               end;
                        muln : begin
                                  for i:=0 to 31 do
                                    resultset^[i]:=
                                      p^.right^.constset^[i] and p^.left^.constset^[i];
                                  t:=gensetconstruktnode(resultset,psetdef(ld));
                               end;
                        subn : begin
                                  for i:=0 to 31 do
                                    resultset^[i]:=
                                      p^.left^.constset^[i] and not(p^.right^.constset^[i]);
                                  t:=gensetconstruktnode(resultset,psetdef(ld));
                               end;
                        symdifn : begin
                                  for i:=0 to 31 do
                                    resultset^[i]:=
                                      p^.left^.constset^[i] xor p^.right^.constset^[i];
                                  t:=gensetconstruktnode(resultset,psetdef(ld));
                               end;
                        unequaln : begin
                                      b:=true;
                                      for i:=0 to 31 do
                                        if p^.right^.constset^[i]=p^.left^.constset^[i] then
                                          begin
                                             b:=false;
                                             break;
                                          end;
                                      t:=genordinalconstnode(ord(b),booldef);
                                   end;
                        equaln : begin
                                    b:=true;
                                    for i:=0 to 31 do
                                      if p^.right^.constset^[i]<>p^.left^.constset^[i] then
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
                else if psetdef(rd)^.settype=smallset then
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
             end
         else
           if ((rd^.deftype=stringdef) and (ld^.deftype=stringdef)) then
             { here we call STR... }
             procinfo.flags:=procinfo.flags or pi_do_call
         { if there is a real float, convert both to float 80 bit }
         else
         if ((rd^.deftype=floatdef) and (pfloatdef(rd)^.typ<>f32bit)) or
           ((ld^.deftype=floatdef) and (pfloatdef(ld)^.typ<>f32bit)) then
           begin
              p^.right:=gentypeconvnode(p^.right,c64floatdef);
              p^.left:=gentypeconvnode(p^.left,c64floatdef);
              firstpass(p^.left);
              firstpass(p^.right);
              calcregisters(p,1,1,0);
              p^.location.loc:=LOC_FPU;
           end
         else
          { if there is one fix comma number, convert both to 32 bit fixcomma }
           if ((rd^.deftype=floatdef) and (pfloatdef(rd)^.typ=f32bit)) or
             ((ld^.deftype=floatdef) and (pfloatdef(ld)^.typ=f32bit)) then
            begin
               if not(porddef(rd)^.typ in [u8bit,s8bit,u16bit,
                 s16bit,s32bit]) or (p^.treetype<>muln) then
                   p^.right:=gentypeconvnode(p^.right,s32fixeddef);

               if not(porddef(rd)^.typ in [u8bit,s8bit,u16bit,
                 s16bit,s32bit]) or (p^.treetype<>muln) then
               p^.left:=gentypeconvnode(p^.left,s32fixeddef);

               firstpass(p^.left);
               firstpass(p^.right);
               calcregisters(p,1,0,0);
               p^.location.loc:=LOC_REGISTER;
            end
         { pointer comperation and subtraction }
         else if (rd^.deftype=pointerdef) and (ld^.deftype=pointerdef) then
           begin
              p^.location.loc:=LOC_REGISTER;
              p^.right:=gentypeconvnode(p^.right,ld);
              firstpass(p^.right);
              calcregisters(p,1,0,0);
              case p^.treetype of
                 equaln,unequaln : ;
                 ltn,lten,gtn,gten:
                   begin
                      if not(cs_extsyntax in aktswitches) then
                        Message(sym_e_type_mismatch);
                   end;
                 subn:
                   begin
                      if not(cs_extsyntax in aktswitches) then
                        Message(sym_e_type_mismatch);
                      p^.resulttype:=s32bitdef;
                      exit;
                   end;
                 else Message(sym_e_type_mismatch);
              end;
           end
         else if (rd^.deftype=objectdef) and (ld^.deftype=objectdef) and
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
                 else Message(sym_e_type_mismatch);
              end;
           end
         else if (rd^.deftype=classrefdef) and (ld^.deftype=classrefdef) then
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
                 else Message(sym_e_type_mismatch);
              end;
           end

         { allows comperasion with nil pointer }
         else if (rd^.deftype=objectdef) and
           pobjectdef(rd)^.isclass then
           begin
              p^.location.loc:=LOC_REGISTER;
              p^.left:=gentypeconvnode(p^.left,rd);
              firstpass(p^.left);
              calcregisters(p,1,0,0);
              case p^.treetype of
                 equaln,unequaln : ;
                 else Message(sym_e_type_mismatch);
              end;
           end
         else if (ld^.deftype=objectdef) and
           pobjectdef(ld)^.isclass then
           begin
              p^.location.loc:=LOC_REGISTER;
              p^.right:=gentypeconvnode(p^.right,ld);
              firstpass(p^.right);
              calcregisters(p,1,0,0);
              case p^.treetype of
                 equaln,unequaln : ;
                 else Message(sym_e_type_mismatch);
              end;
           end
         else if (rd^.deftype=classrefdef) then
           begin
              p^.left:=gentypeconvnode(p^.left,rd);
              firstpass(p^.left);
              calcregisters(p,1,0,0);
              case p^.treetype of
                 equaln,unequaln : ;
                 else Message(sym_e_type_mismatch);
              end;
           end
         else if (ld^.deftype=classrefdef) then
           begin
              p^.right:=gentypeconvnode(p^.right,ld);
              firstpass(p^.right);
              calcregisters(p,1,0,0);
              case p^.treetype of
                 equaln,unequaln : ;
                 else Message(sym_e_type_mismatch);
              end;
           end

         else if (rd^.deftype=pointerdef) then
           begin
              p^.location.loc:=LOC_REGISTER;
              p^.left:=gentypeconvnode(p^.left,s32bitdef);
              firstpass(p^.left);
              calcregisters(p,1,0,0);
              if p^.treetype=addn then
                begin
                   if not(cs_extsyntax in aktswitches) then
                     Message(sym_e_type_mismatch);
                end
              else Message(sym_e_type_mismatch);
           end
         else if (ld^.deftype=pointerdef) then
           begin
              p^.location.loc:=LOC_REGISTER;
              p^.right:=gentypeconvnode(p^.right,s32bitdef);
              firstpass(p^.right);
              calcregisters(p,1,0,0);
              case p^.treetype of
                 addn,subn : if not(cs_extsyntax in aktswitches) then
                               Message(sym_e_type_mismatch);
                 else Message(sym_e_type_mismatch);
              end;
           end
         else if (rd^.deftype=procvardef) and (ld^.deftype=procvardef) and
           is_equal(rd,ld) then
           begin
              calcregisters(p,1,0,0);
              p^.location.loc:=LOC_REGISTER;
              case p^.treetype of
                 equaln,unequaln : ;
                 else Message(sym_e_type_mismatch);
              end;
           end
         else if (ld^.deftype=enumdef) and (rd^.deftype=enumdef)
            and (is_equal(ld,rd)) then
           begin
              calcregisters(p,1,0,0);
              case p^.treetype of
                 equaln,unequaln,
                 ltn,lten,gtn,gten : ;
                 else Message(sym_e_type_mismatch);
              end;
           end
{$ifdef SUPPORT_MMX}
         else if (cs_mmx in aktswitches) and is_mmx_able_array(ld)
           and is_mmx_able_array(rd) and is_equal(ld,rd) then
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
                    Message(sym_e_type_mismatch);
                else
                  Message(sym_e_type_mismatch);
              end;
              p^.location.loc:=LOC_MMXREGISTER;
              calcregisters(p,0,0,1);
       end
{$endif SUPPORT_MMX}
         { the general solution is to convert to 32 bit int }
         else
           begin
              { but an int/int gives real/real! }
              if p^.treetype=slashn then
                begin
                   Message(parser_w_use_int_div_int_op);
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
         p^.resulttype:=new(pstringdef,init(length(p^.values^)));}
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
              t:=genrealconstnode(-p^.left^.valued);
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
         else if (cs_mmx in aktswitches) and
           is_mmx_able_array(p^.left^.resulttype) then
             begin
               if (p^.left^.location.loc<>LOC_MMXREGISTER) and
                 (p^.registersmmx<1) then
                 p^.registersmmx:=1;
               { if saturation is on, p^.left^.resulttype isn't
                 "mmx able" (FK)
               if (cs_mmx_saturation in aktswitches^) and
                 (porddef(parraydef(p^.resulttype)^.definition)^.typ in
                 [s32bit,u32bit]) then
                 Message(sym_e_type_mismatch);
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
              Message(sym_e_type_mismatch);
           end;
      end;

    procedure firstaddr(var p : ptree);

      var
         hp  : ptree;
         hp2 : pdefcoll;
         store_valid : boolean;

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
                   if not(cs_tp_compatible in aktswitches) then
                     begin
                        p^.resulttype:=new(pprocvardef,init);

                        pprocvardef(p^.resulttype)^.options:=
                          p^.left^.symtableprocentry^.definition^.options;

                        pprocvardef(p^.resulttype)^.retdef:=
                          p^.left^.symtableprocentry^.definition^.retdef;

                        hp2:=p^.left^.symtableprocentry^.definition^.para1;
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
                  if not(cs_typed_addresses in aktswitches) then
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
           if (cs_mmx in aktswitches) and
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
         { assignements to open arrays aren't allowed }
         if is_open_array(p^.left^.resulttype) then
           Message(sym_e_type_mismatch);
         { test if we can avoid copying string to temp
           as in s:=s+...; (PM) }
{$ifdef dummyi386}
         if ((p^.right^.treetype=addn) or (p^.right^.treetype=subn)) and
            equal_trees(p^.left,p^.right^.left) and
            (ret_in_acc(p^.left^.resulttype)) and
            (not cs_rangechecking in aktswitches^) then
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

         if (p^.left^.resulttype^.deftype=stringdef) and (assigned(p^.right^.resulttype)) then
          begin
            if not (p^.right^.resulttype^.deftype in [stringdef,orddef]) then
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
         { allow only ordinal constants }
         if not((p^.left^.treetype=ordconstn) and
            (p^.right^.treetype=ordconstn)) then
           Message(cg_e_illegal_expression);
         { upper limit must be greater or equalt than lower limit }
         { not if u32bit }
         if (p^.left^.value>p^.right^.value) and
            (( p^.left^.value<0) or (p^.right^.value>=0)) then
           Message(cg_e_upper_lower_than_lower);
         { both types must be compatible }
         if not(isconvertable(p^.left^.resulttype,p^.right^.resulttype,
           ct,ordconstn,false)) and
           not(is_equal(p^.left^.resulttype,p^.right^.resulttype)) then
           Message(sym_e_type_mismatch);
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
                Message(sym_e_type_mismatch);
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
           else
           { indexed access to arrays }
             p^.resulttype:=cchardef;

         { the register calculation is easy if a const index is used }
         if p^.right^.treetype=ordconstn then
           p^.registers32:=p^.left^.registers32
         else
           begin
              p^.registers32:=max(p^.left^.registers32,p^.right^.registers32);

              { not correct, but what works better ? }
              if p^.left^.registers32>0 then
                p^.registers32:=max(p^.registers32,2)
              else
              { min. one register }
                p^.registers32:=max(p^.registers32,1);
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
              { call shortstring_to_ansistring or ansistring_to_shortstring }
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
              p^.valuef:=p^.left^.value shl 16;
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
              p^.valuef:=round(p^.left^.valued*65536);
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
              p^.valued:=round(p^.left^.valuef/65536.0);
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
       begin
          is_assignment_overloaded:=false;
          if assigned(overloaded_operators[assignment]) then
            passproc:=overloaded_operators[assignment]^.definition
          else
            passproc:=nil;
          while passproc<>nil do
            begin
              if (passproc^.retdef=to_def) and (passproc^.para1^.data=from_def) then
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
       firstconvert : array[tc_u8bit_2_s32bit..tc_cchar_charpointer] of
         tfirstconvproc = (first_bigger_smaller,first_nothing,first_bigger_smaller,
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
                           first_cchar_charpointer);

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

       { remove obsolete type conversions }
       if is_equal(p^.left^.resulttype,p^.resulttype) then
         begin
            hp:=p;
            p:=p^.left;
            p^.resulttype:=hp^.resulttype;
            putnode(hp);
            exit;
         end;
       p^.registers32:=p^.left^.registers32;
       p^.registersfpu:=p^.left^.registersfpu;
{$ifdef SUPPORT_MMX}
       p^.registersmmx:=p^.left^.registersmmx;
{$endif}
       set_location(p^.location,p^.left^.location);
       if (not(isconvertable(p^.left^.resulttype,p^.resulttype,
           p^.convtyp,p^.left^.treetype,p^.explizit))) then
         begin
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
           {Procedures have a resulttype of voiddef and functions of their
           own resulttype. They will therefore always be incompatible with
           a procvar. Because isconvertable cannot check for procedures we
           use an extra check for them.}
           if (cs_tp_compatible in aktswitches) and
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
                               Message(sym_e_type_mismatch);
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
                        Message(sym_e_type_mismatch);
                      end;
                    aprocdef^.deftype:=proctype;
                    firstconvert[p^.convtyp](p);
                  end
                else
                  Message(sym_e_type_mismatch);
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
                     if (p^.resulttype^.deftype in [recorddef,stringdef,arraydef,objectdef]) and
                        (p^.left^.location.loc in [LOC_REGISTER,LOC_CREGISTER]) then
                       Message(cg_e_illegal_type_conversion);
                end
              else
                Message(sym_e_type_mismatch);
           end
         end
       else
         begin
            { just a test: p^.explizit:=false; }
            { ordinale contants are direct converted }
            if (p^.left^.treetype=ordconstn) and is_ordinal(p^.resulttype) then
              begin
                 { perform range checking }
                 if not(p^.explizit and (cs_tp_compatible in aktswitches)) then
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
              if not((p^.left^.resulttype^.deftype=stringdef) and
                     (defcoll^.data^.deftype=stringdef)) and
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
              if (cs_strict_var_strings in aktswitches) and
                 (p^.left^.resulttype^.deftype=stringdef) and
                 (defcoll^.data^.deftype=stringdef) and
                 (defcoll^.paratyp=vs_var) and
                 not(is_equal(p^.left^.resulttype,defcoll^.data)) then
                 Message(parser_e_strict_var_string_violation);
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
     {$ifdef UseBrowser}
         curtokenpos : tfileposinfo;
     {$endif UseBrowser}

         { only Dummy }
         hcvt : tconverttype;
         regi : tregister;
         store_valid, old_count_ref : boolean;

      { types.is_equal can't handle a formaldef ! }
      function is_equal(def1,def2 : pdef) : boolean;

        begin
           { all types can be passed to a  formaldef  }
           is_equal:=(def1^.deftype=formaldef) or
             (assigned(def2) and types.is_equal(def1,def2));
        end;

      function is_in_limit(def_from,def_to : pdef) : boolean;

        begin
           is_in_limit:=(def_from^.deftype = orddef) and
                        (def_to^.deftype = orddef) and
                        (porddef(def_from)^.low>porddef(def_to)^.low) and
                        (porddef(def_from)^.high<porddef(def_to)^.high);
        end;


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
                   actprocsym:=p^.symtableprocentry;
                   { determine length of parameter list }
                   pt:=p^.left;
                   paralength:=0;
                   while assigned(pt) do
                     begin
                        inc(paralength);
                        pt:=pt^.right;
                     end;

                   { alle in Frage kommenden Prozeduren in eine }
                   { verkettete Liste einfÅgen                  }
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
                             { nur wenn die ParameterlÑnge pa·t, dann EinfÅgen }
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

                   { nun alle Parameter nacheinander vergleichen }
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
                        { sollte nirgendwo ein Parameter exakt passen, }
                        { so alle Prozeduren entfernen, bei denen      }
                        { der Parameter auch nach einer impliziten     }
                        { Typkonvertierung nicht passt                 }
                        else
                          begin
                             { erst am Anfang }
                             while (assigned(procs)) and
                               not(isconvertable(pt^.resulttype,procs^.nextpara^.data,
                                 hcvt,pt^.left^.treetype,false)) do
                               begin
                                  hp:=procs^.next;
                                  dispose(procs);
                                  procs:=hp;
                               end;
                             { und jetzt aus der Mitte }
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
                        { nun bei denn Prozeduren den nextpara-Zeiger auf den }
                        { naechsten Parameter setzen                          }
                        hp:=procs;
                        while assigned(hp) do
                          begin
                             hp^.nextpara:=hp^.nextpara^.next;
                             hp:=hp^.next;
                          end;
                        pt:=pt^.right;
                     end;

                   if procs=nil then
                     if (parsing_para_level=0) or (p^.left<>nil) then
                       begin
                          Message(parser_e_illegal_parameter_list);
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
                                       if (def_from^.deftype=orddef) and (def_to^.deftype=orddef) then
                                         if is_in_limit(def_from,def_to) or
                                           ((hp^.nextpara^.paratyp=vs_var) and
                                           (def_from^.size=def_to^.size)) then
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
                             { nun bei denn Prozeduren den nextpara-Zeiger auf den }
                             { naechsten Parameter setzen                          }
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
                     Message(cg_e_cant_choose_overload_function);
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
                          Message(cg_e_cant_choose_overload_function);
                          error(too_much_matches);
                     end;
{$endif CHAINPROCSYMS}
     {$ifdef UseBrowser}
                   if make_ref then
                     begin
                        get_cur_file_pos(curtokenpos);
                        add_new_ref(procs^.data^.lastref,@curtokenpos);
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

              { handle predefined procedures }
              if (p^.procdefinition^.options and pointernproc)<>0 then
                begin
                   { settextbuf needs two args }
                   if assigned(p^.left^.right) then
                     pt:=geninlinenode(pprocdef(p^.procdefinition)^.extnumber,p^.left)
                   else
                     begin
                        pt:=geninlinenode(pprocdef(p^.procdefinition)^.extnumber,p^.left^.left);
                        putnode(p^.left);
                     end;
                   putnode(p);
                   firstpass(pt);
                   { was placed after the exit          }
                   { caused GPF                         }
                   { error caused and corrected by (PM) }
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
                     comment(v_fatal,'Unable to inline object methods');
                   if assigned(p^.right) and (p^.right^.treetype<>procinlinen) then
                     comment(v_fatal,'Unable to inline procvar calls');
                   { p^.treetype:=procinlinen; }
                   if not assigned(p^.right) then
                     begin
                        if assigned(p^.procdefinition^.code) then
                          inlinecode:=genprocinlinenode(p,ptree(p^.procdefinition^.code))
                        else
                          comment(v_fatal,'no code for inline procedure stored');
                        if assigned(inlinecode) then
                          begin
                             firstpass(inlinecode);
                             { consider it has not inlined if called
                               again inside the args }
                             p^.procdefinition^.options:=p^.procdefinition^.options and (not poinline);
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
                   if (cs_mmx in aktswitches) and
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
{$ifdef TEST_FUNCRET}
         p^.resulttype:=p^.retdef;
         p^.location.loc:=LOC_REFERENCE;
         if ret_in_param(p^.retdef) or
            (@procinfo<>pprocinfo(p^.funcretprocinfo)) then
           p^.registers32:=1;
         { no claim if setting higher return values }
         if must_be_valid and
            (@procinfo=pprocinfo(p^.funcretprocinfo)) and
            not procinfo.funcret_is_valid then
           note(uninitialized_function_return);
         if count_ref then pprocinfo(p^.funcretprocinfo)^.funcret_is_valid:=true;
{$else TEST_FUNCRET}
         p^.resulttype:=procinfo.retdef;
         p^.location.loc:=LOC_REFERENCE;
         if ret_in_param(procinfo.retdef) then
           p^.registers32:=1;
         if must_be_valid and
           not(procinfo.funcret_is_valid) {and
           ((procinfo.flags and pi_uses_asm)=0)} then
           Message(sym_w_function_result_not_set);
         if count_ref then procinfo.funcret_is_valid:=true;
{$endif TEST_FUNCRET}
          end;


    { intern inline suborutines }
    procedure firstinline(var p : ptree);

      var
         hp,hpp : ptree;
         store_count_ref,isreal,store_valid,file_is_typed : boolean;

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
              p^.registers32:=p^.left^.registers32;
              p^.registersfpu:=p^.left^.registersfpu;
{$ifdef SUPPORT_MMX}
              p^.registersmmx:=p^.left^.registersmmx;
{$endif SUPPORT_MMX}
              set_location(p^.location,p^.left^.location);
           end;
           case p^.inlinenumber of
             in_lo_word,in_hi_word:
               begin
                  if p^.registers32<1 then
                    p^.registers32:=1;
                  p^.resulttype:=u8bitdef;
                  p^.location.loc:=LOC_REGISTER;
               end;
             in_lo_long,in_hi_long:
               begin
                  if p^.registers32<1 then
                    p^.registers32:=1;
                  p^.resulttype:=u16bitdef;
                  p^.location.loc:=LOC_REGISTER;
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
                           Message(sym_e_type_mismatch)
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
                            Message(sym_e_type_mismatch);
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
                       hp:=genordinalconstnode(length(p^.left^.values^),s32bitdef);
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
                  p^.resulttype:=p^.left^.resulttype;
                  p^.location.loc:=LOC_REGISTER;
                  if not is_ordinal(p^.resulttype) then
                     Message(sym_e_type_mismatch)
                  else
                    begin
                  if (p^.resulttype^.deftype=enumdef) and
                     (penumdef(p^.resulttype)^.has_jumps) then
                    begin
                      Message(parser_e_succ_and_pred_enums_with_assign_not_possible);
                    end
                  else if p^.left^.treetype=ordconstn then
                         begin
                            if p^.inlinenumber=in_pred_x then
                              hp:=genordinalconstnode(p^.left^.value+1,
                                p^.left^.resulttype)
                            else
                              hp:=genordinalconstnode(p^.left^.value-1,
                                p^.left^.resulttype);
                            disposetree(p);
                            firstpass(hp);
                            p:=hp;
                         end;
                    end;
               end;
             in_dec_dword,
             in_dec_word,
             in_dec_byte,
             in_inc_dword,
             in_inc_word,
             in_inc_byte :
               begin
                  p^.resulttype:=voiddef;
                  if p^.left^.location.loc<>LOC_REFERENCE then
                    Message(cg_e_illegal_expression);
               end;
            in_inc_x,
            in_dec_x:
              begin
                 p^.resulttype:=voiddef;
                 if assigned(p^.left) then
                   begin
                      firstcallparan(p^.left,nil);
                      { first param must be var }
                      if p^.left^.left^.location.loc<>LOC_REFERENCE then
                        Message(cg_e_illegal_expression);
                      { check type }
                      if (p^.left^.resulttype^.deftype=pointerdef) or
                        (p^.left^.resulttype^.deftype=enumdef) or
                        ( (p^.left^.resulttype^.deftype=orddef) and
                          (porddef(p^.left^.resulttype)^.typ in [u8bit,s8bit,u16bit,s16bit,u32bit,s32bit])
                        ) then
                        begin
                           { two paras ? }
                           if assigned(p^.left^.right) then
                             begin
                                { insert a type conversion         }
                                { the second param is always longint }
                                p^.left^.right^.left:=gentypeconvnode(
                                  p^.left^.right^.left,
                                  s32bitdef);
                                { check the type conversion }
                                firstpass(p^.left^.right^.left);
                                if assigned(p^.left^.right^.right) then
                                  Message(cg_e_illegal_expression);
                             end;
                        end
                      else
                        Message(sym_e_type_mismatch);
                   end
                 else
                   Message(sym_e_type_mismatch);
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
                              { test the type here
                                so we can use a trick in cgi386 (PM) }
                              hpp:=p^.left;
                              while (hpp<>hp) do
                                begin
                                   { should we allow type conversion ? (PM)
                                   if not isconvertable(hpp^.resulttype,
                                     pfiledef(hp^.resulttype)^.typed_as,convtyp,hpp^.treetype) then
                                     Message(sym_e_type_mismatch);
                                   if not(is_equal(hpp^.resulttype,pfiledef(hp^.resulttype)^.typed_as)) then
                                     begin
                                        hpp^.left:=gentypeconvnode(hpp^.left,pfiledef(hp^.resulttype)^.typed_as);
                                     end; }
                                   if not is_equal(hpp^.resulttype,pfiledef(hp^.resulttype)^.typed_as) then
                                     Message(sym_e_type_mismatch);
                                   hpp:=hpp^.right;
                                end;
                              { once again for typeconversions }
                              firstcallparan(p^.left,nil);
                           end;
                         end; { endif assigned(hp) }
                       { insert type conversions for write(ln) }
                       if (not file_is_typed) and
                          ((p^.inlinenumber=in_write_x) or (p^.inlinenumber=in_writeln_x)) then
                         begin
                            hp:=p^.left;
                            while assigned(hp) do
                              begin
                                if assigned(hp^.left^.resulttype) then
                                  begin
                                   if hp^.left^.resulttype^.deftype=floatdef then
                                     begin
                                        isreal:=true;
                                     end
                                   else if hp^.left^.resulttype^.deftype=orddef then
                                     case porddef(hp^.left^.resulttype)^.typ of
                                       u8bit,s8bit,
                                       u16bit,s16bit :
                                         hp^.left:=gentypeconvnode(hp^.left,s32bitdef);
                                       end
                                   { but we convert only if the first index<>0, because in this case }
                                   { we have a ASCIIZ string                                         }
                                   else if (hp^.left^.resulttype^.deftype=arraydef) and
                                           (parraydef(hp^.left^.resulttype)^.lowrange<>0) and
                                           (parraydef(hp^.left^.resulttype)^.definition^.deftype=orddef) and
                                           (porddef(parraydef(hp^.left^.resulttype)^.definition)^.typ=uchar) then
                                     hp^.left:=gentypeconvnode(hp^.left,cstringdef);
                                  end;
                                 hp:=hp^.right;
                              end;
                         end;
                       { pass all parameters again }
                       firstcallparan(p^.left,nil);
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
                       isreal:=false;
                       { valid string ? }
                       if not assigned(hp) or
                          (hp^.left^.resulttype^.deftype<>stringdef) or
                          (hp^.right=nil) or
                          (hp^.left^.location.loc<>LOC_REFERENCE) then
                         Message(cg_e_illegal_expression);
                       { !!!! check length of string }

                       while assigned(hp^.right) do hp:=hp^.right;

                       { check and convert the first param }
                       if hp^.is_colon_para then
                         Message(cg_e_illegal_expression)
                       else if hp^.resulttype^.deftype=orddef then
                         case porddef(hp^.left^.resulttype)^.typ of
                           u8bit,s8bit,
                           u16bit,s16bit :
                             hp^.left:=gentypeconvnode(hp^.left,s32bitdef);
                         end
                       else if hp^.resulttype^.deftype=floatdef then
                         begin
                            isreal:=true;
                         end
                       else Message(cg_e_illegal_expression);

                       { some format options ? }
                       hp:=p^.left^.right;
                       if assigned(hp) and hp^.is_colon_para then
                         begin
                            hp^.left:=gentypeconvnode(hp^.left,s32bitdef);
                            hp:=hp^.right;
                         end;
                       if assigned(hp) and hp^.is_colon_para then
                         begin
                            if isreal then
                              hp^.left:=gentypeconvnode(hp^.left,s32bitdef)
                            else
                              Message(parser_e_illegal_colon_qualifier);
                            hp:=hp^.right;
                         end;

                       { for first local use }
                       must_be_valid:=false;
                       count_ref:=true;
                       if assigned(hp) then
                         firstcallparan(hp,nil);
                    end
                  else
                    Message(parser_e_illegal_parameter_list);
                  { check params once more }
                  if codegenerror then
                    exit;
                  must_be_valid:=true;
                  firstcallparan(p^.left,nil);
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
                        Message(sym_e_type_mismatch);
                   end
                 else
                   Message(sym_e_type_mismatch);
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
                           Message(sym_e_type_mismatch);
                         end;
                    end
                  else
                    Message(parser_e_varid_or_typeid_expected);
               end
                 else internalerror(8);
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
           Message(parser_e_pointer_type_expected);

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

    procedure firstsetcons(var p : ptree);

      var
         hp : ptree;

      begin
         p^.location.loc:=LOC_MEM;
         hp:=p^.left;
         { is done by getnode*
         p^.registers32:=0;
         p^.registersfpu:=0;
         }
         while assigned(hp) do
           begin
              firstpass(hp^.left);

              if codegenerror then
                exit;

              p^.registers32:=max(p^.registers32,hp^.left^.registers32);
              p^.registersfpu:=max(p^.registersfpu,hp^.left^.registersfpu);;
{$ifdef SUPPORT_MMX}
              p^.registersmmx:=max(p^.registersmmx,hp^.left^.registersmmx);
{$endif SUPPORT_MMX}
              hp:=hp^.right;
           end;
         { result type is already set }
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
           procinfo.flags:=procinfo.flags or pi_do_call;
      end;

    procedure firststatement(var p : ptree);

      begin
         { left is the next statement in the list }
         p^.resulttype:=voiddef;

         { no temps over several statements }
         cleartempgen;
         { right is the statement itself calln assignn or a complex one }
         firstpass(p^.right);
         if (not (cs_extsyntax in aktswitches)) and
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
              if cs_maxoptimieren in aktswitches then
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
                        set_current_file_line(hp^.left);
                        disposetree(hp^.left);
                        hp^.left:=nil;
                        Message(cg_w_unreachable_code);

                        { old lines }
                        set_current_file_line(hp^.right);
                     end;
                end;
              if assigned(hp^.right) then
                begin
                   cleartempgen;
                   firstpass(hp^.right);
                   if (not (cs_extsyntax in aktswitches)) and
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
         if not(cs_littlesize in aktswitches ) then
           t_times:=t_times*8;

         cleartempgen;
         must_be_valid:=true;
         firstpass(p^.left);
         if codegenerror then
           exit;
         if not((p^.left^.resulttype^.deftype=orddef) and
            (porddef(p^.left^.resulttype)^.typ=bool8bit)) then
            begin
               Message(sym_e_type_mismatch);
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
            (porddef(p^.left^.resulttype)^.typ=bool8bit)) then
            begin
               Message(sym_e_type_mismatch);
               exit;
            end;

         p^.registers32:=p^.left^.registers32;
         p^.registersfpu:=p^.left^.registersfpu;
{$ifdef SUPPORT_MMX}
         p^.registersmmx:=p^.left^.registersmmx;
{$endif SUPPORT_MMX}

         { determines registers weigths }
         if not(cs_littlesize in aktswitches ) then
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
         if not(cs_littlesize in aktswitches ) then
           t_times:=t_times*8;

         cleartempgen;
         if p^.t1<>nil then
           firstpass(p^.t1);

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
          Message(parser_e_ordinal_expected);

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
           I think also, that all values should be set to their maximum (FK)
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
         if not(cs_littlesize in aktswitches ) then
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
      end;

    procedure firsttryfinally(var p : ptree);

      begin
      end;

    procedure firstis(var p : ptree);

      begin
         firstpass(p^.left);
         firstpass(p^.right);

         if (p^.right^.resulttype^.deftype<>classrefdef) then
           Message(sym_e_type_mismatch);
         if codegenerror then
           exit;

         left_right_max(p);

         { left must be a class }
         if (p^.left^.resulttype^.deftype<>objectdef) or
           not(pobjectdef(p^.left^.resulttype)^.isclass) then
           Message(sym_e_type_mismatch);

         { the operands must be related }
         if (not(pobjectdef(p^.left^.resulttype)^.isrelated(
           pobjectdef(pclassrefdef(p^.right^.resulttype)^.definition)))) and
           (not(pobjectdef(pclassrefdef(p^.right^.resulttype)^.definition)^.isrelated(
           pobjectdef(p^.left^.resulttype)))) then
           Message(sym_e_type_mismatch);

         p^.location.loc:=LOC_FLAGS;
         p^.resulttype:=booldef;
      end;

    procedure firstas(var p : ptree);

      begin
         firstpass(p^.right);
         firstpass(p^.left);
         if (p^.right^.resulttype^.deftype<>classrefdef) then
           Message(sym_e_type_mismatch);

         if codegenerror then
           exit;

         left_right_max(p);
(*       this was wrong,no ??
         p^.registersfpu:=max(p^.left^.registersfpu,p^.left^.registersfpu);
         p^.registers32:=max(p^.left^.registers32,p^.right^.registers32);
{$ifdef SUPPORT_MMX}
         p^.registersmmx:=max(p^.left^.registersmmx,p^.right^.registersmmx);
{$endif SUPPORT_MMX}             *)

         { left must be a class }
         if (p^.left^.resulttype^.deftype<>objectdef) or
           not(pobjectdef(p^.left^.resulttype)^.isclass) then
           Message(sym_e_type_mismatch);

         { the operands must be related }
         if (not(pobjectdef(p^.left^.resulttype)^.isrelated(
           pobjectdef(pclassrefdef(p^.right^.resulttype)^.definition)))) and
           (not(pobjectdef(pclassrefdef(p^.right^.resulttype)^.definition)^.isrelated(
           pobjectdef(p^.left^.resulttype)))) then
           Message(sym_e_type_mismatch);

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
                Message(sym_e_type_mismatch);

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
             firstsimplenewdispose,firstnothing,firstsetcons,firstblock,
             firststatement,firstnothing,firstif,firstnothing,
             firstnothing,first_while_repeat,first_while_repeat,firstfor,
             firstexitn,firstwith,firstcase,firstlabel,
             firstgoto,firstsimplenewdispose,firsttryexcept,firstraise,
             firstnothing,firsttryfinally,firstis,firstas,firstadd,
             firstnothing,firstadd,firstprocinline,firstnothing,firstloadvmt);

      var
         oldcodegenerror : boolean;
         oldswitches : Tcswitches;
         { there some calls of do_firstpass in the parser }
         oldis : pinputfile;
         oldnr : longint;
{$ifdef extdebug}
         str1,str2 : string;
         oldp : ptree;
         not_first : boolean;
{$endif extdebug}

      begin
{$ifdef extdebug}
         if (p^.firstpasscount>0) and only_one_pass then
           exit;
{$endif extdebug}
         { if we save there the whole stuff, }
         { line numbers become more correct  }
         oldis:=current_module^.current_inputfile;
         oldnr:=current_module^.current_inputfile^.line_no;
         oldcodegenerror:=codegenerror;
         oldswitches:=aktswitches;
{$ifdef extdebug}
         if p^.firstpasscount>0 then
           begin
              move(p^,str1[1],sizeof(ttree));
              str1[0]:=char(sizeof(ttree));
              new(oldp);
              oldp^:=p^;
              not_first:=true;
           end
         else
           not_first:=false;
{$endif extdebug}

         codegenerror:=false;
         current_module^.current_inputfile:=
           pinputfile(current_module^.sourcefiles.get_file(p^.fileinfo.fileindex));
         current_module^.current_inputfile^.line_no:=p^.fileinfo.line;
         aktswitches:=p^.pragmas;

         if not(p^.error) then
           begin
              procedures[p^.treetype](p);
              p^.error:=codegenerror;
              codegenerror:=codegenerror or oldcodegenerror;
           end
         else codegenerror:=true;
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
         aktswitches:=oldswitches;
         current_module^.current_inputfile:=oldis;
         current_module^.current_inputfile^.line_no:=oldnr;
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
  Revision 1.27  1998-06-05 00:01:06  florian
    * bugs with assigning related objects and passing objects by reference
      to a procedure

  Revision 1.26  1998/06/04 09:55:39  pierre
    * demangled name of procsym reworked to become independant of the mangling scheme

  Come test_funcret improvements (not yet working)S: ----------------------------------------------------------------------

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
