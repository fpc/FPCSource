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
        objects,cobjects,verbose,systems,globals,aasm,symtable,
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

    procedure error(const t : tmsgconst);

      begin
         if not(codegenerror) then
           verbose.Message(t);
         codegenerror:=true;
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


    { calculates the needed registers for a binary operator }
    procedure calcregisters(p : ptree;r32,fpu,mmx : word);

      begin
         p^.registers32:=max(p^.left^.registers32,p^.right^.registers32);
         p^.registersfpu:=max(p^.left^.registersfpu,p^.right^.registersfpu);
{$ifdef SUPPORT_MMX}
         p^.registersmmx:=max(p^.left^.registersmmx,p^.right^.registersmmx);
{$endif SUPPORT_MMX}

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
             var doconv : tconverttype;fromtreetype : ttreetyp) : boolean;

      { from_is_cstring mu· true sein, wenn def_from die Definition einer }
      { Stringkonstanten ist, nîtig wegen der Konvertierung von String-   }
      { konstante zu nullterminiertem String                              }

      { Hilfsliste: u8bit,s32bit,uvoid,
                    bool8bit,uchar,s8bit,s16bit,u16bit,u32bit }

      const
         basedefconverts : array[u8bit..u32bit,u8bit..u32bit] of tconverttype =
           {u8bit}
           ((tc_only_rangechecks32bit,tc_u8bit_2_s32bit,tc_not_possible,
             tc_not_possible,tc_not_possible,tc_only_rangechecks32bit,tc_u8bit_2_s16bit,
             tc_u8bit_2_u16bit,{tc_not_possible}tc_u8bit_2_u32bit),

           {s32bit}
            (tc_s32bit_2_u8bit,tc_only_rangechecks32bit,tc_not_possible,
             tc_not_possible,tc_not_possible,tc_s32bit_2_s8bit,
             tc_s32bit_2_s16bit,tc_s32bit_2_u16bit,{tc_not_possible}tc_s32bit_2_u32bit),

           {uvoid}
            (tc_not_possible,tc_not_possible,tc_not_possible,tc_not_possible,
             tc_not_possible,tc_not_possible,tc_not_possible,tc_not_possible,
             tc_not_possible),

           {bool8bit}
            (tc_not_possible,tc_not_possible,tc_not_possible,
             tc_only_rangechecks32bit,tc_not_possible,tc_not_possible,tc_not_possible,
             tc_not_possible,tc_not_possible),

           {uchar}
            (tc_not_possible,tc_not_possible,tc_not_possible,
             tc_not_possible,tc_only_rangechecks32bit,tc_not_possible,tc_not_possible,
             tc_not_possible,tc_not_possible),

           {s8bit}
            (tc_only_rangechecks32bit,tc_s8bit_2_s32bit,tc_not_possible,
             tc_not_possible,tc_not_possible,tc_only_rangechecks32bit,tc_s8bit_2_s16bit,
             tc_s8bit_2_u16bit,{tc_not_possible}tc_s8bit_2_u32bit),

           {s16bit}
            (tc_s16bit_2_u8bit,tc_s16bit_2_s32bit,tc_not_possible,
             tc_not_possible,tc_not_possible,tc_s16bit_2_s8bit,tc_only_rangechecks32bit,
             tc_only_rangechecks32bit,{tc_not_possible}tc_s8bit_2_u32bit),

           {u16bit}
            (tc_u16bit_2_u8bit,tc_u16bit_2_s32bit,tc_not_possible,
             tc_not_possible,tc_not_possible,tc_u16bit_2_s8bit,tc_only_rangechecks32bit,
             tc_only_rangechecks32bit,{tc_not_possible}tc_u16bit_2_u32bit),

           {u32bit}
            (tc_u32bit_2_u8bit,{tc_not_possible}tc_u32bit_2_s32bit,tc_not_possible,
             tc_not_possible,tc_not_possible,tc_u32bit_2_s8bit,tc_u32bit_2_s16bit,
             tc_u32bit_2_u16bit,tc_only_rangechecks32bit)
            );

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
                   if (pfloatdef(def_to)^.typ=s64bit) then
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
         else if (def_from^.deftype=objectdef) and (def_to^.deftype=objectdef) and
           pobjectdef(def_from)^.isclass and pobjectdef(def_to)^.isclass then
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
              p^.retdef:=pfuncretsym(p^.symtableentry)^.retdef;
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
                      ) then
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
         s1,s2:^string;

         { this totally forgets to set the pi_do_call flag !! }
      label
         no_overload;

      begin
         { first do the two subtrees }
         firstpass(p^.left);
         firstpass(p^.right);

         if codegenerror then
           exit;

         new(s1);
         new(s2);
         { overloaded operator ? }
         if (p^.treetype=caretn) or
            (p^.left^.resulttype^.deftype=recorddef) or
            { <> and = are defined for classes }
            ((p^.left^.resulttype^.deftype=objectdef) and
             (not(pobjectdef(p^.left^.resulttype)^.isclass) or
              not(p^.treetype in [equaln,unequaln])
             )
            ) or
            (p^.right^.resulttype^.deftype=recorddef) or
            { <> and = are defined for classes }
            ((p^.right^.resulttype^.deftype=objectdef) and
             (not(pobjectdef(p^.right^.resulttype)^.isclass) or
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
                 caretn:
                   t:=gencallnode(overloaded_operators[caret],nil);
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
              dispose(s1);
              dispose(s2);
              firstpass(t);
              putnode(p);
              p:=t;
              exit;
           end;
         no_overload:
         { compact consts }
         lt:=p^.left^.treetype;
         rt:=p^.right^.treetype;

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
              dispose(s1);
              dispose(s2);
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
              dispose(s1);
              dispose(s2);
              firstpass(p);
              exit;
           end;
         concatstrings:=false;
         if (lt=ordconstn) and (rt=ordconstn) and
           (p^.left^.resulttype^.deftype=orddef) and
           (porddef(p^.left^.resulttype)^.typ=uchar) and
           (p^.right^.resulttype^.deftype=orddef) and
           (porddef(p^.right^.resulttype)^.typ=uchar) then
           begin
              s1^:=char(byte(p^.left^.value));
              s2^:=char(byte(p^.right^.value));
              concatstrings:=true;
           end
         else if (lt=stringconstn) and (rt=ordconstn) and
           (p^.right^.resulttype^.deftype=orddef) and
           (porddef(p^.right^.resulttype)^.typ=uchar) then
           begin
              s1^:=Pstring(p^.left^.value)^;
              s2^:=char(byte(p^.right^.value));
              concatstrings:=true;
           end
         else if (lt=ordconstn) and (rt=stringconstn) and
           (p^.left^.resulttype^.deftype=orddef) and
           (porddef(p^.left^.resulttype)^.typ=uchar) then
           begin
              s1^:=char(byte(p^.left^.value));
              s2^:=pstring(p^.right^.value)^;
              concatstrings:=true;
           end
         else if (lt=stringconstn) and (rt=stringconstn) then
           begin
              s1^:=pstring(p^.left^.value)^;
              s2^:=pstring(p^.right^.value)^;
              concatstrings:=true;
           end;

         if concatstrings then
           begin
              case p^.treetype of
                 addn : t:=genstringconstnode(s1^+s2^);
                 ltn : t:=genordinalconstnode(byte(s1^<s2^),booldef);
                 lten : t:=genordinalconstnode(byte(s1^<=s2^),booldef);
                 gtn : t:=genordinalconstnode(byte(s1^>s2^),booldef);
                 gten : t:=genordinalconstnode(byte(s1^>=s2^),booldef);
                 equaln : t:=genordinalconstnode(byte(s1^=s2^),booldef);
                 unequaln : t:=genordinalconstnode(byte(s1^<>s2^),booldef);
              end;
              dispose(s1);
              dispose(s2);
              disposetree(p);
              p:=t;
              exit;
           end;
         rd:=p^.right^.resulttype;
         ld:=p^.left^.resulttype;
         dispose(s1);
         dispose(s2);

         { we can set this globally but it not allways true }
         { procinfo.flags:=procinfo.flags or pi_do_call;    }

         { if both are boolean: }
         if ((ld^.deftype=orddef) and
            (porddef(ld)^.typ=bool8bit)) and
            ((rd^.deftype=orddef) and
            (porddef(rd)^.typ=bool8bit)) then
           begin
              if (p^.treetype=andn) or (p^.treetype=orn) then
                begin
                   calcregisters(p,0,0,0);
                   p^.location.loc:=LOC_JUMP;
                end
              else if p^.treetype in [unequaln,equaln,xorn] then
                begin
                   { I'am not very content with this solution, but it's
                     a working hack    (FK)                             }
                   p^.left:=gentypeconvnode(p^.left,u8bitdef);
                   p^.right:=gentypeconvnode(p^.right,u8bitdef);
                   p^.left^.convtyp:=tc_bool_2_u8bit;
                   p^.left^.explizit:=true;
                   firstpass(p^.left);
                   p^.left^.resulttype:=booldef;
                   p^.right^.convtyp:=tc_bool_2_u8bit;
                   p^.right^.explizit:=true;
                   firstpass(p^.right);
                   p^.right^.resulttype:=booldef;
                   calcregisters(p,1,0,0);
                   { is done commonly for all data types
                   p^.location.loc:=LOC_FLAGS;
                   p^.resulttype:=booldef;
                   }
                end
              else Message(sym_e_type_mismatch);
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
                firstpass(p^.left);
                firstpass(p^.right);
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
         case p^.treetype of
            ltn,lten,gtn,gten,equaln,unequaln:
              begin
                 p^.resulttype:=booldef;
                 p^.location.loc:=LOC_FLAGS;
              end;
            addn:
              begin
                 { the result of a string addition is a string of length 255 }
                 if (p^.left^.resulttype^.deftype=stringdef) or
                    (p^.right^.resulttype^.deftype=stringdef) then
                   p^.resulttype:=cstringdef
                 else
                   p^.resulttype:=p^.left^.resulttype;
              end;
            else p^.resulttype:=p^.left^.resulttype;
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

         p^.registers32:=max(p^.left^.registers32,p^.right^.registers32);
         p^.registersfpu:=max(p^.left^.registersfpu,p^.right^.registersfpu);
{$ifdef SUPPORT_MMX}
         p^.registersmmx:=max(p^.left^.registersmmx,p^.right^.registersmmx);
{$endif SUPPORT_MMX}
         if p^.registers32<2 then p^.registers32:=2;

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
{$ifdef GDB}
         {why this !!! lost of dummy type definitions
         one per const string !!!
         p^.resulttype:=new(pstringdef,init(length(p^.values^)));}
         p^.resulttype:=cstringdef;
{$Else GDB}
         p^.resulttype:=new(pstringdef,init(length(p^.values^)));
{$endif * GDB *}
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
         and (current_module^.output_format<>of_nasm)
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

      var
         hp  : ptree;
         hp2 : pdefcoll;

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
           exit;

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
           ct,ordconstn)) and
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
                ct,ordconstn)) and
              not(is_equal(p^.right^.resulttype,
                parraydef(p^.left^.resulttype)^.rangedef)) then
                Message(sym_e_type_mismatch);
           end;
         { Never convert a boolean or a char !}
                 { maybe type conversion }
                 if (p^.right^.resulttype^.deftype<>enumdef) and
                  not ((p^.right^.resulttype^.deftype=orddef) and
                  (Porddef(p^.right^.resulttype)^.typ in [bool8bit,uchar])) then
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

      var l : longint;

           begin
                   if p^.left^.treetype=stringconstn then
                     l:=length(pstring(p^.left^.value)^)
                   else
                     l:=pstringdef(p^.left^.resulttype)^.len;
                   if l<>parraydef(p^.resulttype)^.highrange-parraydef(p^.resulttype)^.lowrange+1 then
                     Message(sym_e_type_mismatch);
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

    procedure first_bool_byte(var p : ptree);

       begin
          p^.location.loc:=LOC_REGISTER;
          { Florian I think this is overestimated
            but I still do not really understand how to get this right (PM) }
          { Hmmm, I think we need only one reg to return the result of      }
          { this node => so
          if p^.registers32<1 then
            p^.registers32:=1;
            should work (FK)
          }
          p^.registers32:=p^.left^.registers32+1;
       end;

    procedure first_proc_to_procvar(var p : ptree);

      var
         hp : ptree;
         hp2 : pdefcoll;

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
                 hp2,hp3:Pdefcoll;
                 aprocdef : pprocdef;
                 proctype : tdeftype;

    const
       firstconvert : array[tc_u8bit_2_s32bit..tc_cchar_charpointer] of
         tfirstconvproc = (first_bigger_smaller,first_nothing,first_bigger_smaller,
                           first_bigger_smaller,first_bigger_smaller,
                           first_bigger_smaller,first_bigger_smaller,
                           first_bigger_smaller,first_locmem,
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
                           first_int_real,first_real_fix,
                           first_fix_real,first_int_fix,first_real_real,
                           first_locmem,first_bool_byte,first_proc_to_procvar,
                           first_cchar_charpointer);

    begin
       aprocdef:=nil;
       { if explicite type conversation, then run firstpass }
       if p^.explizit then
         firstpass(p^.left);

       if codegenerror then
         exit;

       if not assigned(p^.left^.resulttype) then
        begin
          codegenerror:=true;
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
       if (not(isconvertable(p^.left^.resulttype,p^.resulttype,p^.convtyp,p^.left^.treetype))) then
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
                          p^.convtyp:=tc_bool_2_u8bit;
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
                               p:=hp;
                               exit;
                            end
                          else
                            begin
                               if not isconvertable(s32bitdef,p^.resulttype,p^.convtyp,ordconstn { nur Dummy} ) then
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
                                 p:=hp;
                                 exit;
                              end
                            else
                              begin
                                 if not isconvertable(p^.left^.resulttype,s32bitdef,p^.convtyp,ordconstn { nur Dummy} ) then
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
                                 disposetree(p);
                                 p:=hp;
                                 exit;
                              end
                            else
                              begin
                                 { this is wrong because it converts to a 4 byte long var !!
                                   if not isconvertable(p^.left^.resulttype,s32bitdef,p^.convtyp,ordconstn  nur Dummy ) then }
                                 if not isconvertable(p^.left^.resulttype,u8bitdef,p^.convtyp,ordconstn { nur Dummy} ) then
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
                 p:=hp;
                 exit;
              end;
            if p^.convtyp<>tc_equal then
              firstconvert[p^.convtyp](p);
         end;
    end;

    { *************** subroutine handling **************** }

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
              firstpass(p^.left);

              if codegenerror then
                begin
                   dec(parsing_para_level);
                   exit;
                end;

              p^.resulttype:=p^.left^.resulttype;
           end
         { if we know the routine which is called, then the type }
         { conversions are inserted                            }
         else
           begin
               if count_ref then
                     begin
                     store_valid:=must_be_valid;
                     if (defcoll^.paratyp<>vs_var) then
                       must_be_valid:=true
                     else
                       must_be_valid:=false;
                     { here we must add something for the implicit type }
                     { conversion from array of char to pchar }
                     if isconvertable(p^.left^.resulttype,defcoll^.data,convtyp,p^.left^.treetype) then
                       if convtyp=tc_array_to_pointer then
                         must_be_valid:=false;
                     firstpass(p^.left);
                     must_be_valid:=store_valid;
                     End;
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
                   { don't generate an type conversion for open arrays }
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
         st : psymtable;
         actprocsym : pprocsym;
         def_from,def_to,conv_to : pdef;
         pt : ptree;
         exactmatch : boolean;
         paralength,l : longint;
         pdc : pdefcoll;

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
                        (porddef(def_from)^.von>porddef(def_to)^.von) and
                        (porddef(def_from)^.bis<porddef(def_to)^.bis);
        end;


      begin
         { release registers! }
         { if procdefinition<>nil then we called firstpass already }
         { it seems to be bad because of the registers }
         { at least we can avoid the overloaded search !! }
         procs:=nil;
         { made this global for disposing !! }
         store_valid:=must_be_valid;
         if not assigned(p^.procdefinition) then
           begin
              must_be_valid:=false;
              { procedure variable ? }
              if not(assigned(p^.right)) then
                begin
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
                   actprocsym:=p^.symtableprocentry;
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
                               not(isconvertable(pt^.resulttype,procs^.nextpara^.data,hcvt,pt^.left^.treetype)) do
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
                                    hcvt,pt^.left^.treetype)) then
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
                                              ((porddef(conv_to)^.von<porddef(def_to)^.von) and
                                              (porddef(conv_to)^.bis>porddef(def_to)^.bis)) then
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
                   add_new_ref(procs^.data^.lastref);
     {$endif UseBrowser}
                   p^.procdefinition:=procs^.data;
                   p^.resulttype:=procs^.data^.retdef;
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

                   { work trough all parameters to insert the type conversions }
                   if assigned(p^.left) then
                     begin
                        old_count_ref:=count_ref;
                        count_ref:=true;
                        firstcallparan(p^.left,p^.procdefinition^.para1);
                        count_ref:=old_count_ref;
                     end;
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
                     procinfo.flags:=procinfo.flags or pi_do_call;

                   { calc the correture value for the register }
{$ifdef i386}
                   { calc the correture value for the register }
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
                end
              else
                begin
                   { procedure variable }
                   { die Typen der Parameter berechnen }

                   { procedure does a call }
                   procinfo.flags:=procinfo.flags or pi_do_call;

{$ifdef i386}
                   { calc the correture value for the register }
                   for regi:=R_EAX to R_EDI do
                     inc(reg_pushes[regi],t_times*2);
{$endif}
{$ifdef m68k}
                   { calc the correture value for the register }
                   for regi:=R_D0 to R_A6 do
                     inc(reg_pushes[regi],t_times*2);
{$endif}
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
                   { this was missing , leads to a bug below if
                     the procvar is a function }
                   p^.procdefinition:=pprocdef(p^.right^.resulttype);
                end;
         end; { not assigned(p^.procdefinition) }

         { get a register for the return value }
         if (p^.resulttype<>pdef(voiddef)) then
           begin
              { the constructor returns the result with the flags }
              if (p^.procdefinition^.options and poconstructor)<>0 then
                begin
                   { extra handling of classes }
                   { p^.methodpointer should be assigned! }
                   if assigned(p^.methodpointer) and assigned(p^.methodpointer^.resulttype) and
                     (p^.methodpointer^.resulttype^.deftype=classrefdef) then
                     begin
                        p^.location.loc:=LOC_REGISTER;
                        p^.registers32:=1;
                     end
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

         { determine the registers of the procedure variable }
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
{$ifdef GDB}
         if must_be_valid and not pprocinfo(p^.funcretprocinfo)^.funcret_is_valid then
           note(uninitialized_function_return);
         if count_ref then pprocinfo(p^.funcretprocinfo)^.funcret_is_valid:=true;
{$endif * GDB *}
{$else TEST_FUNCRET}
         p^.resulttype:=procinfo.retdef;
         p^.location.loc:=LOC_REFERENCE;
         if ret_in_param(procinfo.retdef) then
           p^.registers32:=1;
{$ifdef GDB}
         if must_be_valid and
           not(procinfo.funcret_is_valid) {and
           ((procinfo.flags and pi_uses_asm)=0)} then
           Message(sym_w_function_result_not_set);
         if count_ref then procinfo.funcret_is_valid:=true;
{$endif * GDB *}
{$endif TEST_FUNCRET}
          end;


    { intern inline suborutines }
    procedure firstinline(var p : ptree);

      var
         hp,hpp : ptree;
         isreal,store_valid,file_is_typed : boolean;
         convtyp : tconverttype;

      procedure do_lowhigh(adef : pdef);

        var
           v : longint;
           enum : penumsym;

        begin
           case Adef^.deftype of
             orddef:
               begin
                  if p^.inlinenumber=in_low_x then
                    v:=porddef(Adef)^.von
                  else
                    v:=porddef(Adef)^.bis;
                  hp:=genordinalconstnode(v,adef);
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
         { if we handle writeln; p^.left contains no valid address }
         if assigned(p^.left) then
           begin
              p^.registers32:=p^.left^.registers32;
              p^.registersfpu:=p^.left^.registersfpu;
{$ifdef SUPPORT_MMX}
              p^.registersmmx:=p^.left^.registersmmx;
{$endif SUPPORT_MMX}
              set_location(p^.location,p^.left^.location);
           end;
           store_valid:=must_be_valid;
           if not (p^.inlinenumber in [in_read_x,in_readln_x,in_sizeof_x,
                                       in_typeof_x,in_ord_x,
                                       in_reset_typedfile,in_rewrite_typedfile]) then
             must_be_valid:=true
             else must_be_valid:=false;
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
                         if (porddef(p^.left^.resulttype)^.typ=uchar) or
                            (porddef(p^.left^.resulttype)^.typ=bool8bit) then
                           begin
                              if porddef(p^.left^.resulttype)^.typ=bool8bit then
                                begin
                                   hp:=gentypeconvnode(p^.left,u8bitdef);
                                   putnode(p);
                                   p:=hp;
                                   p^.convtyp:=tc_bool_2_u8bit;
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
                  p^.resulttype:=u8bitdef;
                  { String nach Stringkonvertierungen brauchen wir hier nicht }
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
                      exit;
                    end;
                       if p^.left^.treetype=ordconstn then
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
                       { nochmals alle Parameter bearbeiten }
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
                       p^.left^.right:=hp;
                       must_be_valid:=true;
                       firstcallparan(p^.left,nil);
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
       end;

    procedure firstsubscriptn(var p : ptree);

      begin
         firstpass(p^.left);

         if codegenerror then
           exit;

         p^.resulttype:=p^.vs^.definition;
         if count_ref and not must_be_valid then
           if (p^.vs^.properties and sp_protected)<>0 then
             Message(parser_e_cant_write_protected_member);
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

         p^.registers32:=max(p^.left^.registers32,p^.right^.registers32);
         p^.registersfpu:=max(p^.left^.registersfpu,p^.right^.registersfpu);
{$ifdef SUPPORT_MMX}
         p^.registersmmx:=max(p^.left^.registersmmx,p^.right^.registersmmx);
{$endif SUPPORT_MMX}
         { this is not allways true due to optimization }
         { but if we don't set this we get problems with optimizing self code }
         if psetdef(p^.right^.resulttype)^.settype<>smallset then
           procinfo.flags:=procinfo.flags or pi_do_call;
      end;

    { !!!!!!!!!!!! unused }
    procedure firstexpr(var p : ptree);

      begin
         firstpass(p^.left);
         if codegenerror then
           exit;
         p^.registers32:=p^.left^.registers32;
         p^.registersfpu:=p^.left^.registersfpu;
{$ifdef SUPPORT_MMX}
         p^.registersmmx:=p^.left^.registersmmx;
{$endif SUPPORT_MMX}
         if (cs_extsyntax in aktswitches) and (p^.left^.resulttype<>pdef(voiddef)) then
           Message(cg_e_illegal_expression);
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
                                                        current_module^.current_inputfile:=hp^.left^.inputfile;
                                                        current_module^.current_inputfile^.line_no:=hp^.left^.line;

                                                        disposetree(hp^.left);
                            hp^.left:=nil;
                            Message(cg_w_unreachable_code);

                            { old lines }
                            current_module^.current_inputfile:=hp^.right^.inputfile;
                            current_module^.current_inputfile^.line_no:=hp^.right^.line;
                         end;
                end;
              if assigned(hp^.right) then
                begin
                   cleartempgen;
                   firstpass(hp^.right);
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

         p^.registersfpu:=max(p^.left^.registersfpu,p^.right^.registersfpu);
         p^.registers32:=max(p^.left^.registers32,p^.right^.registers32);
{$ifdef SUPPORT_MMX}
         p^.registersmmx:=max(p^.left^.registersmmx,p^.right^.registersmmx);
{$endif SUPPORT_MMX}

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

         p^.registersfpu:=max(p^.left^.registersfpu,p^.left^.registersfpu);
         p^.registers32:=max(p^.left^.registers32,p^.right^.registers32);
{$ifdef SUPPORT_MMX}
         p^.registersmmx:=max(p^.left^.registersmmx,p^.right^.registersmmx);
{$endif SUPPORT_MMX}

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
                   p^.registersfpu:=max(p^.left^.registersfpu,
                     p^.right^.registersfpu);
                   p^.registers32:=max(p^.left^.registers32,
                     p^.right^.registers32);
{$ifdef SUPPORT_MMX}
                   p^.registersmmx:=max(p^.left^.registersmmx,
                     p^.right^.registersmmx);
{$endif SUPPORT_MMX}
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

               p^.registers32:=max(p^.left^.registers32,
                 p^.right^.registers32);
               p^.registersfpu:=max(p^.left^.registersfpu,
                 p^.right^.registersfpu);
{$ifdef SUPPORT_MMX}
               p^.registersmmx:=max(p^.left^.registersmmx,
                 p^.right^.registersmmx);
{$endif SUPPORT_MMX}
               p^.resulttype:=voiddef;
            end
         else
           begin
              { optimization }
              disposetree(p);
              p:=nil;
           end;
      end;

{    procedure firstprocinline(var p : ptree);
      var old_inline_proc_firsttemp : longint;

      begin
         old_inline_proc_firsttemp:=procinfo.firsttemp;
         procinfo.firsttemp:=procinfo.firsttemp+p^.inlineproc^.definition^.localst^.datasize;
      end; }

    type
       firstpassproc = procedure(var p : ptree);

    procedure firstpass(var p : ptree);

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
             firstnothing,firstnothing,firstif,firstnothing,
             firstnothing,first_while_repeat,first_while_repeat,firstfor,
             firstexitn,firstwith,firstcase,firstlabel,
             firstgoto,firstsimplenewdispose,firsttryexcept,firstraise,
             firstnothing,firsttryfinally,firstis,firstas,firstadd,
             firstnothing,firstnothing,firstloadvmt);

      var
         oldcodegenerror : boolean;
         oldswitches : Tcswitches;
         { there some calls of do_firstpass in the parser }
         oldis : pinputfile;
         oldnr : longint;

      begin
         { if we save there the whole stuff, }
         { line numbers become more correct  }
         oldis:=current_module^.current_inputfile;
         oldnr:=current_module^.current_inputfile^.line_no;
         oldcodegenerror:=codegenerror;
         oldswitches:=aktswitches;
{$ifdef extdebug}
        inc(p^.firstpasscount);
{$endif extdebug}

         codegenerror:=false;
         current_module^.current_inputfile:=p^.inputfile;
         current_module^.current_inputfile^.line_no:=p^.line;
         aktswitches:=p^.pragmas;

         if not(p^.error) then
           begin
              procedures[p^.treetype](p);
              p^.error:=codegenerror;
              codegenerror:=codegenerror or oldcodegenerror;
           end
         else codegenerror:=true;
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

end.
{
  $Log$
  Revision 1.5  1998-04-08 16:58:04  pierre
    * several bugfixes
      ADD ADC and AND are also sign extended
      nasm output OK (program still crashes at end
      and creates wrong assembler files !!)
      procsym types sym in tdef removed !!

  Revision 1.4  1998/04/07 22:45:04  florian
    * bug0092, bug0115 and bug0121 fixed
    + packed object/class/array

  Revision 1.3  1998/03/28 23:09:56  florian
    * secondin bugfix (m68k and i386)
    * overflow checking bugfix (m68k and i386) -- pretty useless in
      secondadd, since everything is done using 32-bit
    * loading pointer to routines hopefully fixed (m68k)
    * flags problem with calls to RTL internal routines fixed (still strcmp
      to fix) (m68k)
    * #ELSE was still incorrect (didn't take care of the previous level)
    * problem with filenames in the command line solved
    * problem with mangledname solved
    * linking name problem solved (was case insensitive)
    * double id problem and potential crash solved
    * stop after first error
    * and=>test problem removed
    * correct read for all float types
    * 2 sigsegv fixes and a cosmetic fix for Internal Error
    * push/pop is now correct optimized (=> mov (%esp),reg)

  Revision 1.2  1998/03/26 11:18:31  florian
    - switch -Sa removed
    - support of a:=b:=0 removed

  Revision 1.1.1.1  1998/03/25 11:18:14  root
  * Restored version

  Revision 1.41  1998/03/13 22:45:59  florian
    * small bug fixes applied

  Revision 1.40  1998/03/10 23:48:36  florian
    * a couple of bug fixes to get the compiler with -OGaxz compiler, sadly
      enough, it doesn't run

  Revision 1.39  1998/03/10 16:27:41  pierre
    * better line info in stabs debug
    * symtabletype and lexlevel separated into two fields of tsymtable
    + ifdef MAKELIB for direct library output, not complete
    + ifdef CHAINPROCSYMS for overloaded seach across units, not fully
      working
    + ifdef TESTFUNCRET for setting func result in underfunction, not
      working

  Revision 1.38  1998/03/10 01:11:11  peter
    * removed one of my previous optimizations with string+char, which
      generated wrong code

  Revision 1.37  1998/03/09 10:44:38  peter
    + string='', string<>'', string:='', string:=char optimizes (the first 2
      were already in cg68k2)

  Revision 1.36  1998/03/06 00:52:38  peter
    * replaced all old messages from errore.msg, only ExtDebug and some
      Comment() calls are left
    * fixed options.pas

  Revision 1.35  1998/03/04 08:38:19  florian
    * problem with unary minus fixed

  Revision 1.34  1998/03/03 01:08:31  florian
    * bug0105 and bug0106 problem solved

  Revision 1.33  1998/03/02 01:48:56  peter
    * renamed target_DOS to target_GO32V1
    + new verbose system, merged old errors and verbose units into one new
      verbose.pas, so errors.pas is obsolete

  Revision 1.32  1998/03/01 22:46:14  florian
    + some win95 linking stuff
    * a couple of bugs fixed:
      bug0055,bug0058,bug0059,bug0064,bug0072,bug0093,bug0095,bug0098

  Revision 1.31  1998/02/28 17:26:46  carl
    * bugfix #47 and more checking for aprocdef

  Revision 1.30  1998/02/13 10:35:20  daniel
  * Made Motorola version compilable.
  * Fixed optimizer

  Revision 1.29  1998/02/12 17:19:16  florian
    * fixed to get remake3 work, but needs additional fixes (output, I don't like
      also that aktswitches isn't a pointer)

  Revision 1.28  1998/02/12 11:50:23  daniel
  Yes! Finally! After three retries, my patch!

  Changes:

  Complete rewrite of psub.pas.
  Added support for DLL's.
  Compiler requires less memory.
  Platform units for each platform.

  Revision 1.27  1998/02/11 21:56:34  florian
    * bugfixes: bug0093, bug0053, bug0088, bug0087, bug0089

  Revision 1.26  1998/02/07 23:05:03  florian
    * once more MMX

  Revision 1.25  1998/02/07 09:39:24  florian
    * correct handling of in_main
    + $D,$T,$X,$V like tp

  Revision 1.24  1998/02/06 10:34:21  florian
    * bug0082 and bug0084 fixed

  Revision 1.23  1998/02/05 21:54:34  florian
    + more MMX

  Revision 1.22  1998/02/05 20:54:30  peter
    * fixed a Sigsegv

  Revision 1.21  1998/02/04 23:04:21  florian
    + unary minus for mmx data types added

  Revision 1.20  1998/02/04 22:00:56  florian
    + NOT operator for mmx arrays

  Revision 1.19  1998/02/04 14:38:49  florian
    * clean up
    * a lot of potential bugs removed adding some neccessary register allocations
      (FPU!)
    + allocation of MMX registers

  Revision 1.18  1998/02/03 23:07:34  florian
    * AS and IS do now a correct type checking
    + is_convertable handles now also instances of classes

  Revision 1.17  1998/02/01 19:40:51  florian
    * clean up
    * bug0029 fixed

  Revision 1.16  1998/02/01 17:14:04  florian
    + comparsion of class references

  Revision 1.15  1998/01/30 21:23:59  carl
    * bugfix of compiler crash with new/dispose (fourth crash of new bug)
    * bugfix of write/read compiler crash

  Revision 1.14  1998/01/25 22:29:00  florian
    * a lot bug fixes on the DOM

  Revision 1.13  1998/01/21 22:34:25  florian
    + comparsion of Delphi classes

  Revision 1.12  1998/01/21 21:29:55  florian
    * some fixes for Delphi classes

  Revision 1.11  1998/01/16 23:34:13  florian
    + nil is compatible with class variable (tobject(x):=nil)

  Revision 1.10  1998/01/16 22:34:40  michael
  * Changed 'conversation' to 'conversion'. Waayyy too much chatting going on
    in this compiler :)

  Revision 1.9  1998/01/13 23:11:10  florian
    + class methods

  Revision 1.8  1998/01/07 00:17:01  michael
  Restored released version (plus fixes) as current

  Revision 1.7  1997/12/10 23:07:26  florian
  * bugs fixed: 12,38 (also m68k),39,40,41
  + warning if a system unit is without -Us compiled
  + warning if a method is virtual and private (was an error)
  * some indentions changed
  + factor does a better error recovering (omit some crashes)
  + problem with @type(x) removed (crashed the compiler)

  Revision 1.6  1997/12/09 13:54:26  carl
  + renamed some stuff (real types mostly)

  Revision 1.5  1997/12/04 12:02:19  pierre
     + added a counter of max firstpass's for a ptree
       for debugging only in ifdef extdebug

  Revision 1.4  1997/12/03 13:53:01  carl
  + ifdef i386.

  Revision 1.3  1997/11/29 15:38:43  florian
  * bug0033 fixed
  * duplicate strings are now really once generated (there was a bug)

  Revision 1.2  1997/11/28 11:11:43  pierre
     negativ real constants are not supported by nasm assembler

  Revision 1.1.1.1  1997/11/27 08:32:59  michael
  FPC Compiler CVS start


  Pre-CVS log:

    CEC    Carl-Eric Codere
    FK     Florian Klaempfl
    PM     Pierre Muller
    +      feature added
    -      removed
    *      bug fixed or changed

  History:
       6th september 1997:
         + added basic support for MC68000   (CEC)
            (lines: 189,1860,1884 + ifdef m68k)
      19th september 1997:
         + added evalution of constant sets  (FK)
         + empty and constant sets are now compatible with all other
           set types (FK)
      20th september 1997:
         * p^.register32 bug in firstcalln (max with register32 of p^.left i.e. args) (PM)
      24th september 1997:
         * line_no and inputfile are now in firstpass saved (FK)
      25th september 1997:
         + support of high for open arrays (FK)
         + the high parameter is now pushed for open arrays (FK)
      1th october 1997:
         + added support for unary minus operator and for:=overloading (PM)
      2nd october 1997:
         + added handling of in_ord_x (PM)
           boolean to byte with ord is special because the location may be different
      3rd october 1997:
         + renamed ret_in_eax to ret_in_acc (CEC)
         + find ifdef m68k to find other changes (CEC)
         * bugfix or calc correct val for regs. for m68k in firstcalln (CEC)
      4th october 1997:
         + added code for in_pred_x in_succ_x
           fails for enums with jumps (PM)
     25th october 1997:
         + direct evalution of pred and succ with const parameter (FK)
      6th november 1997:
         * added typeconversion for floatdef in write(ln) for text to s64real (PM)
         + code for str with length arg rewritten (PM)
      13th november 1997:
         * floatdef in write(ln) for text for different types in RTL (PM)
         * bug causing convertability from floatdef to orddef removed (PM)
         * typecasting from voiddef to any type not allowed anymore (PM)
         + handling of different real const to diff realtype (PM)
      18th november 1997:
         * changed first_type_conv function arg as var p : ptree
           to be able to change the tree (PM)
}
