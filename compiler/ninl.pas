{
    $Id$
    Copyright (c) 1998-2000 by Florian Klaempfl

    Type checking and register allocation for inline nodes

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
unit ninl;

{$i defines.inc}

interface

    uses
       node,htypechk,cpuinfo;

    {$i compinnr.inc}

    type
       tinlinenode = class(tunarynode)
          inlinenumber : byte;
          constructor create(number : byte;is_const:boolean;l : tnode);virtual;
          function getcopy : tnode;override;
          function pass_1 : tnode;override;
          function det_resulttype:tnode;override;
          function docompare(p: tnode): boolean; override;
       end;

    var
       cinlinenode : class of tinlinenode;

   function geninlinenode(number : byte;is_const:boolean;l : tnode) : tinlinenode;

implementation

    uses
      verbose,globals,systems,
      globtype,
      symconst,symtype,symdef,symsym,symtable,types,
      pass_1,
      ncal,ncon,ncnv,nadd,nld,nbas,
      cpubase,hcodegen,tgcpu
{$ifdef newcg}
      ,cgbase
{$endif newcg}
      ;

   function geninlinenode(number : byte;is_const:boolean;l : tnode) : tinlinenode;

     begin
        geninlinenode:=cinlinenode.create(number,is_const,l);
     end;

{*****************************************************************************
                           TINLINENODE
*****************************************************************************}

    constructor tinlinenode.create(number : byte;is_const:boolean;l : tnode);

      begin
         inherited create(inlinen,l);
         if is_const then
           include(flags,nf_inlineconst);
         inlinenumber:=number;
      end;


    function tinlinenode.getcopy : tnode;
      var
         n : tinlinenode;
      begin
         n:=tinlinenode(inherited getcopy);
         n.inlinenumber:=inlinenumber;
         result:=n;
      end;


    function tinlinenode.det_resulttype:tnode;
      var
         counter   : longint;
         ppn       : tcallparanode;
         dummycoll : tparaitem;
         isreal    : boolean;
      begin
         result:=nil;
         { if we handle writeln; left contains no valid address }
         if assigned(left) then
           begin
             if left.nodetype=callparan then
               tcallparanode(left).get_paratype
             else
               resulttypepass(left);
           end;
         inc(parsing_para_level);

         { handle intern constant functions in separate case }
         if nf_inlineconst in flags then
          begin
            isreal:=(left.nodetype=realconstn);
            case inlinenumber of
              in_const_pi,
              in_const_int,
              in_const_sqrt,
              in_const_arctan,
              in_const_cos,
              in_const_sin,
              in_const_exp,
              in_const_ln :
                resulttype:=pbestrealtype^;
              in_const_frac,
              in_const_round,
              in_const_trunc :
                resulttype:=s32bittype;
              in_const_abs,
              in_const_sqr :
                begin
                  if isreal then
                   resulttype:=pbestrealtype^
                  else
                   resulttype:=s32bittype;
                end;
              in_const_odd :
                resulttype:=booltype;
              in_const_swap_word,
              in_const_swap_long :
                resulttype:=left.resulttype;
              in_const_ptr :
                resulttype:=voidfarpointertype;
              else
                internalerror(881);
            end;
          end
         else
          begin
            case inlinenumber of
              in_lo_long,
              in_hi_long :
                begin
                  set_varstate(left,true);
                  resulttype:=u16bittype;
                  if (m_tp in aktmodeswitches) or
                     (m_delphi in aktmodeswitches) then
                    CGMessage(type_w_maybe_wrong_hi_lo);
                  if not is_integer(left.resulttype.def) then
                    CGMessage(type_e_mismatch)
                end;

              in_lo_qword,
              in_hi_qword :
                begin
                  set_varstate(left,true);
                  resulttype:=u32bittype;
                  if (m_tp in aktmodeswitches) or
                     (m_delphi in aktmodeswitches) then
                    CGMessage(type_w_maybe_wrong_hi_lo);
                  if not is_integer(left.resulttype.def) then
                    CGMessage(type_e_mismatch);
                end;

              in_lo_word,
              in_hi_word :
                begin
                  set_varstate(left,true);
                  resulttype:=u8bittype;
                  if not is_integer(left.resulttype.def) then
                    CGMessage(type_e_mismatch);
                end;

              in_sizeof_x:
                begin
                  set_varstate(left,false);
                  resulttype:=s32bittype;
                end;

              in_typeof_x:
                begin
                  set_varstate(left,false);
                  resulttype:=voidpointertype;
                end;

              in_ord_x:
                begin
                   set_varstate(left,true);
                   case left.resulttype.def^.deftype of
                     orddef :
                       begin
                         case porddef(left.resulttype.def)^.typ of
                           bool8bit,
                           uchar:
                             resulttype:=u8bittype;
                           bool16bit,
                           uwidechar :
                             resulttype:=u16bittype;
                           uvoid :
                             CGMessage(type_e_mismatch)
                           else
                             resulttype:=left.resulttype;
                         end;
                       end;
                     enumdef :
                       resulttype:=s32bittype;
                     else
                       CGMessage(type_e_mismatch);
                   end;
                end;

              in_chr_byte:
                begin
                   set_varstate(left,true);
                   resulttype:=cchartype;
                end;

              in_length_string:
                begin
                   set_varstate(left,true);
                   if is_ansistring(left.resulttype.def) or
                      is_widestring(left.resulttype.def) or
                      is_dynamic_array(left.resulttype.def) then
                     resulttype:=s32bittype
                   else
                     resulttype:=u8bittype;

                   { check the type, must be string or char }
                   if (left.resulttype.def^.deftype<>stringdef) and
                      (not is_char(left.resulttype.def)) then
                     CGMessage(type_e_mismatch);
                end;

              in_typeinfo_x:
                begin
                   set_varstate(left,true);
                   resulttype:=voidpointertype;
                end;

              in_assigned_x:
                begin
                   set_varstate(left,true);
                   resulttype:=booltype;
                end;

              in_ofs_x :
                internalerror(2000101001);

              in_seg_x :
                begin
                  set_varstate(left,false);
                  resulttype:=s32bittype;
                end;

              in_pred_x,
              in_succ_x:
                begin
                   set_varstate(left,true);
                   resulttype:=left.resulttype;
                   if not is_ordinal(resulttype.def) then
                     CGMessage(type_e_ordinal_expr_expected)
                   else
                     begin
                       if (resulttype.def^.deftype=enumdef) and
                          (penumdef(resulttype.def)^.has_jumps) then
                         CGMessage(type_e_succ_and_pred_enums_with_assign_not_possible);
                     end;
                end;

              in_setlength_x:
                begin
                   resulttype:=voidtype;
                   if assigned(left) then
                     begin
                        ppn:=tcallparanode(left);
                        counter:=0;
                        { check type }
                        while assigned(ppn.right) do
                          begin
                             inserttypeconv(ppn.left,s32bittype);
                             inc(counter);
                             ppn:=tcallparanode(ppn.right);
                          end;
                        { last param must be var }
                        valid_for_assign(ppn.left,false);
                        set_varstate(ppn.left,false);
                        { first param must be a string or dynamic array ...}
                        if not((ppn.left.resulttype.def^.deftype=stringdef) or
                           (is_dynamic_array(ppn.left.resulttype.def))) then
                          CGMessage(type_e_mismatch);

                        { only dynamic arrays accept more dimensions }
                        if (counter>1) and
                          (not(is_dynamic_array(left.resulttype.def))) then
                          CGMessage(type_e_mismatch);

                       { convert shortstrings to openstring parameters }
                       { (generate the hightree) (JM)                  }
                       if (ppn.left.resulttype.def^.deftype = stringdef) and
                          (pstringdef(ppn.left.resulttype.def)^.string_typ =
                            st_shortstring) then
                         begin
                           dummycoll:=tparaitem.create;
                           dummycoll.paratyp:=vs_var;
                           dummycoll.paratype:=openshortstringtype;
                           tcallparanode(ppn).insert_typeconv(dummycoll,false);
                           dummycoll.destroy;
                         end;
                     end
                   else
                     CGMessage(type_e_mismatch);
                end;

              in_finalize_x:
                begin
                   resulttype:=voidtype;
                   if assigned(left) and assigned(tcallparanode(left).left) then
                     begin
                        { first param must be var }
                        valid_for_assign(tcallparanode(left).left,false);
                        set_varstate(tcallparanode(left).left,true);

                        { two parameters?, the last parameter must be a longint }
                        if assigned(tcallparanode(left).right) then
                         inserttypeconv(tcallparanode(tcallparanode(left).right).left,s32bittype);
                     end
                   else
                     CGMessage(type_e_mismatch);
                end;

              in_inc_x,
              in_dec_x:
                begin
                  resulttype:=voidtype;
                  if assigned(left) then
                    begin
                       set_varstate(left,true);
                       if codegenerror then
                        exit;
                       { first param must be var }
                       valid_for_assign(tcallparanode(left).left,false);

                       if (left.resulttype.def^.deftype in [enumdef,pointerdef]) or
                          is_ordinal(left.resulttype.def) then
                        begin
                          { two paras ? }
                          if assigned(tcallparanode(left).right) then
                           begin
                             { insert a type conversion       }
                             { the second param is always longint }
                             inserttypeconv(tcallparanode(tcallparanode(left).right).left,s32bittype);

                             if assigned(tcallparanode(tcallparanode(left).right).right) then
                              CGMessage(cg_e_illegal_expression);
                           end;
                        end
                       else
                        CGMessage(type_e_ordinal_expr_expected);
                    end
                  else
                    CGMessage(type_e_mismatch);
                end;

              in_read_x,
              in_readln_x,
              in_write_x,
              in_writeln_x :
                begin
                  resulttype:=voidtype;
                end;

              in_settextbuf_file_x :
                begin
                  resulttype:=voidtype;
                end;

              { the firstpass of the arg has been done in firstcalln ? }
              in_reset_typedfile,
              in_rewrite_typedfile :
                begin
                  set_varstate(left,true);
                  resulttype:=voidtype;
                end;

              in_str_x_string :
                begin
                  resulttype:=voidtype;
                  set_varstate(left,false);
                end;

              in_val_x :
                begin
                  resulttype:=voidtype;
                end;

              in_include_x_y,
              in_exclude_x_y:
                begin
                  resulttype:=voidtype;
                  if assigned(left) then
                    begin
                       set_varstate(left,true);
                       { remove warning when result is passed }
                       set_funcret_is_valid(tcallparanode(left).left);
                       { first param must be var }
                       valid_for_assign(tcallparanode(left).left,false);
                       { check type }
                       if assigned(left.resulttype.def) and
                          (left.resulttype.def^.deftype=setdef) then
                         begin
                            { two paras ? }
                            if assigned(tcallparanode(left).right) then
                              begin
                                 { insert a type conversion       }
                                 { to the type of the set elements  }
                                 inserttypeconv(tcallparanode(tcallparanode(left).right).left,
                                   psetdef(left.resulttype.def)^.elementtype);
                                 { only three parameters are allowed }
                                 if assigned(tcallparanode(tcallparanode(left).right).right) then
                                   CGMessage(cg_e_illegal_expression);
                              end;
                         end
                       else
                         CGMessage(type_e_mismatch);
                    end
                  else
                    CGMessage(type_e_mismatch);
                end;

              in_low_x,
              in_high_x:
                begin
                  set_varstate(left,false);
                  case left.resulttype.def^.deftype of
                    orddef,enumdef:
                      resulttype:=left.resulttype;
                    setdef:
                      resulttype:=psetdef(left.resulttype.def)^.elementtype;
                    arraydef:
                      resulttype:=parraydef(left.resulttype.def)^.rangetype;
                    stringdef:
                      resulttype:=u8bittype;
                    else
                      CGMessage(type_e_mismatch);
                  end;
                end;

              in_pi :
                resulttype:=s80floattype;

              in_cos_extended,
              in_sin_extended,
              in_arctan_extended,
              in_abs_extended,
              in_sqr_extended,
              in_sqrt_extended,
              in_ln_extended:
                begin
                  resulttype:=s80floattype;
                  set_varstate(left,true);
                  if (left.resulttype.def^.deftype<>floatdef) or
                     (pfloatdef(left.resulttype.def)^.typ<>s80real) then
                    inserttypeconv(left,s80floattype);
                end;

 {$ifdef SUPPORT_MMX}
              in_mmx_pcmpeqb..in_mmx_pcmpgtw:
                begin
                end;
 {$endif SUPPORT_MMX}

              in_assert_x_y :
                begin
                  resulttype:=voidtype;
                  if assigned(left) then
                    begin
                       set_varstate(left,true);
                       { check type }
                       if is_boolean(left.resulttype.def) then
                         begin
                            { must always be a string }
                            inserttypeconv(tcallparanode(tcallparanode(left).right).left,cshortstringtype);
                         end
                       else
                         CGMessage(type_e_mismatch);
                    end
                  else
                    CGMessage(type_e_mismatch);
                end;

               else
                internalerror(8);
            end;
            dec(parsing_para_level);
          end;
      end;


{$ifdef fpc}
{$maxfpuregisters 0}
{$endif fpc}
    function tinlinenode.pass_1 : tnode;
      var
         vl,vl2 : longint;
         vr      : bestreal;
         p1,hp,hpp  :  tnode;
         srsym : psym;
{$ifndef NOCOLONCHECK}
         frac_para,length_para : tnode;
{$endif ndef NOCOLONCHECK}
         extra_register,
         isreal,
         iswrite,
         file_is_typed : boolean;

      function do_lowhigh(const t:ttype) : tnode;

        var
           v : tconstexprint;
           enum : penumsym;

        begin
           case t.def^.deftype of
             orddef:
               begin
                  if inlinenumber=in_low_x then
                    v:=porddef(t.def)^.low
                  else
                    v:=porddef(t.def)^.high;

                  { low/high of torddef are longints, so we need special }
                  { handling for cardinal and 64bit types (JM)           }
                  if is_signed(t.def) and
                     is_64bitint(t.def) then
                    if (inlinenumber=in_low_x) then
                      v := int64($80000000) shl 32
                    else
                      v := (int64($7fffffff) shl 32) or $ffffffff
                  else
                    if is_64bitint(t.def) then
                      { we have to use a dirty trick for high(qword),     }
                      { because it's bigger than high(tconstexprint) (JM) }
                      v := 0
                    else
                      if not is_signed(t.def) then
                        v := cardinal(v);
                  hp:=cordconstnode.create(v,t);
                  firstpass(hp);
                  { fix high(qword) }
                  if not is_signed(t.def) and
                     is_64bitint(t.def) and
                     (inlinenumber = in_high_x) then
                    tordconstnode(hp).value :=
                      tconstexprint(qword($ffffffff) shl 32 or $ffffffff);
                  do_lowhigh:=hp;
               end;
             enumdef:
               begin
                  enum:=penumsym(Penumdef(t.def)^.firstenum);
                  v:=Penumdef(t.def)^.maxval;
                  if inlinenumber=in_high_x then
                    while assigned(enum) and (enum^.value <> v) do
                      enum:=enum^.nextenum;
                  if not assigned(enum) then
                    internalerror(309993)
                  else
                    hp:=genenumnode(enum);
                  do_lowhigh:=hp;
               end;
           else
             internalerror(87);
           end;
        end;

      function getconstrealvalue : bestreal;

        begin
           case left.nodetype of
              ordconstn:
                getconstrealvalue:=tordconstnode(left).value;
              realconstn:
                getconstrealvalue:=trealconstnode(left).value_real;
              else
                internalerror(309992);
           end;
        end;

      procedure setconstrealvalue(r : bestreal);

        var
           hp : tnode;

        begin
           hp:=crealconstnode.create(r,pbestrealtype^);
           firstpass(hp);
           result:=hp;
        end;

      begin
         result:=nil;
         { if we handle writeln; left contains no valid address }
         if assigned(left) then
           begin
              if left.nodetype=callparan then
                tcallparanode(left).firstcallparan(nil,false)
              else
                firstpass(left);
              left_max;
              set_location(location,left.location);
           end;
         inc(parsing_para_level);
         { handle intern constant functions in separate case }
         if nf_inlineconst in flags then
          begin
            hp:=nil;
            { no parameters? }
            if not assigned(left) then
             begin
               case inlinenumber of
                 in_const_pi :
                   hp:=crealconstnode.create(pi,pbestrealtype^);
                 else
                   internalerror(89);
               end;
             end
            else
            { process constant expression with parameter }
             begin
               vl:=0;
               vl2:=0; { second parameter Ex: ptr(vl,vl2) }
               vr:=0;
               isreal:=false;
               case left.nodetype of
                 realconstn :
                   begin
                     isreal:=true;
                     vr:=trealconstnode(left).value_real;
                   end;
                 ordconstn :
                   vl:=tordconstnode(left).value;
                 callparan :
                   begin
                     { both exists, else it was not generated }
                     vl:=tordconstnode(tcallparanode(left).left).value;
                     vl2:=tordconstnode(tcallparanode(tcallparanode(left).right).left).value;
                   end;
                 else
                   CGMessage(cg_e_illegal_expression);
               end;
               case inlinenumber of
                 in_const_trunc :
                   begin
                     if isreal then
                       begin
                          if (vr>=2147483648.0) or (vr<=-2147483649.0) then
                            begin
                               CGMessage(parser_e_range_check_error);
                               hp:=cordconstnode.create(1,s32bittype)
                            end
                          else
                            hp:=cordconstnode.create(trunc(vr),s32bittype)
                       end
                     else
                      hp:=cordconstnode.create(trunc(vl),s32bittype);
                   end;
                 in_const_round :
                   begin
                     if isreal then
                       begin
                          if (vr>=2147483647.5) or (vr<=-2147483648.5) then
                            begin
                               CGMessage(parser_e_range_check_error);
                               hp:=cordconstnode.create(1,s32bittype)
                            end
                          else
                            hp:=cordconstnode.create(round(vr),s32bittype)
                       end
                     else
                      hp:=cordconstnode.create(round(vl),s32bittype);
                   end;
                 in_const_frac :
                   begin
                     if isreal then
                      hp:=crealconstnode.create(frac(vr),pbestrealtype^)
                     else
                      hp:=crealconstnode.create(frac(vl),pbestrealtype^);
                   end;
                 in_const_int :
                   begin
                     if isreal then
                      hp:=crealconstnode.create(int(vr),pbestrealtype^)
                     else
                      hp:=crealconstnode.create(int(vl),pbestrealtype^);
                   end;
                 in_const_abs :
                   begin
                     if isreal then
                      hp:=crealconstnode.create(abs(vr),pbestrealtype^)
                     else
                      hp:=cordconstnode.create(abs(vl),left.resulttype);
                   end;
                 in_const_sqr :
                   begin
                     if isreal then
                      hp:=crealconstnode.create(sqr(vr),pbestrealtype^)
                     else
                      hp:=cordconstnode.create(sqr(vl),left.resulttype);
                   end;
                 in_const_odd :
                   begin
                     if isreal then
                      CGMessage1(type_e_integer_expr_expected,left.resulttype.def^.typename)
                     else
                      hp:=cordconstnode.create(byte(odd(vl)),booltype);
                   end;
                 in_const_swap_word :
                   begin
                     if isreal then
                      CGMessage1(type_e_integer_expr_expected,left.resulttype.def^.typename)
                     else
                      hp:=cordconstnode.create((vl and $ff) shl 8+(vl shr 8),left.resulttype);
                   end;
                 in_const_swap_long :
                   begin
                     if isreal then
                      CGMessage(type_e_mismatch)
                     else
                      hp:=cordconstnode.create((vl and $ffff) shl 16+(vl shr 16),left.resulttype);
                   end;
                 in_const_ptr :
                   begin
                     if isreal then
                      CGMessage(type_e_mismatch)
                     else
                      hp:=cordconstnode.create((vl2 shl 4)+vl,voidfarpointertype);
                   end;
                 in_const_sqrt :
                   begin
                     if isreal then
                       begin
                          if vr<0.0 then
                           CGMessage(type_e_wrong_math_argument)
                          else
                           hp:=crealconstnode.create(sqrt(vr),pbestrealtype^)
                       end
                     else
                       begin
                          if vl<0 then
                           CGMessage(type_e_wrong_math_argument)
                          else
                           hp:=crealconstnode.create(sqrt(vl),pbestrealtype^);
                       end;
                   end;
                 in_const_arctan :
                   begin
                     if isreal then
                      hp:=crealconstnode.create(arctan(vr),pbestrealtype^)
                     else
                      hp:=crealconstnode.create(arctan(vl),pbestrealtype^);
                   end;
                 in_const_cos :
                   begin
                     if isreal then
                      hp:=crealconstnode.create(cos(vr),pbestrealtype^)
                     else
                      hp:=crealconstnode.create(cos(vl),pbestrealtype^);
                   end;
                 in_const_sin :
                   begin
                     if isreal then
                      hp:=crealconstnode.create(sin(vr),pbestrealtype^)
                     else
                      hp:=crealconstnode.create(sin(vl),pbestrealtype^);
                   end;
                 in_const_exp :
                   begin
                     if isreal then
                      hp:=crealconstnode.create(exp(vr),pbestrealtype^)
                     else
                      hp:=crealconstnode.create(exp(vl),pbestrealtype^);
                   end;
                 in_const_ln :
                   begin
                     if isreal then
                       begin
                          if vr<=0.0 then
                           CGMessage(type_e_wrong_math_argument)
                          else
                           hp:=crealconstnode.create(ln(vr),pbestrealtype^)
                       end
                     else
                       begin
                          if vl<=0 then
                           CGMessage(type_e_wrong_math_argument)
                          else
                           hp:=crealconstnode.create(ln(vl),pbestrealtype^);
                       end;
                   end;
                 else
                   internalerror(88);
               end;
             end;
            if hp=nil then
             hp:=tnode.create(errorn);
            firstpass(hp);
            result:=hp;
          end
         else
          begin
            case inlinenumber of
             in_lo_qword,
             in_hi_qword,
             in_lo_long,
             in_hi_long,
             in_lo_word,
             in_hi_word:

               begin
                  if left.nodetype=ordconstn then
                   begin
                     case inlinenumber of
                       in_lo_word :
                         hp:=cordconstnode.create(tordconstnode(left).value and $ff,left.resulttype);
                       in_hi_word :
                         hp:=cordconstnode.create(tordconstnode(left).value shr 8,left.resulttype);
                       in_lo_long :
                         hp:=cordconstnode.create(tordconstnode(left).value and $ffff,left.resulttype);
                       in_hi_long :
                         hp:=cordconstnode.create(tordconstnode(left).value shr 16,left.resulttype);
                       in_lo_qword :
                         hp:=cordconstnode.create(tordconstnode(left).value and $ffffffff,left.resulttype);
                       in_hi_qword :
                         hp:=cordconstnode.create(tordconstnode(left).value shr 32,left.resulttype);
                     end;
                     firstpass(hp);
                     result:=hp;
                   end
                  else
                   begin
                     if registers32<1 then
                      registers32:=1;
                     location.loc:=LOC_REGISTER;
                   end;
               end;

             in_sizeof_x:
               begin
                 if push_high_param(left.resulttype.def) then
                  begin
                    srsym:=searchsymonlyin(tloadnode(left).symtable,'high'+pvarsym(tloadnode(left).symtableentry)^.name);
                    hp:=caddnode.create(addn,cloadnode.create(pvarsym(srsym),tloadnode(left).symtable),
                                     cordconstnode.create(1,s32bittype));
                    if (left.resulttype.def^.deftype=arraydef) and
                       (parraydef(left.resulttype.def)^.elesize<>1) then
                      hp:=caddnode.create(muln,hp,cordconstnode.create(parraydef(left.resulttype.def)^.elesize,s32bittype));
                    firstpass(hp);
                    result:=hp;
                  end
                 else
                  begin
                    if registers32<1 then
                       registers32:=1;
                    location.loc:=LOC_REGISTER;
                  end;
               end;

             in_typeof_x:
               begin
                  if registers32<1 then
                    registers32:=1;
                  location.loc:=LOC_REGISTER;
               end;

             in_ord_x:
               begin
                 if (left.nodetype=ordconstn) then
                  begin
                    hp:=cordconstnode.create(tordconstnode(left).value,s32bittype);
                    firstpass(hp);
                    result:=hp;
                  end
                 else
                  begin
                    case left.resulttype.def^.deftype of
                      orddef :
                        begin
                          case porddef(left.resulttype.def)^.typ of
                            uchar:
                              begin
                                hp:=ctypeconvnode.create(left,u8bittype);
                                left:=nil;
                                include(hp.flags,nf_explizit);
                                firstpass(hp);
                                result:=hp;
                              end;
                            uwidechar:
                              begin
                                hp:=ctypeconvnode.create(left,u16bittype);
                                left:=nil;
                                include(hp.flags,nf_explizit);
                                firstpass(hp);
                                result:=hp;
                              end;
                            bool8bit:
                              begin
                                hp:=ctypeconvnode.create(left,u8bittype);
                                left:=nil;
                                ttypeconvnode(hp).convtype:=tc_bool_2_int;
                                include(hp.flags,nf_explizit);
                                firstpass(hp);
                                result:=hp;
                              end;
                            bool16bit:
                              begin
                                hp:=ctypeconvnode.create(left,u16bittype);
                                left:=nil;
                                ttypeconvnode(hp).convtype:=tc_bool_2_int;
                                include(hp.flags,nf_explizit);
                                firstpass(hp);
                                result:=hp;
                              end;
                            bool32bit:
                              begin
                                hp:=ctypeconvnode.create(left,u32bittype);
                                left:=nil;
                                ttypeconvnode(hp).convtype:=tc_bool_2_int;
                                include(hp.flags,nf_explizit);
                                firstpass(hp);
                                result:=hp;
                              end;
                            else
                              begin
                                { all other orddef need no transformation }
                                hp:=left;
                                left:=nil;
                                result:=hp;
                              end;
                          end;
                        end;
                      enumdef :
                        begin
                          hp:=ctypeconvnode.create(left,s32bittype);
                          left:=nil;
                          include(hp.flags,nf_explizit);
                          firstpass(hp);
                          result:=hp;
                        end
                    end;
                  end;
               end;

             in_chr_byte:
               begin
                  hp:=ctypeconvnode.create(left,cchartype);
                  left:=nil;
                  include(hp.flags,nf_explizit);
                  firstpass(hp);
                  result:=hp;
               end;


             in_length_string:
               begin
                  { we don't need string conversations here }
                  if (left.nodetype=typeconvn) and
                     (ttypeconvnode(left).left.resulttype.def^.deftype=stringdef) then
                    begin
                       hp:=ttypeconvnode(left).left;
                       ttypeconvnode(left).left:=nil;
                       left.free;
                       left:=hp;
                    end;

                  { evaluates length of constant strings direct }
                  if (left.nodetype=stringconstn) then
                    begin
                       hp:=cordconstnode.create(tstringconstnode(left).len,s32bittype);
                       firstpass(hp);
                       result:=hp;
                    end
                  { length of char is one always }
                  else if is_constcharnode(left) then
                    begin
                       hp:=cordconstnode.create(1,s32bittype);
                       firstpass(hp);
                       result:=hp;
                    end;
               end;

             in_typeinfo_x:
               begin
                  location.loc:=LOC_REGISTER;
                  registers32:=1;
               end;

             in_assigned_x:
               begin
                  location.loc:=LOC_FLAGS;
               end;

             in_ofs_x :
               internalerror(2000101001);

             in_seg_x :
               begin
                 hp:=cordconstnode.create(0,s32bittype);
                 firstpass(hp);
                 result:=hp;
               end;

             in_pred_x,
             in_succ_x:
               begin
                  if left.nodetype=ordconstn then
                   begin
                     if inlinenumber=in_succ_x then
                      hp:=cordconstnode.create(tordconstnode(left).value+1,left.resulttype)
                     else
                      hp:=cordconstnode.create(tordconstnode(left).value-1,left.resulttype);
                     firstpass(hp);
                     result:=hp;
                   end
                  else
                   begin
                     if is_64bitint(resulttype.def) then
                       begin
                          if (registers32<2) then
                            registers32:=2
                       end
                     else
                       begin
                          if (registers32<1) then
                            registers32:=1;
                       end;
                     location.loc:=LOC_REGISTER;
                   end;
               end;

             in_setlength_x:
               begin
               end;

             in_finalize_x:
               begin
               end;

             in_inc_x,
             in_dec_x:
               begin
                  { check type }
                  if is_64bitint(left.resulttype.def) or
                     { range/overflow checking doesn't work properly }
                     { with the inc/dec code that's generated (JM)   }
                     ((left.resulttype.def^.deftype = orddef) and
                      not(is_char(left.resulttype.def)) and
                      not(is_boolean(left.resulttype.def)) and
                      (aktlocalswitches *
                       [cs_check_overflow,cs_check_range] <> [])) then
                    { convert to simple add (JM) }
                    begin
                      { extra parameter? }
                      if assigned(tcallparanode(left).right) then
                        begin
                          { Yes, use for add node }
                          hpp := tcallparanode(tcallparanode(left).right).left;
                          tcallparanode(tcallparanode(left).right).left := nil;
                          if assigned(tcallparanode(tcallparanode(left).right).right) then
                            CGMessage(cg_e_illegal_expression);
                        end
                      else
                        { no, create constant 1 }
                        hpp := cordconstnode.create(1,s32bittype);
                      { addition/substraction depending on inc/dec }
                      if inlinenumber = in_inc_x then
                        hp := caddnode.create(addn,tcallparanode(left).left.getcopy,hpp)
                      else
                        hp := caddnode.create(subn,tcallparanode(left).left.getcopy,hpp);
                      { assign result of addition }
                      hpp := cassignmentnode.create(tcallparanode(left).left,hp);
                      tcallparanode(left).left := nil;
                      { firstpass it }
                      firstpass(hpp);
                      { return new node }
                      result := hpp;
                    end
                  else if (left.resulttype.def^.deftype in [enumdef,pointerdef]) or
                          is_ordinal(left.resulttype.def) then
                    begin
                       { two paras ? }
                       if assigned(tcallparanode(left).right) then
                         begin
                            { need we an additional register ? }
                            if not(is_constintnode(tcallparanode(tcallparanode(left).right).left)) and
                              (tcallparanode(tcallparanode(left).right).left.location.loc in [LOC_MEM,LOC_REFERENCE]) and
                              (tcallparanode(tcallparanode(left).right).left.registers32<=1) then
                              inc(registers32);

                            { do we need an additional register to restore the first parameter? }
                            if tcallparanode(tcallparanode(left).right).left.registers32>=registers32 then
                              inc(registers32);
                         end;
                    end;
               end;

             in_read_x,
             in_readln_x,
             in_write_x,
             in_writeln_x :
               begin
                  { needs a call }
                  procinfo^.flags:=procinfo^.flags or pi_do_call;
                  { true, if readln needs an extra register }
                  extra_register:=false;
                  { we must know if it is a typed file or not }
                  { but we must first do the firstpass for it }
                  file_is_typed:=false;
                  if assigned(left) then
                    begin
                       iswrite:=(inlinenumber in [in_write_x,in_writeln_x]);
                       tcallparanode(left).firstcallparan(nil,true);
                       set_varstate(left,iswrite);
                       { now we can check }
                       hp:=left;
                       while assigned(tcallparanode(hp).right) do
                         hp:=tcallparanode(hp).right;
                       { if resulttype.def is not assigned, then automatically }
                       { file is not typed.                             }
                       if assigned(hp) and assigned(hp.resulttype.def) then
                         Begin
                           if (hp.resulttype.def^.deftype=filedef) then
                           if (pfiledef(hp.resulttype.def)^.filetyp=ft_untyped) then
                             begin
                              if (inlinenumber in [in_readln_x,in_writeln_x]) then
                                CGMessage(type_e_no_readln_writeln_for_typed_file)
                              else
                                CGMessage(type_e_no_read_write_for_untyped_file);
                             end
                           else if (pfiledef(hp.resulttype.def)^.filetyp=ft_typed) then
                            begin
                              file_is_typed:=true;
                              { test the type }
                              if (inlinenumber in [in_readln_x,in_writeln_x]) then
                                CGMessage(type_e_no_readln_writeln_for_typed_file);
                              hpp:=left;
                              while (hpp<>hp) do
                               begin
                                 if (tcallparanode(hpp).left.nodetype=typen) then
                                   CGMessage(type_e_cant_read_write_type);
                                 if not is_equal(hpp.resulttype.def,pfiledef(hp.resulttype.def)^.typedfiletype.def) then
                                   CGMessage(type_e_mismatch);
                                 { generate the high() value for the shortstring }
                                 if ((not iswrite) and is_shortstring(tcallparanode(hpp).left.resulttype.def)) or
                                    (is_chararray(tcallparanode(hpp).left.resulttype.def)) then
                                   tcallparanode(hpp).gen_high_tree(true);
                                 { read(ln) is call by reference (JM) }
                                 if not iswrite then
                                   make_not_regable(tcallparanode(hpp).left);
                                 hpp:=tcallparanode(hpp).right;
                               end;
                            end;
                         end; { endif assigned(hp) }

                       { insert type conversions for write(ln) }
                       if (not file_is_typed) then
                         begin
                            hp:=left;
                            while assigned(hp) do
                              begin
{$ifdef i386}
                                incrementregisterpushed($ff);
{$endif}
{$ifdef m68k}
                                for regi:=R_D0 to R_A6 do
                                  inc(reg_pushes[regi],t_times*2);
{$endif}
                                if (tcallparanode(hp).left.nodetype=typen) then
                                  CGMessage(type_e_cant_read_write_type);
                                if assigned(tcallparanode(hp).left.resulttype.def) then
                                  begin
                                    isreal:=false;
                                    { support writeln(procvar) }
                                    if (tcallparanode(hp).left.resulttype.def^.deftype=procvardef) then
                                     begin
                                       p1:=ccallnode.create(nil,nil,nil,nil);
                                       tcallnode(p1).set_procvar(tcallparanode(hp).left);
                                       firstpass(p1);
                                       tcallparanode(hp).left:=p1;
                                     end;
                                    case tcallparanode(hp).left.resulttype.def^.deftype of
                                      filedef :
                                        begin
                                          { only allowed as first parameter }
                                          if assigned(tcallparanode(hp).right) then
                                            CGMessage(type_e_cant_read_write_type);
                                        end;
                                      stringdef :
                                        begin
                                          { generate the high() value for the shortstring }
                                          if (not iswrite) and
                                             is_shortstring(tcallparanode(hp).left.resulttype.def) then
                                            tcallparanode(hp).gen_high_tree(true);
                                        end;
                                      pointerdef :
                                        begin
                                          if not is_pchar(tcallparanode(hp).left.resulttype.def) then
                                            CGMessage(type_e_cant_read_write_type);
                                        end;
                                      floatdef :
                                        begin
                                          isreal:=true;
                                        end;
                                      orddef :
                                        begin
                                          case porddef(tcallparanode(hp).left.resulttype.def)^.typ of
                                            uchar,
                                            u32bit,s32bit,
                                            u64bit,s64bit:
                                              ;
                                            u8bit,s8bit,
                                            u16bit,s16bit :
                                              if iswrite then
                                                tcallparanode(hp).left:=ctypeconvnode.create(tcallparanode(hp).left,s32bittype);
                                            bool8bit,
                                            bool16bit,
                                            bool32bit :
                                              if iswrite then
                                                tcallparanode(hp).left:=ctypeconvnode.create(tcallparanode(hp).left,booltype)
                                              else
                                                CGMessage(type_e_cant_read_write_type);
                                            else
                                              CGMessage(type_e_cant_read_write_type);
                                          end;
                                          if not(iswrite) and
                                            not(is_64bitint(tcallparanode(hp).left.resulttype.def)) then
                                            extra_register:=true;
                                        end;
                                      arraydef :
                                        begin
                                          if is_chararray(tcallparanode(hp).left.resulttype.def) then
                                            tcallparanode(hp).gen_high_tree(true)
                                          else
                                            CGMessage(type_e_cant_read_write_type);
                                        end;
                                      else
                                        CGMessage(type_e_cant_read_write_type);
                                    end;

                                    { some format options ? }
                                    if cpf_is_colon_para in tcallparanode(hp).callparaflags then
                                      begin
                                         if cpf_is_colon_para in tcallparanode(tcallparanode(hp).right).callparaflags then
                                           begin
                                              frac_para:=hp;
                                              length_para:=tcallparanode(hp).right;
                                              hp:=tcallparanode(hp).right;
                                              hpp:=tcallparanode(hp).right;
                                           end
                                         else
                                           begin
                                              length_para:=hp;
                                              frac_para:=nil;
                                              hpp:=tcallparanode(hp).right;
                                           end;
                                         { can be nil if you use "write(e:0:6)" while e is undeclared (JM) }
                                         if assigned(tcallparanode(hpp).left.resulttype.def) then
                                           isreal:=(tcallparanode(hpp).left.resulttype.def^.deftype=floatdef)
                                         else exit;
                                         if (not is_integer(tcallparanode(length_para).left.resulttype.def)) then
                                          CGMessage1(type_e_integer_expr_expected,tcallparanode(length_para).left.resulttype.def^.typename)
                                        else
                                          tcallparanode(length_para).left:=ctypeconvnode.create(tcallparanode(length_para).left,s32bittype);
                                        if assigned(frac_para) then
                                          begin
                                            if isreal then
                                             begin
                                               if (not is_integer(tcallparanode(frac_para).left.resulttype.def)) then
                                                 CGMessage1(type_e_integer_expr_expected,tcallparanode(frac_para).left.resulttype.def^.typename)
                                               else
                                                 tcallparanode(frac_para).left:=ctypeconvnode.create(tcallparanode(frac_para).left,s32bittype);
                                             end
                                            else
                                             CGMessage(parser_e_illegal_colon_qualifier);
                                          end;
                                        { do the checking for the colon'd arg }
                                        hp:=length_para;
                                      end;
                                  end;
                                 hp:=tcallparanode(hp).right;
                              end;
                         end;
                       { pass all parameters again for the typeconversions }
                       if codegenerror then
                         exit;
                       tcallparanode(left).firstcallparan(nil,true);
                       set_varstate(left,true);
                       { calc registers }
                       left_max;
                       if extra_register then
                         inc(registers32);
                    end;
               end;

            in_settextbuf_file_x :
              begin
                 { warning here left is the callparannode
                   not the argument directly }
                 { left.left is text var }
                 { left.right.left is the buffer var }
                 { firstcallparan(left,nil);
                   already done in firstcalln }
                 { now we know the type of buffer }
                 srsym:=searchsymonlyin(systemunit,'SETTEXTBUF');
                 hp:=ccallparanode.create(cordconstnode.create(tcallparanode(left).left.resulttype.def^.size,s32bittype),left);
                 hp:=ccallnode.create(hp,pprocsym(srsym),systemunit,nil);
                 left:=nil;
                 firstpass(hp);
                 result:=hp;
              end;

            in_reset_typedfile,
            in_rewrite_typedfile :
              begin
                 procinfo^.flags:=procinfo^.flags or pi_do_call;
              end;

            in_str_x_string :
              begin
                 procinfo^.flags:=procinfo^.flags or pi_do_call;
                 { first pass just the string for first local use }
                 hp:=tcallparanode(left).right;
                 tcallparanode(left).right:=nil;
                 tcallparanode(left).firstcallparan(nil,true);
                 { remove warning when result is passed }
                 set_funcret_is_valid(tcallparanode(left).left);
                 tcallparanode(left).right:=hp;
                 tcallparanode(tcallparanode(left).right).firstcallparan(nil,true);
                 set_varstate(tcallparanode(left).right,true);
                 hp:=left;
                 { valid string ? }
                 if not assigned(hp) or
                    (tcallparanode(hp).left.resulttype.def^.deftype<>stringdef) or
                    (tcallparanode(hp).right=nil) then
                   CGMessage(cg_e_illegal_expression);
                 { we need a var parameter }
                 valid_for_assign(tcallparanode(hp).left,false);
                 { generate the high() value for the shortstring }
                 if is_shortstring(tcallparanode(hp).left.resulttype.def) then
                   tcallparanode(hp).gen_high_tree(true);

                 { !!!! check length of string }

                 while assigned(tcallparanode(hp).right) do
                   hp:=tcallparanode(hp).right;

                 if not assigned(tcallparanode(hp).resulttype.def) then
                   exit;
                 { check and convert the first param }
                 if (cpf_is_colon_para in tcallparanode(hp).callparaflags) or
                    not assigned(hp.resulttype.def) then
                   CGMessage(cg_e_illegal_expression);

                 isreal:=false;
                 case hp.resulttype.def^.deftype of
                   orddef :
                     begin
                       case porddef(tcallparanode(hp).left.resulttype.def)^.typ of
                         u32bit,s32bit,
                         s64bit,u64bit:
                           ;
                         u8bit,s8bit,
                         u16bit,s16bit:
                           tcallparanode(hp).left:=ctypeconvnode.create(tcallparanode(hp).left,s32bittype);
                         else
                           CGMessage(type_e_integer_or_real_expr_expected);
                       end;
                     end;
                   floatdef :
                     begin
                       isreal:=true;
                     end;
                   else
                     CGMessage(type_e_integer_or_real_expr_expected);
                 end;

                 { some format options ? }
                 hpp:=tcallparanode(left).right;
                 if assigned(hpp) and (cpf_is_colon_para in tcallparanode(hpp).callparaflags) then
                   begin
                     firstpass(tcallparanode(hpp).left);
                     set_varstate(tcallparanode(hpp).left,true);
                     if (not is_integer(tcallparanode(hpp).left.resulttype.def)) then
                       CGMessage1(type_e_integer_expr_expected,tcallparanode(hpp).left.resulttype.def^.typename)
                     else
                       tcallparanode(hpp).left:=ctypeconvnode.create(tcallparanode(hpp).left,s32bittype);
                     hpp:=tcallparanode(hpp).right;
                     if assigned(hpp) and (cpf_is_colon_para in tcallparanode(hpp).callparaflags) then
                       begin
                         if isreal then
                          begin
                            if (not is_integer(tcallparanode(hpp).left.resulttype.def)) then
                              CGMessage1(type_e_integer_expr_expected,tcallparanode(hpp).left.resulttype.def^.typename)
                            else
                              begin
                                firstpass(tcallparanode(hpp).left);
                                set_varstate(tcallparanode(hpp).left,true);
                                tcallparanode(hpp).left:=ctypeconvnode.create(tcallparanode(hpp).left,s32bittype);
                              end;
                          end
                         else
                          CGMessage(parser_e_illegal_colon_qualifier);
                       end;
                   end;

                 { pass all parameters again for the typeconversions }
                 if codegenerror then
                   exit;
                 tcallparanode(left).firstcallparan(nil,true);
                 { calc registers }
                 left_max;
              end;

            in_val_x :
              begin
                 procinfo^.flags:=procinfo^.flags or pi_do_call;
                 resulttype:=voidtype;
                 { check the amount of parameters }
                 if not(assigned(left)) or
                    not(assigned(tcallparanode(left).right)) then
                  begin
                    CGMessage(parser_e_wrong_parameter_size);
                    exit;
                  end;
                 If Assigned(tcallparanode(tcallparanode(left).right).right) Then
                  {there is a "code" parameter}
                    Begin
                 { first pass just the code parameter for first local use}
                      hp := tcallparanode(left).right;
                      tcallparanode(left).right := nil;
                      make_not_regable(tcallparanode(left).left);
                      tcallparanode(left).firstcallparan(nil,true);
                      set_varstate(left,false);
                      if codegenerror then exit;
                      tcallparanode(left).right := hp;
                    {code has to be a var parameter}
                      if valid_for_assign(tcallparanode(left).left,false) then
                       begin
                         if (tcallparanode(left).left.resulttype.def^.deftype <> orddef) or
                           not(porddef(tcallparanode(left).left.resulttype.def)^.typ in
                               [u16bit,s16bit,u32bit,s32bit]) then
                          CGMessage(type_e_mismatch);
                       end;
                      hpp := tcallparanode(left).right
                    End
                 Else hpp := left;
                 {now hpp = the destination value tree}
                 { first pass just the destination parameter for first local use}
                 hp:=tcallparanode(hpp).right;
                 tcallparanode(hpp).right:=nil;
                 {hpp = destination}
                 make_not_regable(tcallparanode(hpp).left);
                 tcallparanode(hpp).firstcallparan(nil,true);
                 set_varstate(hpp,false);

                 if codegenerror then
                   exit;
                 { remove warning when result is passed }
                 set_funcret_is_valid(tcallparanode(hpp).left);
                 tcallparanode(hpp).right := hp;
                 if valid_for_assign(tcallparanode(hpp).left,false) then
                  begin
                    If Not((tcallparanode(hpp).left.resulttype.def^.deftype = floatdef) or
                           ((tcallparanode(hpp).left.resulttype.def^.deftype = orddef) And
                            (POrdDef(tcallparanode(hpp).left.resulttype.def)^.typ in
                             [u32bit,s32bit,
                              u8bit,s8bit,u16bit,s16bit,s64bit,u64bit]))) Then
                      CGMessage(type_e_mismatch);
                  end;
                {hp = source (String)}
                 { count_ref := false; WHY ?? }
                 tcallparanode(hp).firstcallparan(nil,true);
                 set_varstate(hp,true);
                 if codegenerror then
                   exit;
                 { if not a stringdef then insert a type conv which
                   does the other type checking }
                 If (tcallparanode(hp).left.resulttype.def^.deftype<>stringdef) then
                  begin
                    tcallparanode(hp).left:=ctypeconvnode.create(tcallparanode(hp).left,cshortstringtype);
                    firstpass(tcallparanode(hp).left);
                  end;
                 { calc registers }
                 left_max;

                 { val doesn't calculate the registers really }
                 { correct, we need one register extra   (FK) }
                 if is_64bitint(tcallparanode(hpp).left.resulttype.def) then
                   inc(registers32,2)
                 else
                   inc(registers32,1);
              end;

            in_include_x_y,
            in_exclude_x_y:
              begin
                 registers32:=left.registers32;
                 registersfpu:=left.registersfpu;
{$ifdef SUPPORT_MMX}
                 registersmmx:=left.registersmmx;
{$endif SUPPORT_MMX}
              end;

            in_low_x,
            in_high_x:
              begin
                case left.resulttype.def^.deftype of
                  orddef,enumdef:
                    begin
                      hp:=do_lowhigh(left.resulttype);
                      firstpass(hp);
                      result:=hp;
                    end;
                  setdef:
                    begin
                      hp:=do_lowhigh(Psetdef(left.resulttype.def)^.elementtype);
                      firstpass(hp);
                      result:=hp;
                    end;
                  arraydef:
                    begin
                      if inlinenumber=in_low_x then
                       begin
                         hp:=cordconstnode.create(Parraydef(left.resulttype.def)^.lowrange,parraydef(left.resulttype.def)^.rangetype);
                         firstpass(hp);
                         result:=hp;
                       end
                      else
                       begin
                         if is_open_array(left.resulttype.def) or
                           is_array_of_const(left.resulttype.def) then
                          begin
                            srsym:=searchsymonlyin(tloadnode(left).symtable,'high'+pvarsym(tloadnode(left).symtableentry)^.name);
                            hp:=cloadnode.create(pvarsym(srsym),tloadnode(left).symtable);
                            firstpass(hp);
                            result:=hp;
                          end
                         else
                          begin
                            hp:=cordconstnode.create(Parraydef(left.resulttype.def)^.highrange,parraydef(left.resulttype.def)^.rangetype);
                            firstpass(hp);
                            result:=hp;
                          end;
                       end;
                    end;
                  stringdef:
                    begin
                      if inlinenumber=in_low_x then
                       begin
                         hp:=cordconstnode.create(0,u8bittype);
                         firstpass(hp);
                         result:=hp;
                       end
                      else
                       begin
                         if is_open_string(left.resulttype.def) then
                          begin
                            srsym:=searchsymonlyin(tloadnode(left).symtable,'high'+pvarsym(tloadnode(left).symtableentry)^.name);
                            hp:=cloadnode.create(pvarsym(srsym),tloadnode(left).symtable);
                            firstpass(hp);
                            result:=hp;
                          end
                         else
                          begin
                            hp:=cordconstnode.create(Pstringdef(left.resulttype.def)^.len,u8bittype);
                            firstpass(hp);
                            result:=hp;
                          end;
                       end;
                   end;
                end;
              end;

            in_cos_extended:
              begin
                 if left.nodetype in [ordconstn,realconstn] then
                   setconstrealvalue(cos(getconstrealvalue))
                 else
                   begin
                     location.loc:=LOC_FPU;
                     registers32:=left.registers32;
                     registersfpu:=left.registersfpu;
{$ifdef SUPPORT_MMX}
                     registersmmx:=left.registersmmx;
{$endif SUPPORT_MMX}
                   end;
              end;

            in_sin_extended:
              begin
                 if left.nodetype in [ordconstn,realconstn] then
                   setconstrealvalue(sin(getconstrealvalue))
                 else
                   begin
                     location.loc:=LOC_FPU;
                     registers32:=left.registers32;
                     registersfpu:=left.registersfpu;
{$ifdef SUPPORT_MMX}
                     registersmmx:=left.registersmmx;
{$endif SUPPORT_MMX}
                   end;
              end;

            in_arctan_extended:
              begin
                 if left.nodetype in [ordconstn,realconstn] then
                   setconstrealvalue(arctan(getconstrealvalue))
                 else
                   begin
                     location.loc:=LOC_FPU;
                     registers32:=left.registers32;
                     registersfpu:=left.registersfpu;
{$ifdef SUPPORT_MMX}
                     registersmmx:=left.registersmmx;
{$endif SUPPORT_MMX}
                   end;
              end;

            in_pi:
              begin
                if block_type=bt_const then
                  setconstrealvalue(pi)
                else
                  location.loc:=LOC_FPU;
              end;

            in_abs_extended:
              begin
                 if left.nodetype in [ordconstn,realconstn] then
                   setconstrealvalue(abs(getconstrealvalue))
                 else
                   begin
                     location.loc:=LOC_FPU;
                     registers32:=left.registers32;
                     registersfpu:=left.registersfpu;
{$ifdef SUPPORT_MMX}
                     registersmmx:=left.registersmmx;
{$endif SUPPORT_MMX}
                   end;
              end;

            in_sqr_extended:
              begin
                 if left.nodetype in [ordconstn,realconstn] then
                   setconstrealvalue(sqr(getconstrealvalue))
                 else
                   begin
                     location.loc:=LOC_FPU;
                     registers32:=left.registers32;
                     registersfpu:=left.registersfpu;
{$ifdef SUPPORT_MMX}
                     registersmmx:=left.registersmmx;
{$endif SUPPORT_MMX}
                   end;
              end;

            in_sqrt_extended:
              begin
                 if left.nodetype in [ordconstn,realconstn] then
                   begin
                      vr:=getconstrealvalue;
                      if vr<0.0 then
                        begin
                           CGMessage(type_e_wrong_math_argument);
                           setconstrealvalue(0);
                        end
                      else
                        setconstrealvalue(sqrt(vr));
                   end
                 else
                   begin
                     location.loc:=LOC_FPU;
                     registers32:=left.registers32;
                     registersfpu:=left.registersfpu;
{$ifdef SUPPORT_MMX}
                     registersmmx:=left.registersmmx;
{$endif SUPPORT_MMX}
                   end;
              end;

            in_ln_extended:
              begin
                 if left.nodetype in [ordconstn,realconstn] then
                   begin
                      vr:=getconstrealvalue;
                      if vr<=0.0 then
                        begin
                           CGMessage(type_e_wrong_math_argument);
                           setconstrealvalue(0);
                        end
                      else
                        setconstrealvalue(ln(vr));
                   end
                 else
                   begin
                     location.loc:=LOC_FPU;
                     registers32:=left.registers32;
                     registersfpu:=left.registersfpu;
{$ifdef SUPPORT_MMX}
                     registersmmx:=left.registersmmx;
{$endif SUPPORT_MMX}
                   end;
              end;

{$ifdef SUPPORT_MMX}
            in_mmx_pcmpeqb..in_mmx_pcmpgtw:
              begin
              end;
{$endif SUPPORT_MMX}

            in_assert_x_y :
               begin
                 registers32:=left.registers32;
                 registersfpu:=left.registersfpu;
{$ifdef SUPPORT_MMX}
                 registersmmx:=left.registersmmx;
{$endif SUPPORT_MMX}
                 { We've checked the whole statement for correctness, now we
                   can remove it if assertions are off }
                 if not(cs_do_assertion in aktlocalswitches) then
                   { we need a valid node, so insert a nothingn }
                   result:=cnothingnode.create;
               end;

             else
               internalerror(8);
             end;
          end;
         dec(parsing_para_level);
       end;
{$ifdef fpc}
{$maxfpuregisters default}
{$endif fpc}

    function tinlinenode.docompare(p: tnode): boolean;
      begin
        docompare :=
          inherited docompare(p) and
          (inlinenumber = tinlinenode(p).inlinenumber);
      end;



begin
   cinlinenode:=tinlinenode;
end.
{
  $Log$
  Revision 1.33  2001-04-04 21:30:43  florian
    * applied several fixes to get the DD8 Delphi Unit compiled
     e.g. "forward"-interfaces are working now

  Revision 1.32  2001/04/02 21:20:31  peter
    * resulttype rewrite

  Revision 1.31  2001/03/23 00:16:07  florian
    + some stuff to compile FreeCLX added

  Revision 1.30  2001/03/12 12:47:46  michael
  + Patches from peter

  Revision 1.29  2001/03/03 12:38:08  jonas
    * fixed low() for signed types < 64bit

  Revision 1.28  2001/02/26 19:44:53  peter
    * merged generic m68k updates from fixes branch

  Revision 1.27  2001/02/22 11:24:40  jonas
    * fixed bug in previous fix (hopped over revision 1.26 because that one
      also removed the fix for high(cardinal))

  Revision 1.26  2001/02/21 20:50:59  peter
    * fix to compile again, but high(cardinal) with $R+ still fails!

  Revision 1.25  2001/02/21 12:57:46  jonas
    * fixed high/low for cardinal, int64 and qword

  Revision 1.24  2001/01/06 19:54:11  peter
    * merged fix for 1310

  Revision 1.23  2001/01/06 18:28:39  peter
    * fixed wrong notes about locals

  Revision 1.22  2000/12/31 11:14:10  jonas
    + implemented/fixed docompare() mathods for all nodes (not tested)
    + nopt.pas, nadd.pas, i386/n386opt.pas: optimized nodes for adding strings
      and constant strings/chars together
    * n386add.pas: don't copy temp strings (of size 256) to another temp string
      when adding

  Revision 1.21  2000/12/25 00:07:26  peter
    + new tlinkedlist class (merge of old tstringqueue,tcontainer and
      tlinkedlist objects)

  Revision 1.20  2000/12/17 14:35:41  peter
    * fixed crash with val()

  Revision 1.19  2000/11/29 00:30:33  florian
    * unused units removed from uses clause
    * some changes for widestrings

  Revision 1.18  2000/11/12 15:27:22  jonas
    * also don't do conversion for chars/booleans (hopefully final change :/)

  Revision 1.17  2000/11/11 21:08:13  jonas
    * don't do inc/dec to add/sub conversion for enums

  Revision 1.16  2000/11/11 16:18:35  peter
    * ptr returns farpointer

  Revision 1.15  2000/11/11 15:59:07  jonas
    * convert inc/dec to add/sub when range/overflow checking is on

  Revision 1.14  2000/11/09 17:46:54  florian
    * System.TypeInfo fixed
    + System.Finalize implemented
    + some new keywords for interface support added

  Revision 1.13  2000/11/04 16:48:32  florian
    * innr.inc renamed to make compiler compilation easier because the rtl contains
      a file of the same name

  Revision 1.12  2000/10/31 22:02:48  peter
    * symtable splitted, no real code changes

  Revision 1.11  2000/10/26 14:15:06  jonas
    * fixed setlength for shortstrings

  Revision 1.10  2000/10/21 18:16:11  florian
    * a lot of changes:
       - basic dyn. array support
       - basic C++ support
       - some work for interfaces done
       ....

  Revision 1.9  2000/10/15 08:38:46  jonas
    * added missing getcopy for previous addition

  Revision 1.8  2000/10/14 18:27:53  jonas
    * merged fix for inc/dec on 64bit types from tcinl

  Revision 1.7  2000/10/14 10:14:50  peter
    * moehrendorf oct 2000 rewrite

  Revision 1.6  2000/10/01 19:48:24  peter
    * lot of compile updates for cg11

  Revision 1.5  2000/09/28 19:49:52  florian
  *** empty log message ***

  Revision 1.4  2000/09/28 16:34:47  florian
  *** empty log message ***

  Revision 1.3  2000/09/27 21:33:22  florian
    * finally nadd.pas compiles

  Revision 1.2  2000/09/27 20:25:44  florian
    * more stuff fixed

  Revision 1.1  2000/09/26 14:59:34  florian
    * more conversion work done

}
