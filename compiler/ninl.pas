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
       node;

    type
       type
          tinlinenode = class(tunarynode)
             inlinenumber : byte;
             constructor create(number : byte;is_const:boolean;l : tnode);virtual;
             function getcopy : tnode;override;
             function pass_1 : tnode;override;
          end;

    var
       cinlinenode : class of tinlinenode;

   function geninlinenode(number : byte;is_const:boolean;l : tnode) : tinlinenode;

implementation

    uses
      cobjects,verbose,globals,systems,
      globtype,
      symconst,symtable,aasm,types,
      htypechk,pass_1,
      ncal,cpubase
{$ifdef newcg}
      ,cgbase
      ,tgobj
      ,tgcpu
{$else newcg}
      ,hcodegen
{$ifdef i386}
      ,tgeni386
{$endif}
{$endif newcg}
      ;

   function geninlinenode(number : byte;is_const:boolean;l : tnode) : tinlinenode;

     begin
        geninlinenode:=cinlinenode.create(number,is_const,l);
     end:

{*****************************************************************************
                           TINLINENODE
*****************************************************************************}

    constructor tinlinenode.create(number : byte;is_const:boolean;l : tnode);

      begin
         inherited create(inlinen,l);
         if is_const then
           include(flags,nf_is_const);
         inlinenumber:=number;
      end;

    function tinlinenode.getcopy : tnode;

      var
         n : tinlinenode;

      begin
         n:=tinlinenode(inherited getcopy);
         n.inlinenumber:=inlinenumber;
      end;

{$ifdef fpc}
{$maxfpuregisters 0}
{$endif fpc}
    function tinlinenode.pass_1 : tnode;override;
      var
         vl,vl2  : longint;
         vr      : bestreal;
         p1,hp,hpp  :  tnode;
{$ifndef NOCOLONCHECK}
         frac_para,length_para : tnode;
{$endif ndef NOCOLONCHECK}
         extra_register,
         isreal,
         dowrite,
         file_is_typed : boolean;

      procedure do_lowhigh(adef : pdef);

        var
           v : longint;
           enum : penumsym;

        begin
           case Adef^.deftype of
             orddef:
               begin
                  if inlinenumber=in_low_x then
                    v:=porddef(adef)^.low
                  else
                    v:=porddef(adef)^.high;
                  hp:=genordinalconstnode(v,adef);
                  firstpass(hp);
                  disposetree(p);
                  p:=hp;
               end;
             enumdef:
               begin
                  enum:=Penumdef(Adef)^.firstenum;
                  if inlinenumber=in_high_x then
                    while enum^.nextenum<>nil do
                      enum:=enum^.nextenum;
                  hp:=genenumnode(enum);
                  disposetree(p);
                  p:=hp;
               end;
           else
             internalerror(87);
           end;
        end;

      function getconstrealvalue : bestreal;

        begin
           case left.treetype of
              ordconstn:
                getconstrealvalue:=left.value;
              realconstn:
                getconstrealvalue:=left.value_real;
              else
                internalerror(309992);
           end;
        end;

      procedure setconstrealvalue(r : bestreal);

        var
           hp : tnode;

        begin
           hp:=genrealconstnode(r,bestrealdef^);
           disposetree(p);
           p:=hp;
           firstpass(p);
        end;

      procedure handleextendedfunction;

        begin
           location.loc:=LOC_FPU;
           resulttype:=s80floatdef;
           { redo firstpass for varstate status PM }
           set_varstate(left,true);
           if (left.resulttype^.deftype<>floatdef) or
             (pfloatdef(left.resulttype)^.typ<>s80real) then
             begin
                left:=gentypeconvnode(left,s80floatdef);
                firstpass(left);
             end;
           registers32:=left.registers32;
           registersfpu:=left.registersfpu;
{$ifdef SUPPORT_MMX}
           registersmmx:=left.registersmmx;
{$endif SUPPORT_MMX}
        end;

      begin
         { if we handle writeln; left contains no valid address }
         if assigned(left) then
           begin
              if left.treetype=callparan then
                firstcallparan(left,nil,false)
              else
                firstpass(left);
              left_right_max(p);
              set_location(location,left.location);
           end;
         inc(parsing_para_level);
         { handle intern constant functions in separate case }
         if inlineconst then
          begin
            hp:=nil;
            { no parameters? }
            if not assigned(left) then
             begin
               case inlinenumber of
                 in_const_pi :
                   hp:=genrealconstnode(pi,bestrealdef^);
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
               case left.treetype of
                 realconstn :
                   begin
                     isreal:=true;
                     vr:=left.value_real;
                   end;
                 ordconstn :
                   vl:=left.value;
                 callparan :
                   begin
                     { both exists, else it was not generated }
                     vl:=left.left.value;
                     vl2:=left.right.left.value;
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
                               hp:=genordinalconstnode(1,s32bitdef)
                            end
                          else
                            hp:=genordinalconstnode(trunc(vr),s32bitdef)
                       end
                     else
                      hp:=genordinalconstnode(trunc(vl),s32bitdef);
                   end;
                 in_const_round :
                   begin
                     if isreal then
                       begin
                          if (vr>=2147483647.5) or (vr<=-2147483648.5) then
                            begin
                               CGMessage(parser_e_range_check_error);
                               hp:=genordinalconstnode(1,s32bitdef)
                            end
                          else
                            hp:=genordinalconstnode(round(vr),s32bitdef)
                       end
                     else
                      hp:=genordinalconstnode(round(vl),s32bitdef);
                   end;
                 in_const_frac :
                   begin
                     if isreal then
                      hp:=genrealconstnode(frac(vr),bestrealdef^)
                     else
                      hp:=genrealconstnode(frac(vl),bestrealdef^);
                   end;
                 in_const_int :
                   begin
                     if isreal then
                      hp:=genrealconstnode(int(vr),bestrealdef^)
                     else
                      hp:=genrealconstnode(int(vl),bestrealdef^);
                   end;
                 in_const_abs :
                   begin
                     if isreal then
                      hp:=genrealconstnode(abs(vr),bestrealdef^)
                     else
                      hp:=genordinalconstnode(abs(vl),left.resulttype);
                   end;
                 in_const_sqr :
                   begin
                     if isreal then
                      hp:=genrealconstnode(sqr(vr),bestrealdef^)
                     else
                      hp:=genordinalconstnode(sqr(vl),left.resulttype);
                   end;
                 in_const_odd :
                   begin
                     if isreal then
                      CGMessage1(type_e_integer_expr_expected,left.resulttype^.typename)
                     else
                      hp:=genordinalconstnode(byte(odd(vl)),booldef);
                   end;
                 in_const_swap_word :
                   begin
                     if isreal then
                      CGMessage1(type_e_integer_expr_expected,left.resulttype^.typename)
                     else
                      hp:=genordinalconstnode((vl and $ff) shl 8+(vl shr 8),left.resulttype);
                   end;
                 in_const_swap_long :
                   begin
                     if isreal then
                      CGMessage(type_e_mismatch)
                     else
                      hp:=genordinalconstnode((vl and $ffff) shl 16+(vl shr 16),left.resulttype);
                   end;
                 in_const_ptr :
                   begin
                     if isreal then
                      CGMessage(type_e_mismatch)
                     else
                      hp:=genordinalconstnode((vl2 shl 16) or vl,voidpointerdef);
                   end;
                 in_const_sqrt :
                   begin
                     if isreal then
                       begin
                          if vr<0.0 then
                           CGMessage(type_e_wrong_math_argument)
                          else
                           hp:=genrealconstnode(sqrt(vr),bestrealdef^)
                       end
                     else
                       begin
                          if vl<0 then
                           CGMessage(type_e_wrong_math_argument)
                          else
                           hp:=genrealconstnode(sqrt(vl),bestrealdef^);
                       end;
                   end;
                 in_const_arctan :
                   begin
                     if isreal then
                      hp:=genrealconstnode(arctan(vr),bestrealdef^)
                     else
                      hp:=genrealconstnode(arctan(vl),bestrealdef^);
                   end;
                 in_const_cos :
                   begin
                     if isreal then
                      hp:=genrealconstnode(cos(vr),bestrealdef^)
                     else
                      hp:=genrealconstnode(cos(vl),bestrealdef^);
                   end;
                 in_const_sin :
                   begin
                     if isreal then
                      hp:=genrealconstnode(sin(vr),bestrealdef^)
                     else
                      hp:=genrealconstnode(sin(vl),bestrealdef^);
                   end;
                 in_const_exp :
                   begin
                     if isreal then
                      hp:=genrealconstnode(exp(vr),bestrealdef^)
                     else
                      hp:=genrealconstnode(exp(vl),bestrealdef^);
                   end;
                 in_const_ln :
                   begin
                     if isreal then
                       begin
                          if vr<=0.0 then
                           CGMessage(type_e_wrong_math_argument)
                          else
                           hp:=genrealconstnode(ln(vr),bestrealdef^)
                       end
                     else
                       begin
                          if vl<=0 then
                           CGMessage(type_e_wrong_math_argument)
                          else
                           hp:=genrealconstnode(ln(vl),bestrealdef^);
                       end;
                   end;
                 else
                   internalerror(88);
               end;
             end;
            disposetree(p);
            if hp=nil then
             hp:=genzeronode(errorn);
            firstpass(hp);
            p:=hp;
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
                  set_varstate(left,true);
                  if registers32<1 then
                    registers32:=1;
                  if inlinenumber in [in_lo_word,in_hi_word] then
                    resulttype:=u8bitdef
                  else if inlinenumber in [in_lo_qword,in_hi_qword] then
                    begin
                       resulttype:=u32bitdef;
                       if (m_tp in aktmodeswitches) or
                          (m_delphi in aktmodeswitches) then
                         CGMessage(type_w_maybe_wrong_hi_lo);
                    end
                  else
                    begin
                       resulttype:=u16bitdef;
                       if (m_tp in aktmodeswitches) or
                          (m_delphi in aktmodeswitches) then
                         CGMessage(type_w_maybe_wrong_hi_lo);
                    end;
                  location.loc:=LOC_REGISTER;
                  if not is_integer(left.resulttype) then
                    CGMessage(type_e_mismatch)
                  else
                    begin
                      if left.treetype=ordconstn then
                       begin
                         case inlinenumber of
                          in_lo_word : hp:=genordinalconstnode(left.value and $ff,left.resulttype);
                          in_hi_word : hp:=genordinalconstnode(left.value shr 8,left.resulttype);
                          in_lo_long : hp:=genordinalconstnode(left.value and $ffff,left.resulttype);
                          in_hi_long : hp:=genordinalconstnode(left.value shr 16,left.resulttype);
                          in_lo_qword : hp:=genordinalconstnode(left.value and $ffffffff,left.resulttype);
                          in_hi_qword : hp:=genordinalconstnode(left.value shr 32,left.resulttype);
                         end;
                         disposetree(p);
                         firstpass(hp);
                         p:=hp;
                       end;
                    end;
               end;

             in_sizeof_x:
               begin
                 set_varstate(left,false);
                 if push_high_param(left.resulttype) then
                  begin
                    getsymonlyin(left.symtable,'high'+pvarsym(left.symtableentry)^.name);
                    hp:=gennode(addn,genloadnode(pvarsym(srsym),left.symtable),
                                     genordinalconstnode(1,s32bitdef));
                    if (left.resulttype^.deftype=arraydef) and
                       (parraydef(left.resulttype)^.elesize<>1) then
                      hp:=gennode(muln,hp,genordinalconstnode(parraydef(left.resulttype)^.elesize,s32bitdef));
                    disposetree(p);
                    p:=hp;
                    firstpass(p);
                  end;
                 if registers32<1 then
                    registers32:=1;
                 resulttype:=s32bitdef;
                 location.loc:=LOC_REGISTER;
               end;

             in_typeof_x:
               begin
                  set_varstate(left,false);
                  if registers32<1 then
                    registers32:=1;
                  location.loc:=LOC_REGISTER;
                  resulttype:=voidpointerdef;
               end;

             in_ord_x:
               begin
                  set_varstate(left,true);
                  if (left.treetype=ordconstn) then
                    begin
                       hp:=genordinalconstnode(left.value,s32bitdef);
                       disposetree(p);
                       p:=hp;
                       firstpass(p);
                    end
                  else
                    begin
                       { otherwise you get a crash if you try ord on an expression containing }
                       { an undeclared variable (JM)                                          }
                       if not assigned(left.resulttype) then
                         exit;
                       if (left.resulttype^.deftype=orddef) then
                         if (porddef(left.resulttype)^.typ in [uchar,uwidechar,bool8bit]) then
                           case porddef(left.resulttype)^.typ of
                            uchar:
                               begin
                                  hp:=gentypeconvnode(left,u8bitdef);
                                  putnode(p);
                                  p:=hp;
                                  explizit:=true;
                                  firstpass(p);
                               end;
                            uwidechar:
                               begin
                                  hp:=gentypeconvnode(left,u16bitdef);
                                  putnode(p);
                                  p:=hp;
                                  explizit:=true;
                                  firstpass(p);
                               end;
                            bool8bit:
                               begin
                                  hp:=gentypeconvnode(left,u8bitdef);
                                  putnode(p);
                                  p:=hp;
                                  convtyp:=tc_bool_2_int;
                                  explizit:=true;
                                  firstpass(p);
                               end
                           end
                         { can this happen ? }
                         else if (porddef(left.resulttype)^.typ=uvoid) then
                           CGMessage(type_e_mismatch)
                         else
                           { all other orddef need no transformation }
                           begin
                              hp:=left;
                              putnode(p);
                              p:=hp;
                           end
                       else if (left.resulttype^.deftype=enumdef) then
                         begin
                            hp:=gentypeconvnode(left,s32bitdef);
                            putnode(p);
                            p:=hp;
                            explizit:=true;
                            firstpass(p);
                         end
                       else
                         begin
                            { can anything else be ord() ?}
                            CGMessage(type_e_mismatch);
                         end;
                    end;
               end;

             in_chr_byte:
               begin
                  set_varstate(left,true);
                  hp:=gentypeconvnode(left,cchardef);
                  putnode(p);
                  p:=hp;
                  explizit:=true;
                  firstpass(p);
               end;

             in_length_string:
               begin
                  set_varstate(left,true);
                  if is_ansistring(left.resulttype) then
                    resulttype:=s32bitdef
                  else
                    resulttype:=u8bitdef;
                  { we don't need string conversations here }
                  if (left.treetype=typeconvn) and
                     (left.left.resulttype^.deftype=stringdef) then
                    begin
                       hp:=left.left;
                       putnode(left);
                       left:=hp;
                    end;

                  { check the type, must be string or char }
                  if (left.resulttype^.deftype<>stringdef) and
                     (not is_char(left.resulttype)) then
                    CGMessage(type_e_mismatch);

                  { evaluates length of constant strings direct }
                  if (left.treetype=stringconstn) then
                    begin
                       hp:=genordinalconstnode(left.length,s32bitdef);
                       disposetree(p);
                       firstpass(hp);
                       p:=hp;
                    end
                  { length of char is one allways }
                  else if is_constcharnode(left) then
                    begin
                       hp:=genordinalconstnode(1,s32bitdef);
                       disposetree(p);
                       firstpass(hp);
                       p:=hp;
                    end;
               end;

             in_typeinfo_x:
               begin
                  resulttype:=voidpointerdef;
                  location.loc:=LOC_REGISTER;
                  registers32:=1;
               end;

             in_assigned_x:
               begin
                  set_varstate(left,true);
                  resulttype:=booldef;
                  location.loc:=LOC_FLAGS;
               end;

             in_ofs_x,
             in_seg_x :
               set_varstate(left,false);
             in_pred_x,
             in_succ_x:
               begin
                  resulttype:=left.resulttype;
                  if is_64bitint(resulttype) then
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
                  set_varstate(left,true);
                  if not is_ordinal(resulttype) then
                    CGMessage(type_e_ordinal_expr_expected)
                  else
                    begin
                      if (resulttype^.deftype=enumdef) and
                         (penumdef(resulttype)^.has_jumps) then
                        CGMessage(type_e_succ_and_pred_enums_with_assign_not_possible)
                      else
                        if left.treetype=ordconstn then
                         begin
                           if inlinenumber=in_succ_x then
                             hp:=genordinalconstnode(left.value+1,left.resulttype)
                           else
                             hp:=genordinalconstnode(left.value-1,left.resulttype);
                           disposetree(p);
                           firstpass(hp);
                           p:=hp;
                         end;
                    end;
               end;

             in_inc_x,
             in_dec_x:
               begin
                 resulttype:=voiddef;
                 if assigned(left) then
                   begin
                      firstcallparan(left,nil,true);
                      set_varstate(left,true);
                      if codegenerror then
                       exit;
                      { first param must be var }
                      valid_for_assign(left.left,false);
                      { check type }
                      if (left.resulttype^.deftype in [enumdef,pointerdef]) or
                         is_ordinal(left.resulttype) then
                        begin
                           { two paras ? }
                           if assigned(left.right) then
                             begin
                                { insert a type conversion       }
                                { the second param is always longint }
                                left.right.left:=gentypeconvnode(left.right.left,s32bitdef);
                                { check the type conversion }
                                firstpass(left.right.left);

                                { need we an additional register ? }
                                if not(is_constintnode(left.right.left)) and
                                  (left.right.left.location.loc in [LOC_MEM,LOC_REFERENCE]) and
                                  (left.right.left.registers32<=1) then
                                  inc(registers32);

                                { do we need an additional register to restore the first parameter? }
                                if left.right.left.registers32>=registers32 then
                                  inc(registers32);

                                if assigned(left.right.right) then
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
                  { needs a call }
                  procinfo^.flags:=procinfo^.flags or pi_do_call;
                  resulttype:=voiddef;
                  { true, if readln needs an extra register }
                  extra_register:=false;
                  { we must know if it is a typed file or not }
                  { but we must first do the firstpass for it }
                  file_is_typed:=false;
                  if assigned(left) then
                    begin
                       dowrite:=(inlinenumber in [in_write_x,in_writeln_x]);
                       firstcallparan(left,nil,true);
                       set_varstate(left,dowrite);
                       { now we can check }
                       hp:=left;
                       while assigned(hp.right) do
                         hp:=hp.right;
                       { if resulttype is not assigned, then automatically }
                       { file is not typed.                             }
                       if assigned(hp) and assigned(hp.resulttype) then
                         Begin
                           if (hp.resulttype^.deftype=filedef) then
                           if (pfiledef(hp.resulttype)^.filetyp=ft_untyped) then
                             begin
                              if (inlinenumber in [in_readln_x,in_writeln_x]) then
                                CGMessage(type_e_no_readln_writeln_for_typed_file)
                              else
                                CGMessage(type_e_no_read_write_for_untyped_file);
                             end
                           else if (pfiledef(hp.resulttype)^.filetyp=ft_typed) then
                            begin
                              file_is_typed:=true;
                              { test the type }
                              if (inlinenumber in [in_readln_x,in_writeln_x]) then
                                CGMessage(type_e_no_readln_writeln_for_typed_file);
                              hpp:=left;
                              while (hpp<>hp) do
                               begin
                                 if (hpp.left.treetype=typen) then
                                   CGMessage(type_e_cant_read_write_type);
                                 if not is_equal(hpp.resulttype,pfiledef(hp.resulttype)^.typedfiletype.def) then
                                   CGMessage(type_e_mismatch);
                                 { generate the high() value for the shortstring }
                                 if ((not dowrite) and is_shortstring(hpp.left.resulttype)) or
                                    (is_chararray(hpp.left.resulttype)) then
                                   gen_high_tree(hpp,true);
                                 { read(ln) is call by reference (JM) }
                                 if not dowrite then
                                   make_not_regable(hpp.left);
                                 hpp:=hpp.right;
                               end;
                            end;
                         end; { endif assigned(hp) }

                       { insert type conversions for write(ln) }
                       if (not file_is_typed) then
                         begin
                            hp:=left;
                            while assigned(hp) do
                              begin
                                incrementregisterpushed($ff);
                                if (hp.left.treetype=typen) then
                                  CGMessage(type_e_cant_read_write_type);
                                if assigned(hp.left.resulttype) then
                                  begin
                                    isreal:=false;
                                    { support writeln(procvar) }
                                    if (hp.left.resulttype^.deftype=procvardef) then
                                     begin
                                       p1:=gencallnode(nil,nil);
                                       p1^.right:=hp.left;
                                       p1^.resulttype:=pprocvardef(hp.left.resulttype)^.rettype.def;
                                       firstpass(p1);
                                       hp.left:=p1;
                                     end;
                                    case hp.left.resulttype^.deftype of
                                      filedef :
                                        begin
                                          { only allowed as first parameter }
                                          if assigned(hp.right) then
                                            CGMessage(type_e_cant_read_write_type);
                                        end;
                                      stringdef :
                                        begin
                                          { generate the high() value for the shortstring }
                                          if (not dowrite) and
                                             is_shortstring(hp.left.resulttype) then
                                            gen_high_tree(hp,true);
                                        end;
                                      pointerdef :
                                        begin
                                          if not is_pchar(hp.left.resulttype) then
                                            CGMessage(type_e_cant_read_write_type);
                                        end;
                                      floatdef :
                                        begin
                                          isreal:=true;
                                        end;
                                      orddef :
                                        begin
                                          case porddef(hp.left.resulttype)^.typ of
                                            uchar,
                                            u32bit,s32bit,
                                            u64bit,s64bit:
                                              ;
                                            u8bit,s8bit,
                                            u16bit,s16bit :
                                              if dowrite then
                                                hp.left:=gentypeconvnode(hp.left,s32bitdef);
                                            bool8bit,
                                            bool16bit,
                                            bool32bit :
                                              if dowrite then
                                                hp.left:=gentypeconvnode(hp.left,booldef)
                                              else
                                                CGMessage(type_e_cant_read_write_type);
                                            else
                                              CGMessage(type_e_cant_read_write_type);
                                          end;
                                          if not(dowrite) and
                                            not(is_64bitint(hp.left.resulttype)) then
                                            extra_register:=true;
                                        end;
                                      arraydef :
                                        begin
                                          if is_chararray(hp.left.resulttype) then
                                            gen_high_tree(hp,true)
                                          else
                                            CGMessage(type_e_cant_read_write_type);
                                        end;
                                      else
                                        CGMessage(type_e_cant_read_write_type);
                                    end;

                                    { some format options ? }
                                    if hp.is_colon_para then
                                      begin
                                         if hp.right.is_colon_para then
                                           begin
                                              frac_para:=hp;
                                              length_para:=hp.right;
                                              hp:=hp.right;
                                              hpp:=hp.right;
                                           end
                                         else
                                           begin
                                              length_para:=hp;
                                              frac_para:=nil;
                                              hpp:=hp.right;
                                           end;
                                         { can be nil if you use "write(e:0:6)" while e is undeclared (JM) }
                                         if assigned(hpp.left.resulttype) then
                                           isreal:=(hpp.left.resulttype^.deftype=floatdef)
                                         else exit;
                                         if (not is_integer(length_para^.left.resulttype)) then
                                          CGMessage1(type_e_integer_expr_expected,length_para^.left.resulttype^.typename)
                                        else
                                          length_para^.left:=gentypeconvnode(length_para^.left,s32bitdef);
                                        if assigned(frac_para) then
                                          begin
                                            if isreal then
                                             begin
                                               if (not is_integer(frac_para^.left.resulttype)) then
                                                 CGMessage1(type_e_integer_expr_expected,frac_para^.left.resulttype^.typename)
                                               else
                                                 frac_para^.left:=gentypeconvnode(frac_para^.left,s32bitdef);
                                             end
                                            else
                                             CGMessage(parser_e_illegal_colon_qualifier);
                                          end;
                                        { do the checking for the colon'd arg }
                                        hp:=length_para;
                                      end;
                                  end;
                                 hp:=hp.right;
                              end;
                         end;
                       { pass all parameters again for the typeconversions }
                       if codegenerror then
                         exit;
                       firstcallparan(left,nil,true);
                       set_varstate(left,true);
                       { calc registers }
                       left_right_max(p);
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
                 getsymonlyin(systemunit,'SETTEXTBUF');
                 hp:=gencallnode(pprocsym(srsym),systemunit);
                 hp.left:=gencallparanode(
                   genordinalconstnode(left.left.resulttype^.size,s32bitdef),left);
                 putnode(p);
                 p:=hp;
                 firstpass(p);
              end;

             { the firstpass of the arg has been done in firstcalln ? }
             in_reset_typedfile,
             in_rewrite_typedfile :
               begin
                  procinfo^.flags:=procinfo^.flags or pi_do_call;
                  firstpass(left);
                  set_varstate(left,true);
                  resulttype:=voiddef;
               end;

             in_str_x_string :
               begin
                  procinfo^.flags:=procinfo^.flags or pi_do_call;
                  resulttype:=voiddef;
                  { check the amount of parameters }
                  if not(assigned(left)) or
                     not(assigned(left.right)) then
                   begin
                     CGMessage(parser_e_wrong_parameter_size);
                     exit;
                   end;
                  { first pass just the string for first local use }
                  hp:=left.right;
                  left.right:=nil;
                  firstcallparan(left,nil,true);
                  set_varstate(left,false);
                  { remove warning when result is passed }
                  set_funcret_is_valid(left.left);
                  left.right:=hp;
                  firstcallparan(left.right,nil,true);
                  set_varstate(left.right,true);
                  hp:=left;
                  { valid string ? }
                  if not assigned(hp) or
                     (hp.left.resulttype^.deftype<>stringdef) or
                     (hp.right=nil) then
                    CGMessage(cg_e_illegal_expression);
                  { we need a var parameter }
                  valid_for_assign(hp.left,false);
                  { generate the high() value for the shortstring }
                  if is_shortstring(hp.left.resulttype) then
                    gen_high_tree(hp,true);

                  { !!!! check length of string }

                  while assigned(hp.right) do
                    hp:=hp.right;

                  if not assigned(hp.resulttype) then
                    exit;
                  { check and convert the first param }
                  if (hp.is_colon_para) or
                     not assigned(hp.resulttype) then
                    CGMessage(cg_e_illegal_expression);

                  isreal:=false;
                  case hp.resulttype^.deftype of
                    orddef :
                      begin
                        case porddef(hp.left.resulttype)^.typ of
                          u32bit,s32bit,
                          s64bit,u64bit:
                            ;
                          u8bit,s8bit,
                          u16bit,s16bit:
                            hp.left:=gentypeconvnode(hp.left,s32bitdef);
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
                  hpp:=left.right;
                  if assigned(hpp) and hpp.is_colon_para then
                    begin
                      firstpass(hpp.left);
                      set_varstate(hpp.left,true);
                      if (not is_integer(hpp.left.resulttype)) then
                        CGMessage1(type_e_integer_expr_expected,hpp.left.resulttype^.typename)
                      else
                        hpp.left:=gentypeconvnode(hpp.left,s32bitdef);
                      hpp:=hpp.right;
                      if assigned(hpp) and hpp.is_colon_para then
                        begin
                          if isreal then
                           begin
                             if (not is_integer(hpp.left.resulttype)) then
                               CGMessage1(type_e_integer_expr_expected,hpp.left.resulttype^.typename)
                             else
                               begin
                                 firstpass(hpp.left);
                                 set_varstate(hpp.left,true);
                                 hpp.left:=gentypeconvnode(hpp.left,s32bitdef);
                               end;
                           end
                          else
                           CGMessage(parser_e_illegal_colon_qualifier);
                        end;
                    end;

                  { pass all parameters again for the typeconversions }
                  if codegenerror then
                    exit;
                  firstcallparan(left,nil,true);
                  { calc registers }
                  left_right_max(p);
               end;

             in_val_x :
               begin
                  procinfo^.flags:=procinfo^.flags or pi_do_call;
                  resulttype:=voiddef;
                  { check the amount of parameters }
                  if not(assigned(left)) or
                     not(assigned(left.right)) then
                   begin
                     CGMessage(parser_e_wrong_parameter_size);
                     exit;
                   end;
                  If Assigned(left.right.right) Then
                   {there is a "code" parameter}
                     Begin
                  { first pass just the code parameter for first local use}
                       hp := left.right;
                       left.right := nil;
                       make_not_regable(left.left);
                       firstcallparan(left, nil,true);
                       set_varstate(left,false);
                       if codegenerror then exit;
                       left.right := hp;
                     {code has to be a var parameter}
                       if valid_for_assign(left.left,false) then
                        begin
                          if (left.left.resulttype^.deftype <> orddef) or
                            not(porddef(left.left.resulttype)^.typ in
                                [u16bit,s16bit,u32bit,s32bit]) then
                           CGMessage(type_e_mismatch);
                        end;
                       hpp := left.right
                     End
                  Else hpp := left;
                  {now hpp = the destination value tree}
                  { first pass just the destination parameter for first local use}
                  hp:=hpp.right;
                  hpp.right:=nil;
                  {hpp = destination}
                  make_not_regable(hpp.left);
                  firstcallparan(hpp,nil,true);
                  set_varstate(hpp,false);

                  if codegenerror then
                    exit;
                  { remove warning when result is passed }
                  set_funcret_is_valid(hpp.left);
                  hpp.right := hp;
                  if valid_for_assign(hpp.left,false) then
                   begin
                     If Not((hpp.left.resulttype^.deftype = floatdef) or
                            ((hpp.left.resulttype^.deftype = orddef) And
                             (POrdDef(hpp.left.resulttype)^.typ in
                              [u32bit,s32bit,
                               u8bit,s8bit,u16bit,s16bit,s64bit,u64bit]))) Then
                       CGMessage(type_e_mismatch);
                   end;
                 {hp = source (String)}
                  { count_ref := false; WHY ?? }
                  firstcallparan(hp,nil,true);
                  set_varstate(hp,true);
                  if codegenerror then
                    exit;
                  { if not a stringdef then insert a type conv which
                    does the other type checking }
                  If (hp.left.resulttype^.deftype<>stringdef) then
                   begin
                     hp.left:=gentypeconvnode(hp.left,cshortstringdef);
                     firstpass(hp);
                   end;
                  { calc registers }
                  left_right_max(p);

                  { val doesn't calculate the registers really }
                  { correct, we need one register extra   (FK) }
                  if is_64bitint(hpp.left.resulttype) then
                    inc(registers32,2)
                  else
                    inc(registers32,1);
               end;

             in_include_x_y,
             in_exclude_x_y:
               begin
                 resulttype:=voiddef;
                 if assigned(left) then
                   begin
                      firstcallparan(left,nil,true);
                      set_varstate(left,true);
                      registers32:=left.registers32;
                      registersfpu:=left.registersfpu;
{$ifdef SUPPORT_MMX}
                      registersmmx:=left.registersmmx;
{$endif SUPPORT_MMX}
                      { remove warning when result is passed }
                      set_funcret_is_valid(left.left);
                      { first param must be var }
                      valid_for_assign(left.left,false);
                      { check type }
                      if assigned(left.resulttype) and
                         (left.resulttype^.deftype=setdef) then
                        begin
                           { two paras ? }
                           if assigned(left.right) then
                             begin
                                { insert a type conversion       }
                                { to the type of the set elements  }
                                left.right.left:=gentypeconvnode(
                                  left.right.left,
                                  psetdef(left.resulttype)^.elementtype.def);
                                { check the type conversion }
                                firstpass(left.right.left);
                                { only three parameters are allowed }
                                if assigned(left.right.right) then
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
                  { this fixes tests\webtbs\tbug879.pp (FK)
                  if left.treetype in [typen,loadn,subscriptn] then
                    begin
                  }
                       case left.resulttype^.deftype of
                          orddef,enumdef:
                            begin
                               do_lowhigh(left.resulttype);
                               firstpass(p);
                            end;
                          setdef:
                            begin
                               do_lowhigh(Psetdef(left.resulttype)^.elementtype.def);
                               firstpass(p);
                            end;
                         arraydef:
                            begin
                              if inlinenumber=in_low_x then
                               begin
                                 hp:=genordinalconstnode(Parraydef(left.resulttype)^.lowrange,
                                   Parraydef(left.resulttype)^.rangetype.def);
                                 disposetree(p);
                                 p:=hp;
                                 firstpass(p);
                               end
                              else
                               begin
                                 if is_open_array(left.resulttype) or
                                   is_array_of_const(left.resulttype) then
                                  begin
                                    getsymonlyin(left.symtable,'high'+pvarsym(left.symtableentry)^.name);
                                    hp:=genloadnode(pvarsym(srsym),left.symtable);
                                    disposetree(p);
                                    p:=hp;
                                    firstpass(p);
                                  end
                                 else
                                  begin
                                    hp:=genordinalconstnode(Parraydef(left.resulttype)^.highrange,
                                      Parraydef(left.resulttype)^.rangetype.def);
                                    disposetree(p);
                                    p:=hp;
                                    firstpass(p);
                                  end;
                               end;
                           end;
                         stringdef:
                           begin
                              if inlinenumber=in_low_x then
                               begin
                                 hp:=genordinalconstnode(0,u8bitdef);
                                 disposetree(p);
                                 p:=hp;
                                 firstpass(p);
                               end
                              else
                               begin
                                 if is_open_string(left.resulttype) then
                                  begin
                                    getsymonlyin(left.symtable,'high'+pvarsym(left.symtableentry)^.name);
                                    hp:=genloadnode(pvarsym(srsym),left.symtable);
                                    disposetree(p);
                                    p:=hp;
                                    firstpass(p);
                                  end
                                 else
                                  begin
                                    hp:=genordinalconstnode(Pstringdef(left.resulttype)^.len,u8bitdef);
                                    disposetree(p);
                                    p:=hp;
                                    firstpass(p);
                                  end;
                               end;
                           end;
                         else
                           CGMessage(type_e_mismatch);
                         end;
                  {
                    end
                  else
                    CGMessage(type_e_varid_or_typeid_expected);
                  }
               end;

             in_cos_extended:
               begin
                  if left.treetype in [ordconstn,realconstn] then
                    setconstrealvalue(cos(getconstrealvalue))
                  else
                    handleextendedfunction;
               end;

             in_sin_extended:
               begin
                  if left.treetype in [ordconstn,realconstn] then
                    setconstrealvalue(sin(getconstrealvalue))
                  else
                    handleextendedfunction;
               end;

             in_arctan_extended:
               begin
                  if left.treetype in [ordconstn,realconstn] then
                    setconstrealvalue(arctan(getconstrealvalue))
                  else
                    handleextendedfunction;
               end;

             in_pi:
               if block_type=bt_const then
                 setconstrealvalue(pi)
               else
                 begin
                    location.loc:=LOC_FPU;
                    resulttype:=s80floatdef;
                 end;

             in_abs_extended:
               begin
                  if left.treetype in [ordconstn,realconstn] then
                    setconstrealvalue(abs(getconstrealvalue))
                  else
                    handleextendedfunction;
               end;

             in_sqr_extended:
               begin
                  if left.treetype in [ordconstn,realconstn] then
                    setconstrealvalue(sqr(getconstrealvalue))
                  else
                    handleextendedfunction;
               end;

             in_sqrt_extended:
               begin
                  if left.treetype in [ordconstn,realconstn] then
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
                    handleextendedfunction;
               end;

             in_ln_extended:
               begin
                  if left.treetype in [ordconstn,realconstn] then
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
                    handleextendedfunction;
               end;

{$ifdef SUPPORT_MMX}
            in_mmx_pcmpeqb..in_mmx_pcmpgtw:
              begin
              end;
{$endif SUPPORT_MMX}
            in_assert_x_y :
               begin
                 resulttype:=voiddef;
                 if assigned(left) then
                   begin
                      firstcallparan(left,nil,true);
                      set_varstate(left,true);
                      registers32:=left.registers32;
                      registersfpu:=left.registersfpu;
{$ifdef SUPPORT_MMX}
                      registersmmx:=left.registersmmx;
{$endif SUPPORT_MMX}
                      { check type }
                      if is_boolean(left.resulttype) then
                        begin
                           { must always be a string }
                           left.right.left:=gentypeconvnode(left.right.left,cshortstringdef);
                           firstpass(left.right.left);
                        end
                      else
                        CGMessage(type_e_mismatch);
                   end
                 else
                   CGMessage(type_e_mismatch);
                 { We've checked the whole statement for correctness, now we
                   can remove it if assertions are off }
                 if not(cs_do_assertion in aktlocalswitches) then
                  begin
                    disposetree(left);
                    putnode(p);
                    { we need a valid node, so insert a nothingn }
                    p:=genzeronode(nothingn);
                  end;
               end;

              else
               internalerror(8);
             end;
            end;
           { generate an error if no resulttype is set }
           if not assigned(resulttype) then
             resulttype:=generrordef;
         dec(parsing_para_level);
       end;
{$ifdef fpc}
{$maxfpuregisters default}
{$endif fpc}

begin
   cinlinenode:=tinlinenode;
end.
{
  $Log$
  Revision 1.1  2000-09-26 14:59:34  florian
    * more conversion work done

}