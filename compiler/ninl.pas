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

    {$i innr.inc}

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
      pass_1,htypechk,
      ncal,ncon,ncnv,nadd,nld,
      cpubase
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
      end;

{$ifdef fpc}
{$maxfpuregisters 0}
{$endif fpc}
    function tinlinenode.pass_1 : tnode;
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
                  pass_1:=hp;
               end;
             enumdef:
               begin
                  enum:=Penumdef(Adef)^.firstenum;
                  if inlinenumber=in_high_x then
                    while enum^.nextenum<>nil do
                      enum:=enum^.nextenum;
                  hp:=genenumnode(enum);
                  pass_1:=hp;
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
           hp:=genrealconstnode(r,bestrealdef^);
           firstpass(hp);
        end;

      procedure handleextendedfunction;

        begin
           location.loc:=LOC_FPU;
           resulttype:=s80floatdef;
           { redo firstpass for varstate status PM }
           left.set_varstate(true);
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
              if left.nodetype=callparan then
                tcallparanode(left).firstcallparan(nil,false)
              else
                firstpass(left);
              left_right_max(self);
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
            if hp=nil then
             hp:=tnode.create(errorn);
            firstpass(hp);
            pass_1:=hp;
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
                  left.set_varstate(true);
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
                      if left.nodetype=ordconstn then
                       begin
                         case inlinenumber of
                          in_lo_word : hp:=genordinalconstnode(tordconstnode(left).value and $ff,left.resulttype);
                          in_hi_word : hp:=genordinalconstnode(tordconstnode(left).value shr 8,left.resulttype);
                          in_lo_long : hp:=genordinalconstnode(tordconstnode(left).value and $ffff,left.resulttype);
                          in_hi_long : hp:=genordinalconstnode(tordconstnode(left).value shr 16,left.resulttype);
                          in_lo_qword : hp:=genordinalconstnode(tordconstnode(left).value and $ffffffff,left.resulttype);
                          in_hi_qword : hp:=genordinalconstnode(tordconstnode(left).value shr 32,left.resulttype);
                         end;
                         firstpass(hp);
                         pass_1:=hp;
                       end;
                    end;
               end;

             in_sizeof_x:
               begin
                 left.set_varstate(false);
                 if push_high_param(left.resulttype) then
                  begin
                    getsymonlyin(tloadnode(left).symtable,'high'+pvarsym(tloadnode(left).symtableentry)^.name);
                    hp:=caddnode.create(addn,genloadnode(pvarsym(srsym),tloadnode(left).symtable),
                                     genordinalconstnode(1,s32bitdef));
                    if (left.resulttype^.deftype=arraydef) and
                       (parraydef(left.resulttype)^.elesize<>1) then
                      hp:=caddnode.create(muln,hp,genordinalconstnode(parraydef(left.resulttype)^.elesize,s32bitdef));
                    firstpass(hp);
                  end;
                 if hp.registers32<1 then
                    hp.registers32:=1;
                 hp.resulttype:=s32bitdef;
                 hp.location.loc:=LOC_REGISTER;
                 pass_1:=hp;
               end;

             in_typeof_x:
               begin
                  left.set_varstate(false);
                  if registers32<1 then
                    registers32:=1;
                  location.loc:=LOC_REGISTER;
                  resulttype:=voidpointerdef;
               end;

             in_ord_x:
               begin
                  left.set_varstate(true);
                  if (left.nodetype=ordconstn) then
                    begin
                       hp:=genordinalconstnode(tordconstnode(left).value,s32bitdef);
                       firstpass(hp);
                       pass_1:=hp;
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
                                  left:=nil;
                                  include(hp.flags,nf_explizit);
                                  firstpass(hp);
                                  pass_1:=hp;
                               end;
                            uwidechar:
                               begin
                                  hp:=gentypeconvnode(left,u16bitdef);
                                  left:=nil;
                                  include(hp.flags,nf_explizit);
                                  firstpass(hp);
                                  pass_1:=hp;
                               end;
                            bool8bit:
                               begin
                                  hp:=gentypeconvnode(left,u8bitdef);
                                  left:=nil;
                                  ttypeconvnode(hp).convtyp:=tc_bool_2_int;
                                  include(hp.flags,nf_explizit);
                                  firstpass(hp);
                                  pass_1:=hp;
                               end
                           end
                         { can this happen ? }
                         else if (porddef(left.resulttype)^.typ=uvoid) then
                           CGMessage(type_e_mismatch)
                         else
                           { all other orddef need no transformation }
                           begin
                              hp:=left;
                              left:=nil;
                              pass_1:=hp;
                           end
                       else if (left.resulttype^.deftype=enumdef) then
                         begin
                            hp:=gentypeconvnode(left,s32bitdef);
                            left:=nil;
                            include(hp.flags,nf_explizit);
                            firstpass(hp);
                            pass_1:=hp;
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
                  left.set_varstate(true);
                  hp:=gentypeconvnode(left,cchardef);
                  left:=nil;
                  include(hp.flags,nf_explizit);
                  firstpass(hp);
                  pass_1:=hp;
               end;

             in_length_string:
               begin
                  left.set_varstate(true);
                  if is_ansistring(left.resulttype) then
                    resulttype:=s32bitdef
                  else
                    resulttype:=u8bitdef;
                  { we don't need string conversations here }
                  if (left.nodetype=typeconvn) and
                     (ttypeconvnode(left).left.resulttype^.deftype=stringdef) then
                    begin
                       hp:=ttypeconvnode(left).left;
                       ttypeconvnode(left).left:=nil;
                       left.free;
                       left:=hp;
                    end;

                  { check the type, must be string or char }
                  if (left.resulttype^.deftype<>stringdef) and
                     (not is_char(left.resulttype)) then
                    CGMessage(type_e_mismatch);

                  { evaluates length of constant strings direct }
                  if (left.nodetype=stringconstn) then
                    begin
                       hp:=genordinalconstnode(tstringconstnode(left).len,s32bitdef);
                       firstpass(hp);
                       pass_1:=hp;
                    end
                  { length of char is one allways }
                  else if is_constcharnode(left) then
                    begin
                       hp:=genordinalconstnode(1,s32bitdef);
                       firstpass(hp);
                       pass_1:=hp;
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
                  left.set_varstate(true);
                  resulttype:=booldef;
                  location.loc:=LOC_FLAGS;
               end;

             in_ofs_x,
             in_seg_x :
               left.set_varstate(false);
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
                  left.set_varstate(true);
                  if not is_ordinal(resulttype) then
                    CGMessage(type_e_ordinal_expr_expected)
                  else
                    begin
                      if (resulttype^.deftype=enumdef) and
                         (penumdef(resulttype)^.has_jumps) then
                        CGMessage(type_e_succ_and_pred_enums_with_assign_not_possible)
                      else
                        if left.nodetype=ordconstn then
                         begin
                           if inlinenumber=in_succ_x then
                             hp:=genordinalconstnode(tordconstnode(left).value+1,left.resulttype)
                           else
                             hp:=genordinalconstnode(tordconstnode(left).value-1,left.resulttype);
                           firstpass(hp);
                           pass_1:=hp;
                         end;
                    end;
               end;

             in_inc_x,
             in_dec_x:
               begin
                 resulttype:=voiddef;
                 if assigned(left) then
                   begin
                      tcallparanode(left).firstcallparan(nil,true);
                      left.set_varstate(true);
                      if codegenerror then
                       exit;
                      { first param must be var }
                      valid_for_assign(tcallparanode(left).left,false);
                      { check type }
                      if (left.resulttype^.deftype in [enumdef,pointerdef]) or
                         is_ordinal(left.resulttype) then
                        begin
                           { two paras ? }
                           if assigned(tcallparanode(left).right) then
                             begin
                                { insert a type conversion       }
                                { the second param is always longint }
                                tcallparanode(tcallparanode(left).right).left:=
                                  gentypeconvnode(tcallparanode(tcallparanode(left).right).left,s32bitdef);
                                { check the type conversion }
                                firstpass(tcallparanode(tcallparanode(left).right).left);

                                { need we an additional register ? }
                                if not(is_constintnode(tcallparanode(tcallparanode(left).right).left)) and
                                  (tcallparanode(tcallparanode(left).right).left.location.loc in [LOC_MEM,LOC_REFERENCE]) and
                                  (tcallparanode(tcallparanode(left).right).left.registers32<=1) then
                                  inc(registers32);

                                { do we need an additional register to restore the first parameter? }
                                if tcallparanode(tcallparanode(left).right).left.registers32>=registers32 then
                                  inc(registers32);

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
                       tcallparanode(left).firstcallparan(nil,true);
                       left.set_varstate(dowrite);
                       { now we can check }
                       hp:=left;
                       while assigned(tcallparanode(hp).right) do
                         hp:=tcallparanode(hp).right;
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
                                 if (tcallparanode(hpp).left.nodetype=typen) then
                                   CGMessage(type_e_cant_read_write_type);
                                 if not is_equal(hpp.resulttype,pfiledef(hp.resulttype)^.typedfiletype.def) then
                                   CGMessage(type_e_mismatch);
                                 { generate the high() value for the shortstring }
                                 if ((not dowrite) and is_shortstring(tcallparanode(hpp).left.resulttype)) or
                                    (is_chararray(tcallparanode(hpp).left.resulttype)) then
                                   tcallparanode(hpp).gen_high_tree(true);
                                 { read(ln) is call by reference (JM) }
                                 if not dowrite then
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
                                incrementregisterpushed($ff);
                                if (tcallparanode(hp).left.nodetype=typen) then
                                  CGMessage(type_e_cant_read_write_type);
                                if assigned(tcallparanode(hp).left.resulttype) then
                                  begin
                                    isreal:=false;
                                    { support writeln(procvar) }
                                    if (tcallparanode(hp).left.resulttype^.deftype=procvardef) then
                                     begin
                                       p1:=gencallnode(nil,nil);
                                       tcallnode(p1).right:=tcallparanode(hp).left;
                                       p1.resulttype:=pprocvardef(tcallparanode(hp).left.resulttype)^.rettype.def;
                                       firstpass(p1);
                                       tcallparanode(hp).left:=p1;
                                     end;
                                    case tcallparanode(hp).left.resulttype^.deftype of
                                      filedef :
                                        begin
                                          { only allowed as first parameter }
                                          if assigned(tcallparanode(hp).right) then
                                            CGMessage(type_e_cant_read_write_type);
                                        end;
                                      stringdef :
                                        begin
                                          { generate the high() value for the shortstring }
                                          if (not dowrite) and
                                             is_shortstring(tcallparanode(hp).left.resulttype) then
                                            tcallparanode(hp).gen_high_tree(true);
                                        end;
                                      pointerdef :
                                        begin
                                          if not is_pchar(tcallparanode(hp).left.resulttype) then
                                            CGMessage(type_e_cant_read_write_type);
                                        end;
                                      floatdef :
                                        begin
                                          isreal:=true;
                                        end;
                                      orddef :
                                        begin
                                          case porddef(tcallparanode(hp).left.resulttype)^.typ of
                                            uchar,
                                            u32bit,s32bit,
                                            u64bit,s64bit:
                                              ;
                                            u8bit,s8bit,
                                            u16bit,s16bit :
                                              if dowrite then
                                                tcallparanode(hp).left:=gentypeconvnode(tcallparanode(hp).left,s32bitdef);
                                            bool8bit,
                                            bool16bit,
                                            bool32bit :
                                              if dowrite then
                                                tcallparanode(hp).left:=gentypeconvnode(tcallparanode(hp).left,booldef)
                                              else
                                                CGMessage(type_e_cant_read_write_type);
                                            else
                                              CGMessage(type_e_cant_read_write_type);
                                          end;
                                          if not(dowrite) and
                                            not(is_64bitint(tcallparanode(hp).left.resulttype)) then
                                            extra_register:=true;
                                        end;
                                      arraydef :
                                        begin
                                          if is_chararray(tcallparanode(hp).left.resulttype) then
                                            tcallparanode(hp).gen_high_tree(true)
                                          else
                                            CGMessage(type_e_cant_read_write_type);
                                        end;
                                      else
                                        CGMessage(type_e_cant_read_write_type);
                                    end;

                                    { some format options ? }
                                    if nf_is_colon_para in hp.flags then
                                      begin
                                         if nf_is_colon_para in tcallparanode(hp).right.flags then
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
                                         if assigned(tcallparanode(hpp).left.resulttype) then
                                           isreal:=(tcallparanode(hpp).left.resulttype^.deftype=floatdef)
                                         else exit;
                                         if (not is_integer(tcallparanode(length_para).left.resulttype)) then
                                          CGMessage1(type_e_integer_expr_expected,tcallparanode(length_para).left.resulttype^.typename)
                                        else
                                          tcallparanode(length_para).left:=gentypeconvnode(tcallparanode(length_para).left,s32bitdef);
                                        if assigned(frac_para) then
                                          begin
                                            if isreal then
                                             begin
                                               if (not is_integer(tcallparanode(frac_para).left.resulttype)) then
                                                 CGMessage1(type_e_integer_expr_expected,tcallparanode(frac_para).left.resulttype^.typename)
                                               else
                                                 tcallparanode(frac_para).left:=gentypeconvnode(tcallparanode(frac_para).left,s32bitdef);
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
                       left.set_varstate(true);
                       { calc registers }
                       left_right_max(self);
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
                 tcallnode(hp).left:=gencallparanode(
                   genordinalconstnode(tcallparanode(left).left.resulttype^.size,s32bitdef),left);
                 left:=nil;
                 firstpass(hp);
                 pass_1:=hp;
              end;

             { the firstpass of the arg has been done in firstcalln ? }
             in_reset_typedfile,
             in_rewrite_typedfile :
               begin
                  procinfo^.flags:=procinfo^.flags or pi_do_call;
                  firstpass(left);
                  left.set_varstate(true);
                  resulttype:=voiddef;
               end;

             in_str_x_string :
               begin
                  procinfo^.flags:=procinfo^.flags or pi_do_call;
                  resulttype:=voiddef;
                  { check the amount of parameters }
                  if not(assigned(left)) or
                     not(assigned(tcallparanode(left).right)) then
                   begin
                     CGMessage(parser_e_wrong_parameter_size);
                     exit;
                   end;
                  { first pass just the string for first local use }
                  hp:=tcallparanode(left).right;
                  tcallparanode(left).right:=nil;
                  tcallparanode(left).firstcallparan(nil,true);
                  left.set_varstate(false);
                  { remove warning when result is passed }
                  set_funcret_is_valid(tcallparanode(left).left);
                  tcallparanode(left).right:=hp;
                  tcallparanode(tcallparanode(left).right).firstcallparan(nil,true);
                  tcallparanode(left).right.set_varstate(true);
                  hp:=left;
                  { valid string ? }
                  if not assigned(hp) or
                     (tcallparanode(hp).left.resulttype^.deftype<>stringdef) or
                     (tcallparanode(hp).right=nil) then
                    CGMessage(cg_e_illegal_expression);
                  { we need a var parameter }
                  valid_for_assign(tcallparanode(hp).left,false);
                  { generate the high() value for the shortstring }
                  if is_shortstring(tcallparanode(hp).left.resulttype) then
                    tcallparanode(hp).gen_high_tree(true);

                  { !!!! check length of string }

                  while assigned(tcallparanode(hp).right) do
                    hp:=tcallparanode(hp).right;

                  if not assigned(tcallparanode(hp).resulttype) then
                    exit;
                  { check and convert the first param }
                  if (nf_is_colon_para in hp.flags) or
                     not assigned(hp.resulttype) then
                    CGMessage(cg_e_illegal_expression);

                  isreal:=false;
                  case hp.resulttype^.deftype of
                    orddef :
                      begin
                        case porddef(tcallparanode(hp).left.resulttype)^.typ of
                          u32bit,s32bit,
                          s64bit,u64bit:
                            ;
                          u8bit,s8bit,
                          u16bit,s16bit:
                            tcallparanode(hp).left:=gentypeconvnode(tcallparanode(hp).left,s32bitdef);
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
                  if assigned(hpp) and (nf_is_colon_para in hpp.flags) then
                    begin
                      firstpass(tcallparanode(hpp).left);
                      tcallparanode(hpp).left.set_varstate(true);
                      if (not is_integer(tcallparanode(hpp).left.resulttype)) then
                        CGMessage1(type_e_integer_expr_expected,tcallparanode(hpp).left.resulttype^.typename)
                      else
                        tcallparanode(hpp).left:=gentypeconvnode(tcallparanode(hpp).left,s32bitdef);
                      hpp:=tcallparanode(hpp).right;
                      if assigned(hpp) and (nf_is_colon_para in hpp.flags) then
                        begin
                          if isreal then
                           begin
                             if (not is_integer(tcallparanode(hpp).left.resulttype)) then
                               CGMessage1(type_e_integer_expr_expected,tcallparanode(hpp).left.resulttype^.typename)
                             else
                               begin
                                 firstpass(tcallparanode(hpp).left);
                                 tcallparanode(hpp).left.set_varstate(true);
                                 tcallparanode(hpp).left:=gentypeconvnode(tcallparanode(hpp).left,s32bitdef);
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
                  left_right_max(self);
               end;

             in_val_x :
               begin
                  procinfo^.flags:=procinfo^.flags or pi_do_call;
                  resulttype:=voiddef;
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
                       tcallparanode(left).set_varstate(false);
                       if codegenerror then exit;
                       tcallparanode(left).right := hp;
                     {code has to be a var parameter}
                       if valid_for_assign(tcallparanode(left).left,false) then
                        begin
                          if (tcallparanode(left).left.resulttype^.deftype <> orddef) or
                            not(porddef(tcallparanode(left).left.resulttype)^.typ in
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
                  hpp.set_varstate(false);

                  if codegenerror then
                    exit;
                  { remove warning when result is passed }
                  set_funcret_is_valid(tcallparanode(hpp).left);
                  tcallparanode(hpp).right := hp;
                  if valid_for_assign(tcallparanode(hpp).left,false) then
                   begin
                     If Not((tcallparanode(hpp).left.resulttype^.deftype = floatdef) or
                            ((tcallparanode(hpp).left.resulttype^.deftype = orddef) And
                             (POrdDef(tcallparanode(hpp).left.resulttype)^.typ in
                              [u32bit,s32bit,
                               u8bit,s8bit,u16bit,s16bit,s64bit,u64bit]))) Then
                       CGMessage(type_e_mismatch);
                   end;
                 {hp = source (String)}
                  { count_ref := false; WHY ?? }
                  tcallparanode(hp).firstcallparan(nil,true);
                  hp.set_varstate(true);
                  if codegenerror then
                    exit;
                  { if not a stringdef then insert a type conv which
                    does the other type checking }
                  If (tcallparanode(hp).left.resulttype^.deftype<>stringdef) then
                   begin
                     tcallparanode(hp).left:=gentypeconvnode(tcallparanode(hp).left,cshortstringdef);
                     firstpass(hp);
                   end;
                  { calc registers }
                  left_right_max(self);

                  { val doesn't calculate the registers really }
                  { correct, we need one register extra   (FK) }
                  if is_64bitint(tcallparanode(hpp).left.resulttype) then
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
                      tcallparanode(left).firstcallparan(nil,true);
                      left.set_varstate(true);
                      registers32:=left.registers32;
                      registersfpu:=left.registersfpu;
{$ifdef SUPPORT_MMX}
                      registersmmx:=left.registersmmx;
{$endif SUPPORT_MMX}
                      { remove warning when result is passed }
                      set_funcret_is_valid(tcallparanode(left).left);
                      { first param must be var }
                      valid_for_assign(tcallparanode(left).left,false);
                      { check type }
                      if assigned(left.resulttype) and
                         (left.resulttype^.deftype=setdef) then
                        begin
                           { two paras ? }
                           if assigned(tcallparanode(left).right) then
                             begin
                                { insert a type conversion       }
                                { to the type of the set elements  }
                                tcallparanode(tcallparanode(left).right).left:=gentypeconvnode(
                                  tcallparanode(tcallparanode(left).right).left,
                                  psetdef(left.resulttype)^.elementtype.def);
                                { check the type conversion }
                                firstpass(tcallparanode(tcallparanode(left).right).left);
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
                  left.set_varstate(false);
                  { this fixes tests\webtbs\tbug879.pp (FK)
                  if left.nodetype in [typen,loadn,subscriptn] then
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
                                 firstpass(hp);
                                 pass_1:=hp;
                               end
                              else
                               begin
                                 if is_open_array(left.resulttype) or
                                   is_array_of_const(left.resulttype) then
                                  begin
                                    getsymonlyin(tloadnode(left).symtable,'high'+pvarsym(tloadnode(left).symtableentry)^.name);
                                    hp:=genloadnode(pvarsym(srsym),tloadnode(left).symtable);
                                    firstpass(hp);
                                    pass_1:=hp;
                                  end
                                 else
                                  begin
                                    hp:=genordinalconstnode(Parraydef(left.resulttype)^.highrange,
                                      Parraydef(left.resulttype)^.rangetype.def);
                                    firstpass(hp);
                                    pass_1:=hp;
                                  end;
                               end;
                           end;
                         stringdef:
                           begin
                              if inlinenumber=in_low_x then
                               begin
                                 hp:=genordinalconstnode(0,u8bitdef);
                                 firstpass(hp);
                                 pass_1:=hp;
                               end
                              else
                               begin
                                 if is_open_string(left.resulttype) then
                                  begin
                                    getsymonlyin(tloadnode(left).symtable,'high'+pvarsym(tloadnode(left).symtableentry)^.name);
                                    hp:=genloadnode(pvarsym(srsym),tloadnode(left).symtable);
                                    firstpass(hp);
                                    pass_1:=hp;
                                  end
                                 else
                                  begin
                                    hp:=genordinalconstnode(Pstringdef(left.resulttype)^.len,u8bitdef);
                                    firstpass(hp);
                                    pass_1:=hp;
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
                  if left.nodetype in [ordconstn,realconstn] then
                    setconstrealvalue(cos(getconstrealvalue))
                  else
                    handleextendedfunction;
               end;

             in_sin_extended:
               begin
                  if left.nodetype in [ordconstn,realconstn] then
                    setconstrealvalue(sin(getconstrealvalue))
                  else
                    handleextendedfunction;
               end;

             in_arctan_extended:
               begin
                  if left.nodetype in [ordconstn,realconstn] then
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
                  if left.nodetype in [ordconstn,realconstn] then
                    setconstrealvalue(abs(getconstrealvalue))
                  else
                    handleextendedfunction;
               end;

             in_sqr_extended:
               begin
                  if left.nodetype in [ordconstn,realconstn] then
                    setconstrealvalue(sqr(getconstrealvalue))
                  else
                    handleextendedfunction;
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
                    handleextendedfunction;
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
                      tcallparanode(left).firstcallparan(nil,true);
                      left.set_varstate(true);
                      registers32:=left.registers32;
                      registersfpu:=left.registersfpu;
{$ifdef SUPPORT_MMX}
                      registersmmx:=left.registersmmx;
{$endif SUPPORT_MMX}
                      { check type }
                      if is_boolean(left.resulttype) then
                        begin
                           { must always be a string }
                           tcallparanode(tcallparanode(left).right).left:=
                             gentypeconvnode(tcallparanode(tcallparanode(left).right).left,cshortstringdef);
                           firstpass(tcallparanode(tcallparanode(left).right).left);
                        end
                      else
                        CGMessage(type_e_mismatch);
                   end
                 else
                   CGMessage(type_e_mismatch);
                 { We've checked the whole statement for correctness, now we
                   can remove it if assertions are off }
                 if not(cs_do_assertion in aktlocalswitches) then
                   { we need a valid node, so insert a nothingn }
                   pass_1:=cnothingnode.create;
               end;

              else
               internalerror(8);
             end;
            end;
           { generate an error if no resulttype is set }
           if not assigned(resulttype) then
             resulttype:=generrordef;
           { ... also if the node will be replaced }
           if not assigned(pass_1.resulttype) then
             pass_1.resulttype:=generrordef;
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
  Revision 1.4  2000-09-28 16:34:47  florian
  *** empty log message ***

  Revision 1.3  2000/09/27 21:33:22  florian
    * finally nadd.pas compiles

  Revision 1.2  2000/09/27 20:25:44  florian
    * more stuff fixed

  Revision 1.1  2000/09/26 14:59:34  florian
    * more conversion work done

}