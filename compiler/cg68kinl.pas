{
    $Id$
    Copyright (c) 1998-2000 by Florian Klaempfl

    Generate m68k inline nodes

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
unit cg68kinl;
interface

    uses
      tree;

    procedure secondinline(var p : ptree);


implementation

    uses
      globtype,systems,symconst,
      cobjects,verbose,globals,
      aasm,types,symtable,
      hcodegen,temp_gen,pass_2,
      cpubase,cga68k,tgen68k,cg68kld,cg68kcal;


{*****************************************************************************
                                Helpers
*****************************************************************************}

    { reverts the parameter list }
    var nb_para : integer;

    function reversparameter(p : ptree) : ptree;

       var
         hp1,hp2 : ptree;

      begin
         hp1:=nil;
         nb_para := 0;
         while assigned(p) do
           begin
              { pull out }
              hp2:=p;
              p:=p^.right;
              inc(nb_para);
              { pull in }
              hp2^.right:=hp1;
              hp1:=hp2;
           end;
         reversparameter:=hp1;
       end;


{*****************************************************************************
                             SecondInLine
*****************************************************************************}

    procedure secondinline(var p : ptree);
       const
         { tfloattype = (f32bit,s32real,s64real,s80real,s64bit); }
         float_name: array[tfloattype] of string[8]=
           ('FIXED','SINGLE','REAL','EXTENDED','COMP','FIXED16');
         addqconstsubop:array[in_inc_x..in_dec_x] of tasmop=(A_ADDQ,A_SUBQ);
         addconstsubop:array[in_inc_x..in_dec_x] of tasmop=(A_ADD,A_SUB);
         addsubop:array[in_inc_x..in_dec_x] of tasmop=(A_ADD,A_SUB);
       var
         aktfile : treference;
         ft : tfiletype;
         opsize : topsize;
         asmop : tasmop;
         pushed : tpushed;
         {inc/dec}
         addconstant : boolean;
         addvalue : longint;


      procedure handlereadwrite(doread,doln : boolean);
      { produces code for READ(LN) and WRITE(LN) }

        procedure loadstream;
          const
            io:array[0..1] of string[7]=('_OUTPUT','_INPUT');
          var
            r : preference;
          begin
            new(r);
            reset_reference(r^);
            r^.symbol:=stringdup(
            'U_'+upper(target_info.system_unit)+io[byte(doread)]);
            exprasmlist^.concat(new(paicpu,op_ref_reg(A_LEA,S_L,r,R_A0)))
          end;

        var
           node,hp    : ptree;
           typedtyp,
           pararesult : pdef;
           has_length : boolean;
           dummycoll  : tdefcoll;
           iolabel    : pasmlabel;
           npara      : longint;

        begin
           { I/O check }
           if (cs_check_io in aktlocalswitches) and
              not(po_iocheck in aktprocsym^.definition^.procoptions) then
             begin
                getlabel(iolabel);
                emitl(A_LABEL,iolabel);
             end
           else
             iolabel:=nil;
           { for write of real with the length specified }
           has_length:=false;
           hp:=nil;
           { reserve temporary pointer to data variable }
           aktfile.symbol:=nil;
           gettempofsizereference(4,aktfile);
           { first state text data }
           ft:=ft_text;
           { and state a parameter ? }
           if p^.left=nil then
             begin
                { the following instructions are for "writeln;" }
                loadstream;
                { save @aktfile in temporary variable }
                exprasmlist^.concat(new(paicpu,op_reg_ref(A_MOVE,S_L,R_A0,newreference(aktfile))));
             end
           else
             begin
                { revers paramters }
                node:=reversparameter(p^.left);

                p^.left := node;
                npara := nb_para;
                { calculate data variable }
                { is first parameter a file type ? }
                if node^.left^.resulttype^.deftype=filedef then
                  begin
                     ft:=pfiledef(node^.left^.resulttype)^.filetype;
                     if ft=ft_typed then
                       typedtyp:=pfiledef(node^.left^.resulttype)^.typed_as;
                     secondpass(node^.left);
                     if codegenerror then
                       exit;

                     { save reference in temporary variables }
                     if node^.left^.location.loc<>LOC_REFERENCE then
                       begin
                          CGMessage(cg_e_illegal_expression);
                          exit;
                       end;

                     exprasmlist^.concat(new(paicpu,op_ref_reg(A_LEA,S_L,newreference(node^.left^.location.reference),R_A0)));

                     { skip to the next parameter }
                     node:=node^.right;
                  end
                else
                  begin
                  { load stdin/stdout stream }
                     loadstream;
                  end;

                { save @aktfile in temporary variable }
                exprasmlist^.concat(new(paicpu,op_reg_ref(A_MOVE,S_L,R_A0,newreference(aktfile))));
                if doread then
                { parameter by READ gives call by reference }
                  dummycoll.paratyp:=vs_var
                { an WRITE Call by "Const" }
                else
                  dummycoll.paratyp:=vs_const;

                { because of secondcallparan, which otherwise attaches }
                if ft=ft_typed then
                  { this is to avoid copy of simple const parameters }
                  {dummycoll.data:=new(pformaldef,init)}
                  dummycoll.data:=cformaldef
                else
                  { I think, this isn't a good solution (FK) }
                  dummycoll.data:=nil;

                while assigned(node) do
                  begin
                     pushusedregisters(pushed,$ff);
                     hp:=node;
                     node:=node^.right;
                     hp^.right:=nil;
                     if hp^.is_colon_para then
                       CGMessage(parser_e_illegal_colon_qualifier);
                     if ft=ft_typed then
                       never_copy_const_param:=true;
                     secondcallparan(hp,@dummycoll,false);
                     if ft=ft_typed then
                       never_copy_const_param:=false;
                     hp^.right:=node;
                     if codegenerror then
                       exit;

                     emit_push_mem(aktfile);
                     if (ft=ft_typed) then
                       begin
                          { OK let's try this }
                          { first we must only allow the right type }
                          { we have to call blockread or blockwrite }
                          { but the real problem is that            }
                          { reset and rewrite should have set       }
                          { the type size                           }
                          { as recordsize for that file !!!!        }
                          { how can we make that                    }
                          { I think that is only possible by adding }
                          { reset and rewrite to the inline list a call        }
                          { allways read only one record by element }
                            push_int(typedtyp^.size);
                            if doread then
                              emitcall('FPC_TYPED_READ',true)
                            else
                              emitcall('FPC_TYPED_WRITE',true);
                       end
                     else
                       begin
                          { save current position }
                          pararesult:=hp^.left^.resulttype;
                          { handle possible field width  }
                          { of course only for write(ln) }
                          if not doread then
                            begin
                               { handle total width parameter }
                              if assigned(node) and node^.is_colon_para then
                                begin
                                   hp:=node;
                                   node:=node^.right;
                                   hp^.right:=nil;
                                   secondcallparan(hp,@dummycoll,false);
                                   hp^.right:=node;
                                   if codegenerror then
                                     exit;
                                   has_length:=true;
                                end
                              else
                                if pararesult^.deftype<>floatdef then
                                  push_int(0)
                                else
                                  push_int(-32767);
                            { a second colon para for a float ? }
                              if assigned(node) and node^.is_colon_para then
                                begin
                                   hp:=node;
                                   node:=node^.right;
                                   hp^.right:=nil;
                                   secondcallparan(hp,@dummycoll,false);
                                   hp^.right:=node;
                                   if pararesult^.deftype<>floatdef then
                                     CGMessage(parser_e_illegal_colon_qualifier);
                                   if codegenerror then
                                     exit;
                                end
                              else
                                begin
                                  if pararesult^.deftype=floatdef then
                                    push_int(-1);
                                end
                            end;
                          case pararesult^.deftype of
                       stringdef : begin
                                     if doread then
                                       begin
                                       { push maximum string length }
                                       push_int(pstringdef(pararesult)^.len);
                                       case pstringdef(pararesult)^.string_typ of
                                        st_shortstring:
                                          emitcall ('FPC_READ_TEXT_STRING',true);
                                        st_ansistring:
                                          emitcall ('FPC_READ_TEXT_ANSISTRING',true);
                                        st_longstring:
                                          emitcall ('FPC_READ_TEXT_LONGSTRING',true);
                                        st_widestring:
                                          emitcall ('FPC_READ_TEXT_ANSISTRING',true);
                                        end
                                       end
                                     else
                                       Case pstringdef(Pararesult)^.string_typ of
                                        st_shortstring:
                                          emitcall ('FPC_WRITE_TEXT_STRING',true);
                                        st_ansistring:
                                          emitcall ('FPC_WRITE_TEXT_ANSISTRING',true);
                                        st_longstring:
                                          emitcall ('FPC_WRITE_TEXT_LONGSTRING',true);
                                        st_widestring:
                                          emitcall ('FPC_WRITE_TEXT_ANSISTRING',true);
                                        end;
                                   end;
                      pointerdef : begin
                                     if is_equal(ppointerdef(pararesult)^.definition,cchardef) then
                                       begin
                                         if doread then
                                           emitcall('FPC_READ_TEXT_PCHAR_AS_POINTER',true)
                                         else
                                           emitcall('FPC_WRITE_TEXT_PCHAR_AS_POINTER',true);
                                       end;
                                   end;
                        arraydef : begin
                                     if (parraydef(pararesult)^.lowrange=0) and
                                        is_equal(parraydef(pararesult)^.definition,cchardef) then
                                       begin
                                         if doread then
                                           emitcall('FPC_READ_TEXT_PCHAR_AS_ARRAY',true)
                                         else
                                           emitcall('FPC_WRITE_TEXT_PCHAR_AS_ARRAY',true);
                                       end;
                                   end;
                        floatdef : begin
                                     if doread then
                                       emitcall('FPC_READ_TEXT_'+float_name[pfloatdef(pararesult)^.typ],true)
                                     else
                                       emitcall('FPC_WRITE_TEXT_'+float_name[pfloatdef(pararesult)^.typ],true);
                                   end;
                          orddef : begin
                                     case porddef(pararesult)^.typ of
                                          u8bit : if doread then
                                                    emitcall('FPC_READ_TEXT_BYTE',true);
                                          s8bit : if doread then
                                                    emitcall('FPC_READ_TEXT_SHORTINT',true);
                                         u16bit : if doread then
                                                    emitcall('FPC_READ_TEXT_WORD',true);
                                         s16bit : if doread then
                                                    emitcall('FPC_READ_TEXT_INTEGER',true);
                                         s32bit : if doread then
                                                    emitcall('FPC_READ_TEXT_LONGINT',true)
                                                  else
                                                    emitcall('FPC_WRITE_TEXT_LONGINT',true);
                                         u32bit : if doread then
                                                    emitcall('FPC_READ_TEXT_CARDINAL',true)
                                                  else
                                                    emitcall('FPC_WRITE_TEXT_CARDINAL',true);
                                          uchar : if doread then
                                                    emitcall('FPC_READ_TEXT_CHAR',true)
                                                  else
                                                    emitcall('FPC_WRITE_TEXT_CHAR',true);
                                       bool8bit,
                                      bool16bit,
                                      bool32bit : if  doread then
                                                    CGMessage(parser_e_illegal_parameter_list)
                                                  else
                                                    emitcall('FPC_WRITE_TEXT_BOOLEAN',true);
                                     end;
                                   end;
                          end;
                       end;
                   { load ESI in methods again }
                     popusedregisters(pushed);
                     maybe_loada5;
                  end;
             end;
         { Insert end of writing for textfiles }
           if ft=ft_text then
             begin
               pushusedregisters(pushed,$ff);
               emit_push_mem(aktfile);
               if doread then
                begin
                  if doln then
                    emitcall('FPC_READLN_END',true)
                  else
                    emitcall('FPC_READ_END',true);
                end
               else
                begin
                  if doln then
                    emitcall('FPC_WRITELN_END',true)
                  else
                    emitcall('FPC_WRITE_END',true);
                end;
               popusedregisters(pushed);
               maybe_loada5;
             end;
         { Insert IOCheck if set }
           if assigned(iolabel) then
             begin
                { registers are saved in the procedure }
                exprasmlist^.concat(new(paicpu,op_csymbol(A_PEA,S_L,newcsymbol(iolabel^.name,0))));
                emitcall('FPC_IOCHECK',true);
             end;
         { Freeup all used temps }
           ungetiftemp(aktfile);
           if assigned(p^.left) then
             begin
                p^.left:=reversparameter(p^.left);
                if npara<>nb_para then
                  CGMessage(cg_f_internal_error_in_secondinline);
                hp:=p^.left;
                while assigned(hp) do
                  begin
                     if assigned(hp^.left) then
                       if (hp^.left^.location.loc in [LOC_MEM,LOC_REFERENCE]) then
                         ungetiftemp(hp^.left^.location.reference);
                     hp:=hp^.right;
                  end;
             end;
        end;

      procedure handle_str;

        var
           hp,node : ptree;
           dummycoll : tdefcoll;
           is_real,has_length : boolean;

          begin
           pushusedregisters(pushed,$ff);
           node:=p^.left;
           is_real:=false;
           has_length:=false;
           while assigned(node^.right) do node:=node^.right;
           { if a real parameter somewhere then call REALSTR }
           if (node^.left^.resulttype^.deftype=floatdef) then
             is_real:=true;

           node:=p^.left;
           { we have at least two args }
           { with at max 2 colon_para in between }

           { first arg longint or float }
           hp:=node;
           node:=node^.right;
           hp^.right:=nil;
           dummycoll.data:=hp^.resulttype;
           { string arg }

           dummycoll.paratyp:=vs_var;
           secondcallparan(hp,@dummycoll,false);
           if codegenerror then
             exit;

           dummycoll.paratyp:=vs_const;
           disposetree(hp);
           p^.left:=nil;

           { second arg }
           hp:=node;
           node:=node^.right;
           hp^.right:=nil;
           { frac  para }
           if hp^.is_colon_para and assigned(node) and
              node^.is_colon_para then
             begin
                dummycoll.data:=hp^.resulttype;
                secondcallparan(hp,@dummycoll,false);
                if codegenerror then
                  exit;
                disposetree(hp);
                hp:=node;
                node:=node^.right;
                hp^.right:=nil;
                has_length:=true;
             end
           else
             if is_real then
             push_int(-1);

           { third arg, length only if is_real }
           if hp^.is_colon_para then
             begin
                dummycoll.data:=hp^.resulttype;
                secondcallparan(hp,@dummycoll,false);
                if codegenerror then
                  exit;
                disposetree(hp);
                hp:=node;
                node:=node^.right;
                hp^.right:=nil;
             end
           else
             if is_real then
               push_int(-32767)
             else
               push_int(-1);

           { last arg longint or real }
           secondcallparan(hp,@dummycoll,false);
           if codegenerror then
             exit;

           disposetree(hp);

           if is_real then
             emitcall('FPC_STR_'+float_name[pfloatdef(hp^.resulttype)^.typ],true)
           else if porddef(hp^.resulttype)^.typ=u32bit then
             emitcall('FPC_STR_CARDINAL',true)
           else
             emitcall('FPC_STR_LONGINT',true);
           popusedregisters(pushed);
        end;

      var
         r : preference;
         l : longint;
         ispushed : boolean;
         hregister : tregister;
         otlabel,oflabel,filenamestring : pasmlabel;
         oldpushedparasize : longint;
      begin
      { save & reset pushedparasize }
         oldpushedparasize:=pushedparasize;
         pushedparasize:=0;
         case p^.inlinenumber of
            in_assert_x_y:
              begin
               { !!!!!!!!! }
              end;
            in_lo_word,
            in_hi_word :
              begin
                       secondpass(p^.left);
                       p^.location.loc:=LOC_REGISTER;
                       if p^.left^.location.loc<>LOC_REGISTER then
                         begin
                            if p^.left^.location.loc=LOC_CREGISTER then
                              begin
                                 p^.location.register:=getregister32;
                                 emit_reg_reg(A_MOVE,S_W,p^.left^.location.register,
                                   p^.location.register);
                              end
                            else
                              begin
                                 del_reference(p^.left^.location.reference);
                                 p^.location.register:=getregister32;
                                 exprasmlist^.concat(new(paicpu,op_ref_reg(A_MOVE,S_W,
                                  newreference(p^.left^.location.reference),
                                  p^.location.register)));
                              end;
                         end
                       else p^.location.register:=p^.left^.location.register;
                       if p^.inlinenumber=in_hi_word then
                         exprasmlist^.concat(new(paicpu,op_const_reg(A_LSR,S_W,8,p^.location.register)));
                       p^.location.register:=p^.location.register;
              end;
            in_high_x :
              begin
                 if is_open_array(p^.left^.resulttype) then
                   begin
                      secondpass(p^.left);
                      del_reference(p^.left^.location.reference);
                      p^.location.register:=getregister32;
                      new(r);
                      reset_reference(r^);
                      r^.base:=highframepointer;
                      r^.offset:=highoffset+4;
                      exprasmlist^.concat(new(paicpu,op_ref_reg(A_MOVE,S_L,
                        r,p^.location.register)));
                   end
              end;
            in_sizeof_x,
            in_typeof_x :
              begin
               { sizeof(openarray) handling }
                 if (p^.inlinenumber=in_sizeof_x) and
                    is_open_array(p^.left^.resulttype) then
                  begin
                  { sizeof(openarray)=high(openarray)+1 }
                    secondpass(p^.left);
                    del_reference(p^.left^.location.reference);
                    p^.location.register:=getregister32;
                    new(r);
                    reset_reference(r^);
                    r^.base:=highframepointer;
                    r^.offset:=highoffset+4;
                    exprasmlist^.concat(new(paicpu,op_ref_reg(A_MOVE,S_L,
                      r,p^.location.register)));
                    exprasmlist^.concat(new(paicpu,op_const_reg(A_ADD,S_L,
                      1,p^.location.register)));
                    if parraydef(p^.left^.resulttype)^.elesize<>1 then
                      exprasmlist^.concat(new(paicpu,op_const_reg(A_MULS,S_L,
                        parraydef(p^.left^.resulttype)^.elesize,p^.location.register)));
                  end
                 else
                  begin
                    { for both cases load vmt }
                    if p^.left^.treetype=typen then
                      begin
                        exprasmlist^.concat(new(paicpu,op_csymbol_reg(A_LEA,
                          S_L,newcsymbol(pobjectdef(p^.left^.resulttype)^.vmt_mangledname,0),
                          R_A0)));
                        p^.location.register:=getregister32;
                        emit_reg_reg(A_MOVE,S_L,R_A0,p^.location.register);
                      end
                    else
                      begin
                        secondpass(p^.left);
                        del_reference(p^.left^.location.reference);
                        p^.location.loc:=LOC_REGISTER;
                        p^.location.register:=getregister32;
                        { load VMT pointer }
                        inc(p^.left^.location.reference.offset,
                          pobjectdef(p^.left^.resulttype)^.vmt_offset);
                        exprasmlist^.concat(new(paicpu,op_ref_reg(A_MOVE,S_L,
                          newreference(p^.left^.location.reference),
                          p^.location.register)));
                      end;
                    { in sizeof load size }
                    if p^.inlinenumber=in_sizeof_x then
                      begin
                         new(r);
                         reset_reference(r^);
                        { load the address in A0 }
                        { because now supposedly p^.location.register is an }
                        { address.                                          }
                        emit_reg_reg(A_MOVE, S_L, p^.location.register, R_A0);
                        r^.base:=R_A0;
                        exprasmlist^.concat(new(paicpu,op_ref_reg(A_MOVE,S_L,r,
                          p^.location.register)));
                      end;
                  end;
              end;
            in_lo_long,
            in_hi_long : begin
                       secondpass(p^.left);
                       p^.location.loc:=LOC_REGISTER;
                       if p^.left^.location.loc<>LOC_REGISTER then
                         begin
                            if p^.left^.location.loc=LOC_CREGISTER then
                              begin
                                 p^.location.register:=getregister32;
                                 emit_reg_reg(A_MOVE,S_L,p^.left^.location.register,
                                   p^.location.register);
                              end
                            else
                              begin
                                 del_reference(p^.left^.location.reference);
                                 p^.location.register:=getregister32;
                                 exprasmlist^.concat(new(paicpu,op_ref_reg(A_MOVE,S_L,
                                  newreference(p^.left^.location.reference),
                                  p^.location.register)));
                              end;
                         end
                       else p^.location.register:=p^.left^.location.register;
                       if p^.inlinenumber=in_hi_long then
                         begin
                           exprasmlist^.concat(new(paicpu,op_const_reg(A_MOVEQ, S_L, 16, R_D1)));
                           exprasmlist^.concat(new(paicpu,op_reg_reg(A_LSR,S_L,R_D1,p^.location.register)));
                         end;
                       p^.location.register:=p^.location.register;
                    end;
            in_length_string :
              begin
                 secondpass(p^.left);
                 set_location(p^.location,p^.left^.location);
                 { length in ansi strings is at offset -8 }
                 if is_ansistring(p^.left^.resulttype) then
                   dec(p^.location.reference.offset,8);
              end;
            in_pred_x,
            in_succ_x:
              begin
                 secondpass(p^.left);
                 if p^.inlinenumber=in_pred_x then
                   asmop:=A_SUB
                 else
                   asmop:=A_ADD;
                 case p^.resulttype^.size of
                   4 : opsize:=S_L;
                   2 : opsize:=S_W;
                   1 : opsize:=S_B;
                 else
                    internalerror(10080);
                 end;
                 p^.location.loc:=LOC_REGISTER;
                 if p^.left^.location.loc<>LOC_REGISTER then
                   begin
                      p^.location.register:=getregister32;
                      if p^.left^.location.loc=LOC_CREGISTER then
                        emit_reg_reg(A_MOVE,opsize,p^.left^.location.register,
                          p^.location.register)
                      else
                      if p^.left^.location.loc=LOC_FLAGS then
                        exprasmlist^.concat(new(paicpu,op_reg(flag_2_set[p^.left^.location.resflags],S_NO,
                                  p^.location.register)))
                      else
                        begin
                           del_reference(p^.left^.location.reference);
                           exprasmlist^.concat(new(paicpu,op_ref_reg(A_MOVE,opsize,newreference(p^.left^.location.reference),
                             p^.location.register)));
                        end;
                   end
                 else p^.location.register:=p^.left^.location.register;
                 exprasmlist^.concat(new(paicpu,op_const_reg(asmop,opsize,1,
                   p^.location.register)))
                 { here we should insert bounds check ? }
                 { and direct call to bounds will crash the program }
                 { if we are at the limit }
                 { we could also simply say that pred(first)=first and succ(last)=last }
                 { could this be usefull I don't think so (PM)
                 emitoverflowcheck;}
              end;
            in_dec_x,
            in_inc_x :
              begin
              { set defaults }
                addvalue:=1;
                addconstant:=true;
              { load first parameter, must be a reference }
                secondpass(p^.left^.left);
                case p^.left^.left^.resulttype^.deftype of
                  orddef,
                 enumdef : begin
                             case p^.left^.left^.resulttype^.size of
                              1 : opsize:=S_B;
                              2 : opsize:=S_W;
                              4 : opsize:=S_L;
                             end;
                           end;
              pointerdef : begin
                             opsize:=S_L;
                             addvalue:=ppointerdef(p^.left^.left^.resulttype)^.definition^.size;
                           end;
                else
                 internalerror(10081);
                end;
              { second argument specified?, must be a s32bit in register }
                if assigned(p^.left^.right) then
                 begin
                   secondpass(p^.left^.right^.left);
                 { when constant, just multiply the addvalue }
                   if is_constintnode(p^.left^.right^.left) then
                    addvalue:=addvalue*get_ordinal_value(p^.left^.right^.left)
                   else
                    begin
                      case p^.left^.right^.left^.location.loc of
                   LOC_REGISTER,
                  LOC_CREGISTER : hregister:=p^.left^.right^.left^.location.register;
                        LOC_MEM,
                  LOC_REFERENCE : begin
                                    hregister:=getregister32;
                                    exprasmlist^.concat(new(paicpu,op_ref_reg(A_MOVE,S_L,
                                      newreference(p^.left^.right^.left^.location.reference),hregister)));
                                  end;
                       else
                        internalerror(10082);
                       end;
                    { insert multiply with addvalue if its >1 }
                      if addvalue>1 then
                       exprasmlist^.concat(new(paicpu,op_const_reg(A_MULS,opsize,
                         addvalue,hregister)));
                      addconstant:=false;
                    end;
                 end;
              { write the add instruction }
                if addconstant then
                 begin
                   if (addvalue > 0) and (addvalue < 9) then
                    exprasmlist^.concat(new(paicpu,op_const_ref(addqconstsubop[p^.inlinenumber],opsize,
                      addvalue,newreference(p^.left^.left^.location.reference))))
                   else
                    exprasmlist^.concat(new(paicpu,op_const_ref(addconstsubop[p^.inlinenumber],opsize,
                      addvalue,newreference(p^.left^.left^.location.reference))));
                 end
                else
                 begin
                   exprasmlist^.concat(new(paicpu,op_reg_ref(addsubop[p^.inlinenumber],opsize,
                      hregister,newreference(p^.left^.left^.location.reference))));
                   ungetregister32(hregister);
                 end;
                emitoverflowcheck(p^.left^.left);
              end;
            in_assigned_x :
              begin
                secondpass(p^.left^.left);
                p^.location.loc:=LOC_FLAGS;
                if (p^.left^.left^.location.loc=LOC_REGISTER) or
                   (p^.left^.left^.location.loc=LOC_CREGISTER) then
                 begin
                   exprasmlist^.concat(new(paicpu,op_reg(A_TST,S_L,
                    p^.left^.left^.location.register)));
                   ungetregister32(p^.left^.left^.location.register);
                 end
                else
                 begin
                   exprasmlist^.concat(new(paicpu,op_ref(A_TST,S_L,
                   newreference(p^.left^.left^.location.reference))));
                   del_reference(p^.left^.left^.location.reference);
                 end;
                p^.location.resflags:=F_NE;
              end;
             in_reset_typedfile,in_rewrite_typedfile :
               begin
                  pushusedregisters(pushed,$ffff);
                  exprasmlist^.concat(new(paicpu,op_const_reg(A_MOVE,S_L,
                    pfiledef(p^.left^.resulttype)^.typed_as^.size,R_SPPUSH)));
                  secondload(p^.left);
                  emitpushreferenceaddr(exprasmlist,p^.left^.location.reference);
                  if p^.inlinenumber=in_reset_typedfile then
                    emitcall('FPC_RESET_TYPED',true)
                  else
                    emitcall('FPC_REWRITE_TYPED',true);
                  popusedregisters(pushed);
               end;
            in_write_x :
              handlereadwrite(false,false);
            in_writeln_x :
              handlereadwrite(false,true);
            in_read_x :
              handlereadwrite(true,false);
            in_readln_x :
              handlereadwrite(true,true);
            in_str_x_string :
              begin
                 handle_str;
                 maybe_loada5;
              end;
            in_include_x_y,
            in_exclude_x_y:
              begin
                 CGMessage(cg_e_include_not_implemented);
{ !!!!!!!  }
(*               secondpass(p^.left^.left);
                 if p^.left^.right^.left^.treetype=ordconstn then
                   begin
                      { calculate bit position }
                      l:=1 shl (p^.left^.right^.left^.value mod 32);

                      { determine operator }
                      if p^.inlinenumber=in_include_x_y then
                        asmop:=A_OR
                      else
                        begin
                           asmop:=A_AND;
                           l:=not(l);
                        end;
                      if (p^.left^.left^.location.loc=LOC_REFERENCE) then
                        begin
                           inc(p^.left^.left^.location.reference.offset,(p^.left^.right^.left^.value div 32)*4);
                           exprasmlist^.concat(new(paicpu,op_const_ref(asmop,S_L,
                             l,newreference(p^.left^.left^.location.reference))));
                           del_reference(p^.left^.left^.location.reference);
                        end
                      else
                        { LOC_CREGISTER }
                        exprasmlist^.concat(new(paicpu,op_const_reg(asmop,S_L,
                          l,p^.left^.left^.location.register)));
                   end
                 else
                   begin
                      { generate code for the element to set }
                      ispushed:=maybe_push(p^.left^.right^.left^.registers32,p^.left^.left);
                      secondpass(p^.left^.right^.left);
                      if ispushed then
                        restore(p^.left^.left);
                      { determine asm operator }
                      if p^.inlinenumber=in_include_x_y then
                        asmop:=A_BTS
                      else
                        asmop:=A_BTR;
                      if psetdef(p^.left^.resulttype)^.settype=smallset then
                        begin
                           if p^.left^.right^.left^.location.loc in [LOC_CREGISTER,LOC_REGISTER] then
                             hregister:=p^.left^.right^.left^.location.register
                           else
                             begin
                                hregister:=R_EDI;
                                exprasmlist^.concat(new(paicpu,op_ref_reg(A_MOV,S_L,
                                  newreference(p^.left^.right^.left^.location.reference),R_EDI)));
                             end;
                          if (p^.left^.left^.location.loc=LOC_REFERENCE) then
                            exprasmlist^.concat(new(paicpu,op_reg_ref(asmop,S_L,R_EDI,
                              newreference(p^.left^.right^.left^.location.reference))))
                          else
                            exprasmlist^.concat(new(paicpu,op_reg_reg(asmop,S_L,R_EDI,
                              p^.left^.right^.left^.location.register)));
                        end
                      else
                        begin
                           internalerror(10083);
                        end;
                   end;
                   *)
               end;

         else
           internalerror(9);
         end;
         pushedparasize:=oldpushedparasize;
      end;

end.
{
  $Log$
  Revision 1.2  2000-07-13 11:32:36  michael
  + removed logs

}
