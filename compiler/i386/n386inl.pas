{
    $Id$
    Copyright (c) 1998-2000 by Florian Klaempfl

    Generate i386 inline nodes

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
unit n386inl;

{$i defines.inc}

interface

    uses
       node,ninl;

    type
       ti386inlinenode = class(tinlinenode)
          procedure pass_2;override;
       end;

implementation

    uses
      globtype,systems,
      cutils,cobjects,verbose,globals,fmodule,
      symconst,symbase,symtype,symdef,symsym,aasm,types,
      hcodegen,temp_gen,pass_1,pass_2,
      cpubase,cpuasm,
      nbas,ncon,ncal,ncnv,nld,
      cgai386,tgeni386,n386util;


{*****************************************************************************
                                Helpers
*****************************************************************************}

    { reverts the parameter list }
    var nb_para : longint;

    function reversparameter(p : tnode) : tnode;

       var
         hp1,hp2 : tnode;

      begin
         hp1:=nil;
         nb_para := 0;
         while assigned(p) do
           begin
              { pull out }
              hp2:=p;
              p:=tbinarynode(p).right;
              inc(nb_para);
              { pull in }
              tbinarynode(hp2).right:=hp1;
              hp1:=hp2;
           end;
         reversparameter:=hp1;
       end;


{*****************************************************************************
                              TI386INLINENODE
*****************************************************************************}

    procedure StoreDirectFuncResult(var dest:tnode);
      var
        hp : tnode;
        hdef : porddef;
        hreg : tregister;
        hregister : tregister;
        oldregisterdef : boolean;
        op : tasmop;
        opsize : topsize;

      begin
        { Get the accumulator first so it can't be used in the dest }
        if (dest.resulttype^.deftype=orddef) and
          not(is_64bitint(dest.resulttype)) then
          hregister:=getexplicitregister32(accumulator);
        { process dest }
        SecondPass(dest);
        if Codegenerror then
         exit;
        { store the value }
        Case dest.resulttype^.deftype of
          floatdef:
            if dest.location.loc=LOC_CFPUREGISTER then
              begin
                 floatstoreops(pfloatdef(dest.resulttype)^.typ,op,opsize);
                 emit_reg(op,opsize,correct_fpuregister(dest.location.register,fpuvaroffset+1));
              end
            else
              begin
                 inc(fpuvaroffset);
                 floatstore(PFloatDef(dest.resulttype)^.typ,dest.location.reference);
                 { floatstore decrements the fpu var offset }
                 { but in fact we didn't increment it       }
              end;
          orddef:
            begin
              if is_64bitint(dest.resulttype) then
                begin
                   emit_movq_reg_loc(R_EDX,R_EAX,dest.location);
                end
              else
               begin
                 Case dest.resulttype^.size of
                  1 : hreg:=regtoreg8(hregister);
                  2 : hreg:=regtoreg16(hregister);
                  4 : hreg:=hregister;
                 End;
                 emit_mov_reg_loc(hreg,dest.location);
                 If (cs_check_range in aktlocalswitches) and
                    {no need to rangecheck longints or cardinals on 32bit processors}
                    not((porddef(dest.resulttype)^.typ = s32bit) and
                        (porddef(dest.resulttype)^.low = longint($80000000)) and
                        (porddef(dest.resulttype)^.high = $7fffffff)) and
                    not((porddef(dest.resulttype)^.typ = u32bit) and
                        (porddef(dest.resulttype)^.low = 0) and
                        (porddef(dest.resulttype)^.high = longint($ffffffff))) then
                  Begin
                    {do not register this temporary def}
                    OldRegisterDef := RegisterDef;
                    RegisterDef := False;
                    hdef:=nil;
                    Case PordDef(dest.resulttype)^.typ of
                      u8bit,u16bit,u32bit:
                        begin
                          new(hdef,init(u32bit,0,$ffffffff));
                          hreg:=hregister;
                        end;
                      s8bit,s16bit,s32bit:
                        begin
                          new(hdef,init(s32bit,$80000000,$7fffffff));
                          hreg:=hregister;
                        end;
                    end;
                    { create a fake node }
                    hp := cnothingnode.create;
                    hp.location.loc := LOC_REGISTER;
                    hp.location.register := hreg;
                    if assigned(hdef) then
                      hp.resulttype:=hdef
                    else
                      hp.resulttype:=dest.resulttype;
                    { emit the range check }
                    emitrangecheck(hp,dest.resulttype);
                    if assigned(hdef) then
                      Dispose(hdef, Done);
                    RegisterDef := OldRegisterDef;
                    hp.free;
                  End;
                 ungetregister(hregister);
               end;
            End;
          else
            internalerror(66766766);
        end;
        { free used registers }
        del_locref(dest.location);
      end;

    procedure ti386inlinenode.pass_2;
       const
         {tfloattype = (s32real,s64real,s80real,s64bit,f16bit,f32bit);}
{        float_name: array[tfloattype] of string[8]=
           ('S32REAL','S64REAL','S80REAL','S64BIT','F16BIT','F32BIT'); }
         incdecop:array[in_inc_x..in_dec_x] of tasmop=(A_INC,A_DEC);
         addsubop:array[in_inc_x..in_dec_x] of tasmop=(A_ADD,A_SUB);
       var
         aktfile : treference;
         ft : tfiletyp;
         opsize : topsize;
         op,
         asmop : tasmop;
         pushed : tpushed;
         {inc/dec}
         addconstant : boolean;
         addvalue : longint;
         hp : tnode;


      procedure handlereadwrite(doread,doln : boolean);
      { produces code for READ(LN) and WRITE(LN) }

        procedure loadstream;
          const
            io:array[boolean] of string[6]=('OUTPUT','INPUT');
          var
            r : preference;
          begin
            new(r);
            reset_reference(r^);
            r^.symbol:=newasmsymbol(
            'U_SYSTEM_'+io[doread]);
            getexplicitregister32(R_EDI);
            emit_ref_reg(A_LEA,S_L,r,R_EDI)
          end;

        const
           rdwrprefix:array[boolean] of string[15]=('FPC_WRITE_TEXT_','FPC_READ_TEXT_');
        var
           node       : tcallparanode;
           hp         : tnode;
           typedtyp,
           pararesult : pdef;
           orgfloattype : tfloattype;
           dummycoll  : tparaitem;
           iolabel    : pasmlabel;
           npara      : longint;
           esireloaded : boolean;

        begin
           { here we don't use register calling conventions }
           dummycoll.init;
           dummycoll.register:=R_NO;
           { I/O check }
           if (cs_check_io in aktlocalswitches) and
              not(po_iocheck in aktprocsym^.definition^.procoptions) then
             begin
                getaddrlabel(iolabel);
                emitlab(iolabel);
             end
           else
             iolabel:=nil;
           { for write of real with the length specified }
           hp:=nil;
           { reserve temporary pointer to data variable }
           aktfile.symbol:=nil;
           gettempofsizereference(4,aktfile);
           { first state text data }
           ft:=ft_text;
           { and state a parameter ? }
           if left=nil then
             begin
                { the following instructions are for "writeln;" }
                loadstream;
                { save @aktfile in temporary variable }
                emit_reg_ref(A_MOV,S_L,R_EDI,newreference(aktfile));
{$ifndef noAllocEdi}
                ungetregister32(R_EDI);
{$endif noAllocEdi}
             end
           else
             begin
                { revers paramters }
                node:=tcallparanode(reversparameter(left));

                left := node;
                npara := nb_para;
                { calculate data variable }
                { is first parameter a file type ? }
                if node.left.resulttype^.deftype=filedef then
                  begin
                     ft:=pfiledef(node.left.resulttype)^.filetyp;
                     if ft=ft_typed then
                       typedtyp:=pfiledef(node.left.resulttype)^.typedfiletype.def;
                     secondpass(node.left);
                     if codegenerror then
                       exit;

                     { save reference in temporary variables }
                     if node.left.location.loc<>LOC_REFERENCE then
                       begin
                          CGMessage(cg_e_illegal_expression);
                          exit;
                       end;
{$ifndef noAllocEdi}
                     getexplicitregister32(R_EDI);
{$endif noAllocEdi}

                     emit_ref_reg(A_LEA,S_L,newreference(node.left.location.reference),R_EDI);
                     del_reference(node.left.location.reference);
                     { skip to the next parameter }
                     node:=tcallparanode(node.right);
                  end
                else
                  begin
                  { load stdin/stdout stream }
                     loadstream;
                  end;

                { save @aktfile in temporary variable }
                emit_reg_ref(A_MOV,S_L,R_EDI,newreference(aktfile));
{$ifndef noAllocEdi}
                ungetregister32(R_EDI);
{$endif noAllocEdi}
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
                  dummycoll.paratype.setdef(cformaldef)
                else
                  { I think, this isn't a good solution (FK) }
                  dummycoll.paratype.reset;

                while assigned(node) do
                  begin
                     esireloaded:=false;
                     pushusedregisters(pushed,$ff);
                     hp:=node;
                     node:=tcallparanode(node.right);
                     tcallparanode(hp).right:=nil;
                     if cpf_is_colon_para in tcallparanode(hp).callparaflags then
                       CGMessage(parser_e_illegal_colon_qualifier);
                     { when float is written then we need bestreal to be pushed
                       convert here else we loose the old float type }
                     if (not doread) and
                        (ft<>ft_typed) and
                        (tcallparanode(hp).left.resulttype^.deftype=floatdef) then
                      begin
                        orgfloattype:=pfloatdef(tcallparanode(hp).left.resulttype)^.typ;
                        tcallparanode(hp).left:=gentypeconvnode(tcallparanode(hp).left,bestrealdef^);
                        firstpass(tcallparanode(hp).left);
                      end;
                     { when read ord,floats are functions, so they need this
                       parameter as their destination instead of being pushed }
                     if doread and
                        (ft<>ft_typed) and
                        (tcallparanode(hp).resulttype^.deftype in [orddef,floatdef]) then
                      begin
                      end
                     else
                      begin
                        if ft=ft_typed then
                          never_copy_const_param:=true;
                        { reset data type }
                        dummycoll.paratype.reset;
                        { create temporary defs for high tree generation }
                        if doread and (is_shortstring(tcallparanode(hp).resulttype)) then
                          dummycoll.paratype.setdef(openshortstringdef)
                        else
                          if (is_chararray(tcallparanode(hp).resulttype)) then
                            dummycoll.paratype.setdef(openchararraydef);
                        tcallparanode(hp).secondcallparan(@dummycoll,false,false,false,0,0);
                        if ft=ft_typed then
                          never_copy_const_param:=false;
                      end;
                     tcallparanode(hp).right:=node;
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
                          { the type size                          }
                          { as recordsize for that file !!!!    }
                          { how can we make that                    }
                          { I think that is only possible by adding }
                          { reset and rewrite to the inline list a call }
                          { allways read only one record by element }
                            push_int(typedtyp^.size);
                            if doread then
                              emitcall('FPC_TYPED_READ')
                            else
                              emitcall('FPC_TYPED_WRITE');
                       end
                     else
                       begin
                          { save current position }
                          pararesult:=tcallparanode(hp).left.resulttype;
                          { handle possible field width  }
                          { of course only for write(ln) }
                          if not doread then
                            begin
                               { handle total width parameter }
                              if assigned(node) and (cpf_is_colon_para in node.callparaflags) then
                                begin
                                   hp:=node;
                                   node:=tcallparanode(node.right);
                                   tcallparanode(hp).right:=nil;
                                   dummycoll.paratype.setdef(hp.resulttype);
                                   dummycoll.paratyp:=vs_value;
                                   tcallparanode(hp).secondcallparan(@dummycoll,false,false,false,0,0);
                                   tcallparanode(hp).right:=node;
                                   if codegenerror then
                                     exit;
                                end
                              else
                                if pararesult^.deftype<>floatdef then
                                  push_int(0)
                                else
                                  push_int(-32767);
                            { a second colon para for a float ? }
                              if assigned(node) and (cpf_is_colon_para in node.callparaflags) then
                                begin
                                   hp:=node;
                                   node:=tcallparanode(node.right);
                                   tcallparanode(hp).right:=nil;
                                   dummycoll.paratype.setdef(hp.resulttype);
                                   dummycoll.paratyp:=vs_value;
                                   tcallparanode(hp).secondcallparan(@dummycoll,false,false,false,0,0);
                                   tcallparanode(hp).right:=node;
                                   if pararesult^.deftype<>floatdef then
                                     CGMessage(parser_e_illegal_colon_qualifier);
                                   if codegenerror then
                                     exit;
                                end
                              else
                                begin
                                  if pararesult^.deftype=floatdef then
                                    push_int(-1);
                                end;
                             { push also the real type for floats }
                              if pararesult^.deftype=floatdef then
                                push_int(ord(orgfloattype));
                            end;
                          case pararesult^.deftype of
                            stringdef :
                              begin
                                emitcall(rdwrprefix[doread]+pstringdef(pararesult)^.stringtypname);
                              end;
                            pointerdef :
                              begin
                                if is_pchar(pararesult) then
                                  emitcall(rdwrprefix[doread]+'PCHAR_AS_POINTER')
                              end;
                            arraydef :
                              begin
                                if is_chararray(pararesult) then
                                  emitcall(rdwrprefix[doread]+'PCHAR_AS_ARRAY')
                              end;
                            floatdef :
                              begin
                                emitcall(rdwrprefix[doread]+'FLOAT');
                                {
                                if pfloatdef(resulttype)^.typ<>f32bit then
                                  dec(fpuvaroffset);
                                }
                                if doread then
                                  begin
                                     maybe_loadesi;
                                     esireloaded:=true;
                                     StoreDirectFuncResult(tcallparanode(hp).left);
                                  end;
                              end;
                            orddef :
                              begin
                                case porddef(pararesult)^.typ of
                                  s8bit,s16bit,s32bit :
                                    emitcall(rdwrprefix[doread]+'SINT');
                                  u8bit,u16bit,u32bit :
                                    emitcall(rdwrprefix[doread]+'UINT');
                                  uchar :
                                    emitcall(rdwrprefix[doread]+'CHAR');
                                  s64bit :
                                    emitcall(rdwrprefix[doread]+'INT64');
                                  u64bit :
                                    emitcall(rdwrprefix[doread]+'QWORD');
                                  bool8bit,
                                  bool16bit,
                                  bool32bit :
                                    emitcall(rdwrprefix[doread]+'BOOLEAN');
                                end;
                                if doread then
                                  begin
                                     maybe_loadesi;
                                     esireloaded:=true;
                                     StoreDirectFuncResult(tcallparanode(hp).left);
                                  end;
                              end;
                          end;
                       end;
                   { load ESI in methods again }
                     popusedregisters(pushed);
                     if not(esireloaded) then
                       maybe_loadesi;
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
                    emitcall('FPC_READLN_END')
                  else
                    emitcall('FPC_READ_END');
                end
               else
                begin
                  if doln then
                    emitcall('FPC_WRITELN_END')
                  else
                    emitcall('FPC_WRITE_END');
                end;
               popusedregisters(pushed);
               maybe_loadesi;
             end;
         { Insert IOCheck if set }
           if assigned(iolabel) then
             begin
                { registers are saved in the procedure }
                emit_sym(A_PUSH,S_L,iolabel);
                emitcall('FPC_IOCHECK');
             end;
         { Freeup all used temps }
           ungetiftemp(aktfile);
           if assigned(left) then
             begin
                left:=reversparameter(left);
                if npara<>nb_para then
                  CGMessage(cg_f_internal_error_in_secondinline);
                hp:=left;
                while assigned(hp) do
                  begin
                     if assigned(tcallparanode(hp).left) then
                       if (tcallparanode(hp).left.location.loc in [LOC_MEM,LOC_REFERENCE]) then
                         ungetiftemp(tcallparanode(hp).left.location.reference);
                     hp:=tcallparanode(hp).right;
                  end;
             end;
        end;

      procedure handle_str;

        var
           hp,
           node : tcallparanode;
           dummycoll : tparaitem;
           //hp2 : tstringconstnode;
           is_real : boolean;
           realtype : tfloattype;
           procedureprefix : string;

          begin
           dummycoll.init;
           dummycoll.register:=R_NO;
           pushusedregisters(pushed,$ff);
           node:=tcallparanode(left);
           is_real:=false;
           while assigned(node.right) do node:=tcallparanode(node.right);
           { if a real parameter somewhere then call REALSTR }
           if (node.left.resulttype^.deftype=floatdef) then
            begin
              is_real:=true;
              realtype:=pfloatdef(node.left.resulttype)^.typ;
            end;

           node:=tcallparanode(left);
           { we have at least two args }
           { with at max 2 colon_para in between }

           { string arg }
           hp:=node;
           node:=tcallparanode(node.right);
           hp.right:=nil;
           dummycoll.paratyp:=vs_var;
           if is_shortstring(hp.resulttype) then
             dummycoll.paratype.setdef(openshortstringdef)
           else
             dummycoll.paratype.setdef(hp.resulttype);
           procedureprefix:='FPC_'+pstringdef(hp.resulttype)^.stringtypname+'_';
           tcallparanode(hp).secondcallparan(@dummycoll,false,false,false,0,0);
           if codegenerror then
             exit;

           dummycoll.paratyp:=vs_const;
           left.free;
           left:=nil;
           { second arg }
           hp:=node;
           node:=tcallparanode(node.right);
           hp.right:=nil;

           { if real push real type }
           if is_real then
             push_int(ord(realtype));

           { frac  para }
           if (cpf_is_colon_para in hp.callparaflags) and assigned(node) and
              (cpf_is_colon_para in node.callparaflags) then
             begin
                dummycoll.paratype.setdef(hp.resulttype);
                dummycoll.paratyp:=vs_value;
                tcallparanode(hp).secondcallparan(@dummycoll,false,false,false,0,0);
                if codegenerror then
                  exit;
                hp.free;
                hp:=node;
                node:=tcallparanode(node.right);
                hp.right:=nil;
             end
           else
             if is_real then
             push_int(-1);

           { third arg, length only if is_real }
           if (cpf_is_colon_para in hp.callparaflags) then
             begin
                dummycoll.paratype.setdef(hp.resulttype);
                dummycoll.paratyp:=vs_value;
                tcallparanode(hp).secondcallparan(@dummycoll,false,false,false,0,0);
                if codegenerror then
                  exit;
                hp.free;
                hp:=node;
                node:=tcallparanode(node.right);
                hp.right:=nil;
             end
           else
             if is_real then
               push_int(-32767)
             else
               push_int(-1);

           { Convert float to bestreal }
           if is_real then
            begin
              hp.left:=gentypeconvnode(hp.left,bestrealdef^);
              firstpass(hp.left);
            end;

           { last arg longint or real }
           dummycoll.paratype.setdef(hp.resulttype);
           dummycoll.paratyp:=vs_value;
           tcallparanode(hp).secondcallparan(@dummycoll,false,false,false,0,0);
           if codegenerror then
             exit;

           if is_real then
             emitcall(procedureprefix+'FLOAT')
           else
             case porddef(hp.resulttype)^.typ of
                u32bit:
                  emitcall(procedureprefix+'CARDINAL');

                u64bit:
                  emitcall(procedureprefix+'QWORD');

                s64bit:
                  emitcall(procedureprefix+'INT64');

                else
                  emitcall(procedureprefix+'LONGINT');
             end;
           hp.free;

           popusedregisters(pushed);
        end;


        Procedure Handle_Val;
        var
           hp,node,
           code_para, dest_para : tcallparanode;
           hreg,hreg2: TRegister;
           hdef: POrdDef;
           procedureprefix : string;
           hr, hr2: TReference;
           dummycoll : tparaitem;
           has_code, has_32bit_code, oldregisterdef: boolean;
           r : preference;

          begin
           dummycoll.init;
           dummycoll.register:=R_NO;
           node:=tcallparanode(left);
           hp:=node;
           node:=tcallparanode(node.right);
           hp.right:=nil;
          {if we have 3 parameters, we have a code parameter}
           has_code := Assigned(node.right);
           has_32bit_code := false;
           reset_reference(hr);
           hreg := R_NO;

           If has_code then
             Begin
               {code is an orddef, that's checked in tcinl}
               code_para := hp;
               hp := node;
               node := tcallparanode(node.right);
               hp.right := nil;
               has_32bit_code := (porddef(tcallparanode(code_para).left.resulttype)^.typ in [u32bit,s32bit]);
             End;

          {hp = destination now, save for later use}
           dest_para := hp;

          {if EAX is already in use, it's a register variable. Since we don't
           need another register besides EAX, release the one we got}
           If hreg <> R_EAX Then ungetregister32(hreg);

          {load and push the address of the destination}
           dummycoll.paratyp:=vs_var;
           dummycoll.paratype.setdef(dest_para.resulttype);
           dest_para.secondcallparan(@dummycoll,false,false,false,0,0);
           if codegenerror then
             exit;

          {save the regvars}
           pushusedregisters(pushed,$ff);

          {now that we've already pushed the addres of dest_para.left on the
           stack, we can put the real parameters on the stack}

           If has_32bit_code Then
             Begin
               dummycoll.paratyp:=vs_var;
               dummycoll.paratype.setdef(code_para.resulttype);
               code_para.secondcallparan(@dummycoll,false,false,false,0,0);
               if codegenerror then
                 exit;
               code_para.free;
             End
           Else
             Begin
           {only 32bit code parameter is supported, so fake one}
               GetTempOfSizeReference(4,hr);
               emitpushreferenceaddr(hr);
             End;

          {node = first parameter = string}
           dummycoll.paratyp:=vs_const;
           dummycoll.paratype.setdef(node.resulttype);
           node.secondcallparan(@dummycoll,false,false,false,0,0);
           if codegenerror then
             exit;

           Case dest_para.resulttype^.deftype of
             floatdef:
               begin
                  procedureprefix := 'FPC_VAL_REAL_';
                  if pfloatdef(resulttype)^.typ<>f32bit then
                    inc(fpuvaroffset);
               end;
             orddef:
               if is_64bitint(dest_para.resulttype) then
                 begin
                    if is_signed(dest_para.resulttype) then
                      procedureprefix := 'FPC_VAL_INT64_'
                    else
                      procedureprefix := 'FPC_VAL_QWORD_';
                 end
               else
                 begin
                    if is_signed(dest_para.resulttype) then
                      begin
                        {if we are converting to a signed number, we have to include the
                         size of the destination, so the Val function can extend the sign
                         of the result to allow proper range checking}
                        emit_const(A_PUSH,S_L,dest_para.resulttype^.size);
                        procedureprefix := 'FPC_VAL_SINT_'
                      end
                    else
                      procedureprefix := 'FPC_VAL_UINT_';
                 end;
           End;
           emitcall(procedureprefix+pstringdef(node.resulttype)^.stringtypname);
           { before disposing node we need to ungettemp !! PM }
           if node.left.location.loc in [LOC_REFERENCE,LOC_MEM] then
             ungetiftemp(node.left.location.reference);
           node.free;
           left := nil;

          {reload esi in case the dest_para/code_para is a class variable or so}
           maybe_loadesi;

           If (dest_para.resulttype^.deftype = orddef) Then
             Begin
              {store the result in a safe place, because EAX may be used by a
               register variable}
               hreg := getexplicitregister32(R_EAX);
               emit_reg_reg(A_MOV,S_L,R_EAX,hreg);
               if is_64bitint(dest_para.resulttype) then
                 begin
                    hreg2:=getexplicitregister32(R_EDX);
                    emit_reg_reg(A_MOV,S_L,R_EDX,hreg2);
                 end;
              {as of now, hreg now holds the location of the result, if it was
               integer}
             End;

           { restore the register vars}

           popusedregisters(pushed);

           If has_code and Not(has_32bit_code) Then
             {only 16bit code is possible}
             Begin
              {load the address of the code parameter}
               secondpass(code_para.left);
              {move the code to its destination}
{$ifndef noAllocEdi}
               getexplicitregister32(R_EDI);
{$endif noAllocEdi}
               emit_ref_reg(A_MOV,S_L,NewReference(hr),R_EDI);
               emit_mov_reg_loc(R_DI,code_para.left.location);
{$ifndef noAllocEdi}
               ungetregister32(R_EDI);
{$endif noAllocEdi}
               code_para.free;
             End;

          {restore the address of the result}
{$ifndef noAllocEdi}
           getexplicitregister32(R_EDI);
{$endif noAllocEdi}
           emit_reg(A_POP,S_L,R_EDI);

          {set up hr2 to a refernce with EDI as base register}
           reset_reference(hr2);
           hr2.base := R_EDI;

          {save the function result in the destination variable}
           Case dest_para.left.resulttype^.deftype of
             floatdef:
               floatstore(PFloatDef(dest_para.left.resulttype)^.typ, hr2);
             orddef:
               Case PordDef(dest_para.left.resulttype)^.typ of
                 u8bit,s8bit:
                   emit_reg_ref(A_MOV, S_B,
                     RegToReg8(hreg),newreference(hr2));
                 u16bit,s16bit:
                   emit_reg_ref(A_MOV, S_W,
                     RegToReg16(hreg),newreference(hr2));
                 u32bit,s32bit:
                   emit_reg_ref(A_MOV, S_L,
                     hreg,newreference(hr2));
                 u64bit,s64bit:
                   begin
                      emit_reg_ref(A_MOV, S_L,
                        hreg,newreference(hr2));
                      r:=newreference(hr2);
                      inc(r^.offset,4);
                      emit_reg_ref(A_MOV, S_L,
                        hreg2,r);
                   end;
               End;
           End;
{$ifndef noAllocEdi}
           ungetregister32(R_EDI);
{$endif noAllocEdi}
           If (cs_check_range in aktlocalswitches) and
              (dest_para.left.resulttype^.deftype = orddef) and
              (not(is_64bitint(dest_para.left.resulttype))) and
            {the following has to be changed to 64bit checking, once Val
             returns 64 bit values (unless a special Val function is created
             for that)}
            {no need to rangecheck longints or cardinals on 32bit processors}
               not((porddef(dest_para.left.resulttype)^.typ = s32bit) and
                   (porddef(dest_para.left.resulttype)^.low = longint($80000000)) and
                   (porddef(dest_para.left.resulttype)^.high = $7fffffff)) and
               not((porddef(dest_para.left.resulttype)^.typ = u32bit) and
                   (porddef(dest_para.left.resulttype)^.low = 0) and
                   (porddef(dest_para.left.resulttype)^.high = longint($ffffffff))) then
             Begin
               hp:=tcallparanode(dest_para.left.getcopy);
               hp.location.loc := LOC_REGISTER;
               hp.location.register := hreg;
              {do not register this temporary def}
               OldRegisterDef := RegisterDef;
               RegisterDef := False;
               Case PordDef(dest_para.left.resulttype)^.typ of
                 u8bit,u16bit,u32bit: new(hdef,init(u32bit,0,$ffffffff));
                 s8bit,s16bit,s32bit: new(hdef,init(s32bit,$80000000,$7fffffff));
               end;
               hp.resulttype := hdef;
               emitrangecheck(hp,dest_para.left.resulttype);
               hp.right := nil;
               Dispose(hp.resulttype, Done);
               RegisterDef := OldRegisterDef;
               hp.free;
             End;
          {dest_para.right is already nil}
           dest_para.free;
           UnGetIfTemp(hr);
        end;

      var
         r : preference;
         //hp : tcallparanode;
         hp2 : tstringconstnode;
         dummycoll  : tparaitem;
         l : longint;
         ispushed : boolean;
         hregister : tregister;
         otlabel,oflabel{,l1}   : pasmlabel;
         oldpushedparasize : longint;
         def : pdef;
         hr,hr2 : treference;

      begin
      { save & reset pushedparasize }
         oldpushedparasize:=pushedparasize;
         pushedparasize:=0;
         case inlinenumber of
            in_assert_x_y:
              begin
                 { the node should be removed in the firstpass }
                 if not (cs_do_assertion in aktlocalswitches) then
                  internalerror(7123458);
                 otlabel:=truelabel;
                 oflabel:=falselabel;
                 getlabel(truelabel);
                 getlabel(falselabel);
                 secondpass(tcallparanode(left).left);
                 maketojumpbool(tcallparanode(left).left);
                 emitlab(falselabel);
                 { erroraddr }
                 emit_reg(A_PUSH,S_L,R_EBP);
                 { lineno }
                 emit_const(A_PUSH,S_L,aktfilepos.line);
                 { filename string }
                 hp2:=genstringconstnode(current_module^.sourcefiles^.get_file_name(aktfilepos.fileindex),st_shortstring);
                 secondpass(hp2);
                 if codegenerror then
                  exit;
                 emitpushreferenceaddr(hp2.location.reference);
                 hp2.free;
                 { push msg }
                 secondpass(tcallparanode(tcallparanode(left).right).left);
                 emitpushreferenceaddr(tcallparanode(tcallparanode(left).right).left.location.reference);
                 { call }
                 emitcall('FPC_ASSERT');
                 emitlab(truelabel);
                 truelabel:=otlabel;
                 falselabel:=oflabel;
              end;
            in_lo_word,
            in_hi_word :
              begin
                 secondpass(left);
                 location.loc:=LOC_REGISTER;
                 if left.location.loc<>LOC_REGISTER then
                   begin
                     if left.location.loc=LOC_CREGISTER then
                       begin
                          location.register:=reg32toreg16(getregister32);
                          emit_reg_reg(A_MOV,S_W,left.location.register,
                            location.register);
                       end
                     else
                       begin
                          del_reference(left.location.reference);
                          location.register:=reg32toreg16(getregister32);
                          emit_ref_reg(A_MOV,S_W,newreference(left.location.reference),
                            location.register);
                       end;
                   end
                 else location.register:=left.location.register;
                 if inlinenumber=in_hi_word then
                   emit_const_reg(A_SHR,S_W,8,location.register);
                 location.register:=reg16toreg8(location.register);
              end;
            in_sizeof_x,
            in_typeof_x :
              begin
                 { for both cases load vmt }
                 if left.nodetype=typen then
                   begin
                      location.register:=getregister32;
                      emit_sym_ofs_reg(A_MOV,
                        S_L,newasmsymbol(pobjectdef(left.resulttype)^.vmt_mangledname),0,
                        location.register);
                   end
                 else
                   begin
                      secondpass(left);
                      del_reference(left.location.reference);
                      location.loc:=LOC_REGISTER;
                      location.register:=getregister32;
                      { load VMT pointer }
                      inc(left.location.reference.offset,
                        pobjectdef(left.resulttype)^.vmt_offset);
                      emit_ref_reg(A_MOV,S_L,
                      newreference(left.location.reference),
                        location.register);
                   end;
                 { in sizeof load size }
                 if inlinenumber=in_sizeof_x then
                   begin
                      new(r);
                      reset_reference(r^);
                      r^.base:=location.register;
                      emit_ref_reg(A_MOV,S_L,r,
                        location.register);
                   end;
              end;
            in_lo_long,
            in_hi_long :
              begin
                 secondpass(left);
                 location.loc:=LOC_REGISTER;
                 if left.location.loc<>LOC_REGISTER then
                   begin
                      if left.location.loc=LOC_CREGISTER then
                        begin
                           location.register:=getregister32;
                           emit_reg_reg(A_MOV,S_L,left.location.register,
                             location.register);
                        end
                      else
                        begin
                           del_reference(left.location.reference);
                           location.register:=getregister32;
                           emit_ref_reg(A_MOV,S_L,newreference(left.location.reference),
                             location.register);
                        end;
                   end
                 else location.register:=left.location.register;
                 if inlinenumber=in_hi_long then
                   emit_const_reg(A_SHR,S_L,16,location.register);
                 location.register:=reg32toreg16(location.register);
              end;
            in_lo_qword,
            in_hi_qword:
              begin
                 secondpass(left);
                 location.loc:=LOC_REGISTER;
                 case left.location.loc of
                    LOC_CREGISTER:
                      begin
                         location.register:=getregister32;
                         if inlinenumber=in_hi_qword then
                           emit_reg_reg(A_MOV,S_L,left.location.registerhigh,
                             location.register)
                         else
                           emit_reg_reg(A_MOV,S_L,left.location.registerlow,
                             location.register)
                      end;
                    LOC_MEM,LOC_REFERENCE:
                      begin
                         del_reference(left.location.reference);
                         location.register:=getregister32;
                         r:=newreference(left.location.reference);
                         if inlinenumber=in_hi_qword then
                           inc(r^.offset,4);
                         emit_ref_reg(A_MOV,S_L,
                           r,location.register);
                      end;
                    LOC_REGISTER:
                      begin
                         if inlinenumber=in_hi_qword then
                           begin
                              location.register:=left.location.registerhigh;
                              ungetregister32(left.location.registerlow);
                           end
                         else
                           begin
                              location.register:=left.location.registerlow;
                              ungetregister32(left.location.registerhigh);
                           end;
                      end;
                 end;
              end;
            in_length_string :
              begin
                 secondpass(left);
                 set_location(location,left.location);
                 { length in ansi strings is at offset -8 }
                 if is_ansistring(left.resulttype) then
                   dec(location.reference.offset,8)
                 { char is always 1, so make it a constant value }
                 else if is_char(left.resulttype) then
                   begin
                     clear_location(location);
                     location.loc:=LOC_MEM;
                     location.reference.is_immediate:=true;
                     location.reference.offset:=1;
                   end;
              end;
            in_pred_x,
            in_succ_x:
              begin
                 secondpass(left);
                 if not (cs_check_overflow in aktlocalswitches) then
                   if inlinenumber=in_pred_x then
                     asmop:=A_DEC
                   else
                     asmop:=A_INC
                 else
                   if inlinenumber=in_pred_x then
                     asmop:=A_SUB
                   else
                     asmop:=A_ADD;
                 case resulttype^.size of
                   8 : opsize:=S_L;
                   4 : opsize:=S_L;
                   2 : opsize:=S_W;
                   1 : opsize:=S_B;
                 else
                   internalerror(10080);
                 end;
                 location.loc:=LOC_REGISTER;
                 if resulttype^.size=8 then
                   begin
                      if left.location.loc<>LOC_REGISTER then
                        begin
                           if left.location.loc=LOC_CREGISTER then
                             begin
                                location.registerlow:=getregister32;
                                location.registerhigh:=getregister32;
                                emit_reg_reg(A_MOV,opsize,left.location.registerlow,
                                  location.registerlow);
                                emit_reg_reg(A_MOV,opsize,left.location.registerhigh,
                                  location.registerhigh);
                             end
                           else
                             begin
                                del_reference(left.location.reference);
                                location.registerlow:=getregister32;
                                location.registerhigh:=getregister32;
                                emit_ref_reg(A_MOV,opsize,newreference(left.location.reference),
                                  location.registerlow);
                                r:=newreference(left.location.reference);
                                inc(r^.offset,4);
                                emit_ref_reg(A_MOV,opsize,r,
                                  location.registerhigh);
                             end;
                        end
                      else
                        begin
                           location.registerhigh:=left.location.registerhigh;
                           location.registerlow:=left.location.registerlow;
                        end;
                      if inlinenumber=in_succ_x then
                        begin
                           emit_const_reg(A_ADD,opsize,1,
                             location.registerlow);
                           emit_const_reg(A_ADC,opsize,0,
                             location.registerhigh);
                        end
                      else
                        begin
                           emit_const_reg(A_SUB,opsize,1,
                             location.registerlow);
                           emit_const_reg(A_SBB,opsize,0,
                             location.registerhigh);
                        end;
                   end
                 else
                   begin
                      if left.location.loc<>LOC_REGISTER then
                        begin
                           { first, we've to release the source location ... }
                           if left.location.loc in [LOC_MEM,LOC_REFERENCE] then
                             del_reference(left.location.reference);

                           location.register:=getregister32;
                           if (resulttype^.size=2) then
                             location.register:=reg32toreg16(location.register);
                           if (resulttype^.size=1) then
                             location.register:=reg32toreg8(location.register);
                           if left.location.loc=LOC_CREGISTER then
                             emit_reg_reg(A_MOV,opsize,left.location.register,
                               location.register)
                           else
                           if left.location.loc=LOC_FLAGS then
                             emit_flag2reg(left.location.resflags,location.register)
                           else
                             emit_ref_reg(A_MOV,opsize,newreference(left.location.reference),
                               location.register);
                        end
                      else location.register:=left.location.register;
                      if not (cs_check_overflow in aktlocalswitches) then
                        emit_reg(asmop,opsize,
                        location.register)
                      else
                        emit_const_reg(asmop,opsize,1,
                        location.register);
                   end;
                 emitoverflowcheck(self);
                 emitrangecheck(self,resulttype);
              end;
            in_dec_x,
            in_inc_x :
              begin
              { set defaults }
                addvalue:=1;
                addconstant:=true;
              { load first parameter, must be a reference }
                secondpass(tcallparanode(left).left);
                case tcallparanode(left).left.resulttype^.deftype of
                  orddef,
                 enumdef : begin
                             case tcallparanode(left).left.resulttype^.size of
                              1 : opsize:=S_B;
                              2 : opsize:=S_W;
                              4 : opsize:=S_L;
                              8 : opsize:=S_L;
                             end;
                           end;
              pointerdef : begin
                             opsize:=S_L;
                             if porddef(ppointerdef(tcallparanode(left).left.resulttype)^.pointertype.def)=voiddef then
                              addvalue:=1
                             else
                              addvalue:=ppointerdef(tcallparanode(left).left.resulttype)^.pointertype.def^.size;
                           end;
                else
                 internalerror(10081);
                end;
              { second argument specified?, must be a s32bit in register }
                if assigned(tcallparanode(left).right) then
                 begin
                   ispushed:=maybe_push(tcallparanode(tcallparanode(left).right).left.registers32,
                     tcallparanode(left).left,false);
                   secondpass(tcallparanode(tcallparanode(left).right).left);
                   if ispushed then
                     restore(tcallparanode(left).left,false);
                 { when constant, just multiply the addvalue }
                   if is_constintnode(tcallparanode(tcallparanode(left).right).left) then
                    addvalue:=addvalue*get_ordinal_value(tcallparanode(tcallparanode(left).right).left)
                   else
                    begin
                      case tcallparanode(tcallparanode(left).right).left.location.loc of
                   LOC_REGISTER,
                  LOC_CREGISTER : hregister:=tcallparanode(tcallparanode(left).right).left.location.register;
                        LOC_MEM,
                  LOC_REFERENCE : begin
                                    del_reference(tcallparanode(tcallparanode(left).right).left.location.reference);
                                    hregister:=getregister32;
                                    emit_ref_reg(A_MOV,S_L,
                                      newreference(tcallparanode(tcallparanode(left).right).left.location.reference),hregister);
                                  end;
                       else
                        internalerror(10082);
                       end;
                    { insert multiply with addvalue if its >1 }
                      if addvalue>1 then
                       emit_const_reg(A_IMUL,opsize,
                         addvalue,hregister);
                      addconstant:=false;
                    end;
                 end;
              { write the add instruction }
                if addconstant then
                 begin
                   if (addvalue=1) and not(cs_check_overflow in aktlocalswitches) then
                     begin
                        if tcallparanode(left).left.location.loc=LOC_CREGISTER then
                          emit_reg(incdecop[inlinenumber],opsize,
                            tcallparanode(left).left.location.register)
                        else
                          emit_ref(incdecop[inlinenumber],opsize,
                            newreference(tcallparanode(left).left.location.reference))
                     end
                   else
                     begin
                        if tcallparanode(left).left.location.loc=LOC_CREGISTER then
                          emit_const_reg(addsubop[inlinenumber],opsize,
                            addvalue,tcallparanode(left).left.location.register)
                        else
                          emit_const_ref(addsubop[inlinenumber],opsize,
                            addvalue,newreference(tcallparanode(left).left.location.reference));
                     end
                 end
                else
                 begin
                    { BUG HERE : detected with nasm :
                      hregister is allways 32 bit
                      it should be converted to 16 or 8 bit depending on op_size  PM }
                    { still not perfect :
                      if hregister is already a 16 bit reg ?? PM }
                    { makeregXX is the solution (FK) }
                    case opsize of
                      S_B : hregister:=makereg8(hregister);
                      S_W : hregister:=makereg16(hregister);
                    end;
                    if tcallparanode(left).left.location.loc=LOC_CREGISTER then
                      emit_reg_reg(addsubop[inlinenumber],opsize,
                        hregister,tcallparanode(left).left.location.register)
                    else
                      emit_reg_ref(addsubop[inlinenumber],opsize,
                        hregister,newreference(tcallparanode(left).left.location.reference));
                    case opsize of
                      S_B : hregister:=reg8toreg32(hregister);
                      S_W : hregister:=reg16toreg32(hregister);
                    end;
                   ungetregister32(hregister);
                 end;
                emitoverflowcheck(tcallparanode(left).left);
                emitrangecheck(tcallparanode(left).left,tcallparanode(left).left.resulttype);
              end;
            in_typeinfo_x:
               begin
                  pstoreddef(ttypenode(tcallparanode(left).left).typenodetype)^.generate_rtti;
                  location.register:=getregister32;
                  new(r);
                  reset_reference(r^);
                  r^.symbol:=pstoreddef(ttypenode(tcallparanode(left).left).typenodetype)^.rtti_label;
                  emit_ref_reg(A_MOV,S_L,r,location.register);
               end;
            in_assigned_x :
              begin
                 secondpass(tcallparanode(left).left);
                 location.loc:=LOC_FLAGS;
                 if (tcallparanode(left).left.location.loc in [LOC_REGISTER,LOC_CREGISTER]) then
                   begin
                      emit_reg_reg(A_OR,S_L,
                        tcallparanode(left).left.location.register,
                        tcallparanode(left).left.location.register);
                      ungetregister32(tcallparanode(left).left.location.register);
                   end
                 else
                   begin
                      emit_const_ref(A_CMP,S_L,0,
                        newreference(tcallparanode(left).left.location.reference));
                      del_reference(tcallparanode(left).left.location.reference);
                   end;
                 location.resflags:=F_NE;
              end;
             in_reset_typedfile,in_rewrite_typedfile :
               begin
                  pushusedregisters(pushed,$ff);
                  emit_const(A_PUSH,S_L,pfiledef(left.resulttype)^.typedfiletype.def^.size);
                  secondpass(left);
                  emitpushreferenceaddr(left.location.reference);
                  if inlinenumber=in_reset_typedfile then
                    emitcall('FPC_RESET_TYPED')
                  else
                    emitcall('FPC_REWRITE_TYPED');
                  popusedregisters(pushed);
               end;
            in_setlength_x:
               begin
                  pushusedregisters(pushed,$ff);
                  l:=0;
                  { push dimensions }
                  hp:=left;
                  while assigned(tcallparanode(hp).right) do
                    begin
                       inc(l);
                       hp:=tcallparanode(hp).right;
                    end;
                  def:=tcallparanode(hp).left.resulttype;
                  hp:=left;
                  if is_dynamic_array(def) then
                    begin
                       { get temp. space }
                       gettempofsizereference(l*4,hr);
                       { copy dimensions }
                       hp:=left;
                       while assigned(tcallparanode(hp).right) do
                         begin
                            secondpass(tcallparanode(hp).left);
                            emit_mov_loc_ref(tcallparanode(hp).left.location,hr,
                              S_L,true);
                            inc(hr.offset,4);
                            hp:=tcallparanode(hp).right;
                         end;
                    end
                  else
                    begin
                       secondpass(tcallparanode(hp).left);
                       emit_push_loc(tcallparanode(hp).left.location);
                       hp:=tcallparanode(hp).right;
                    end;
                  { handle shortstrings separately since the hightree must be }
                  { pushed too (JM)                                           }
                  if not(is_dynamic_array(def)) and
                     (pstringdef(def)^.string_typ = st_shortstring) then
                    begin
                      dummycoll.init;
                      dummycoll.paratyp:=vs_var;
                      dummycoll.paratype.setdef(openshortstringdef);
                      tcallparanode(hp).secondcallparan(@dummycoll,false,false,false,0,0);
                      if codegenerror then
                        exit;
                    end
                  else secondpass(tcallparanode(hp).left);
                  if is_dynamic_array(def) then
                    begin
                       emitpushreferenceaddr(hr);
                       push_int(l);
                       reset_reference(hr2);
                       hr2.symbol:=pstoreddef(def)^.get_inittable_label;
                       emitpushreferenceaddr(hr2);
                       emitpushreferenceaddr(tcallparanode(hp).left.location.reference);
                       emitcall('FPC_DYNARR_SETLENGTH');
                       ungetiftemp(hr);
                    end
                  else
                    { must be string }
                    begin
                       case pstringdef(def)^.string_typ of
                          st_widestring:
                            begin
                              emitpushreferenceaddr(tcallparanode(hp).left.location.reference);
                              emitcall('FPC_WIDESTR_SETLENGTH');
                            end;
                          st_ansistring:
                            begin
                              emitpushreferenceaddr(tcallparanode(hp).left.location.reference);
                              emitcall('FPC_ANSISTR_SETLENGTH');
                            end;
                          st_shortstring:
                            emitcall('FPC_SHORTSTR_SETLENGTH');
                       end;
                    end;
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
                 maybe_loadesi;
              end;
            in_val_x :
              Begin
                handle_val;
              End;
            in_include_x_y,
            in_exclude_x_y:
              begin
                 secondpass(tcallparanode(left).left);
                 if tcallparanode(tcallparanode(left).right).left.nodetype=ordconstn then
                   begin
                      { calculate bit position }
                      l:=1 shl (tordconstnode(tcallparanode(tcallparanode(left).right).left).value mod 32);

                      { determine operator }
                      if inlinenumber=in_include_x_y then
                        asmop:=A_OR
                      else
                        begin
                           asmop:=A_AND;
                           l:=not(l);
                        end;
                      if (tcallparanode(left).left.location.loc=LOC_REFERENCE) then
                        begin
                           inc(tcallparanode(left).left.location.reference.offset,
                             (tordconstnode(tcallparanode(tcallparanode(left).right).left).value div 32)*4);
                           emit_const_ref(asmop,S_L,
                             l,newreference(tcallparanode(left).left.location.reference));
                           del_reference(tcallparanode(left).left.location.reference);
                        end
                      else
                        { LOC_CREGISTER }
                        emit_const_reg(asmop,S_L,
                          l,tcallparanode(left).left.location.register);
                   end
                 else
                   begin
                      { generate code for the element to set }
                      ispushed:=maybe_push(tcallparanode(tcallparanode(left).right).left.registers32,
                        tcallparanode(left).left,false);
                      secondpass(tcallparanode(tcallparanode(left).right).left);
                      if ispushed then
                        restore(tcallparanode(left).left,false);
                      { determine asm operator }
                      if inlinenumber=in_include_x_y then
                        asmop:=A_BTS
                      else
                        asmop:=A_BTR;
                      if psetdef(left.resulttype)^.settype=smallset then
                        begin
                           if tcallparanode(tcallparanode(left).right).left.location.loc in [LOC_CREGISTER,LOC_REGISTER] then
                             { we don't need a mod 32 because this is done automatically  }
                             { by the bts instruction. For proper checking we would       }
                             { need a cmp and jmp, but this should be done by the         }
                             { type cast code which does range checking if necessary (FK) }
                             hregister:=makereg32(tcallparanode(tcallparanode(left).right).left.location.register)
                           else
                             begin
                                getexplicitregister32(R_EDI);
                                hregister:=R_EDI;
                                opsize:=def2def_opsize(
                                  tcallparanode(tcallparanode(left).right).left.resulttype,u32bitdef);
                                if opsize in [S_B,S_W,S_L] then
                                 op:=A_MOV
                                else
                                 op:=A_MOVZX;
                                emit_ref_reg(op,opsize,
                                  newreference(
                                    tcallparanode(tcallparanode(left).right).left.location.reference),R_EDI);
                             end;
                          if (tcallparanode(left).left.location.loc=LOC_REFERENCE) then
                            emit_reg_ref(asmop,S_L,hregister,
                              newreference(tcallparanode(left).left.location.reference))
                          else
                            emit_reg_reg(asmop,S_L,hregister,
                              tcallparanode(left).left.location.register);
                        if hregister = R_EDI then
                          ungetregister32(R_EDI);
                        end
                      else
                        begin
                           pushsetelement(tcallparanode(tcallparanode(left).right).left);
                           { normset is allways a ref }
                           emitpushreferenceaddr(tcallparanode(left).left.location.reference);
                           if inlinenumber=in_include_x_y then
                             emitcall('FPC_SET_SET_BYTE')
                           else
                             emitcall('FPC_SET_UNSET_BYTE');
                           {CGMessage(cg_e_include_not_implemented);}
                        end;
                   end;
              end;
            in_pi:
              begin
                emit_none(A_FLDPI,S_NO);
                inc(fpuvaroffset);
              end;
            in_sin_extended,
            in_arctan_extended,
            in_abs_extended,
            in_sqr_extended,
            in_sqrt_extended,
            in_ln_extended,
            in_cos_extended:
              begin
                 secondpass(left);
                 case left.location.loc of
                    LOC_FPU:
                      ;
                    LOC_CFPUREGISTER:
                      begin
                         emit_reg(A_FLD,S_NO,
                           correct_fpuregister(left.location.register,fpuvaroffset));
                         inc(fpuvaroffset);
                      end;
                    LOC_REFERENCE,LOC_MEM:
                      begin
                         floatload(pfloatdef(left.resulttype)^.typ,left.location.reference);
                         del_reference(left.location.reference);
                      end
                    else
                      internalerror(309991);
                 end;
                 case inlinenumber of
                    in_sin_extended,
                    in_cos_extended:
                      begin
                         if inlinenumber=in_sin_extended then
                           emit_none(A_FSIN,S_NO)
                         else
                           emit_none(A_FCOS,S_NO);
                         {
                         getlabel(l1);
                         emit_reg(A_FNSTSW,S_NO,R_AX);
                         emit_none(A_SAHF,S_NO);
                         emitjmp(C_NP,l1);
                         emit_reg(A_FSTP,S_NO,R_ST0);
                         emit_none(A_FLDZ,S_NO);
                         emitlab(l1);
                         }
                      end;
                    in_arctan_extended:
                      begin
                         emit_none(A_FLD1,S_NO);
                         emit_none(A_FPATAN,S_NO);
                      end;
                    in_abs_extended:
                      emit_none(A_FABS,S_NO);
                    in_sqr_extended:
                      begin
                         (* emit_reg(A_FLD,S_NO,R_ST0);
                         { emit_none(A_FMULP,S_NO); nasm does not accept this PM }
                         emit_reg_reg(A_FMULP,S_NO,R_ST0,R_ST1);
                           can be shorten to *)
                         emit_reg_reg(A_FMUL,S_NO,R_ST0,R_ST0);
                      end;
                    in_sqrt_extended:
                      emit_none(A_FSQRT,S_NO);
                    in_ln_extended:
                      begin
                         emit_none(A_FLDLN2,S_NO);
                         emit_none(A_FXCH,S_NO);
                         emit_none(A_FYL2X,S_NO);
                      end;
                 end;
              end;
{$ifdef SUPPORT_MMX}
            in_mmx_pcmpeqb..in_mmx_pcmpgtw:
              begin
                 if left.location.loc=LOC_REGISTER then
                   begin
                      {!!!!!!!}
                   end
                 else if tcallparanode(left).left.location.loc=LOC_REGISTER then
                   begin
                      {!!!!!!!}
                   end
                 else
                   begin
                      {!!!!!!!}
                   end;
              end;
{$endif SUPPORT_MMX}
            else internalerror(9);
         end;
         { reset pushedparasize }
         pushedparasize:=oldpushedparasize;
      end;

begin
   cinlinenode:=ti386inlinenode;
end.
{
  $Log$
  Revision 1.4  2000-10-31 22:02:56  peter
    * symtable splitted, no real code changes

  Revision 1.3  2000/10/26 14:15:07  jonas
    * fixed setlength for shortstrings

  Revision 1.2  2000/10/21 18:16:13  florian
    * a lot of changes:
       - basic dyn. array support
       - basic C++ support
       - some work for interfaces done
       ....

  Revision 1.1  2000/10/15 09:33:31  peter
    * moved n386*.pas to i386/ cpu_target dir

  Revision 1.2  2000/10/15 09:08:58  peter
    * use System for the systemunit instead of target dependent

  Revision 1.1  2000/10/14 10:14:49  peter
    * moehrendorf oct 2000 rewrite

}