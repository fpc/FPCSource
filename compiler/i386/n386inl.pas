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
      cutils,verbose,globals,fmodule,
      symconst,symbase,symtype,symdef,symsym,aasm,types,
      cgbase,temp_gen,pass_1,pass_2,
      cpubase,
      nbas,ncon,ncal,ncnv,nld,
      cga,tgcpu,n386util;


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

{$ifndef hascompilerproc}
    procedure StoreDirectFuncResult(var dest:tnode);
      var
        hp : tnode;
        htype : ttype;
        hreg : tregister;
        hregister : tregister;
        oldregisterdef : boolean;
        op : tasmop;
        opsize : topsize;

      begin
        { Get the accumulator first so it can't be used in the dest }
        if (dest.resulttype.def.deftype=orddef) and
          not(is_64bitint(dest.resulttype.def)) then
          hregister:=getexplicitregister32(accumulator);
        { process dest }
        SecondPass(dest);
        if Codegenerror then
         exit;
        { store the value }
        Case dest.resulttype.def.deftype of
          floatdef:
            if dest.location.loc=LOC_CFPUREGISTER then
              begin
                 floatstoreops(tfloatdef(dest.resulttype.def).typ,op,opsize);
                 emit_reg(op,opsize,correct_fpuregister(dest.location.register,fpuvaroffset+1));
              end
            else
              begin
                 inc(fpuvaroffset);
                 floatstore(tfloatdef(dest.resulttype.def).typ,dest.location.reference);
                 { floatstore decrements the fpu var offset }
                 { but in fact we didn't increment it       }
              end;
          orddef:
            begin
              if is_64bitint(dest.resulttype.def) then
                begin
                   emit_movq_reg_loc(R_EDX,R_EAX,dest.location);
                end
              else
               begin
                 Case dest.resulttype.def.size of
                  1 : hreg:=regtoreg8(hregister);
                  2 : hreg:=regtoreg16(hregister);
                  4 : hreg:=hregister;
                 End;
                 emit_mov_reg_loc(hreg,dest.location);
                 If (cs_check_range in aktlocalswitches) and
                    {no need to rangecheck longints or cardinals on 32bit processors}
                    not((torddef(dest.resulttype.def).typ = s32bit) and
                        (torddef(dest.resulttype.def).low = longint($80000000)) and
                        (torddef(dest.resulttype.def).high = $7fffffff)) and
                    not((torddef(dest.resulttype.def).typ = u32bit) and
                        (torddef(dest.resulttype.def).low = 0) and
                        (torddef(dest.resulttype.def).high = longint($ffffffff))) then
                  Begin
                    {do not register this temporary def}
                    OldRegisterDef := RegisterDef;
                    RegisterDef := False;
                    htype.reset;
                    Case torddef(dest.resulttype.def).typ of
                      u8bit,u16bit,u32bit:
                        begin
                          htype.setdef(torddef.create(u32bit,0,longint($ffffffff)));
                          hreg:=hregister;
                        end;
                      s8bit,s16bit,s32bit:
                        begin
                          htype.setdef(torddef.create(s32bit,longint($80000000),$7fffffff));
                          hreg:=hregister;
                        end;
                    end;
                    { create a fake node }
                    hp := cnothingnode.create;
                    hp.location.loc := LOC_REGISTER;
                    hp.location.register := hreg;
                    if assigned(htype.def) then
                      hp.resulttype:=htype
                    else
                      hp.resulttype:=dest.resulttype;
                    { emit the range check }
                    emitrangecheck(hp,dest.resulttype.def);
                    if assigned(htype.def) then
                      htype.def.free;
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
{$endif not hascomppilerproc}

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

{$ifndef hascompilerproc}
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
           pararesult : tdef;
           orgfloattype : tfloattype;
           dummycoll  : tparaitem;
           iolabel    : tasmlabel;
           npara      : longint;
           esireloaded : boolean;
        label
          myexit;
        begin
           { here we don't use register calling conventions }
           dummycoll:=TParaItem.Create;
           dummycoll.register:=R_NO;
           { I/O check }
           if (cs_check_io in aktlocalswitches) and
              not(po_iocheck in aktprocsym.definition.procoptions) then
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
                ungetregister32(R_EDI);
             end
           else
             begin
                { revers paramters }
                node:=tcallparanode(reversparameter(left));

                left := node;
                npara := nb_para;
                { calculate data variable }
                { is first parameter a file type ? }
                if node.left.resulttype.def.deftype=filedef then
                  begin
                     ft:=tfiledef(node.left.resulttype.def).filetyp;
                     if ft=ft_typed then
                       typedtyp:=tfiledef(node.left.resulttype.def).typedfiletype.def;
                     secondpass(node.left);
                     if codegenerror then
                       goto myexit;

                     { save reference in temporary variables }
                     if node.left.location.loc<>LOC_REFERENCE then
                       begin
                          CGMessage(cg_e_illegal_expression);
                          goto myexit;
                       end;
                     getexplicitregister32(R_EDI);
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
                ungetregister32(R_EDI);
                if doread then
                { parameter by READ gives call by reference }
                  dummycoll.paratyp:=vs_var
                { an WRITE Call by "Const" }
                else
                  dummycoll.paratyp:=vs_const;

                { because of secondcallparan, which otherwise attaches }
                if ft=ft_typed then
                  { this is to avoid copy of simple const parameters }
                  {dummycoll.data:=new(pformaldef.create)}
                  dummycoll.paratype:=cformaltype
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
                        (tcallparanode(hp).left.resulttype.def.deftype=floatdef) then
                      begin
                        orgfloattype:=tfloatdef(tcallparanode(hp).left.resulttype.def).typ;
                        tcallparanode(hp).left:=ctypeconvnode.create(tcallparanode(hp).left,pbestrealtype^);
                        firstpass(tcallparanode(hp).left);
                      end;
                     { when read ord,floats are functions, so they need this
                       parameter as their destination instead of being pushed }
                     if doread and
                        (ft<>ft_typed) and
                        (tcallparanode(hp).resulttype.def.deftype in [orddef,floatdef]) then
                      begin
                      end
                     else
                      begin
                        if ft=ft_typed then
                          never_copy_const_param:=true;
                        { reset data type }
                        dummycoll.paratype.reset;
                        { create temporary defs for high tree generation }
                        if doread and (is_shortstring(tcallparanode(hp).resulttype.def)) then
                          dummycoll.paratype:=openshortstringtype
                        else
                          if (is_chararray(tcallparanode(hp).resulttype.def)) then
                            dummycoll.paratype:=openchararraytype;
                        tcallparanode(hp).secondcallparan(dummycoll,false,false,false,0,0);
                        if ft=ft_typed then
                          never_copy_const_param:=false;
                      end;
                     tcallparanode(hp).right:=node;
                     if codegenerror then
                       goto myexit;

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
                            push_int(typedtyp.size);
                            saveregvars($ff);
                            if doread then
                              emitcall('FPC_TYPED_READ')
                            else
                              emitcall('FPC_TYPED_WRITE');
                       end
                     else
                       begin
                          { save current position }
                          pararesult:=tcallparanode(hp).left.resulttype.def;
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
                                   dummycoll.paratype.setdef(hp.resulttype.def);
                                   dummycoll.paratyp:=vs_value;
                                   tcallparanode(hp).secondcallparan(dummycoll,false,false,false,0,0);
                                   tcallparanode(hp).right:=node;
                                   if codegenerror then
                                     goto myexit;
                                end
                              else
                                if pararesult.deftype<>floatdef then
                                  push_int(0)
                                else
                                  push_int(-32767);
                            { a second colon para for a float ? }
                              if assigned(node) and (cpf_is_colon_para in node.callparaflags) then
                                begin
                                   hp:=node;
                                   node:=tcallparanode(node.right);
                                   tcallparanode(hp).right:=nil;
                                   dummycoll.paratype.setdef(hp.resulttype.def);
                                   dummycoll.paratyp:=vs_value;
                                   tcallparanode(hp).secondcallparan(dummycoll,false,false,false,0,0);
                                   tcallparanode(hp).right:=node;
                                   if pararesult.deftype<>floatdef then
                                     CGMessage(parser_e_illegal_colon_qualifier);
                                   if codegenerror then
                                     goto myexit;
                                end
                              else
                                begin
                                  if pararesult.deftype=floatdef then
                                    push_int(-1);
                                end;
                             { push also the real type for floats }
                              if pararesult.deftype=floatdef then
                                push_int(ord(orgfloattype));
                            end;
                          saveregvars($ff);
                          case pararesult.deftype of
                            stringdef :
                              begin
                                emitcall(rdwrprefix[doread]+tstringdef(pararesult).stringtypname);
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
                                if tfloatdef(resulttype.def).typ<>f32bit then
                                  dec(fpuvaroffset);
                                }
                                if doread then
                                  begin
                                     maybe_loadself;
                                     esireloaded:=true;
                                     StoreDirectFuncResult(tcallparanode(hp).left);
                                  end;
                              end;
                            orddef :
                              begin
                                case torddef(pararesult).typ of
                                  s8bit,s16bit,s32bit :
                                    emitcall(rdwrprefix[doread]+'SINT');
                                  u8bit,u16bit,u32bit :
                                    emitcall(rdwrprefix[doread]+'UINT');
                                  uchar :
                                    emitcall(rdwrprefix[doread]+'CHAR');
                                  uwidechar :
                                    emitcall(rdwrprefix[doread]+'WIDECHAR');
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
                                     maybe_loadself;
                                     esireloaded:=true;
                                     StoreDirectFuncResult(tcallparanode(hp).left);
                                  end;
                              end;
                          end;
                       end;
                   { load ESI in methods again }
                     popusedregisters(pushed);
                     if not(esireloaded) then
                       maybe_loadself;
                  end;
             end;
         { Insert end of writing for textfiles }
           if ft=ft_text then
             begin
               pushusedregisters(pushed,$ff);
               emit_push_mem(aktfile);
               saveregvars($ff);
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
               maybe_loadself;
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

        myexit:
           dummycoll.free;
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
        label
           myexit;
          begin
           dummycoll:=TParaItem.Create;
           dummycoll.register:=R_NO;
           pushusedregisters(pushed,$ff);
           node:=tcallparanode(left);
           is_real:=false;
           while assigned(node.right) do node:=tcallparanode(node.right);
           { if a real parameter somewhere then call REALSTR }
           if (node.left.resulttype.def.deftype=floatdef) then
            begin
              is_real:=true;
              realtype:=tfloatdef(node.left.resulttype.def).typ;
            end;

           node:=tcallparanode(left);
           { we have at least two args }
           { with at max 2 colon_para in between }

           { string arg }
           hp:=node;
           node:=tcallparanode(node.right);
           hp.right:=nil;
           dummycoll.paratyp:=vs_var;
           if is_shortstring(hp.resulttype.def) then
             dummycoll.paratype:=openshortstringtype
           else
             dummycoll.paratype:=hp.resulttype;
           procedureprefix:='FPC_'+tstringdef(hp.resulttype.def).stringtypname+'_';
           tcallparanode(hp).secondcallparan(dummycoll,false,false,false,0,0);
           if codegenerror then
            goto myexit;

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
                dummycoll.paratype.setdef(hp.resulttype.def);
                dummycoll.paratyp:=vs_value;
                tcallparanode(hp).secondcallparan(dummycoll,false,false,false,0,0);
                if codegenerror then
                  goto myexit;
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
                dummycoll.paratype.setdef(hp.resulttype.def);
                dummycoll.paratyp:=vs_value;
                tcallparanode(hp).secondcallparan(dummycoll,false,false,false,0,0);
                if codegenerror then
                  goto myexit;
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
              hp.left:=ctypeconvnode.create(hp.left,pbestrealtype^);
              firstpass(hp.left);
            end;

           { last arg longint or real }
           dummycoll.paratype.setdef(hp.resulttype.def);
           dummycoll.paratyp:=vs_value;
           tcallparanode(hp).secondcallparan(dummycoll,false,false,false,0,0);
           if codegenerror then
            goto myexit;

           saveregvars($ff);
           if is_real then
             emitcall(procedureprefix+'FLOAT')
           else
             case torddef(hp.resulttype.def).typ of
                u32bit:
                  emitcall(procedureprefix+'CARDINAL');

                u64bit:
                  emitcall(procedureprefix+'QWORD');

                s64bit:
                  emitcall(procedureprefix+'INT64');

                else
                  emitcall(procedureprefix+'LONGINT');
             end;
           popusedregisters(pushed);
           hp.free;

        myexit:
           dummycoll.free;
        end;

        Procedure Handle_Val;
        var
           hp,node,
           code_para, dest_para : tcallparanode;
           hreg,hreg2: TRegister;
           hdef: torddef;
           procedureprefix : string;
           hr, hr2: TReference;
           dummycoll : tparaitem;
           has_code, has_32bit_code, oldregisterdef: boolean;
           r : preference;
          label
            myexit;
          begin
           dummycoll:=TParaItem.Create;
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
               has_32bit_code := (torddef(tcallparanode(code_para).left.resulttype.def).typ in [u32bit,s32bit]);
             End;

          {hp = destination now, save for later use}
           dest_para := hp;

          {if EAX is already in use, it's a register variable. Since we don't
           need another register besides EAX, release the one we got}
           If hreg <> R_EAX Then ungetregister32(hreg);

          {load and push the address of the destination}
           dummycoll.paratyp:=vs_var;
           dummycoll.paratype.setdef(dest_para.resulttype.def);
           dest_para.secondcallparan(dummycoll,false,false,false,0,0);
           if codegenerror then
            goto myexit;

          {save the regvars}
           pushusedregisters(pushed,$ff);

          {now that we've already pushed the addres of dest_para.left on the
           stack, we can put the real parameters on the stack}

           If has_32bit_code Then
             Begin
               dummycoll.paratyp:=vs_var;
               dummycoll.paratype.setdef(code_para.resulttype.def);
               code_para.secondcallparan(dummycoll,false,false,false,0,0);
               if codegenerror then
                goto myexit;
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
           dummycoll.paratype.setdef(node.resulttype.def);
           node.secondcallparan(dummycoll,false,false,false,0,0);
           if codegenerror then
             goto myexit;

           Case dest_para.resulttype.def.deftype of
             floatdef:
               begin
                  procedureprefix := 'FPC_VAL_REAL_';
                  inc(fpuvaroffset);
               end;
             orddef:
               if is_64bitint(dest_para.resulttype.def) then
                 begin
                    if is_signed(dest_para.resulttype.def) then
                      procedureprefix := 'FPC_VAL_INT64_'
                    else
                      procedureprefix := 'FPC_VAL_QWORD_';
                 end
               else
                 begin
                    if is_signed(dest_para.resulttype.def) then
                      begin
                        {if we are converting to a signed number, we have to include the
                         size of the destination, so the Val function can extend the sign
                         of the result to allow proper range checking}
                        emit_const(A_PUSH,S_L,dest_para.resulttype.def.size);
                        procedureprefix := 'FPC_VAL_SINT_'
                      end
                    else
                      procedureprefix := 'FPC_VAL_UINT_';
                 end;
           End;

           saveregvars($ff);
           emitcall(procedureprefix+tstringdef(node.resulttype.def).stringtypname);
           { before disposing node we need to ungettemp !! PM }
           if node.left.location.loc in [LOC_REFERENCE,LOC_MEM] then
             ungetiftemp(node.left.location.reference);
           node.free;
           left := nil;

          {reload esi in case the dest_para/code_para is a class variable or so}
           maybe_loadself;

           If (dest_para.resulttype.def.deftype = orddef) Then
             Begin
              {store the result in a safe place, because EAX may be used by a
               register variable}
               hreg := getexplicitregister32(R_EAX);
               emit_reg_reg(A_MOV,S_L,R_EAX,hreg);
               if is_64bitint(dest_para.resulttype.def) then
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
               getexplicitregister32(R_EDI);
               emit_ref_reg(A_MOV,S_L,NewReference(hr),R_EDI);
               emit_mov_reg_loc(R_DI,code_para.left.location);
               ungetregister32(R_EDI);
               code_para.free;
             End;

          {restore the address of the result}
           getexplicitregister32(R_EDI);
           emit_reg(A_POP,S_L,R_EDI);

          {set up hr2 to a refernce with EDI as base register}
           reset_reference(hr2);
           hr2.base := R_EDI;

          {save the function result in the destination variable}
           Case dest_para.left.resulttype.def.deftype of
             floatdef:
               floatstore(tfloatdef(dest_para.left.resulttype.def).typ, hr2);
             orddef:
               Case torddef(dest_para.left.resulttype.def).typ of
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
           ungetregister32(R_EDI);
           If (cs_check_range in aktlocalswitches) and
              (dest_para.left.resulttype.def.deftype = orddef) and
              (not(is_64bitint(dest_para.left.resulttype.def))) and
            {the following has to be changed to 64bit checking, once Val
             returns 64 bit values (unless a special Val function is created
             for that)}
            {no need to rangecheck longints or cardinals on 32bit processors}
               not((torddef(dest_para.left.resulttype.def).typ = s32bit) and
                   (torddef(dest_para.left.resulttype.def).low = longint($80000000)) and
                   (torddef(dest_para.left.resulttype.def).high = $7fffffff)) and
               not((torddef(dest_para.left.resulttype.def).typ = u32bit) and
                   (torddef(dest_para.left.resulttype.def).low = 0) and
                   (torddef(dest_para.left.resulttype.def).high = longint($ffffffff))) then
             Begin
               hp:=tcallparanode(dest_para.left.getcopy);
               hp.location.loc := LOC_REGISTER;
               hp.location.register := hreg;
              {do not register this temporary def}
               OldRegisterDef := RegisterDef;
               RegisterDef := False;
               Case torddef(dest_para.left.resulttype.def).typ of
                 u8bit,u16bit,u32bit: hdef:=torddef.create(u32bit,0,longint($ffffffff));
                 s8bit,s16bit,s32bit: hdef:=torddef.create(s32bit,longint($80000000),$7fffffff);
               end;
               hp.resulttype.def := hdef;
               emitrangecheck(hp,dest_para.left.resulttype.def);
               hp.right := nil;
               hp.resulttype.def.free;
               RegisterDef := OldRegisterDef;
               hp.free;
             End;
          {dest_para.right is already nil}
           dest_para.free;
           UnGetIfTemp(hr);
        myexit:
           dummycoll.free;
        end;
{$endif not hascompilerproc}

      var
         r : preference;
         //hp : tcallparanode;
         hp2 : tstringconstnode;
         dummycoll  : tparaitem;
         l : longint;
         ispushed : boolean;
         hregister : tregister;
         lengthlab,
         otlabel,oflabel{,l1}   : tasmlabel;
         oldpushedparasize : longint;
         def : tdef;
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
                 hp2:=cstringconstnode.createstr(current_module.sourcefiles.get_file_name(aktfilepos.fileindex),st_shortstring);
                 firstpass(hp2);
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
                        S_L,newasmsymbol(tobjectdef(left.resulttype.def).vmt_mangledname),0,
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
                        tobjectdef(left.resulttype.def).vmt_offset);
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
            in_length_x :
              begin
                 secondpass(left);
                 set_location(location,left.location);
                 { length in ansi strings is at offset -8 }
                 if is_ansistring(left.resulttype.def) or
                    is_widestring(left.resulttype.def) then
                  begin
                    if left.location.loc<>LOC_REGISTER then
                     begin
                       del_location(left.location);
                       hregister:=getregister32;
                       emit_mov_loc_reg(left.location,hregister);
                     end
                    else
                     hregister:=left.location.register;
                    reset_reference(hr);
                    hr.base:=hregister;
                    hr.offset:=-8;
                    getlabel(lengthlab);
                    emit_reg_reg(A_OR,S_L,hregister,hregister);
                    emitjmp(C_Z,lengthlab);
                    emit_ref_reg(A_MOV,S_L,newreference(hr),hregister);
                    emitlab(lengthlab);
                    location.loc:=LOC_REGISTER;
                    location.register:=hregister;
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
                 case resulttype.def.size of
                   8 : opsize:=S_L;
                   4 : opsize:=S_L;
                   2 : opsize:=S_W;
                   1 : opsize:=S_B;
                 else
                   internalerror(10080);
                 end;
                 location.loc:=LOC_REGISTER;
                 if resulttype.def.size=8 then
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
                           if (resulttype.def.size=2) then
                             location.register:=reg32toreg16(location.register);
                           if (resulttype.def.size=1) then
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
                 emitrangecheck(self,resulttype.def);
              end;
            in_dec_x,
            in_inc_x :
              begin
              { set defaults }
                addvalue:=1;
                addconstant:=true;
              { load first parameter, must be a reference }
                secondpass(tcallparanode(left).left);
                case tcallparanode(left).left.resulttype.def.deftype of
                  orddef,
                 enumdef : begin
                             case tcallparanode(left).left.resulttype.def.size of
                              1 : opsize:=S_B;
                              2 : opsize:=S_W;
                              4 : opsize:=S_L;
                              8 : opsize:=S_L;
                             end;
                           end;
              pointerdef : begin
                             opsize:=S_L;
                             if is_void(tpointerdef(tcallparanode(left).left.resulttype.def).pointertype.def) then
                              addvalue:=1
                             else
                              addvalue:=tpointerdef(tcallparanode(left).left.resulttype.def).pointertype.def.size;
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
                emitrangecheck(tcallparanode(left).left,tcallparanode(left).left.resulttype.def);
              end;

            in_typeinfo_x:
               begin
                  tstoreddef(ttypenode(tcallparanode(left).left).resulttype.def).generate_rtti;
                  location.register:=getregister32;
                  new(r);
                  reset_reference(r^);
                  r^.symbol:=tstoreddef(ttypenode(tcallparanode(left).left).resulttype.def).rtti_label;
                  emit_ref_reg(A_LEA,S_L,r,location.register);
               end;

             in_finalize_x:
               begin
                  pushusedregisters(pushed,$ff);
                  { force rtti generation }
                  tstoreddef(ttypenode(tcallparanode(left).left).resulttype.def).generate_rtti;
                  { if a count is passed, push size, typeinfo and count }
                  if assigned(tcallparanode(left).right) then
                    begin
                       secondpass(tcallparanode(tcallparanode(left).right).left);
                       push_int(tcallparanode(left).left.resulttype.def.size);
                       if codegenerror then
                        exit;
                       emit_push_loc(tcallparanode(tcallparanode(left).right).left.location);
                    end;

                  { generate a reference }
                  reset_reference(hr);
                  hr.symbol:=tstoreddef(ttypenode(tcallparanode(left).left).resulttype.def).rtti_label;
                  emitpushreferenceaddr(hr);

                  { data to finalize }
                  secondpass(tcallparanode(left).left);
                  if codegenerror then
                    exit;
                  emitpushreferenceaddr(tcallparanode(left).left.location.reference);
                  saveregvars($ff);
                  if assigned(tcallparanode(left).right) then
                    emitcall('FPC_FINALIZEARRAY')
                  else
                    emitcall('FPC_FINALIZE');
                  popusedregisters(pushed);
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
{$ifndef hascompilerproc}
                  pushusedregisters(pushed,$ff);
                  emit_const(A_PUSH,S_L,tfiledef(left.resulttype.def).typedfiletype.def.size);
                  secondpass(left);
                  emitpushreferenceaddr(left.location.reference);
                  saveregvars($ff);
                  if inlinenumber=in_reset_typedfile then
                    emitcall('FPC_RESET_TYPED')
                  else
                    emitcall('FPC_REWRITE_TYPED');
                  popusedregisters(pushed);
{$else not hascompilerproc}
                  { should be removed in pass_1 (JM) }
                  internalerror(200108132);
{$endif not hascompilerproc}
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
                  def:=tcallparanode(hp).left.resulttype.def;
                  hp:=left;
                  if is_dynamic_array(def) then
                    begin
                       { get temp. space }
                       gettempofsizereference(l*4,hr);
                       { keep data start }
                       hr2:=hr;
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
                     (tstringdef(def).string_typ = st_shortstring) then
                    begin
                      dummycoll:=TParaItem.Create;
                      dummycoll.paratyp:=vs_var;
                      dummycoll.paratype:=openshortstringtype;
                      tcallparanode(hp).secondcallparan(dummycoll,false,false,false,0,0);
                      dummycoll.free;
                      if codegenerror then
                        exit;
                    end
                  else secondpass(tcallparanode(hp).left);
                  if is_dynamic_array(def) then
                    begin
                       emitpushreferenceaddr(hr2);
                       push_int(l);
                       reset_reference(hr2);
                       hr2.symbol:=tstoreddef(def).get_inittable_label;
                       emitpushreferenceaddr(hr2);
                       emitpushreferenceaddr(tcallparanode(hp).left.location.reference);
                       saveregvars($ff);
                       emitcall('FPC_DYNARR_SETLENGTH');
                       ungetiftemp(hr);
                    end
                  else
                    { must be string }
                    begin
                       case tstringdef(def).string_typ of
                          st_widestring:
                            begin
                              emitpushreferenceaddr(tcallparanode(hp).left.location.reference);
                              saveregvars($ff);
                              emitcall('FPC_WIDESTR_SETLENGTH');
                            end;
                          st_ansistring:
                            begin
                              emitpushreferenceaddr(tcallparanode(hp).left.location.reference);
                              saveregvars($ff);
                              emitcall('FPC_ANSISTR_SETLENGTH');
                            end;
                          st_shortstring:
                            begin
                              saveregvars($ff);
                              emitcall('FPC_SHORTSTR_SETLENGTH');
                            end;
                       end;
                    end;
                  popusedregisters(pushed);
               end;
{$ifndef hascompilerproc}
            in_write_x :
              handlereadwrite(false,false);
            in_writeln_x :
              handlereadwrite(false,true);
            in_read_x :
              handlereadwrite(true,false);
            in_readln_x :
              handlereadwrite(true,true);
{$else hascomppilerproc}
              in_read_x,
              in_readln_x,
              in_write_x,
              in_writeln_x :
                { should be removed in the resulttype pass already (JM) }
                internalerror(200108162);
{$endif not hascomppilerproc}
            in_str_x_string :
              begin
{$ifndef hascompilerproc}
                 handle_str;
                 maybe_loadself;
{$else not hascompilerproc}
                 { should be removed in det_resulttype (JM) }
                 internalerror(200108131);
{$endif not hascompilerproc}
              end;
            in_val_x :
              Begin
{$ifdef hascompilerproc}
                 { should be removed in det_resulttype (JM) }
                 internalerror(200108241);
{$else hascompilerproc}
                handle_val;
{$endif hascompilerproc}
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
                        begin
                          secondpass(tcallparanode(left).left);
                          emit_const_reg(asmop,S_L,
                            l,tcallparanode(left).left.location.register);
                        end;
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
                      if tsetdef(left.resulttype.def).settype=smallset then
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
                                  tcallparanode(tcallparanode(left).right).left.resulttype.def,u32bittype.def);
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
                         floatload(tfloatdef(left.resulttype.def).typ,left.location.reference);
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
  Revision 1.21  2001-08-26 13:36:58  florian
    * some cg reorganisation
    * some PPC updates

  Revision 1.20  2001/08/24 12:33:54  jonas
    * fixed big bug in handle_str that caused it to (almost) always call
      fpc_<stringtype>_longint
    * fixed small bug in handle_read_write that caused wrong warnigns about
      uninitialized vars with read(ln)
    + handle_val (processor independent val() handling)

  Revision 1.19  2001/08/23 14:28:36  jonas
    + tempcreate/ref/delete nodes (allows the use of temps in the
      resulttype and first pass)
    * made handling of read(ln)/write(ln) processor independent
    * moved processor independent handling for str and reset/rewrite-typed
      from firstpass to resulttype pass
    * changed names of helpers in text.inc to be generic for use as
      compilerprocs + added "iocheck" directive for most of them
    * reading of ordinals is done by procedures instead of functions
      because otherwise FPC_IOCHECK overwrote the result before it could
      be stored elsewhere (range checking still works)
    * compilerprocs can now be used in the system unit before they are
      implemented
    * added note to errore.msg that booleans can't be read using read/readln

  Revision 1.18  2001/08/13 15:39:52  jonas
    * made in_reset_typedfile/in_rewrite_typedfile handling processor
      independent

  Revision 1.17  2001/08/13 12:41:57  jonas
    * made code for str(x,y) completely processor independent

  Revision 1.16  2001/07/10 18:01:08  peter
    * internal length for ansistring and widestrings

  Revision 1.15  2001/07/08 21:00:18  peter
    * various widestring updates, it works now mostly without charset
      mapping supported

  Revision 1.14  2001/04/13 01:22:19  peter
    * symtable change to classes
    * range check generation and errors fixed, make cycle DEBUG=1 works
    * memory leaks fixed

  Revision 1.13  2001/04/02 21:20:37  peter
    * resulttype rewrite

  Revision 1.12  2001/03/13 11:52:48  jonas
    * fixed some memory leaks

  Revision 1.11  2000/12/25 00:07:33  peter
    + new tlinkedlist class (merge of old tstringqueue,tcontainer and
      tlinkedlist objects)

  Revision 1.10  2000/12/09 22:51:37  florian
    * helper name of val for qword fixed

  Revision 1.9  2000/12/07 17:19:46  jonas
    * new constant handling: from now on, hex constants >$7fffffff are
      parsed as unsigned constants (otherwise, $80000000 got sign extended
      and became $ffffffff80000000), all constants in the longint range
      become longints, all constants >$7fffffff and <=cardinal($ffffffff)
      are cardinals and the rest are int64's.
    * added lots of longint typecast to prevent range check errors in the
      compiler and rtl
    * type casts of symbolic ordinal constants are now preserved
    * fixed bug where the original resulttype.def wasn't restored correctly
      after doing a 64bit rangecheck

  Revision 1.8  2000/12/05 11:44:33  jonas
    + new integer regvar handling, should be much more efficient

  Revision 1.7  2000/11/29 00:30:47  florian
    * unused units removed from uses clause
    * some changes for widestrings

  Revision 1.6  2000/11/12 23:24:15  florian
    * interfaces are basically running

  Revision 1.5  2000/11/09 17:46:56  florian
    * System.TypeInfo fixed
    + System.Finalize implemented
    + some new keywords for interface support added

  Revision 1.4  2000/10/31 22:02:56  peter
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
