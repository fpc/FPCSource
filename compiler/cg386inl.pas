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
unit cg386inl;
interface

    uses
      tree;

    procedure secondinline(var p : ptree);


implementation

    uses
      globtype,systems,
      cobjects,verbose,globals,files,
      symconst,symtable,aasm,types,
      hcodegen,temp_gen,pass_1,pass_2,
      cpubase,cpuasm,
      cgai386,tgeni386,cg386cal;


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

    procedure StoreDirectFuncResult(var dest:ptree);
      var
        hp : ptree;
        hdef : porddef;
        hreg : tregister;
        hregister : tregister;
        oldregisterdef : boolean;
      begin
        { Get the accumulator first so it can't be used in the dest }
        if (dest^.resulttype^.deftype=orddef) and
          not(is_64bitint(dest^.resulttype)) then
          hregister:=getexplicitregister32(accumulator);
        { process dest }
        SecondPass(dest);
        if Codegenerror then
         exit;
        { store the value }
        Case dest^.resulttype^.deftype of
          floatdef:
            floatstore(PFloatDef(dest^.resulttype)^.typ,dest^.location.reference);
          orddef:
            begin
              if is_64bitint(dest^.resulttype) then
                begin
                   emit_movq_reg_loc(R_EDX,R_EAX,dest^.location);
                end
              else
               begin
                 Case dest^.resulttype^.size of
                  1 : hreg:=regtoreg8(hregister);
                  2 : hreg:=regtoreg16(hregister);
                  4 : hreg:=hregister;
                 End;
                 emit_mov_reg_loc(hreg,dest^.location);
                 If (cs_check_range in aktlocalswitches) and
                    {no need to rangecheck longints or cardinals on 32bit processors}
                    not((porddef(dest^.resulttype)^.typ = s32bit) and
                        (porddef(dest^.resulttype)^.low = $80000000) and
                        (porddef(dest^.resulttype)^.high = $7fffffff)) and
                    not((porddef(dest^.resulttype)^.typ = u32bit) and
                        (porddef(dest^.resulttype)^.low = 0) and
                        (porddef(dest^.resulttype)^.high = $ffffffff)) then
                  Begin
                    {do not register this temporary def}
                    OldRegisterDef := RegisterDef;
                    RegisterDef := False;
                    hdef:=nil;
                    Case PordDef(dest^.resulttype)^.typ of
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
                    hp := genzeronode(nothingn);
                    hp^.location.loc := LOC_REGISTER;
                    hp^.location.register := hreg;
                    if assigned(hdef) then
                      hp^.resulttype:=hdef
                    else
                      hp^.resulttype:=dest^.resulttype;
                    { emit the range check }
                    emitrangecheck(hp,dest^.resulttype);
                    hp^.right := nil;
                    if assigned(hdef) then
                      Dispose(hdef, Done);
                    RegisterDef := OldRegisterDef;
                    disposetree(hp);
                  End;
                 ungetregister(hregister);
               end;
            End;
          else
            internalerror(66766766);
        end;
        { free used registers }
        del_locref(dest^.location);
      end;


    procedure secondinline(var p : ptree);
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


      procedure handlereadwrite(doread,doln : boolean);
      { produces code for READ(LN) and WRITE(LN) }

        procedure loadstream;
          const
            io:array[boolean] of string[7]=('_OUTPUT','_INPUT');
          var
            r : preference;
          begin
            new(r);
            reset_reference(r^);
            r^.symbol:=newasmsymbol(
            'U_'+upper(target_info.system_unit)+io[doread]);
{$ifndef noAllocEdi}
            getexplicitregister32(R_EDI);
{$endif noAllocEdi}
            emit_ref_reg(A_LEA,S_L,r,R_EDI)
          end;

        const
           rdwrprefix:array[boolean] of string[15]=('FPC_WRITE_TEXT_','FPC_READ_TEXT_');
        var
           node,hp    : ptree;
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
                getlabel(iolabel);
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
           if p^.left=nil then
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
                node:=reversparameter(p^.left);

                p^.left := node;
                npara := nb_para;
                { calculate data variable }
                { is first parameter a file type ? }
                if node^.left^.resulttype^.deftype=filedef then
                  begin
                     ft:=pfiledef(node^.left^.resulttype)^.filetyp;
                     if ft=ft_typed then
                       typedtyp:=pfiledef(node^.left^.resulttype)^.typedfiletype.def;
                     secondpass(node^.left);
                     if codegenerror then
                       exit;

                     { save reference in temporary variables }
                     if node^.left^.location.loc<>LOC_REFERENCE then
                       begin
                          CGMessage(cg_e_illegal_expression);
                          exit;
                       end;
{$ifndef noAllocEdi}
                     getexplicitregister32(R_EDI);
{$endif noAllocEdi}

                     emit_ref_reg(A_LEA,S_L,newreference(node^.left^.location.reference),R_EDI);

                     { skip to the next parameter }
                     node:=node^.right;
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
                     node:=node^.right;
                     hp^.right:=nil;
                     if hp^.is_colon_para then
                       CGMessage(parser_e_illegal_colon_qualifier);
                     { when float is written then we need bestreal to be pushed
                       convert here else we loose the old float type }
                     if (not doread) and
                        (ft<>ft_typed) and
                        (hp^.left^.resulttype^.deftype=floatdef) then
                      begin
                        orgfloattype:=pfloatdef(hp^.left^.resulttype)^.typ;
                        hp^.left:=gentypeconvnode(hp^.left,bestrealdef^);
                        firstpass(hp^.left);
                      end;
                     { when read ord,floats are functions, so they need this
                       parameter as their destination instead of being pushed }
                     if doread and
                        (ft<>ft_typed) and
                        (hp^.resulttype^.deftype in [orddef,floatdef]) then
                      begin
                      end
                     else
                      begin
                        if ft=ft_typed then
                          never_copy_const_param:=true;
                        { reset data type }
                        dummycoll.paratype.reset;
                        { create temporary defs for high tree generation }
                        if doread and (is_shortstring(hp^.resulttype)) then
                          dummycoll.paratype.setdef(openshortstringdef)
                        else
                          if (is_chararray(hp^.resulttype)) then
                            dummycoll.paratype.setdef(openchararraydef);
                        secondcallparan(hp,@dummycoll,false,false,false,0);
                        if ft=ft_typed then
                          never_copy_const_param:=false;
                      end;
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
                                   dummycoll.paratype.setdef(hp^.resulttype);
                                   dummycoll.paratyp:=vs_value;
                                   secondcallparan(hp,@dummycoll,false,false,false,0);
                                   hp^.right:=node;
                                   if codegenerror then
                                     exit;
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
                                   dummycoll.paratype.setdef(hp^.resulttype);
                                   dummycoll.paratyp:=vs_value;
                                   secondcallparan(hp,@dummycoll,false,false,false,0);
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
                                if pfloatdef(p^.resulttype)^.typ<>f32bit then
                                  dec(fpuvaroffset);
                                }
                                if doread then
                                  begin
                                     maybe_loadesi;
                                     esireloaded:=true;
                                     StoreDirectFuncResult(hp^.left);
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
                                     StoreDirectFuncResult(hp^.left);
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
           dummycoll : tparaitem;
           is_real : boolean;
           realtype : tfloattype;
           procedureprefix : string;

          begin
           dummycoll.init;
           dummycoll.register:=R_NO;
           pushusedregisters(pushed,$ff);
           node:=p^.left;
           is_real:=false;
           while assigned(node^.right) do node:=node^.right;
           { if a real parameter somewhere then call REALSTR }
           if (node^.left^.resulttype^.deftype=floatdef) then
            begin
              is_real:=true;
              realtype:=pfloatdef(node^.left^.resulttype)^.typ;
            end;

           node:=p^.left;
           { we have at least two args }
           { with at max 2 colon_para in between }

           { string arg }
           hp:=node;
           node:=node^.right;
           hp^.right:=nil;
           dummycoll.paratyp:=vs_var;
           if is_shortstring(hp^.resulttype) then
             dummycoll.paratype.setdef(openshortstringdef)
           else
             dummycoll.paratype.setdef(hp^.resulttype);
           procedureprefix:='FPC_'+pstringdef(hp^.resulttype)^.stringtypname+'_';
           secondcallparan(hp,@dummycoll,false,false,false,0);
           if codegenerror then
             exit;

           dummycoll.paratyp:=vs_const;
           disposetree(p^.left);
           p^.left:=nil;
           { second arg }
           hp:=node;
           node:=node^.right;
           hp^.right:=nil;

           { if real push real type }
           if is_real then
             push_int(ord(realtype));

           { frac  para }
           if hp^.is_colon_para and assigned(node) and
              node^.is_colon_para then
             begin
                dummycoll.paratype.setdef(hp^.resulttype);
                dummycoll.paratyp:=vs_value;
                secondcallparan(hp,@dummycoll,false
                  ,false,false,0
                  );
                if codegenerror then
                  exit;
                disposetree(hp);
                hp:=node;
                node:=node^.right;
                hp^.right:=nil;
             end
           else
             if is_real then
             push_int(-1);

           { third arg, length only if is_real }
           if hp^.is_colon_para then
             begin
                dummycoll.paratype.setdef(hp^.resulttype);
                dummycoll.paratyp:=vs_value;
                secondcallparan(hp,@dummycoll,false
                  ,false,false,0
                  );
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

           { Convert float to bestreal }
           if is_real then
            begin
              hp^.left:=gentypeconvnode(hp^.left,bestrealdef^);
              firstpass(hp^.left);
            end;

           { last arg longint or real }
           dummycoll.paratype.setdef(hp^.resulttype);
           dummycoll.paratyp:=vs_value;
           secondcallparan(hp,@dummycoll,false
             ,false,false,0
             );
           if codegenerror then
             exit;

           if is_real then
             emitcall(procedureprefix+'FLOAT')
           else
             case porddef(hp^.resulttype)^.typ of
                u32bit:
                  emitcall(procedureprefix+'CARDINAL');

                u64bit:
                  emitcall(procedureprefix+'QWORD');

                s64bit:
                  emitcall(procedureprefix+'INT64');

                else
                  emitcall(procedureprefix+'LONGINT');
             end;
           disposetree(hp);

           popusedregisters(pushed);
        end;


        Procedure Handle_Val;
        var
           hp,node, code_para, dest_para : ptree;
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
           node:=p^.left;
           hp:=node;
           node:=node^.right;
           hp^.right:=nil;
          {if we have 3 parameters, we have a code parameter}
           has_code := Assigned(node^.right);
           has_32bit_code := false;
           reset_reference(hr);
           hreg := R_NO;

           If has_code then
             Begin
               {code is an orddef, that's checked in tcinl}
               code_para := hp;
               hp := node;
               node := node^.right;
               hp^.right := nil;
               has_32bit_code := (porddef(code_para^.left^.resulttype)^.typ in [u32bit,s32bit]);
             End;

          {hp = destination now, save for later use}
           dest_para := hp;

          {if EAX is already in use, it's a register variable. Since we don't
           need another register besides EAX, release the one we got}
           If hreg <> R_EAX Then ungetregister32(hreg);

          {load and push the address of the destination}
           dummycoll.paratyp:=vs_var;
           dummycoll.paratype.setdef(dest_para^.resulttype);
           secondcallparan(dest_para,@dummycoll,false,false,false,0);
           if codegenerror then
             exit;

          {save the regvars}
           pushusedregisters(pushed,$ff);

          {now that we've already pushed the addres of dest_para^.left on the
           stack, we can put the real parameters on the stack}

           If has_32bit_code Then
             Begin
               dummycoll.paratyp:=vs_var;
               dummycoll.paratype.setdef(code_para^.resulttype);
               secondcallparan(code_para,@dummycoll,false,false,false,0);
               if codegenerror then
                 exit;
               Disposetree(code_para);
             End
           Else
             Begin
           {only 32bit code parameter is supported, so fake one}
               GetTempOfSizeReference(4,hr);
               emitpushreferenceaddr(hr);
             End;

          {node = first parameter = string}
           dummycoll.paratyp:=vs_const;
           dummycoll.paratype.setdef(node^.resulttype);
           secondcallparan(node,@dummycoll,false,false,false,0);
           if codegenerror then
             exit;

           Case dest_para^.resulttype^.deftype of
             floatdef:
               begin
                  procedureprefix := 'FPC_VAL_REAL_';
                  if pfloatdef(p^.resulttype)^.typ<>f32bit then
                    inc(fpuvaroffset);
               end;
             orddef:
               if is_64bitint(dest_para^.resulttype) then
                 begin
                    if is_signed(dest_para^.resulttype) then
                      procedureprefix := 'FPC_VAL_INT64_'
                    else
                      procedureprefix := 'FPC_VAL_QWORD_';
                 end
               else
                 begin
                    if is_signed(dest_para^.resulttype) then
                      begin
                        {if we are converting to a signed number, we have to include the
                         size of the destination, so the Val function can extend the sign
                         of the result to allow proper range checking}
                        emit_const(A_PUSH,S_L,dest_para^.resulttype^.size);
                        procedureprefix := 'FPC_VAL_SINT_'
                      end
                    else
                      procedureprefix := 'FPC_VAL_UINT_';
                 end;
           End;
           emitcall(procedureprefix+pstringdef(node^.resulttype)^.stringtypname);
           { before disposing node we need to ungettemp !! PM }
           if node^.left^.location.loc in [LOC_REFERENCE,LOC_MEM] then
             ungetiftemp(node^.left^.location.reference);
           disposetree(node);
           p^.left := nil;

          {reload esi in case the dest_para/code_para is a class variable or so}
           maybe_loadesi;

           If (dest_para^.resulttype^.deftype = orddef) Then
             Begin
              {store the result in a safe place, because EAX may be used by a
               register variable}
               hreg := getexplicitregister32(R_EAX);
               emit_reg_reg(A_MOV,S_L,R_EAX,hreg);
               if is_64bitint(dest_para^.resulttype) then
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
               secondpass(code_para^.left);
              {move the code to its destination}
{$ifndef noAllocEdi}
               getexplicitregister32(R_EDI);
{$endif noAllocEdi}
               emit_ref_reg(A_MOV,S_L,NewReference(hr),R_EDI);
               emit_mov_reg_loc(R_DI,code_para^.left^.location);
{$ifndef noAllocEdi}
               ungetregister32(R_EDI);
{$endif noAllocEdi}
               Disposetree(code_para);
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
           Case dest_para^.left^.resulttype^.deftype of
             floatdef:
               floatstore(PFloatDef(dest_para^.left^.resulttype)^.typ, hr2);
             orddef:
               Case PordDef(dest_para^.left^.resulttype)^.typ of
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
              (dest_para^.left^.resulttype^.deftype = orddef) and
              (not(is_64bitint(dest_para^.left^.resulttype))) and
            {the following has to be changed to 64bit checking, once Val
             returns 64 bit values (unless a special Val function is created
             for that)}
            {no need to rangecheck longints or cardinals on 32bit processors}
               not((porddef(dest_para^.left^.resulttype)^.typ = s32bit) and
                   (porddef(dest_para^.left^.resulttype)^.low = $80000000) and
                   (porddef(dest_para^.left^.resulttype)^.high = $7fffffff)) and
               not((porddef(dest_para^.left^.resulttype)^.typ = u32bit) and
                   (porddef(dest_para^.left^.resulttype)^.low = 0) and
                   (porddef(dest_para^.left^.resulttype)^.high = $ffffffff)) then
             Begin
               hp := getcopy(dest_para^.left);
               hp^.location.loc := LOC_REGISTER;
               hp^.location.register := hreg;
              {do not register this temporary def}
               OldRegisterDef := RegisterDef;
               RegisterDef := False;
               Case PordDef(dest_para^.left^.resulttype)^.typ of
                 u8bit,u16bit,u32bit: new(hdef,init(u32bit,0,$ffffffff));
                 s8bit,s16bit,s32bit: new(hdef,init(s32bit,$80000000,$7fffffff));
               end;
               hp^.resulttype := hdef;
               emitrangecheck(hp,dest_para^.left^.resulttype);
               hp^.right := nil;
               Dispose(hp^.resulttype, Done);
               RegisterDef := OldRegisterDef;
               disposetree(hp);
             End;
          {dest_para^.right is already nil}
           disposetree(dest_para);
           UnGetIfTemp(hr);
        end;

      var
         r : preference;
         hp : ptree;
         l : longint;
         ispushed : boolean;
         hregister : tregister;
         otlabel,oflabel,l1   : pasmlabel;
         oldpushedparasize : longint;

      begin
      { save & reset pushedparasize }
         oldpushedparasize:=pushedparasize;
         pushedparasize:=0;
         case p^.inlinenumber of
            in_assert_x_y:
              begin
                 { the node should be removed in the firstpass }
                 if not (cs_do_assertion in aktlocalswitches) then
                  internalerror(7123458);
                 otlabel:=truelabel;
                 oflabel:=falselabel;
                 getlabel(truelabel);
                 getlabel(falselabel);
                 secondpass(p^.left^.left);
                 maketojumpbool(p^.left^.left);
                 emitlab(falselabel);
                 { erroraddr }
                 emit_reg(A_PUSH,S_L,R_EBP);
                 { lineno }
                 emit_const(A_PUSH,S_L,aktfilepos.line);
                 { filename string }
                 hp:=genstringconstnode(current_module^.sourcefiles^.get_file_name(aktfilepos.fileindex),st_shortstring);
                 secondpass(hp);
                 if codegenerror then
                  exit;
                 emitpushreferenceaddr(hp^.location.reference);
                 disposetree(hp);
                 { push msg }
                 secondpass(p^.left^.right^.left);
                 emitpushreferenceaddr(p^.left^.right^.left^.location.reference);
                 { call }
                 emitcall('FPC_ASSERT');
                 emitlab(truelabel);
                 truelabel:=otlabel;
                 falselabel:=oflabel;
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
                          p^.location.register:=reg32toreg16(getregister32);
                          emit_reg_reg(A_MOV,S_W,p^.left^.location.register,
                            p^.location.register);
                       end
                     else
                       begin
                          del_reference(p^.left^.location.reference);
                          p^.location.register:=reg32toreg16(getregister32);
                          emit_ref_reg(A_MOV,S_W,newreference(p^.left^.location.reference),
                            p^.location.register);
                       end;
                   end
                 else p^.location.register:=p^.left^.location.register;
                 if p^.inlinenumber=in_hi_word then
                   emit_const_reg(A_SHR,S_W,8,p^.location.register);
                 p^.location.register:=reg16toreg8(p^.location.register);
              end;
            in_sizeof_x,
            in_typeof_x :
              begin
                 { for both cases load vmt }
                 if p^.left^.treetype=typen then
                   begin
                      p^.location.register:=getregister32;
                      emit_sym_ofs_reg(A_MOV,
                        S_L,newasmsymbol(pobjectdef(p^.left^.resulttype)^.vmt_mangledname),0,
                        p^.location.register);
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
                      emit_ref_reg(A_MOV,S_L,
                      newreference(p^.left^.location.reference),
                        p^.location.register);
                   end;
                 { in sizeof load size }
                 if p^.inlinenumber=in_sizeof_x then
                   begin
                      new(r);
                      reset_reference(r^);
                      r^.base:=p^.location.register;
                      emit_ref_reg(A_MOV,S_L,r,
                        p^.location.register);
                   end;
              end;
            in_lo_long,
            in_hi_long :
              begin
                 secondpass(p^.left);
                 p^.location.loc:=LOC_REGISTER;
                 if p^.left^.location.loc<>LOC_REGISTER then
                   begin
                      if p^.left^.location.loc=LOC_CREGISTER then
                        begin
                           p^.location.register:=getregister32;
                           emit_reg_reg(A_MOV,S_L,p^.left^.location.register,
                             p^.location.register);
                        end
                      else
                        begin
                           del_reference(p^.left^.location.reference);
                           p^.location.register:=getregister32;
                           emit_ref_reg(A_MOV,S_L,newreference(p^.left^.location.reference),
                             p^.location.register);
                        end;
                   end
                 else p^.location.register:=p^.left^.location.register;
                 if p^.inlinenumber=in_hi_long then
                   emit_const_reg(A_SHR,S_L,16,p^.location.register);
                 p^.location.register:=reg32toreg16(p^.location.register);
              end;
            in_lo_qword,
            in_hi_qword:
              begin
                 secondpass(p^.left);
                 p^.location.loc:=LOC_REGISTER;
                 case p^.left^.location.loc of
                    LOC_CREGISTER:
                      begin
                         p^.location.register:=getregister32;
                         if p^.inlinenumber=in_hi_qword then
                           emit_reg_reg(A_MOV,S_L,p^.left^.location.registerhigh,
                             p^.location.register)
                         else
                           emit_reg_reg(A_MOV,S_L,p^.left^.location.registerlow,
                             p^.location.register)
                      end;
                    LOC_MEM,LOC_REFERENCE:
                      begin
                         del_reference(p^.left^.location.reference);
                         p^.location.register:=getregister32;
                         r:=newreference(p^.left^.location.reference);
                         if p^.inlinenumber=in_hi_qword then
                           inc(r^.offset,4);
                         emit_ref_reg(A_MOV,S_L,
                           r,p^.location.register);
                      end;
                    LOC_REGISTER:
                      begin
                         if p^.inlinenumber=in_hi_qword then
                           begin
                              p^.location.register:=p^.left^.location.registerhigh;
                              ungetregister32(p^.left^.location.registerlow);
                           end
                         else
                           begin
                              p^.location.register:=p^.left^.location.registerlow;
                              ungetregister32(p^.left^.location.registerhigh);
                           end;
                      end;
                 end;
              end;
            in_length_string :
              begin
                 secondpass(p^.left);
                 set_location(p^.location,p^.left^.location);
                 { length in ansi strings is at offset -8 }
                 if is_ansistring(p^.left^.resulttype) then
                   dec(p^.location.reference.offset,8)
                 { char is always 1, so make it a constant value }
                 else if is_char(p^.left^.resulttype) then
                   begin
                     clear_location(p^.location);
                     p^.location.loc:=LOC_MEM;
                     p^.location.reference.is_immediate:=true;
                     p^.location.reference.offset:=1;
                   end;
              end;
            in_pred_x,
            in_succ_x:
              begin
                 secondpass(p^.left);
                 if not (cs_check_overflow in aktlocalswitches) then
                   if p^.inlinenumber=in_pred_x then
                     asmop:=A_DEC
                   else
                     asmop:=A_INC
                 else
                   if p^.inlinenumber=in_pred_x then
                     asmop:=A_SUB
                   else
                     asmop:=A_ADD;
                 case p^.resulttype^.size of
                   8 : opsize:=S_L;
                   4 : opsize:=S_L;
                   2 : opsize:=S_W;
                   1 : opsize:=S_B;
                 else
                   internalerror(10080);
                 end;
                 p^.location.loc:=LOC_REGISTER;
                 if p^.resulttype^.size=8 then
                   begin
                      if p^.left^.location.loc<>LOC_REGISTER then
                        begin
                           if p^.left^.location.loc=LOC_CREGISTER then
                             begin
                                p^.location.registerlow:=getregister32;
                                p^.location.registerhigh:=getregister32;
                                emit_reg_reg(A_MOV,opsize,p^.left^.location.registerlow,
                                  p^.location.registerlow);
                                emit_reg_reg(A_MOV,opsize,p^.left^.location.registerhigh,
                                  p^.location.registerhigh);
                             end
                           else
                             begin
                                del_reference(p^.left^.location.reference);
                                p^.location.registerlow:=getregister32;
                                p^.location.registerhigh:=getregister32;
                                emit_ref_reg(A_MOV,opsize,newreference(p^.left^.location.reference),
                                  p^.location.registerlow);
                                r:=newreference(p^.left^.location.reference);
                                inc(r^.offset,4);
                                emit_ref_reg(A_MOV,opsize,r,
                                  p^.location.registerhigh);
                             end;
                        end
                      else
                        begin
                           p^.location.registerhigh:=p^.left^.location.registerhigh;
                           p^.location.registerlow:=p^.left^.location.registerlow;
                        end;
                      if p^.inlinenumber=in_succ_x then
                        begin
                           emit_const_reg(A_ADD,opsize,1,
                             p^.location.registerlow);
                           emit_const_reg(A_ADC,opsize,0,
                             p^.location.registerhigh);
                        end
                      else
                        begin
                           emit_const_reg(A_SUB,opsize,1,
                             p^.location.registerlow);
                           emit_const_reg(A_SBB,opsize,0,
                             p^.location.registerhigh);
                        end;
                   end
                 else
                   begin
                      if p^.left^.location.loc<>LOC_REGISTER then
                        begin
                           { first, we've to release the source location ... }
                           if p^.left^.location.loc in [LOC_MEM,LOC_REFERENCE] then
                             del_reference(p^.left^.location.reference);

                           p^.location.register:=getregister32;
                           if (p^.resulttype^.size=2) then
                             p^.location.register:=reg32toreg16(p^.location.register);
                           if (p^.resulttype^.size=1) then
                             p^.location.register:=reg32toreg8(p^.location.register);
                           if p^.left^.location.loc=LOC_CREGISTER then
                             emit_reg_reg(A_MOV,opsize,p^.left^.location.register,
                               p^.location.register)
                           else
                           if p^.left^.location.loc=LOC_FLAGS then
                             emit_flag2reg(p^.left^.location.resflags,p^.location.register)
                           else
                             emit_ref_reg(A_MOV,opsize,newreference(p^.left^.location.reference),
                               p^.location.register);
                        end
                      else p^.location.register:=p^.left^.location.register;
                      if not (cs_check_overflow in aktlocalswitches) then
                        emit_reg(asmop,opsize,
                        p^.location.register)
                      else
                        emit_const_reg(asmop,opsize,1,
                        p^.location.register);
                   end;
                 emitoverflowcheck(p);
                 emitrangecheck(p,p^.resulttype);
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
                              8 : opsize:=S_L;
                             end;
                           end;
              pointerdef : begin
                             opsize:=S_L;
                             if porddef(ppointerdef(p^.left^.left^.resulttype)^.pointertype.def)=voiddef then
                              addvalue:=1
                             else
                              addvalue:=ppointerdef(p^.left^.left^.resulttype)^.pointertype.def^.size;
                           end;
                else
                 internalerror(10081);
                end;
              { second argument specified?, must be a s32bit in register }
                if assigned(p^.left^.right) then
                 begin
                   ispushed:=maybe_push(p^.left^.right^.left^.registers32,p^.left^.left,false);
                   secondpass(p^.left^.right^.left);
                   if ispushed then
                     restore(p^.left^.left,false);
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
                                    del_reference(p^.left^.right^.left^.location.reference);
                                    hregister:=getregister32;
                                    emit_ref_reg(A_MOV,S_L,
                                      newreference(p^.left^.right^.left^.location.reference),hregister);
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
                        if p^.left^.left^.location.loc=LOC_CREGISTER then
                          emit_reg(incdecop[p^.inlinenumber],opsize,
                            p^.left^.left^.location.register)
                        else
                          emit_ref(incdecop[p^.inlinenumber],opsize,
                            newreference(p^.left^.left^.location.reference))
                     end
                   else
                     begin
                        if p^.left^.left^.location.loc=LOC_CREGISTER then
                          emit_const_reg(addsubop[p^.inlinenumber],opsize,
                            addvalue,p^.left^.left^.location.register)
                        else
                          emit_const_ref(addsubop[p^.inlinenumber],opsize,
                            addvalue,newreference(p^.left^.left^.location.reference));
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
                    if p^.left^.left^.location.loc=LOC_CREGISTER then
                      emit_reg_reg(addsubop[p^.inlinenumber],opsize,
                        hregister,p^.left^.left^.location.register)
                    else
                      emit_reg_ref(addsubop[p^.inlinenumber],opsize,
                        hregister,newreference(p^.left^.left^.location.reference));
                    case opsize of
                      S_B : hregister:=reg8toreg32(hregister);
                      S_W : hregister:=reg16toreg32(hregister);
                    end;
                   ungetregister32(hregister);
                 end;
                emitoverflowcheck(p^.left^.left);
                emitrangecheck(p^.left^.left,p^.left^.left^.resulttype);
              end;
            in_assigned_x :
              begin
                 secondpass(p^.left^.left);
                 p^.location.loc:=LOC_FLAGS;
                 if (p^.left^.left^.location.loc in [LOC_REGISTER,LOC_CREGISTER]) then
                   begin
                      emit_reg_reg(A_OR,S_L,
                        p^.left^.left^.location.register,
                        p^.left^.left^.location.register);
                      ungetregister32(p^.left^.left^.location.register);
                   end
                 else
                   begin
                      emit_const_ref(A_CMP,S_L,0,
                        newreference(p^.left^.left^.location.reference));
                      del_reference(p^.left^.left^.location.reference);
                   end;
                 p^.location.resflags:=F_NE;
              end;
             in_reset_typedfile,in_rewrite_typedfile :
               begin
                  pushusedregisters(pushed,$ff);
                  emit_const(A_PUSH,S_L,pfiledef(p^.left^.resulttype)^.typedfiletype.def^.size);
                  secondpass(p^.left);
                  emitpushreferenceaddr(p^.left^.location.reference);
                  if p^.inlinenumber=in_reset_typedfile then
                    emitcall('FPC_RESET_TYPED')
                  else
                    emitcall('FPC_REWRITE_TYPED');
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
                 secondpass(p^.left^.left);
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
                           emit_const_ref(asmop,S_L,
                             l,newreference(p^.left^.left^.location.reference));
                           del_reference(p^.left^.left^.location.reference);
                        end
                      else
                        { LOC_CREGISTER }
                        emit_const_reg(asmop,S_L,
                          l,p^.left^.left^.location.register);
                   end
                 else
                   begin
                      { generate code for the element to set }
                      ispushed:=maybe_push(p^.left^.right^.left^.registers32,p^.left^.left,false);
                      secondpass(p^.left^.right^.left);
                      if ispushed then
                        restore(p^.left^.left,false);
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
{$ifndef noAllocEdi}
                                getexplicitregister32(R_EDI);
{$endif noAllocEdi}
                                hregister:=R_EDI;
                                opsize:=def2def_opsize(p^.left^.right^.left^.resulttype,u32bitdef);
                                if opsize in [S_B,S_W,S_L] then
                                 op:=A_MOV
                                else
                                 op:=A_MOVZX;
                                emit_ref_reg(op,opsize,
                                  newreference(p^.left^.right^.left^.location.reference),R_EDI);
                             end;
                          if (p^.left^.left^.location.loc=LOC_REFERENCE) then
                            emit_reg_ref(asmop,S_L,hregister,
                              newreference(p^.left^.left^.location.reference))
                          else
                            emit_reg_reg(asmop,S_L,hregister,
                              p^.left^.left^.location.register);
{$ifndef noAllocEdi}
                        if hregister = R_EDI then
                          ungetregister32(R_EDI);
{$endif noAllocEdi}
                        end
                      else
                        begin
                           pushsetelement(p^.left^.right^.left);
                           { normset is allways a ref }
                           emitpushreferenceaddr(p^.left^.left^.location.reference);
                           if p^.inlinenumber=in_include_x_y then
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
                 secondpass(p^.left);
                 case p^.left^.location.loc of
                    LOC_FPU:
                      ;
                    LOC_CFPUREGISTER:
                      begin
                         emit_reg(A_FLD,S_NO,
                           correct_fpuregister(p^.left^.location.register,fpuvaroffset));
                         inc(fpuvaroffset);
                      end;
                    LOC_REFERENCE,LOC_MEM:
                      begin
                         floatload(pfloatdef(p^.left^.resulttype)^.typ,p^.left^.location.reference);
                         del_reference(p^.left^.location.reference);
                      end
                    else
                      internalerror(309991);
                 end;
                 case p^.inlinenumber of
                    in_sin_extended,
                    in_cos_extended:
                      begin
                         getlabel(l1);
                         if p^.inlinenumber=in_sin_extended then
                           emit_none(A_FSIN,S_NO)
                         else
                           emit_none(A_FCOS,S_NO);
                         {
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
                         emit_reg(A_FLD,S_NO,R_ST0);
                         emit_none(A_FMULP,S_NO);
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
                 if p^.left^.location.loc=LOC_REGISTER then
                   begin
                      {!!!!!!!}
                   end
                 else if p^.left^.left^.location.loc=LOC_REGISTER then
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

end.
{
  $Log$
  Revision 1.91  2000-01-24 20:11:10  florian
    * internalerror 10 for inlined math functions fixed

  Revision 1.90  2000/01/09 23:16:05  peter
    * added st_default stringtype
    * genstringconstnode extended with stringtype parameter using st_default
      will do the old behaviour

  Revision 1.89  2000/01/09 12:35:00  jonas
    * changed edi allocation to use getexplicitregister32/ungetregister
      (adapted tgeni386 a bit for this) and enabled it by default
    * fixed very big and stupid bug of mine in cg386mat that broke the
      include() code (and make cycle :( ) if you compiled without
      -dnewoptimizations

  Revision 1.88  2000/01/09 01:44:19  jonas
    + (de)allocation info for EDI to fix reported bug on mailinglist.
      Also some (de)allocation info for ESI added. Between -dallocEDI
      because at this time of the night bugs could easily slip in ;)

  Revision 1.87  2000/01/07 01:14:20  peter
    * updated copyright to 2000

  Revision 1.86  1999/12/22 01:01:46  peter
    - removed freelabel()
    * added undefined label detection in internal assembler, this prevents
      a lot of ld crashes and wrong .o files
    * .o files aren't written anymore if errors have occured
    * inlining of assembler labels is now correct

  Revision 1.85  1999/12/20 21:42:35  pierre
    + dllversion global variable
    * FPC_USE_CPREFIX code removed, not necessary anymore
      as we use .edata direct writing by default now.

  Revision 1.84  1999/12/14 10:17:40  florian
    * fixed an internalerror 10 with pred(...)

  Revision 1.83  1999/12/02 12:38:45  florian
    + added support for succ/pred(<qword/int64>)

  Revision 1.82  1999/12/01 12:42:31  peter
    * fixed bug 698
    * removed some notes about unused vars

  Revision 1.81  1999/11/30 10:40:42  peter
    + ttype, tsymlist

  Revision 1.80  1999/11/29 00:30:06  pierre
   * fix for form bug 699

  Revision 1.79  1999/11/20 01:22:18  pierre
    + cond FPC_USE_CPREFIX (needs also some RTL changes)
      this allows to use unit global vars as DLL exports
      (the underline prefix seems needed by dlltool)

  Revision 1.78  1999/11/09 22:54:45  peter
    * fixed wrong asm with inc(qword), but not it's not correctly supported

  Revision 1.77  1999/11/06 14:34:17  peter
    * truncated log to 20 revs

  Revision 1.76  1999/10/29 15:28:51  peter
    * fixed assert, the tree is now disposed in firstpass if assertions
      are off.

  Revision 1.75  1999/10/26 12:30:40  peter
    * const parameter is now checked
    * better and generic check if a node can be used for assigning
    * export fixes
    * procvar equal works now (it never had worked at least from 0.99.8)
    * defcoll changed to linkedlist with pparaitem so it can easily be
      walked both directions

  Revision 1.74  1999/10/21 16:41:38  florian
    * problems with readln fixed: esi wasn't restored correctly when
      reading ordinal fields of objects futher the register allocation
      didn't take care of the extra register when reading ordinal values
    * enumerations can now be used in constant indexes of properties

  Revision 1.73  1999/09/28 20:48:23  florian
    * fixed bug 610
    + added $D- for TP in symtable.pas else it can't be compiled anymore
      (too much symbols :()

  Revision 1.72  1999/09/26 13:26:05  florian
    * exception patch of Romio nevertheless the excpetion handling
      needs some corections regarding register saving
    * gettempansistring is again a procedure

  Revision 1.71  1999/09/16 07:52:37  pierre
    * FLDPI must increment fpuvaroffset

  Revision 1.70  1999/09/15 20:35:38  florian
    * small fix to operator overloading when in MMX mode
    + the compiler uses now fldz and fld1 if possible
    + some fixes to floating point registers
    + some math. functions (arctan, ln, sin, cos, sqrt, sqr, pi) are now inlined
    * .... ???

  Revision 1.69  1999/08/28 15:34:16  florian
    * bug 519 fixed

  Revision 1.68  1999/08/19 13:08:47  pierre
   * emit_??? used

  Revision 1.67  1999/08/10 13:21:08  pierre
   * fpuvaroffset not increased for f32bit float type

  Revision 1.66  1999/08/10 12:47:53  pierre
   * fpuvaroffset problems solved

  Revision 1.65  1999/08/04 00:22:47  florian
    * renamed i386asm and i386base to cpuasm and cpubase

  Revision 1.64  1999/08/03 22:02:42  peter
    * moved bitmask constants to sets
    * some other type/const renamings

  Revision 1.63  1999/07/23 16:05:18  peter
    * alignment is now saved in the symtable
    * C alignment added for records
    * PPU version increased to solve .12 <-> .13 probs

  Revision 1.62  1999/07/05 20:13:10  peter
    * removed temp defines

  Revision 1.61  1999/07/03 14:14:27  florian
    + start of val(int64/qword)
    * longbool, wordbool constants weren't written, fixed

  Revision 1.60  1999/07/01 15:49:09  florian
    * int64/qword type release
    + lo/hi for int64/qword

  Revision 1.59  1999/06/21 16:33:27  jonas
    * fixed include() with smallsets

  Revision 1.58  1999/06/11 11:44:56  peter
  *** empty log message ***

  Revision 1.57  1999/06/02 10:11:43  florian
    * make cycle fixed i.e. compilation with 0.99.10
    * some fixes for qword
    * start of register calling conventions

}