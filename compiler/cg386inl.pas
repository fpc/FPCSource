{
    $Id$
    Copyright (c) 1993-98 by Florian Klaempfl

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
      symtable,aasm,types,
      hcodegen,temp_gen,pass_2,
{$ifdef ag386bin}
      i386base,i386asm,
{$else}
      i386,
{$endif}
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

    procedure StoreDirectFuncResult(dest:ptree);
      var
        hp : ptree;
        hdef : porddef;
        hreg : tregister;
        oldregisterdef : boolean;
      begin
        SecondPass(dest);
        if Codegenerror then
         exit;
        Case dest^.resulttype^.deftype of
          floatdef:
            floatstore(PFloatDef(dest^.resulttype)^.typ,dest^.location.reference);
          orddef:
            begin
              Case dest^.resulttype^.size of
                1 : hreg:=regtoreg8(accumulator);
                2 : hreg:=regtoreg16(accumulator);
                4 : hreg:=accumulator;
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
                        hreg:=accumulator;
                      end;
                    s8bit,s16bit,s32bit:
                      begin
                        new(hdef,init(s32bit,$80000000,$7fffffff));
                        hreg:=accumulator;
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
            End;
          else
            internalerror(66766766);
        end;
      end;


    procedure secondinline(var p : ptree);
       const
         { tfloattype = (f32bit,s32real,s64real,s80real,s64bit); }
         float_name: array[tfloattype] of string[8]=
           ('FIXED','SINGLE','REAL','EXTENDED','COMP','FIXED16');
         incdecop:array[in_inc_x..in_dec_x] of tasmop=(A_INC,A_DEC);
         addsubop:array[in_inc_x..in_dec_x] of tasmop=(A_ADD,A_SUB);
       var
         aktfile : treference;
         ft : tfiletype;
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
            r^.symbol:=newasmsymbol('U_'+upper(target_info.system_unit)+io[doread]);
            concat_external(r^.symbol^.name,EXT_NEAR);
            exprasmlist^.concat(new(pai386,op_ref_reg(A_LEA,S_L,r,R_EDI)))
          end;

        const
           rdwrprefix:array[boolean] of string[15]=('FPC_WRITE_TEXT_','FPC_READ_TEXT_');
        var
           destpara,
           node,hp    : ptree;
           typedtyp,
           pararesult : pdef;
           has_length : boolean;
           dummycoll  : tdefcoll;
           iolabel    : plabel;
           npara      : longint;
        begin
           { I/O check }
           if (cs_check_io in aktlocalswitches) and
              ((aktprocsym^.definition^.options and poiocheck)=0) then
             begin
                getlabel(iolabel);
                emitlab(iolabel);
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
                exprasmlist^.concat(new(pai386,op_reg_ref(A_MOV,S_L,R_EDI,newreference(aktfile))));
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

                     exprasmlist^.concat(new(pai386,op_ref_reg(A_LEA,S_L,newreference(node^.left^.location.reference),R_EDI)));

                     { skip to the next parameter }
                     node:=node^.right;
                  end
                else
                  begin
                  { load stdin/stdout stream }
                     loadstream;
                  end;

                { save @aktfile in temporary variable }
                exprasmlist^.concat(new(pai386,op_reg_ref(A_MOV,S_L,R_EDI,newreference(aktfile))));
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
                     { when read ord,floats are functions, so they need this
                       parameter as their destination instead of being pushed }
                     if doread and
                        (ft<>ft_typed) and
                        (hp^.resulttype^.deftype in [orddef,floatdef]) then
                      destpara:=hp^.left
                     else
                      begin
                        if ft=ft_typed then
                          never_copy_const_param:=true;
                        { reset data type }
                        dummycoll.data:=nil;
                        { support openstring calling for readln(shortstring) }
                        if doread and (is_shortstring(hp^.resulttype)) then
                          dummycoll.data:=openshortstringdef;
                        secondcallparan(hp,@dummycoll,false,false,0);
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
                                   secondcallparan(hp,@dummycoll,false,false,0);
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
                                   secondcallparan(hp,@dummycoll,false,false,0);
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
                            stringdef :
                              begin
{$ifndef OLDREAD}
                                emitcall(rdwrprefix[doread]+pstringdef(pararesult)^.stringtypname,true);
{$else}
                                     if doread then
                                       begin
                                         { push maximum string length }
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
{$endif}
                              end;
                            pointerdef :
                              begin
                                if is_pchar(pararesult) then
                                  emitcall(rdwrprefix[doread]+'PCHAR_AS_POINTER',true)
                              end;
                            arraydef :
                              begin
                                if is_chararray(pararesult) then
                                  emitcall(rdwrprefix[doread]+'PCHAR_AS_ARRAY',true)
                              end;
                            floatdef :
                              begin
{$ifndef OLDREAD}
                                if doread then
                                  begin
                                    emitcall(rdwrprefix[doread]+'FLOAT',true);
                                    StoreDirectFuncResult(destpara);
                                  end
                                else
{$endif}
                                  emitcall(rdwrprefix[doread]+float_name[pfloatdef(pararesult)^.typ],true)
                              end;
                            orddef :
                              begin
                                case porddef(pararesult)^.typ of
{$ifndef OLDREAD}
                                  s8bit,s16bit,s32bit :
                                    emitcall(rdwrprefix[doread]+'SINT',true);
                                  u8bit,u16bit,u32bit :
                                    emitcall(rdwrprefix[doread]+'UINT',true);
{$else}
                                  u8bit :
                                    if doread then
                                      emitcall('FPC_READ_TEXT_BYTE',true);
                                  s8bit :
                                    if doread then
                                      emitcall('FPC_READ_TEXT_SHORTINT',true);
                                  u16bit :
                                    if doread then
                                      emitcall('FPC_READ_TEXT_WORD',true);
                                  s16bit :
                                    if doread then
                                      emitcall('FPC_READ_TEXT_INTEGER',true);
                                  s32bit :
                                    if doread then
                                      emitcall('FPC_READ_TEXT_LONGINT',true)
                                    else
                                      emitcall('FPC_WRITE_TEXT_LONGINT',true);
                                  u32bit :
                                    if doread then
                                      emitcall('FPC_READ_TEXT_CARDINAL',true)
                                    else
                                      emitcall('FPC_WRITE_TEXT_CARDINAL',true);
{$endif}
                                  uchar :
                                    emitcall(rdwrprefix[doread]+'CHAR',true);
                                  s64bitint:
                                    emitcall(rdwrprefix[doread]+'INT64',true);
                                  u64bit :
                                    emitcall(rdwrprefix[doread]+'QWORD',true);
                                  bool8bit,
                                  bool16bit,
                                  bool32bit :
                                    emitcall(rdwrprefix[doread]+'BOOLEAN',true);
                                end;
{$ifndef OLDREAD}
                                if doread then
                                 StoreDirectFuncResult(destpara);
{$endif}
                              end;
                          end;
                       end;
                   { load ESI in methods again }
                     popusedregisters(pushed);
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
               maybe_loadesi;
             end;
         { Insert IOCheck if set }
           if assigned(iolabel) then
             begin
                { registers are saved in the procedure }
                exprasmlist^.concat(new(pai386,op_sym(A_PUSH,S_L,newasmsymbol(lab2str(iolabel)))));
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
           procedureprefix : string;

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

           { string arg }
           hp:=node;
           node:=node^.right;
           hp^.right:=nil;
           dummycoll.paratyp:=vs_var;
           if is_shortstring(hp^.resulttype) then
             dummycoll.data:=openshortstringdef
           else
             dummycoll.data:=hp^.resulttype;
           procedureprefix:='FPC_'+pstringdef(hp^.resulttype)^.stringtypname+'_';
           secondcallparan(hp,@dummycoll,false,false,0);
           if codegenerror then
             exit;

           dummycoll.paratyp:=vs_const;
           disposetree(p^.left);
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
                secondcallparan(hp,@dummycoll,false
                  ,false,0
                  );
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
                secondcallparan(hp,@dummycoll,false
                  ,false,0
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

           { last arg longint or real }
           secondcallparan(hp,@dummycoll,false
             ,false,0
             );
           disposetree(hp);

           if codegenerror then
             exit;

           if is_real then
             emitcall(procedureprefix+float_name[pfloatdef(hp^.resulttype)^.typ],true)
           else
             case porddef(hp^.resulttype)^.typ of
                u32bit:
                  emitcall(procedureprefix+'CARDINAL',true);

                u64bit:
                  emitcall(procedureprefix+'QWORD',true);

                s64bitint:
                  emitcall(procedureprefix+'INT64',true);

                else
                  emitcall(procedureprefix+'LONGINT',true);
             end;
           popusedregisters(pushed);
        end;

{$IfnDef OLDVAL}

        Procedure Handle_Val;

        var
           hp,node, code_para, dest_para : ptree;
           hreg: TRegister;
           hdef: POrdDef;
           procedureprefix : string;
           hr: TReference;
           dummycoll : tdefcoll;
           has_code, has_32bit_code, oldregisterdef: boolean;
           pushed2: TPushed;
           unusedregs: TRegisterSet;

          begin
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

          {the function result will be in EAX, so we need to reserve it so
           that secondpass(dest_para^.left) won't use it}
           hreg := getexplicitregister32(R_EAX);
          {if EAX is already in use, it's a register variable. Since we don't
           need another register besides EAX, release the one we got}
           If hreg <> R_EAX Then ungetregister32(hreg);

          {load the address of the destination}
           secondpass(dest_para^.left);

          {unget EAX (if we got it before), since otherwise pushusedregisters
           will push it on the stack.}
           If (hreg = R_EAX) then Ungetregister32(hreg);

          {save which registers are (not) used now, we'll need it after the
           function call}
           UnusedRegs := Unused;

          {(if necessary) save the address loading of dest_para and possibly
           register variables}
           pushusedregisters(pushed,$ff);

          {only now load the address of the code parameter, since we want
           to handle it before the destination after the function call}

           If has_code and (not has_32bit_code) Then
             Begin
              {make sure this secondpass doesn't use EAX either}
               hreg := getexplicitregister32(R_EAX);
               If hreg <> R_EAX Then ungetregister32(hreg);
               secondpass(code_para^.left);
               If hreg = R_EAX Then ungetregister32(hreg);
              {maybe secondpass(code_para^.left) required more registers than
               secondpass(dest_para^.left). The registers where we can store
               the result afterwards have to be unused in both cases}
               UnusedRegs := UnusedRegs * Unused;
               pushusedregisters(pushed2, $ff)
             End;

          {now that we've already pushed the results from
           secondpass(code_para^.left) and secondpass(dest_para^.left) on the
           stack, we can put the real parameters on the stack}

           If has_32bit_code Then
             Begin
               dummycoll.paratyp:=vs_var;
               dummycoll.data:=code_para^.resulttype;
               secondcallparan(code_para,@dummycoll,false,false,0);
               if codegenerror then
                 exit;
               Disposetree(code_para);
             End
           Else
             Begin
           {only 32bit code parameter is supported, so fake one}
               GetTempOfSizeReference(4,hr);
               emitpushreferenceaddr(exprasmlist,hr);
             End;

          {node = first parameter = string}
           dummycoll.paratyp:=vs_const;
           dummycoll.data:=node^.resulttype;
           secondcallparan(node,@dummycoll,false,false,0);
           if codegenerror then
             exit;

           Case dest_para^.resulttype^.deftype of
             floatdef:
               procedureprefix := 'FPC_VAL_REAL_';
             orddef:
               if is_signed(dest_para^.resulttype) then
                 begin
                   {if we are converting to a signed number, we have to include the
                    size of the destination, so the Val function can extend the sign
                    of the result to allow proper range checking}
                   exprasmlist^.concat(new(pai386,op_const(A_PUSH,S_L,dest_para^.resulttype^.size)));
                   procedureprefix := 'FPC_VAL_SINT_'
                 end
               else
                 procedureprefix := 'FPC_VAL_UINT_';
           End;
           emitcall(procedureprefix+pstringdef(node^.resulttype)^.stringtypname,true);
           { before disposing node we need to ungettemp !! PM }
           if node^.left^.location.loc in [LOC_REFERENCE,LOC_MEM] then
             ungetiftemp(node^.left^.location.reference);
           disposetree(node);
           p^.left := nil;

          {reload esi in case the dest_para/code_para is a class variable or so}
           maybe_loadesi;

           If (dest_para^.resulttype^.deftype = orddef) Then
             Begin
              {restore which registers are used by register variables and/or
               the address loading of the dest/code_para, so we can store the
               result in a safe place}
               unused := UnusedRegs;
              {as of now, hreg now holds the location of the result, if it was
               integer}
               hreg := getexplicitregister32(R_EAX);
               emit_reg_reg(A_MOV,S_L,R_EAX,hreg);
             End;

           If has_code and Not(has_32bit_code) Then
             {only 16bit code is possible}
             Begin
              {restore the address loaded by secondpass(code_para)}
               popusedregisters(pushed2);
              {move the code to its destination}
               exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_L,NewReference(hr),R_EDI)));
               emit_mov_reg_loc(R_DI,code_para^.left^.location);
               Disposetree(code_para);
             End;

          {restore the addresses loaded by secondpass(dest_para)}
           popusedregisters(pushed);
          {save the function result in the destination variable}
           Case dest_para^.left^.resulttype^.deftype of
             floatdef:
               floatstore(PFloatDef(dest_para^.left^.resulttype)^.typ,dest_para^.left^.location.reference);
             orddef:
               Case PordDef(dest_para^.left^.resulttype)^.typ of
                 u8bit,s8bit:
                   emit_mov_reg_loc(RegToReg8(hreg),dest_para^.left^.location);
                 u16bit,s16bit:
                   emit_mov_reg_loc(RegToReg16(hreg),dest_para^.left^.location);
                 u32bit,s32bit:
                   emit_mov_reg_loc(hreg,dest_para^.left^.location);
                 {u64bit,s64bitint: ???}
               End;
           End;
           If (cs_check_range in aktlocalswitches) and
              (dest_para^.left^.resulttype^.deftype = orddef) and
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
{$EndIf OLDVAL}

      var
         r : preference;
         hp : ptree;
         l : longint;
         ispushed : boolean;
         hregister : tregister;
         otlabel,oflabel   : plabel;
         oldpushedparasize : longint;

      begin
      { save & reset pushedparasize }
         oldpushedparasize:=pushedparasize;
         pushedparasize:=0;
         case p^.inlinenumber of
            in_assert_x_y:
              begin
                 otlabel:=truelabel;
                 oflabel:=falselabel;
                 getlabel(truelabel);
                 getlabel(falselabel);
                 secondpass(p^.left^.left);
                 if cs_do_assertion in aktlocalswitches then
                   begin
                      maketojumpbool(p^.left^.left);
                      emitlab(falselabel);
                      { erroraddr }
                      exprasmlist^.concat(new(pai386,op_reg(A_PUSH,S_L,R_EBP)));
                      { lineno }
                      exprasmlist^.concat(new(pai386,op_const(A_PUSH,S_L,aktfilepos.line)));
                      { filename string }
                      hp:=genstringconstnode(current_module^.sourcefiles^.get_file_name(aktfilepos.fileindex));
                      secondpass(hp);
                      if codegenerror then
                       exit;
                      emitpushreferenceaddr(exprasmlist,hp^.location.reference);
                      disposetree(hp);
                      { push msg }
                      secondpass(p^.left^.right^.left);
                      emitpushreferenceaddr(exprasmlist,p^.left^.right^.left^.location.reference);
                      { call }
                      emitcall('FPC_ASSERT',true);
                      emitlab(truelabel);
                   end;
                 freelabel(truelabel);
                 freelabel(falselabel);
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
                          exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_W,newreference(p^.left^.location.reference),
                            p^.location.register)));
                       end;
                   end
                 else p^.location.register:=p^.left^.location.register;
                 if p^.inlinenumber=in_hi_word then
                   exprasmlist^.concat(new(pai386,op_const_reg(A_SHR,S_W,8,p^.location.register)));
                 p^.location.register:=reg16toreg8(p^.location.register);
              end;
            in_sizeof_x,
            in_typeof_x :
              begin
                 { for both cases load vmt }
                 if p^.left^.treetype=typen then
                   begin
                      p^.location.register:=getregister32;
                      exprasmlist^.concat(new(pai386,op_sym_ofs_reg(A_MOV,
                        S_L,newasmsymbol(pobjectdef(p^.left^.resulttype)^.vmt_mangledname),0,
                        p^.location.register)));
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
                      exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_L,
                      newreference(p^.left^.location.reference),
                        p^.location.register)));
                   end;
                 { in sizeof load size }
                 if p^.inlinenumber=in_sizeof_x then
                   begin
                      new(r);
                      reset_reference(r^);
                      r^.base:=p^.location.register;
                      exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_L,r,
                        p^.location.register)));
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
                           exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_L,newreference(p^.left^.location.reference),
                             p^.location.register)));
                        end;
                   end
                 else p^.location.register:=p^.left^.location.register;
                 if p^.inlinenumber=in_hi_long then
                   exprasmlist^.concat(new(pai386,op_const_reg(A_SHR,S_L,16,p^.location.register)));
                 p^.location.register:=reg32toreg16(p^.location.register);
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
                        begin
                           del_reference(p^.left^.location.reference);
                           exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,opsize,newreference(p^.left^.location.reference),
                             p^.location.register)));
                        end;
                   end
                 else p^.location.register:=p^.left^.location.register;

                 if not (cs_check_overflow in aktlocalswitches) then
                   exprasmlist^.concat(new(pai386,op_reg(asmop,opsize,
                     p^.location.register)))
                 else
                   exprasmlist^.concat(new(pai386,op_const_reg(asmop,opsize,1,
                     p^.location.register)));
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
                             end;
                           end;
              pointerdef : begin
                             opsize:=S_L;
                             if porddef(ppointerdef(p^.left^.left^.resulttype)^.definition)=voiddef then
                              addvalue:=1
                             else
                              addvalue:=ppointerdef(p^.left^.left^.resulttype)^.definition^.savesize;
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
                                    del_reference(p^.left^.right^.left^.location.reference);
                                    hregister:=getregister32;
                                    exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_L,
                                      newreference(p^.left^.right^.left^.location.reference),hregister)));
                                  end;
                       else
                        internalerror(10082);
                       end;
                    { insert multiply with addvalue if its >1 }
                      if addvalue>1 then
                       exprasmlist^.concat(new(pai386,op_const_reg(A_IMUL,opsize,
                         addvalue,hregister)));
                      addconstant:=false;
                    end;
                 end;
              { write the add instruction }
                if addconstant then
                 begin
                   if (addvalue=1) and not(cs_check_overflow in aktlocalswitches) then
                     begin
                        if p^.left^.left^.location.loc=LOC_CREGISTER then
                          exprasmlist^.concat(new(pai386,op_reg(incdecop[p^.inlinenumber],opsize,
                            p^.left^.left^.location.register)))
                        else
                          exprasmlist^.concat(new(pai386,op_ref(incdecop[p^.inlinenumber],opsize,
                            newreference(p^.left^.left^.location.reference))))
                     end
                   else
                     begin
                        if p^.left^.left^.location.loc=LOC_CREGISTER then
                          exprasmlist^.concat(new(pai386,op_const_reg(addsubop[p^.inlinenumber],opsize,
                            addvalue,p^.left^.left^.location.register)))
                        else
                          exprasmlist^.concat(new(pai386,op_const_ref(addsubop[p^.inlinenumber],opsize,
                            addvalue,newreference(p^.left^.left^.location.reference))));
                     end
                 end
                else
                 begin
                    { BUG HERE : detected with nasm :
                      hregister is allways 32 bit
                      it should be converted to 16 or 8 bit depending on op_size  PM }
                    { still not perfect :
                      if hregister is already a 16 bit reg ?? PM }
                    case opsize of
                      S_B : hregister:=reg32toreg8(hregister);
                      S_W : hregister:=reg32toreg16(hregister);
                    end;
                    if p^.left^.left^.location.loc=LOC_CREGISTER then
                      exprasmlist^.concat(new(pai386,op_reg_reg(addsubop[p^.inlinenumber],opsize,
                        hregister,p^.left^.left^.location.register)))
                    else
                      exprasmlist^.concat(new(pai386,op_reg_ref(addsubop[p^.inlinenumber],opsize,
                        hregister,newreference(p^.left^.left^.location.reference))));
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
                      exprasmlist^.concat(new(pai386,op_reg_reg(A_OR,S_L,
                        p^.left^.left^.location.register,
                        p^.left^.left^.location.register)));
                      ungetregister32(p^.left^.left^.location.register);
                   end
                 else
                   begin
                      exprasmlist^.concat(new(pai386,op_const_ref(A_CMP,S_L,0,
                        newreference(p^.left^.left^.location.reference))));
                      del_reference(p^.left^.left^.location.reference);
                   end;
                 p^.location.resflags:=F_NE;
              end;
             in_reset_typedfile,in_rewrite_typedfile :
               begin
                  pushusedregisters(pushed,$ff);
                  exprasmlist^.concat(new(pai386,op_const(A_PUSH,S_L,pfiledef(p^.left^.resulttype)^.typed_as^.size)));
                  secondpass(p^.left);
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
                 maybe_loadesi;
              end;
{$IfnDef OLDVAL}
            in_val_x :
              Begin
                handle_val;
              End;
{$EndIf OLDVAL}
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
                           exprasmlist^.concat(new(pai386,op_const_ref(asmop,S_L,
                             l,newreference(p^.left^.left^.location.reference))));
                           del_reference(p^.left^.left^.location.reference);
                        end
                      else
                        { LOC_CREGISTER }
                        exprasmlist^.concat(new(pai386,op_const_reg(asmop,S_L,
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
                                opsize:=def2def_opsize(p^.left^.right^.left^.resulttype,u32bitdef);
                                if opsize in [S_B,S_W,S_L] then
                                 op:=A_MOV
                                else
                                 op:=A_MOVZX;
                                exprasmlist^.concat(new(pai386,op_ref_reg(op,opsize,
                                  newreference(p^.left^.right^.left^.location.reference),R_EDI)));
                             end;
                          if (p^.left^.left^.location.loc=LOC_REFERENCE) then
                            exprasmlist^.concat(new(pai386,op_reg_ref(asmop,S_L,hregister,
                              newreference(p^.left^.right^.left^.location.reference))))
                          else
                            exprasmlist^.concat(new(pai386,op_reg_reg(asmop,S_L,hregister,
                              p^.left^.right^.left^.location.register)));
                        end
                      else
                        begin
                           pushsetelement(p^.left^.right^.left);
                           { normset is allways a ref }
                           emitpushreferenceaddr(exprasmlist,
                             p^.left^.left^.location.reference);
                           if p^.inlinenumber=in_include_x_y then
                             emitcall('FPC_SET_SET_BYTE',true)
                           else
                             emitcall('FPC_SET_UNSET_BYTE',true);
                           {CGMessage(cg_e_include_not_implemented);}
                        end;
                   end;
              end;
            else internalerror(9);
         end;
         { remove temp. objects, we don't generate them here }
         removetemps(exprasmlist,temptoremove);
         temptoremove^.clear;
         { reset pushedparasize }
         pushedparasize:=oldpushedparasize;
      end;

end.
{
  $Log$
  Revision 1.42  1999-04-14 09:11:59  peter
    * fixed include

  Revision 1.41  1999/04/08 23:59:49  pierre
   * temp string for val code freed

  Revision 1.40  1999/04/08 15:57:46  peter
    + subrange checking for readln()

  Revision 1.39  1999/04/07 15:31:16  pierre
    * all formaldefs are now a sinlge definition
      cformaldef (this was necessary for double_checksum)
    + small part of double_checksum code

  Revision 1.38  1999/04/05 11:07:26  jonas
    * fixed some typos in the constants of the range checking for Val

  Revision 1.37  1999/04/01 22:07:51  peter
    * universal string names (ansistr instead of stransi) for val/str

  Revision 1.36  1999/04/01 06:21:04  jonas
    * added initialization for has_32bit_code (caused problems with Val statement
      without code parameter)

  Revision 1.35  1999/03/31 20:30:49  michael
  * fixed typo: odlval to oldval

  Revision 1.34  1999/03/31 17:13:09  jonas
    * bugfix for -Ox with internal val code
    * internal val code now requires less free registers
    * internal val code no longer needs a temp var for range checking

  Revision 1.33  1999/03/26 00:24:15  peter
    * last para changed to long for easier pushing with 4 byte aligns

  Revision 1.32  1999/03/26 00:05:26  peter
    * released valintern
    + deffile is now removed when compiling is finished
    * ^( compiles now correct
    + static directive
    * shrd fixed

  Revision 1.31  1999/03/24 23:16:49  peter
    * fixed bugs 212,222,225,227,229,231,233

  Revision 1.30  1999/03/16 17:52:56  jonas
    * changes for internal Val code (do a "make cycle OPT=-dvalintern" to test)
    * in cgi386inl: also range checking for subrange types (compile with "-dreadrangecheck")
    * in cgai386: also small fixes to emitrangecheck

  Revision 1.29  1999/02/25 21:02:27  peter
    * ag386bin updates
    + coff writer

  Revision 1.28  1999/02/22 02:15:11  peter
    * updates for ag386bin

  Revision 1.27  1999/02/17 14:21:40  pierre
   * unused local removed

  Revision 1.26  1999/02/15 11:40:21  pierre
   * pred/succ with overflow check must use ADD DEC !!

  Revision 1.25  1999/02/05 10:56:19  florian
    * in some cases a writeln of temp. ansistrings cause a memory leak, fixed

  Revision 1.24  1999/01/21 22:10:39  peter
    * fixed array of const
    * generic platform independent high() support

  Revision 1.23  1999/01/06 12:23:29  florian
    * str(...) for ansi/long and widestrings fixed

  Revision 1.22  1998/12/11 23:36:07  florian
    + again more stuff for int64/qword:
         - comparision operators
         - code generation for: str, read(ln), write(ln)

  Revision 1.21  1998/12/11 00:02:50  peter
    + globtype,tokens,version unit splitted from globals

  Revision 1.20  1998/11/27 14:50:32  peter
    + open strings, $P switch support

  Revision 1.19  1998/11/26 13:10:40  peter
    * new int - int conversion -dNEWCNV
    * some function renamings

  Revision 1.18  1998/11/24 17:04:27  peter
    * fixed length(char) when char is a variable

  Revision 1.17  1998/11/05 12:02:33  peter
    * released useansistring
    * removed -Sv, its now available in fpc modes

  Revision 1.16  1998/10/22 17:11:13  pierre
    + terminated the include exclude implementation for i386
    * enums inside records fixed

  Revision 1.15  1998/10/21 15:12:50  pierre
    * bug fix for IOCHECK inside a procedure with iocheck modifier
    * removed the GPF for unexistant overloading
      (firstcall was called with procedinition=nil !)
    * changed typen to what Florian proposed
      gentypenode(p : pdef) sets the typenodetype field
      and resulttype is only set if inside bt_type block !

  Revision 1.14  1998/10/20 08:06:40  pierre
    * several memory corruptions due to double freemem solved
      => never use p^.loc.location:=p^.left^.loc.location;
    + finally I added now by default
      that ra386dir translates global and unit symbols
    + added a first field in tsymtable and
      a nextsym field in tsym
      (this allows to obtain ordered type info for
      records and objects in gdb !)

  Revision 1.13  1998/10/13 16:50:02  pierre
    * undid some changes of Peter that made the compiler wrong
      for m68k (I had to reinsert some ifdefs)
    * removed several memory leaks under m68k
    * removed the meory leaks for assembler readers
    * cross compiling shoud work again better
      ( crosscompiling sysamiga works
       but as68k still complain about some code !)

  Revision 1.12  1998/10/08 17:17:12  pierre
    * current_module old scanner tagged as invalid if unit is recompiled
    + added ppheap for better info on tracegetmem of heaptrc
      (adds line column and file index)
    * several memory leaks removed ith help of heaptrc !!

  Revision 1.11  1998/10/05 21:33:15  peter
    * fixed 161,165,166,167,168

  Revision 1.10  1998/10/05 12:32:44  peter
    + assert() support

  Revision 1.8  1998/10/02 10:35:09  peter
    * support for inc(pointer,value) which now increases with value instead
      of 0*value :)

  Revision 1.7  1998/09/21 08:45:07  pierre
    + added vmt_offset in tobjectdef.write for fututre use
      (first steps to have objects without vmt if no virtual !!)
    + added fpu_used field for tabstractprocdef  :
      sets this level to 2 if the functions return with value in FPU
      (is then set to correct value at parsing of implementation)
      THIS MIGHT refuse some code with FPU expression too complex
      that were accepted before and even in some cases
      that don't overflow in fact
      ( like if f : float; is a forward that finally in implementation
       only uses one fpu register !!)
      Nevertheless I think that it will improve security on
      FPU operations !!
    * most other changes only for UseBrowser code
      (added symtable references for record and objects)
      local switch for refs to args and local of each function
      (static symtable still missing)
      UseBrowser still not stable and probably broken by
      the definition hash array !!

  Revision 1.6  1998/09/20 12:26:37  peter
    * merged fixes

  Revision 1.5  1998/09/17 09:42:15  peter
    + pass_2 for cg386
    * Message() -> CGMessage() for pass_1/pass_2

  Revision 1.4  1998/09/14 10:43:49  peter
    * all internal RTL functions start with FPC_

  Revision 1.3.2.1  1998/09/20 12:20:07  peter
    * Fixed stack not on 4 byte boundary when doing a call

  Revision 1.3  1998/09/05 23:03:57  florian
    * some fixes to get -Or work:
      - inc/dec didn't take care of CREGISTER
      - register calculcation of inc/dec was wrong
      - var/const parameters get now assigned 32 bit register, but
        const parameters only if they are passed by reference !

  Revision 1.2  1998/09/04 08:41:40  peter
    * updated some error CGMessages

  Revision 1.1  1998/08/31 12:22:14  peter
    * secondinline moved to cg386inl

  Revision 1.19  1998/08/31 08:52:03  peter
    * fixed error 10 with succ() and pref()

  Revision 1.18  1998/08/20 21:36:38  peter
    * fixed 'with object do' bug

  Revision 1.17  1998/08/19 16:07:36  jonas
    * changed optimizer switches + cleanup of DestroyRefs in daopt386.pas

  Revision 1.16  1998/08/18 09:24:36  pierre
    * small warning position bug fixed
    * support_mmx switches splitting was missing
    * rhide error and warning output corrected

  Revision 1.15  1998/08/13 11:00:09  peter
    * fixed procedure<>procedure construct

  Revision 1.14  1998/08/11 14:05:33  peter
    * fixed sizeof(array of char)

  Revision 1.13  1998/08/10 14:49:45  peter
    + localswitches, moduleswitches, globalswitches splitting

  Revision 1.12  1998/07/30 13:30:31  florian
    * final implemenation of exception support, maybe it needs
      some fixes :)

  Revision 1.11  1998/07/24 22:16:52  florian
    * internal error 10 together with array access fixed. I hope
      that's the final fix.

  Revision 1.10  1998/07/18 22:54:23  florian
    * some ansi/wide/longstring support fixed:
       o parameter passing
       o returning as result from functions

  Revision 1.9  1998/07/07 17:40:37  peter
    * packrecords 4 works
    * word aligning of parameters

  Revision 1.8  1998/07/06 15:51:15  michael
  Added length checking for string reading

  Revision 1.7  1998/07/06 14:19:51  michael
  + Added calls for reading/writing ansistrings

  Revision 1.6  1998/07/01 15:28:48  peter
    + better writeln/readln handling, now 100% like tp7

  Revision 1.5  1998/06/25 14:04:17  peter
    + internal inc/dec

  Revision 1.4  1998/06/25 08:48:06  florian
    * first version of rtti support

  Revision 1.3  1998/06/09 16:01:33  pierre
    + added procedure directive parsing for procvars
      (accepted are popstack cdecl and pascal)
    + added C vars with the following syntax
      var C calias 'true_c_name';(can be followed by external)
      reason is that you must add the Cprefix

      which is target dependent

  Revision 1.2  1998/06/08 13:13:29  pierre
    + temporary variables now in temp_gen.pas unit
      because it is processor independent
    * mppc68k.bat modified to undefine i386 and support_mmx
      (which are defaults for i386)

  Revision 1.1  1998/06/05 17:44:10  peter
    * splitted cgi386

}

