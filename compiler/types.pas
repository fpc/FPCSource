{
    $Id$
    Copyright (C) 1993-98 by Florian Klaempfl

    This unit provides some help routines for type handling

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
unit types;

  interface

    uses
       objects,cobjects,globals,symtable,tree,aasm;

    type
       tmmxtype = (mmxno,mmxu8bit,mmxs8bit,mmxu16bit,mmxs16bit,
                   mmxu32bit,mmxs32bit,mmxfixed16,mmxsingle);

    { returns true, if def defines an ordinal type }
    function is_ordinal(def : pdef) : boolean;

    { true if p points to an open array def }
    function is_open_array(p : pdef) : boolean;

    { returns true, if def defines a signed data type (only for ordinal types) }
    function is_signed(def : pdef) : boolean;

    { returns true, if def uses FPU }
    function is_fpu(def : pdef) : boolean;

    { true if the return value is in EAX }
    function ret_in_acc(def : pdef) : boolean;

    { true if uses a parameter as return value }
    function ret_in_param(def : pdef) : boolean;

    { true if a const parameter is too large to copy }
    function dont_copy_const_param(def : pdef) : boolean;
    { true if we must never copy this parameter }
    const
       never_copy_const_param : boolean = false;

    { true, if def1 and def2 are semantical the same }
    function is_equal(def1,def2 : pdef) : boolean;

    { checks for type compatibility (subgroups of type)  }
    { used for case statements... probably missing stuff }
    { to use on other types                              }
    function is_subequal(def1, def2: pdef): boolean;

    { true, if two parameter lists are equal }
    function equal_paras(def1,def2 : pdefcoll) : boolean;

    { gibt den ordinalen Werten der Node zurueck oder falls sie }
    { keinen ordinalen Wert hat, wird ein Fehler erzeugt        }
    function get_ordinal_value(p : ptree) : longint;

    { if l isn't in the range of def a range check error is generated }
    procedure testrange(def : pdef;l : longint);

    { returns the range of def }
    procedure getrange(def : pdef;var l : longint;var h : longint);

    { generates a VMT for _class }
    procedure genvmt(_class : pobjectdef);

    { true, if p is a pointer to a const int value }
    function is_constintnode(p : ptree) : boolean;

    { like is_constintnode }
    function is_constboolnode(p : ptree) : boolean;
    function is_constrealnode(p : ptree) : boolean;
    function is_constcharnode(p : ptree) : boolean;

    { some type helper routines for MMX support }
    function is_mmx_able_array(p : pdef) : boolean;

    { returns the mmx type }
    function mmx_type(p : pdef) : tmmxtype;

  implementation

    uses verbose;

    function is_constintnode(p : ptree) : boolean;

      begin
         {DM: According to me, an orddef with anysize, is
          a correct constintnode. Anyway I commented changed s32bit check,
          because it caused problems with statements like a:=high(word).}
         is_constintnode:=((p^.treetype=ordconstn) and
           (p^.resulttype^.deftype=orddef) and
           (porddef(p^.resulttype)^.typ in [u8bit,s8bit,u16bit,s16bit,
            u32bit,s32bit,uauto]));
      end;

    function is_constcharnode(p : ptree) : boolean;

      begin
         is_constcharnode:=((p^.treetype=ordconstn) and
           (p^.resulttype^.deftype=orddef) and
           (porddef(p^.resulttype)^.typ=uchar));
      end;

    function is_constrealnode(p : ptree) : boolean;

      begin
         is_constrealnode:=(p^.treetype=realconstn);
      end;

    function is_constboolnode(p : ptree) : boolean;

      begin
         is_constboolnode:=((p^.treetype=ordconstn) and
           (p^.resulttype^.deftype=orddef) and
           (porddef(p^.resulttype)^.typ=bool8bit));
      end;

    function equal_paras(def1,def2 : pdefcoll) : boolean;

      begin
         while (assigned(def1)) and (assigned(def2)) do
           begin
              if not(is_equal(def1^.data,def2^.data)) or
                 (def1^.paratyp<>def2^.paratyp) then
                begin
                   equal_paras:=false;
                   exit;
                end;
              def1:=def1^.next;
              def2:=def2^.next;
           end;
         if (def1=nil) and (def2=nil) then
           equal_paras:=true
         else
           equal_paras:=false;
      end;

    { returns true, if def uses FPU }
    function is_fpu(def : pdef) : boolean;
      begin
         is_fpu:=(def^.deftype=floatdef) and (pfloatdef(def)^.typ<>f32bit);
      end;
    function is_ordinal(def : pdef) : boolean;

      var
         dt : tbasetype;

      begin
         case def^.deftype of
            orddef : begin
                          dt:=porddef(def)^.typ;
                          is_ordinal:=(dt=s32bit) or (dt=u32bit) or (dt=uchar) or (dt=u8bit) or
                            (dt=s8bit) or (dt=s16bit) or (dt=bool8bit) or (dt=u16bit);
                       end;
            enumdef : is_ordinal:=true;
            else is_ordinal:=false;
         end;
      end;

    function is_signed(def : pdef) : boolean;

      var
         dt : tbasetype;

      begin
         case def^.deftype of
            orddef : begin
                          dt:=porddef(def)^.typ;
                          is_signed:=(dt=s32bit) or (dt=s8bit) or (dt=s16bit);
                       end;
            enumdef : is_signed:=false;
            else internalerror(1001);
         end;
      end;

    { true, if p points to an open array def }
    function is_open_array(p : pdef) : boolean;

      begin
         is_open_array:=(p^.deftype=arraydef) and
                 (parraydef(p)^.lowrange=0) and
                 (parraydef(p)^.highrange=-1);
      end;

    { true if the return value is in accumulator (EAX for i386), D0 for 68k }
    function ret_in_acc(def : pdef) : boolean;

      begin
         ret_in_acc:=(def^.deftype=orddef) or
                     (def^.deftype=pointerdef) or
                     (def^.deftype=enumdef) or
                     (def^.deftype=procvardef) or
                     (def^.deftype=classrefdef) or
                     ((def^.deftype=objectdef) and
                      ((pobjectdef(def)^.options and oois_class)<>0)
                     ) or
                     ((def^.deftype=setdef) and
                      (psetdef(def)^.settype=smallset)) or
                     ((def^.deftype=floatdef) and
                      (pfloatdef(def)^.typ=f32bit));
      end;

    { true if uses a parameter as return value }
    function ret_in_param(def : pdef) : boolean;

      begin
         ret_in_param:=(def^.deftype=arraydef) or
                       (def^.deftype=stringdef) or
                       ((def^.deftype=objectdef) and
                        ((pobjectdef(def)^.options and oois_class)=0)
                       ) or
                       (def^.deftype=recorddef) or
                       ((def^.deftype=setdef) and
                        (psetdef(def)^.settype<>smallset));
      end;

    { true if a const parameter is too large to copy }
    function dont_copy_const_param(def : pdef) : boolean;

      begin
         dont_copy_const_param:=(def^.deftype=arraydef) or
                       (def^.deftype=stringdef) or
                       (def^.deftype=objectdef) or
                       (def^.deftype=formaldef) or
                       (def^.deftype=recorddef) or
                       (def^.deftype=formaldef) or
                       ((def^.deftype=setdef) and
                        (psetdef(def)^.settype<>smallset));
      end;

    procedure testrange(def : pdef;l : longint);

      var
         lv,hv: longint;

      begin
         getrange(def,lv,hv);
         if (def^.deftype=orddef) and
            (porddef(def)^.typ=u32bit) then
           begin
              if lv<=hv then
                begin
                   if (l<lv) or (l>hv) then
                    Message(parser_e_range_check_error);
                end
              else
                { this happens with the wrap around problem  }
                { if lv is positive and hv is over $7ffffff  }
                { so it seems negative                       }
                begin
                   if ((l>=0) and (l<lv)) or
                      ((l<0) and (l>hv)) then
                    Message(parser_e_range_check_error);
                end;
           end
         else if (l<lv) or (l>hv) then
           Message(parser_e_range_check_error);
      end;

    procedure getrange(def : pdef;var l : longint;var h : longint);

      begin
         if def^.deftype=orddef then
           case porddef(def)^.typ of
              s32bit,s16bit,u16bit,s8bit,u8bit :
                begin
                   l:=porddef(def)^.von;
                   h:=porddef(def)^.bis;
                end;
              bool8bit : begin
                            l:=0;
                            h:=1;
                         end;
              uchar : begin
                         l:=0;
                         h:=255;
                      end;
              u32bit : begin
                          { this should work now }
                          l:=porddef(def)^.von;
                          h:=porddef(def)^.bis;
                       end;
           end
         else
           if def^.deftype=enumdef then
             begin
                l:=0;
                h:=penumdef(def)^.max;
             end;
      end;

    function get_ordinal_value(p : ptree) : longint;

      begin
         if p^.treetype=ordconstn then
           get_ordinal_value:=p^.value
         else
           Message(parser_e_ordinal_expected);
      end;

    function mmx_type(p : pdef) : tmmxtype;

      begin
         mmx_type:=mmxno;
         if is_mmx_able_array(p) then
           begin
              if parraydef(p)^.definition^.deftype=floatdef then
                case pfloatdef(parraydef(p)^.definition)^.typ of
                  s32real:
                    mmx_type:=mmxsingle;
                  f16bit:
                    mmx_type:=mmxfixed16
                end
              else
                case porddef(parraydef(p)^.definition)^.typ of
                   u8bit:
                     mmx_type:=mmxu8bit;
                   s8bit:
                     mmx_type:=mmxs8bit;
                   u16bit:
                     mmx_type:=mmxu16bit;
                   s16bit:
                     mmx_type:=mmxs16bit;
                   u32bit:
                     mmx_type:=mmxu32bit;
                   s32bit:
                     mmx_type:=mmxs32bit;
                end;
           end;
      end;

    function is_mmx_able_array(p : pdef) : boolean;

      begin
{$ifdef SUPPORT_MMX}
         if (cs_mmx_saturation in aktswitches) then
           begin
              is_mmx_able_array:=(p^.deftype=arraydef) and
                (
                 ((parraydef(p)^.definition^.deftype=orddef) and
                  (
                  (parraydef(p)^.lowrange=0) and
                  (parraydef(p)^.highrange=1) and
                  (porddef(parraydef(p)^.definition)^.typ in [u32bit,s32bit])
                  ) or
                  (
                  (parraydef(p)^.lowrange=0) and
                  (parraydef(p)^.highrange=3) and
                  (porddef(parraydef(p)^.definition)^.typ in [u16bit,s16bit])
                  )
                 )
                ) or
                (
                 ((parraydef(p)^.definition^.deftype=floatdef) and
                  (
                   (parraydef(p)^.lowrange=0) and
                   (parraydef(p)^.highrange=3) and
                   (pfloatdef(parraydef(p)^.definition)^.typ=f16bit)
                  ) or
                  (
                   (parraydef(p)^.lowrange=0) and
                   (parraydef(p)^.highrange=1) and
                   (pfloatdef(parraydef(p)^.definition)^.typ=s32real)
                  )
                 )
                );
           end
         else
           begin
              is_mmx_able_array:=(p^.deftype=arraydef) and
                (
                 ((parraydef(p)^.definition^.deftype=orddef) and
                  (
                  (parraydef(p)^.lowrange=0) and
                  (parraydef(p)^.highrange=1) and
                  (porddef(parraydef(p)^.definition)^.typ in [u32bit,s32bit])
                  ) or
                  (
                  (parraydef(p)^.lowrange=0) and
                  (parraydef(p)^.highrange=3) and
                  (porddef(parraydef(p)^.definition)^.typ in [u16bit,s16bit])
                  ) or
                  (
                  (parraydef(p)^.lowrange=0) and
                  (parraydef(p)^.highrange=7) and
                  (porddef(parraydef(p)^.definition)^.typ in [u8bit,s8bit])
                  )
                 )
                ) or
                (
                 ((parraydef(p)^.definition^.deftype=floatdef) and
                  (
                   (parraydef(p)^.lowrange=0) and
                   (parraydef(p)^.highrange=3) and
                   (pfloatdef(parraydef(p)^.definition)^.typ=f32bit)
                  )
                  or
                  (
                   (parraydef(p)^.lowrange=0) and
                   (parraydef(p)^.highrange=1) and
                   (pfloatdef(parraydef(p)^.definition)^.typ=s32real)
                  )
                 )
                );
           end;
{$else SUPPORT_MMX}
         is_mmx_able_array:=false;
{$endif SUPPORT_MMX}
      end;

    function is_equal(def1,def2 : pdef) : boolean;

      var
         b : boolean;
         hd : pdef;
         hp1,hp2 : pdefcoll;

      begin
         { both types must exists }
         if not (assigned(def1) and assigned(def2)) then
          begin
            is_equal:=false;
            exit;
          end;

         { be sure, that if there is a stringdef, that this is def1 }
         if def2^.deftype=stringdef then
           begin
              hd:=def1;
              def1:=def2;
              def2:=hd;
           end;
         b:=false;

         { wenn beide auf die gleiche Definition zeigen sind sie wohl gleich...}
         if def1=def2 then
           b:=true
         else
         { pointer with an equal definition are equal }
           if (def1^.deftype=pointerdef) and (def2^.deftype=pointerdef) then
         { here a problem detected in tabsolutesym }
         { the types can be forward type !!        }
             begin
                if assigned(def1^.sym) and ((def1^.sym^.properties and sp_forwarddef)<>0) then
                  b:=(def1^.sym=def2^.sym)
                else
                  b:=is_equal(ppointerdef(def1)^.definition,ppointerdef(def2)^.definition);
             end
         else
         { Grundtypen sind gleich, wenn sie den selben Grundtyp haben, }
         { und wenn noetig den selben Unterbereich haben }
           if (def1^.deftype=orddef) and (def2^.deftype=orddef) then
             begin
                case porddef(def1)^.typ of
                   u32bit,u8bit,s32bit,s8bit,u16bit,s16bit : begin
                                     if porddef(def1)^.typ=porddef(def2)^.typ then
                                       if (porddef(def1)^.von=porddef(def2)^.von) and
                                          (porddef(def1)^.bis=porddef(def2)^.bis) then
                                           b:=true;
                                  end;
                   uvoid,bool8bit,uchar :
                     b:=porddef(def1)^.typ=porddef(def2)^.typ;
                end;
             end
         else
           if (def1^.deftype=floatdef) and (def2^.deftype=floatdef) then
             b:=pfloatdef(def1)^.typ=pfloatdef(def2)^.typ
         else
            { strings with the same length are equal }
            if (def1^.deftype=stringdef) and (def2^.deftype=stringdef) and
               (pstringdef(def1)^.len=pstringdef(def2)^.len) then
            b:=true
    { STRING[N] ist equivalent zu ARRAY[0..N] OF CHAR (N<256) }
{
         else if ((def1^.deftype=stringdef) and (def2^.deftype=arraydef)) and
              (parraydef(def2)^.definition^.deftype=orddef) and
              (porddef(parraydef(def1)^.definition)^.typ=uchar) and
              (parraydef(def2)^.lowrange=0) and
              (parraydef(def2)^.highrange=pstringdef(def1)^.len) then
              b:=true }
          else
            if (def1^.deftype=formaldef) and (def2^.deftype=formaldef) then
              b:=true
          { file types with the same file element type are equal }
          { this is a problem for assign !!                      }
          { changed to allow if one is untyped                   }
          { all typed files are equal to the special             }
          { typed file that has voiddef as elemnt type           }
          { but must NOT match for text file !!!                 }
          else
            if (def1^.deftype=filedef) and (def2^.deftype=filedef) then
              b:=(pfiledef(def1)^.filetype=pfiledef(def2)^.filetype) and
                 ((
                 ((pfiledef(def1)^.typed_as=nil) and
                  (pfiledef(def2)^.typed_as=nil)) or
                 (
                  (pfiledef(def1)^.typed_as<>nil) and
                  (pfiledef(def2)^.typed_as<>nil) and
                  is_equal(pfiledef(def1)^.typed_as,pfiledef(def2)^.typed_as)
                 ) or
                 ( (pfiledef(def1)^.typed_as=pdef(voiddef)) or
                   (pfiledef(def2)^.typed_as=pdef(voiddef))
                 )))
          { sets with the same element type are equal }
          else
            if (def1^.deftype=setdef) and (def2^.deftype=setdef) then
              begin
                 if assigned(psetdef(def1)^.setof) and
                    assigned(psetdef(def2)^.setof) then
                   b:=is_equal(psetdef(def1)^.setof,psetdef(def2)^.setof)
                 else b:=true;
              end
          else
            if (def1^.deftype=procvardef) and (def2^.deftype=procvardef) then
              begin
                 { poassembler isn't important for compatibility }
                 b:=((pprocvardef(def1)^.options and not(poassembler))=
                     (pprocvardef(def2)^.options and not(poassembler))
                    ) and
                   is_equal(pprocvardef(def1)^.retdef,pprocvardef(def2)^.retdef);
                 { now evalute the parameters }
                 if b then
                   begin
                      hp1:=pprocvardef(def1)^.para1;
                      hp2:=pprocvardef(def1)^.para1;
                      while assigned(hp1) and assigned(hp2) do
                        begin
                           if not(is_equal(hp1^.data,hp2^.data)) or
                             not(hp1^.paratyp=hp2^.paratyp) then
                             begin
                                b:=false;
                                break;
                             end;
                           hp1:=hp1^.next;
                           hp2:=hp2^.next;
                        end;
                      b:=(hp1=nil) and (hp2=nil);
                   end;
              end
          else
            if (def1^.deftype=arraydef) and (def2^.deftype=arraydef) and
              (is_open_array(def1) or is_open_array(def2)) then
              begin
                 b:=is_equal(parraydef(def1)^.definition,parraydef(def2)^.definition);
              end
          else
            if (def1^.deftype=classrefdef) and (def2^.deftype=classrefdef) then
              begin
                 { similar to pointerdef: }
                 if assigned(def1^.sym) and ((def1^.sym^.properties and sp_forwarddef)<>0) then
                   b:=(def1^.sym=def2^.sym)
                 else
                   b:=is_equal(pclassrefdef(def1)^.definition,pclassrefdef(def2)^.definition);
              end;
         is_equal:=b;
      end;


    function is_subequal(def1, def2: pdef): boolean;
    Begin
      if assigned(def1) and assigned(def2) then
      Begin
        is_subequal := FALSE;
        if (def1^.deftype = orddef) and (def2^.deftype = orddef) then
          Begin
            { see p.47 of Turbo Pascal 7.01 manual for the separation of types }
            { range checking for case statements is done with testrange        }
            case porddef(def1)^.typ of
              s32bit,u32bit,u8bit,s8bit,s16bit,u16bit:
                Begin
{ PROBABLE CODE GENERATION BUG HERE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! }
{                   if porddef(def2)^.typ in [s32bit,u32bit,u8bit,s8bit,s16bit,u16bit] then
                     is_subequal := TRUE; }
                    if (porddef(def2)^.typ = s32bit) or
                       (porddef(def2)^.typ = u32bit) or
                       (porddef(def2)^.typ = u8bit) or
                       (porddef(def2)^.typ = s8bit) or
                       (porddef(def2)^.typ = s16bit) or
                       (porddef(def2)^.typ = u16bit) then
                     Begin
                       is_subequal:=TRUE;
                     end;
                end;
              bool8bit: if porddef(def2)^.typ = bool8bit then is_subequal := TRUE;
              uchar: if porddef(def2)^.typ = uchar then is_subequal := TRUE;
            end;
          end
        else
          Begin
            { I assume that both enumerations are equal when the first }
            { pointers are equal.                                      }
            if (def1^.deftype = enumdef) and (def2^.deftype =enumdef) then
              Begin
                if penumdef(def1)^.first = penumdef(def2)^.first then
                   is_subequal := TRUE;
              end;
          end;
      end; { endif assigned ... }
    end;

    type
       pprocdefcoll = ^tprocdefcoll;

       tprocdefcoll = record
          next : pprocdefcoll;
          data : pprocdef;
       end;

       psymcoll = ^tsymcoll;

       tsymcoll = record
          next : psymcoll;
          name : pstring;
          data : pprocdefcoll;
       end;

    var
       wurzel : psymcoll;
       nextvirtnumber : longint;
       _c : pobjectdef;
       has_constructor,has_virtual_method : boolean;

    procedure eachsym(sym : psym);{$ifndef FPC}far;{$endif}

      var
         procdefcoll : pprocdefcoll;
         hp : pprocdef;
         symcoll : psymcoll;
         _name : string;
         stored : boolean;

      begin
         { nur Unterprogrammsymbole werden in die VMT aufgenommen }
         if sym^.typ=procsym then
           begin
              _name:=sym^.name;
              symcoll:=wurzel;
              while assigned(symcoll) do
                begin
                   { wenn das Symbol in der Liste schon existiert }
                   if _name=symcoll^.name^ then
                     begin
                        { walk thorugh all defs of the symbol }
                        hp:=pprocsym(sym)^.definition;
                        while assigned(hp) do
                          begin
                             { compare with all stored definitions }
                             procdefcoll:=symcoll^.data;
                             stored:=false;
                             while assigned(procdefcoll) do
                               begin
                                  { compare parameters }
                                  if equal_paras(procdefcoll^.data^.para1,hp^.para1) and
                                     (
                                       ((procdefcoll^.data^.options and povirtualmethod)<>0) or
                                       ((hp^.options and povirtualmethod)<>0)
                                     ) then
                                    begin
                                       { wenn sie gleich sind }
                                       { und eine davon virtual deklariert ist }
                                       { Fehler falls nur eine VIRTUAL }
                                       if (procdefcoll^.data^.options and povirtualmethod)<>
                                          (hp^.options and povirtualmethod) then
                                            Message1(parser_e_overloaded_are_not_both_virtual,_c^.name^+'.'+_name);

                                       { check, if the overridden directive is set }
                                       { (povirtualmethod is set! }

                                       { class ? }
                                       if ((_c^.options and oois_class)<>0) and
                                         ((hp^.options and pooverridingmethod)=0) then
                                            Message1(parser_e_must_use_override,_c^.name^+'.'+_name);

                                       { error, if the return types aren't equal }
                                       if not(is_equal(procdefcoll^.data^.retdef,hp^.retdef)) then
                                         Message1(parser_e_overloaded_methodes_not_same_ret,_c^.name^+'.'+_name);


                                       { the flags have to match      }
                                       { except abstract and override }
                                       if (procdefcoll^.data^.options and not(poabstractmethod or pooverridingmethod))<>
                                         (hp^.options and not(poabstractmethod or pooverridingmethod)) then
                                            Message1(parser_e_header_dont_match_forward,_c^.name^+'.'+_name);

                                       { now set the number }
                                       hp^.extnumber:=procdefcoll^.data^.extnumber;
                                       { and exchange }
                                       procdefcoll^.data:=hp;
                                       stored:=true;
                                    end;
                                  procdefcoll:=procdefcoll^.next;
                               end;
                             { if it isn't saved in the list }
                             { we create a new entry         }
                             if not(stored) then
                               begin
                                  new(procdefcoll);
                                  procdefcoll^.data:=hp;
                                  procdefcoll^.next:=symcoll^.data;
                                  symcoll^.data:=procdefcoll;
                                  { if the method is virtual ... }
                                  if (hp^.options and povirtualmethod)<>0 then
                                    begin
                                       { ... it will get a number }
                                       hp^.extnumber:=nextvirtnumber;
                                       inc(nextvirtnumber);
                                    end;
                                  { check, if a method should be overridden }
                                  if (hp^.options and pooverridingmethod)<>0 then
                                   Message1(parser_e_nothing_to_be_overridden,_c^.name^+'.'+_name);
                               end;
                             hp:=hp^.nextoverloaded;
                          end;
                        exit;
                     end;
                   symcoll:=symcoll^.next;
                end;
              { if not, generate a new symbol item }
              new(symcoll);
              symcoll^.name:=stringdup(sym^.name);
              symcoll^.next:=wurzel;
              symcoll^.data:=nil;
              wurzel:=symcoll;
              hp:=pprocsym(sym)^.definition;

              { inserts all definitions }
              while assigned(hp) do
                begin
                   new(procdefcoll);
                   procdefcoll^.data:=hp;
                   procdefcoll^.next:=symcoll^.data;
                   symcoll^.data:=procdefcoll;

                   { if it's a virtual method }
                   if (hp^.options and povirtualmethod)<>0 then
                     begin
                        { then it gets a number ... }
                        hp^.extnumber:=nextvirtnumber;
                        { and we inc the number }
                        inc(nextvirtnumber);
                        has_virtual_method:=true;
                     end;

                   if (hp^.options and poconstructor)<>0 then
                     has_constructor:=true;

                   { check, if a method should be overridden }
                   if (hp^.options and pooverridingmethod)<>0 then
                     Message1(parser_e_nothing_to_be_overridden,_c^.name^+'.'+_name);
                   { next overloaded method }
                   hp:=hp^.nextoverloaded;
                end;
           end;
      end;

    procedure genvmt(_class : pobjectdef);

      procedure do_genvmt(p : pobjectdef);

        begin
           { start with the base class }
           if assigned(p^.childof) then
             do_genvmt(p^.childof);

           { walk through all public syms }
           _c:=_class;
{$ifdef tp}
           p^.publicsyms^.foreach(eachsym);
{$else}
           p^.publicsyms^.foreach(@eachsym);
{$endif}
        end;

      var
         symcoll : psymcoll;
         procdefcoll : pprocdefcoll;
         i : longint;

      begin
         wurzel:=nil;
         nextvirtnumber:=0;

         has_constructor:=false;
         has_virtual_method:=false;

         { generates a tree of all used methods }
         do_genvmt(_class);

         if has_virtual_method and not(has_constructor) then
           begin
              exterror:=strpnew(_class^.name^);
              Message(parser_w_virtual_without_constructor);
           end;
         { generates the VMT }

         { walk trough all numbers for virtual methods and search }
         { the method                                             }
         for i:=0 to nextvirtnumber-1 do
           begin
              symcoll:=wurzel;

              { walk trough all symbols }
              while assigned(symcoll) do
                begin

                   { walk trough all methods }
                   procdefcoll:=symcoll^.data;
                   while assigned(procdefcoll) do
                     begin
                        { writes the addresses to the VMT }
                        { but only this which are declared as virtual }
                        if procdefcoll^.data^.extnumber=i then
                          begin
                             if (procdefcoll^.data^.options and povirtualmethod)<>0 then
                               begin
                                  { if a method is abstract, then is also the }
                                  { class abstract and it's not allow to      }
                                  { generates an instance                     }
                                  if (procdefcoll^.data^.options and poabstractmethod)<>0 then
                                    begin
                                       _class^.options:=_class^.options or oois_abstract;
                                       datasegment^.concat(new(pai_const,init_symbol('ABSTRACTERROR')));
                                    end
                                  else
                                    begin
                                      datasegment^.concat(new(pai_const,init_symbol(
                                        strpnew(procdefcoll^.data^.mangledname))));
                                      if (procdefcoll^.data^.options and povirtualmethod)<>0 then
                                        maybe_concat_external(procdefcoll^.data^.owner,
                                          procdefcoll^.data^.mangledname);
                                    end;
                               end;
                          end;
                        procdefcoll:=procdefcoll^.next;
                     end;
                   symcoll:=symcoll^.next;
                end;
           end;
         { disposes the above generated tree }
         symcoll:=wurzel;
         while assigned(symcoll) do
           begin
              wurzel:=symcoll^.next;
              stringdispose(symcoll^.name);
              procdefcoll:=symcoll^.data;
              while assigned(procdefcoll) do
                begin
                   symcoll^.data:=procdefcoll^.next;
                   dispose(procdefcoll);
                   procdefcoll:=symcoll^.data;
                end;
              dispose(symcoll);
              symcoll:=wurzel;
           end;
      end;

end.
{
  $Log$
  Revision 1.3  1998-04-08 11:34:22  peter
    * nasm works (linux only tested)

  Revision 1.2  1998/03/28 23:09:57  florian
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

  Revision 1.1.1.1  1998/03/25 11:18:15  root
  * Restored version

  Revision 1.24  1998/03/21 23:59:40  florian
    * indexed properties fixed
    * ppu i/o of properties fixed
    * field can be also used for write access
    * overriding of properties

  Revision 1.23  1998/03/20 23:31:35  florian
    * bug0113 fixed
    * problem with interdepened units fixed ("options.pas problem")
    * two small extensions for future AMD 3D support

  Revision 1.22  1998/03/10 01:17:30  peter
    * all files have the same header
    * messages are fully implemented, EXTDEBUG uses Comment()
    + AG... files for the Assembler generation

  Revision 1.21  1998/03/06 01:09:01  peter
    * removed the conflicts that had occured

  Revision 1.20  1998/03/06 00:53:01  peter
    * replaced all old messages from errore.msg, only ExtDebug and some
      Comment() calls are left
    * fixed options.pas

  Revision 1.19  1998/03/05 22:40:56  florian
    + warning about missing constructor added

  Revision 1.18  1998/03/04 17:34:14  michael
  + Changed ifdef FPK to ifdef FPC

  Revision 1.17  1998/03/02 01:49:38  peter
    * renamed target_DOS to target_GO32V1
    + new verbose system, merged old errors and verbose units into one new
      verbose.pas, so errors.pas is obsolete

  Revision 1.16  1998/02/13 10:35:55  daniel
  * Made Motorola version compilable.
  * Fixed optimizer

  Revision 1.15  1998/02/12 17:19:33  florian
    * fixed to get remake3 work, but needs additional fixes (output, I don't like
      also that aktswitches isn't a pointer)

  Revision 1.14  1998/02/12 11:50:52  daniel
  Yes! Finally! After three retries, my patch!

  Changes:

  Complete rewrite of psub.pas.
  Added support for DLL's.
  Compiler requires less memory.
  Platform units for each platform.

  Revision 1.13  1998/02/11 21:56:41  florian
    * bugfixes: bug0093, bug0053, bug0088, bug0087, bug0089

  Revision 1.12  1998/02/07 23:05:08  florian
    * once more MMX

  Revision 1.11  1998/02/06 10:34:35  florian
    * bug0082 and bug0084 fixed

  Revision 1.10  1998/02/05 22:27:07  florian
    * small problems fixed: remake3 should now work

  Revision 1.9  1998/02/05 21:54:36  florian
    + more MMX

  Revision 1.8  1998/01/31 00:43:37  carl
    - removed in in is_subequal, because the code generator is buggy!
      (instead uses if...)

  Revision 1.7  1998/01/16 18:03:21  florian
    * small bug fixes, some stuff of delphi styled constructores added

  Revision 1.6  1998/01/11 19:24:35  carl
    + type checking routine (is_subequal) for case statements

  Revision 1.5  1998/01/09 23:08:38  florian
    + C++/Delphi styled //-comments
    * some bugs in Delphi object model fixed
    + override directive

  Revision 1.4  1998/01/09 16:08:24  florian
    * abstract methods call now abstracterrorproc if they are called
      a class with an abstract method can be create with a class reference else
      the compiler forbides this

  Revision 1.3  1998/01/07 00:17:12  michael
  Restored released version (plus fixes) as current

  Revision 1.2  1997/11/28 18:14:51  pierre
   working version with several bug fixes

  Revision 1.1.1.1  1997/11/27 08:33:03  michael
  FPC Compiler CVS start


  Pre-CVS log:

  CEC   Carl-Eric Codere
  FK    Florian Klaempfl
  PM    Pierre Muller
  +     feature added
  -     removed
  *     bug fixed or changed

  History:
      22th september 1997
         + function dont_copy_const_param added (FK)
      25th september 1997
         + is_open_array added (FK)
         + is_equal handles now also open arrays (FK)
      2nd october 1997
         + added then boolean never_copy_const_param for use in typed write
           where we must push the reference anyway (PM)
      3rd october 1997:
         + renamed ret_in_eax to ret_in_acc (for accumulator for port.) (CEC)
         - removed reference to i386 unit (CEC)
     25th october 1997:
         * poassembler isn't important for compatiblity of proc vars (FK)
      3rd november 1997:
         + added formaldef type to types where we dont_copy_const_param (PM)
      20rd november 1997:
         + added is_fpu function (PM)
}
