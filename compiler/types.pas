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
       cobjects,globals,symtable;

    type
       tmmxtype = (mmxno,mmxu8bit,mmxs8bit,mmxu16bit,mmxs16bit,
                   mmxu32bit,mmxs32bit,mmxfixed16,mmxsingle);

    { returns true, if def defines an ordinal type }
    function is_ordinal(def : pdef) : boolean;

    { returns true, if def defines an ordinal type }
    function is_integer(def : pdef) : boolean;

    { true if p points to an open array def }
    function is_open_array(p : pdef) : boolean;

    { true if o is an ansi string def }
    function is_ansistring(p : pdef) : boolean;

    { true if o is a long string def }
    function is_longstring(p : pdef) : boolean;

    { true if o is a wide string def }
    function is_widestring(p : pdef) : boolean;

    { true if o is a short string def }
    function is_shortstring(p : pdef) : boolean;

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

    { true, if two parameter lists are equal        }
    { if value_equal_const is true, call by value   }
    { and call by const parameter are assumed as    }
    { equal                                         }
    function equal_paras(def1,def2 : pdefcoll;value_equal_const : boolean) : boolean;

    { true if a function can be assigned to a procvar }
    function proc_to_procvar_equal(def1,def2 : pabstractprocdef) : boolean;

    { if l isn't in the range of def a range check error is generated }
    procedure testrange(def : pdef;l : longint);

    { returns the range of def }
    procedure getrange(def : pdef;var l : longint;var h : longint);

    { generates a VMT for _class }
    procedure genvmt(_class : pobjectdef);

    { some type helper routines for MMX support }
    function is_mmx_able_array(p : pdef) : boolean;

    { returns the mmx type }
    function mmx_type(p : pdef) : tmmxtype;

  implementation

    uses verbose,aasm;

    function equal_paras(def1,def2 : pdefcoll;value_equal_const : boolean) : boolean;
      begin
         while (assigned(def1)) and (assigned(def2)) do
           begin
              if value_equal_const then
                begin
                   if not(is_equal(def1^.data,def2^.data)) or
                     ((def1^.paratyp<>def2^.paratyp) and
                      ((def1^.paratyp=vs_var) or
                       (def1^.paratyp=vs_var)
                      )
                     ) then
                     begin
                        equal_paras:=false;
                        exit;
                     end;
                end
              else
                begin
                   if not(is_equal(def1^.data,def2^.data)) or
                     (def1^.paratyp<>def2^.paratyp) then
                     begin
                        equal_paras:=false;
                        exit;
                     end;
                end;
              def1:=def1^.next;
              def2:=def2^.next;
           end;
         if (def1=nil) and (def2=nil) then
           equal_paras:=true
         else
           equal_paras:=false;
      end;


    { true if a function can be assigned to a procvar }
    function proc_to_procvar_equal(def1,def2 : pabstractprocdef) : boolean;
      begin
         if is_equal(def1^.retdef,def2^.retdef) and
            equal_paras(def1^.para1,def2^.para1,false) and
            ((def1^.options and po_comptatibility_options)=
             (def2^.options and po_comptatibility_options)) then
           proc_to_procvar_equal:=true
         else
           proc_to_procvar_equal:=false;
      end;


    { returns true, if def uses FPU }
    function is_fpu(def : pdef) : boolean;
      begin
         is_fpu:=(def^.deftype=floatdef) and (pfloatdef(def)^.typ<>f32bit);
      end;


    { true if p is an ordinal }
    function is_ordinal(def : pdef) : boolean;
      var
         dt : tbasetype;
      begin
         case def^.deftype of
          orddef : begin
                     dt:=porddef(def)^.typ;
                     is_ordinal:=dt in [uchar,u8bit,u16bit,u32bit,s8bit,s16bit,s32bit,bool8bit,bool16bit,bool32bit];
                   end;
         enumdef : is_ordinal:=true;
         else
           is_ordinal:=false;
         end;
      end;


    { true if p is an integer }
    function is_integer(def : pdef) : boolean;
      begin
        is_integer:=(def^.deftype=orddef) and
                    (porddef(def)^.typ in [uauto,u8bit,u16bit,u32bit,s8bit,s16bit,s32bit]);
      end;


    { true if p is signed (integer) }
    function is_signed(def : pdef) : boolean;
      var
         dt : tbasetype;
      begin
         case def^.deftype of
            orddef : begin
                       dt:=porddef(def)^.typ;
                       is_signed:=(dt in [s8bit,s16bit,s32bit]);
                     end;
           enumdef : is_signed:=false;
         else
           is_signed:=false;
         end;
      end;


    { true, if p points to an open array def }
    function is_open_array(p : pdef) : boolean;
      begin
         is_open_array:=(p^.deftype=arraydef) and
                        (parraydef(p)^.lowrange=0) and
                        (parraydef(p)^.highrange=-1);
      end;


    { true if p is an ansi string def }
    function is_ansistring(p : pdef) : boolean;
      begin
         is_ansistring:=(p^.deftype=stringdef) and
                        (pstringdef(p)^.string_typ=st_ansistring);
      end;


    { true if p is an long string def }
    function is_longstring(p : pdef) : boolean;
      begin
         is_longstring:=(p^.deftype=stringdef) and
                        (pstringdef(p)^.string_typ=st_longstring);
      end;


    { true if p is an wide string def }
    function is_widestring(p : pdef) : boolean;
      begin
         is_widestring:=(p^.deftype=stringdef) and
                        (pstringdef(p)^.string_typ=st_widestring);
      end;


    { true if p is an short string def }
    function is_shortstring(p : pdef) : boolean;
      begin
         is_shortstring:=(p^.deftype=stringdef) and
                         (pstringdef(p)^.string_typ=st_shortstring);
      end;


    { true if the return value is in accumulator (EAX for i386), D0 for 68k }
    function ret_in_acc(def : pdef) : boolean;

      begin
         ret_in_acc:=(def^.deftype in [orddef,pointerdef,enumdef,classrefdef]) or
                     ((def^.deftype=stringdef) and (pstringdef(def)^.string_typ in [st_ansistring,st_widestring])) or
                     ((def^.deftype=procvardef) and ((pprocvardef(def)^.options and pomethodpointer)=0)) or
                     ((def^.deftype=objectdef) and pobjectdef(def)^.isclass) or
                     ((def^.deftype=setdef) and (psetdef(def)^.settype=smallset)) or
                     ((def^.deftype=floatdef) and (pfloatdef(def)^.typ=f32bit));
      end;


    { true if uses a parameter as return value }
    function ret_in_param(def : pdef) : boolean;
      begin
         ret_in_param:=(def^.deftype in [arraydef,recorddef]) or
           ((def^.deftype=stringdef) and (pstringdef(def)^.string_typ in [st_shortstring,st_longstring])) or
           ((def^.deftype=procvardef) and ((pprocvardef(def)^.options and pomethodpointer)<>0)) or
           ((def^.deftype=objectdef) and ((pobjectdef(def)^.options and oois_class)=0)) or
           ((def^.deftype=setdef) and (psetdef(def)^.settype<>smallset));
      end;


    { true if a const parameter is too large to copy }
    function dont_copy_const_param(def : pdef) : boolean;
      begin
         dont_copy_const_param:=(def^.deftype in [arraydef,objectdef,formaldef,recorddef]) or
           ((def^.deftype=stringdef) and (pstringdef(def)^.string_typ in [st_shortstring,st_longstring])) or
           ((def^.deftype=procvardef) and ((pprocvardef(def)^.options and pomethodpointer)<>0)) or
           ((def^.deftype=setdef) and (psetdef(def)^.settype<>smallset));
      end;


    { test if l is in the range of def, outputs error if out of range }
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


    { return the range from def in l and h }
    procedure getrange(def : pdef;var l : longint;var h : longint);
      begin
        case def^.deftype of
         orddef : begin
                    l:=porddef(def)^.low;
                    h:=porddef(def)^.high;
                  end;
        enumdef : begin
                    l:=penumdef(def)^.min;
                    h:=penumdef(def)^.max;
                  end;
        end;
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
         if (cs_mmx_saturation in aktlocalswitches) then
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
         { ordinals are equal only when the ordinal type is equal }
           if (def1^.deftype=orddef) and (def2^.deftype=orddef) then
             begin
                case porddef(def1)^.typ of
                u8bit,u16bit,u32bit,
                s8bit,s16bit,s32bit:
                  b:=((porddef(def1)^.typ=porddef(def2)^.typ) and
                   (porddef(def1)^.low=porddef(def2)^.low) and
                   (porddef(def1)^.high=porddef(def2)^.high));
                uvoid,uchar,
                bool8bit,bool16bit,bool32bit:
                  b:=(porddef(def1)^.typ=porddef(def2)^.typ);
                end;
             end
         else
           if (def1^.deftype=floatdef) and (def2^.deftype=floatdef) then
             b:=pfloatdef(def1)^.typ=pfloatdef(def2)^.typ
         else
            { strings with the same length are equal }
            if (def1^.deftype=stringdef) and (def2^.deftype=stringdef) and
               (pstringdef(def1)^.string_typ=pstringdef(def2)^.string_typ) then
               begin
                  b:=not(is_shortstring(def1)) or
                    (pstringdef(def1)^.len=pstringdef(def2)^.len);
               end
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
                   b:=(psetdef(def1)^.setof^.deftype=psetdef(def2)^.setof^.deftype)
                 else
                   b:=true;
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
           u8bit,u16bit,u32bit,
           s8bit,s16bit,s32bit : is_subequal:=(porddef(def2)^.typ in [s32bit,u32bit,u8bit,s8bit,s16bit,u16bit]);
  bool8bit,bool16bit,bool32bit : is_subequal:=(porddef(def2)^.typ in [bool8bit,bool16bit,bool32bit]);
                         uchar : is_subequal:=(porddef(def2)^.typ=uchar);
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

      { creates a new entry in the procsym list }
      procedure newentry;

        begin
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

      begin
         { put only sub routines into the VMT }
         if sym^.typ=procsym then
           begin
              _name:=sym^.name;
              symcoll:=wurzel;
              while assigned(symcoll) do
                begin
                   { does the symbol already exist in the list ? }
                   if _name=symcoll^.name^ then
                     begin
                        { walk through all defs of the symbol }
                        hp:=pprocsym(sym)^.definition;
                        while assigned(hp) do
                          begin
                             { compare with all stored definitions }
                             procdefcoll:=symcoll^.data;
                             stored:=false;
                             while assigned(procdefcoll) do
                               begin
                                  { compare parameters }
                                  if equal_paras(procdefcoll^.data^.para1,hp^.para1,false) and
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
                                         begin
                                            { in classes, we hide the old method }
                                            if _c^.isclass then
                                              begin
                                                 { warn only if it is the first time,
                                                   we hide the method }
                                                 if _c=hp^._class then
                                                   Message1(parser_w_should_use_override,_c^.name^+'.'+_name);
                                                 newentry;
                                                 exit;
                                              end
                                            else
                                              begin
                                                 Message1(parser_e_overloaded_are_not_both_virtual,_c^.name^+'.'+_name);
                                              end;
                                         end;

                                       { check, if the overridden directive is set }
                                       { (povirtualmethod is set! }

                                       { class ? }
                                       if _c^.isclass and
                                         ((hp^.options and pooverridingmethod)=0) then
                                         begin
                                            { warn only if it is the first time,
                                              we hide the method }
                                            if _c=hp^._class then
                                              Message1(parser_w_should_use_override,_c^.name^+'.'+_name);
                                            newentry;
                                            exit;
                                         end;

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
             newentry;
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
            Message1(parser_w_virtual_without_constructor,_class^.name^);


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
  Revision 1.24  1998-09-04 08:36:49  peter
    * fixed boolean:=integer which is not explicit

  Revision 1.23  1998/09/01 17:39:55  peter
    + internal constant functions

  Revision 1.22  1998/09/01 12:53:28  peter
    + aktpackenum

  Revision 1.21  1998/08/19 00:42:45  peter
    + subrange types for enums
    + checking for bounds type with ranges

  Revision 1.20  1998/08/18 14:17:14  pierre
    * bug about assigning the return value of a function to
      a procvar fixed : warning
      assigning a proc to a procvar need @ in FPC mode !!
    * missing file/line info restored

  Revision 1.19  1998/08/18 09:24:48  pierre
    * small warning position bug fixed
    * support_mmx switches splitting was missing
    * rhide error and warning output corrected

  Revision 1.18  1998/08/14 18:18:49  peter
    + dynamic set contruction
    * smallsets are now working (always longint size)

  Revision 1.17  1998/08/05 16:00:17  florian
    * some fixes for ansi strings

  Revision 1.16  1998/07/20 23:35:50  michael
  Const ansistrings are not copied.

  Revision 1.15  1998/07/18 22:54:32  florian
    * some ansi/wide/longstring support fixed:
       o parameter passing
       o returning as result from functions

  Revision 1.14  1998/06/12 14:50:50  peter
    * removed the tree dependency to types.pas
    * long_fil.pas support (not fully tested yet)

  Revision 1.13  1998/06/03 22:49:07  peter
    + wordbool,longbool
    * rename bis,von -> high,low
    * moved some systemunit loading/creating to psystem.pas

  Revision 1.12  1998/05/12 10:47:00  peter
    * moved printstatus to verb_def
    + V_Normal which is between V_Error and V_Warning and doesn't have a
      prefix like error: warning: and is included in V_Default
    * fixed some messages
    * first time parameter scan is only for -v and -T
    - removed old style messages

  Revision 1.11  1998/05/01 16:38:46  florian
    * handling of private and protected fixed
    + change_keywords_to_tp implemented to remove
      keywords which aren't supported by tp
    * break and continue are now symbols of the system unit
    + widestring, longstring and ansistring type released

  Revision 1.10  1998/04/29 10:34:08  pierre
    + added some code for ansistring (not complete nor working yet)
    * corrected operator overloading
    * corrected nasm output
    + started inline procedures
    + added starstarn : use ** for exponentiation (^ gave problems)
    + started UseTokenInfo cond to get accurate positions

  Revision 1.9  1998/04/21 10:16:49  peter
    * patches from strasbourg
    * objects is not used anymore in the fpc compiled version

  Revision 1.8  1998/04/12 22:39:44  florian
    * problem with read access to properties solved
    * correct handling of hidding methods via virtual (COM)
    * correct result type of constructor calls (COM), the resulttype
      depends now on the type of the class reference

  Revision 1.7  1998/04/10 21:36:56  florian
    + some stuff to support method pointers (procedure of object) added
      (declaration, parameter handling)

  Revision 1.6  1998/04/10 15:39:49  florian
    * more fixes to get classes.pas compiled

  Revision 1.5  1998/04/09 23:02:16  florian
    * small problems solved to get remake3 work

  Revision 1.4  1998/04/08 16:58:09  pierre
    * several bugfixes
      ADD ADC and AND are also sign extended
      nasm output OK (program still crashes at end
      and creates wrong assembler files !!)
      procsym types sym in tdef removed !!

  Revision 1.3  1998/04/08 11:34:22  peter
    * nasm works (linux only tested)
}
