{
    Copyright (c) 1998-2002 by Florian Klaempfl

    Generate assembler for constant nodes which are the same for
    all (most) processors

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
unit ncgcon;

{$i fpcdefs.inc}

interface

    uses
       aasmbase,
       node,ncon;

    type
       tcgdataconstnode = class(tdataconstnode)
          procedure pass_generate_code;override;
       end;

       tcgrealconstnode = class(trealconstnode)
          procedure pass_generate_code;override;
       end;

       tcgordconstnode = class(tordconstnode)
          procedure pass_generate_code;override;
       end;

       tcgpointerconstnode = class(tpointerconstnode)
          procedure pass_generate_code;override;
       end;

       tcgstringconstnode = class(tstringconstnode)
          procedure pass_generate_code;override;
       end;

       tcgsetconstnode = class(tsetconstnode)
         protected
          function emitvarsetconst: tasmsymbol; virtual;
          procedure handlevarsetconst;
         public
          procedure pass_generate_code;override;
       end;

       tcgnilnode = class(tnilnode)
          procedure pass_generate_code;override;
       end;

       tcgguidconstnode = class(tguidconstnode)
          procedure pass_generate_code;override;
       end;


implementation

    uses
      globtype,widestr,systems,
      verbose,globals,cutils,
      symconst,symdef,aasmtai,aasmdata,aasmcpu,defutil,
      cpuinfo,cpubase,
      cgbase,cgobj,cgutils,
      ncgutil, cclasses,asmutils,tgobj
      ;


{*****************************************************************************
                           TCGDATACONSTNODE
*****************************************************************************}

    procedure tcgdataconstnode.pass_generate_code;
      var
        l : tasmlabel;
        i : longint;
        b : byte;
      begin
        location_reset_ref(location,LOC_CREFERENCE,OS_NO,const_align(maxalign));
        current_asmdata.getdatalabel(l);
        maybe_new_object_file(current_asmdata.asmlists[al_typedconsts]);
        new_section(current_asmdata.asmlists[al_typedconsts],sec_rodata_norel,l.name,const_align(maxalign));
        current_asmdata.asmlists[al_typedconsts].concat(Tai_label.Create(l));
        data.seek(0);
        for i:=0 to data.size-1 do
          begin
            data.read(b,1);
            current_asmdata.asmlists[al_typedconsts].concat(Tai_const.Create_8bit(b));
          end;
        location.reference.symbol:=l;
      end;

{*****************************************************************************
                           TCGREALCONSTNODE
*****************************************************************************}

    procedure tcgrealconstnode.pass_generate_code;
      { I suppose the parser/pass_1 must make sure the generated real  }
      { constants are actually supported by the target processor? (JM) }
      const
        floattype2ait:array[tfloattype] of taitype=
          (ait_real_32bit,ait_real_64bit,ait_real_80bit,ait_real_80bit,ait_comp_64bit,ait_comp_64bit,ait_real_128bit);

      { Since the value is stored always as bestreal, we share a single pool
        between all float types. This requires type and hiloswapped flag to
        be matched along with the value }
      type
        tfloatkey = record
          value: bestreal;
          typ: tfloattype;
          swapped: boolean;
        end;

      var
         lastlabel : tasmlabel;
         realait : taitype;
         entry : PHashSetItem;
         key: tfloatkey;
{$ifdef ARM}
         hiloswapped : boolean;
{$endif ARM}

      begin
        location_reset_ref(location,LOC_CREFERENCE,def_cgsize(resultdef),const_align(resultdef.alignment));
        lastlabel:=nil;
        realait:=floattype2ait[tfloatdef(resultdef).floattype];
{$ifdef ARM}
        hiloswapped:=is_double_hilo_swapped;
{$endif ARM}
        { const already used ? }
        if not assigned(lab_real) then
          begin
            { there may be gap between record fields, zero it out }
            fillchar(key,sizeof(key),0);
            key.value:=value_real;
            key.typ:=tfloatdef(resultdef).floattype;
{$ifdef ARM}
            key.swapped:=hiloswapped;
{$endif ARM}
            entry := current_asmdata.ConstPools[sp_floats].FindOrAdd(@key, sizeof(key));

            lab_real := TAsmLabel(entry^.Data);  // is it needed anymore?

             { :-(, we must generate a new entry }
             if not(assigned(lab_real)) then
               begin
                  current_asmdata.getdatalabel(lastlabel);
                  entry^.Data:=lastlabel;
                  lab_real:=lastlabel;
                  maybe_new_object_file(current_asmdata.asmlists[al_typedconsts]);
                  new_section(current_asmdata.asmlists[al_typedconsts],sec_rodata_norel,lastlabel.name,const_align(resultdef.alignment));
                  current_asmdata.asmlists[al_typedconsts].concat(Tai_label.Create(lastlabel));
                  case realait of
                    ait_real_32bit :
                      begin
                        current_asmdata.asmlists[al_typedconsts].concat(Tai_real_32bit.Create(ts32real(value_real)));
                        { range checking? }
                        if floating_point_range_check_error and
                          (tai_real_32bit(current_asmdata.asmlists[al_typedconsts].last).value=MathInf.Value) then
                          Message(parser_e_range_check_error);
                      end;

                    ait_real_64bit :
                      begin
{$ifdef ARM}
                        if hiloswapped then
                          current_asmdata.asmlists[al_typedconsts].concat(Tai_real_64bit.Create_hiloswapped(ts64real(value_real)))
                        else
{$endif ARM}
                          current_asmdata.asmlists[al_typedconsts].concat(Tai_real_64bit.Create(ts64real(value_real)));

                        { range checking? }
                        if floating_point_range_check_error and
                          (tai_real_64bit(current_asmdata.asmlists[al_typedconsts].last).value=MathInf.Value) then
                          Message(parser_e_range_check_error);
                     end;

                    ait_real_80bit :
                      begin
                        current_asmdata.asmlists[al_typedconsts].concat(Tai_real_80bit.Create(value_real,resultdef.size));

                        { range checking? }
                        if floating_point_range_check_error and
                          (tai_real_80bit(current_asmdata.asmlists[al_typedconsts].last).value=MathInf.Value) then
                          Message(parser_e_range_check_error);
                      end;
{$ifdef cpufloat128}
                    ait_real_128bit :
                      begin
                        current_asmdata.asmlists[al_typedconsts].concat(Tai_real_128bit.Create(value_real));

                        { range checking? }
                        if floating_point_range_check_error and
                          (tai_real_128bit(current_asmdata.asmlists[al_typedconsts].last).value=MathInf.Value) then
                          Message(parser_e_range_check_error);
                      end;
{$endif cpufloat128}

                    { the round is necessary for native compilers where comp isn't a float }
                    ait_comp_64bit :
                      if (value_real>9223372036854775807.0) or (value_real<-9223372036854775808.0) then
                        message(parser_e_range_check_error)
                      else
                        current_asmdata.asmlists[al_typedconsts].concat(Tai_comp_64bit.Create(round(value_real)));
                  else
                    internalerror(10120);
                  end;
               end;
          end;
        location.reference.symbol:=lab_real;
      end;

{*****************************************************************************
                            TCGORDCONSTNODE
*****************************************************************************}

    procedure tcgordconstnode.pass_generate_code;
      begin
         location_reset(location,LOC_CONSTANT,def_cgsize(resultdef));
{$ifdef cpu64bitalu}
         location.value:=value.svalue;
{$else cpu64bitalu}
         location.value64:=value.svalue;
{$endif cpu64bitalu}
      end;


{*****************************************************************************
                          TCGPOINTERCONSTNODE
*****************************************************************************}

    procedure tcgpointerconstnode.pass_generate_code;
      begin
         { an integer const. behaves as a memory reference }
         location_reset(location,LOC_CONSTANT,OS_ADDR);
         location.value:=aint(value);
      end;


{*****************************************************************************
                          TCGSTRINGCONSTNODE
*****************************************************************************}

    procedure tcgstringconstnode.pass_generate_code;
      var
         lastlabel: tasmlabofs;
         pc: pchar;
         l: longint;
         href: treference;
         pool: THashSet;
         entry: PHashSetItem;
         winlikewidestring: boolean;

      const
        PoolMap: array[tconststringtype] of TConstPoolType = (
          sp_conststr,
          sp_shortstr,
          sp_longstr,
          sp_ansistr,
          sp_widestr,
          sp_unicodestr
        );
      begin
         { for empty ansistrings we could return a constant 0 }
         if (cst_type in [cst_ansistring,cst_widestring,cst_unicodestring]) and (len=0) then
          begin
            location_reset(location,LOC_CONSTANT,OS_ADDR);
            location.value:=0;
            exit;
          end;
         winlikewidestring:=(cst_type=cst_widestring) and (tf_winlikewidestring in target_info.flags);
         { const already used ? }
         if not assigned(lab_str) then
           begin
              pool := current_asmdata.ConstPools[PoolMap[cst_type]];

              if cst_type in [cst_widestring, cst_unicodestring] then
                entry := pool.FindOrAdd(pcompilerwidestring(value_str)^.data,len*cwidechartype.size)
              else
              if cst_type = cst_ansistring then
                entry := PHashSetItem(TTagHashSet(pool).FindOrAdd(value_str,len,tstringdef(resultdef).encoding))
              else
                entry := pool.FindOrAdd(value_str,len);

              lab_str := TAsmLabel(entry^.Data);  // is it needed anymore?

              { :-(, we must generate a new entry }
              if not assigned(entry^.Data) then
                begin
                   case cst_type of
                      cst_ansistring:
                        begin
                           if len=0 then
                             InternalError(2008032301)   { empty string should be handled above }
                           else
                             begin
                               lastlabel:=emit_ansistring_const(current_asmdata.AsmLists[al_typedconsts],value_str,len,tstringdef(resultdef).encoding);
                               { because we hardcode the offset below due to it
                                 not being stored in the hashset, check here }
                               if lastlabel.ofs<>get_string_symofs(st_ansistring,false) then
                                 internalerror(2012051703);
                             end;
                        end;
                      cst_unicodestring,
                      cst_widestring:
                        begin
                           if len=0 then
                             InternalError(2008032302)   { empty string should be handled above }
                           else
                             begin
                               lastlabel := emit_unicodestring_const(current_asmdata.AsmLists[al_typedconsts],
                                               value_str,
                                               tstringdef(resultdef).encoding,
                                               winlikewidestring);
                               { because we hardcode the offset below due to it
                                 not being stored in the hashset, check here }
                               if lastlabel.ofs<>get_string_symofs(tstringdef(resultdef).stringtype,winlikewidestring) then
                                 internalerror(2012051704);
                             end;
                        end;
                      cst_shortstring:
                        begin
                          current_asmdata.getdatalabel(lastlabel.lab);
                          maybe_new_object_file(current_asmdata.asmlists[al_typedconsts]);
                          new_section(current_asmdata.asmlists[al_typedconsts],sec_rodata_norel,lastlabel.lab.name,const_align(sizeof(pint)));

                          current_asmdata.asmlists[al_typedconsts].concat(Tai_label.Create(lastlabel.lab));
                          { truncate strings larger than 255 chars }
                          if len>255 then
                           l:=255
                          else
                           l:=len;
                          { include length and terminating zero for quick conversion to pchar }
                          getmem(pc,l+2);
                          move(value_str^,pc[1],l);
                          pc[0]:=chr(l);
                          pc[l+1]:=#0;
                          current_asmdata.asmlists[al_typedconsts].concat(Tai_string.Create_pchar(pc,l+2));
                        end;
                      cst_conststring:
                        begin
                          current_asmdata.getdatalabel(lastlabel.lab);
                          maybe_new_object_file(current_asmdata.asmlists[al_typedconsts]);
                          new_section(current_asmdata.asmlists[al_typedconsts],sec_rodata_norel,lastlabel.lab.name,const_align(sizeof(pint)));

                          current_asmdata.asmlists[al_typedconsts].concat(Tai_label.Create(lastlabel.lab));
                          { include terminating zero }
                          getmem(pc,len+1);
                          move(value_str^,pc[0],len);
                          pc[len]:=#0;
                          current_asmdata.asmlists[al_typedconsts].concat(Tai_string.Create_pchar(pc,len+1));
                        end;
                      else
                        internalerror(2013120103);
                   end;
                   lab_str:=lastlabel.lab;
                   entry^.Data:=lastlabel.lab;
                end;
           end;
         if cst_type in [cst_ansistring, cst_widestring, cst_unicodestring] then
           begin
             location_reset(location, LOC_REGISTER, OS_ADDR);
             reference_reset_symbol(href, lab_str,
               get_string_symofs(tstringdef(resultdef).stringtype,winlikewidestring),
               const_align(sizeof(pint)));
             location.register:=cg.getaddressregister(current_asmdata.CurrAsmList);
             cg.a_loadaddr_ref_reg(current_asmdata.CurrAsmList,href,location.register);
           end
         else
           begin
             location_reset_ref(location, LOC_CREFERENCE, def_cgsize(resultdef), const_align(sizeof(pint)));
             location.reference.symbol:=lab_str;
           end;
      end;


{*****************************************************************************
                           TCGSETCONSTNODE
*****************************************************************************}

    function tcgsetconstnode.emitvarsetconst: tasmsymbol;
      type
        setbytes=array[0..31] of byte;
        Psetbytes=^setbytes;
      var
        lab: tasmlabel;
        i: longint;
      begin
        current_asmdata.getdatalabel(lab);
        result:=lab;
        lab_set:=lab;
        maybe_new_object_file(current_asmdata.asmlists[al_typedconsts]);
        new_section(current_asmdata.asmlists[al_typedconsts],sec_rodata_norel,result.name,const_align(8));
        current_asmdata.asmlists[al_typedconsts].concat(Tai_label.Create(lab));
        if (source_info.endian=target_info.endian) then
          for i:=0 to 31 do
            current_asmdata.asmlists[al_typedconsts].concat(Tai_const.Create_8bit(Psetbytes(value_set)^[i]))
        else
          for i:=0 to 31 do
            current_asmdata.asmlists[al_typedconsts].concat(Tai_const.Create_8bit(reverse_byte(Psetbytes(value_set)^[i])));
      end;


    procedure tcgsetconstnode.handlevarsetconst;
      var
         entry       : PHashSetItem;
      begin
        location_reset_ref(location,LOC_CREFERENCE,OS_NO,const_align(8));
        { const already used ? }
        if not assigned(lab_set) then
          begin
            entry := current_asmdata.ConstPools[sp_varsets].FindOrAdd(value_set, 32);

             { :-(, we must generate a new entry }
             if not assigned(entry^.Data) then
               entry^.Data:=emitvarsetconst;
             lab_set := TAsmSymbol(entry^.Data);
          end;
        location.reference.symbol:=lab_set;
      end;


    procedure tcgsetconstnode.pass_generate_code;
       type
         setbytes=array[0..31] of byte;
         Psetbytes=^setbytes;

        procedure smallsetconst;
          begin
            location_reset(location,LOC_CONSTANT,int_cgsize(resultdef.size));
            if (source_info.endian=target_info.endian) then
              begin
                { not plongint, because that will "sign extend" the set on 64 bit platforms }
                { if changed to "paword", please also modify "32-resultdef.size*8" and      }
                { cross-endian code below                                                   }
                { Extra aint type cast to avoid range errors                                }
                location.value:=aint(pCardinal(value_set)^)
              end
            else
              begin
                location.value:=swapendian(Pcardinal(value_set)^);
                location.value:=aint(
                                   reverse_byte (location.value         and $ff)         or
                                  (reverse_byte((location.value shr  8) and $ff) shl  8) or
                                  (reverse_byte((location.value shr 16) and $ff) shl 16) or
                                  (reverse_byte((location.value shr 24) and $ff) shl 24)
                                );
              end;
            if (target_info.endian=endian_big) then
              location.value:=location.value shr (32-resultdef.size*8);
          end;

        procedure varsetconst;
          var
             lastlabel   : tasmlabel;
             i           : longint;
             entry       : PHashSetItem;
          begin
            location_reset_ref(location,LOC_CREFERENCE,OS_NO,const_align(8));
            lastlabel:=nil;
            { const already used ? }
            if not assigned(lab_set) then
              begin
                entry := current_asmdata.ConstPools[sp_varsets].FindOrAdd(value_set, 32);

                lab_set := TAsmLabel(entry^.Data);  // is it needed anymore?

                 { :-(, we must generate a new entry }
                 if not assigned(entry^.Data) then
                   begin
                     current_asmdata.getdatalabel(lastlabel);
                     lab_set:=lastlabel;
                     entry^.Data:=lastlabel;
                     maybe_new_object_file(current_asmdata.asmlists[al_typedconsts]);
                     new_section(current_asmdata.asmlists[al_typedconsts],sec_rodata_norel,lastlabel.name,const_align(8));
                     current_asmdata.asmlists[al_typedconsts].concat(Tai_label.Create(lastlabel));
                     if (source_info.endian=target_info.endian) then
                       for i:=0 to 31 do
                         current_asmdata.asmlists[al_typedconsts].concat(Tai_const.Create_8bit(Psetbytes(value_set)^[i]))
                     else
                       for i:=0 to 31 do
                         current_asmdata.asmlists[al_typedconsts].concat(Tai_const.Create_8bit(reverse_byte(Psetbytes(value_set)^[i])));
                   end;
              end;
            location.reference.symbol:=lab_set;
          end;

      begin
        adjustforsetbase;

        { small sets are loaded as constants }
        if is_smallset(resultdef) then
          smallsetconst
        else
          handlevarsetconst;
      end;


{*****************************************************************************
                             TCGNILNODE
*****************************************************************************}

    procedure tcgnilnode.pass_generate_code;
      begin
         location_reset(location,LOC_CONSTANT,OS_ADDR);
         location.value:=0;
      end;


{*****************************************************************************
                          TCGGUIDCONSTNODE
*****************************************************************************}

    procedure tcgguidconstnode.pass_generate_code;
      var
         lastlabel   : tasmlabel;
         i           : longint;
         entry       : PHashSetItem;
      begin
        location_reset_ref(location,LOC_CREFERENCE,OS_NO,const_align(16));
        lastlabel:=nil;
        { const already used ? }
        if not assigned(lab_set) then
          begin
            entry := current_asmdata.ConstPools[sp_guids].FindOrAdd(@value,sizeof(value));
            lab_set := TAsmLabel(entry^.Data);  // is it needed anymore?

             { :-(, we must generate a new entry }
             if not assigned(entry^.Data) then
               begin
                 current_asmdata.getdatalabel(lastlabel);
                 lab_set:=lastlabel;
                 entry^.Data:=lastlabel;
                 maybe_new_object_file(current_asmdata.asmlists[al_typedconsts]);
                 new_section(current_asmdata.asmlists[al_typedconsts],sec_rodata_norel,lastlabel.name,const_align(16));
                 current_asmdata.asmlists[al_typedconsts].concat(Tai_label.Create(lastlabel));
                 current_asmdata.asmlists[al_typedconsts].concat(Tai_const.Create_32bit(longint(value.D1)));
                 current_asmdata.asmlists[al_typedconsts].concat(Tai_const.Create_16bit(value.D2));
                 current_asmdata.asmlists[al_typedconsts].concat(Tai_const.Create_16bit(value.D3));
                 for i:=low(value.D4) to high(value.D4) do
                   current_asmdata.asmlists[al_typedconsts].concat(Tai_const.Create_8bit(value.D4[i]));
               end;
          end;
        location.reference.symbol:=lab_set;
      end;


begin
   cdataconstnode:=tcgdataconstnode;
   crealconstnode:=tcgrealconstnode;
   cordconstnode:=tcgordconstnode;
   cpointerconstnode:=tcgpointerconstnode;
   cstringconstnode:=tcgstringconstnode;
   csetconstnode:=tcgsetconstnode;
   cnilnode:=tcgnilnode;
   cguidconstnode:=tcgguidconstnode;
end.
