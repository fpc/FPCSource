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
      symconst,symdef,aasmbase,aasmtai,aasmdata,aasmcpu,defutil,
      cpuinfo,cpubase,
      cgbase,cgobj,cgutils,
      ncgutil
      ;


{*****************************************************************************
                           TCGREALCONSTNODE
*****************************************************************************}

    procedure tcgdataconstnode.pass_generate_code;
      var
        l : tasmlabel;
        i : longint;
        b : byte;
      begin
        location_reset(location,LOC_CREFERENCE,OS_NO);
        current_asmdata.getdatalabel(l);
        maybe_new_object_file(current_asmdata.asmlists[al_typedconsts]);
        new_section(current_asmdata.asmlists[al_typedconsts],sec_rodata,l.name,const_align(maxalign));
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
          (ait_real_32bit,ait_real_64bit,ait_real_80bit,ait_comp_64bit,ait_comp_64bit,ait_real_128bit);
      var
         hp1 : tai;
         lastlabel : tasmlabel;
         realait : taitype;
{$ifdef ARM}
         hiloswapped : boolean;
{$endif ARM}

      begin
        location_reset(location,LOC_CREFERENCE,def_cgsize(resultdef));
        lastlabel:=nil;
        realait:=floattype2ait[tfloatdef(resultdef).floattype];
{$ifdef ARM}
        hiloswapped:=is_double_hilo_swapped;
{$endif ARM}
        { const already used ? }
        if not assigned(lab_real) then
          begin
             { tries to find an old entry }
             hp1:=tai(current_asmdata.asmlists[al_typedconsts].first);
             while assigned(hp1) do
               begin
                  if hp1.typ=ait_label then
                    lastlabel:=tai_label(hp1).labsym
                  else
                    begin
                       if (hp1.typ=realait) and (lastlabel<>nil) then
                         begin
                            if is_number_float(value_real) and
                              (
                               ((realait=ait_real_32bit) and (tai_real_32bit(hp1).value=value_real) and is_number_float(tai_real_32bit(hp1).value) and (get_real_sign(value_real) = get_real_sign(tai_real_32bit(hp1).value))) or
                               ((realait=ait_real_64bit) and
{$ifdef ARM}
                                 ((tai_real_64bit(hp1).formatoptions=fo_hiloswapped)=hiloswapped) and
{$endif ARM}
                                 (tai_real_64bit(hp1).value=value_real) and is_number_float(tai_real_64bit(hp1).value) and (get_real_sign(value_real) = get_real_sign(tai_real_64bit(hp1).value))) or
                               ((realait=ait_real_80bit) and (tai_real_80bit(hp1).value=value_real) and is_number_float(tai_real_80bit(hp1).value) and (get_real_sign(value_real) = get_real_sign(tai_real_80bit(hp1).value))) or
{$ifdef cpufloat128}
                               ((realait=ait_real_128bit) and (tai_real_128bit(hp1).value=value_real) and is_number_float(tai_real_128bit(hp1).value) and (get_real_sign(value_real) = get_real_sign(tai_real_128bit(hp1).value))) or
{$endif cpufloat128}
                               ((realait=ait_comp_64bit) and (tai_comp_64bit(hp1).value=value_real) and is_number_float(tai_comp_64bit(hp1).value) and (get_real_sign(value_real) = get_real_sign(tai_comp_64bit(hp1).value)))
                              ) then
                              begin
                                { found! }
                                lab_real:=lastlabel;
                                break;
                              end;
                         end;
                       lastlabel:=nil;
                    end;
                  hp1:=tai(hp1.next);
               end;
             { :-(, we must generate a new entry }
             if not assigned(lab_real) then
               begin
                  current_asmdata.getdatalabel(lastlabel);
                  lab_real:=lastlabel;
                  maybe_new_object_file(current_asmdata.asmlists[al_typedconsts]);
                  new_section(current_asmdata.asmlists[al_typedconsts],sec_rodata_norel,lastlabel.name,const_align(resultdef.size));
                  current_asmdata.asmlists[al_typedconsts].concat(Tai_label.Create(lastlabel));
                  case realait of
                    ait_real_32bit :
                      begin
                        current_asmdata.asmlists[al_typedconsts].concat(Tai_real_32bit.Create(ts32real(value_real)));
                        { range checking? }
                        if ((cs_check_range in current_settings.localswitches) or
                          (cs_check_overflow in current_settings.localswitches)) and
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
                        if ((cs_check_range in current_settings.localswitches) or
                          (cs_check_overflow in current_settings.localswitches)) and
                          (tai_real_64bit(current_asmdata.asmlists[al_typedconsts].last).value=MathInf.Value) then
                          Message(parser_e_range_check_error);
                     end;

                    ait_real_80bit :
                      begin
                        current_asmdata.asmlists[al_typedconsts].concat(Tai_real_80bit.Create(value_real));

                        { range checking? }
                        if ((cs_check_range in current_settings.localswitches) or
                          (cs_check_overflow in current_settings.localswitches)) and
                          (tai_real_80bit(current_asmdata.asmlists[al_typedconsts].last).value=MathInf.Value) then
                          Message(parser_e_range_check_error);
                      end;
{$ifdef cpufloat128}
                    ait_real_128bit :
                      begin
                        current_asmdata.asmlists[al_typedconsts].concat(Tai_real_128bit.Create(value_real));

                        { range checking? }
                        if ((cs_check_range in current_settings.localswitches) or
                          (cs_check_overflow in current_settings.localswitches)) and
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
         hp1,hp2 : tai;
         l1,
         lastlabel   : tasmlabel;
         lastlabelhp : tai;
         pc       : pchar;
         same_string : boolean;
         l,j,
         i,mylength  : longint;
      begin
         { for empty ansistrings we could return a constant 0 }
         if (cst_type in [cst_ansistring,cst_widestring]) and (len=0) then
          begin
            location_reset(location,LOC_CONSTANT,OS_ADDR);
            location.value:=0;
            exit;
          end;
         { return a constant reference in memory }
         location_reset(location,LOC_CREFERENCE,def_cgsize(resultdef));
         { const already used ? }
         lastlabel:=nil;
         lastlabelhp:=nil;
         if not assigned(lab_str) then
           begin
              if is_shortstring(resultdef) then
                mylength:=len+2
              else
                mylength:=len+1;
              { widestrings can't be reused yet }
              if not(is_widestring(resultdef)) then
                begin
                  { tries to find an old entry }
                  hp1:=tai(current_asmdata.asmlists[al_typedconsts].first);
                  while assigned(hp1) do
                    begin
                       if hp1.typ=ait_label then
                         begin
                           lastlabel:=tai_label(hp1).labsym;
                           lastlabelhp:=hp1;
                         end
                       else
                         begin
                            same_string:=false;
                            if (hp1.typ=ait_string) and
                               (lastlabel<>nil) and
                               (tai_string(hp1).len=mylength) then
                              begin
                                 case cst_type of
                                   cst_conststring:
                                     begin
                                       j:=0;
                                       same_string:=true;
                                       if len>0 then
                                         begin
                                           for i:=0 to len-1 do
                                             begin
                                               if tai_string(hp1).str[j]<>value_str[i] then
                                                 begin
                                                   same_string:=false;
                                                   break;
                                                 end;
                                               inc(j);
                                             end;
                                         end;
                                     end;
                                   cst_shortstring:
                                     begin
                                       { if shortstring then check the length byte first and
                                         set the start index to 1 }
                                       if len=ord(tai_string(hp1).str[0]) then
                                         begin
                                           j:=1;
                                           same_string:=true;
                                           if len>0 then
                                             begin
                                               for i:=0 to len-1 do
                                                begin
                                                  if tai_string(hp1).str[j]<>value_str[i] then
                                                   begin
                                                     same_string:=false;
                                                     break;
                                                   end;
                                                  inc(j);
                                                end;
                                             end;
                                         end;
                                     end;
                                   cst_ansistring,
                                   cst_widestring :
                                     begin
                                       { before the string the following sequence must be found:
                                         <label>
                                           constsymbol <datalabel>
                                           constint -1
                                           constint <len>
                                         we must then return <label> to reuse
                                       }
                                       hp2:=tai(lastlabelhp.previous);
                                       if assigned(hp2) and
                                          (hp2.typ=ait_const) and
                                          (tai_const(hp2).consttype=aitconst_aint) and
                                          (tai_const(hp2).value=len) and
                                          assigned(hp2.previous) and
                                          (tai(hp2.previous).typ=ait_const) and
                                          (tai_const(hp2.previous).consttype=aitconst_aint) and
                                          (tai_const(hp2.previous).value=-1) and
                                          assigned(hp2.previous.previous) and
                                          (tai(hp2.previous.previous).typ=ait_const) and
                                          (tai_const(hp2.previous.previous).consttype=aitconst_ptr) and
                                          assigned(hp2.previous.previous.previous) and
                                          (tai(hp2.previous.previous.previous).typ=ait_label) then
                                         begin
                                           lastlabel:=tai_label(hp2.previous.previous.previous).labsym;
                                           same_string:=true;
                                           j:=0;
                                           if len>0 then
                                             begin
                                               for i:=0 to len-1 do
                                                begin
                                                  if tai_string(hp1).str[j]<>value_str[i] then
                                                   begin
                                                     same_string:=false;
                                                     break;
                                                   end;
                                                  inc(j);
                                                end;
                                             end;
                                         end;
                                     end;
                                 end;
                                 { found ? }
                                 if same_string then
                                   begin
                                     lab_str:=lastlabel;
                                     break;
                                   end;
                              end;
                            lastlabel:=nil;
                         end;
                       hp1:=tai(hp1.next);
                    end;
                end;
              { :-(, we must generate a new entry }
              if not assigned(lab_str) then
                begin
                   current_asmdata.getdatalabel(lastlabel);
                   lab_str:=lastlabel;
                   maybe_new_object_file(current_asmdata.asmlists[al_typedconsts]);
                   if (len=0) or
                      not(cst_type in [cst_ansistring,cst_widestring]) then
                     new_section(current_asmdata.asmlists[al_typedconsts],sec_rodata_norel,lastlabel.name,const_align(sizeof(pint)))
                   else
                     new_section(current_asmdata.asmlists[al_typedconsts],sec_rodata,lastlabel.name,const_align(sizeof(pint)));
                   current_asmdata.asmlists[al_typedconsts].concat(Tai_label.Create(lastlabel));
                   { generate an ansi string ? }
                   case cst_type of
                      cst_ansistring:
                        begin
                           { an empty ansi string is nil! }
                           if len=0 then
                             current_asmdata.asmlists[al_typedconsts].concat(Tai_const.Create_sym(nil))
                           else
                             begin
                                current_asmdata.getdatalabel(l1);
                                current_asmdata.asmlists[al_typedconsts].concat(Tai_const.Create_sym(l1));
                                current_asmdata.asmlists[al_typedconsts].concat(Tai_const.Create_pint(-1));
                                current_asmdata.asmlists[al_typedconsts].concat(Tai_const.Create_pint(len));
                                { make sure the string doesn't get dead stripped if the header is referenced }
                                if (target_info.system in systems_darwin) then
                                  current_asmdata.asmlists[al_typedconsts].concat(tai_directive.create(asd_reference,l1.name));
                                current_asmdata.asmlists[al_typedconsts].concat(Tai_label.Create(l1));
                                { ... and vice versa }
                                if (target_info.system in systems_darwin) then
                                  current_asmdata.asmlists[al_typedconsts].concat(tai_directive.create(asd_reference,lab_str.name));
                                { include also terminating zero }
                                getmem(pc,len+1);
                                move(value_str^,pc^,len);
                                pc[len]:=#0;
                                current_asmdata.asmlists[al_typedconsts].concat(Tai_string.Create_pchar(pc,len+1));
                             end;
                        end;
                      cst_widestring:
                        begin
                           { an empty wide string is nil! }
                           if len=0 then
                             current_asmdata.asmlists[al_typedconsts].concat(Tai_const.Create_sym(nil))
                           else
                             begin
                                current_asmdata.getdatalabel(l1);
                                current_asmdata.asmlists[al_typedconsts].concat(Tai_const.Create_sym(l1));

                                { we use always UTF-16 coding for constants }
                                { at least for now                          }
                                { Consts.concat(Tai_const.Create_8bit(2)); }
                                if tf_winlikewidestring in target_info.flags then
                                  current_asmdata.asmlists[al_typedconsts].concat(Tai_const.Create_32bit(len*cwidechartype.size))
                                else
                                  begin
                                    current_asmdata.asmlists[al_typedconsts].concat(Tai_const.Create_pint(-1));
                                    current_asmdata.asmlists[al_typedconsts].concat(Tai_const.Create_pint(len*cwidechartype.size));
                                  end;
                                { make sure the string doesn't get dead stripped if the header is referenced }
                                if (target_info.system in systems_darwin) then
                                  current_asmdata.asmlists[al_typedconsts].concat(tai_directive.create(asd_reference,l1.name));
                                current_asmdata.asmlists[al_typedconsts].concat(Tai_label.Create(l1));
                                { ... and vice versa }
                                if (target_info.system in systems_darwin) then
                                  current_asmdata.asmlists[al_typedconsts].concat(tai_directive.create(asd_reference,lab_str.name));
                                for i:=0 to len-1 do
                                  current_asmdata.asmlists[al_typedconsts].concat(Tai_const.Create_16bit(pcompilerwidestring(value_str)^.data[i]));
                                { terminating zero }
                                current_asmdata.asmlists[al_typedconsts].concat(Tai_const.Create_16bit(0));
                             end;
                        end;
                      cst_shortstring:
                        begin
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
                          { include terminating zero }
                          getmem(pc,len+1);
                          move(value_str^,pc[0],len);
                          pc[len]:=#0;
                          current_asmdata.asmlists[al_typedconsts].concat(Tai_string.Create_pchar(pc,len+1));
                        end;
                   end;
                end;
           end;
         location.reference.symbol:=lab_str;
      end;


{*****************************************************************************
                           TCGSETCONSTNODE
*****************************************************************************}

    procedure tcgsetconstnode.pass_generate_code;
      var
         hp1         : tai;
         lastlabel   : tasmlabel;
         i, diff     : longint;
         neededtyp   : taiconst_type;
      type
         setbytes=array[0..31] of byte;
         Psetbytes=^setbytes;
      begin
        adjustforsetbase;

        { small sets are loaded as constants }
        if not(is_varset(resultdef)) then
         begin
           location_reset(location,LOC_CONSTANT,int_cgsize(resultdef.size));
           if (source_info.endian=target_info.endian) then
             begin
{$if defined(FPC_NEW_BIGENDIAN_SETS) or defined(FPC_LITTLE_ENDIAN)}
               { not plongint, because that will "sign extend" the set on 64 bit platforms }
               { if changed to "paword", please also modify "32-resultdef.size*8" and      }
               { cross-endian code below                                                   }
               { Extra aint type cast to avoid range errors                                }
               location.value:=aint(pCardinal(value_set)^)
{$else}
               location.value:=reverse_byte(Psetbytes(value_set)^[0]);
               location.value:=location.value or (reverse_byte(Psetbytes(value_set)^[1]) shl 8);
               location.value:=location.value or (reverse_byte(Psetbytes(value_set)^[2]) shl 16);
               location.value:=location.value or (reverse_byte(Psetbytes(value_set)^[3]) shl 24);
{$endif}
             end
           else
             begin
               location.value:=swapendian(Pcardinal(value_set)^);
               location.value:= reverse_byte (location.value         and $ff)         or
                               (reverse_byte((location.value shr  8) and $ff) shl  8) or
                               (reverse_byte((location.value shr 16) and $ff) shl 16) or
                               (reverse_byte((location.value shr 24) and $ff) shl 24);
             end;
           if (target_info.endian=endian_big) then
             location.value:=location.value shr (32-resultdef.size*8);
           exit;
         end;
        location_reset(location,LOC_CREFERENCE,OS_NO);
        neededtyp:=aitconst_8bit;
        lastlabel:=nil;
        { const already used ? }
        if not assigned(lab_set) then
          begin
             { tries to found an old entry }
             hp1:=tai(current_asmdata.asmlists[al_typedconsts].first);
             while assigned(hp1) do
               begin
                  if hp1.typ=ait_label then
                    lastlabel:=tai_label(hp1).labsym
                  else
                    begin
                      if (lastlabel<>nil) and
                        (hp1.typ=ait_const) and
                        (tai_const(hp1).consttype=neededtyp) then
                        begin
                          if (tai_const(hp1).consttype=aitconst_8bit) then
                           begin
                             { compare normal set }
                             i:=0;
                             while assigned(hp1) and (i<32) do
                              begin
                                if (source_info.endian=target_info.endian) then
                                  begin
{$if defined(FPC_NEW_BIGENDIAN_SETS) or defined(FPC_LITTLE_ENDIAN)}
                                    if tai_const(hp1).value<>Psetbytes(value_set)^[i ] then
{$else}
                                    if tai_const(hp1).value<>reverse_byte(Psetbytes(value_set)^[i xor 3]) then
{$endif}
                                      break
                                  end
                                else if tai_const(hp1).value<>reverse_byte(Psetbytes(value_set)^[i]) then
                                  break;
                                inc(i);
                                hp1:=tai(hp1.next);
                              end;
                             if i=32 then
                              begin
                                { found! }
                                lab_set:=lastlabel;
                                break;
                              end;
                             { leave when the end of consts is reached, so no
                               hp1.next is done }
                             if not assigned(hp1) then
                              break;
                           end
                          else
                           begin
                             { compare small set }
                             if paint(value_set)^=tai_const(hp1).value then
                              begin
                                { found! }
                                lab_set:=lastlabel;
                                break;
                              end;
                           end;
                        end;
                      lastlabel:=nil;
                    end;
                  hp1:=tai(hp1.next);
               end;
             { :-(, we must generate a new entry }
             if not assigned(lab_set) then
               begin
                 current_asmdata.getdatalabel(lastlabel);
                 lab_set:=lastlabel;
                 maybe_new_object_file(current_asmdata.asmlists[al_typedconsts]);
                 new_section(current_asmdata.asmlists[al_typedconsts],sec_rodata_norel,lastlabel.name,const_align(sizeof(pint)));
                 current_asmdata.asmlists[al_typedconsts].concat(Tai_label.Create(lastlabel));
                 { already handled at the start of this method?? (JM)
                 if tsetdef(resultdef).settype=smallset then
                  begin
                    move(value_set^,i,sizeof(longint));
                    Consts.concat(Tai_const.Create_32bit(i));
                  end
                 else
                 }
                  begin
                    if (source_info.endian=target_info.endian) then
{$if defined(FPC_NEW_BIGENDIAN_SETS) or defined(FPC_LITTLE_ENDIAN)}
                      for i:=0 to 31 do
                        current_asmdata.asmlists[al_typedconsts].concat(Tai_const.Create_8bit(Psetbytes(value_set)^[i]))
{$else}
                      for i:=0 to 31 do
                        current_asmdata.asmlists[al_typedconsts].concat(Tai_const.Create_8bit(reverse_byte(Psetbytes(value_set)^[i xor 3])))
{$endif}
                    else
                      for i:=0 to 31 do
                        current_asmdata.asmlists[al_typedconsts].concat(Tai_const.Create_8bit(reverse_byte(Psetbytes(value_set)^[i])));
                  end;
               end;
          end;
        location.reference.symbol:=lab_set;
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
                          TCGPOINTERCONSTNODE
*****************************************************************************}

    procedure tcgguidconstnode.pass_generate_code;
      var
        tmplabel : TAsmLabel;
        i : integer;
      begin
        location_reset(location,LOC_CREFERENCE,OS_NO);
        { label for GUID }
        current_asmdata.getdatalabel(tmplabel);
        current_asmdata.asmlists[al_typedconsts].concat(tai_align.create(const_align(16)));
        current_asmdata.asmlists[al_typedconsts].concat(Tai_label.Create(tmplabel));
        current_asmdata.asmlists[al_typedconsts].concat(Tai_const.Create_32bit(longint(value.D1)));
        current_asmdata.asmlists[al_typedconsts].concat(Tai_const.Create_16bit(value.D2));
        current_asmdata.asmlists[al_typedconsts].concat(Tai_const.Create_16bit(value.D3));
        for i:=low(value.D4) to high(value.D4) do
          current_asmdata.asmlists[al_typedconsts].concat(Tai_const.Create_8bit(value.D4[i]));
        location.reference.symbol:=tmplabel;
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
