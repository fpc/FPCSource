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
       tcgrealconstnode = class(trealconstnode)
          procedure pass_2;override;
       end;

       tcgordconstnode = class(tordconstnode)
          procedure pass_2;override;
       end;

       tcgpointerconstnode = class(tpointerconstnode)
          procedure pass_2;override;
       end;

       tcgstringconstnode = class(tstringconstnode)
          procedure pass_2;override;
       end;

       tcgsetconstnode = class(tsetconstnode)
          procedure pass_2;override;
       end;

       tcgnilnode = class(tnilnode)
          procedure pass_2;override;
       end;

       tcgguidconstnode = class(tguidconstnode)
          procedure pass_2;override;
       end;


implementation

    uses
      globtype,widestr,systems,
      verbose,globals,
      symconst,symdef,aasmbase,aasmtai,aasmcpu,defutil,
      cpuinfo,cpubase,
      cgbase,cgobj,cgutils,
      ncgutil
      ;


{*****************************************************************************
                           TCGREALCONSTNODE
*****************************************************************************}

    procedure tcgrealconstnode.pass_2;
      { I suppose the parser/pass_1 must make sure the generated real  }
      { constants are actually supported by the target processor? (JM) }
      const
        floattype2ait:array[tfloattype] of taitype=
          (ait_real_32bit,ait_real_64bit,ait_real_80bit,ait_comp_64bit,ait_comp_64bit,ait_real_128bit);
      var
         hp1 : tai;
         lastlabel : tasmlabel;
         realait : taitype;
         value_real_sign, hp1_sign: pbyte;
{$ifdef ARM}
         hiloswapped : boolean;
{$endif ARM}

      begin
        location_reset(location,LOC_CREFERENCE,def_cgsize(resulttype.def));
        lastlabel:=nil;
        realait:=floattype2ait[tfloatdef(resulttype.def).typ];
{$ifdef ARM}
        hiloswapped:=aktfputype in [fpu_fpa,fpu_fpa10,fpu_fpa11];
{$endif ARM}
        { const already used ? }
        if not assigned(lab_real) then
          begin
             { tries to find an old entry }
             hp1:=tai(asmlist[al_typedconsts].first);
             while assigned(hp1) do
               begin
                  if hp1.typ=ait_label then
                    lastlabel:=tai_label(hp1).l
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
                  objectlibrary.getdatalabel(lastlabel);
                  lab_real:=lastlabel;
                  maybe_new_object_file(asmlist[al_typedconsts]);
                  new_section(asmlist[al_typedconsts],sec_rodata,lastlabel.name,const_align(resulttype.def.size));
                  asmlist[al_typedconsts].concat(Tai_label.Create(lastlabel));
                  case realait of
                    ait_real_32bit :
                      begin
                        asmlist[al_typedconsts].concat(Tai_real_32bit.Create(ts32real(value_real)));
                        { range checking? }
                        if ((cs_check_range in aktlocalswitches) or
                          (cs_check_overflow in aktlocalswitches)) and
                          (tai_real_32bit(asmlist[al_typedconsts].last).value=double(MathInf)) then
                          Message(parser_e_range_check_error);
                      end;

                    ait_real_64bit :
                      begin
{$ifdef ARM}
                        if hiloswapped then
                          asmlist[al_typedconsts].concat(Tai_real_64bit.Create_hiloswapped(ts64real(value_real)))
                        else
{$endif ARM}
                          asmlist[al_typedconsts].concat(Tai_real_64bit.Create(ts64real(value_real)));

                        { range checking? }
                        if ((cs_check_range in aktlocalswitches) or
                          (cs_check_overflow in aktlocalswitches)) and
                          (tai_real_64bit(asmlist[al_typedconsts].last).value=double(MathInf)) then
                          Message(parser_e_range_check_error);
                     end;

                    ait_real_80bit :
                      begin
                        asmlist[al_typedconsts].concat(Tai_real_80bit.Create(value_real));

                        { range checking? }
                        if ((cs_check_range in aktlocalswitches) or
                          (cs_check_overflow in aktlocalswitches)) and
                          (tai_real_80bit(asmlist[al_typedconsts].last).value=double(MathInf)) then
                          Message(parser_e_range_check_error);
                      end;
{$ifdef cpufloat128}
                    ait_real_128bit :
                      begin
                        asmlist[al_typedconsts].concat(Tai_real_128bit.Create(value_real));

                        { range checking? }
                        if ((cs_check_range in aktlocalswitches) or
                          (cs_check_overflow in aktlocalswitches)) and
                          (tai_real_128bit(asmlist[al_typedconsts].last).value=double(MathInf)) then
                          Message(parser_e_range_check_error);
                      end;
{$endif cpufloat128}

                    { the round is necessary for native compilers where comp isn't a float }
                    ait_comp_64bit :
                      if (value_real>9223372036854775807.0) or (value_real<-9223372036854775808.0) then
                        message(parser_e_range_check_error)
                      else
                        asmlist[al_typedconsts].concat(Tai_comp_64bit.Create(round(value_real)));
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

    procedure tcgordconstnode.pass_2;
      begin
         location_reset(location,LOC_CONSTANT,def_cgsize(resulttype.def));
{$ifdef cpu64bit}
         location.value:=value;
{$else cpu64bit}
         location.value64:=int64(value);
{$endif cpu64bit}
      end;


{*****************************************************************************
                          TCGPOINTERCONSTNODE
*****************************************************************************}

    procedure tcgpointerconstnode.pass_2;
      begin
         { an integer const. behaves as a memory reference }
         location_reset(location,LOC_CONSTANT,OS_ADDR);
         location.value:=aint(value);
      end;


{*****************************************************************************
                          TCGSTRINGCONSTNODE
*****************************************************************************}

    procedure tcgstringconstnode.pass_2;
      var
         hp1,hp2 : tai;
         l1,l2,
         lastlabel   : tasmlabel;
         lastlabelhp : tai;
         pc       : pchar;
         same_string : boolean;
         l,j,
         i,mylength  : longint;
      begin
         { for empty ansistrings we could return a constant 0 }
         if (st_type in [st_ansistring,st_widestring]) and (len=0) then
          begin
            location_reset(location,LOC_CONSTANT,OS_ADDR);
            location.value:=0;
            exit;
          end;
         { return a constant reference in memory }
         location_reset(location,LOC_CREFERENCE,def_cgsize(resulttype.def));
         { const already used ? }
         lastlabel:=nil;
         lastlabelhp:=nil;
         if not assigned(lab_str) then
           begin
              if is_shortstring(resulttype.def) then
                mylength:=len+2
              else
                mylength:=len+1;
              { widestrings can't be reused yet }
              if not(is_widestring(resulttype.def)) then
                begin
                  { tries to find an old entry }
                  hp1:=tai(asmlist[al_typedconsts].first);
                  while assigned(hp1) do
                    begin
                       if hp1.typ=ait_label then
                         begin
                           lastlabel:=tai_label(hp1).l;
                           lastlabelhp:=hp1;
                         end
                       else
                         begin
                            same_string:=false;
                            if (hp1.typ=ait_string) and
                               (lastlabel<>nil) and
                               (tai_string(hp1).len=mylength) then
                              begin
                                 case st_type of
                                   st_conststring:
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
                                   st_shortstring:
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
                                   st_ansistring,
                                   st_widestring :
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
                                          (hp2.typ=ait_const_aint) and
                                          (tai_const(hp2).value=-1) and
                                          assigned(hp2.previous) and
                                          (tai(hp2.previous).typ=ait_const_aint) and
                                          (tai_const(hp2.previous).value=len) and
                                          assigned(hp2.previous.previous) and
                                          (tai(hp2.previous.previous).typ=ait_const_ptr) and
                                          assigned(hp2.previous.previous.previous) and
                                          (tai(hp2.previous.previous.previous).typ=ait_label) then
                                         begin
                                           lastlabel:=tai_label(hp2.previous.previous.previous).l;
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
                   objectlibrary.getdatalabel(lastlabel);
                   lab_str:=lastlabel;
                   maybe_new_object_file(asmlist[al_typedconsts]);
                   new_section(asmlist[al_typedconsts],sec_rodata,lastlabel.name,const_align(sizeof(aint)));
                   asmlist[al_typedconsts].concat(Tai_label.Create(lastlabel));
                   { generate an ansi string ? }
                   case st_type of
                      st_ansistring:
                        begin
                           { an empty ansi string is nil! }
                           if len=0 then
                             asmlist[al_typedconsts].concat(Tai_const.Create_sym(nil))
                           else
                             begin
                                objectlibrary.getdatalabel(l1);
                                objectlibrary.getdatalabel(l2);
                                asmlist[al_typedconsts].concat(Tai_label.Create(l2));
                                asmlist[al_typedconsts].concat(Tai_const.Create_sym(l1));
                                asmlist[al_typedconsts].concat(Tai_const.Create_aint(-1));
                                asmlist[al_typedconsts].concat(Tai_const.Create_aint(len));
                                asmlist[al_typedconsts].concat(Tai_label.Create(l1));
                                { include also terminating zero }
                                getmem(pc,len+1);
                                move(value_str^,pc^,len);
                                pc[len]:=#0;
                                asmlist[al_typedconsts].concat(Tai_string.Create_pchar(pc,len+1));
                                { return the offset of the real string }
                                lab_str:=l2;
                             end;
                        end;
                      st_widestring:
                        begin
                           { an empty wide string is nil! }
                           if len=0 then
                             asmlist[al_typedconsts].concat(Tai_const.Create_sym(nil))
                           else
                             begin
                                objectlibrary.getdatalabel(l1);
                                objectlibrary.getdatalabel(l2);
                                asmlist[al_typedconsts].concat(Tai_label.Create(l2));
                                asmlist[al_typedconsts].concat(Tai_const.Create_sym(l1));

                                { we use always UTF-16 coding for constants }
                                { at least for now                          }
                                { Consts.concat(Tai_const.Create_8bit(2)); }
                                asmlist[al_typedconsts].concat(Tai_const.Create_aint(-1));
                                asmlist[al_typedconsts].concat(Tai_const.Create_aint(len*cwidechartype.def.size));
                                asmlist[al_typedconsts].concat(Tai_label.Create(l1));
                                for i:=0 to len-1 do
                                  asmlist[al_typedconsts].concat(Tai_const.Create_16bit(pcompilerwidestring(value_str)^.data[i]));
                                { terminating zero }
                                asmlist[al_typedconsts].concat(Tai_const.Create_16bit(0));
                                { return the offset of the real string }
                                lab_str:=l2;
                             end;
                        end;
                      st_shortstring:
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
                          asmlist[al_typedconsts].concat(Tai_string.Create_pchar(pc,l+2));
                        end;
                      st_conststring:
                        begin
                          { include terminating zero }
                          getmem(pc,len+1);
                          move(value_str^,pc[0],len);
                          pc[len]:=#0;
                          asmlist[al_typedconsts].concat(Tai_string.Create_pchar(pc,len+1));
                        end;
                   end;
                end;
           end;
         location.reference.symbol:=lab_str;
      end;


{*****************************************************************************
                           TCGSETCONSTNODE
*****************************************************************************}

    procedure tcgsetconstnode.pass_2;
      var
         hp1         : tai;
         lastlabel   : tasmlabel;
         i           : longint;
         neededtyp   : taitype;
         indexadjust : longint;
      type
         setbytes=array[0..31] of byte;
         Psetbytes=^setbytes;
      begin
        { xor indexadjust with indexes in a set typecasted to an array of   }
        { bytes to get the correct locations, also when endianess of source }
        { and destiantion differs (JM)                                      }
        if (source_info.endian = target_info.endian) then
          indexadjust := 0
        else
          indexadjust := 3;
        { small sets are loaded as constants }
        if tsetdef(resulttype.def).settype=smallset then
         begin
           location_reset(location,LOC_CONSTANT,OS_32);
           location.value:=pLongint(value_set)^;
           exit;
         end;
        location_reset(location,LOC_CREFERENCE,OS_NO);
        neededtyp:=ait_const_8bit;
        lastlabel:=nil;
        { const already used ? }
        if not assigned(lab_set) then
          begin
             { tries to found an old entry }
             hp1:=tai(asmlist[al_typedconsts].first);
             while assigned(hp1) do
               begin
                  if hp1.typ=ait_label then
                    lastlabel:=tai_label(hp1).l
                  else
                    begin
                      if (lastlabel<>nil) and (hp1.typ=neededtyp) then
                        begin
                          if (hp1.typ=ait_const_8bit) then
                           begin
                             { compare normal set }
                             i:=0;
                             while assigned(hp1) and (i<32) do
                              begin
                                if tai_const(hp1).value<>Psetbytes(value_set)^[i xor indexadjust] then
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
                 objectlibrary.getdatalabel(lastlabel);
                 lab_set:=lastlabel;
                 maybe_new_object_file(asmlist[al_typedconsts]);
                 new_section(asmlist[al_typedconsts],sec_rodata,lastlabel.name,const_align(sizeof(aint)));
                 asmlist[al_typedconsts].concat(Tai_label.Create(lastlabel));
                 { already handled at the start of this method?? (JM)
                 if tsetdef(resulttype.def).settype=smallset then
                  begin
                    move(value_set^,i,sizeof(longint));
                    Consts.concat(Tai_const.Create_32bit(i));
                  end
                 else
                 }
                  begin
                    for i:=0 to 31 do
                      asmlist[al_typedconsts].concat(Tai_const.Create_8bit(Psetbytes(value_set)^[i xor indexadjust]));
                  end;
               end;
          end;
        location.reference.symbol:=lab_set;
      end;


{*****************************************************************************
                             TCGNILNODE
*****************************************************************************}

    procedure tcgnilnode.pass_2;
      begin
         location_reset(location,LOC_CONSTANT,OS_ADDR);
         location.value:=0;
      end;


{*****************************************************************************
                          TCGPOINTERCONSTNODE
*****************************************************************************}

    procedure tcgguidconstnode.pass_2;
      var
        tmplabel : TAsmLabel;
        i : integer;
      begin
        location_reset(location,LOC_CREFERENCE,OS_NO);
        { label for GUID }
        objectlibrary.getdatalabel(tmplabel);
        asmlist[al_typedconsts].concat(tai_align.create(const_align(16)));
        asmlist[al_typedconsts].concat(Tai_label.Create(tmplabel));
        asmlist[al_typedconsts].concat(Tai_const.Create_32bit(longint(value.D1)));
        asmlist[al_typedconsts].concat(Tai_const.Create_16bit(value.D2));
        asmlist[al_typedconsts].concat(Tai_const.Create_16bit(value.D3));
        for i:=low(value.D4) to high(value.D4) do
          asmlist[al_typedconsts].concat(Tai_const.Create_8bit(value.D4[i]));
        location.reference.symbol:=tmplabel;
      end;


begin
   crealconstnode:=tcgrealconstnode;
   cordconstnode:=tcgordconstnode;
   cpointerconstnode:=tcgpointerconstnode;
   cstringconstnode:=tcgstringconstnode;
   csetconstnode:=tcgsetconstnode;
   cnilnode:=tcgnilnode;
   cguidconstnode:=tcgguidconstnode;
end.
