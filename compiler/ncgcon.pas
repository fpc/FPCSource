{
    $Id$
    Copyright (c) 1998-2000 by Florian Klaempfl

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

{$i defines.inc}

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
      symconst,symdef,aasm,types,
      cpubase,
      tgobj;


{*****************************************************************************
                           TCGREALCONSTNODE
*****************************************************************************}

    procedure tcgrealconstnode.pass_2;
      { I suppose the parser/pass_1 must make sure the generated real  }
      { constants are actually supported by the target processor? (JM) }

      const
        floattype2ait:array[tfloattype] of tait=
          (ait_real_32bit,ait_real_64bit,ait_real_80bit,ait_comp_64bit);

      var
         hp1 : tai;
         lastlabel : tasmlabel;
         realait : tait;

      begin
        lastlabel:=nil;
        realait:=floattype2ait[tfloatdef(resulttype.def).typ];
        { const already used ? }
        if not assigned(lab_real) then
          begin
             { tries to find an old entry }
             hp1:=tai(Consts.first);
             while assigned(hp1) do
               begin
                  if hp1.typ=ait_label then
                    lastlabel:=tai_label(hp1).l
                  else
                    begin
                       if (hp1.typ=realait) and (lastlabel<>nil) then
                         begin
                            if(
                               ((realait=ait_real_32bit) and (tai_real_32bit(hp1).value=value_real)) or
                               ((realait=ait_real_64bit) and (tai_real_64bit(hp1).value=value_real)) or
                               ((realait=ait_real_80bit) and (tai_real_80bit(hp1).value=value_real)) or
                               ((realait=ait_comp_64bit) and (tai_comp_64bit(hp1).value=value_real))
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
                  getdatalabel(lastlabel);
                  lab_real:=lastlabel;
                  if (cs_create_smart in aktmoduleswitches) then
                   Consts.concat(Tai_cut.Create);
                  Consts.concat(Tai_label.Create(lastlabel));
                  case realait of
                    ait_real_32bit :
                      Consts.concat(Tai_real_32bit.Create(value_real));
                    ait_real_64bit :
                      Consts.concat(Tai_real_64bit.Create(value_real));
                    ait_real_80bit :
                      Consts.concat(Tai_real_80bit.Create(value_real));
                    ait_comp_64bit :
                      Consts.concat(Tai_comp_64bit.Create(value_real));
                  else
                    internalerror(10120);
                  end;
               end;
          end;
        reset_reference(location.reference);
        location.reference.symbol:=lab_real;
        location.loc:=LOC_MEM;
      end;

{*****************************************************************************
                            TCGORDCONSTNODE
*****************************************************************************}

    procedure tcgordconstnode.pass_2;
      var
         l : tasmlabel;

      begin
         location.loc:=LOC_MEM;
         { still needs to be made more generic (and optimal), this is for }
         { when Peter implements LOC_ORDCONST (JM)                        }
         if is_64bitint(resulttype.def) then
           begin
              getdatalabel(l);
              if (cs_create_smart in aktmoduleswitches) then
                Consts.concat(Tai_cut.Create);
              Consts.concat(Tai_label.Create(l));
              Consts.concat(Tai_const.Create_32bit(longint(value)));
              Consts.concat(Tai_const.Create_32bit(longint(value shr 32)));
              reset_reference(location.reference);
              location.reference.symbol:=l;
           end
         else
           begin
              { non int64 const. behaves as a memory reference }
              location.reference.is_immediate:=true;
              location.reference.offset:=longint(value);
           end;
      end;


{*****************************************************************************
                          TCGPOINTERCONSTNODE
*****************************************************************************}

    procedure tcgpointerconstnode.pass_2;
      begin
         { an integer const. behaves as a memory reference }
         location.loc:=LOC_MEM;
         location.reference.is_immediate:=true;
         location.reference.offset:=longint(value);
      end;


{*****************************************************************************
                          TCGSTRINGCONSTNODE
*****************************************************************************}

    procedure tcgstringconstnode.pass_2;
      var
         hp1 : tai;
         l1,l2,
         lastlabel   : tasmlabel;
         pc       : pchar;
         same_string : boolean;
         l,j,
         i,mylength  : longint;
      begin
         { for empty ansistrings we could return a constant 0 }
         if (is_ansistring(resulttype.def) or
             is_widestring(resulttype.def)) and
            (len=0) then
          begin
            location.loc:=LOC_MEM;
            location.reference.is_immediate:=true;
            location.reference.offset:=0;
            exit;
          end;
         { const already used ? }
         lastlabel:=nil;
         if not assigned(lab_str) then
           begin
              if is_shortstring(resulttype.def) then
                mylength:=len+2
              else
                mylength:=len+1;
              { widestrings can't be reused yet }
              if not(is_widestring(resulttype.def)) then
                begin
                  { tries to found an old entry }
                  hp1:=tai(Consts.first);
                  while assigned(hp1) do
                    begin
                       if hp1.typ=ait_label then
                         lastlabel:=tai_label(hp1).l
                       else
                         begin
                            { when changing that code, be careful that }
                            { you don't use typed consts, which are    }
                            { are also written to consts           }
                            { currently, this is no problem, because   }
                            { typed consts have no leading length or   }
                            { they have no trailing zero           }
                            if (hp1.typ=ait_string) and (lastlabel<>nil) and
                               (tai_string(hp1).len=mylength) then
                              begin
                                 same_string:=true;
                                 { if shortstring then check the length byte first and
                                   set the start index to 1 }
                                 if is_shortstring(resulttype.def) then
                                  begin
                                    if len<>ord(tai_string(hp1).str[0]) then
                                     same_string:=false;
                                    j:=1;
                                  end
                                 else
                                  j:=0;
                                 { don't check if the length byte was already wrong }
                                 if same_string then
                                  begin
                                    for i:=0 to len do
                                     begin
                                       if tai_string(hp1).str[j]<>value_str[i] then
                                        begin
                                          same_string:=false;
                                          break;
                                        end;
                                       inc(j);
                                     end;
                                  end;
                                 { found ? }
                                 if same_string then
                                  begin
                                    lab_str:=lastlabel;
                                    { create a new entry for ansistrings, but reuse the data }
                                    if (st_type in [st_ansistring,st_widestring]) then
                                     begin
                                       getdatalabel(l2);
                                       Consts.concat(Tai_label.Create(l2));
                                       Consts.concat(Tai_const_symbol.Create(lab_str));
                                       { return the offset of the real string }
                                       lab_str:=l2;
                                     end;
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
                   getdatalabel(lastlabel);
                   lab_str:=lastlabel;
                   if (cs_create_smart in aktmoduleswitches) then
                    Consts.concat(Tai_cut.Create);
                   Consts.concat(Tai_label.Create(lastlabel));
                   { generate an ansi string ? }
                   case st_type of
                      st_ansistring:
                        begin
                           { an empty ansi string is nil! }
                           if len=0 then
                             Consts.concat(Tai_const.Create_32bit(0))
                           else
                             begin
                                getdatalabel(l1);
                                getdatalabel(l2);
                                Consts.concat(Tai_label.Create(l2));
                                Consts.concat(Tai_const_symbol.Create(l1));
                                Consts.concat(Tai_const.Create_32bit(len));
                                Consts.concat(Tai_const.Create_32bit(len));
                                Consts.concat(Tai_const.Create_32bit(-1));
                                Consts.concat(Tai_label.Create(l1));
                                getmem(pc,len+2);
                                move(value_str^,pc^,len);
                                pc[len]:=#0;
                                { to overcome this problem we set the length explicitly }
                                { with the ending null char }
                                Consts.concat(Tai_string.Create_length_pchar(pc,len+1));
                                { return the offset of the real string }
                                lab_str:=l2;
                             end;
                        end;
                      st_widestring:
                        begin
                           { an empty wide string is nil! }
                           if len=0 then
                             Consts.concat(Tai_const.Create_32bit(0))
                           else
                             begin
                                getdatalabel(l1);
                                getdatalabel(l2);
                                Consts.concat(Tai_label.Create(l2));
                                Consts.concat(Tai_const_symbol.Create(l1));

                                { we use always UTF-16 coding for constants }
                                { at least for now                          }
                                { Consts.concat(Tai_const.Create_8bit(2)); }
                                Consts.concat(Tai_const.Create_32bit(len));
                                Consts.concat(Tai_const.Create_32bit(len));
                                Consts.concat(Tai_const.Create_32bit(-1));
                                Consts.concat(Tai_label.Create(l1));
                                for i:=0 to len-1 do
                                  Consts.concat(Tai_const.Create_16bit(pcompilerwidestring(value_str)^.data[i]));
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
                          { also length and terminating zero }
                          getmem(pc,l+3);
                          move(value_str^,pc[1],l+1);
                          pc[0]:=chr(l);
                          { to overcome this problem we set the length explicitly }
                          { with the ending null char }
                          pc[l+1]:=#0;
                          Consts.concat(Tai_string.Create_length_pchar(pc,l+2));
                        end;
                   end;
                end;
           end;
         reset_reference(location.reference);
         location.reference.symbol:=lab_str;
         location.loc:=LOC_MEM;
      end;


{*****************************************************************************
                           TCGSETCONSTNODE
*****************************************************************************}

    procedure tcgsetconstnode.pass_2;
      var
         hp1     : tai;
         lastlabel   : tasmlabel;
         i         : longint;
         neededtyp   : tait;
      begin
        { small sets are loaded as constants }
        if tsetdef(resulttype.def).settype=smallset then
         begin
           location.loc:=LOC_MEM;
           location.reference.is_immediate:=true;
           location.reference.offset:=plongint(value_set)^;
           exit;
         end;
        neededtyp:=ait_const_8bit;
        lastlabel:=nil;
        { const already used ? }
        if not assigned(lab_set) then
          begin
             { tries to found an old entry }
             hp1:=tai(Consts.first);
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
                                if tai_const(hp1).value<>value_set^[i] then
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
                             if plongint(value_set)^=tai_const(hp1).value then
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
                 getdatalabel(lastlabel);
                 lab_set:=lastlabel;
                 if (cs_create_smart in aktmoduleswitches) then
                  Consts.concat(Tai_cut.Create);
                 Consts.concat(Tai_label.Create(lastlabel));
                 if tsetdef(resulttype.def).settype=smallset then
                  begin
                    move(value_set^,i,sizeof(longint));
                    Consts.concat(Tai_const.Create_32bit(i));
                  end
                 else
                  begin
                    for i:=0 to 31 do
                      Consts.concat(Tai_const.Create_8bit(value_set^[i]));
                  end;
               end;
          end;
        reset_reference(location.reference);
        location.reference.symbol:=lab_set;
        location.loc:=LOC_MEM;
      end;


{*****************************************************************************
                             TCGNILNODE
*****************************************************************************}

    procedure tcgnilnode.pass_2;
      begin
         location.loc:=LOC_MEM;
         location.reference.is_immediate:=true;
         location.reference.offset:=0;
      end;


{*****************************************************************************
                          TCGPOINTERCONSTNODE
*****************************************************************************}

    procedure tcgguidconstnode.pass_2;
      var
        tmplabel : TAsmLabel;
        i : integer;
      begin
        location.loc:=LOC_MEM;
        { label for GUID }
        getdatalabel(tmplabel);
        consts.concat(Tai_label.Create(tmplabel));
        consts.concat(Tai_const.Create_32bit(value.D1));
        consts.concat(Tai_const.Create_16bit(value.D2));
        consts.concat(Tai_const.Create_16bit(value.D3));
        for i:=Low(value.D4) to High(value.D4) do
          consts.concat(Tai_const.Create_8bit(value.D4[i]));
        reset_reference(location.reference);
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
{
  $Log$
  Revision 1.5  2002-03-31 20:26:34  jonas
    + a_loadfpu_* and a_loadmm_* methods in tcg
    * register allocation is now handled by a class and is mostly processor
      independent (+rgobj.pas and i386/rgcpu.pas)
    * temp allocation is now handled by a class (+tgobj.pas, -i386\tgcpu.pas)
    * some small improvements and fixes to the optimizer
    * some register allocation fixes
    * some fpuvaroffset fixes in the unary minus node
    * push/popusedregisters is now called rg.save/restoreusedregisters and
      (for i386) uses temps instead of push/pop's when using -Op3 (that code is
      also better optimizable)
    * fixed and optimized register saving/restoring for new/dispose nodes
    * LOC_FPU locations now also require their "register" field to be set to
      R_ST, not R_ST0 (the latter is used for LOC_CFPUREGISTER locations only)
    - list field removed of the tnode class because it's not used currently
      and can cause hard-to-find bugs

  Revision 1.4  2002/02/26 09:12:39  jonas
    * fixed problem when compiling the compiler with Delphi (reported by
      "Luc Langlois" <L_Langlois@Videotron.ca>) (lo/hi don't work as in FPC
      when used with int64's under Delphi)

  Revision 1.3  2001/12/31 09:52:02  jonas
    * empty widestrings can also be optimized to the constant '0'

  Revision 1.2  2001/10/20 19:28:37  peter
    * interface 2 guid support
    * guid constants support

  Revision 1.1  2001/09/30 16:17:17  jonas
    * made most constant and mem handling processor independent

}
