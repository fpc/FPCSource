{
    $Id$
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
      cginfo,cgbase,tgobj,rgobj
{$ifdef delphi}
      ,dmisc
{$endif}
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

      begin
        location_reset(location,LOC_CREFERENCE,def_cgsize(resulttype.def));
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
                  objectlibrary.getdatalabel(lastlabel);
                  lab_real:=lastlabel;
                  if (cs_create_smart in aktmoduleswitches) then
                   Consts.concat(Tai_cut.Create);
                  consts.concat(tai_align.create(const_align(4)));
                  Consts.concat(Tai_label.Create(lastlabel));
                  case realait of
                    ait_real_32bit :
                      Consts.concat(Tai_real_32bit.Create(ts32real(value_real)));
                    ait_real_64bit :
                      Consts.concat(Tai_real_64bit.Create(ts64real(value_real)));
                    ait_real_80bit :
                      Consts.concat(Tai_real_80bit.Create(value_real));
{$ifdef ver1_0}
                    ait_comp_64bit :
                      Consts.concat(Tai_comp_64bit.Create(value_real));
{$else ver1_0}
                    { the round is necessary for native compilers where comp isn't a float }
                    ait_comp_64bit :
                      Consts.concat(Tai_comp_64bit.Create(round(value_real)));
{$endif ver1_0}
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
         location.valueqword:=TConstExprUInt(value);
      end;


{*****************************************************************************
                          TCGPOINTERCONSTNODE
*****************************************************************************}

    procedure tcgpointerconstnode.pass_2;
      begin
         { an integer const. behaves as a memory reference }
         location_reset(location,LOC_CONSTANT,OS_ADDR);
         location.value:=AWord(value);
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
         if (st_type in [st_ansistring,st_widestring]) and
            (len=0) then
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
                  { tries to found an old entry }
                  hp1:=tai(Consts.first);
                  while assigned(hp1) do
                    begin
                       if hp1.typ=ait_label then
                         begin
                           lastlabel:=tai_label(hp1).l;
                           lastlabelhp:=hp1;
                         end
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
                                 case st_type of
                                   st_shortstring:
                                     begin
                                       if len=ord(tai_string(hp1).str[0]) then
                                        j:=1
                                       else
                                        same_string:=false;
                                     end;
                                   st_ansistring,
                                   st_widestring :
                                     begin
                                       { before the string the following sequence must be found:
                                         <label>
                                           constsymbol <datalabel>
                                           const32 <len>
                                           const32 <len>
                                           const32 -1
                                         we must then return <label> to reuse
                                       }
                                       hp2:=tai(lastlabelhp.previous);
                                       if assigned(hp2) and
                                          (hp2.typ=ait_const_32bit) and
                                          (tai_const(hp2).value=-1) and
                                          assigned(hp2.previous) and
                                          (tai(hp2.previous).typ=ait_const_32bit) and
                                          (tai_const(hp2.previous).value=len) and
                                          assigned(hp2.previous.previous) and
                                          (tai(hp2.previous.previous).typ=ait_const_32bit) and
                                          (tai_const(hp2.previous.previous).value=len) and
                                          assigned(hp2.previous.previous.previous) and
                                          (tai(hp2.previous.previous.previous).typ=ait_const_symbol) and
                                          assigned(hp2.previous.previous.previous.previous) and
                                          (tai(hp2.previous.previous.previous.previous).typ=ait_label) then
                                         begin
                                           lastlabel:=tai_label(hp2.previous.previous.previous.previous).l;
                                           j:=0;
                                         end
                                       else
                                         same_string:=false;
                                     end;
                                   else
                                     same_string:=false;
                                 end;
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
                   if (cs_create_smart in aktmoduleswitches) then
                    Consts.concat(Tai_cut.Create);
                   consts.concat(tai_align.create(const_align(4)));
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
                                objectlibrary.getdatalabel(l1);
                                objectlibrary.getdatalabel(l2);
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
                                objectlibrary.getdatalabel(l1);
                                objectlibrary.getdatalabel(l2);
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
                                { terminating zero }
                                Consts.concat(Tai_const.Create_16bit(0));
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
      type
         setbytes=array[0..31] of byte;
         Psetbytes=^setbytes;
      begin
        { small sets are loaded as constants }
        if tsetdef(resulttype.def).settype=smallset then
         begin
           location_reset(location,LOC_CONSTANT,OS_32);
           location.value:=PAWord(value_set)^;
           exit;
         end;
        location_reset(location,LOC_CREFERENCE,OS_NO);
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
                            {$ifdef oldset}
                                if tai_const(hp1).value<>value_set^[i] then
                            {$else}
                                if tai_const(hp1).value<>Psetbytes(value_set)^[i] then
                            {$endif}
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
                 objectlibrary.getdatalabel(lastlabel);
                 lab_set:=lastlabel;
                 if (cs_create_smart in aktmoduleswitches) then
                  Consts.concat(Tai_cut.Create);
                 consts.concat(tai_align.create(const_align(4)));
                 Consts.concat(Tai_label.Create(lastlabel));
                 if tsetdef(resulttype.def).settype=smallset then
                  begin
                    move(value_set^,i,sizeof(longint));
                    Consts.concat(Tai_const.Create_32bit(i));
                  end
                 else
                  begin
                    for i:=0 to 31 do
                      Consts.concat(Tai_const.Create_8bit(Psetbytes(value_set)^[i]));
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
        consts.concat(tai_align.create(const_align(16)));
        consts.concat(Tai_label.Create(tmplabel));
        consts.concat(Tai_const.Create_32bit(value.D1));
        consts.concat(Tai_const.Create_16bit(value.D2));
        consts.concat(Tai_const.Create_16bit(value.D3));
        for i:=Low(value.D4) to High(value.D4) do
          consts.concat(Tai_const.Create_8bit(value.D4[i]));
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
  Revision 1.27  2003-04-24 22:29:57  florian
    * fixed a lot of PowerPC related stuff

  Revision 1.26  2003/01/05 13:36:53  florian
    * x86-64 compiles
    + very basic support for float128 type (x86-64 only)

  Revision 1.25  2002/12/29 16:58:11  peter
    * write terminating 0 for widestring constants

  Revision 1.24  2002/12/07 14:10:21  carl
    * fix warnings by adding explicit typecasts

  Revision 1.23  2002/11/25 17:43:17  peter
    * splitted defbase in defutil,symutil,defcmp
    * merged isconvertable and is_equal into compare_defs(_ext)
    * made operator search faster by walking the list only once

  Revision 1.22  2002/11/09 15:36:50  carl
    * align all constants correctly (default of 4 size for real type constants)

  Revision 1.21  2002/10/06 21:01:50  peter
    * use tconstexpruint instead of qword

  Revision 1.20  2002/10/05 12:43:25  carl
    * fixes for Delphi 6 compilation
     (warning : Some features do not work under Delphi)

  Revision 1.19  2002/08/18 20:06:23  peter
    * inlining is now also allowed in interface
    * renamed write/load to ppuwrite/ppuload
    * tnode storing in ppu
    * nld,ncon,nbas are already updated for storing in ppu

  Revision 1.18  2002/08/11 14:32:26  peter
    * renamed current_library to objectlibrary

  Revision 1.17  2002/08/11 13:24:11  peter
    * saving of asmsymbols in ppu supported
    * asmsymbollist global is removed and moved into a new class
      tasmlibrarydata that will hold the info of a .a file which
      corresponds with a single module. Added librarydata to tmodule
      to keep the library info stored for the module. In the future the
      objectfiles will also be stored to the tasmlibrarydata class
    * all getlabel/newasmsymbol and friends are moved to the new class

  Revision 1.16  2002/08/10 17:15:06  jonas
    * endianess fix

  Revision 1.15  2002/07/23 12:34:30  daniel
  * Readded old set code. To use it define 'oldset'. Activated by default
    for ppc.

  Revision 1.14  2002/07/22 11:48:04  daniel
  * Sets are now internally sets.

  Revision 1.13  2002/07/20 11:57:53  florian
    * types.pas renamed to defbase.pas because D6 contains a types
      unit so this would conflicts if D6 programms are compiled
    + Willamette/SSE2 instructions to assembler added

  Revision 1.12  2002/07/01 18:46:22  peter
    * internal linker
    * reorganized aasm layer

  Revision 1.11  2002/07/01 16:23:53  peter
    * cg64 patch
    * basics for currency
    * asnode updates for class and interface (not finished)

  Revision 1.10  2002/05/18 13:34:09  peter
    * readded missing revisions

  Revision 1.9  2002/05/16 19:46:37  carl
  + defines.inc -> fpcdefs.inc to avoid conflicts if compiling by hand
  + try to fix temp allocation (still in ifdef)
  + generic constructor calls
  + start of tassembler / tmodulebase class cleanup

  Revision 1.7  2002/04/04 19:05:57  peter
    * removed unused units
    * use tlocation.size in cg.a_*loc*() routines

  Revision 1.6  2002/04/02 17:11:28  peter
    * tlocation,treference update
    * LOC_CONSTANT added for better constant handling
    * secondadd splitted in multiple routines
    * location_force_reg added for loading a location to a register
      of a specified size
    * secondassignment parses now first the right and then the left node
      (this is compatible with Kylix). This saves a lot of push/pop especially
      with string operations
    * adapted some routines to use the new cg methods

  Revision 1.5  2002/03/31 20:26:34  jonas
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

}







