{
    $Id$
    Copyright (c) 1993-98 by Florian Klaempfl

    Generate i386 assembler for constants

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
unit cg386con;
interface

    uses
      tree;

{$define SMALLSETORD}


    procedure secondrealconst(var p : ptree);
    procedure secondfixconst(var p : ptree);
    procedure secondordconst(var p : ptree);
    procedure secondstringconst(var p : ptree);
    procedure secondsetconst(var p : ptree);
    procedure secondniln(var p : ptree);


implementation

    uses
      globtype,systems,
      cobjects,verbose,globals,
      symtable,aasm,types,
      hcodegen,temp_gen,pass_2,
{$ifndef OLDASM}
      i386base,i386asm,
{$else}
      i386,
{$endif}
      cgai386,tgeni386;

{*****************************************************************************
                             SecondRealConst
*****************************************************************************}

    procedure secondrealconst(var p : ptree);
      const
        floattype2ait:array[tfloattype] of tait=
          (ait_real_32bit,ait_real_64bit,ait_real_80bit,ait_comp_64bit,ait_none,ait_none);

      var
         hp1 : pai;
         lastlabel : plabel;
         realait : tait;
      begin
         lastlabel:=nil;
         realait:=floattype2ait[pfloatdef(p^.resulttype)^.typ];
         { const already used ? }
         if not assigned(p^.lab_real) then
           begin
              { tries to found an old entry }
              hp1:=pai(consts^.first);
              while assigned(hp1) do
                begin
{$ifdef NEWLAB}
                   if hp1^.typ=ait_symbol then
                     lastlabel:=pasmlabel(pai_symbol(hp1)^.sym)
{$else}
                   if hp1^.typ=ait_label then
                     lastlabel:=pai_label(hp1)^.l
{$endif}
                   else
                     begin
                        if (hp1^.typ=realait) and (lastlabel<>nil) then
                          begin
                             if(
                                ((realait=ait_real_32bit) and (pai_real_32bit(hp1)^.value=p^.value_real)) or
                                ((realait=ait_real_64bit) and (pai_real_64bit(hp1)^.value=p^.value_real)) or
                                ((realait=ait_real_80bit) and (pai_real_80bit(hp1)^.value=p^.value_real)) or
                                ((realait=ait_comp_64bit) and (pai_comp_64bit(hp1)^.value=p^.value_real))
                               ) then
                               begin
                                  { found! }
                                  p^.lab_real:=lastlabel;
                                  break;
                               end;
                          end;
                        lastlabel:=nil;
                     end;
                   hp1:=pai(hp1^.next);
                end;
              { :-(, we must generate a new entry }
              if not assigned(p^.lab_real) then
                begin
                   getdatalabel(lastlabel);
                   p^.lab_real:=lastlabel;
                   if (cs_smartlink in aktmoduleswitches) then
                    consts^.concat(new(pai_cut,init));
                   consts^.concat(new(pai_label,init(lastlabel)));
                   case realait of
                     ait_real_32bit :
                       consts^.concat(new(pai_real_32bit,init(p^.value_real)));
                     ait_real_64bit :
                       consts^.concat(new(pai_real_64bit,init(p^.value_real)));
                     ait_real_80bit :
                       consts^.concat(new(pai_real_80bit,init(p^.value_real)));
                     ait_comp_64bit :
                       consts^.concat(new(pai_comp_64bit,init(p^.value_real)));
                   else
                     internalerror(10120);
                   end;
                end;
           end;
         reset_reference(p^.location.reference);
         p^.location.reference.symbol:=newasmsymbol(lab2str(p^.lab_real));
         p^.location.loc:=LOC_MEM;
      end;


{*****************************************************************************
                             SecondFixConst
*****************************************************************************}

    procedure secondfixconst(var p : ptree);
      begin
         { an fix comma const. behaves as a memory reference }
         p^.location.loc:=LOC_MEM;
         p^.location.reference.is_immediate:=true;
         p^.location.reference.offset:=p^.value_fix;
      end;


{*****************************************************************************
                             SecondOrdConst
*****************************************************************************}

    procedure secondordconst(var p : ptree);
      begin
         { an integer const. behaves as a memory reference }
         p^.location.loc:=LOC_MEM;
         p^.location.reference.is_immediate:=true;
         p^.location.reference.offset:=p^.value;
      end;


{*****************************************************************************
                             SecondStringConst
*****************************************************************************}

    procedure secondstringconst(var p : ptree);
      var
         hp1 : pai;
         l1,l2,
         lastlabel   : plabel;
         pc          : pchar;
         same_string : boolean;
         l,j,
         i,mylength  : longint;
      begin
         lastlabel:=nil;
         { const already used ? }
         if not assigned(p^.lab_str) then
           begin
              if is_shortstring(p^.resulttype) then
               mylength:=p^.length+2
              else
               mylength:=p^.length+1;
              { tries to found an old entry }
              hp1:=pai(consts^.first);
              while assigned(hp1) do
                begin
{$ifdef NEWLAB}
                   if hp1^.typ=ait_symbol then
                     lastlabel:=pasmlabel(pai_symbol(hp1)^.sym)
{$else}
                   if hp1^.typ=ait_label then
                     lastlabel:=pai_label(hp1)^.l
{$endif}
                   else
                     begin
                        { when changing that code, be careful that }
                        { you don't use typed consts, which are    }
                        { are also written to consts               }
                        { currently, this is no problem, because   }
                        { typed consts have no leading length or   }
                        { they have no trailing zero               }
                        if (hp1^.typ=ait_string) and (lastlabel<>nil) and
                           (pai_string(hp1)^.len=mylength) then
                          begin
                             same_string:=true;
                             { if shortstring then check the length byte first and
                               set the start index to 1 }
                             if is_shortstring(p^.resulttype) then
                              begin
                                if p^.length<>ord(pai_string(hp1)^.str[0]) then
                                 same_string:=false;
                                j:=1;
                              end
                             else
                              j:=0;
                             { don't check if the length byte was already wrong }
                             if same_string then
                              begin
                                for i:=0 to p^.length do
                                 begin
                                   if pai_string(hp1)^.str[j]<>p^.value_str[i] then
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
                                p^.lab_str:=lastlabel;
                                { create a new entry for ansistrings, but reuse the data }
                                if (p^.stringtype in [st_ansistring,st_widestring]) then
                                 begin
                                   getdatalabel(l2);
                                   consts^.concat(new(pai_label,init(l2)));
                                   consts^.concat(new(pai_const_symbol,initname(lab2str(p^.lab_str))));
                                   { return the offset of the real string }
                                   p^.lab_str:=l2;
                                 end;
                                break;
                              end;
                          end;
                        lastlabel:=nil;
                     end;
                   hp1:=pai(hp1^.next);
                end;
              { :-(, we must generate a new entry }
              if not assigned(p^.lab_str) then
                begin
                   getdatalabel(lastlabel);
                   p^.lab_str:=lastlabel;
                   if (cs_smartlink in aktmoduleswitches) then
                    consts^.concat(new(pai_cut,init));
                   consts^.concat(new(pai_label,init(lastlabel)));
                   { generate an ansi string ? }
                   case p^.stringtype of
                      st_ansistring:
                        begin
                           { an empty ansi string is nil! }
                           if p^.length=0 then
                             consts^.concat(new(pai_const,init_32bit(0)))
                           else
                             begin
                                getdatalabel(l1);
                                getdatalabel(l2);
                                consts^.concat(new(pai_label,init(l2)));
                                consts^.concat(new(pai_const_symbol,initname(lab2str(l1))));
                                consts^.concat(new(pai_const,init_32bit(p^.length)));
                                consts^.concat(new(pai_const,init_32bit(p^.length)));
                                consts^.concat(new(pai_const,init_32bit(-1)));
                                consts^.concat(new(pai_label,init(l1)));
                                getmem(pc,p^.length+2);
                                move(p^.value_str^,pc^,p^.length);
                                pc[p^.length]:=#0;
                                { to overcome this problem we set the length explicitly }
                                { with the ending null char }
                                consts^.concat(new(pai_string,init_length_pchar(pc,p^.length+1)));
                                { return the offset of the real string }
                                p^.lab_str:=l2;
                             end;
                        end;
                      st_shortstring:
                        begin
                          { truncate strings larger than 255 chars }
                          if p^.length>255 then
                           l:=255
                          else
                           l:=p^.length;
                          { also length and terminating zero }
                          getmem(pc,l+3);
                          move(p^.value_str^,pc[1],l+1);
                          pc[0]:=chr(l);
                          { to overcome this problem we set the length explicitly }
                          { with the ending null char }
                          pc[l+1]:=#0;
                          consts^.concat(new(pai_string,init_length_pchar(pc,l+2)));
                        end;
                   end;
                end;
           end;
         reset_reference(p^.location.reference);
         p^.location.reference.symbol:=newasmsymbol(lab2str(p^.lab_str));
         p^.location.loc:=LOC_MEM;
      end;


{*****************************************************************************
                             SecondSetCons
*****************************************************************************}

    procedure secondsetconst(var p : ptree);
      var
         hp1         : pai;
         lastlabel   : plabel;
         i           : longint;
         neededtyp   : tait;
      begin
{$ifdef SMALLSETORD}
        { small sets are loaded as constants }
        if psetdef(p^.resulttype)^.settype=smallset then
         begin
           p^.location.loc:=LOC_MEM;
           p^.location.reference.is_immediate:=true;
           p^.location.reference.offset:=plongint(p^.value_set)^;
           exit;
         end;
{$endif}
        if psetdef(p^.resulttype)^.settype=smallset then
         neededtyp:=ait_const_32bit
        else
         neededtyp:=ait_const_8bit;
        lastlabel:=nil;
        { const already used ? }
        if not assigned(p^.lab_set) then
          begin
             { tries to found an old entry }
             hp1:=pai(consts^.first);
             while assigned(hp1) do
               begin
{$ifdef NEWLAB}
                  if hp1^.typ=ait_symbol then
                    lastlabel:=pasmlabel(pai_symbol(hp1)^.sym)
{$else}
                  if hp1^.typ=ait_label then
                    lastlabel:=pai_label(hp1)^.l
{$endif}
                  else
                    begin
                      if (lastlabel<>nil) and (hp1^.typ=neededtyp) then
                        begin
                          if (hp1^.typ=ait_const_8bit) then
                           begin
                             { compare normal set }
                             i:=0;
                             while assigned(hp1) and (i<32) do
                              begin
                                if pai_const(hp1)^.value<>p^.value_set^[i] then
                                 break;
                                inc(i);
                                hp1:=pai(hp1^.next);
                              end;
                             if i=32 then
                              begin
                                { found! }
                                p^.lab_set:=lastlabel;
                                break;
                              end;
                             { leave when the end of consts is reached, so no
                               hp1^.next is done }
                             if not assigned(hp1) then
                              break;
                           end
                          else
                           begin
                             { compare small set }
                             if plongint(p^.value_set)^=pai_const(hp1)^.value then
                              begin
                                { found! }
                                p^.lab_set:=lastlabel;
                                break;
                              end;
                           end;
                        end;
                      lastlabel:=nil;
                    end;
                  hp1:=pai(hp1^.next);
               end;
             { :-(, we must generate a new entry }
             if not assigned(p^.lab_set) then
               begin
                 getdatalabel(lastlabel);
                 p^.lab_set:=lastlabel;
                 if (cs_smartlink in aktmoduleswitches) then
                  consts^.concat(new(pai_cut,init));
                 consts^.concat(new(pai_label,init(lastlabel)));
                 if psetdef(p^.resulttype)^.settype=smallset then
                  begin
                    move(p^.value_set^,i,sizeof(longint));
                    consts^.concat(new(pai_const,init_32bit(i)));
                  end
                 else
                  begin
                    for i:=0 to 31 do
                      consts^.concat(new(pai_const,init_8bit(p^.value_set^[i])));
                  end;
               end;
          end;
        reset_reference(p^.location.reference);
        p^.location.reference.symbol:=newasmsymbol(lab2str(p^.lab_set));
        p^.location.loc:=LOC_MEM;
      end;


{*****************************************************************************
                             SecondNilN
*****************************************************************************}

    procedure secondniln(var p : ptree);
      begin
         p^.location.loc:=LOC_MEM;
         p^.location.reference.is_immediate:=true;
         p^.location.reference.offset:=0;
      end;


end.
{
  $Log$
  Revision 1.35  1999-05-21 13:54:47  peter
    * NEWLAB for label as symbol

  Revision 1.34  1999/05/12 00:19:41  peter
    * removed R_DEFAULT_SEG
    * uniform float names

  Revision 1.33  1999/05/06 09:05:12  peter
    * generic write_float and str_float
    * fixed constant float conversions

  Revision 1.32  1999/05/01 13:24:06  peter
    * merged nasm compiler
    * old asm moved to oldasm/

  Revision 1.31  1999/04/07 15:16:43  pierre
   * zero length string were generated multiple times

  Revision 1.30  1999/03/31 13:51:49  peter
    * shortstring reuse fixed

  Revision 1.29  1999/02/25 21:02:25  peter
    * ag386bin updates
    + coff writer

  Revision 1.28  1999/02/22 02:15:08  peter
    * updates for ag386bin

  Revision 1.27  1999/01/19 14:21:59  peter
    * shortstring truncated after 255 chars

  Revision 1.26  1998/12/11 00:02:49  peter
    + globtype,tokens,version unit splitted from globals

  Revision 1.25  1998/12/10 14:39:30  florian
    * bug with p(const a : ansistring) fixed
    * duplicate constant ansistrings were handled wrong, fixed

  Revision 1.24  1998/11/28 15:36:02  michael
  Fixed generation of constant ansistrings

  Revision 1.23  1998/11/26 14:39:12  peter
    * ansistring -> pchar fixed
    * ansistring constants fixed
    * ansistring constants are now written once

  Revision 1.22  1998/11/24 13:40:59  peter
    * release smallsetord, so small sets constant are handled like longints

  Revision 1.21  1998/11/24 12:52:41  peter
    * sets are not written twice anymore
    * optimize for emptyset+single element which uses a new routine from
      set.inc FPC_SET_CREATE_ELEMENT

  Revision 1.20  1998/11/16 12:11:29  peter
    * fixed ansistring crash

  Revision 1.19  1998/11/05 23:40:45  pierre
   * fix for const strings

  Revision 1.18  1998/11/05 15:26:38  pierre
   * fix for missing zero after string const

  Revision 1.17  1998/11/05 12:02:32  peter
    * released useansistring
    * removed -Sv, its now available in fpc modes

  Revision 1.16  1998/11/04 21:07:43  michael
  * undid peters change. Constant ansistrings should end on null too cd ..

  Revision 1.15  1998/11/04 10:11:36  peter
    * ansistring fixes

  Revision 1.14  1998/09/17 09:42:13  peter
    + pass_2 for cg386
    * Message() -> CGMessage() for pass_1/pass_2

  Revision 1.13  1998/09/07 18:45:53  peter
    * update smartlinking, uses getdatalabel
    * renamed ptree.value vars to value_str,value_real,value_set

  Revision 1.12  1998/08/28 10:56:57  peter
    * removed warnings

  Revision 1.11  1998/08/14 18:18:39  peter
    + dynamic set contruction
    * smallsets are now working (always longint size)

  Revision 1.10  1998/08/04 13:22:46  pierre
    * weird bug fixed :
      a pchar ' ' (simple space or any other letter) was found to
      be equal to a string of length zero !!!
      thus printing out non sense
      found that out while checking Control-C !!
    + added column info also in RHIDE format as
      it might be usefull later

  Revision 1.9  1998/07/20 18:40:10  florian
    * handling of ansi string constants should now work

  Revision 1.8  1998/07/20 10:23:00  florian
    * better ansi string assignement

  Revision 1.7  1998/07/18 22:54:25  florian
    * some ansi/wide/longstring support fixed:
       o parameter passing
       o returning as result from functions

  Revision 1.6  1998/07/18 17:11:07  florian
    + ansi string constants fixed
    + switch $H partial implemented

  Revision 1.5  1998/06/25 08:48:07  florian
    * first version of rtti support

  Revision 1.4  1998/06/08 13:13:31  pierre
    + temporary variables now in temp_gen.pas unit
      because it is processor independent
    * mppc68k.bat modified to undefine i386 and support_mmx
      (which are defaults for i386)

  Revision 1.3  1998/06/05 17:44:11  peter
    * splitted cgi386

  Revision 1.2  1998/06/05 16:13:31  pierre
    * fix for real and string consts inside inlined procs

  Revision 1.1  1998/05/23 01:21:02  peter
    + aktasmmode, aktoptprocessor, aktoutputformat
    + smartlink per module $SMARTLINK-/+ (like MMX) and moved to aktswitches
    + $LIBNAME to set the library name where the unit will be put in
    * splitted cgi386 a bit (codeseg to large for bp7)
    * nasm, tasm works again. nasm moved to ag386nsm.pas

}
