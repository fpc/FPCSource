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

    procedure secondrealconst(var p : ptree);
    procedure secondfixconst(var p : ptree);
    procedure secondordconst(var p : ptree);
    procedure secondstringconst(var p : ptree);
    procedure secondsetcons(var p : ptree);
    procedure secondniln(var p : ptree);


implementation

    uses
      cobjects,verbose,globals,
      symtable,aasm,i386,
      hcodegen,cgai386,temp_gen,tgeni386,cgi386;

{*****************************************************************************
                             SecondRealConst
*****************************************************************************}

    procedure secondrealconst(var p : ptree);
      var
         hp1 : pai;
         lastlabel : plabel;
         found : boolean;
      begin
         clear_reference(p^.location.reference);
         lastlabel:=nil;
         found:=false;
         { const already used ? }
         if p^.labnumber=-1 then
           begin
              { tries to found an old entry }
              hp1:=pai(consts^.first);
              while assigned(hp1) do
                begin
                   if hp1^.typ=ait_label then
                     lastlabel:=pai_label(hp1)^.l
                   else
                     begin
                        if (hp1^.typ=p^.realtyp) and (lastlabel<>nil) then
                          begin
                             if ((p^.realtyp=ait_real_64bit) and (pai_double(hp1)^.value=p^.valued)) or
                               ((p^.realtyp=ait_real_extended) and (pai_extended(hp1)^.value=p^.valued)) or
                               ((p^.realtyp=ait_real_32bit) and (pai_single(hp1)^.value=p^.valued)) then
                               begin
                                  { found! }
                                  p^.labnumber:=lastlabel^.nb;
                                  break;
                               end;
                          end;
                        lastlabel:=nil;
                     end;
                   hp1:=pai(hp1^.next);
                end;
              { :-(, we must generate a new entry }
              if p^.labnumber=-1 then
                begin
                   getlabel(lastlabel);
                   p^.labnumber:=lastlabel^.nb;
                   concat_constlabel(lastlabel,constreal);
                   case p^.realtyp of
                     ait_real_64bit : consts^.concat(new(pai_double,init(p^.valued)));
                     ait_real_32bit : consts^.concat(new(pai_single,init(p^.valued)));
                  ait_real_extended : consts^.concat(new(pai_extended,init(p^.valued)));
                   else
                     internalerror(10120);
                   end;
                end;
           end;
         stringdispose(p^.location.reference.symbol);
         if assigned(lastlabel) then
           p^.location.reference.symbol:=stringdup(constlabel2str(lastlabel,constreal))
         else
           p^.location.reference.symbol:=stringdup(constlabelnb2str(p^.labnumber,constreal));
      end;


{*****************************************************************************
                             SecondFixConst
*****************************************************************************}

    procedure secondfixconst(var p : ptree);
      begin
         { an fix comma const. behaves as a memory reference }
         p^.location.loc:=LOC_MEM;
         p^.location.reference.isintvalue:=true;
         p^.location.reference.offset:=p^.valuef;
      end;


{*****************************************************************************
                             SecondOrdConst
*****************************************************************************}

    procedure secondordconst(var p : ptree);
      begin
         { an integer const. behaves as a memory reference }
         p^.location.loc:=LOC_MEM;
         p^.location.reference.isintvalue:=true;
         p^.location.reference.offset:=p^.value;
      end;


{*****************************************************************************
                             SecondStringConst
*****************************************************************************}

    procedure secondstringconst(var p : ptree);
      var
         hp1 : pai;
         lastlabel : plabel;
         pc : pchar;
         same_string : boolean;
         i : word;

      begin
         clear_reference(p^.location.reference);
         lastlabel:=nil;
         { const already used ? }
         if p^.labstrnumber=-1 then
           begin
              { tries to found an old entry }
              hp1:=pai(consts^.first);
              while assigned(hp1) do
                begin
                   if hp1^.typ=ait_label then
                     lastlabel:=pai_label(hp1)^.l
                   else
                     begin
                        if (hp1^.typ=ait_string) and (lastlabel<>nil) and
                          (pai_string(hp1)^.len=length(p^.values^)+2) then
                          begin
                             same_string:=true;
{$ifndef UseAnsiString}
                             for i:=1 to length(p^.values^) do
                               if pai_string(hp1)^.str[i]<>p^.values^[i] then
{$else}
                             for i:=0 to p^.length do
                               if pai_string(hp1)^.str[i]<>p^.values[i] then
{$endif}
                                 begin
                                    same_string:=false;
                                    break;
                                 end;
                             if same_string then
                               begin
                                  { found! }
                                  p^.labstrnumber:=lastlabel^.nb;
                                  break;
                               end;
                          end;
                        lastlabel:=nil;
                     end;
                   hp1:=pai(hp1^.next);
                end;
              { :-(, we must generate a new entry }
              if p^.labstrnumber=-1 then
                begin
                   getlabel(lastlabel);
                   p^.labstrnumber:=lastlabel^.nb;
{$ifndef UseAnsiString}
                   getmem(pc,length(p^.values^)+3);
                   move(p^.values^,pc^,length(p^.values^)+1);
                   pc[length(p^.values^)+1]:=#0;
                   concat_constlabel(lastlabel,conststring);
                   { we still will have a problem if there is a #0 inside the pchar }
                   consts^.concat(new(pai_string,init_length_pchar(pc,length(p^.values^)+2)));
{$else UseAnsiString}
                   if cs_ansistrings in aktswitches then
                     begin
                        concat_constlabel(lastlabel,conststring);
                        consts^.concat(new(pai_const,init_32bit(p^.length)));
                        consts^.concat(new(pai_const,init_32bit(p^.length)));
                        consts^.concat(new(pai_const,init_32bit(-1)));
                        getmem(pc,p^.length+1);
                        move(p^.values^,pc^,p^.length+1);
                        { to overcome this problem we set the length explicitly }
                        { with the ending null char }
                        consts^.concat(new(pai_string,init_length_pchar(pc,p^.length+1)));
                     end
                   else
                     begin
                        getmem(pc,p^.length+3);
                        move(p^.values^,pc[1],p^.length+1);
                        pc[0]:=chr(p^.length);
                        concat_constlabel(lastlabel,conststring);
                        { to overcome this problem we set the length explicitly }
                        { with the ending null char }
                        consts^.concat(new(pai_string,init_length_pchar(pc,p^.length+2)));
                     end;
{$endif UseAnsiString}
                end;
           end;
         stringdispose(p^.location.reference.symbol);
         if assigned(lastlabel) then
           p^.location.reference.symbol:=stringdup(constlabel2str(lastlabel,conststring))
         else
           p^.location.reference.symbol:=stringdup(constlabelnb2str(p^.labstrnumber,conststring));
         p^.location.loc := LOC_MEM;
      end;


{*****************************************************************************
                             SecondSetCons
*****************************************************************************}

    procedure secondsetcons(var p : ptree);
      var
         l : plabel;
         i : longint;
         hp : ptree;
         href,sref : treference;
{$ifdef TestSmallSet}
         smallsetvalue : longint;
         hr,hr2 : tregister;
{$endif TestSmallSet}
      begin
         { this should be reimplemented for smallsets }
         { differently  (PM) }
         { produce constant part }
{$ifdef TestSmallSet}
         if psetdef(p^.resulttype)^.settype=smallset then
           begin
              smallsetvalue:=(p^.constset^[3]*256)+p^.constset^[2];
              smallsetvalue:=(smallsetvalue*256+p^.constset^[1])*256+p^.constset^[0];
              {consts^.concat(new(pai_const,init_32bit(smallsetvalue)));}
              hr:=getregister32;
              exprasmlist^.concat(new(pai386,op_const_reg(A_MOV,S_L,
                smallsetvalue,hr)));
              hp:=p^.left;
              if assigned(hp) then
                begin
                   while assigned(hp) do
                     begin
                        secondpass(hp^.left);
                        if codegenerror then
                          exit;
                        case hp^.left^.location.loc of
                          LOC_MEM,LOC_REFERENCE :
                            begin
                               hr2:=getregister32;
                               exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_L,
                               newreference(hp^.left^.location.reference),hr2)));
                               exprasmlist^.concat(new(pai386,op_reg_reg(A_BTS,S_NO,
                                 hr2,hr)));
                               ungetregister32(hr2);
                            end;
                          LOC_REGISTER,LOC_CREGISTER :
                            exprasmlist^.concat(new(pai386,op_reg_reg(A_BTS,S_NO,
                              hp^.left^.location.register,hr)));
                          else
                            internalerror(10567);
                          end;
                        hp:=hp^.right;
                     end;
                end;
              p^.location.loc:=LOC_REGISTER;
              p^.location.register:=hr;
           end
         else
{$endif TestSmallSet}
           begin
             href.symbol := Nil;
             clear_reference(href);
             getlabel(l);
             stringdispose(p^.location.reference.symbol);
             href.symbol:=stringdup(constlabel2str(l,constseta));
             concat_constlabel(l,constseta);
             for i:=0 to 31 do
               consts^.concat(new(pai_const,init_8bit(p^.constset^[i])));
             hp:=p^.left;
             if assigned(hp) then
               begin
                  sref.symbol:=nil;
                  gettempofsizereference(32,sref);
                  concatcopy(href,sref,32,false);
                  while assigned(hp) do
                    begin
                       secondpass(hp^.left);
                       if codegenerror then
                         exit;
                       pushsetelement(hp^.left);
                       emitpushreferenceaddr(exprasmlist,sref);
                       { register is save in subroutine }
                       emitcall('SET_SET_BYTE',true);
                       hp:=hp^.right;
                    end;
                  p^.location.reference:=sref;
               end
             else
               p^.location.reference:=href;
           end;
      end;


{*****************************************************************************
                             SecondNilN
*****************************************************************************}

    procedure secondniln(var p : ptree);
      begin
         p^.location.loc:=LOC_MEM;
         p^.location.reference.isintvalue:=true;
         p^.location.reference.offset:=0;
      end;


end.
{
  $Log$
  Revision 1.6  1998-07-18 17:11:07  florian
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