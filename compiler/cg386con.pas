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

   uses tree;

    procedure secondrealconst(var p : ptree);
    procedure secondfixconst(var p : ptree);
    procedure secondordconst(var p : ptree);
    procedure secondniln(var p : ptree);
    procedure secondstringconst(var p : ptree);

implementation

   uses
     cobjects,verbose,
     symtable,aasm,i386,
     hcodegen;

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

    procedure secondfixconst(var p : ptree);

      begin
         { an fix comma const. behaves as a memory reference }
         p^.location.loc:=LOC_MEM;
         p^.location.reference.isintvalue:=true;
         p^.location.reference.offset:=p^.valuef;
      end;

    procedure secondordconst(var p : ptree);

      begin
         { an integer const. behaves as a memory reference }
         p^.location.loc:=LOC_MEM;
         p^.location.reference.isintvalue:=true;
         p^.location.reference.offset:=p^.value;
      end;

    procedure secondniln(var p : ptree);

      begin
         p^.location.loc:=LOC_MEM;
         p^.location.reference.isintvalue:=true;
         p^.location.reference.offset:=0;
      end;

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
{$else UseAnsiString}
                   pc:=getpcharcopy(p);
{$endif UseAnsiString}

                   concat_constlabel(lastlabel,conststring);
{$ifdef UseAnsiString}
  {$ifdef debug}
                   consts^.concat(new(pai_asm_comment,init('Header of ansistring')));
  {$endif debug}
                   consts^.concat(new(pai_const,init_32bit(p^.length)));
                   consts^.concat(new(pai_const,init_32bit(p^.length)));
                   consts^.concat(new(pai_const,init_32bit(-1)));
                   { to overcome this problem we set the length explicitly }
                   { with the ending null char }
                   consts^.concat(new(pai_string,init_length_pchar(pc,p^.length+1)));
{$else UseAnsiString}
                   { we still will have a problem if there is a #0 inside the pchar }
                   consts^.concat(new(pai_string,init_length_pchar(pc,length(p^.values^)+2)));
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

end.
{
  $Log$
  Revision 1.2  1998-06-05 16:13:31  pierre
    * fix for real and string consts inside inlined procs

  Revision 1.1  1998/05/23 01:21:02  peter
    + aktasmmode, aktoptprocessor, aktoutputformat
    + smartlink per module $SMARTLINK-/+ (like MMX) and moved to aktswitches
    + $LIBNAME to set the library name where the unit will be put in
    * splitted cgi386 a bit (codeseg to large for bp7)
    * nasm, tasm works again. nasm moved to ag386nsm.pas

}