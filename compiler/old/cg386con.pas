{
    $Id$
    Copyright (c) 1998-2000 by Florian Klaempfl

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

{$i defines.inc}

interface

    uses
      tree;

    procedure secondrealconst(var p : ptree);
    procedure secondfixconst(var p : ptree);
    procedure secondordconst(var p : ptree);
    procedure secondpointerconst(var p : ptree);
    procedure secondstringconst(var p : ptree);
    procedure secondsetconst(var p : ptree);
    procedure secondniln(var p : ptree);


implementation

    uses
      globtype,systems,
      cobjects,verbose,globals,
      symconst,symtable,aasm,types,
      hcodegen,temp_gen,pass_2,
      cpubase,cpuasm,
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
         lastlabel : pasmlabel;
         realait : tait;

      begin
         if (p^.value_real=1.0) then
           begin
              emit_none(A_FLD1,S_NO);
              p^.location.loc:=LOC_FPU;
              inc(fpuvaroffset);
           end
         else if (p^.value_real=0.0) then
           begin
              emit_none(A_FLDZ,S_NO);
              p^.location.loc:=LOC_FPU;
              inc(fpuvaroffset);
           end
         else
           begin
              lastlabel:=nil;
              realait:=floattype2ait[pfloatdef(p^.resulttype)^.typ];
              { const already used ? }
              if not assigned(p^.lab_real) then
                begin
                   { tries to find an old entry }
                   hp1:=pai(consts^.first);
                   while assigned(hp1) do
                     begin
                        if hp1^.typ=ait_label then
                          lastlabel:=pai_label(hp1)^.l
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
                        if (cs_create_smart in aktmoduleswitches) then
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
              p^.location.reference.symbol:=p^.lab_real;
              p^.location.loc:=LOC_MEM;
           end;
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

      var
         l : pasmlabel;

      begin
         p^.location.loc:=LOC_MEM;
         if is_64bitint(p^.resulttype) then
           begin
              getdatalabel(l);
              if (cs_create_smart in aktmoduleswitches) then
                consts^.concat(new(pai_cut,init));
              consts^.concat(new(pai_label,init(l)));
              consts^.concat(new(pai_const,init_32bit(lo(p^.value))));
              consts^.concat(new(pai_const,init_32bit(hi(p^.value))));
              reset_reference(p^.location.reference);
              p^.location.reference.symbol:=l;
           end
         else
           begin
              { non int64 const. behaves as a memory reference }
              p^.location.reference.is_immediate:=true;
              p^.location.reference.offset:=p^.value;
           end;
      end;


{*****************************************************************************
                             SecondPointerConst
*****************************************************************************}

    procedure secondpointerconst(var p : ptree);
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
         lastlabel   : pasmlabel;
         pc       : pchar;
         same_string : boolean;
         l,j,
         i,mylength  : longint;
      begin
         { for empty ansistrings we could return a constant 0 }
         if is_ansistring(p^.resulttype) and
            (p^.length=0) then
          begin
            p^.location.loc:=LOC_MEM;
            p^.location.reference.is_immediate:=true;
            p^.location.reference.offset:=0;
            exit;
          end;
         { const already used ? }
         lastlabel:=nil;
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
                   if hp1^.typ=ait_label then
                     lastlabel:=pai_label(hp1)^.l
                   else
                     begin
                        { when changing that code, be careful that }
                        { you don't use typed consts, which are    }
                        { are also written to consts           }
                        { currently, this is no problem, because   }
                        { typed consts have no leading length or   }
                        { they have no trailing zero           }
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
                                   consts^.concat(new(pai_const_symbol,init(p^.lab_str)));
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
                   if (cs_create_smart in aktmoduleswitches) then
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
                                consts^.concat(new(pai_const_symbol,init(l1)));
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
         p^.location.reference.symbol:=p^.lab_str;
         p^.location.loc:=LOC_MEM;
      end;


{*****************************************************************************
                             SecondSetCons
*****************************************************************************}

    procedure secondsetconst(var p : ptree);
      var
         hp1     : pai;
         lastlabel   : pasmlabel;
         i         : longint;
         neededtyp   : tait;
      begin
        { small sets are loaded as constants }
        if psetdef(p^.resulttype)^.settype=smallset then
         begin
           p^.location.loc:=LOC_MEM;
           p^.location.reference.is_immediate:=true;
           p^.location.reference.offset:=plongint(p^.value_set)^;
           exit;
         end;
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
                  if hp1^.typ=ait_label then
                    lastlabel:=pai_label(hp1)^.l
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
                 if (cs_create_smart in aktmoduleswitches) then
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
        p^.location.reference.symbol:=p^.lab_set;
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
  Revision 1.1  2000-10-14 10:14:56  peter
    * moehrendorf oct 2000 rewrite

  Revision 1.4  2000/09/24 21:19:49  peter
    * delphi compile fixes

  Revision 1.3  2000/08/16 13:06:06  florian
    + support of 64 bit integer constants

  Revision 1.2  2000/07/13 11:32:33  michael
  + removed logs

}
