{
    $Id$
    Copyright (c) 1998-2002 by Florian Klaempfl, Pierre Muller

    interprets the commandline options which are i386 specific

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
unit cpuswtch;

{$i fpcdefs.inc}

interface

uses
  options;

type
  toptionx86_64=class(toption)
    procedure interpret_proc_specific_options(const opt:string);override;
  end;

implementation

uses
  cutils,globtype,systems,globals;

procedure toptionx86_64.interpret_proc_specific_options(const opt:string);
var
  j     : longint;
  More  : string;
begin
  More:=Upper(copy(opt,3,length(opt)-2));
  case opt[2] of
   'O' : Begin
           j := 3;
           While (j <= Length(Opt)) Do
             Begin
               case opt[j] of
                 '-' :
                   begin
                     initglobalswitches:=initglobalswitches-[cs_optimize,cs_fastoptimize,cs_slowoptimize,cs_littlesize,
                       cs_regalloc,cs_uncertainopts];
                     FillChar(ParaAlignment,sizeof(ParaAlignment),0);
                   end;
                 'a' :
                   begin
                     UpdateAlignmentStr(Copy(Opt,j+1,255),ParaAlignment);
                     j:=length(Opt);
                   end;
                 'g' : initglobalswitches:=initglobalswitches+[cs_littlesize];
                 'G' : initglobalswitches:=initglobalswitches-[cs_littlesize];
                 'r' :
                   begin
                     initglobalswitches:=initglobalswitches+[cs_regalloc];
                     Simplify_ppu:=false;
                   end;
                 'u' : initglobalswitches:=initglobalswitches+[cs_uncertainopts];
                 '1' : initglobalswitches:=initglobalswitches-[cs_fastoptimize,cs_slowoptimize]+[cs_optimize];
                 '2' : initglobalswitches:=initglobalswitches-[cs_slowoptimize]+[cs_optimize,cs_fastoptimize];
                 '3' : initglobalswitches:=initglobalswitches+[cs_optimize,cs_fastoptimize,cs_slowoptimize];
                 else IllegalPara(opt);
               End;
               Inc(j)
             end;
         end;
   'R' : begin
           if More='ATT' then
            initasmmode:=asmmode_i386_att
           else
            if More='INTEL' then
             initasmmode:=asmmode_i386_intel
           else
            if More='DIRECT' then
             initasmmode:=asmmode_direct
           else
            IllegalPara(opt);
         end;
  else
   IllegalPara(opt);
  end;
end;


initialization
  coption:=toptionx86_64;
end.
{
  $Log$
  Revision 1.2  2002-08-10 14:53:38  carl
    + moved target_cpu_string to cpuinfo
    * renamed asmmode enum.
    * assembler reader has now less ifdef's
    * move from nppcmem.pas -> ncgmem.pas vec. node.

  Revision 1.1  2002/07/24 22:38:15  florian
    + initial release of x86-64 target code

  Revision 1.7  2002/05/18 13:34:22  peter
    * readded missing revisions

  Revision 1.6  2002/05/16 19:46:50  carl
  + defines.inc -> fpcdefs.inc to avoid conflicts if compiling by hand
  + try to fix temp allocation (still in ifdef)
  + generic constructor calls
  + start of tassembler / tmodulebase class cleanup
}
