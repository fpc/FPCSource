{
    $Id$
    Copyright (c) 1998-2000 by Florian Klaempfl, Pierre Muller

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
unit opts386;
interface

uses
  options;

type
  poption386=^toption386;
  toption386=object(toption)
    procedure interpret_proc_specific_options(const opt:string);virtual;
  end;

implementation

uses
  globtype,systems,globals;

procedure toption386.interpret_proc_specific_options(const opt:string);
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
                 '-' : initglobalswitches:=initglobalswitches-[cs_optimize,cs_fastoptimize,cs_slowoptimize,cs_littlesize,
                           cs_regalloc,cs_uncertainopts];
                 'g' : initglobalswitches:=initglobalswitches+[cs_littlesize];
                 'G' : initglobalswitches:=initglobalswitches-[cs_littlesize];
                 'r' : initglobalswitches:=initglobalswitches+[cs_regalloc];
                 'u' : initglobalswitches:=initglobalswitches+[cs_optimize,cs_uncertainopts];
                 '1' : initglobalswitches:=initglobalswitches-[cs_slowoptimize,cs_uncertainopts]+[cs_optimize,cs_fastoptimize];
                 '2' : initglobalswitches:=initglobalswitches-[cs_uncertainopts]+[cs_optimize,cs_fastoptimize,cs_slowoptimize];
                 '3' : initglobalswitches:=initglobalswitches+[cs_optimize,cs_fastoptimize,cs_slowoptimize,cs_uncertainopts];
                 'p' :
                   Begin
                     If j < Length(Opt) Then
                       Begin
                         Case opt[j+1] Of
                           '1': initoptprocessor := Class386;
                           '2': initoptprocessor := ClassP5;
                           '3': initoptprocessor := ClassP6
                           Else IllegalPara(Opt)
                         End;
                         Inc(j);
                       End
                     Else IllegalPara(opt)
                   End
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
             initasmmode:=asmmode_i386_direct
           else
            IllegalPara(opt);
         end;
  else
   IllegalPara(opt);
  end;
end;

end.
{
  $Log$
  Revision 1.17  2000-01-07 01:14:28  peter
    * updated copyright to 2000

  Revision 1.16  1999/08/04 13:02:47  jonas
    * all tokens now start with an underscore
    * PowerPC compiles!!

  Revision 1.15  1998/12/11 00:03:22  peter
    + globtype,tokens,version unit splitted from globals

  Revision 1.14  1998/11/12 09:00:26  michael
  * Fixed syntax error

  Revision 1.13  1998/11/11 20:11:39  jonas
    * lower optimization levels disable higher optimization level features
    * check if there actually is a character after -Op before comparing it with a number

  Revision 1.12  1998/10/13 13:10:20  peter
    * new style for m68k/i386 infos and enums

  Revision 1.11  1998/09/25 09:57:08  peter
    * moved -A to options.pas, becuase the code is the same

}
