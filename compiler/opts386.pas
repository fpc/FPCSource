{
    $Id$
    Copyright (c) 1993-98 by Florian Klaempfl, Pierre Muller

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
  systems,globals;

procedure toption386.interpret_proc_specific_options(const opt:string);
var
  j     : longint;
  More  : string;
begin
  More:=Upper(copy(opt,3,length(opt)-2));
  case opt[2] of
   'A' : begin
           if set_string_asm(More) then
            initoutputformat:=target_asm.id
           else
            IllegalPara(opt);
         end;
   'O' : begin
           for j:=3 to length(opt) do
           case opt[j] of
            '-' : initglobalswitches:=initglobalswitches-[cs_optimize,cs_maxoptimize,cs_littlesize];
            'a' : initglobalswitches:=initglobalswitches+[cs_optimize];
            'g' : initglobalswitches:=initglobalswitches+[cs_littlesize];
            'G' : initglobalswitches:=initglobalswitches-[cs_littlesize];
            'x' : initglobalswitches:=initglobalswitches+[cs_optimize,cs_maxoptimize];
            'z' : initglobalswitches:=initglobalswitches+[cs_optimize,cs_uncertainopts];
            '2' : initoptprocessor:=pentium2;
            '3' : initoptprocessor:=int386;
            '4' : initoptprocessor:=int486;
            '5' : initoptprocessor:=pentium;
            '6' : initoptprocessor:=pentiumpro;
            '7' : initoptprocessor:=cx6x86;
            '8' : initoptprocessor:=amdk6
            else IllegalPara(opt);
            end;
          end;
    'R' : begin
            if More='ATT' then
             initasmmode:=I386_ATT
            else
             if More='INTEL' then
              initasmmode:=I386_INTEL
            else
             if More='DIRECT' then
              initasmmode:=I386_DIRECT
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
  Revision 1.9  1998-08-10 14:50:04  peter
    + localswitches, moduleswitches, globalswitches splitting

  Revision 1.8  1998/06/16 08:56:22  peter
    + targetcpu
    * cleaner pmodules for newppu

  Revision 1.7  1998/05/30 14:31:05  peter
    + $ASMMODE

  Revision 1.6  1998/05/28 17:26:48  peter
    * fixed -R switch, it didn't work after my previous akt/init patch
    * fixed bugs 110,130,136

  Revision 1.5  1998/05/23 01:21:14  peter
    + aktasmmode, aktoptprocessor, aktoutputformat
    + smartlink per module $SMARTLINK-/+ (like MMX) and moved to aktswitches
    + $LIBNAME to set the library name where the unit will be put in
    * splitted cgi386 a bit (codeseg to large for bp7)
    * nasm, tasm works again. nasm moved to ag386nsm.pas

  Revision 1.4  1998/05/10 12:07:15  jonas
    + switches for 6x86 and k6 optimizations

  Revision 1.3  1998/04/29 10:33:55  pierre
    + added some code for ansistring (not complete nor working yet)
    * corrected operator overloading
    * corrected nasm output
    + started inline procedures
    + added starstarn : use ** for exponentiation (^ gave problems)
    + started UseTokenInfo cond to get accurate positions

  Revision 1.1.1.1  1998/03/25 11:18:14  root
  * Restored version

  Revision 1.17  1998/03/10 01:17:21  peter
    * all files have the same header
    * messages are fully implemented, EXTDEBUG uses Comment()
    + AG... files for the Assembler generation

  Revision 1.16  1998/03/06 01:09:00  peter
    * removed the conflicts that had occured

  Revision 1.15  1998/03/06 00:52:30  peter
    * replaced all old messages from errore.msg, only ExtDebug and some
      Comment() calls are left
    * fixed options.pas

  Revision 1.14  1998/03/05 22:41:52  florian
    + missing constructor to options object added

  Revision 1.13  1998/03/05 02:44:14  peter
    * options cleanup and use of .msg file

  Revision 1.12  1998/03/04 17:33:47  michael
  + Changed ifdef FPK to ifdef FPC

  Revision 1.11  1998/03/02 21:21:39  jonas
    + added support for uncertain optimizations

  Revision 1.10  1998/03/02 01:48:47  peter
    * renamed target_DOS to target_GO32V1
    + new verbose system, merged old errors and verbose units into one new
      verbose.pas, so errors.pas is obsolete

  Revision 1.9  1998/02/22 23:03:20  peter
    * renamed msource->mainsource and name->unitname
    * optimized filename handling, filename is not seperate anymore with
      path+name+ext, this saves stackspace and a lot of fsplit()'s
    * recompiling of some units in libraries fixed
    * shared libraries are working again
    + $LINKLIB <lib> to support automatic linking to libraries
    + libraries are saved/read from the ppufile, also allows more libraries
      per ppufile

  Revision 1.8  1998/02/13 10:35:12  daniel
  * Made Motorola version compilable.
  * Fixed optimizer

  Revision 1.7  1998/02/12 11:50:15  daniel
  Yes! Finally! After three retries, my patch!

  Changes:

  Complete rewrite of psub.pas.
  Added support for DLL's.
  Compiler requires less memory.
  Platform units for each platform.

  Revision 1.6  1998/02/08 23:56:55  peter
    + -O- to turn off optimizes

  Revision 1.5  1998/01/23 17:12:14  pierre
    * added some improvements for as and ld :
      - doserror and dosexitcode treated separately
      - PATH searched if doserror=2
    + start of long and ansi string (far from complete)
      in conditionnal UseLongString and UseAnsiString
    * options.pas cleaned (some variables shifted to globals)gl

  Revision 1.4  1998/01/07 00:16:54  michael
  Restored released version (plus fixes) as current

  Revision 1.2  1997/12/15 09:11:29  florian
    + again opts386.pas commited (there was an error)

  Revision 1.1.1.1  1997/11/27 08:32:57  michael
  FPC Compiler CVS start


  Pre-CVS log:

  CEC   Carl-Eric Codere
  FK    Florian Klaempfl
  PM    Pierre Muller
  +     feature added
  -     removed
  *     bug fixed or changed

  History:
       8th october 1997:
         * started from options.pas (FK)
       23th november 1997:
         + added -R option for different assembler reading style (PM)
}
