{
    $Id$
    Copyright (c) 1993-98 by Florian Klaempfl, Pierre Muller

    interprets the commandline options which are m68k specific

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
    }

unit opts68k;
interface

uses
  options;

type
  poption68k=^toption68k;
  toption68k=object(toption)
    procedure interpret_proc_specific_options(const opt:string);virtual;
  end;

implementation

uses
  globals;

procedure toption68k.interpret_proc_specific_options(const opt:string);
var
  j : longint;
begin
  case opt[2] of
   'A' : begin
           if copy(opt,3,length(opt)-2)='o' then
            begin
              output_format:=of_o;
              assem_need_external_list := false;
            end
           else
            if copy(opt,3,length(opt)-2)='m' then
             begin
               output_format:=of_mot;
               assem_need_external_list := true;
             end
           else
            if copy(opt,3,length(opt)-2)='i' then
             begin
               output_format:=of_mit;
               assem_need_external_list := false;
             end
           else
            if copy(opt,3,length(opt)-2)='gas' then
             begin
               output_format:=of_gas;
               assem_need_external_list := false;
             end
           else
            IllegalPara(opt);
         end;
   'O' : begin
           for j:=3 to length(opt) do
            case opt[j] of
             '-' : initswitches:=initswitches-[cs_optimize,cs_maxoptimieren,cs_littlesize];
             'a' : initswitches:=initswitches+[cs_optimize];
             'g' : initswitches:=initswitches+[cs_littlesize];
             'G' : initswitches:=initswitches-[cs_littlesize];
             'x' : initswitches:=initswitches+[cs_optimize,
                    cs_maxoptimieren];
             '2' : opt_processors := MC68020;
             else
              IllegalPara(opt);
             end;
         end;
  else IllegalPara(opt);
  end;
end;

end.
{
  $Log$
  Revision 1.1  1998-03-25 11:18:16  root
  Initial revision

  Revision 1.13  1998/03/13 22:45:58  florian
    * small bug fixes applied

  Revision 1.12  1998/03/10 16:27:39  pierre
    * better line info in stabs debug
    * symtabletype and lexlevel separated into two fields of tsymtable
    + ifdef MAKELIB for direct library output, not complete
    + ifdef CHAINPROCSYMS for overloaded seach across units, not fully
      working
    + ifdef TESTFUNCRET for setting func result in underfunction, not
      working

  Revision 1.11  1998/03/10 01:17:21  peter
    * all files have the same header
    * messages are fully implemented, EXTDEBUG uses Comment()
    + AG... files for the Assembler generation

  Revision 1.10  1998/03/06 00:52:31  peter
    * replaced all old messages from errore.msg, only ExtDebug and some
      Comment() calls are left
    * fixed options.pas

  Revision 1.9  1998/03/05 02:44:15  peter
    * options cleanup and use of .msg file

  Revision 1.8  1998/03/04 17:33:48  michael
  + Changed ifdef FPK to ifdef FPC

  Revision 1.7  1998/03/02 01:48:47  peter
    * renamed target_DOS to target_GO32V1
    + new verbose system, merged old errors and verbose units into one new
      verbose.pas, so errors.pas is obsolete

  Revision 1.6  1998/02/22 23:03:20  peter
    * renamed msource->mainsource and name->unitname
    * optimized filename handling, filename is not seperate anymore with
      path+name+ext, this saves stackspace and a lot of fsplit()'s
    * recompiling of some units in libraries fixed
    * shared libraries are working again
    + $LINKLIB <lib> to support automatic linking to libraries
    + libraries are saved/read from the ppufile, also allows more libraries
      per ppufile

  Revision 1.5  1998/02/21 03:34:27  carl
    + mit asm syntax support

  Revision 1.4  1998/02/13 10:35:12  daniel
  * Made Motorola version compilable.
  * Fixed optimizer

  Revision 1.3  1998/02/12 11:50:15  daniel
  Yes! Finally! After three retries, my patch!

  Changes:

  Complete rewrite of psub.pas.
  Added support for DLL's.
  Compiler requires less memory.
  Platform units for each platform.

  Revision 1.2  1998/01/09 19:22:03  carl
  * externals are now generated as appropriate

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
      4th cotober 1997:
         + copied stuff from opts386.pas and started unit (CEC)
      8th cotober 1997:
         * new command line options management (FK)
}

