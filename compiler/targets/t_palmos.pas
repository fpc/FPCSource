{
    $Id$
    Copyright (c) 2001 by Peter Vreman

    This unit implements support import,export,link routines
    for the (i386) Amiga target

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
unit t_palmos;

{$i defines.inc}

interface


implementation

    uses
       link,
       cutils,cclasses,
       globtype,globals,systems,verbose,script,fmodule;

{*****************************************************************************
                                     Initialize
*****************************************************************************}

{$ifdef m68k}
    const
       target_m68k_palmos_info : ttargetinfo =
          (
            target       : target_m68k_PalmOS;
            name         : 'PalmOS';
            shortname    : 'palmos';
            flags        : [];
            cpu          : m68k;
            short_name   : 'PALMOS';
            unit_env     : 'PALMUNITS';
            sharedlibext : '.so';
            staticlibext : '.a';
            sourceext    : '.pp';
            pasext       : '.pas';
            exeext       : '';
            defext       : '';
            scriptext    : '.sh';
            smartext     : '.sl';
            unitext      : '.ppu';
            unitlibext   : '.ppl';
            asmext       : '.s';
            objext       : '.o';
            resext       : '.res';
            resobjext    : '.or';
            libprefix    : 'libp';
            Cprefix      : '_';
            newline      : #10;
            assem        : as_m68k_as;
            assemextern  : as_m68k_as;
            link         : ld_m68k_palmos;
            linkextern   : ld_m68k_palmos;
            ar           : ar_m68k_ar;
            res          : res_none;
            endian       : endian_big;
            stackalignment : 2;
            maxCrecordalignment : 32;
            size_of_pointer : 4;
            size_of_longint : 4;
            heapsize     : 128*1024;
            maxheapsize  : 32768*1024;
            stacksize    : 8192;
            DllScanSupported:false;
            use_bound_instruction : false;
            use_function_relative_addresses : false
          );
{$endif m68k}

initialization
{$ifdef m68k}
  RegisterTarget(target_m68k_palmos_info);
{$endif m68k}
end.
{
  $Log$
  Revision 1.1  2001-04-18 22:02:04  peter
    * registration of targets and assemblers

}
