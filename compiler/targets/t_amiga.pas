{
    $Id$
    Copyright (c) 2001-2002 by Peter Vreman

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
unit t_amiga;

{$i fpcdefs.inc}

interface


implementation

    uses
       link,
       cutils,cclasses,
       globtype,globals,systems,verbose,script,fmodule;

{*****************************************************************************
                                     Initialize
*****************************************************************************}

    const
       target_m68k_amiga_info : ttargetinfo =
          (
            target       : target_m68k_Amiga;
            name         : 'Commodore Amiga';
            shortname    : 'amiga';
            flags        : [];
            cpu          : cpu_m68k;
            short_name   : 'AMIGA';
            unit_env     : '';
            extradefines : '';
            sharedlibext : '.library';
            staticlibext : '.a';
            sourceext    : '.pp';
            pasext       : '.pas';
            exeext       : '';
            defext       : '';
            scriptext    : '';
            smartext     : '.sl';
            unitext      : '.ppa';
            unitlibext   : '.ppl';
            asmext       : '.asm';
            objext       : '.o';
            resext       : '.res';
            resobjext    : '.or';
            staticlibprefix : '';
            sharedlibprefix : '';
            Cprefix      : '_';
            newline      : #10;
            dirsep       : '/';
            files_case_relevent : true;
            assem        : as_m68k_as;
            assemextern  : as_m68k_as;
            link         : ld_m68k_amiga;
            linkextern   : ld_m68k_amiga;
            ar           : ar_m68k_ar;
            res          : res_none;
            script       : script_amiga;
            endian       : endian_big;
            stackalignment : 2;
            maxCrecordalignment : 4;
            heapsize     : 128*1024;
            stacksize    : 8192;
            DllScanSupported:false;
            use_function_relative_addresses : false
          );


initialization
  RegisterTarget(target_m68k_amiga_info);
end.
{
  $Log$
  Revision 1.11  2002-05-18 13:34:26  peter
    * readded missing revisions

  Revision 1.10  2002/05/16 19:46:53  carl
  + defines.inc -> fpcdefs.inc to avoid conflicts if compiling by hand
  + try to fix temp allocation (still in ifdef)
  + generic constructor calls
  + start of tassembler / tmodulebase class cleanup

  Revision 1.8  2002/04/22 18:19:22  carl
  - remove use_bound_instruction field

  Revision 1.7  2002/04/20 21:43:18  carl
  * fix stack size for some targets
  + add offset to parameters from frame pointer info.
  - remove some unused stuff

  Revision 1.6  2002/04/15 19:16:57  carl
  - remove size_of_pointer field

}
