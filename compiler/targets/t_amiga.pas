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
unit t_amiga;

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

    const
       target_m68k_amiga_info : ttargetinfo =
          (
            target       : target_m68k_Amiga;
            name         : 'Commodore Amiga';
            shortname    : 'amiga';
            flags        : [];
            cpu          : m68k;
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
            size_of_longint : 4;
            heapsize     : 128*1024;
            maxheapsize  : 32768*1024;
            stacksize    : 8192;
            DllScanSupported:false;
            use_bound_instruction : false;
            use_function_relative_addresses : false
          );


initialization
  RegisterTarget(target_m68k_amiga_info);
end.
{
  $Log$
  Revision 1.6  2002-04-15 19:16:57  carl
  - remove size_of_pointer field

  Revision 1.5  2001/09/17 21:29:15  peter
    * merged netbsd, fpu-overflow from fixes branch

  Revision 1.4  2001/08/07 18:47:15  peter
    * merged netbsd start
    * profile for win32

  Revision 1.3  2001/06/03 15:15:31  peter
    * dllprt0 stub for linux shared libs
    * pass -init and -fini for linux shared libs
    * libprefix splitted into staticlibprefix and sharedlibprefix

  Revision 1.2  2001/06/02 19:22:44  peter
    * extradefines field added

  Revision 1.1  2001/04/18 22:02:04  peter
    * registration of targets and assemblers

}
