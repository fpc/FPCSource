{
    $Id$
    Copyright (c) 2001-2002 Pavel ??????

    This unit implements support import,export,link routines
    for the (i386) WDOSX target

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
unit t_wdosx;

{$i fpcdefs.inc}

interface


implementation

    uses
       cutils,
       fmodule,globals,systems,
       import,export,link,t_win32,i_wdosx;

  type
    timportlibwdosx=class(timportlibwin32)
      procedure GetDefExt(var N:longint;var P:pStr4);override;
    end;

    texportlibwdosx=texportlibwin32;

    tlinkerwdosx=class(tlinkerwin32)
    public
       function  MakeExecutable:boolean;override;
    end;

    tDLLScannerWdosx=class(tDLLScannerWin32)
    public
      procedure GetDefExt(var N:longint;var P:pStr4);override;
    end;

const
 DefaultDLLExtensions:array[1..2]of string[4]=('.WDL','.DLL');


{*****************************************************************************
                             TIMPORTLIBWDOSX
*****************************************************************************}
    procedure timportlibwdosx.GetDefExt(var N:longint;var P:pStr4);
     begin
      N:=sizeof(DefaultDLLExtensions)div sizeof(DefaultDLLExtensions[1]);
      pointer(P):=@DefaultDLLExtensions;
     end;

{*****************************************************************************
                             TLINKERWDOSX
*****************************************************************************}
function TLinkerWdosx.MakeExecutable:boolean;
begin
 Result:=inherited;
 if Result then
  DoExec(FindUtil('stubit'),current_module.exefilename^,false,false);
end;

{****************************************************************************
                            TDLLScannerWdosx
****************************************************************************}
    procedure tDLLScannerWdosx.GetDefExt(var N:longint;var P:pStr4);
     begin
      N:=sizeof(DefaultDLLExtensions)div sizeof(DefaultDLLExtensions[1]);
      pointer(P):=@DefaultDLLExtensions;
     end;

{*****************************************************************************
                                     Initialize
*****************************************************************************}

initialization
  RegisterExternalLinker(system_i386_wdosx_info,TLinkerWdosx);
  RegisterImport(system_i386_wdosx,TImportLibWdosx);
  RegisterExport(system_i386_wdosx,TExportLibWdosx);
  RegisterDLLScanner(system_i386_wdosx,TDLLScannerWdosx);
    {RegisterAr(ar_gnu_arw_info);}
    {RegisterRes(res_gnu_windres_info);}
  RegisterTarget(system_i386_wdosx_info);
end.

{
  $Log$
  Revision 1.10  2002-08-12 15:08:44  carl
    + stab register indexes for powerpc (moved from gdb to cpubase)
    + tprocessor enumeration moved to cpuinfo
    + linker in target_info is now a class
    * many many updates for m68k (will soon start to compile)
    - removed some ifdef or correct them for correct cpu

  Revision 1.9  2002/07/26 21:15:46  florian
    * rewrote the system handling

  Revision 1.8  2002/05/18 13:34:27  peter
    * readded missing revisions

  Revision 1.7  2002/05/16 19:46:53  carl
  + defines.inc -> fpcdefs.inc to avoid conflicts if compiling by hand
  + try to fix temp allocation (still in ifdef)
  + generic constructor calls
  + start of tassembler / tmodulebase class cleanup

  Revision 1.5  2002/05/12 16:53:18  peter
    * moved entry and exitcode to ncgutil and cgobj
    * foreach gets extra argument for passing local data to the
      iterator function
    * -CR checks also class typecasts at runtime by changing them
      into as
    * fixed compiler to cycle with the -CR option
    * fixed stabs with elf writer, finally the global variables can
      be watched
    * removed a lot of routines from cga unit and replaced them by
      calls to cgobj
    * u32bit-s32bit updates for and,or,xor nodes. When one element is
      u32bit then the other is typecasted also to u32bit without giving
      a rangecheck warning/error.
    * fixed pascal calling method with reversing also the high tree in
      the parast, detected by tcalcst3 test

  Revision 1.4  2002/04/22 18:19:22  carl
  - remove use_bound_instruction field

  Revision 1.3  2002/04/20 21:43:18  carl
  * fix stack size for some targets
  + add offset to parameters from frame pointer info.
  - remove some unused stuff

  Revision 1.2  2002/04/15 19:16:57  carl
  - remove size_of_pointer field

  Revision 1.1  2002/04/04 18:09:49  carl
  + added wdosx patch from Pavel

}
