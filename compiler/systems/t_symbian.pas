{
    Copyright (c) 1998-2002 by Peter Vreman

    This unit implements support import,export,link routines
    for the (i386) Win32 target

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
unit t_symbian;

{$i fpcdefs.inc}

interface

    uses
       cutils,cclasses,
       aasmbase,aasmtai,aasmdata,aasmcpu,fmodule,globtype,globals,systems,verbose,
       symconst,symdef,symsym,
       script,gendef,
       cpubase,
       import,export,link,cgobj,t_win, i_symbian;

implementation

  uses
    SysUtils,
    cfileutils,
    cpuinfo,cgutils,dbgbase,
    owar,ogbase,ogcoff;

{*****************************************************************************
                                     Initialize
*****************************************************************************}

initialization
{$ifdef i386}
  RegisterExternalLinker(system_i386_symbian_info,TExternalLinkerWin);
  RegisterInternalLinker(system_i386_symbian_info,TInternalLinkerWin);
  RegisterImport(system_i386_symbian,TImportLibWin);
  RegisterExport(system_i386_symbian,TExportLibWin);
  RegisterDLLScanner(system_i386_symbian,TDLLScannerWin);
//  RegisterRes(res_gnu_windres_info);
  RegisterTarget(system_i386_symbian_info);
{$endif i386}
end.
