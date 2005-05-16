{
    $Id: t_wdosx.pas,v 1.6 2005/02/14 17:13:10 peter Exp $
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
    end;

    texportlibwdosx=texportlibwin32;

    tlinkerwdosx=class(tlinkerwin32)
    public
       function  MakeExecutable:boolean;override;
    end;

    tDLLScannerWdosx=class(tDLLScannerWin32)
    end;


{*****************************************************************************
                             TIMPORTLIBWDOSX
*****************************************************************************}

{*****************************************************************************
                             TLINKERWDOSX
*****************************************************************************}
function TLinkerWdosx.MakeExecutable:boolean;
var
 b: boolean;
begin
 b := Inherited MakeExecutable;
 if b then
  DoExec(FindUtil('stubit'),current_module.exefilename^,false,false);
 Result := b;
end;

{****************************************************************************
                            TDLLScannerWdosx
****************************************************************************}

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
  $Log: t_wdosx.pas,v $
  Revision 1.6  2005/02/14 17:13:10  peter
    * truncate log

}
