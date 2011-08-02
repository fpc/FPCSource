{
    Copyright (c) 2011 by Sven Barth

    This unit implements support import,export,link routines
    for the Symobi Target

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
unit t_symobi;

{$i fpcdefs.inc}

interface


implementation

  uses
    SysUtils,
    cutils,
    ogbase,ogcoff,
    globtype,globals,systems,verbose,
    import,export,link,t_win,i_symobi,rescmn,comprsrc;


    type
      TImportLibSymobi=class(TImportLibWin)
      end;

      TExportLibSymobi=class(TExportLibWin)
      end;

      TInternalLinkerSymobi = class(TInternalLinkerWin)
        constructor create;override;
        procedure ConcatEntryName; override;
		function MakeExecutable:boolean;override;
      end;
	  
{****************************************************************************
                            TInternalLinkerSymobi
****************************************************************************}

    constructor TInternalLinkerSymobi.create;
      begin
        inherited create;
        CExeoutput:=TPECoffexeoutput;
        CObjInput:=TPECoffObjInput;
      end;

    procedure TInternalLinkerSymobi.ConcatEntryName;
      begin
        with LinkScript do
          begin
            if IsSharedLibrary then
			  { TODO : verify this }
              Concat('ENTRYNAME _DLLMainCRTStartup')
            else
			  { we need to use the startupname defined in the helper }
              Concat('ENTRYNAME FPC_SYSTEMMAIN');
          end;
      end;
	  
	function TInternalLinkerSymobi.MakeExecutable:boolean;
	  begin
	    result:=inherited MakeExecutable;
	  end;

	  
{*****************************************************************************
                                     Initialize
*****************************************************************************}

initialization
{$ifdef i386}
  { Symobi }
  RegisterInternalLinker(system_i386_symobi_info,TInternalLinkerSymobi);
  RegisterImport(system_i386_symobi,TImportLibSymobi);
  RegisterExport(system_i386_symobi,TExportLibSymobi);
  RegisterRes(res_ext_info,TWinLikeResourceFile);
  RegisterTarget(system_i386_symobi_info);
{$endif i386}
end.
