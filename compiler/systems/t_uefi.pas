{
    Copyright (c) 2014 by Olivier Coursière

    This unit implements support import,export,link routines
    for the uefi Target
    Based on Sven Barth's t_nativent

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
unit t_uefi;

{$i fpcdefs.inc}

interface


implementation

  uses
    SysUtils,
    cutils,
    ogbase,ogcoff,
    globtype,globals,systems,verbose,
    import,export,link,t_win,i_uefi;


    type
      TImportLibUEFI=class(TImportLibWin)
      end;

      TExportLibUEFI=class(TExportLibWin)
      end;

      TInternalLinkerUEFI = class(TInternalLinkerWin)
        constructor create;override;
        procedure ConcatEntryName; override;
      end;

{****************************************************************************
                            TInternalLinkerUEFI
****************************************************************************}

    constructor TInternalLinkerUEFI.create;
      begin
        inherited create;
        CExeoutput:=TPECoffexeoutput;
        CObjInput:=TPECoffObjInput;
      end;

    procedure TInternalLinkerUEFI.ConcatEntryName;
      begin
        with LinkScript do
          begin
            if IsSharedLibrary then
              begin
                // for now we use {$apptype native} for kernel mode code
                if apptype=app_native then
                  Concat('ENTRYNAME EFI_MAIN')
                else
                  Concat('ENTRYNAME EFI_MAIN')
              end
                else
                  Concat('ENTRYNAME PASCALMAIN');
          end;
      end;



{*****************************************************************************
                                     Initialize
*****************************************************************************}

initialization
  RegisterLinker(ld_int_uefi,TInternalLinkerUEFI);
{$ifdef i386}
  { UEFI }
  RegisterImport(system_i386_uefi,TImportLibUEFI);
  RegisterExport(system_i386_uefi,TExportLibUEFI);
  RegisterTarget(system_i386_uefi_info);
{$endif i386}
{$ifdef x86_64}
  { UEFI 64}
  RegisterImport(system_x86_64_uefi,TImportLibUEFI);
  RegisterExport(system_x86_64_uefi,TExportLibUEFI);
  RegisterTarget(system_x86_64_uefi_info);
{$endif x86_64}
end.
