{
    $Id$
    Copyright (c) 2002 Pavel ??????

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

{$i defines.inc}

interface


implementation

    uses
{$ifdef Delphi}
       dmisc,
{$else Delphi}
       dos,
{$endif Delphi}
       cutils,cclasses,
       aasm,fmodule,globtype,globals,systems,verbose,
       symconst,symsym,
       script,gendef,
       cpubase,cpuasm,
{$ifdef GDB}
       gdb,
{$endif}
       import,export,link,t_win32;

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

    const
       target_i386_wdosx_info : ttargetinfo =
          (
            target       : target_i386_wdosx;
            name         : 'WDOSX DOS extender';
            shortname    : 'WDOSX';
            flags        : [];
            cpu          : cpu_i386;
            unit_env     : 'WDOSXUNITS';
            extradefines : 'MSWINDOWS';
            sourceext    : '.pp';
            pasext       : '.pas';
            exeext       : '.exe';
            defext       : '.def';
            scriptext    : '.bat';
            smartext     : '.sld';
            unitext      : '.ppd';
            unitlibext   : '.ppl';
            asmext       : '.sd';
            objext       : '.od';
            resext       : '.rc';
            resobjext    : '.odr';
            sharedlibext : '.dll';
            staticlibext : '.ad';
            staticlibprefix : 'libp';
            sharedlibprefix : '';
            sharedClibext : '.dll';
            staticClibext : '.a';
            staticClibprefix : 'lib';
            sharedClibprefix : '';
            Cprefix      : '_';
            newline      : #13#10;
            dirsep       : '\';
            files_case_relevent : false;
            assem        : as_i386_pecoffwdosx;
            assemextern  : as_i386_aswdosx;
            link         : ld_i386_wdosx;
            linkextern   : ld_i386_wdosx;
            ar           : ar_gnu_arw;
            res          : res_gnu_windres;
            script       : script_dos;
            endian       : endian_little;
            alignment    :
              (
                procalign       : 4;
                loopalign       : 4;
                jumpalign       : 0;
                constalignmin   : 0;
                constalignmax   : 4;
                varalignmin     : 0;
                varalignmax     : 4;
                localalignmin   : 0;
                localalignmax   : 4;
                paraalign       : 4;
                recordalignmin  : 0;
                recordalignmax  : 2;
                maxCrecordalign : 16
              );
            first_parm_offset : 8;
            heapsize     : 256*1024;
            stacksize    : 32*1024*1024;
            DllScanSupported:true;
            use_function_relative_addresses : true
          );


initialization
  RegisterLinker(ld_i386_wdosx,TLinkerWdosx);
  RegisterImport(target_i386_wdosx,TImportLibWdosx);
  RegisterExport(target_i386_wdosx,TExportLibWdosx);
  RegisterDLLScanner(target_i386_wdosx,TDLLScannerWdosx);
    {RegisterAr(ar_gnu_arw_info);}
    {RegisterRes(res_gnu_windres_info);}
  RegisterTarget(target_i386_wdosx_info);
end.

{
  $Log$
  Revision 1.4  2002-04-22 18:19:22  carl
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