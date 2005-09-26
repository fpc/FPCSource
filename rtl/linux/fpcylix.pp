{
    Kylix compatibility unit
    The stuff in this unit might be rather linux or even
    i386-linux centric and even contain windows like stuff.

    This file is part of the Free Pascal run time library.
    Copyright (c) 2005 by Florian Klaempfl

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY;without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$mode objfpc}
unit fpcylix;

  interface

    uses
      cthreads,cwstring,dynlibs;
      
    const
      MAX_PATH = 4095;

    var
      MainInstance: PtrUInt;

    function GetModuleName(Module: HMODULE): string;
    function GetModuleHandle(ModuleName: PChar): HMODULE;
    function GetModuleFileName(Module: HMODULE; Buffer: PChar; BufLen: Integer): Integer;

  implementation

    function GetModuleName(Module: HMODULE): string;
      begin
        result:='';
      end;


    function GetModuleHandle(ModuleName: PChar): HMODULE;
      begin
        result:=NilHandle;
      end;


    function GetModuleFileName(Module: HMODULE; Buffer: PChar; BufLen: Integer): Integer;
      begin
        result:=0;
      end;

end.
