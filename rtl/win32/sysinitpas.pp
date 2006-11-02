{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2006 by Florian Klaempfl and Pavel Ozerski
    member of the Free Pascal development team.

    Win32 pascal only startup code

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit sysinitpas;

  interface

  implementation

    var
      SysInstance : Longint;external name '_FPC_SysInstance';

    procedure EXE_Entry; external name '_FPC_EXE_Entry';
    function DLL_entry : longbool; external name '_FPC_DLL_Entry';


    procedure _FPC_mainCRTStartup;stdcall;public name '_mainCRTStartup';
    begin
      IsConsole:=true;
      Exe_entry;
    end;


    procedure _FPC_WinMainCRTStartup;stdcall;public name '_WinMainCRTStartup';
    begin
      IsConsole:=false;
      Exe_entry;
    end;


    procedure _FPC_DLLMainCRTStartup(_hinstance,_dllreason,_dllparam:longint);stdcall;public name '_DLLMainCRTStartup';
    begin
      IsConsole:=true;
      sysinstance:=_hinstance;
      dllreason:=_dllreason;
      dllparam:=_dllparam;
      DLL_Entry;
    end;


    procedure _FPC_DLLWinMainCRTStartup(_hinstance,_dllreason,_dllparam:longint);stdcall;public name '_DLLWinMainCRTStartup';
    begin
      IsConsole:=false;
      sysinstance:=_hinstance;
      dllreason:=_dllreason;
      dllparam:=_dllparam;
      DLL_Entry;
    end;

end.
