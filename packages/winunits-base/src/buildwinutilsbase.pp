{
   Dummy unit to compile everything in one go

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2 of the License, or (at your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the
   Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
   Boston, MA 02110-1301, USA.
}
{$IFNDEF FPC_DOTTEDUNITS}
unit buildwinutilsbase;
{$ENDIF FPC_DOTTEDUNITS}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
    WinApi.Flatsb, WinApi.Winver, WinApi.Mmsystem, WinApi.Comconst, WinApi.Commctrl, WinApi.Comobj, WinApi.Commdlg,
    WinApi.Ole2, WinApi.Activex, WinApi.Shellapi, WinApi.Shlobj, WinApi.Oleserver,  WinApi.Shfolder, WinApi.Richedit,
    WinApi.Imagehlp, WinApi.Wininet, WinApi.Uxtheme, WinApi.Dwmapi, WinApi.Multimon, WinApi.Htmlhelp, WinApi.Winutils,
    WinApi.Comserv, WinApi.Winspool, WinApi.Imm, WinApi.ImmDyn, WinApi.Nb30, WinApi.Stdole2,
    WinApi.Eventsink, WinApi.Typelib, WinApi.Libkinect10, WinApi.Urlmon, WinApi.Winhttp,
    WinApi.Shlwapi, WinApi.Httpapi;
{$ELSE FPC_DOTTEDUNITS}
uses
    flatsb, winver, mmsystem, comconst, commctrl, comobj, commdlg,
    ole2, activex, shellapi, shlobj, oleserver,  shfolder, richedit,
    imagehlp, wininet, uxtheme, dwmapi, multimon, htmlhelp, winutils,
    comserv, winspool, imm, imm_dyn, nb30, stdole2,
    eventsink, typelib, libkinect10, urlmon, winhttp,
    shlwapi, httpapi;
{$ENDIF FPC_DOTTEDUNITS}

implementation

end.
