{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2005 Free Pascal development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{  Declarations for comdlg WinCE API

}

{$ifdef read_interface}

//*****************************************************************************
// consts
//*****************************************************************************
const
    ComdlgDLL     = 'commdlg';


//*****************************************************************************
// types
//*****************************************************************************

//*****************************************************************************
// functions
//*****************************************************************************

function ChooseColor(_para1:LPCHOOSECOLOR):WINBOOL; external ComdlgDLL name 'ChooseColor';
function ChooseFont(_para1:LPCHOOSEFONTW):WINBOOL; external ComdlgDLL name 'ChooseFontW';
function ChooseFontW(_para1:LPCHOOSEFONTW):WINBOOL; external ComdlgDLL name 'ChooseFontW';
function CommDlgExtendedError : DWORD; external ComdlgDLL name 'CommDlgExtendedError';
function CommDlg_OpenSave_GetFilePathA(_hdlg:HWND;_psz:LPSTR;_cbmax : longint) : LRESULT;
function CommDlg_OpenSave_GetFilePathW(_hdlg:HWND;_psz:LPWSTR;_cbmax : longint) : LRESULT;
function CommDlg_OpenSave_GetFilePath(_hdlg:HWND;_psz:LPWSTR;_cbmax : longint) : LRESULT;
function CommDlg_OpenSave_GetFolderPath(_hdlg:HWND;_psz:LPWSTR;_cbmax : longint) : LRESULT;
function CommDlg_OpenSave_GetFolderPathA(_hdlg:HWND;_psz:LPSTR;_cbmax : longint) : LRESULT;
function CommDlg_OpenSave_GetFolderPathW(_hdlg:HWND;_psz:LPWSTR;_cbmax : longint) : LRESULT;
function CommDlg_OpenSave_GetFolderIDList(_hdlg:HWND;_pidl:LPVOID;_cbmax : longint) : LRESULT;
function CommDlg_OpenSave_GetSpecA(_hdlg:HWND;_psz:LPSTR;_cbmax : longint) : LRESULT;
function CommDlg_OpenSave_GetSpecW(_hdlg:HWND;_psz:LPWSTR;_cbmax : longint) : LRESULT;
function CommDlg_OpenSave_GetSpec(_hdlg:HWND;_psz:LPWSTR;_cbmax : longint) : LRESULT;
function CommDlg_OpenSave_HideControl(_hdlg:HWND;_id : longint) : LRESULT;
function CommDlg_OpenSave_SetControlText(_hdlg:HWND;_id : longint;_text : LPSTR) : LRESULT;
function CommDlg_OpenSave_SetDefExt(_hdlg:HWND;_pszext : LPSTR) : LRESULT;
function PageSetupDlg(_para1:LPPAGESETUPDLGW):WINBOOL; external ComdlgDLL name 'PageSetupDlgW';
function PageSetupDlgW(_para1:LPPAGESETUPDLGW):WINBOOL; external ComdlgDLL name 'PageSetupDlgW';
function PrintDlg(_para1:LPPRINTDLG):WINBOOL; external ComdlgDLL name 'PrintDlg';


{$endif read_interface}

{$ifdef read_implementation}

function CommDlg_OpenSave_GetSpecA(_hdlg:HWND;_psz:LPSTR;_cbmax : longint) : LRESULT;
begin
  CommDlg_OpenSave_GetSpecA:=SNDMSG(_hdlg,CDM_GETSPEC,WPARAM(_cbmax),LPARAM(_psz));
end;

function CommDlg_OpenSave_GetSpecW(_hdlg:HWND;_psz:LPWSTR;_cbmax : longint) : LRESULT;
begin
  CommDlg_OpenSave_GetSpecW:=SNDMSG(_hdlg,CDM_GETSPEC,WPARAM(_cbmax),LPARAM(_psz));
end;

function CommDlg_OpenSave_GetFilePathA(_hdlg:HWND;_psz:LPSTR;_cbmax : longint) : LRESULT;
begin
  CommDlg_OpenSave_GetFilePathA:=SNDMSG(_hdlg,CDM_GETFILEPATH,WPARAM(_cbmax),LPARAM(_psz));
end;

function CommDlg_OpenSave_GetFilePathW(_hdlg:HWND;_psz:LPWSTR;_cbmax : longint) : LRESULT;
begin
  CommDlg_OpenSave_GetFilePathW:=SNDMSG(_hdlg,CDM_GETFILEPATH,WPARAM(_cbmax),LPARAM(LPWSTR(_psz)));
end;

function CommDlg_OpenSave_GetFolderPathA(_hdlg:HWND;_psz:LPSTR;_cbmax : longint) : LRESULT;
begin
  CommDlg_OpenSave_GetFolderPathA:=SNDMSG(_hdlg,CDM_GETFOLDERPATH,WPARAM(_cbmax),LPARAM(LPSTR(_psz)));
end;

function CommDlg_OpenSave_GetFolderPathW(_hdlg:HWND;_psz:LPWSTR;_cbmax : longint) : LRESULT;
begin
  CommDlg_OpenSave_GetFolderPathW:=SNDMSG(_hdlg,CDM_GETFOLDERPATH,WPARAM(_cbmax),LPARAM(LPWSTR(_psz)));
end;

function CommDlg_OpenSave_GetFolderIDList(_hdlg:HWND;_pidl:LPVOID;_cbmax : longint) : LRESULT;
begin
  CommDlg_OpenSave_GetFolderIDList:=SNDMSG(_hdlg,CDM_GETFOLDERIDLIST,WPARAM(_cbmax),LPARAM(_pidl));
end;

function CommDlg_OpenSave_GetFolderPath(_hdlg:HWND;_psz:LPWSTR;_cbmax : longint) : LRESULT;
begin
  CommDlg_OpenSave_GetFolderPath:=SNDMSG(_hdlg,CDM_GETFOLDERPATH,WPARAM(_cbmax),LPARAM(LPWSTR(_psz)));
end;

function CommDlg_OpenSave_GetFilePath(_hdlg:HWND;_psz:LPWSTR;_cbmax : longint) : LRESULT;
begin
  CommDlg_OpenSave_GetFilePath:=SNDMSG(_hdlg,CDM_GETFILEPATH,WPARAM(_cbmax),LPARAM(_psz));
end;

function CommDlg_OpenSave_GetSpec(_hdlg:HWND;_psz:LPWSTR;_cbmax : longint) : LRESULT;
begin
  CommDlg_OpenSave_GetSpec:=SNDMSG(_hdlg,CDM_GETSPEC,WPARAM(_cbmax),LPARAM(_psz));
end;

function CommDlg_OpenSave_SetControlText(_hdlg:HWND;_id : longint;_text : LPSTR) : LRESULT;
begin
  CommDlg_OpenSave_SetControlText:=SNDMSG(_hdlg,CDM_SETCONTROLTEXT,WPARAM(_id),LPARAM(_text));
end;

function CommDlg_OpenSave_HideControl(_hdlg:HWND;_id : longint) : LRESULT;
begin
  CommDlg_OpenSave_HideControl:=SNDMSG(_hdlg,CDM_HIDECONTROL,WPARAM(_id),0);
end;

function CommDlg_OpenSave_SetDefExt(_hdlg:HWND;_pszext : LPSTR) : LRESULT;
begin
  CommDlg_OpenSave_SetDefExt:=SNDMSG(_hdlg,CDM_SETDEFEXT,0,LPARAM(_pszext));
end;

{$endif read_implementation}


