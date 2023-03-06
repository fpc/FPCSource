{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2022 by Michael van Canneyt and other members of the
    Free Pascal development team

    Registry definitions

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

Type
  HKEY = THandle;
  PHKEY = ^HKEY;
  
{$if defined(windows) and not defined(XMLREG)}

{ Direct mapping to constants in Windows unit }

Const
  HKEY_CLASSES_ROOT     = {$IFDEF FPC_DOTTEDUNITS}WinApi.{$ENDIF}Windows.HKEY_CLASSES_ROOT;
  HKEY_CURRENT_USER     = {$IFDEF FPC_DOTTEDUNITS}WinApi.{$ENDIF}Windows.HKEY_CURRENT_USER;
  HKEY_LOCAL_MACHINE    = {$IFDEF FPC_DOTTEDUNITS}WinApi.{$ENDIF}Windows.HKEY_LOCAL_MACHINE;
  HKEY_USERS            = {$IFDEF FPC_DOTTEDUNITS}WinApi.{$ENDIF}Windows.HKEY_USERS;
  HKEY_PERFORMANCE_DATA = {$IFDEF FPC_DOTTEDUNITS}WinApi.{$ENDIF}Windows.HKEY_PERFORMANCE_DATA;
  HKEY_CURRENT_CONFIG   = {$IFDEF FPC_DOTTEDUNITS}WinApi.{$ENDIF}Windows.HKEY_CURRENT_CONFIG;
  HKEY_DYN_DATA         = {$IFDEF FPC_DOTTEDUNITS}WinApi.{$ENDIF}Windows.HKEY_DYN_DATA;

  KEY_ALL_ACCESS         = {$IFDEF FPC_DOTTEDUNITS}WinApi.{$ENDIF}Windows.KEY_ALL_ACCESS;
  KEY_CREATE_LINK        = {$IFDEF FPC_DOTTEDUNITS}WinApi.{$ENDIF}Windows.KEY_CREATE_LINK;
  KEY_CREATE_SUB_KEY     = {$IFDEF FPC_DOTTEDUNITS}WinApi.{$ENDIF}Windows.KEY_CREATE_SUB_KEY;
  KEY_ENUMERATE_SUB_KEYS = {$IFDEF FPC_DOTTEDUNITS}WinApi.{$ENDIF}Windows.KEY_ENUMERATE_SUB_KEYS;
  KEY_EXECUTE            = {$IFDEF FPC_DOTTEDUNITS}WinApi.{$ENDIF}Windows.KEY_EXECUTE;
  KEY_NOTIFY             = {$IFDEF FPC_DOTTEDUNITS}WinApi.{$ENDIF}Windows.KEY_NOTIFY;
  KEY_QUERY_VALUE        = {$IFDEF FPC_DOTTEDUNITS}WinApi.{$ENDIF}Windows.KEY_QUERY_VALUE;
  KEY_READ               = {$IFDEF FPC_DOTTEDUNITS}WinApi.{$ENDIF}Windows.KEY_READ;
  KEY_SET_VALUE          = {$IFDEF FPC_DOTTEDUNITS}WinApi.{$ENDIF}Windows.KEY_SET_VALUE;
  KEY_WRITE              = {$IFDEF FPC_DOTTEDUNITS}WinApi.{$ENDIF}Windows.KEY_WRITE;
{$ifndef WinCE}
  KEY_WOW64_64KEY        = {$IFDEF FPC_DOTTEDUNITS}WinApi.{$ENDIF}Windows.KEY_WOW64_64KEY;
  KEY_WOW64_32KEY        = {$IFDEF FPC_DOTTEDUNITS}WinApi.{$ENDIF}Windows.KEY_WOW64_32KEY;
{$endif WinCE}
{$else}

Const
  HKEY_CLASSES_ROOT     = HKEY($80000000);
  HKEY_CURRENT_USER     = HKEY($80000001);
  HKEY_LOCAL_MACHINE    = HKEY($80000002);
  HKEY_USERS            = HKEY($80000003);
  HKEY_PERFORMANCE_DATA = HKEY($80000004);
  HKEY_CURRENT_CONFIG   = HKEY($80000005);
  HKEY_DYN_DATA         = HKEY($80000006);

  KEY_ALL_ACCESS         = $F003F;
  KEY_CREATE_LINK        = 32;
  KEY_CREATE_SUB_KEY     = 4;
  KEY_ENUMERATE_SUB_KEYS = 8;
  KEY_EXECUTE            = $20019;
  KEY_NOTIFY             = 16;
  KEY_QUERY_VALUE        = 1;
  KEY_READ               = $20019;
  KEY_SET_VALUE          = 2;
  KEY_WRITE              = $20006;
  KEY_WOW64_64KEY        = $100;
  KEY_WOW64_32KEY        = $200;
  
{$endif windows}
