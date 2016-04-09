{
  This file is part of the Free Pascal run time library.

  A file in Amiga system run time library.
  Copyright (c) 2003 by Nils Sjöholm.
  member of the Amiga RTL development team.

  This is a unit for preferences.library

  See the file COPYING.FPC, included in this distribution,
  for details about the copyright.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

**********************************************************************}
{
  History:

  First version of this unit.
  17 Jan 2003.

  Changed cardinal > longword.
  Changed startcode for unit.
  12 Feb 2003.

  nils.sjoholm@mailbox.swipnet.se Nils Sjoholm
}

UNIT PREFERENCES;

INTERFACE
USES Exec;

VAR PreferencesBase : pLibrary = nil;

const
    PREFERENCESNAME : PChar = 'preferences.library';


  {
      $VER: preferences.h 39.6 (26.07.2000)

          preferences.library include

      (C) Copyright 2000 Satanic Dreams Software
      All Rights Reserved
   }
  { followed by the data, a total of ps_Size bytes  }

  type
     PPrefsStruct = ^tPrefsStruct;
     tPrefsStruct = record
          ps_Size : WORD;
       end;

  { -- Scalos specific Prefs: }
  { ------------------ ScalosPrefs ------------------------- }
  { Name: "Scalos" or "ScalosPrefs" }
  { ID: "MAIN" }
  { struct [8] }

  const
     SCP_IconOffsets = $80000001;
  { word }
     SCP_IconNormFrame = $80000002;
  { word }
     SCP_IconSelFrame = $80000003;
  { word }
     SCP_IconTextMode = $80000004;
  { word }
     SCP_IconSecLine = $80000005;
  { word }
     SCP_IconTextSkip = $80000006;
  { byte image or image+text }
     SCP_BobsType = $80000010;
  { byte system or custom }
     SCP_BobsMethod = $80000011;
  { byte solid or transparent }
     SCP_BobsTranspMode = $80000012;
  { byte ghosted or real transparent }
     SCP_BobsTranspType = $80000013;
  { long when transparent }
     SCP_BobsTransp = $80000014;
  { string }
     SCP_ScreenTitle = $80000020;
  { string }
     SCP_RootWinTitle = $80000021;
  { string }
     SCP_WindowTitle = $80000022;
  { byte }
     SCP_Separator = $80000023;
  { byte }
     SCP_TitleRefresh = $80000024;
  { string }
     SCP_PathsDefIcons = $80000025;
  { string }
     SCP_PathsDiskCopy = $80000026;
  { string }
     SCP_PathsWBStartup = $80000027;
  { string }
     SCP_PathsHome = $80000028;
  { byte }
     SCP_MiscAutoRemove = $80000029;
  { byte }
     SCP_MiscClickTransp = $8000002a;
  { byte }
     SCP_MiscHardEmulation = $8000002b;
  { byte }
     SCP_MiscUseExAll = $8000002c;
  { byte }
     SCP_MiscWindowType = $8000002d;
  { byte }
     SCP_MiscDoWaitDelay = $8000002e;
  { byte delay }
     SCP_MiscDiskiconsRefresh = $8000002f;
  { byte }
     SCP_MiscMenuCurrentDir = $80000030;
  { byte }
     SCP_NewIconsTransparent = $80000031;
  { long }
     SCP_NewIconsPrecision = $80000032;
  { string }
     SCP_TextModeFont = $80000033;
  { byte }
     SCP_TextModeDateFormat = $80000034;
  { list }
     SCP_PlugInList = $80000035;


FUNCTION AllocPrefsHandle(name : pCHAR location 'a0') : POINTER; syscall PreferencesBase 30;
FUNCTION FindPreferences(PrefsHandle : POINTER location 'a0'; ID : longword location 'd0'; d1arg : longword location 'd1') : pPrefsStruct; syscall PreferencesBase 66;
PROCEDURE FreePrefsHandle(PrefsHandle : POINTER location 'a0'); syscall PreferencesBase 36;
FUNCTION GetEntry(PrefsHandle : POINTER location 'a0'; ID : longword location 'd0'; d1arg : longword location 'd1'; a1arg : POINTER location 'a1'; Struct_Size : WORD location 'd2'; Entry : longword location 'd3') : longword; syscall PreferencesBase 78;
FUNCTION GetPreferences(PrefsHandle : POINTER location 'a0'; ID : longword location 'd0'; d1arg : longword location 'd1'; a1arg : POINTER location 'a1'; Struct_Size : WORD location 'd2') : longword; syscall PreferencesBase 48;
PROCEDURE ReadPrefsHandle(PrefsHandle : POINTER location 'a0'; Filename : pCHAR location 'a1'); syscall PreferencesBase 54;
FUNCTION RemEntry(PrefsHandle : POINTER location 'a0'; ID : longword location 'd0'; d1arg : longword location 'd1'; Entry : longword location 'd2') : longword; syscall PreferencesBase 84;
PROCEDURE SetEntry(PrefsHandle : POINTER location 'a0'; ID : longword location 'd0'; d1arg : longword location 'd1'; a1arg : POINTER location 'a1'; Struct_Size : WORD location 'd2'; Entry : longword location 'd3'); syscall PreferencesBase 72;
PROCEDURE SetPreferences(PrefsHandle : POINTER location 'a0'; ID : longword location 'd0'; d1arg : longword location 'd1'; a1arg : POINTER location 'a1'; Struct_Size : WORD location 'd2'); syscall PreferencesBase 42;
PROCEDURE WritePrefsHandle(PrefsHandle : POINTER location 'a0'; Filename : pCHAR location 'a1'); syscall PreferencesBase 60;

IMPLEMENTATION

const
    { Change VERSION and LIBVERSION to proper values }
    VERSION : string[2] = '0';
    LIBVERSION : longword = 0;

initialization
  PreferencesBase := OpenLibrary(PREFERENCESNAME,LIBVERSION);
finalization
  if Assigned(PreferencesBase) then
    CloseLibrary(PreferencesBase);
END. (* UNIT PREFERENCES *)



