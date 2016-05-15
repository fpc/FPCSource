{
  This file is part of the Free Pascal run time library.

  A file in Amiga system run time library.
  Copyright (c) 2003 by Nils Sjöholm.
  member of the Amiga RTL development team.

  This is a unit for lucyplay.library

  See the file COPYING.FPC, included in this distribution,
  for details about the copyright.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

**********************************************************************}
{
  History:

  First version of this library.
  16 Jan 2003.

  Changed cardinal > longword.
  Changed startcode for unit.
  12 Feb 2003.

  nils.sjoholm@mailbox.swipnet.se Nils Sjoholm
}

UNIT LUCYPLAY;

INTERFACE
USES Exec;

VAR LucyPlayBase : pLibrary = nil;

const
    LUCYPLAYNAME : PChar = 'lucyplay.library';

  type

     PLucyPlaySample = ^tLucyPlaySample;
     tLucyPlaySample = record
          SampleRate : ULONG;        {  sample frequency                 }
          Size : ULONG;              {  sample size (in bytes)           }
          _Type : ULONG;             {  AHI type (mono/stereo, 8/16bit)  }
          SampleData : pointer;      {  pointer to the sample data       }
       end;

     PLucyPlayJoystick = ^tLucyPlayJoystick;
     tLucyPlayJoystick = record
          Up : UBYTE;        {  0 means FALSE,  1 means TRUE     }
          Down : UBYTE;      {  if (Up == 1) Down = 0;           }
          Left : UBYTE;      {  Don't forget to check for        }
          Right : UBYTE;     {   Up/Down + Left/Right combos!    }
          Red : UBYTE;       {  Standard fire button             }
          Blue : UBYTE;      {  for 2-button sticks/pads         }
          Green : UBYTE;     {  for CD32 compatible pads only    }
          Yellow : UBYTE;    {  for CD32 compatible pads only    }
          Reverse : UBYTE;   {  for CD32 compatible pads only    }
          Forward : UBYTE;   {  for CD32 compatible pads only    }
          Play : UBYTE;      {  for CD32 compatible pads only    }
          Error : UBYTE;     {  hope this is always FALSE ;)     }
       end;

  const
     INVALID_ID =  not (0);

  const
     LUC_JOY_RIGHT = 1;
     LUC_JOY_LEFT = 2;
     LUC_JOY_DOWN = 4;
     LUC_JOY_UP = 8;
     LUC_JOY_PLAY = 16;
     LUC_JOY_REVERSE = 32;
     LUC_JOY_FORWARD = 64;
     LUC_JOY_GREEN = 128;
     LUC_JOY_YELLOW = 256;
     LUC_JOY_RED = 512;
     LUC_JOY_BLUE = 1024;
     LUC_ERR_ALLOCMEM = 1;
     LUC_ERR_CREATEIOREQUEST = 2;
     LUC_ERR_CREATEMSGPORT = 3;
     LUC_ERR_FILETYPE = 4;
     LUC_ERR_INTERNAL = 5;
     LUC_ERR_OPENDEVICE = 6;
     LUC_ERR_OPENFILE = 7;
     LUC_ERR_OPENLIBRARY = 8;
     LUC_ERR_READJOYPORT = 9;
     LUC_ERR_DOIO = 10;

PROCEDURE lucAudioFree(smp : pLucyPlaySample location 'a0'); syscall LucyPlayBase 48;
FUNCTION lucAudioInit : LONGINT; syscall LucyPlayBase 30;
PROCEDURE lucAudioKill; syscall LucyPlayBase 36;
FUNCTION lucAudioLoad(fname : pCHAR location 'a0') : pLucyPlaySample; syscall LucyPlayBase 42;
PROCEDURE lucAudioPlay(smp : pLucyPlaySample location 'a0'); syscall LucyPlayBase 54;
PROCEDURE lucAudioStop; syscall LucyPlayBase 60;
PROCEDURE lucAudioWait; syscall LucyPlayBase 66;
FUNCTION lucBestModeID(w : longword location 'd0'; h : longword location 'd1'; d : longword location 'd2') : longword; syscall LucyPlayBase 96;
FUNCTION lucError : longword; syscall LucyPlayBase 108;
FUNCTION lucJoyInit : pLucyPlayJoystick; syscall LucyPlayBase 72;
FUNCTION lucJoyInitForce : pLucyPlayJoystick; syscall LucyPlayBase 102;
PROCEDURE lucJoyKill(joy : pLucyPlayJoystick location 'a0'); syscall LucyPlayBase 78;
PROCEDURE lucJoyRead(joy : pLucyPlayJoystick location 'a0'); syscall LucyPlayBase 84;
FUNCTION lucJoyReadBool : longword; syscall LucyPlayBase 90;

IMPLEMENTATION

const
    { Change VERSION and LIBVERSION to proper values }
    VERSION : string[2] = '0';
    LIBVERSION : longword = 0;

initialization
  LucyPlayBase := OpenLibrary(LUCYPLAYNAME,LIBVERSION);
finalization
  if Assigned(LucyPlayBase) then
    CloseLibrary(LucyPlayBase);
END. (* UNIT LUCYPLAY *)



