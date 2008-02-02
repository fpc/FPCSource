{
    This file is part of the Free Pascal run time library.

    A file in Amiga system run time library.
    Copyright (c) 1998 by Nils Sjoholm
    member of the Amiga RTL development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit gameport;

INTERFACE

uses exec;

const

{*****   GamePort commands *****}

    GPD_READEVENT       = CMD_NONSTD + 0;
    GPD_ASKCTYPE        = CMD_NONSTD + 1;
    GPD_SETCTYPE        = CMD_NONSTD + 2;
    GPD_ASKTRIGGER      = CMD_NONSTD + 3;
    GPD_SETTRIGGER      = CMD_NONSTD + 4;

{*****   GamePort structures *****}

{ gpt_Keys }

    GPTB_DOWNKEYS       = 0;
    GPTF_DOWNKEYS       = 1;
    GPTB_UPKEYS         = 1;
    GPTF_UPKEYS         = 2;

type

    pGamePortTrigger = ^tGamePortTrigger;
    tGamePortTrigger = record
        gpt_Keys        : Word;        { key transition triggers }
        gpt_Timeout     : Word;        { time trigger (vertical blank units) }
        gpt_XDelta      : Word;        { X distance trigger }
        gpt_YDelta      : Word;        { Y distance trigger }
    end;


const

{***** Controller Types *****}

    GPCT_ALLOCATED      = -1;   { allocated by another user }
    GPCT_NOCONTROLLER   = 0;

    GPCT_MOUSE          = 1;
    GPCT_RELJOYSTICK    = 2;
    GPCT_ABSJOYSTICK    = 3;


{***** Errors *****}

    GPDERR_SETCTYPE     = 1;    { this controller not valid at this time }

IMPLEMENTATION

end.
