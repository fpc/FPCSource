{
    This file is part of the Free Pascal run time library.

    A file in Amiga system run time library.
    Copyright (c) 1998-2003 by Nils Sjoholm
    member of the Amiga RTL development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{
    History:

    Changed integer > smallint.
    09 Feb 2003.

    nils.sjoholm@mailbox.swipnet.se
}

unit audio;

INTERFACE

uses exec;

const

    AUDIONAME           : PChar = 'audio.device';

    ADHARD_CHANNELS     = 4;

    ADALLOC_MINPREC     = -128;
    ADALLOC_MAXPREC     = 127;

    ADCMD_FREE          = CMD_NONSTD + 0;
    ADCMD_SETPREC       = CMD_NONSTD + 1;
    ADCMD_FINISH        = CMD_NONSTD + 2;
    ADCMD_PERVOL        = CMD_NONSTD + 3;
    ADCMD_LOCK          = CMD_NONSTD + 4;
    ADCMD_WAITCYCLE     = CMD_NONSTD + 5;
    ADCMDB_NOUNIT       = 5;
    ADCMDF_NOUNIT       = 32;
    ADCMD_ALLOCATE      = ADCMDF_NOUNIT + 0;

    ADIOB_PERVOL        = 4;
    ADIOF_PERVOL        = 16;
    ADIOB_SYNCCYCLE     = 5;
    ADIOF_SYNCCYCLE     = 32;
    ADIOB_NOWAIT        = 6;
    ADIOF_NOWAIT        = 64;
    ADIOB_WRITEMESSAGE  = 7;
    ADIOF_WRITEMESSAGE  = 128;

    ADIOERR_NOALLOCATION        = -10;
    ADIOERR_ALLOCFAILED         = -11;
    ADIOERR_CHANNELSTOLEN       = -12;

type
    pIOAudio = ^tIOAudio;
    tIOAudio = record
        ioa_Request     : tIORequest;
        ioa_AllocKey    : smallint;
        ioa_Data        : Pointer;
        ioa_Length      : ULONG;
        ioa_Period      : Word;
        ioa_Volume      : Word;
        ioa_Cycles      : Word;
        ioa_WriteMsg    : tMessage;
    end;

IMPLEMENTATION

end.
