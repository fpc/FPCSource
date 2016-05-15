{
    This file is part of the Free Pascal run time library.

    timer.device definitions (V50) for MorphOS/PowerPC
    Copyright (c) 2002-3 The MorphOS Development Team, All Rights Reserved.

    Free Pascal conversion
    Copyright (c) 2004 Karoly Balogh for Genesi S.a.r.l. <www.genesi.lu>

    AROS conversion
    Copyright (c) 2011 Marcus Sackrow

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}


{ * timer.device definitions
  *********************************************************************
  * }


const  // Checked OK 04.08.2011 ALB
  UNIT_MICROHZ      = 0;
  UNIT_VBLANK       = 1;
  UNIT_ECLOCK       = 2;
  UNIT_WAITUNTIL    = 3;
  UNIT_WAITECLOCK   = 4;

const  // Checked OK 04.08.2011 ALB
  TIMERNAME = 'timer.device';


type  // Checked OK 04.08.2011 ALB
  PTimeVal = ^TTimeVal;
  TTimeVal = record
    tv_secs : DWord;
    tv_micro: DWord;
  end;

type  // Checked OK 04.08.2011 ALB
  PEClockVal = ^TEClockVal;
  TEClockVal = record
    ev_hi: DWord;
    ev_lo: DWord;
  end;

type  // Checked OK 04.08.2011 ALB
  PTimeRequest = ^TTimeRequest;
  TTimeRequest = record
    tr_node: TIORequest;
    tr_time: TTimeVal;
  end;


const  // Checked OK 04.08.2011 ALB
   TR_ADDREQUEST = (CMD_NONSTD + 0);
   TR_GETSYSTIME = (CMD_NONSTD + 1);
   TR_SETSYSTIME = (CMD_NONSTD + 2);



