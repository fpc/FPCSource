{

    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1993,99 by Carl Eric Codere

    This include implements VESA basic access.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}


TYPE

  pModeList = ^tModeList;
  tModeList = Array [0..255] of word; {list of modes terminated by -1}
                                      {VESA modes are >=100h}

  TVESAinfo = packed record  { VESA Information request }
    signature : array [1..4] of char;     { This should be VESA   }
    version   : word;                     { VESA revision         }
    str       : pChar;                    { pointer to OEM string }
    caps      : longint;                  { video capabilities    }
    modeList  : pModeList;                { pointer to SVGA modes }
    pad       : array [18..260] of byte;  { extra padding more then   }
  end;                             { VESA standard because of bugs on }
                                   { some video cards.                }

  TVESAModeInfo = packed record
    attr           : word;             { mode attributes   (1.0)    }
    winAAttr,
    winBAttr       : byte;             { window attributes (1.0)    }
    winGranularity : word;  {in K}     { Window granularity (1.0)   }
    winSize        : word;  {in K}     { window size       (1.0)    }
    winASeg,                           { Window A Segment address (1.0) }
    winBSeg        : word;             { Window B Segment address (1.0) }
    winFunct       : procedure;        { Function to swtich bank    }
    BytesPerScanLine: word;            {bytes per scan line (1.0)   }
    { extended information }
    xRes, yRes : word;    {pixels}
    xCharSize,
    yCharSize  : byte;
    planes     : byte;
    bitsPixel  : byte;
    banks      : byte;
    memModel   : byte;
    bankSize   : byte;  {in K}
    NumberOfPages: byte;

    pad : array [29..260] of byte; { always put some more space then required}
  end;


var
  VESAInfo    : TVESAInfo;         { VESA Driver information  }
  VESAModeInfo    : TVESAModeInfo;     { Current Mode information }
  hasVesa: Boolean;       { true if we have a VESA compatible graphics card}
                          { initialized in QueryAdapterInfo in graph.inc }
{
  $Log$
  Revision 1.1  1999-11-08 11:15:21  peter
    * move graph.inc to the target dir

  Revision 1.2  1999/09/26 13:31:07  jonas
    * changed name of modeinfo variable to vesamodeinfo and fixed
      associated errors (fillchar(modeinfo,sizeof(tmodeinfo),#0) instead
      of sizeof(TVesamodeinfo) etc)
    * changed several sizeof(type) to sizeof(varname) to avoid similar
      errors in the future

  Revision 1.1  1999/09/24 22:52:40  jonas
    * optimized patternline a bit (always use hline when possible)
    * isgraphmode stuff cleanup
    * vesainfo.modelist now gets disposed in cleanmode instead of in
      closegraph (required moving of some declarations from vesa.inc to
      new vesah.inc)
    * queryadapter gets no longer called from initgraph (is called from
      initialization of graph unit)
    * bugfix for notput in 32k and 64k vesa modes
    * a div replaced by / in fillpoly

}