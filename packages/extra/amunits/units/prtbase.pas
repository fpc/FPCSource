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

    Update for AmigaOS 3.0.
    Some const and records added and changes
    was made to a few records.
    31 Jan 2003.

    nils.sjoholm@mailbox.swipnet.se Nils Sjoholm
}

{
        printer device data definition
}

unit prtbase;

INTERFACE

uses exec, parallel, serial, amigados, intuition, timer,utility;


Type

    pDeviceData = ^tDeviceData;
    tDeviceData = record
        dd_Device       : tLibrary;     { standard library node }
        dd_Segment      : Pointer;      { A0 when initialized }
        dd_ExecBase     : Pointer;      { A6 for exec }
        dd_CmdVectors   : Pointer;      { command table for device commands }
        dd_CmdBytes     : Pointer;      { bytes describing which command queue }
        dd_NumCommands  : Word;         { the number of commands supported }
    end;

Const
     IOB_QUEUED = 4;
     IOB_CURRENT = 5;
     IOB_SERVICING = 6;
     IOB_DONE = 7;
     IOF_QUEUED = 1 shl IOB_QUEUED;
     IOF_CURRENT = 1 shl IOB_CURRENT;
     IOF_SERVICING = 1 shl IOB_SERVICING;
     IOF_DONE = 1 shl IOB_DONE;
  { pd_Flags  }
     PB_IOR0 = 0;
     PB_IOR1 = 1;
     PB_IOOPENED = 2;
     PB_EXPUNGED = 7;
     PBF_IOR0 = 1 shl PB_IOR0;
     PBF_IOR1 = 1 shl PB_IOR1;
     PBF_IOOPENDED = 1 shl PB_IOOPENED;
     PBF_EXPUNGED = 1 shl PB_EXPUNGED;
  { du_Flags (actually placed in pd_Unit.mp_Node.ln_Pri)  }
     DUB_STOPPED = 0;
     DUF_STOPPED = 1 shl DUB_STOPPED;

     P_OLDSTKSIZE        = $0800;        { stack size for child task }
     P_STKSIZE           = $1000;
     P_BUFSIZE           = 256;          { size of internal buffers for text i/o }
     P_SAFESIZE          = 128;          { safety margin for text output buffer }

{**************************************************************************}

{
        "struct PrinterData" was a very bad concept in the old V1.0 days
        because it is both: the device and the unit.

        Starting with V44 PrinterData may be duplicated for many Units. But all
        new fields that are specific to the Unit  are now part of the new
        "struct PrinterUnit". Don't touch the private fields!

        A note on the function pointers in these data structure definitions:
        unless otherwise specified, all functions expect that their parameters
        are passed on the *stack* rather than in CPU registers. Every parameter
        must be passed a 32 bit long word, i.e. an "UWORD" will use the same
        stack space as an "ULONG".
}

Type
       pPrinterData = ^tPrinterData;
       tPrinterData = record
            pd_Device : tDeviceData;
            pd_Unit : tMsgPort;
            pd_PrinterSegment : BPTR;
            pd_PrinterType : WORD;
            pd_SegmentData : Pointer;
            pd_PrintBuf : Pointer;
            pd_PWrite : Pointer;
            pd_PBothReady : Pointer;
            pd_ior0 : record
                case longint of
                   0 : ( pd_p0 : tIOExtPar );
                   1 : ( pd_s0 : tIOExtSer );
                end;
            pd_ior1 : record
                case longint of
                   0 : ( pd_p1 : tIOExtPar );
                   1 : ( pd_s1 : tIOExtSer );
                end;
            pd_TIOR : tTimeRequest;
            pd_IORPort : tMsgPort;
            pd_TC : tTask;
            pd_OldStk : array[0..(P_OLDSTKSIZE)-1] of BYTE;
            pd_Flags : BYTE;
            pd_pad : BYTE;
            pd_Preferences : tPreferences;
            pd_PWaitEnabled : BYTE;
            pd_Flags1 : BYTE;
            pd_Stk : array[0..(P_STKSIZE)-1] of BYTE;
         end;

Const

{ Printer Class }

    PPCB_GFX            = 0;            { graphics (bit position) }
    PPCF_GFX            = 1;            { graphics (and/or flag) }
    PPCB_COLOR          = 1;            { color (bit position) }
    PPCF_COLOR          = 2;            { color (and/or flag) }

    PPC_BWALPHA         = 0;            { black&white alphanumerics }
    PPC_BWGFX           = 1;            { black&white graphics }
    PPC_COLORALPHA      = 2;            { color alphanumerics }
    PPC_COLORGFX        = 3;            { color graphics }


{ extended PED structure (V44)  }
    PPCB_EXTENDED = 2;
    PPCF_EXTENDED = $4;

{
        Some printer drivers (PrinterPS) do not support
        strip printing. An application has to print a page
        using a single print request or through clever use
        of the PRD_DUMPRPORTTAGS printing callback hook.
}

{ no strip printing, please  }
       PPCB_NOSTRIP = 3;
       PPCF_NOSTRIP = $8;

{ Color Class }

    PCC_BW              = 1;            { black&white only }
    PCC_YMC             = 2;            { yellow/magenta/cyan only }
    PCC_YMC_BW          = 3;            { yellow/magenta/cyan or black&white }
    PCC_YMCB            = 4;            { yellow/magenta/cyan/black }
    PCC_4COLOR          = 4;            { a flag for YMCB and BGRW }
    PCC_ADDITIVE        = 8;            { not ymcb but blue/green/red/white }
    PCC_WB              = 9;            { black&white only, 0 == BLACK }
    PCC_BGR             = 10;           { blue/green/red }
    PCC_BGR_WB          = 11;           { blue/green/red or black&white }
    PCC_BGRW            = 12;           { blue/green/red/white }

{
        The picture must be scanned once for each color component, as the
        printer can only define one color at a time.  ie. If 'PCC_YMC' then
        first pass sends all 'Y' info to printer, second pass sends all 'M'
        info, and third pass sends all C info to printer.  The CalComp
        PlotMaster is an example of this type of printer.
}

    PCC_MULTI_PASS      = $10;          { see explanation above }

Type

    pPrinterExtendedData = ^tPrinterExtendedData;
    tPrinterExtendedData = record
        ped_PrinterName : STRPTR;         { printer name, null terminated }
        ped_Init        : procedure;      { called after LoadSeg }
        ped_Expunge     : procedure;      { called before UnLoadSeg }
        ped_Open        : function:LONG;  { called at OpenDevice }
        ped_Close       : procedure;      { called at CloseDevice }
        ped_PrinterClass : uByte;         { printer class }
        ped_ColorClass  : uByte;          { color class }
        ped_MaxColumns  : uByte;          { number of print columns available }
        ped_NumCharSets : uByte;          { number of character sets }
        ped_NumRows     : uWord;          { number of 'pins' in print head }
        ped_MaxXDots    : ULONG;          { number of dots max in a raster dump }
        ped_MaxYDots    : ULONG;          { number of dots max in a raster dump }
        ped_XDotsInch   : uWord;          { horizontal dot density }
        ped_YDotsInch   : uWord;          { vertical dot density }
        ped_Commands    : Pointer;        { printer text command table }
        ped_DoSpecial   : function:LONG;  { special command handler }
        ped_Render      : function:LONG;  { raster render function }
        ped_TimeoutSecs : Longint;        { good write timeout }

        { the following only exists if the segment version is >= 33 }

        ped_8BitChars   : Pointer;      { conv. strings for the extended font }
        ped_PrintMode   : Longint;      { set if text printed, otherwise 0 }

        { the following only exists if the segment version is >= 34 }
        { ptr to conversion function for all chars }

        ped_ConvFunc    : Pointer;
        {*************************************************************
         *
         * The following only exists if the segment version is >= 44
         * AND PPCB_EXTENDED is set in ped_PrinterClass:
         *
         ************************************************************}

        { Attributes and features }
        ped_TagList : PTagItem;
        { driver specific preferences:
         *
         *      LONG ped_DoPreferences(struct printerIO * ior,
         *                             LONG command);
         }
        ped_DoPreferences : function :LONG;
        { custom error handling:
         *
         *      VOID ped_CallErrHook(struct printerIO * ior,
         *                           struct Hook * hook);
         }
        ped_CallErrHook : procedure ;
    end;

    { The following tags are used to define more printer driver features  }

    const
       PRTA_Dummy = TAG_USER + $50000;
    {                                                                           }
    { V44 features  }
    { LBOOL  }
       PRTA_8BitGuns = PRTA_Dummy + 1;
    { LBOOL  }
       PRTA_ConvertSource = PRTA_Dummy + 2;
    { LBOOL  }
       PRTA_FloydDithering = PRTA_Dummy + 3;
    { LBOOL  }
       PRTA_AntiAlias = PRTA_Dummy + 4;
    { LBOOL  }
       PRTA_ColorCorrection = PRTA_Dummy + 5;
    { LBOOL  }
       PRTA_NoIO = PRTA_Dummy + 6;
    { LBOOL  }
       PRTA_NewColor = PRTA_Dummy + 7;
    { LONG  }
       PRTA_ColorSize = PRTA_Dummy + 8;
    { LBOOL  }
       PRTA_NoScaling = PRTA_Dummy + 9;
    { User interface  }
    { STRPTR    }
       PRTA_DitherNames = PRTA_Dummy + 20;
    { STRPTR    }
       PRTA_ShadingNames = PRTA_Dummy + 21;
    { LBOOL  }
       PRTA_ColorCorrect = PRTA_Dummy + 22;
    { STRPTR    }
       PRTA_DensityInfo = PRTA_Dummy + 23;
    { Hardware page borders  }
    { LONG, inches/1000  }
       PRTA_LeftBorder = PRTA_Dummy + 30;
    { LONG, inches/1000  }
       PRTA_TopBorder = PRTA_Dummy + 31;
    { LBOOL  }
       PRTA_MixBWColor = PRTA_Dummy + 32;
    { Driver Preferences  }
    { LBOOL  }
       PRTA_Preferences = PRTA_Dummy + 40;
type
    pPrinterSegment = ^tprinterSegment;
    tPrinterSegment = record
        ps_NextSegment  : ULONG;        { (actually a BPTR) }
        ps_runAlert     : ULONG;        { MOVEQ #0,D0 : RTS }
        ps_Version      : Word;         { segment version }
        ps_Revision     : Word;         { segment revision }
        ps_PED          : tPrinterExtendedData;  { printer extended data }
    end;

{**************************************************************************}

{
        Driver specific preferences. This structure is device specific: every
        driver must base its preferences structure on this to allow version
        checking etc.

        The application will read/write this structure as an I/O buffer.
}
      PPrtDriverPreferences = ^tPrtDriverPreferences;
       tPrtDriverPreferences = record
            pdp_Version : UWORD;   { PRIVATE! driver specific version }
            { PRIVATE! driver specific id }
            pdp_PrinterID : array[0..31] of UBYTE;
            pdp_PrefName : array[0..(FILENAME_SIZE - 16)-1] of char;
            { size of this structure }
            pdp_Length : ULONG;
         end;

IMPLEMENTATION

end.
