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

{
        printer device data definition
}

unit prtbase;

INTERFACE

uses exec, parallel, serial, amigados, intuition, timer;


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

    P_OLDSTKSIZE        = $0800;        { stack size for child task }
    P_STKSIZE           = $1000;
    P_BUFSIZE           = 256;          { size of internal buffers for text i/o }
    P_SAFESIZE          = 128;          { safety margin for text output buffer }

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
        ped_PrinterName : STRPTR;       { printer name, null terminated }
        ped_Init        : Pointer;      { called after LoadSeg }
        ped_Expunge     : Pointer;      { called before UnLoadSeg }
        ped_Open        : Pointer;      { called at OpenDevice }
        ped_Close       : Pointer;      { called at CloseDevice }
        ped_PrinterClass : Byte;        { printer class }
        ped_ColorClass  : Byte;         { color class }
        ped_MaxColumns  : Byte;         { number of print columns available }
        ped_NumCharSets : Byte;         { number of character sets }
        ped_NumRows     : Word;         { number of 'pins' in print head }
        ped_MaxXDots    : ULONG;        { number of dots max in a raster dump }
        ped_MaxYDots    : ULONG;        { number of dots max in a raster dump }
        ped_XDotsInch   : Word;         { horizontal dot density }
        ped_YDotsInch   : Word;         { vertical dot density }
        ped_Commands    : Pointer;      { printer text command table }
        ped_DoSpecial   : Pointer;      { special command handler }
        ped_Render      : Pointer;      { raster render function }
        ped_TimeoutSecs : Longint;      { good write timeout }

        { the following only exists if the segment version is >= 33 }

        ped_8BitChars   : Pointer;      { conv. strings for the extended font }
        ped_PrintMode   : Longint;      { set if text printed, otherwise 0 }

        { the following only exists if the segment version is >= 34 }
        { ptr to conversion function for all chars }

        ped_ConvFunc    : Pointer;
    end;


    pPrinterSegment = ^tprinterSegment;
    tPrinterSegment = record
        ps_NextSegment  : ULONG;        { (actually a BPTR) }
        ps_runAlert     : ULONG;        { MOVEQ #0,D0 : RTS }
        ps_Version      : Word;         { segment version }
        ps_Revision     : Word;         { segment revision }
        ps_PED          : tPrinterExtendedData;  { printer extended data }
    end;

IMPLEMENTATION

end.


