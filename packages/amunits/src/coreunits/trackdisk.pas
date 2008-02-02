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

unit trackdisk;

INTERFACE

uses exec;

{
 *--------------------------------------------------------------------
 *
 * Physical drive constants
 *
 *--------------------------------------------------------------------
}

Const

    NUMSECS     = 11;
    NUMUNITS    = 4;

{
 *--------------------------------------------------------------------
 *
 * Useful constants
 *
 *--------------------------------------------------------------------
 }

{-- sizes before mfm encoding }
    TD_SECTOR   = 512;
    TD_SECSHIFT = 9;            { log TD_SECTOR }

{
 *--------------------------------------------------------------------
 *
 * Driver Specific Commands
 *
 *--------------------------------------------------------------------
 }

{
 *-- TD_NAME is a generic macro to get the name of the driver.  This
 *-- way if the name is ever changed you will pick up the change
 *-- automatically.
 *--
 *-- Normal usage would be:
 *--
 *-- char internalName[] = TD_NAME;
 *--
 }

    TD_NAME     : PChar = 'trackdisk.device';

    TDF_EXTCOM  = $00010000;            { for internal use only! }


    TD_MOTOR            = CMD_NONSTD + 0;       { control the disk's motor }
    TD_SEEK             = CMD_NONSTD + 1;       { explicit seek (for testing) }
    TD_FORMAT           = CMD_NONSTD + 2;       { format disk }
    TD_REMOVE           = CMD_NONSTD + 3;       { notify when disk changes }
    TD_CHANGENUM        = CMD_NONSTD + 4;       { number of disk changes }
    TD_CHANGESTATE      = CMD_NONSTD + 5;       { is there a disk in the drive? }
    TD_PROTSTATUS       = CMD_NONSTD + 6;       { is the disk write protected? }
    TD_RAWREAD          = CMD_NONSTD + 7;       { read raw bits from the disk }
    TD_RAWWRITE         = CMD_NONSTD + 8;       { write raw bits to the disk }
    TD_GETDRIVETYPE     = CMD_NONSTD + 9;       { get the type of the disk drive }
    TD_GETNUMTRACKS     = CMD_NONSTD + 10;      { # of tracks for this type drive }
    TD_ADDCHANGEINT     = CMD_NONSTD + 11;      { TD_REMOVE done right }
    TD_REMCHANGEINT     = CMD_NONSTD + 12;      { remove softint set by ADDCHANGEINT }
    TD_GETGEOMETRY      = CMD_NONSTD + 13;
    TD_EJECT            = CMD_NONSTD + 14;      { for those drives that support it }
    TD_LASTCOMM         = CMD_NONSTD + 15;

{
 *
 * The disk driver has an "extended command" facility.  These commands
 * take a superset of the normal IO Request block.
 *
 }

    ETD_WRITE           = CMD_WRITE + TDF_EXTCOM;
    ETD_READ            = CMD_READ + TDF_EXTCOM;
    ETD_MOTOR           = TD_MOTOR + TDF_EXTCOM;
    ETD_SEEK            = TD_SEEK + TDF_EXTCOM;
    ETD_FORMAT          = TD_FORMAT + TDF_EXTCOM;
    ETD_UPDATE          = CMD_UPDATE + TDF_EXTCOM;
    ETD_CLEAR           = CMD_CLEAR + TDF_EXTCOM;
    ETD_RAWREAD         = TD_RAWREAD + TDF_EXTCOM;
    ETD_RAWWRITE        = TD_RAWWRITE + TDF_EXTCOM;

{
 *
 * extended IO has a larger than normal io request block.
 *
 }

Type

    pIOExtTD = ^tIOExtTD;
    tIOExtTD = record
        iotd_Req        : tIOStdReq;
        iotd_Count      : ULONG;
        iotd_SecLabel   : ULONG;
    end;

{
 *  This is the structure returned by TD_DRIVEGEOMETRY
 *  Note that the layout can be defined three ways:
 *
 *  1. TotalSectors
 *  2. Cylinders and CylSectors
 *  3. Cylinders, Heads, and TrackSectors.
 *
 *  #1 is most accurate, #2 is less so, and #3 is least accurate.  All
 *  are usable, though #2 and #3 may waste some portion of the available
 *  space on some drives.
 }
       pDriveGeometry = ^tDriveGeometry;
       tDriveGeometry = record
        dg_SectorSize,          {    in bytes }
        dg_TotalSectors,        {    total # of sectors on drive }
        dg_Cylinders,           {    number of cylinders }
        dg_CylSectors,          {    number of sectors/cylinder }
        dg_Heads,               {    number of surfaces }
        dg_TrackSectors,        {    number of sectors/track }
        dg_BufMemType : ULONG;          {    preferred buffer memory type }
                                        {    (usually MEMF_PUBLIC) }
        dg_DeviceType,          {    codes as defined in the SCSI-2 spec}
        dg_Flags      : Byte;               {    flags, including removable }
        dg_Reserved   : Word;
       END;


Const
{    device types }
    DG_DIRECT_ACCESS       = 0 ;
    DG_SEQUENTIAL_ACCESS   = 1 ;
    DG_PRINTER             = 2 ;
    DG_PROCESSOR           = 3 ;
    DG_WORM                = 4 ;
    DG_CDROM               = 5 ;
    DG_SCANNER             = 6 ;
    DG_OPTICAL_DISK        = 7 ;
    DG_MEDIUM_CHANGER      = 8 ;
    DG_COMMUNICATION       = 9 ;
    DG_UNKNOWN             = 31;

{    flags }
    DGB_REMOVABLE          = 0;
    DGF_REMOVABLE          = 1;

{
** raw read and write can be synced with the index pulse.  This flag
** in io request's IO_FLAGS field tells the driver that you want this.
}

    IOTDB_INDEXSYNC     = 4;
    IOTDF_INDEXSYNC     = 16;

{
** raw read and write can be synced with a $4489 sync pattern.  This flag
** in io request's IO_FLAGS field tells the driver that you want this.
}
    IOTDB_WORDSYNC = 5;
    IOTDF_WORDSYNC = 32;


{ labels are TD_LABELSIZE bytes per sector }

    TD_LABELSIZE        = 16;

{
** This is a bit in the FLAGS field of OpenDevice.  If it is set, then
** the driver will allow you to open all the disks that the trackdisk
** driver understands.  Otherwise only 3.5" disks will succeed.
}

    TDB_ALLOW_NON_3_5   = 0;
    TDF_ALLOW_NON_3_5   = 1;

{
**  If you set the TDB_ALLOW_NON_3_5 bit in OpenDevice, then you don't
**  know what type of disk you really got.  These defines are for the
**  TD_GETDRIVETYPE command.  In addition, you can find out how many
**  tracks are supported via the TD_GETNUMTRACKS command.
}

    DRIVE3_5            = 1;
    DRIVE5_25           = 2;
    DRIVE3_5_150RPM     = 3;

{
 *--------------------------------------------------------------------
 *
 * Driver error defines
 *
 *--------------------------------------------------------------------
 }

    TDERR_NotSpecified          = 20;   { general catchall }
    TDERR_NoSecHdr              = 21;   { couldn't even find a sector }
    TDERR_BadSecPreamble        = 22;   { sector looked wrong }
    TDERR_BadSecID              = 23;   { ditto }
    TDERR_BadHdrSum             = 24;   { header had incorrect checksum }
    TDERR_BadSecSum             = 25;   { data had incorrect checksum }
    TDERR_TooFewSecs            = 26;   { couldn't find enough sectors }
    TDERR_BadSecHdr             = 27;   { another "sector looked wrong" }
    TDERR_WriteProt             = 28;   { can't write to a protected disk }
    TDERR_DiskChanged           = 29;   { no disk in the drive }
    TDERR_SeekError             = 30;   { couldn't find track 0 }
    TDERR_NoMem                 = 31;   { ran out of memory }
    TDERR_BadUnitNum            = 32;   { asked for a unit > NUMUNITS }
    TDERR_BadDriveType          = 33;   { not a drive that trackdisk groks }
    TDERR_DriveInUse            = 34;   { someone else allocated the drive }
    TDERR_PostReset             = 35;   { user hit reset; awaiting doom }

{
 *--------------------------------------------------------------------
 *
 * public portion of the unit structure
 *
 *--------------------------------------------------------------------
 }

Type

    pTDU_PublicUnit = ^tTDU_PublicUnit;
    tTDU_PublicUnit = record
        tdu_Unit        : tUnit;         { base message port }
        tdu_Comp01Track : Word;         { track for first precomp }
        tdu_Comp10Track : Word;         { track for second precomp }
        tdu_Comp11Track : Word;         { track for third precomp }
        tdu_StepDelay   : ULONG;        { time to wait after stepping }
        tdu_SettleDelay : ULONG;        { time to wait after seeking }
        tdu_RetryCnt    : Byte;         { # of times to retry }
        tdu_PubFlags    : Byte;         {    public flags, see below }
        tdu_CurrTrk     : Word;         {    track the heads are over... }
                                        {    ONLY ACCESS WHILE UNIT IS STOPPED! }
        tdu_CalibrateDelay : ULONG;     {    time to wait after stepping }
                                        {    during a recalibrate }
        tdu_Counter     : ULONG;        {    counter for disk changes... }
                                        {    ONLY ACCESS WHILE UNIT IS STOPPED! }

    end;

CONST
{    flags for tdu_PubFlags }
    TDPB_NOCLICK  =  0;
    TDPF_NOCLICK  =  1;

IMPLEMENTATION

end.
