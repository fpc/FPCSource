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

unit hardblocks;

{
    History:

    Updated for AmigaOs 3.9.
    A few changes in records.
    28 Jan 2003.

    nils.sjoholm@mailbox.swipnet.se Nils Sjoholm
}

INTERFACE

uses exec;

{       Changes
**        Expanded envec
**        Added storage for driveinit name up to 31 letters.
**        Added storage for filesysten name up to 83 letters.
**}


{--------------------------------------------------------------------
 *
 *      This file describes blocks of data that exist on a hard disk
 *      to describe that disk.  They are not generically accessable to
 *      the user as they do not appear on any DOS drive.  The blocks
 *      are tagged with a unique identifier, checksummed, and linked
 *      together.  The root of these blocks is the RigidDiskBlock.
 *
 *      The RigidDiskBlock must exist on the disk within the first
 *      RDB_LOCATION_LIMIT blocks.  This inhibits the use of the zero
 *      cylinder in an AmigaDOS partition: although it is strictly
 *      possible to store the RigidDiskBlock data in the reserved
 *      area of a partition, this practice is discouraged since the
 *      reserved blocks of a partition are overwritten by "Format",
 *      "Install", "DiskCopy", etc.  The recommended disk layout,
 *      then, is to use the first cylinder(s) to store all the drive
 *      data specified by these blocks: i.e. partition descriptions,
 *      file system load images, drive bad block maps, spare blocks,
 *      etc.
 *
 *      Though all descriptions in this file contemplate 512 blocks
 *      per track this desecription works functionally with any block
 *      size. The LSEG blocks should make most efficient use of the
 *      disk block size possible, for example. While this specification
 *      can support 256 byte sectors that is deprecated at this time.
 *
 *      This version adds some modest storage spaces for inserting
 *      the actual source filename for files installed on the RDBs
 *      as either DriveInit code or Filesystem code. This makes
 *      creating a mountfile suitable for use with the "C:Mount"
 *      command that can be used for manually mounting the disk if
 *      ever required.
 *
 *
 *------------------------------------------------------------------}

{
 *  NOTE
 *      optional block addresses below contain $ffffffff to indicate
 *      a NULL address, as zero is a valid address
}

type

    pRigidDiskBlock = ^tRigidDiskBlock;
    tRigidDiskBlock = record
        rdb_ID          : ULONG;      { 4 character identifier }
        rdb_SummedLongs : ULONG;      { size of this checksummed structure }
        rdb_ChkSum      : ULONG;      { block checksum (longword sum to zero) }
        rdb_HostID      : ULONG;      { SCSI Target ID of host }
        rdb_BlockBytes  : ULONG;      { size of disk blocks }
        rdb_Flags       : ULONG;      { see below for defines }

    { block list heads }

        rdb_BadBlockList : ULONG;     { optional bad block list }
        rdb_PartitionList : ULONG;    { optional first partition block }
        rdb_FileSysHeaderList : ULONG; { optional file system header block }
        rdb_DriveInit   : ULONG;      { optional drive-specific init code }

                                { DriveInit(lun,rdb,ior): "C" stk & d0/a0/a1 }

        rdb_Reserved1   : Array [0..5] of ULONG; { set to $ffffffff }

    { physical drive characteristics }

        rdb_Cylinders   : ULONG;      { number of drive cylinders }
        rdb_Sectors     : ULONG;      { sectors per track }
        rdb_Heads       : ULONG;      { number of drive heads }
        rdb_Interleave  : ULONG;      { interleave }
        rdb_Park        : ULONG;      { landing zone cylinder }
        rdb_Reserved2   : Array [0..2] of ULONG;
        rdb_WritePreComp : ULONG;     { starting cylinder: write precompensation }
        rdb_ReducedWrite : ULONG;     { starting cylinder: reduced write current }
        rdb_StepRate    : ULONG;      { drive step rate }
        rdb_Reserved3   : Array [0..4] of ULONG;

    { logical drive characteristics }

        rdb_RDBBlocksLo : ULONG;      { low block of range reserved for hardblocks }
        rdb_RDBBlocksHi : ULONG;      { high block of range for these hardblocks }
        rdb_LoCylinder  : ULONG;      { low cylinder of partitionable disk area }
        rdb_HiCylinder  : ULONG;      { high cylinder of partitionable data area }
        rdb_CylBlocks   : ULONG;      { number of blocks available per cylinder }
        rdb_AutoParkSeconds : ULONG;  { zero for no auto park }
        rdb_Reserved4   : Array [0..1] of ULONG;

    { drive identification }

        rdb_DiskVendor  : Array [0..7] of Char;
        rdb_DiskProduct : Array [0..15] of Char;
        rdb_DiskRevision : Array [0..3] of Char;
        rdb_ControllerVendor : Array [0..7] of Char;
        rdb_ControllerProduct : Array [0..15] of Char;
        rdb_ControllerRevision : Array [0..3] of Char;
        rdb_DriveInitName : array[0..39] of char;
    end;

const
    IDNAME_RIGIDDISK    = $5244534B;   { RDSK }

    RDB_LOCATION_LIMIT  = 16;

    RDBFB_LAST          = 0;    { no disks exist to be configured after }
    RDBFF_LAST          = $01;  {   this one on this controller }
    RDBFB_LASTLUN       = 1;    { no LUNs exist to be configured greater }
    RDBFF_LASTLUN       = $02;  {   than this one at this SCSI Target ID }
    RDBFB_LASTTID       = 2;    { no Target IDs exist to be configured }
    RDBFF_LASTTID       = $04;  {   greater than this one on this SCSI bus }
    RDBFB_NORESELECT    = 3;    { don't bother trying to perform reselection }
    RDBFF_NORESELECT    = $08;  {   when talking to this drive }
    RDBFB_DISKID        = 4;    { rdb_Disk... identification valid }
    RDBFF_DISKID        = $10;
    RDBFB_CTRLRID       = 5;    { rdb_Controller... identification valid }
    RDBFF_CTRLRID       = $20;
                                { added 7/20/89 by commodore: }
    RDBFB_SYNCH         = 6;    { drive supports scsi synchronous mode }
    RDBFF_SYNCH         = $40;  { CAN BE DANGEROUS TO USE IF IT DOESN'T! }

{------------------------------------------------------------------}

type

    pBadBlockEntry = ^tBadBlockEntry;
    tBadBlockEntry = record
        bbe_BadBlock    : ULONG;      { block number of bad block }
        bbe_GoodBlock   : ULONG;      { block number of replacement block }
    end;

    pBadBlockBlock = ^tBadBlockBlock;
    tBadBlockBlock = record
        bbb_ID          : ULONG;      { 4 character identifier }
        bbb_SummedLongs : ULONG;      { size of this checksummed structure }
        bbb_ChkSum      : Longint;      { block checksum (longword sum to zero) }
        bbb_HostID      : ULONG;      { SCSI Target ID of host }
        bbb_Next        : ULONG;      { block number of the next BadBlockBlock }
        bbb_Reserved    : ULONG;
        bbb_BlockPairs  : Array [0..60] of tBadBlockEntry; { bad block entry pairs }
    { note [61] assumes 512 byte blocks }
    end;

const

    IDNAME_BADBLOCK     = $42414442;   { BADB }

{------------------------------------------------------------------}

type

    pPartitionBlock = ^tPartitionBlock;
    tPartitionBlock = record
        pb_ID           : ULONG;      { 4 character identifier }
        pb_SummedLongs  : ULONG;      { size of this checksummed structure }
        pb_ChkSum       : Longint;      { block checksum (longword sum to zero) }
        pb_HostID       : ULONG;      { SCSI Target ID of host }
        pb_Next         : ULONG;      { block number of the next PartitionBlock }
        pb_Flags        : ULONG;      { see below for defines }
        pb_Reserved1    : Array [0..1] of ULONG;
        pb_DevFlags     : ULONG;      { preferred flags for OpenDevice }
        pb_DriveName    : Array [0..31] of Char; { preferred DOS device name: BSTR form }
                                        { (not used if this name is in use) }
        pb_Reserved2    : Array [0..14] of ULONG; { filler to 32 longwords }
        pb_Environment  : Array [0..19] of ULONG; { environment vector for this partition }
        pb_EReserved    : Array [0..11] of ULONG; { reserved for future environment vector }
    end;

const

    IDNAME_PARTITION    = $50415254;    { PART }

    PBFB_BOOTABLE       = 0;    { this partition is intended to be bootable }
    PBFF_BOOTABLE       = 1;    {   (expected directories and files exist) }
    PBFB_NOMOUNT        = 1;    { do not mount this partition (e.g. manually }
    PBFF_NOMOUNT        = 2;    {   mounted, but space reserved here) }

{------------------------------------------------------------------}

type

    pFileSysHeaderBlock = ^tFileSysHeaderBlock;
    tFileSysHeaderBlock = record
        fhb_ID          : ULONG;      { 4 character identifier }
        fhb_SummedLongs : ULONG;      { size of this checksummed structure }
        fhb_ChkSum      : Longint;      { block checksum (longword sum to zero) }
        fhb_HostID      : ULONG;      { SCSI Target ID of host }
        fhb_Next        : ULONG;      { block number of next FileSysHeaderBlock }
        fhb_Flags       : ULONG;      { see below for defines }
        fhb_Reserved1   : Array [0..1] of ULONG;
        fhb_DosType     : ULONG;      { file system description: match this with }
                                { partition environment's DE_DOSTYPE entry }
        fhb_Version     : ULONG;      { release version of this code }
        fhb_PatchFlags  : ULONG;      { bits set for those of the following that }
                                {   need to be substituted into a standard }
                                {   device node for this file system: e.g. }
                                {   0x180 to substitute SegList & GlobalVec }
        fhb_Type        : ULONG;      { device node type: zero }
        fhb_Task        : ULONG;      { standard dos "task" field: zero }
        fhb_Lock        : ULONG;      { not used for devices: zero }
        fhb_Handler     : ULONG;      { filename to loadseg: zero placeholder }
        fhb_StackSize   : ULONG;      { stacksize to use when starting task }
        fhb_Priority    : Longint;      { task priority when starting task }
        fhb_Startup     : Longint;      { startup msg: zero placeholder }
        fhb_SegListBlocks : Longint;    { first of linked list of LoadSegBlocks: }
                                {   note that this entry requires some }
                                {   processing before substitution }
        fhb_GlobalVec   : Longint;      { BCPL global vector when starting task }
        fhb_Reserved2   : Array [0..22] of ULONG; { (those reserved by PatchFlags) }
        fhb_FileSysName : array[0..83] of char;  { File system file name as loaded. }
    end;

const

    IDNAME_FILESYSHEADER        = $46534844;    { FSHD }

{------------------------------------------------------------------}

Type

    pLoadSegBlock = ^tLoadSegBlock;
    tLoadSegBlock = record
        lsb_ID          : ULONG;      { 4 character identifier }
        lsb_SummedLongs : ULONG;      { size of this checksummed structure }
        lsb_ChkSum      : Longint;      { block checksum (longword sum to zero) }
        lsb_HostID      : ULONG;      { SCSI Target ID of host }
        lsb_Next        : ULONG;      { block number of the next LoadSegBlock }
        lsb_LoadData    : Array [0..122] of ULONG;    { data for "loadseg" }
    { note [123] assumes 512 byte blocks }
    end;

const

    IDNAME_LOADSEG      = $4C534547;    { LSEG }

IMPLEMENTATION

end.
