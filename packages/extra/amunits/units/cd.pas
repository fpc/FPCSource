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

unit cd;

INTERFACE

uses exec;
  { Only V40+ }

{*************************************************************************
 *                                                                        *
 *   CD Commands                                                          *
 *                                                                        *
 *************************************************************************}

const
   CD_RESET          =   1;
   CD_READ           =   2;
   CD_WRITE          =   3;
   CD_UPDATE         =   4;
   CD_CLEAR          =   5;
   CD_STOP           =   6;
   CD_START          =   7;
   CD_FLUSH          =   8;
   CD_MOTOR          =   9;
   CD_SEEK           =  10;
   CD_FORMAT         =  11;
   CD_REMOVE         =  12;
   CD_CHANGENUM      =  13;
   CD_CHANGESTATE    =  14;
   CD_PROTSTATUS     =  15;

   CD_GETDRIVETYPE   =  18;
   CD_GETNUMTRACKS   =  19;
   CD_ADDCHANGEINT   =  20;
   CD_REMCHANGEINT   =  21;
   CD_GETGEOMETRY    =  22;
   CD_EJECT          =  23;


   CD_INFO           =  32;
   CD_CONFIG         =  33;
   CD_TOCMSF         =  34;
   CD_TOCLSN         =  35;

   CD_READXL         =  36;

   CD_PLAYTRACK      =  37;
   CD_PLAYMSF        =  38;
   CD_PLAYLSN        =  39;
   CD_PAUSE          =  40;
   CD_SEARCH         =  41;

   CD_QCODEMSF       =  42;
   CD_QCODELSN       =  43;
   CD_ATTENUATE      =  44;

   CD_ADDFRAMEINT    =  45;
   CD_REMFRAMEINT    =  46;


{*************************************************************************
 *                                                                        *
 *   Device Driver Error Codes                                            *
 *                                                                        *
 *************************************************************************}

   CDERR_OPENFAIL     =  (-1); { device/unit failed to open   }
   CDERR_ABORTED      =  (-2); { request terminated early             }
   CDERR_NOCMD        =  (-3); { command not supported by device      }
   CDERR_BADLENGTH    =  (-4); { invalid length (IO_LENGTH/IO_OFFSET) }
   CDERR_BADADDRESS   =  (-5); { invalid address (IO_DATA misaligned) }
   CDERR_UNITBUSY     =  (-6); { device opens ok, but unit is busy    }
   CDERR_SELFTEST     =  (-7); { hardware failed self-test            }

   CDERR_NotSpecified   = 20;   { general catchall                     }
   CDERR_NoSecHdr       = 21;   { couldn't even find a sector          }
   CDERR_BadSecPreamble = 22;   { sector looked wrong                  }
   CDERR_BadSecID       = 23;   { ditto                                }
   CDERR_BadHdrSum      = 24;   { header had incorrect checksum        }
   CDERR_BadSecSum      = 25;   { data had incorrect checksum          }
   CDERR_TooFewSecs     = 26;   { couldn't find enough sectors         }
   CDERR_BadSecHdr      = 27;   { another "sector looked wrong"        }
   CDERR_WriteProt      = 28;   { can't write to a protected disk      }
   CDERR_NoDisk         = 29;   { no disk in the drive                 }
   CDERR_SeekError      = 30;   { couldn't find track 0                }
   CDERR_NoMem          = 31;   { ran out of memory                    }
   CDERR_BadUnitNum     = 32;   { asked for a unit > NUMUNITS          }
   CDERR_BadDriveType   = 33;   { not a drive cd.device understands    }
   CDERR_DriveInUse     = 34;   { someone else allocated the drive     }
   CDERR_PostReset      = 35;   { user hit reset; awaiting doom        }
   CDERR_BadDataType    = 36;   { data on disk is wrong type   }
   CDERR_InvalidState   = 37;   { invalid cmd under current conditions }

   CDERR_Phase          = 42;   { illegal or unexpected SCSI phase     }
   CDERR_NoBoard        = 50;   { open failed for non-existant board   }



{*************************************************************************
 *                                                                        *
 * Configuration                                                          *
 *                                                                        *
 *       The drive is configured by TagList items defined as follows:     *
 *                                                                        *
 *************************************************************************}

   TAGCD_PLAYSPEED        = $0001;
   TAGCD_READSPEED        = $0002;
   TAGCD_READXLSPEED      = $0003;
   TAGCD_SECTORSIZE       = $0004;
   TAGCD_XLECC            = $0005;
   TAGCD_EJECTRESET       = $0006;


{*************************************************************************
 *                                                                        *
 * Information                                                            *
 *                                                                        *
 *      Information/Status structure describes current speed settings     *
 *      for read and play commands, sector size, audio attenuation        *
 *      precision, and drive status.                                      *
 *                                                                        *
 *************************************************************************}

Type

 pCDInfo = ^tCDInfo;
 tCDInfo = record
                                        {                                Default     }
    PlaySpeed,                          { Audio play speed       (75)        }
    ReadSpeed,                          { Data-rate of CD_READ command   (Max)       }
    ReadXLSpeed,                        { Data-rate of CD_READXL command (75)        }
    SectorSize,                         { Number of bytes per sector     (2048)      }
    XLECC,                              { CDXL ECC enabled/disabled                  }
    EjectReset : WORD;                  { Reset on eject enabled/disabled            }
    Reserved1 : Array[0..3] of WORD;    { Reserved for future expansion              }
    MaxSpeed,                           { Maximum speed drive can handle (75, 150)   }
    AudioPrecision,                     { 0 = no attenuator, 1 = mute only,          }
                                        { other = (# levels - 1)                     }
    Status : WORD;                      { See flags below                            }
    Reserved2 : Array[0..3] of WORD;    { Reserved for future expansion              }
 end;


const
{ Flags for Status }

   CDSTSB_CLOSED    = 0; { Drive door is closed                        }
   CDSTSB_DISK      = 1; { A disk has been detected                    }
   CDSTSB_SPIN      = 2; { Disk is spinning (motor is on)              }
   CDSTSB_TOC       = 3; { Table of contents read.  Disk is valid.     }
   CDSTSB_CDROM     = 4; { Track 1 contains CD-ROM data                }
   CDSTSB_PLAYING   = 5; { Audio is playing                            }
   CDSTSB_PAUSED    = 6; { Pause mode (pauses on play command)         }
   CDSTSB_SEARCH    = 7; { Search mode (Fast Forward/Fast Reverse)     }
   CDSTSB_DIRECTION = 8; { Search direction (0 = Forward, 1 = Reverse) }

   CDSTSF_CLOSED    = $0001;
   CDSTSF_DISK      = $0002;
   CDSTSF_SPIN      = $0004;
   CDSTSF_TOC       = $0008;
   CDSTSF_CDROM     = $0010;
   CDSTSF_PLAYING   = $0020;
   CDSTSF_PAUSED    = $0040;
   CDSTSF_SEARCH    = $0080;
   CDSTSF_DIRECTION = $0100;


{ Modes for CD_SEARCH }

   CDMODE_NORMAL  = 0;         { Normal play at current play speed    }
   CDMODE_FFWD    = 1;         { Fast forward play (skip-play forward)}
   CDMODE_FREV    = 2;         { Fast reverse play (skip-play reverse)}


{*************************************************************************
 *                                                                        *
 * Position Information                                           *
 *                                                                        *
 *      Position information can be described in two forms: MSF and LSN   *
 *      form.  MSF (Minutes, Seconds, Frames) form is a time encoding.    *
 *      LSN (Logical Sector Number) form is frame (sector) count.         *
 *      The desired form is selected using the io_Flags field of the      *
 *      IOStdReq structure.  The flags and the union are described        *
 *      below.                                                            *
 *                                                                        *
 *************************************************************************}

Type
 pRMSF = ^tRMSF;
 tRMSF = record
    Reserved,       { Reserved (always zero) }
    Minute,         { Minutes (0-72ish)      }
    Second,         { Seconds (0-59)         }
    Frame  : Byte;  { Frame   (0-74)         }
 end;

 pLSNMSF = ^tLSNMSF;
 tLSNMSF = record
    MSF     : tRMSF;         { Minute, Second, Frame  }
    LSN     : ULONG;         { Logical Sector Number  }
 end;


{*************************************************************************
 *                                                                        *
 * CD Transfer Lists                                                      *
 *                                                                        *
 *      A CDXL node is a double link node; however only single linkage    *
 *      is used by the device driver.  If you wish to construct a         *
 *      transfer list manually, it is only neccessary to define the       *
 *      mln_Succ pointer of the MinNode.  You may also use the Exec       *
 *      list functions by defining a List or MinList structure and by     *
 *      using the AddHead/AddTail functions to create the list.  This     *
 *      will create a double-linked list.  Although a double-linked       *
 *      list is not required by the device driver, you may wish use it    *
 *      for your own purposes.  Don't forget to initialize the            *
 *      the List/MinList before using it!                                 *
 *                                                                        *
 *************************************************************************}

 pCDXL = ^tCDXL;
 tCDXL = record
    Node              : tMinNode;       { double linkage                  }
    Buffer            : Pointer;       { data destination (word aligned) }
    Length,                            { must be even # bytes            }
    Actual            : Longint;       { bytes transferred               }
    IntData           : Pointer;       { interrupt server data segment   }
    IntCode           : Pointer;       { interrupt server code entry     }
 end;


{*************************************************************************
 *                                                                        *
 * CD Table of Contents                                           *
 *                                                                        *
 *      The CD_TOC command returns an array of CDTOC entries.             *
 *      Entry zero contains summary information describing how many       *
 *      tracks the disk has and the play-time of the disk.                *
 *      Entries 1 through N (N = Number of tracks on disk) contain        *
 *      information about the track.                                      *
 *                                                                        *
 *************************************************************************}

 pTOCSummary = ^tTOCSummary;
 tTOCSummary = record
    FirstTrack,            { First track on disk (always 1)            }
    LastTrack   : Byte;    { Last track on disk                        }
    LeadOut     : tLSNMSF;  { Beginning of lead-out track (end of disk) }
 end;


 pTOCEntry = ^tTOCEntry;
 tTOCEntry = record
    CtlAdr,             { Q-Code info                  }
    Track  : Byte;      { Track number                 }
    Position  : tLSNMSF; { Start position of this track }
 end;


 pCDTOC = ^tCDTOC;
 tCDTOC = record
    Summary  : tTOCSummary;  { First entry (0) is summary information }
    Entry    : tTOCEntry;    { Entries 1-N are track entries          }
 end;



{*************************************************************************
 *                                                                        *
 * Q-Code Packets                                                         *
 *                                                                        *
 *      Q-Code packets are only returned when audio is playing.   *
 *      Currently, only position packets are returned (ADR_POSITION)      *
 *      The other ADR_ types are almost never encoded on the disk         *
 *      and are of little use anyway.  To avoid making the QCode          *
 *      structure a union, these other ADR_ structures are not defined.   *
 *                                                                        *
 *************************************************************************}

 pQCode = ^tQCode;
 tQCode = record
  CtlAdr,               { Data type / QCode type           }
  Track,                { Track number             }
  Index,                { Track subindex number            }
  Zero : Byte;          { The "Zero" byte of Q-Code packet }
  TrackPosition,        { Position from start of track     }
  DiskPosition  : tLSNMSF;  { Position from start of disk      }
 end;

const
   CTLADR_CTLMASK = $F0;   { Control field }

   CTL_CTLMASK    = $D0;   { To be ANDed with CtlAdr before compared  }

   CTL_2AUD       = $00;   { 2 audio channels without preemphasis     }
   CTL_2AUDEMPH   = $10;   { 2 audio channels with preemphasis        }
   CTL_4AUD       = $80;   { 4 audio channels without preemphasis     }
   CTL_4AUDEMPH   = $90;   { 4 audio channels with preemphasis        }
   CTL_DATA       = $40;   { CD-ROM Data                              }

   CTL_COPYMASK   = $20;   { To be ANDed with CtlAdr before compared  }

   CTL_COPY       = $20;   { When true, this audio/data can be copied }

   CTLADR_ADRMASK = $0F;   { Address field                            }

   ADR_POSITION   = $01;   { Q-Code is position information   }
   ADR_UPC        = $02;   { Q-Code is UPC information (not used)     }
   ADR_ISRC       = $03;   { Q-Code is ISRC (not used)                }
   ADR_HYBRID     = $05;   { This disk is a hybrid disk               }

IMPLEMENTATION

end.
