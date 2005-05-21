{****************************************************************************


    This file is part of the Free Pascal run time library.
    Copyright (c) 2002 by the Free Pascal development team.

    OS/2 Presentation Manager spooler constants, types
    and functions implemented in PMSPL.DLL.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 ****************************************************************************}

unit PMSpl;

interface

{$MODE OBJFPC}

uses OS2Def;

const
    SPL_INI_SPOOLER       = 'PM_SPOOLER';
    SPL_INI_QUEUE         = 'PM_SPOOLER_QUEUE';
    SPL_INI_PRINTER       = 'PM_SPOOLER_PRINTER';
    SPL_INI_PRINTERDESCR  = 'PM_SPOOLER_PRINTER_DESCR';
    SPL_INI_QUEUEDESCR    = 'PM_SPOOLER_QUEUE_DESCR';
    SPL_INI_QUEUEDD       = 'PM_SPOOLER_QUEUE_DD';
    SPL_INI_QUEUEDDDATA   = 'PM_SPOOLER_QUEUE_DDDATA';

{ General SPL return values }
    SPL_ERROR   = 0;
    SPL_OK      = 1;

type
{ Handle to a spool file }
    HSpl = cardinal;

{ Used in recording of PM_Q_STD data via SplStdxxx calls }
    HStd = cardinal;
    PHStd = ^HStd;

{ Spooler manager open data }
    QMOpenData = PChar;
    PQMOpenData = ^PChar;

{ Spooler Queue Processor interface }

const
{ Control codes for SplQpControl }
    SPLC_ABORT    =  1;
    SPLC_PAUSE    =  2;
    SPLC_CONTINUE =  3;

{ Flag defines for optional SplQpQueryFlags }
{ Set this to allow spooler to bypass Queue Processor for PM_Q_Raw
  jobs. This allows print while a job is still spooling. }
    QP_RawData_Bypass = $00000001;

type
{ Handle to a spooler queue processor }
    HProc = cardinal;

    PQPOpenData = ^PChar;

const
{ Definition for elements within the PQPOpenData block }
    QPDAT_ADDRESS     =  0;
    QPDAT_DRIVER_NAME =  1;
    QPDAT_DRIVER_DATA =  2;
    QPDAT_DATA_TYPE   =  3;
    QPDAT_COMMENT     =  4;
    QPDAT_PROC_PARAMS =  5;
    QPDAT_SPL_PARAMS  =  6;      { SplQmOpen Spooler params }
    QPDAT_NET_PARAMS  =  7;      { SplQmOpen Network params }
    QPDAT_DOC_NAME    =  8;      { SplQmStartDoc name       }
    QPDAT_QUEUE_NAME  =  9;      { Queue name for job       }
    QPDAT_TOKEN       = 10;      { SplQmOpen token name     }
    QPDAT_JOBID       = 11;      { SQM job identity         }

type
    TSQPOpenData = record
        pszLogAddress: PChar;
        pszDriverName: PChar;
        pdriv: PDrivData;
        pszDataType: PChar;
        pszComment: PChar;
        pszProcParams: PChar;
        pszSpoolParams: PChar;
        pszNetworkParams: PChar;
        pszDocName: PChar;
        pszQueueName: PChar;
        pszToken: PChar;
        idJobId: word;
    end;
    PSQPOpenData = ^TSQPOpenData;
    SQPOpenData = TSQPOpenData;

{ Error information and return codes }
const
{ Error information for SplMessageBox }
    SPLINFO_QPERROR     =  $0001;
    SPLINFO_DDERROR     =  $0002;
    SPLINFO_SPLERROR    =  $0004;
    SPLINFO_OTHERERROR  =  $0080;
    SPLINFO_INFORMATION =  $0100;
    SPLINFO_WARNING     =  $0200;
    SPLINFO_ERROR       =  $0400;
    SPLINFO_SEVERE      =  $0800;
    SPLINFO_USERINTREQD =  $1000;

{ Error Data for SplMessageBox }
    SPLDATA_PRINTERJAM   = $0001;
    SPLDATA_FORMCHGREQD  = $0002;
    SPLDATA_CARTCHGREQD  = $0004;
    SPLDATA_PENCHGREQD   = $0008;
    SPLDATA_DATAERROR    = $0010;
    SPLDATA_UNEXPECTERROR= $0020;
    SPLDATA_OTHER        = $8000;

{ Return code for fSplStdQueryLength }
    SSQL_ERROR = -1;

type
    SPLERR = cardinal;

const
{ length for character arrays in structs (excluding zero terminator) }
    CNLEN         = 15;             { Computer name length      }
    UNLEN         = 20;             { Maximum user name length  }
    QNLEN         = 12;             { Queue name maximum length }
    PDLEN         =  8;             { Print destination length  }
    DTLEN         =  9;             { Spool file data type      }
                                    { e.g. PM_Q_STD,PM_Q_RAW    }
    QP_DATATYPE_SIZE      = 15;     { returned by SplQpQueryDt  }
    DRIV_DEVICENAME_SIZE  = 31;     { see DRIVDATA struc        }
    DRIV_NAME_SIZE        =  8;     { name of device driver     }
    PRINTERNAME_SIZE      = 32;     { max printer name length   }
    FORMNAME_SIZE         = 31;     { max form name length      }
    MAXCOMMENTSZ          = 48;     { queue comment length      }

type
    TDrivProps = record
        pszKeyName: PChar;
        cbBuf: cardinal;
        pBuf: pointer;
    end;
    PDrivProps =^TDrivProps;
    DrivProps = TDrivProps;
    NPDrivProps = ^DrivProps;

    TPrJInfo = record
        uJobId: word;
        szUserName: array [0..UNLen] of char;
        pad_1: char;
        szNotifyName: array [0..CNLen] of char;
        szDataType: array [0..DTLen] of char;
        pszParms: PChar;
        uPosition: word;
        fsStatus: word;
        pszStatus: PChar;
        ulSubmitted: cardinal;
        ulSize: cardinal;
        pszComment: PChar;
    end;
    PPrJInfo = ^TPrJInfo;
    PrJInfo = TPrJInfo;
    NPPrJInfo = ^TPrJInfo;

    TPrJInfo2 = record
        uJobId: word;
        uPriority: word;
        pszUserName: PChar;
        uPosition: word;
        fsStatus: word;
        ulSubmitted: cardinal;
        ulSize: cardinal;
        pszComment: PChar;
        pszDocument: PChar;
    end;
    PPrJInfo2 = ^TPrJInfo2;
    PrJInfo2 = TPrJInfo2;
    NPPrJInfo2 = ^TPrJInfo2;

    TPrJInfo3 = record
        uJobId: word;
        uPriority: word;
        pszUserName: PChar;
        uPosition: word;
        fsStatus: word;
        ulSubmitted: cardinal;
        ulSize: cardinal;
        pszComment: PChar;
        pszDocument: PChar;
        pszNotifyName: PChar;
        pszDataType: PChar;
        pszParms: PChar;
        pszStatus: PChar;
        pszQueue: PChar;
        pszQProcName: PChar;
        pszQProcParms: PChar;
        pszDriverName: PChar;
        pDriverData: PDrivData;
        pszPrinterName: PChar;
    end;
    PPrJInfo3 = ^TPrJInfo3;
    PrJInfo3 = TPrJInfo3;
    NPPrJInfo3 = ^TPrJInfo3;

    TPrDInfo = record
        szName: array [0..PDLen] of char;
        szUserName: array [0..UNLen] of char;
        uJobId: word;
        fsStatus: word;
        pszStatus: PChar;
        time: word;
    end;
    PPrDInfo = ^TPrDInfo;
    PrDInfo = TPrDInfo;
    NPPrDInfo = ^TPrDInfo;

    TPrDInfo3 = record
        pszPrinterName: PChar;
        pszUserName: PChar;
        pszLogAddr: PChar;
        uJobId: word;
        fsStatus: word;
        pszStatus: PChar;
        pszComment: PChar;
        pszDrivers: PChar;
        time: word;
        usTimeOut: word;
    end;
    PPrDInfo3 = ^TPrDInfo3;
    PrDInfo3 = TPrDInfo3;
    NPPrDInfo3 = ^TPrDInfo3;

    TPrQInfo = record
        szName: array [0..QNLen] of char;
        pad_1: char;
        uPriority: word;
        uStartTime: word;
        uUntilTime: word;
        pszSepFile: PChar;
        pszPrProc: PChar;
        pszDestinations: PChar;
        pszParms: PChar;
        pszComment: PChar;
        fsStatus: word;
        cJobs: word;
    end;
    PPrQInfo = ^TPrQInfo;
    PrQInfo = TPrQInfo;
    NPPrQInfo = ^TPrQInfo;

    TPrQInfo3 = record
        pszName: PChar;
        uPriority: word;
        uStartTime: word;
        uUntilTime: word;
        fsType: word;
        pszSepFile: PChar;
        pszPrProc: PChar;
        pszParms: PChar;
        pszComment: PChar;
        fsStatus: word;
        cJobs: word;
        pszPrinters: PChar;
        pszDriverName: PChar;
        pDriverData: PDrivData;
    end;
    PPrQInfo3 = ^TPrQInfo3;
    PrQInfo3 = TPrQInfo3;
    NPPrQInfo3 = ^TPrQInfo3;

    TPrQInfo6 = record
        pszName: PChar;
        uPriority: word;
        uStartTime: word;
        uUntilTime: word;
        fsType: word;
        pszSepFile: PChar;
        pszPrProc: PChar;
        pszParms: PChar;
        pszComment: PChar;
        fsStatus: word;
        cJobs: word;
        pszPrinters: PChar;
        pszDriverName: PChar;
        pDriverData: PDrivData;
        pszRemoteComputerName: PChar;
        pszRemoteQueueName: PChar;
    end;
    PPrQInfo6 = ^TPrQInfo6;
    PrQInfo6 = TPrQInfo6;
    NPPrQInfo6 = ^TPrQInfo6;

{ Structure for DosPrintJobGetId }
    TPrIDInfo = record
        uJobId: word;
        szComputerName: array [0..CNLen] of char;
        szQueueName: array [0..QNLen] of char;
        pad_1: char;
    end;
    PPrIDInfo = ^TPrIDInfo;
    PrIDInfo = TPrIDInfo;
    NPPrIDInfo = ^TPrIDInfo;

{ Structure for DosPrintDriverEnum }
    TPrDrivInfo = record
        szDrivName: array [0..Driv_Name_Size+1+Driv_DeviceName_Size] of char;
    end;
    PPrDrivInfo = ^TPrDrivInfo;
    PrDrivInfo = TPrDrivInfo;
    NPPrDrivInfo = ^TPrDrivInfo;

{ Structure for DosPrintQProcessorEnum }
    TPrQProcInfo = record
        szQProcName: array [0..QNLen] of char;
    end;
    PPrQProcInfo = ^TPrQProcInfo;
    PrQProcInfo = TPrQProcInfo;
    NPPrQProcInfo = ^TPrQProcInfo;

{ Structure for DosPrintPortEnum Level 0 }
    TPrPortInfo = record
        szPortName: array [0..PDLen] of char;
    end;
    PPrPortInfo = ^TPrPortInfo;
    PrPortInfo = TPrPortInfo;
    NPPrPortInfo = ^TPrPortInfo;

{ Structure for DosPrintPortEnum Level 1 }
    TPrPortInfo1 = record
        pszPortName : PChar;
        pszPortDriverName : PChar;
        pszPortDriverPathName : PChar;
    end;
    PPrPortInfo1 = ^TPrPortInfo1;
    PrPortInfo1 = TPrPortInfo1;
    NPPrPortInfo1 = ^TPrPortInfo1;


const
{ Values for parmnum in DosPrintQSetInfo/SplSetQueue }
    PRQ_PRIORITY_PARMNUM        =    2;
    PRQ_STARTTIME_PARMNUM       =    3;
    PRQ_UNTILTIME_PARMNUM       =    4;
    PRQ_SEPARATOR_PARMNUM       =    5;
    PRQ_PROCESSOR_PARMNUM       =    6;
    PRQ_DESTINATIONS_PARMNUM    =    7;
    PRQ_PARMS_PARMNUM           =    8;
    PRQ_COMMENT_PARMNUM         =    9;
    PRQ_TYPE_PARMNUM            =   10;
    PRQ_PRINTERS_PARMNUM        =   12;
    PRQ_DRIVERNAME_PARMNUM      =   13;
    PRQ_DRIVERDATA_PARMNUM      =   14;
    PRQ_REMOTE_COMPUTER_PARMNUM =   15;
    PRQ_REMOTE_QUEUE_PARMNUM    =   16;
    PRQ_MAXPARMNUM              =   16;

{ Print queue priority }
    PRQ_MAX_PRIORITY            =    1;     { highest priority }
    PRQ_DEF_PRIORITY            =    5;     { default priority }
    PRQ_MIN_PRIORITY            =    9;     { lowest priority  }
    PRQ_NO_PRIORITY             =    0;

{ Print queue status bitmask and values for level 1 }
    PRQ_STATUS_MASK             =    3;
    PRQ_ACTIVE                  =    0;
    PRQ_PAUSED                  =    1;     { queue is held    }
    PRQ_ERROR                   =    2;
    PRQ_PENDING                 =    3;     { pending deletion }

{ Print queue status bits for level 3 }
    PRQ3_PAUSED                 =    1;     { queue is held    }
    PRQ3_PENDING                =    2;     { pending deletion }

{ Print queue type bits for level 3 }
    PRQ3_TYPE_RAW               =    1;     { spools printer-specific data }
    PRQ3_TYPE_BYPASS            =    2;     { allow print while spooling   }
    PRQ3_TYPE_APPDEFAULT        =    4;     { set for application          }
                                            { default queue                }

{ Values for parmnum in DosPrintJobSetInfo/SplSetJob }
    PRJ_NOTIFYNAME_PARMNUM      =    3;
    PRJ_DATATYPE_PARMNUM        =    4;
    PRJ_PARMS_PARMNUM           =    5;
    PRJ_POSITION_PARMNUM        =    6;
    PRJ_JOBFILEINUSE_PARMNUM    =    7;
    PRJ_COMMENT_PARMNUM         =   11;
    PRJ_DOCUMENT_PARMNUM        =   12;
    PRJ_STATUSCOMMENT_PARMNUM   =   13;
    PRJ_PRIORITY_PARMNUM        =   14;
    PRJ_PROCPARMS_PARMNUM       =   16;
    PRJ_DRIVERDATA_PARMNUM      =   18;
    PRJ_MAXPARMNUM              =   18;

{ Bitmap masks for status field of TPrJInfo }
{ Bits 2-7 also used in device status       }
    PRJ_QSTATUS       = $0003;      { bits 0,1  }
    PRJ_DEVSTATUS     = $0ffc;      { bits 2-11 }
    PRJ_COMPLETE      = $0004;      { bit 2     }
    PRJ_INTERV        = $0008;      { bit 3     }
    PRJ_ERROR         = $0010;      { bit 4     }
    PRJ_DESTOFFLINE   = $0020;      { bit 5     }
    PRJ_DESTPAUSED    = $0040;      { bit 6     }
    PRJ_NOTIFY        = $0080;      { bit 7     }
    PRJ_DESTNOPAPER   = $0100;      { bit 8     }
    PRJ_DESTFORMCHG   = $0200;      { bit 9     }
    PRJ_DESTCRTCHG    = $0400;      { bit 10    }
    PRJ_DESTPENCHG    = $0800;      { bit 11    }
    PRJ_JOBFILEINUSE  = $4000;      { bit 14    }
    PRJ_DELETED       = $8000;      { bit 15    }

{ Values of PRJ_QSTATUS bits in fsStatus field of TPrJInfo }
    PRJ_QS_QUEUED              =   0;
    PRJ_QS_PAUSED              =   1;
    PRJ_QS_SPOOLING            =   2;
    PRJ_QS_PRINTING            =   3;

{ Print Job Priority }
    PRJ_MAX_PRIORITY           =  99;       { lowest priority  }
    PRJ_MIN_PRIORITY           =   1;       { highest priority }
    PRJ_NO_PRIORITY            =   0;

{ Bitmap masks for status field of TPrDInfo }
    PRD_STATUS_MASK     =  $0003;      { bits 0,1  }
    PRD_DEVSTATUS       =  $0ffc;      { bits 2-11 }

{ Values of PRD_STATUS_MASK bits in fsStatus field of TPrDInfo }
    PRD_ACTIVE               =  0;
    PRD_PAUSED               =  1;

{ Control codes used in DosPrintDestControl/SplControlDevice }
    PRD_DELETE                  =  0;
    PRD_PAUSE                   =  1;
    PRD_CONT                    =  2;
    PRD_RESTART                 =  3;

{ Values for parmnum in DosPrintDestSetInfo/SplSetDevice }
    PRD_LOGADDR_PARMNUM    =  3;
    PRD_COMMENT_PARMNUM    =  7;
    PRD_DRIVERS_PARMNUM    =  8;
    PRD_TIMEOUT_PARMNUM    =  10;

type
{ Structure for SplEnumPrinter }
    TPrinterInfo = record
        fltype: cardinal;
        pszComputerName: PChar;
        pszPrintDestinationName: PChar;
        pszDescription: PChar;
        pszLocalName: PChar;
    end;
    PrinterInfo = TPrinterInfo;
    PPrinterInfo = ^TPrinterInfo;

const
{ Flags for fltype in  PRINTERINFO and SplEnumPrinter }
    SPL_PR_QUEUE          = $00000001; { include queues }
    SPL_PR_DIRECT_DEVICE  = $00000002; { unattached devices }
    SPL_PR_QUEUED_DEVICE  = $00000004; { queued devices }
    SPL_PR_LOCAL_ONLY     = $00000100; { exclude remote queues }


function SplQueryDevice (const pszComputerName, pszPrintDeviceName: PChar;
                        ulLevel: cardinal; var pBuf; cbBuf: cardinal;
                        var pcbNeeded: cardinal): longint; cdecl;

function SplQueryQueue (const pszComputerName, pszQueueName: PChar;
                       ulLevel: cardinal; var pBuf; cbBuf: cardinal;
                       var pcbNeeded: cardinal): longint; cdecl;

function SplEnumQueue (const pszComputerName: PChar; ulLevel: cardinal;
      var pBuf; cbBuf: cardinal; var pcReturned, pcTotal, pcbNeeded: cardinal;
                                                var pReserved): longint; cdecl;

function SplQmOpen (const pszToken: PChar; lCount: longint;
                                       var pqmdopData: PChar): cardinal; cdecl;

function SplQmStartDoc (ahspl: cardinal; const pszDocName: PChar): longbool;
                                                                         cdecl;

function SplQmWrite (ahspl: cardinal; lCount: longint; var pData): longbool;
                                                                         cdecl;

function SplQmEndDoc (ahspl: cardinal): longbool; cdecl;

function SplQmClose(ahspl: cardinal): longbool; cdecl;

function SplQmAbort(ahspl: cardinal): longbool; cdecl;

function SplQmAbortDoc (ahspl: cardinal): longbool; cdecl;

function SplMessageBox (const pszLogAddr: PChar; fErrInfo, fErrData: cardinal;
                                             const pszText, pszCaption: PChar;
                                  idWindow, fStyle: cardinal): cardinal; cdecl;

function PrtOpen (const pszDeviceName: PChar; var phDevice: cardinal;
                 var pActionTaken: cardinal;cbFileSize,uFileAttr: cardinal;
                 openFlag,openMode,reserved: cardinal): cardinal;
          cdecl;

function PrtClose (hDevice: cardinal): cardinal; cdecl;

function PrtWrite (hDevice: cardinal; var pchData; cbData: cardinal;
                                    var pcbWritten: cardinal): cardinal; cdecl;

function PrtDevIOCtl (var pData; var pParms; ufunction, uCategory: cardinal;
                                           hDevice: cardinal): cardinal; cdecl;

procedure PrtAbort (hDevice: cardinal); cdecl;

function SplStdOpen (ahdc: cardinal): longbool; cdecl;

function SplStdClose (ahdc: cardinal): longbool; cdecl;

function SplStdStart(ahdc: cardinal): longbool; cdecl;

function SplStdStop (ahdc: cardinal): cardinal; cdecl;

function SplStdDelete (hMetaFile: cardinal): longbool; cdecl;

function SplStdGetBits (hMetaFile: cardinal; offData, cbData: longint;
                                                 var pchData): longbool; cdecl;

function SplStdQueryLength (hMetaFile: cardinal): longint; cdecl;

function SplCreateDevice (const pszComputerName: PChar;ulLevel: cardinal;
                                   var pBuf; cbBuf: cardinal): cardinal; cdecl;

function SplControlDevice (const pszComputerName, pszPortName: PChar;
                                         ulControl: cardinal): cardinal; cdecl;

function SplDeleteDevice (const pszComputerName,
                                   pszPrintDeviceName: PChar): cardinal; cdecl;

function SplEnumDevice (const pszComputerName: PChar; ulLevel: cardinal;
      var pBuf; cbBuf: cardinal; var pcReturned, pcTotal, pcbNeeded: cardinal;
                                               var pReserved): cardinal; cdecl;

function SplSetDevice (const pszComputerName, pszPrintDeviceName: PChar;
     ulLevel: cardinal; var pBuf; cbBuf, ulParmNum: cardinal): cardinal; cdecl;

function SplReleaseJob (const pszComputerName, pszQueueName: PChar;
                                             ulJob: cardinal): cardinal; cdecl;

function SplDeleteJob (const pszComputerName, pszQueueName: PChar;
                                             ulJob: cardinal): cardinal; cdecl;

function SplEnumJob (const pszComputerName, pszQueueName: PChar;
                              ulLevel: cardinal; var pBuf;cbBuf: cardinal;
                                 var pcReturned, pcTotal, pcbNeeded: cardinal;
                                               var pReserved): cardinal; cdecl;

function SplQueryJob (const pszComputerName, pszQueueName: PChar;
                          ulJob, ulLevel: cardinal; var pBuf; cbBuf: cardinal;
                                     var pcbNeeded: cardinal): cardinal; cdecl;

function SplHoldJob (const pszComputerName, pszQueueName: PChar;
                                             ulJob: cardinal): cardinal; cdecl;

function SplSetJob (const pszComputerName, pszQueueName: PChar;
                                          ulJob, ulLevel: cardinal; var pBuf;
                                  cbBuf, ulParmNum: cardinal): cardinal; cdecl;

function SplCreateQueue (const pszComputerName: PChar; ulLevel: cardinal;
                                   var pBuf; cbBuf: cardinal): cardinal; cdecl;

function SplReleaseQueue (const pszComputerName,
                                         pszQueueName: PChar): cardinal; cdecl;

function SplDeleteQueue (const pszComputerName, pszQueueName: PChar): cardinal;
                                                                         cdecl;

function SplHoldQueue (const pszComputerName, pszQueueName: PChar): cardinal;
                                                                         cdecl;

function SplPurgeQueue (const pszComputerName, pszQueueName: PChar): cardinal;
                                                                         cdecl;

function SplSetQueue (const pszComputerName, pszQueueName: PChar;
       ulLevel: cardinal; var pBuf;cbBuf,ulParmNum: cardinal): cardinal; cdecl;

function SplEnumDriver (const pszComputerName: PChar; ulLevel: cardinal;
        var pBuf; cbBuf: cardinal; var pcReturned,pcTotal,pcbNeeded: cardinal;
                                               var pReserved): cardinal; cdecl;

function SplEnumPort (const pszComputerName: PChar; ulLevel: cardinal;
      var pBuf; cbBuf: cardinal; var pcReturned, pcTotal, pcbNeeded: cardinal;
                                               var pReserved): cardinal; cdecl;

function SplEnumQueueProcessor (const pszComputerName: PChar;
       ulLevel: cardinal; var pBuf; cbBuf: cardinal; var pcReturned, pcTotal,
                          pcbNeeded: cardinal; var pReserved): cardinal; cdecl;

function SplEnumPrinter (const pszComputerName: PChar;
         uLevel, fltype: cardinal; var pBuf; cbbuf: cardinal; var pcReturned,
                 pcTotal, pcbNeeded: cardinal; var pReserved): cardinal; cdecl;

function SplCopyJob (const pszSrcComputerName, pszSrcQueueName: PChar;
         ulSrcJob: cardinal; const pszTrgComputerName, pszTrgQueueName: PChar;
                                     var pulTrgJob: cardinal): cardinal; cdecl;


implementation

function SplQueryDevice (const pszComputerName, pszPrintDeviceName: PChar;
                              ulLevel: cardinal; var pBuf; cbBuf: cardinal;
                                           var pcbNeeded: cardinal): longint;
                                             cdecl; external 'PMSPL' index 381;

function SplQueryQueue (const pszComputerName, pszQueueName: PChar;
                              ulLevel: cardinal; var pBuf; cbBuf: cardinal;
                                           var pcbNeeded: cardinal): longint;
                                             cdecl; external 'PMSPL' index 397;

function SplEnumQueue (const pszComputerName: PChar; ulLevel: cardinal;
      var pBuf; cbBuf: cardinal; var pcReturned, pcTotal, pcbNeeded: cardinal;
                    var pReserved): longint; cdecl; external 'PMSPL' index 399;

function SplQmOpen (const pszToken: PChar; lCount: longint;
           var pqmdopData: PChar): cardinal; cdecl; external 'PMSPL' index 301;

function SplQmStartDoc (ahspl: cardinal; const pszDocName: PChar): longbool;
                                             cdecl; external 'PMSPL' index 302;

function SplQmWrite (ahspl: cardinal; lCount: longint; var pData): longbool;
                                             cdecl; external 'PMSPL' index 304;

function SplQmEndDoc (ahspl: cardinal): longbool;
                                             cdecl; external 'PMSPL' index 303;

function SplQmClose (ahspl: cardinal): longbool;
                                             cdecl; external 'PMSPL' index 306;

function SplQmAbort (ahspl: cardinal): longbool;
                                             cdecl; external 'PMSPL' index 305;

function SplQmAbortDoc (ahspl: cardinal): longbool;
                                             cdecl; external 'PMSPL' index 308;

function SplMessageBox (const pszLogAddr: PChar; fErrInfo, fErrData: cardinal;
      const pszText, pszCaption: PChar; idWindow, fStyle: cardinal): cardinal;
                                             cdecl; external 'PMSPL' index 307;

function PrtOpen (const pszDeviceName: PChar; var phDevice: cardinal;
                 var pActionTaken: cardinal; cbFileSize, uFileAttr: cardinal;
                            openFlag, openMode, reserved: cardinal): cardinal;
                                             cdecl; external 'PMSPL' index 370;

function PrtClose (hDevice: cardinal): cardinal;
                                             cdecl; external 'PMSPL' index 373;

function PrtWrite (hDevice: cardinal; var pchData; cbData: cardinal;
        var pcbWritten: cardinal): cardinal; cdecl; external 'PMSPL' index 371;

function PrtDevIOCtl (var pData; var pParms; ufunction, uCategory: cardinal;
               hDevice: cardinal): cardinal; cdecl; external 'PMSPL' index 372;

procedure PrtAbort (hDevice: cardinal); cdecl; external 'PMSPL' index 375;

function SplStdOpen (ahdc: cardinal): longbool;
                                             cdecl; external 'PMSPL' index 350;

function SplStdClose (ahdc: cardinal): longbool;
                                             cdecl; external 'PMSPL' index 351;

function SplStdStart (ahdc: cardinal): longbool;
                                             cdecl; external 'PMSPL' index 352;

function SplStdStop (ahdc: cardinal): cardinal;
                                             cdecl; external 'PMSPL' index 353;

function SplStdDelete (hMetaFile: cardinal): longbool;
                                             cdecl; external 'PMSPL' index 354;

function SplStdGetBits (hMetaFile: cardinal; offData, cbData: longint;
                     var pchData): longbool; cdecl; external 'PMSPL' index 355;

function SplStdQueryLength (hMetaFile: cardinal): longint;
                                             cdecl; external 'PMSPL' index 356;

function SplCreateDevice (const pszComputerName: PChar; ulLevel: cardinal;
       var pBuf; cbBuf: cardinal): cardinal; cdecl; external 'PMSPL' index 401;

function SplControlDevice (const pszComputerName, pszPortName: PChar;
             ulControl: cardinal): cardinal; cdecl; external 'PMSPL' index 380;

function SplDeleteDevice (const pszComputerName,
       pszPrintDeviceName: PChar): cardinal; cdecl; external 'PMSPL' index 403;

function SplEnumDevice (const pszComputerName: PChar; ulLevel: cardinal;
        var pBuf; cbBuf: cardinal; var pcReturned,pcTotal,pcbNeeded: cardinal;
                   var pReserved): cardinal; cdecl; external 'PMSPL' index 382;

function SplSetDevice (const pszComputerName, pszPrintDeviceName: PChar;
           ulLevel: cardinal; var pBuf; cbBuf, ulParmNum: cardinal): cardinal;
                                             cdecl; external 'PMSPL' index 402;

function SplReleaseJob (const pszComputerName, pszQueueName: PChar;
                 ulJob: cardinal): cardinal; cdecl; external 'PMSPL' index 384;

function SplDeleteJob (const pszComputerName, pszQueueName: PChar;
                 ulJob: cardinal): cardinal; cdecl; external 'PMSPL' index 386;

function SplEnumJob (const pszComputerName, pszQueueName: PChar;
                           ulLevel: cardinal; var pBuf; cbBuf: cardinal;
                                 var pcReturned, pcTotal, pcbNeeded: cardinal;
                   var pReserved): cardinal; cdecl; external 'PMSPL' index 392;

function SplQueryJob (const pszComputerName, pszQueueName: PChar;
                          ulJob, ulLevel: cardinal; var pBuf; cbBuf: cardinal;
         var pcbNeeded: cardinal): cardinal; cdecl; external 'PMSPL' index 390;

function SplHoldJob (const pszComputerName, pszQueueName: PChar;
                 ulJob: cardinal): cardinal; cdecl; external 'PMSPL' index 385;

function SplSetJob (const pszComputerName, pszQueueName: PChar;
      ulJob, ulLevel: cardinal; var pBuf;cbBuf,ulParmNum: cardinal): cardinal;
                                             cdecl; external 'PMSPL' index 391;

function SplCreateQueue (const pszComputerName: PChar; ulLevel: cardinal;
       var pBuf; cbBuf: cardinal): cardinal; cdecl; external 'PMSPL' index 393;

function SplReleaseQueue (const pszComputerName,
             pszQueueName: PChar): cardinal; cdecl; external 'PMSPL' index 395;

function SplDeleteQueue (const pszComputerName, pszQueueName: PChar): cardinal;
                                             cdecl; external 'PMSPL' index 396;

function SplHoldQueue (const pszComputerName, pszQueueName: PChar): cardinal;
                                             cdecl; external 'PMSPL' index 394;

function SplPurgeQueue (const pszComputerName, pszQueueName: PChar): cardinal;
                                             cdecl; external 'PMSPL' index 404;

function SplSetQueue (const pszComputerName, pszQueueName: PChar;
             ulLevel: cardinal; var pBuf;cbBuf,ulParmNum: cardinal): cardinal;
                                             cdecl; external 'PMSPL' index 398;

function SplEnumDriver (const pszComputerName: PChar; ulLevel: cardinal;
      var pBuf; cbBuf: cardinal; var pcReturned, pcTotal, pcbNeeded: cardinal;
                   var pReserved): cardinal; cdecl; external 'PMSPL' index 406;

function SplEnumPort (const pszComputerName: PChar; ulLevel: cardinal;
        var pBuf; cbBuf: cardinal; var pcReturned,pcTotal,pcbNeeded: cardinal;
                   var pReserved): cardinal; cdecl; external 'PMSPL' index 408;

function SplEnumQueueProcessor (const pszComputerName: PChar;ulLevel: cardinal;
       var pBuf; cbBuf: cardinal; var pcReturned, pcTotal, pcbNeeded: cardinal;
                   var pReserved): cardinal; cdecl; external 'PMSPL' index 407;

function SplEnumPrinter (const pszComputerName: PChar;uLevel, fltype: cardinal;
       var pBuf; cbbuf: cardinal; var pcReturned, pcTotal, pcbNeeded: cardinal;
                   var pReserved): cardinal; cdecl; external 'PMSPL' index 441;

function SplCopyJob (const pszSrcComputerName, pszSrcQueueName: PChar;
         ulSrcJob: cardinal; const pszTrgComputerName, pszTrgQueueName: PChar;
         var pulTrgJob: cardinal): cardinal; cdecl; external 'PMSPL' index 442;


end.
