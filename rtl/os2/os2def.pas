{****************************************************************************


    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2002 by the Free Pascal development team.

    Common OS/2 types and constants (including error codes)

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 ****************************************************************************}
unit OS2Def;

interface

// Common Error definitions

type
       ERRORID=Cardinal;  // errid
       PERRORID=^ERRORID;

const
       Severity_NoError = $0;
       Severity_Warning = $4;
       Severity_Error = $8;
       Severity_Severe = $C;
       Severity_Unrecoverable = $10;

       WinErr_Base = $1000;
       GPIErr_Base = $2000;
       DevErr_Base = $3000;
       SplErr_Base = $4000;

       Address = 0;
       Driver_Name = 1;
       Driver_Data = 2;
       Data_Type = 3;
       Comment = 4;
       Proc_Name = 5;
       Proc_Params = 6;
       Spl_Params = 7;
       Network_Params = 8;

       PD_Job_Property = $0001;
       FAttr_Sel_Italic = $0001;
       FAttr_Sel_Underscore = $0002;
       FAttr_Sel_Outline = $0008;
       FAttr_Sel_Strikeout = $0010;
       FAttr_Sel_Bold = $0020;
       FAttr_Type_Kerning = $0004;
       FAttr_Type_MBCS = $0008;
       FAttr_Type_DBCS = $0010;
       FAttr_Type_Antialiased = $0020;
       FAttr_FontUse_NoMix = $0002;
       FAttr_FontUse_Outline = $0004;
       FAttr_FontUse_Transformable = $0008;
       FaceSize = 32;

       FM_Type_Fixed = $0001;
       FM_Type_Licensed = $0002;
       FM_Type_Kerning = $0004;
       FM_Type_DBCS = $0010;
       FM_Type_MBCS = $0018;
       FM_Type_64K = $8000;
       FM_Type_Atoms = $4000;
       FM_Type_FamTrunc = $2000;
       FM_Type_FaceTrunc = $1000;
       FM_Defn_Outline = $0001;
       FM_Defn_IFI = $0002;
       FM_Defn_Win = $0004;
       FM_Defn_Generic = $8000;
       FM_Sel_Italic = $0001;
       FM_Sel_Underscore = $0002;
       FM_Sel_Negative = $0004;
       FM_Sel_Outline = $0008;
       FM_Sel_Strikeout = $0010;
       FM_Sel_Bold = $0020;
       FM_Cap_NoMix = $0001;

    type
       PCardinal = ^cardinal;
       PLongint = ^longint;
       PInteger = ^integer;
       PShortint = ^shortint;
       PPointer = ^pointer;
       PByte = ^byte;

       PointL = record
          X: longint;
          Y: longint;
       end;

       PPointL = ^PointL;

       PointS = record
          X: integer;
          Y: integer;
       end;

       PPointS = ^PointS;

       RectL = record
          xLeft: longint;
          yBottom: longint;
          xRight: longint;
          yTop: longint;
       end;

       PRectL = ^RectL;

       NPrectL = ^RectL;

       Str8 = array[0..7] of char;

       PStr8 = ^Str8;

       DrivData = record
          cb: longint;
          lVersion: longint;
          szDeviceName: array[0..31] of char;
          abGeneralData: array[0..0] of char;
       end;

       PDrivData = ^DrivData;

       DevOpenStruc = record
          pszLogAddress: PChar;
          pszDriverName: PChar;
          pdriv: PDrivData;
          pszDataType: PChar;
          pszComment: PChar;
          pszQueueProcName: PChar;
          pszQueueProcParams: PChar;
          pszSpoolerParams: PChar;
          pszNetworkParams: PChar;
       end;

       PDevOpenStruc = ^DevOpenStruc;

       PDevOpenData = PDevOpenStruc;

       PrintDest = record
          cb: cardinal;
          lType: longint;
          pszToken: PChar;
          lCount: longint;
          pdopData: PDevOpenData;
          fl: cardinal;
          pszPrinter: PChar;
       end;

       PPrintDest = ^PrintDest;

       FAttrs = record
          usRecordLength: word;
          fsSelection: word;
          lMatch: longint;
          szFacename: array[0..FaceSize-1] of char;
          idRegistry: word;
          usCodePage: word;
          lMaxBaselineExt: longint;
          lAveCharWidth: longint;
          fsType: word;
          fsFontUse: word;
       end;

       PFAttrs = ^FAttrs;

       Panose = record
          bFamilyType: byte;
          bSerifStyle: byte;
          bWeight: byte;
          bProportion: byte;
          bContrast: byte;
          bStrokeVariation: byte;
          bArmStyle: byte;
          bLetterform: byte;
          bMidline: byte;
          bXHeight: byte;
          abReserved: array[0..1] of byte;
       end;

       FontMetrics = record
          szFamilyname: array[0..FaceSize-1] of char;
          szFacename: array[0..FaceSize-1] of char;
          idRegistry: word;
          usCodePage: word;
          lEmHeight: longint;
          lXHeight: longint;
          lMaxAscender: longint;
          lMaxDescender: longint;
          lLowerCaseAscent: longint;
          lLowerCaseDescent: longint;
          lInternalLeading: longint;
          lExternalLeading: longint;
          lAveCharWidth: longint;
          lMaxCharInc: longint;
          lEmInc: longint;
          lMaxBaselineExt: longint;
          sCharSlope: integer;
          sInlineDir: integer;
          sCharRot: integer;
          usWeightClass: word;
          usWidthClass: word;
          sXDeviceRes: integer;
          sYDeviceRes: integer;
          sFirstChar: integer;
          sLastChar: integer;
          sDefaultChar: integer;
          sBreakChar: integer;
          sNominalPointSize: integer;
          sMinimumPointSize: integer;
          sMaximumPointSize: integer;
          fsType: word;
          fsDefn: word;
          fsSelection: word;
          fsCapabilities: word;
          lSubscriptXSize: longint;
          lSubscriptYSize: longint;
          lSubscriptXOffset: longint;
          lSubscriptYOffset: longint;
          lSuperscriptXSize: longint;
          lSuperscriptYSize: longint;
          lSuperscriptXOffset: longint;
          lSuperscriptYOffset: longint;
          lUnderscoreSize: longint;
          lUnderscorePosition: longint;
          lStrikeoutSize: longint;
          lStrikeoutPosition: longint;
          sKerningPairs: integer;
          sFamilyClass: integer;
          lMatch: longint;
          FamilyNameAtom: longint;
          FaceNameAtom: longint;
          _Panose: Panose;
       end;

       PFontMetrics = ^FontMetrics;

      { Null terminated strings are often declared as array[0..0] of byte  }
      { in header files, the following type makes type conversion possible }
      CharArray = array[0..0] of char;

{Names beginning with T for compatibility}
        TPointL = PointL;
        TPointS = PointS;
        TRectL = RectL;
        TStr8 = Str8;
        TDrivData = DrivData;
        TDevOpenStruc = DevOpenStruc;
        TPrintDest = PrintDest;
        TFAttrs = FAttrs;
        TPanose = Panose;
        TFontMetrics = FontMetrics;
        TCharArray = CharArray;

{Another bunch of compatibility things}
        HWnd = cardinal;
        THWnd = HWnd;
        PHWnd = ^HWnd;
        HAB = cardinal;
        THAB = HAB;
        PHAB = ^HAB;
        HMQ = cardinal;
        THMQ = HMQ;
        PHMQ = ^HMQ;
        HPS = cardinal;
        THPS = HPS;
        PHPS = ^HPS;
        HRGN = cardinal;
        THRGN = HRGN;
        PHRGN = ^HRGN;
        HBitmap = cardinal;
        THBitmap = HBitmap;
        PHBitmap = ^HBitmap;
        HEv = cardinal;
        THEv = HEv;
        PHEv = ^HEv;
        HMtx = cardinal;
        THMtx = HMtx;
        PHMtx = ^HMtx;
        ULONG = cardinal;
        MParam = cardinal;
        MResult = cardinal;
        Fixed = longint;
        Fixed88 = integer;

{ Constants from bseord.h header file (ordinal numbers of API functions) }

    const
(* ANSICALL.DLL *)
       Ord_AnsiINJECT = 1;
       Ord_AnsiKEYDEF = 2;
       Ord_AnsiINTERP = 3;
(* PMBIDI.DLL *)
       Ord_NlsCONVERTBIDINUMERICS = 1;
       Ord_NlsCONVERTBIDISTRING = 2;
       Ord_NlsINTSETBIDIATT = 3;
       Ord_NlsINTQUERYBIDIATT = 4;
       Ord_NlsSETBIDIATT = 5;
       Ord_NlsQUERYBIDIATT = 6;
       Ord_NlsINVERSESTRING = 7;
       Ord_NlsSETBIDIPRINT = 8;
       Ord_NlsEDITSHAPE = 9;
       Ord_NlsSHAPEBIDISTRING = 10;
       Ord_NLPOPUP = 11;
(* BKSCALLS.DLL *)
       Ord_BksMAIN = 1;
(* PMDRAG.DLL *)
       Ord_DrgACCESSDRAGINFO = 1;
       Ord_DrgADDSTRHANDLE = 2;
       Ord_DrgALLOCDRAGINFO = 3;
       Ord_DrgALLOCDRAGTRANSFER = 4;
       Ord_DrgDELETEDRAGINFOSTRHANDLES = 5;
       Ord_DrgDELETESTRHANDLE = 6;
       Ord_DrgDRAG = 7;
       Ord_DrgFREEDRAGINFO = 8;
       Ord_DrgFREEDRAGTRANSFER = 9;
       Ord_DrgGETPS = 10;
       Ord_DrgPOSTTRANSFERMSG = 11;
       Ord_DrgPUSHDRAGINFO = 12;
       Ord_DrgQUERYDRAGITEM = 13;
       Ord_DrgQUERYDRAGITEMCOUNT = 14;
       Ord_DrgQUERYDRAGITEMPTR = 15;
       Ord_DrgQUERYNATIVERMF = 16;
       Ord_DrgQUERYNATIVERMFLEN = 17;
       Ord_DrgQUERYSTRNAME = 18;
       Ord_DrgQUERYSTRNAMELEN = 19;
       Ord_DrgQUERYTRUETYPE = 20;
       Ord_DrgQUERYTRUETYPELEN = 21;
       Ord_DrgRELEASEPS = 22;
       Ord_DrgSENDTRANSFERMSG = 23;
       Ord_DrgSETDRAGPOINTER = 24;
       Ord_DrgSETDRAGIMAGE = 25;
       Ord_DrgSETDRAGITEM = 26;
       Ord_DrgVERIFYNATIVERMF = 27;
       Ord_DrgVERIFYRMF = 28;
       Ord_DrgVERIFYTRUETYPE = 29;
       Ord_DrgVERIFYTYPE = 30;
       Ord_DrgVERIFYTYPESET = 31;
       Ord_Drg32ACCESSDRAGINFO = 32;
       Ord_Drg32ADDSTRHANDLE = 33;
       Ord_Drg32ALLOCDRAGINFO = 34;
       Ord_Drg32ALLOCDRAGTRANSFER = 35;
       Ord_Drg32DELETEDRAGINFOSTRHANDLES = 36;
       Ord_Drg32DELETESTRHANDLE = 37;
       Ord_Drg32DRAG = 38;
       Ord_Drg32FREEDRAGINFO = 39;
       Ord_Drg32FREEDRAGTRANSFER = 40;
       Ord_Drg32GETPS = 41;
       Ord_Drg32POSTTRANSFERMSG = 42;
       Ord_Drg32PUSHDRAGINFO = 43;
       Ord_Drg32QUERYDRAGITEM = 44;
       Ord_Drg32QUERYDRAGITEMCOUNT = 45;
       Ord_Drg32QUERYDRAGITEMPTR = 46;
       Ord_Drg32QUERYNATIVERMF = 47;
       Ord_Drg32QUERYNATIVERMFLEN = 48;
       Ord_Drg32QUERYSTRNAME = 49;
       Ord_Drg32QUERYSTRNAMELEN = 50;
       Ord_Drg32QUERYTRUETYPE = 51;
       Ord_Drg32QUERYTRUETYPELEN = 52;
       Ord_Drg32RELEASEPS = 53;
       Ord_Drg32SENDTRANSFERMSG = 54;
       Ord_Drg32SETDRAGPOINTER = 55;
       Ord_Drg32SETDRAGIMAGE = 56;
       Ord_Drg32SETDRAGITEM = 57;
       Ord_Drg32VERIFYNATIVERMF = 58;
       Ord_Drg32VERIFYRMF = 59;
       Ord_Drg32VERIFYTRUETYPE = 60;
       Ord_Drg32VERIFYTYPE = 61;
       Ord_Drg32VERIFYTYPESET = 62;
       Ord_DrgDRAGFILES = 63;
       Ord_DrgACCEPTDROPPEDFILES = 64;
       Ord_Drg32DRAGFILES = 65;
       Ord_Drg32ACCEPTDROPPEDFILES = 66;
(* HELPMGR.DLL *)
       Ord_WinCREATEHELPINSTANCE = 1;
       Ord_WinDESTROYHELPINSTANCE = 2;
       Ord_WinQUERYHELPINSTANCE = 3;
       Ord_WinASSOCIATEHELPINSTANCE = 4;
       Ord_WinLOADHELPTABLE = 5;
       Ord_WinCREATEHELPTABLE = 6;
       Ord_Win32CREATEHELPINSTANCE = 51;
       Ord_Win32DESTROYHELPINSTANCE = 52;
       Ord_Win32QUERYHELPINSTANCE = 53;
       Ord_Win32ASSOCIATEHELPINSTANCE = 54;
       Ord_Win32LOADHELPTABLE = 55;
       Ord_Win32CREATEHELPTABLE = 56;
       Ord_FKAINIT = 64;
       Ord_QUERYFKADISPLAYDETAILS = 65;
       Ord_WinFKAWNDPROC = 66;
       Ord_WinLOADFKA = 67;
       Ord_WinCREATEFKA = 68;
       Ord_WinSETFKAFORM = 69;
       Ord_WinQUERYFKAFORM = 70;
       Ord_WinGOTOFKA = 71;
       Ord_WinGOFROMFKA = 72;
       Ord_DDFINITIALIZE = 74;
       Ord_DDFPARA = 75;
       Ord_DDFSETFORMAT = 76;
       Ord_DDFSETTEXTALIGN = 77;
       Ord_DDFSETCOLOR = 78;
       Ord_DDFINFORM = 79;
       Ord_DDFSETFONTSTYLE = 80;
       Ord_DDFHYPERTEXT = 81;
       Ord_DDFBEGINLIST = 82;
       Ord_DDFLISTITEM = 83;
       Ord_DDFENDLIST = 84;
       Ord_DDFTEXT = 85;
       Ord_DDFMETAFILE = 86;
       Ord_DDFSETFONT = 87;
       Ord_DDFBITMAP = 88;
(* KBDCALLS.DLL *)
       Ord_KbdSETCUSTXT = 1;
       Ord_KbdGETCP = 3;
       Ord_KbdCHARIN = 4;
       Ord_KbdSETCP = 5;
       Ord_KbdSYNCH = 7;
       Ord_KbdREGISTER = 8;
       Ord_KbdSTRINGIN = 9;
       Ord_KbdGETSTATUS = 10;
       Ord_KbdSETSTATUS = 11;
       Ord_KbdGETFOCUS = 12;
       Ord_KbdFLUSHBUFFER = 13;
       Ord_KbdXLATE = 14;
       Ord_KbdCLOSE = 17;
       Ord_KbdFREEFOCUS = 18;
       Ord_KbdDEREGISTER = 20;
       Ord_KbdSETFGND = 21;
       Ord_KbdPEEK = 22;
       Ord_KbdOPEN = 23;
       Ord_KbdGETHWID = 24;
       Ord_KbdSETHWID = 25;
(* MONCALLS.DLL *)
       Ord_DosMONWRITE = 1;
       Ord_DosMONREAD = 2;
       Ord_DosMONCLOSE = 3;
       Ord_DosMONOPEN = 4;
       Ord_DosMONREG = 5;
(* MOUCALLS.DLL *)
       Ord_MouGETPTRSHAPE = 1;
       Ord_MouSETPTRSHAPE = 2;
       Ord_MouGETNUMMICKEYS = 3;
       Ord_MouGETHOTKEY = 4;
       Ord_MouGETSCALEFACT = 6;
       Ord_MouFLUSHQUE = 7;
       Ord_MouGETNUMBUTTONS = 8;
       Ord_MouCLOSE = 9;
       Ord_MouSETHOTKEY = 10;
       Ord_MouSETSCALEFACT = 11;
       Ord_MouGETNUMQUEEL = 13;
       Ord_MouDEREGISTER = 14;
       Ord_MouGETEVENTMASK = 15;
       Ord_MouSETEVENTMASK = 16;
       Ord_MouOPEN = 17;
       Ord_MouREMOVEPTR = 18;
       Ord_MouGETPTRPOS = 19;
       Ord_MouREADEVENTQUE = 20;
       Ord_MouSETPTRPOS = 21;
       Ord_MouGETDEVSTATUS = 22;
       Ord_MouSYNCH = 23;
       Ord_MouREGISTER = 24;
       Ord_MouSETDEVSTATUS = 25;
       Ord_MouDRAWPTR = 26;
       Ord_MouINITREAL = 27;
       Ord_MouGETTHRESHOLD = 29;
       Ord_MouSETTHRESHOLD = 30;
(* MSG.DLL *)
       Ord_DosPUTMESSAGE = 1;
       Ord_DosTRUEGETMESSAGE = 2;
       Ord_DosINSMESSAGE = 3;
       Ord_Dos32INSERTMESSAGE = 4;
       Ord_Dos32PUTMESSAGE = 5;
       Ord_Dos32TRUEGETMESSAGE = 6;
       Ord_DosIQUERYMESSAGECP = 7;
       Ord_Dos32IQUERYMESSAGECP = 8;
(* NLS.DLL *)
       Ord_DosCASEMAP = 1;
       Ord_DosGETCOLLATE = 2;
       Ord_DosGETCTRYINFO = 3;
       Ord_DosGETDBCSEV = 4;
       Ord_Dos32QUERYCTRYINFO = 5;
       Ord_Dos32QUERYDBCSENV = 6;
       Ord_Dos32MAPCASE = 7;
       Ord_Dos32QUERYCOLLATE = 8;
(* NAMPIPES.DLL *)
       Ord_DosMAKENMPIPE = 1;
       Ord_DosQNMPIPEINFO = 2;
       Ord_DosCONNECTNMPIPE = 3;
       Ord_DosDISCONNECTNMPIPE = 4;
       Ord_DosQNMPHANDSTATE = 5;
       Ord_DosSETNMPHANDSTATE = 6;
       Ord_DosPEEKNMPIPE = 7;
       Ord_DosWAITNMPIPE = 8;
       Ord_DosTRANSACTNMPIPE = 9;
       Ord_DosCALLNMPIPE = 10;
       Ord_DosRAWREADNMPIPE = 11;
       Ord_DosRAWWRITENMPIPE = 12;
       Ord_DosSETNMPIPESEM = 13;
       Ord_DosQNMPIPESEMSTATE = 14;
(* DOSCALLS.DLL *)
{
xxx       Ord_DosICreateThread = 1;
}
       Ord_DosCWAIT = 2; (* callgate *)
       Ord_DosENTERCRITSEC = 3; (* callgate *)
       Ord_DosIExecPgm = 4; (* callgate *)
       Ord_DosEXIT = 5; (* callgate *)
       Ord_DosEXITCRITSEC = 6; (* callgate *)
       Ord_DosEXITLIST = 7; (* callgate *)
       Ord_DosGETINFOSEG = 8; (* callgate *)
       Ord_DosGETPRTY = 9; (* callgate *)
       Ord_DosKILLPROCESS = 10; (* callgate *)
       Ord_DosSETPRTY = 11; (* callgate *)
       Ord_DosPTRACE = 12; (* callgate *)
       Ord_DosHOLDSIGNAL = 13; (* callgate *)
       Ord_DosSETSIGHANDLER = 14; (* callgate *)
       Ord_DosFLAGPROCESS = 15; (* callgate *)
       Ord_DosMAKEPIPE = 16; (* callgate *)
       Ord_DosISysSemClear = 17; (* callgate *)
       Ord_DosISemRequest = 18; (* callgate *)
       Ord_DosISysSemSet = 19; (* callgate *)
       Ord_DosSEMSETWAIT = 20; (* callgate *)
       Ord_DosISemWait = 21; (* callgate *)
       Ord_DosMUXSEMWAIT = 22; (* callgate *)
       Ord_DosCLOSESEM = 23; (* callgate *)
       Ord_DosCREATESEM = 24; (* callgate *)
       Ord_DosOPENSEM = 25; (* callgate *)
       Ord_DosRESUMETHREAD = 26; (* callgate *)
       Ord_DosSUSPENDTHREAD = 27; (* callgate *)
       Ord_DosSETDATETIME = 28; (* callgate *)
       Ord_DosTIMERASYNC = 29;
       Ord_DosTIMERSTART = 30;
       Ord_DosTIMERSTOP = 31;
       Ord_DosSLEEP = 32; (* callgate *)
       Ord_DosGETDATETIME = 33; (* callgate *)
       Ord_DosALLOCSEG = 34; (* callgate *)
       Ord_DosALLOCSHRSEG = 35; (* callgate *)
       Ord_DosGETSHRSEG = 36; (* callgate *)
       Ord_DosGIVESEG = 37; (* callgate *)
       Ord_DosREALLOCSEG = 38; (* callgate *)
       Ord_DosFREESEG = 39; (* callgate *)
       Ord_DosALLOCHUGE = 40; (* callgate *)
       Ord_DosGETHUGESHIFT = 41; (* callgate *)
       Ord_DosREALLOCHUGE = 42; (* callgate *)
       Ord_DosCREATECSALIAS = 43; (* callgate *)
       Ord_DosLOADMODULE = 44; (* callgate *)
       Ord_DosGETPROCADDR = 45; (* callgate *)
       Ord_DosFREEMODULE = 46; (* callgate *)
       Ord_DosGETMODHANDLE = 47; (* callgate *)
       Ord_DosGETMODNAME = 48; (* callgate *)
       Ord_DosGETMACHINEMODE = 49; (* callgate *)
       Ord_DosBEEP = 50; (* callgate *)
       Ord_DosCLIACCESS = 51; (* callgate *)
       Ord_DosDEVCONFIG = 52; (* callgate *)
       Ord_DosDEVIOCTL = 53; (* callgate *)
       Ord_DosSGSwitch = 54; (* callgate *)
       Ord_DosSGSwitchMe = 55; (* callgate *)
       Ord_DosBUFRESET = 56; (* callgate *)
       Ord_DosCHDIR = 57; (* callgate *)
       Ord_DosCHGFILEPTR = 58; (* callgate *)
       Ord_DosCLOSE = 59; (* callgate *)
       Ord_DosDELETE = 60; (* callgate *)
       Ord_DosDUPHANDLE = 61; (* callgate *)
       Ord_DosFILELOCKS = 62; (* callgate *)
       Ord_DosFINDCLOSE = 63; (* callgate *)
       Ord_DosFINDFIRST = 64; (* callgate *)
       Ord_DosFINDNEXT = 65; (* callgate *)
       Ord_DosMKDIR = 66; (* callgate *)
       Ord_DosMOVE = 67; (* callgate *)
       Ord_DosNEWSIZE = 68; (* callgate *)
       Ord_DosPORTACCESS = 69; (* callgate *)
       Ord_DosOPEN = 70; (* callgate *)
       Ord_DosQCURDIR = 71; (* callgate *)
       Ord_DosQCURDISK = 72; (* callgate *)
       Ord_DosQFHANDSTATE = 73; (* callgate *)
       Ord_DosQFILEINFO = 74; (* callgate *)
       Ord_DosQFILEMODE = 75; (* callgate *)
       Ord_DosQFSINFO = 76; (* callgate *)
       Ord_DosQHANDTYPE = 77; (* callgate *)
       Ord_DosQVERIFY = 78; (* callgate *)
       Ord_DosIRead = 79; (* callgate *)
       Ord_DosRMDIR = 80; (* callgate *)
       Ord_DosSELECTDISK = 81; (* callgate *)
       Ord_DosSETFHANDSTATE = 82; (* callgate *)
       Ord_DosSETFILEINFO = 83; (* callgate *)
       Ord_DosSETFILEMODE = 84; (* callgate *)
       Ord_DosSETMAXFH = 85; (* callgate *)
       Ord_DosSETVERIFY = 86; (* callgate *)
       Ord_DosIWrite = 87; (* callgate *)
       Ord_DosSYSTEMSERVICE = 88; (* callgate *)
       Ord_DosSETVEC = 89; (* callgate *)
       Ord_DosSYSTRACE = 90; (* callgate *)
       Ord_DosGETENV = 91; (* callgate *)
       Ord_DosGETVERSION = 92; (* callgate *)
       Ord_DosQTraceInfo = 93; (* callgate *)
       Ord_DosGETPID = 94; (* callgate *)
       Ord_DosOPEN2 = 95; (* callgate *)
       Ord_DosLIBINIT = 96; (* callgate *)
       Ord_DosSETFSINFO = 97; (* callgate *)
       Ord_DosQPATHINFO = 98; (* callgate *)
       Ord_DosDEVIOCTL2 = 99; (* callgate *)
       Ord_DosICanonicalize = 100; (* callgate *)
       Ord_DosSetFgnd = 101; (* callgate *)
{
xxx       Ord_DosSwapTaskInit = 102;
xxx       Ord_DosReadPhys = 103;
}
       Ord_DosSETPATHINFO = 104; (* callgate *)
       Ord_DosSGSwitchProc2 = 105; (* callgate *)
       Ord_STRUCHECK = 106; (* callgate *)
       Ord_STRURESUPDATE = 107; (* callgate *)
       Ord_DosISETRELMAXFH = 108; (* callgate *)
       Ord_DosIDEVIOCTL = 109; (* callgate *)
       Ord_Dos32FORCEDELETE = 110;
       Ord_Dos32KILLTHREAD = 111;
       Ord_DosQUERYRASINFO = 112;
       Ord_Dos32DumpProcess  = 113;
       Ord_Dos32SuppressPopUps = 114;
       Ord_THK32ALLOCMEM = 115;
       Ord_THK32FREEMEM = 116;
       Ord_THK32ALLOCSTACK = 117;
       Ord_DosOpen2Compt = 118; (* callgate *)
       Ord_DosGetSTDA = 119; (* callgate *)
       Ord_DosERROR = 120; (* callgate *)
       Ord_DosGETSEG = 121; (* callgate *)
       Ord_DosLOCKSEG = 122; (* callgate *)
       Ord_DosUNLOCKSEG = 123; (* callgate *)
{
xxx       Ord_DosSGSwitchProc = 124;
}
       Ord_DosIRamSemWake = 125; (* callgate *)
       Ord_DosSIZESEG = 126; (* callgate *)
       Ord_DosMEMAVAIL = 127; (* callgate *)
       Ord_DosIRamSemRequest = 128; (* callgate *)
       Ord_DosPHYSICALDISK = 129; (* callgate *)
       Ord_DosGETCP = 130; (* callgate *)
       Ord_DosISetCP = 131; (* callgate *)
{
xxx       Ord_DosGlobalSeg = 132;
xxx       Ord_DosProfile = 133;
}
       Ord_DosSENDSIGNAL = 134; (* callgate *)
       Ord_DosHUGESHIFT = 135;
       Ord_DosHUGEINCR = 136;
       Ord_DosREAD = 137;
       Ord_DosWRITE = 138;
       Ord_DosERRCLASS = 139;
       Ord_DosSEMREQUEST = 140;
       Ord_DosSEMCLEAR = 141;
       Ord_DosSEMWAIT = 142;
       Ord_DosSEMSET = 143;
       Ord_DosEXECPGM = 144;
       Ord_DosCREATETHREAD = 145; (* callgate *)
       Ord_DosSUBSET = 146; (* callgate *)
       Ord_DosSUBALLOC = 147; (* callgate *)
       Ord_DosSUBFREE = 148; (* callgate *)
       Ord_DosREADASYNC = 149;
       Ord_DosWRITEASYNC = 150;
       Ord_DosSEARCHPATH = 151;
       Ord_DosSCANENV = 152;
       Ord_DosSETCP = 153;
       Ord_DosQProcStatus = 154; (* callgate *)
       Ord_DosGETRESOURCE = 155; (* callgate *)
       Ord_DosGETPPID = 156; (* callgate *)
       Ord_DosCALLBACK = 157; (* callgate *)
       Ord_DosICallBack = 158; (* callgate *)
       Ord_DosRetForward = 159; (* callgate *)
       Ord_DosR2STACKREALLOC = 160; (* callgate *)
       Ord_DosFSRAMSEMREQUEST = 161; (* callgate *)
       Ord_DosFSRAMSEMCLEAR = 162; (* callgate *)
       Ord_DosQAPPTYPE = 163;
       Ord_DosSETPROCCP = 164;
       Ord_DosDYNAMICTRACE = 165; (* callgate *)
       Ord_DosQSYSINFO = 166; (* callgate *)
       Ord_DosIMakeNmPipe = 167; (* callgate *)
       Ord_DosICallNmPipe = 168; (* callgate *)
       Ord_DosIConnectNmPipe = 169; (* callgate *)
       Ord_DosIDisconnectNmPipe = 170; (* callgate *)
       Ord_DosIPeekNmPipe = 171; (* callgate *)
       Ord_DosIQNmPipeInfo = 172; (* callgate *)
       Ord_DosIQNmPHandState = 173; (* callgate *)
       Ord_DosISetNmPHandState = 174; (* callgate *)
       Ord_DosITransactNmPipe = 175; (* callgate *)
       Ord_DosIWaitNmPipe = 176; (* callgate *)
       Ord_DosISetNmPipeSem = 177; (* callgate *)
       Ord_DosIQNmPipeSemState = 178; (* callgate *)
       Ord_DosIRawReadNmPipe = 179; (* callgate *)
       Ord_DosIRawWriteNmPipe = 180; (* callgate *)
       Ord_DosFSATTACH = 181; (* callgate *)
       Ord_DosQFSATTACH = 182; (* callgate *)
       Ord_DosFSCTL = 183; (* callgate *)
       Ord_DosFINDFIRST2 = 184; (* callgate *)
       Ord_DosMKDIR2 = 185; (* callgate *)
       Ord_DosFILEIO = 186; (* callgate *)
       Ord_DosFINDNOTIFYCLOSE = 187; (* callgate *)
       Ord_DosFINDNOTIFYFIRST = 188; (* callgate *)
       Ord_DosFINDNOTIFYNEXT = 189; (* callgate *)
       Ord_DosSetTraceInfo = 190; (* callgate *)
       Ord_DosEDITNAME = 191; (* callgate *)
       Ord_DosLogMode = 192; (* callgate *)
       Ord_DosLogEntry = 193; (* callgate *)
       Ord_DosGetLogBuffer = 194; (* callgate *)
       Ord_DosLogRegister = 195;
       Ord_DosLogRead = 196;
       Ord_DosFindFromName = 197; (* callgate *)
       Ord_DosOPLockRelease = 198; (* callgate *)
       Ord_DosOPLockWait = 199; (* callgate *)
       Ord_DosICopy = 200; (* callgate *)
       Ord_DosCOPY = 201;
       Ord_DosIQAppType = 202; (* callgate *)
       Ord_DosFORCEDELETE = 203; (* callgate *)
       Ord_DosENUMATTRIBUTE = 204; (* callgate *)
       Ord_DosOPLockShutdown = 205; (* callgate *)
       Ord_DosSHUTDOWN = 206; (* callgate *)
       Ord_DosGETRESOURCE2 = 207; (* callgate *)
       Ord_DosFREERESOURCE = 208; (* callgate *)
       Ord_Dos32SETMAXFH = 209;
       Ord_Dos32SETVERIFY = 210;
       Ord_Dos32ERRCLASS = 211;
       Ord_Dos32ERROR = 212;
       Ord_DosCreateVDM = 213; (* callgate *)
       Ord_DosMAXPATHLEN = 214;
       Ord_DosPAGESIZE = 215;
       Ord_DosLOCALINFO = 216;
       Ord_DosGLOBALINFO = 217;
       Ord_Dos32SETFILEINFO = 218;
       Ord_Dos32SETPATHINFO = 219;
       Ord_Dos32SETDEFAULTDISK = 220;
       Ord_Dos32SETFHSTATE = 221;
       Ord_Dos32SETFSINFO = 222;
       Ord_Dos32QUERYPATHINFO = 223;
       Ord_Dos32QUERYHTYPE = 224;
       Ord_Dos32QUERYVERIFY = 225;
       Ord_Dos32DELETEDIR = 226;
       Ord_Dos32SCANENV = 227;
       Ord_Dos32SEARCHPATH = 228;
       Ord_Dos32SLEEP = 229;
       Ord_Dos32GETDATETIME = 230;
       Ord_Dos32DEVCONFIG = 231;
       Ord_Dos32ENTERCRITSEC = 232;
       Ord_Dos32EXITCRITSEC = 233;
       Ord_Dos32EXIT = 234;
       Ord_Dos32KILLPROCESS = 235;
       Ord_Dos32SETPRIORITY = 236;
       Ord_Dos32RESUMETHREAD = 237;
       Ord_Dos32SUSPENDTHREAD = 238;
       Ord_Dos32CREATEPIPE = 239;
       Ord_Dos32CALLNPIPE = 240;
       Ord_Dos32CONNECTNPIPE = 241;
       Ord_Dos32DISCONNECTNPIPE = 242;
       Ord_Dos32CREATENPIPE = 243;
       Ord_Dos32PEEKNPIPE = 244;
       Ord_Dos32QUERYNPHSTATE = 245;
       Ord_Dos32RAWREADNPIPE = 246;
       Ord_Dos32RAWWRITENPIPE = 247;
       Ord_Dos32QUERYNPIPEINFO = 248;
       Ord_Dos32QUERYNPIPESEMSTATE = 249;
       Ord_Dos32SETNPHSTATE = 250;
       Ord_Dos32SETNPIPESEM = 251;
       Ord_Dos32TRANSACTNPIPE = 252;
       Ord_Dos32WAITNPIPE = 253;
       Ord_Dos32RESETBUFFER = 254;
       Ord_Dos32SETCURRENTDIR = 255;
       Ord_Dos32SETFILEPTR = 256;
       Ord_Dos32CLOSE = 257;
       Ord_Dos32COPY = 258;
       Ord_Dos32DELETE = 259;
       Ord_Dos32DUPHANDLE = 260;
       Ord_Dos32EDITNAME = 261;
       Ord_THK32FREESTACK = 262;
       Ord_Dos32FINDCLOSE = 263;
       Ord_Dos32FINDFIRST = 264;
       Ord_Dos32FINDNEXT = 265;
       Ord_DosOPENVDD = 266;
       Ord_DosREQUESTVDD = 267;
       Ord_DosCLOSEVDD = 268;
       Ord_Dos32FSATTACH = 269;
       Ord_Dos32CREATEDIR = 270;
       Ord_Dos32MOVE = 271;
       Ord_Dos32SETFILESIZE = 272;
       Ord_Dos32OPEN = 273;
       Ord_Dos32QUERYCURRENTDIR = 274;
       Ord_Dos32QUERYCURRENTDISK = 275;
       Ord_Dos32QUERYFHSTATE = 276;
       Ord_Dos32QUERYFSATTACH = 277;
       Ord_Dos32QUERYFSINFO = 278;
       Ord_Dos32QUERYFILEINFO = 279;
       Ord_Dos32WAITCHILD = 280;
       Ord_Dos32READ = 281;
       Ord_Dos32WRITE = 282;
       Ord_Dos32EXECPGM = 283;
       Ord_Dos32DEVIOCTL = 284;
       Ord_Dos32FSCTL = 285;
       Ord_Dos32BEEP = 286;
       Ord_Dos32PHYSICALDISK = 287;
       Ord_Dos32SETCP = 288;
       Ord_Dos32SETPROCESSCP = 289;
       Ord_Dos32STOPTIMER = 290;
       Ord_Dos32QUERYCP = 291;
       Ord_Dos32SETDATETIME = 292;
       Ord_THK32AllocBlock = 293;
       Ord_THK32FreeBlock = 294;
       Ord_THK32R3DS = 295;
       Ord_Dos32EXITLIST = 296;
       Ord_Dos32AllocProtectedMem = 297;
       Ord_Dos32AliasMem = 298;
       Ord_Dos32ALLOCMEM = 299;
       Ord_Dos32ALLOCSHAREDMEM = 300;
       Ord_Dos32GETNAMEDSHAREDMEM = 301;
       Ord_Dos32GETSHAREDMEM = 302;
       Ord_Dos32GIVESHAREDMEM = 303;
       Ord_Dos32FREEMEM = 304;
       Ord_Dos32SETMEM = 305;
       Ord_Dos32QUERYMEM = 306;
       Ord_Dos32QueryMemState = 307;
       Ord_Dos32OPENVDD = 308;
       Ord_Dos32REQUESTVDD = 309;
       Ord_Dos32CLOSEVDD = 310;
       Ord_Dos32CREATETHREAD = 311;
       Ord_Dos32GETINFOBLOCKS = 312;
       Ord_DosAllocProtSeg = 313; (* callgate *)
       Ord_DosAllocShrProtSeg = 314; (* callgate *)
       Ord_DosAllocProtHuge = 315; (* callgate *)
       Ord_Dos32DYNAMICTRACE = 316;
       Ord_Dos32DEBUG = 317;
       Ord_Dos32LOADMODULE = 318;
       Ord_Dos32QUERYMODULEHANDLE = 319;
       Ord_Dos32QUERYMODULENAME = 320;
       Ord_Dos32QUERYPROCADDR = 321;
       Ord_Dos32FREEMODULE = 322;
       Ord_Dos32QUERYAPPTYPE = 323;
       Ord_Dos32CREATEEVENTSEM = 324;
       Ord_Dos32OPENEVENTSEM = 325;
       Ord_Dos32CLOSEEVENTSEM = 326;
       Ord_Dos32RESETEVENTSEM = 327;
       Ord_Dos32POSTEVENTSEM = 328;
       Ord_Dos32WAITEVENTSEM = 329;
       Ord_Dos32QUERYEVENTSEM = 330;
       Ord_Dos32CREATEMUTEXSEM = 331;
       Ord_Dos32OPENMUTEXSEM = 332;
       Ord_Dos32CLOSEMUTEXSEM = 333;
       Ord_Dos32REQUESTMUTEXSEM = 334;
       Ord_Dos32RELEASEMUTEXSEM = 335;
       Ord_Dos32QUERYMUTEXSEM = 336;
       Ord_Dos32CREATEMUXWAITSEM = 337;
       Ord_Dos32OPENMUXWAITSEM = 338;
       Ord_Dos32CLOSEMUXWAITSEM = 339;
       Ord_Dos32WAITMUXWAITSEM = 340;
       Ord_Dos32ADDMUXWAITSEM = 341;
       Ord_Dos32DELETEMUXWAITSEM = 342;
       Ord_Dos32QUERYMUXWAITSEM = 343;
       Ord_Dos32SUBSETMEM = 344;
       Ord_Dos32SUBALLOCMEM = 345;
       Ord_Dos32SUBFREEMEM = 346;
       Ord_Dos32SUBUNSETMEM = 347;
       Ord_Dos32QUERYSYSINFO = 348;
       Ord_Dos32WAITTHREAD = 349;
       Ord_Dos32ASYNCTIMER = 350;
       Ord_Dos32STARTTIMER = 351;
       Ord_Dos32GETRESOURCE = 352;
       Ord_Dos32FREERESOURCE = 353;
       Ord_Dos32SETEXCEPTIONHANDLER = 354;
       Ord_Dos32UNSETEXCEPTIONHANDLER = 355;
       Ord_Dos32RAISEEXCEPTION = 356;
       Ord_Dos32UNWINDEXCEPTION = 357;
       Ord_Dos32QueryPageUsage = 358;
       Ord_DosQueryModFromCS = 359; (* callgate *)
       Ord_Dos32QueryModFromEIP = 360;
       Ord_DosFPDataArea = 361;
       Ord_Dos32TMRQUERYFREQ = 362;
       Ord_Dos32TMRQUERYTIME = 363;
       Ord_Dos32AliasPerfCtrs = 364;
       Ord_Dos32ConfigPerf = 365;
       Ord_Dos32DeconPerf = 366;
       Ord_Dos32REGISTERPERFCTRS = 367;
       Ord_Dos32QuerySysState = 368;
       Ord_Dos32FlatCS = 369;
       Ord_Dos32FlatDS = 370;
       Ord_Dos32QueryABIOSSupport = 371;
       Ord_Dos32ENUMATTRIBUTE = 372;
       Ord_Dos32QUERYDOSPROPERTY = 373;
       Ord_Dos32SETDOSPROPERTY = 374;
       Ord_DosQUERYDOSPROPERTY = 375;
       Ord_DosSETDOSPROPERTY = 376;
       Ord_Dos32PROFILE = 377;
       Ord_Dos32SETSIGNALEXCEPTIONFOC = 378;
       Ord_Dos32SENDSIGNALEXCEPTION = 379;
       Ord_Dos32ENTERMUSTCOMPLETE = 380;
       Ord_Dos32EXITMUSTCOMPLETE = 381;
       Ord_Dos32SETRELMAXFH = 382;
       Ord_MsgPutMessage = 383;
       Ord_MsgTrueGetMessage = 384;
       Ord_MsgInsMessage = 385;
       Ord_Msg32InsertMessage = 386;
       Ord_Msg32PutMessage = 387;
       Ord_Msg32TrueGetMessage = 388;
       Ord_MsgIQueryMessageCP = 389;
       Ord_Msg32IQueryMessageCP = 390;
       Ord_NLSCaseMap = 391;
       Ord_NLSGetCollate = 392;
       Ord_NLSGetCtryInfo = 393;
       Ord_NLSGetDBCSEv = 394;
       Ord_NLS32QueryCtryInfo = 395;
       Ord_NLS32QueryDBCSEnv = 396;
       Ord_NLS32MapCase = 397;
       Ord_NLS32QueryCollate = 398;
       Ord_NPipeMakeNmPipe = 399;
       Ord_NPipeQNmPipeInfo = 400;
       Ord_NPipeConnectNmPipe = 401;
       Ord_NPipeDisconnectNmPipe = 402;
       Ord_NPipeQNmPHandState = 403;
       Ord_NPipeSetNmPHandState = 404;
       Ord_NPipePeekNmPipe = 405;
       Ord_NPipeWaitNmPipe = 406;
       Ord_NPipeTransactNmPipe = 407;
       Ord_NPipeCallNmPipe = 408;
       Ord_NPipeRawReadNmPipe = 409;
       Ord_NPipeRawWriteNmPipe = 410;
       Ord_NPipeSetNmPipeSem = 411;
       Ord_NPipeQNmPipeSemState = 412;
       Ord_StartLazyWriter = 413;
       Ord_HPFSStartLazyWriter = 413;
       Ord__QueInst_Data = 414;
       Ord_QueInstanceData = 414;
       Ord_Dos32SHUTDOWN = 415;
       Ord_Dos32ICacheModule = 416;
       Ord_Dos32REPLACEMODULE = 417;
       Ord_Dos32ACKNOWLEDGESIGNALEXC = 418;
       Ord_Dos32TIB = 419;
       Ord_DosTMRQUERYFREQ = 420;
       Ord_DosTMRQUERYTIME = 421;
       Ord_DosREGISTERPERFCTRS = 422;
       Ord_DosFLATTOSEL = 423;
       Ord_DosSELTOFLAT = 424;
       Ord_Dos32FLATTOSEL = 425;
       Ord_Dos32SELTOFLAT = 426;
       Ord_DosIODelayCnt = 427;
       Ord_Dos32SETFILELOCKS = 428;
       Ord_Dos32CANCELLOCKREQUEST = 429;
       Ord_DosOPENCHANGENOTIFY = 437; (* callgate *)
       Ord_DosRESETCHANGENOTIFY = 438; (* callgate *)
       Ord_DosCLOSECHANGENOTIFY = 439; (* callgate *)
       Ord_Dos32OPENCHANGENOTIFY = 440;
       Ord_Dos32RESETCHANGENOTIFY = 441;
       Ord_Dos32CLOSECHANGENOTIFY = 442;
       Ord_DosQueryABIOSSupport = 443; (* callgate *)
       Ord_Dos32ForceSystemDump = 444;
       Ord_Dos32GetProcessorStatus = 447;
       Ord_Dos32SetProcessorStatus = 448;
       Ord_DosCreateSpinLock = 449; (* callgate *)
       Ord_DosAcquireSpinLock = 450; (* callgate *)
       Ord_DosReleaseSpinLock = 451; (* callgate *)
       Ord_DosFreeSpinLock = 452; (* callgate *)
       Ord_Dos32TestPSD = 453;
       Ord_Dos32AllocThreadLocalMemory = 454;
       Ord_Dos32FreeThreadLocalMemory = 455;
       Ord_Dos32VerifyPIDTID = 460;
{
xxx       Ord_PTDA_LANMAN_SEC = 464;
xxx       Ord_PTDA_PID = 465;
xxx       Ord_SAS_SEL = 466;
xxx       Ord_TCB_OPCOOKIE = 467;
xxx       Ord_TCB_OPFLAGS = 468;
xxx       Ord_TCB_NEWFLAGS = 469;
xxx       Ord_TCB_USER_ID = 470;
xxx       Ord_TCB_PROC_ID = 471;
xxx       Ord_TCB_FSHARING = 472;
xxx       Ord_TCB_SRVATTRIB = 473;
xxx       Ord_TCB_ALLOWED = 474;
xxx       Ord_TCB_PRTCB = 475;
xxx       Ord_TCB_NUMBER = 476;
xxx       Ord_TCB_THISSFT = 477;
xxx       Ord_TCB_THISCDS = 478;
xxx       Ord_TKOPTDA = 479;
xxx       Ord_PTDA_CRITSEC = 480;
xxx       Ord_PTDA_HOLDSIGCNT = 481;
xxx       Ord_PTDA_PPTDAPARENT = 482;
xxx       Ord_PTDA_PGDATA = 483;
xxx       Ord_PTDA_HANDLE = 484;
xxx       Ord_PTDA_MODULE = 485;
xxx       Ord_PTDA_LDTHANDLE = 486;
xxx       Ord_PTDA_CODEPAGE_TAG = 487;
xxx       Ord_PTDA_JFN_LENGTH = 488;
xxx       Ord_PTDA_JFN_PTABLE = 489;
xxx       Ord_PTDA_JFN_FLG_PTR = 490;
xxx       Ord_PTDA_EXTERR_LOCUS = 491;
xxx       Ord_PTDA_EXTERR = 492;
xxx       Ord_PTDA_EXTERR_ACTION = 493;
xxx       Ord_PTDA_EXTERR_CLASS = 494;
xxx       Ord_PTDA_PPID = 495;
xxx       Ord_PTDA_PROCTYPE = 496;
xxx       Ord_PTDA_CURRTCB = 497;
xxx       Ord_PTDA_CURRTSD = 498;
xxx       Ord_PTDA_SIGNATURE = 499;
xxx   500 T32EXITLIST 
xxx   501 T32ALLOCPROTECTEDMEM 
xxx   502 T32ALIASMEM 
xxx   503 T32ALLOCMEM 
xxx   504 T32ALLOCSHAREDMEM 
xxx   505 T32GETNAMEDSHAREDMEM 
xxx   506 T32GETSHAREDMEM 
xxx   507 T32GIVESHAREDMEM 
xxx   508 T32FREEMEM 
xxx   509 T32SETMEM 
xxx   510 T32QUERYMEM 
xxx   511 T32QUERYMEMSTATE 
xxx   512 T32OPENVDD 
xxx   513 T32REQUESTVDD 
xxx   514 T32CLOSEVDD 
xxx   515 T32CREATETHREAD 
xxx   516 T32DYNAMICTRACE 
xxx   517 T32DEBUG 
xxx   518 T32QUERYPROCADDR 
xxx   519 T32CREATEEVENTSEM 
xxx   520 T32OPENEVENTSEM 
xxx   521 T32CLOSEEVENTSEM 
xxx   522 T32RESETEVENTSEM 
xxx   523 T32POSTEVENTSEM 
xxx   524 T32WAITEVENTSEM 
xxx   525 T32QUERYEVENTSEM 
xxx   526 T32CREATEMUTEXSEM 
xxx   527 T32OPENMUTEXSEM 
xxx   528 T32CLOSEMUTEXSEM 
xxx   529 T32REQUESTMUTEXSEM 
xxx   530 T32RELEASEMUTEXSEM 
xxx   531 T32QUERYMUTEXSEM 
xxx   532 T32CREATEMUXWAITSEM 
xxx   533 T32OPENMUXWAITSEM 
xxx   534 T32CLOSEMUXWAITSEM 
xxx   535 T32WAITMUXWAITSEM 
xxx   536 T32ADDMUXWAITSEM 
xxx   537 T32DELETEMUXWAITSEM 
xxx   538 T32QUERYMUXWAITSEM 
xxx   539 T32QUERYSYSINFO 
xxx   540 T32WAITTHREAD
}
       Ord_DosIOpenL = 541; (* callgate *)
       Ord_DosIProtectOpenL = 542; (* callgate *)
       Ord_DosISetFileSizeL = 543; (* callgate *)
       Ord_DosIProtectSetFileSizeL = 544; (* callgate *)
{
xxx       Ord_Dos32ExceptionCallBack = 545;
}
       Ord_THK32STRLEN = 546;
       Ord_THK32_UNITHUNK = 547;
       Ord_Dos32R3ExceptionDispatcher = 548;
       Ord_DosLibiDisp = 549;
       Ord_DosLibiDisp16 = 550;
       Ord_DosLibiDisp32 = 551;
       Ord_DosR3ExitAddr = 552;
       Ord_Dos32R3ExitAddr = 553;
{
xxx       Ord_Dos32IRead = 554;
xxx       Ord_Dos32IWrite = 556;
}
       Ord_Dos32CreateSpinLock = 557;
       Ord_Dos32AcquireSpinLock = 558;
       Ord_Dos32ReleaseSpinLock = 559;
       Ord_Dos32FreeSpinLock = 560;
       Ord_Dos32PMR3WaitEventSem = 561;
       Ord_Dos32PMR3PostEventSem = 562;
       Ord_Dos32QueryThreadAffinity = 563;
       Ord_Dos32SetThreadAffinity = 564;
       Ord_DosISetFileInfo = 565; (* callgate *)
       Ord_DosISetPathInfo = 566; (* callgate *)
       Ord_DosOpenL = 567; (* callgate *)
       Ord_DosSetFilePtrL = 568;
       Ord_DosIFindNext = 569; (* callgate *)
       Ord_DosSetFileSizeL = 570;
       Ord_Dos32QUERYRESOURCESIZE = 572;
       Ord_DosQUERYRESOURCESIZE = 573;
       Ord_DosPMSemWait = 574;
       Ord_DosPMMuxSemWait = 575;
       Ord_THK16_UNITHUNK = 576;
       Ord_HT16_STARTUP = 577;
       Ord_HT16_CLEANUP = 578;
       Ord_HT32_STARTUP = 579;
       Ord_Dos32INITIALIZEPORTHOLE = 580;
       Ord_HT32_CLEANUP = 581;
       Ord_Dos32QUERYHEADERINFO = 582;
       Ord_DosINITIALIZEPORTHOLE = 583;
       Ord_DosQUERYHEADERINFO = 584;
       Ord__Dos32IMonRead = 585;
       Ord_Mon32IMonRead = 585;
       Ord_Dos32QUERYPROCTYPE = 586;
       Ord_DosQUERYPROCTYPE = 587;
       Ord__Dos32IMonWrite = 588;
       Ord_Mon32IMonWrite = 588;
       Ord_DosISigDispatch = 589;
       Ord_Dos32PMPOSTEVENTSEM = 590;
       Ord_Dos32PMWAITEVENTSEM = 591;
       Ord_Dos32DllTermDisp = 592;
       Ord_Dos32PMREQUESTMUTEXSEM = 593;
       Ord_Dos32IRaiseException = 594;
       Ord_Dos32PMWAITMUXWAITSEM = 595;
       Ord_Dos32PM16SEMCHK = 596;
       Ord_Dos32IQueryFHState = 597; (* callgate *)
       Ord_Dos32ISetFHState = 598; (* callgate *)
       Ord_Dos32LDTSel = 599;
       Ord_Dos32R3Frestor = 600;
       Ord_DosIFindFirst = 601; (* callgate *)
{
xxx   606 MonDosMonOpen
xxx   607 MonDosMonClose
xxx   608 MonDosMonRead
xxx   609 MonDosMonWrite
xxx   610 MonDosMonReg
xxx       Ord_Dos32IProtectWrite = 615;
}
       Ord_DosIProtectSetFileInfo = 617; (* callgate *)
{
xxx       Ord_Dos32IProtectSetFileInfo = 618;
}
       Ord_Dos32IProtectSetFHState = 619; (* callgate *)
       Ord_Dos32IProtectQueryFHState = 620; (* callgate *)
       Ord_Dos32PROTECTSETFILEPTR = 621;
       Ord_DosPROTECTCLOSE = 622; (* callgate *)
       Ord_DosPROTECTFILEIO = 623; (* callgate *)
       Ord_DosPROTECTFILELOCKS = 624; (* callgate *)
       Ord_DosIProtectRead = 625; (* callgate *)
       Ord_DosIProtectWrite = 626; (* callgate *)
       Ord_DosPROTECTNEWSIZE = 627; (* callgate *)
       Ord_DosPROTECTOPEN = 628; (* callgate *)
       Ord_DosPROTECTQFHANDSTATE = 629; (* callgate *)
       Ord_DosPROTECTSETFHANDSTATE = 630; (* callgate *)
       Ord_DosPROTECTQFILEINFO = 631; (* callgate *)
       Ord_DosPROTECTSETFILEINFO = 632; (* callgate *)
       Ord_DosPROTECTCHGFILEPTR = 634; (* callgate *)
       Ord_DosPROTECTENUMATTRIBUTE = 635; (* callgate *)
       Ord_Dos32PROTECTENUMATTRIBUTE = 636;
       Ord_Dos32PROTECTOPEN = 637;
       Ord_Dos32PROTECTCLOSE = 638;
       Ord_Dos32PROTECTSETFILELOCKS = 639;
       Ord_Dos32PROTECTSETFILESIZE = 640;
       Ord_Dos32PROTECTREAD = 641;
       Ord_Dos32PROTECTWRITE = 642;
       Ord_Dos32PROTECTSETFILEINFO = 643;
       Ord_Dos32PROTECTSETFHSTATE = 644;
       Ord_Dos32PROTECTQUERYFHSTATE = 645;
       Ord_Dos32PROTECTQUERYFILEINFO = 646;
{
xxx       Ord_Dos32IProtectRead = 647;
}
       Ord_DosCloseMessageFile = 649;
       Ord_DosLDRDirtyWorker = 650; (* callgate *)
       Ord_Dos16LDRDirtyWorker = 651; (* callgate *)
{
xxx 652 Ord_Dos32IGetInfoBlocks
}
       Ord_T32IGETINFOBLOCKS = 653; (* callgate *)
       Ord_T32GETPROCESSORSTATUS = 656; (* callgate *)
       Ord_T32SETPROCESSORSTATUS = 657; (* callgate *)
       Ord_T32TESTPSD = 658; (* callgate *)
       Ord_T32QUERYTHREADAFFINITY = 659; (* callgate *)
       Ord_T32SETTHREADAFFINITY = 660; (* callgate *)
       Ord_QueDos32READQUEUE = 661;
       Ord_QueDos32PURGEQUEUE = 662;
       Ord_QueDos32CLOSEQUEUE = 663;
       Ord_QueDos32QUERYQUEUE = 664;
       Ord_QueDos32PEEKQUEUE = 665;
       Ord_QueDos32WRITEQUEUE = 666;
       Ord_QueDos32OPENQUEUE = 667;
       Ord_QueDos32CREATEQUEUE = 668;
       Ord_SMGDos32STARTSESSION = 669;
       Ord_SMGDos32SELECTSESSION = 670;
       Ord_SMGDos32SETSESSION = 671;
       Ord_SMGDos32STOPSESSION = 672;
       Ord_SMGREGISTERNOTIFICATION = 673;
       Ord_QueDosREADQUEUE = 674;
       Ord_QueDosPURGEQUEUE = 675;
       Ord_QueDosCLOSEQUEUE = 676;
       Ord_QueDosQUERYQUEUE = 677;
       Ord_QueDosPEEKQUEUE = 678;
       Ord_QueDosWRITEQUEUE = 679;
       Ord_QueDosOPENQUEUE = 680;
       Ord_QueDosCREATEQUEUE = 681;
       Ord_CHRDosSMGETME = 682;
       Ord_CHRDosSMFREEMEM = 683;
       Ord_CHRDosSMGETSGCB = 684;
       Ord_CHRDosSMINITSGCB = 685;
       Ord_SMGDosSMSGDOPOPUP = 686;
       Ord_SMGDosSMSWITCH = 687;
       Ord_SMGDosSMSERVEAPPREQ = 688;
       Ord_SMGDosGETTIMES = 689;
       Ord_SMGDosSMSETTITLE = 690;
       Ord_SMGDosSCRUNLOCK = 691;
       Ord_SMGDosSMDOAPPREQ = 692;
       Ord_SMGDosSTOPSESSION = 693;
       Ord_SMGDosSELECTSESSION = 694;
       Ord_SMGDosSCRLOCK = 695;
       Ord_SMGDosSAVREDRAWWAIT = 696;
       Ord_SMGDosSAVREDRAWUNDO = 697;
       Ord_SMGDosSMSGENDPOPUP = 698;
       Ord_SMGDosSETSESSION = 699;
       Ord_SMGDosSETMNLOCKTIME = 700;
       Ord_SMGDosMODEUNDO = 701;
       Ord_SMGDosSTARTSESSION = 702;
       Ord_SMGDosSMGETSTATUS = 703;
       Ord_SMGDosSMMODEWAIT = 704;
       Ord_SMGDosSMTERMINATE = 705;
       Ord_SMGDosSMGETAPPREQ = 706;
       Ord_SMGDosSMINITIALIZE = 707;
       Ord_SMGDosSMSTART = 708;
       Ord_SMGDosSMPARENTSWITCH = 709;
       Ord_SMGDosSMPAUSE = 710;
       Ord_SMGDosSMHDEINIT = 711;
       Ord_SMGDosSMPMPRESENT = 712;
       Ord_SMGDosSMREGISTERDD = 713;
       Ord_SMGDosSMNOTIFYDD = 714;
       Ord_SMGDosSMNOTIFYDD2 = 715;
       Ord_SMGDosSMOPENDD = 716;
       Ord_SMGDosSMSETSESSIONTYPE = 717;
       Ord_CHRBASEINIT = 718;
       Ord_OS2BASEINIT = 718;
       Ord_MouDosGETPTRSHAPE = 719;
       Ord_MouDosSETPTRSHAPE = 720;
       Ord_MouDosGETNUMMICKEYS = 721;
       Ord_MouDosGETTHRESHOLD = 722;
       Ord_MouDosSHELLINIT = 723;
       Ord_MouDosGETSCALEFACT = 724;
       Ord_MouDosFLUSHQUE = 725;
       Ord_MouDosGETNUMBUTTONS = 726;
       Ord_MouDosCLOSE = 727;
       Ord_MouDosSETTHRESHOLD = 728;
       Ord_MouDosSETSCALEFACT = 729;
       Ord_MouDosGETNUMQUEEL = 730;
       Ord_MouDosDEREGISTER = 731;
       Ord_MouDosGETEVENTMASK = 732;
       Ord_MouDosSETEVENTMASK = 733;
       Ord_MouDosOPEN = 734;
       Ord_MouDosREMOVEPTR = 735;
       Ord_MouDosGETPTRPOS = 736;
       Ord_MouDosREADEVENTQUE = 737;
       Ord_MouDosSETPTRPOS = 738;
       Ord_MouDosGETDEVSTATUS = 739;
       Ord_MouDosSYNCH = 740;
       Ord_MouDosREGISTER = 741;
       Ord_MouDosSETDEVSTATUS = 742;
       Ord_MouDosDRAWPTR = 743;
       Ord_MouDosINITREAL = 744;
       Ord_KbdDosSETCUSTXT = 745;
       Ord_KbdDosPROCESSINIT = 746;
       Ord_KbdDosGETCP = 747;
       Ord_KbdDosCHARIN = 748;
       Ord_KbdDosSETCP = 749;
       Ord_KbdDosLOADINSTANCE = 750;
       Ord_KbdDosSYNCH = 751;
       Ord_KbdDosREGISTER = 752;
       Ord_KbdDosSTRINGIN = 753;
       Ord_KbdDosGETSTATUS = 754;
       Ord_KbdDosSETSTATUS = 755;
       Ord_KbdDosGETFOCUS = 756;
       Ord_KbdDosFLUSHBUFFER = 757;
       Ord_KbdDosXLATE = 758;
       Ord_KbdDosSWITCHFGND = 759;
       Ord_KbdDosSHELLINIT = 760;
       Ord_KbdDosCLOSE = 761;
       Ord_KbdDosFREEFOCUS = 762;
       Ord_KbdDosFREE = 763;
       Ord_KbdDosDEREGISTER = 764;
       Ord_KbdDosSETFGND = 765;
       Ord_KbdDosPEEK = 766;
       Ord_KbdDosOPEN = 767;
       Ord_KbdDosGETHWID = 768;
       Ord_KbdDosSETHWID = 769;
       Ord_VioDosENDPOPUP = 770;
       Ord_VioDosGETPHYSBUF = 771;
       Ord_VioDosGETANSI = 772;
       Ord_VioDosFREE = 773;
       Ord_VioDosSETANSI = 774;
       Ord_VioDosDEREGISTER = 775;
       Ord_VioDosSCROLLUP = 776;
       Ord_VioDosPRTSC = 777;
       Ord_VioDosGETCURPOS = 778;
       Ord_VioDosWRTCELLSTR = 779;
       Ord_VioDosPOPUP = 780;
       Ord_VioDosSCROLLRT = 781;
       Ord_VioDosWRTCHARSTR = 782;
       Ord_VioDosAVS_PRTSC = 783;
       Ord_VioDosSETCURPOS = 784;
       Ord_VioDosSRFUNBLOCK = 785;
       Ord_VioDosSRFBLOCK = 786;
       Ord_VioDosSCRUNLOCK = 787;
       Ord_VioDosWRTTTY = 788;
       Ord_VioDosSAVE = 789;
       Ord_VioDosGETMODE = 790;
       Ord_VioDosSETMODE = 791;
       Ord_VioDosSCRLOCK = 792;
       Ord_VioDosREADCELLSTR = 793;
       Ord_VioDosSAVREDRAWWAIT = 794;
       Ord_VioDosWRTNATTR = 795;
       Ord_VioDosGETCURTYPE = 796;
       Ord_VioDosSAVREDRAWUNDO = 797;
       Ord_VioDosGETFONT = 798;
       Ord_VioDosREADCHARSTR = 799;
       Ord_VioDosGETBUF = 800;
       Ord_VioDosSETCURTYPE = 801;
       Ord_VioDosSETFONT = 802;
       Ord_VioDosHETINIT = 803;
       Ord_VioDosMODEUNDO = 804;
       Ord_VioDosSSWSWITCH = 805;
       Ord_VioDosMODEWAIT = 806;
       Ord_VioDosAVS_PRTSCTOGGLE = 807;
       Ord_VioDosGETCP = 808;
       Ord_VioDosRESTORE = 809;
       Ord_VioDosSETCP = 810;
       Ord_VioDosSHOWBUF = 811;
       Ord_VioDosSCROLLLF = 812;
       Ord_VioDosREGISTER = 813;
       Ord_VioDosGETCONFIG = 814;
       Ord_VioDosSCROLLDN = 815;
       Ord_VioDosWRTCHARSTRATT = 816;
       Ord_VioDosGETSTATE = 817;
       Ord_VioDosPRTSCTOGGLE = 818;
       Ord_VioDosSETSTATE = 819;
       Ord_VioDosWRTNCELL = 820;
       Ord_VioDosWRTNCHAR = 821;
       Ord_VioDosSHELLINIT = 822;
       Ord_VioDosASSOCIATE = 823;
       Ord_VioDosCREATEPS = 824;
       Ord_VioDosDELETESETID = 825;
       Ord_VioDosGETDEVICECELLSIZE = 826;
       Ord_VioDosGETORG = 827;
       Ord_VioDosCREATELOGFONT = 828;
       Ord_VioDosDESTROYPS = 829;
       Ord_VioDosQUERYSETIDS = 830;
       Ord_VioDosSETORG = 831;
       Ord_VioDosQUERYFONTS = 832;
       Ord_VioDosSETDEVICECELLSIZE = 833;
       Ord_VioDosSHOWPS = 834;
       Ord_VioDosGETPSADDRESS = 835;
       Ord_VioDosQUERYCONSOLE = 836;
       Ord_VioDosREDRAWSIZE = 837;
       Ord_VioDosGLOBALREG = 838;
       Ord_XVioDosSETCASTATE = 839;
       Ord_XVioDosCHECKCHARTYPE = 840;
       Ord_XVioDosDESTROYCA = 841;
       Ord_XVioDosCREATECA = 842;
       Ord_VioDosCHECKCHARTYPE = 843;
       Ord_XVioDosGETCASTATE = 844;
       Ord_BvsDosMAIN = 845;
       Ord_BvsDosREDRAWSIZE = 846;
       Ord_BvsDosGETPTRDRAWNAME = 847;
       Ord_AnsiDosINJECT = 848;
       Ord_AnsiDosKEYDEF = 849;
       Ord_AnsiDosINTERP = 850;
       Ord_BksDosMAIN = 851;
       Ord_BmsDosMAIN = 852;
       Ord_MouDosGETHOTKEY = 853;
       Ord_MouDosSETHOTKEY = 854;
       Ord_SMGDosSMSYSINIT = 855;
       Ord_SMGQHKEYBDHANDLE = 856;
       Ord_SMGQHMOUSEHANDLE = 857;
       Ord_CHRQueueRamSem = 858;
       Ord_SMQueueRamSem = 858;
       Ord_CHRArray = 859;
       Ord_SMArray = 859;
       Ord_CHRPIDArray = 860;
       Ord_SMPIDArray = 860;
       Ord_CHRInitialized = 861;
       Ord_SMInitialized = 861;
       Ord_CHRArraySize = 862;
       Ord_SMArraySize = 862;
       Ord_CHRBVSGLOBAL = 863;
       Ord_BVSGLOBAL = 863;
       Ord_CHRSMGINSTANCE = 864;
       Ord_SMGINSTANCE = 864;
       Ord_CHRBVHINSTANCE = 865;
       Ord_BVHINSTANCE = 865;
       Ord_THK32ALIASMEM = 866;
       Ord_THK32FREEALIAS = 867;
       Ord_THK32ALLOCVARLEN = 868;
       Ord_THK32HANDLEBOUNDARY = 869;
       Ord_THK32HANDLESTRING = 870;
       Ord_THK32DEALLOC = 871;
       Ord_THK32XHNDLR = 872;
       Ord_Dos32SETEXTLIBPATH = 873;
       Ord_Dos32QUERYEXTLIBPATH = 874;
       Ord_Dos32PM16SEMRST = 875;
       Ord_Dos32SYSCTL = 876;
       Ord_Dos32QUERYTHREADCONTEXT = 877;
       Ord_DosSGQUERYTOPMOST = 878;
       Ord_Dos32PERFSYSCALL = 976;
       Ord_Dos32OPENL = 981;
       Ord_Dos32PROTECTOPENL = 982;
       Ord_Dos32PROTECTSETFILELOCKSL = 983;
       Ord_Dos32PROTECTSETFILEPTRL = 984;
       Ord_Dos32PROTECTSETFILESIZEL = 985;
       Ord_Dos32SETFILELOCKSL = 986;
       Ord_Dos32CANCELLOCKREQUESTL = 987;
       Ord_Dos32SETFILEPTRL = 988;
       Ord_Dos32SETFILESIZEL = 989;
       Ord_Dos32LISTIOL = 990;
{
xxx       Ord_DosFSRAMSEMREQUEST2 = 991;
xxx       Ord_DosFSRAMSEMCLEAR2 = 992;
}
       Ord_Dos32SystemService = 995;
       Ord_Dos32LISTIO = 996;
       Ord_Dos32CREATETHREAD2 = 997;
       Ord_DosSETEXTLIBPATH = 998;
       Ord_DosQUERYEXTLIBPATH = 999;
{
xxx  1000 T32EXITLIST 
xxx  1001 T32ALLOCPROTECTEDMEM 
xxx  1002 T32ALIASMEM 
xxx  1003 T32ALLOCMEM 
xxx  1004 T32ALLOCSHAREDMEM 
xxx  1005 T32GETNAMEDSHAREDMEM 
xxx  1006 T32GETSHAREDMEM 
xxx  1007 T32GIVESHAREDMEM 
xxx  1008 T32FREEMEM 
xxx  1009 T32SETMEM 
xxx  1010 T32QUERYMEM 
xxx  1011 T32QUERYMEMSTATE 
xxx  1012 T32OPENVDD 
xxx  1013 T32REQUESTVDD 
xxx  1014 T32CLOSEVDD 
xxx  1015 T32CREATETHREAD 
xxx  1016 T32DYNAMICTRACE 
xxx  1017 T32DEBUG 
xxx  1018 T32QUERYPROCADDR 
xxx  1019 T32CREATEEVENTSEM 
xxx  1020 T32OPENEVENTSEM 
xxx  1021 T32CLOSEEVENTSEM 
xxx  1022 T32RESETEVENTSEM 
xxx  1023 T32POSTEVENTSEM 
xxx  1024 T32WAITEVENTSEM 
xxx  1025 T32QUERYEVENTSEM 
xxx  1026 T32CREATEMUTEXSEM 
xxx  1027 T32OPENMUTEXSEM 
xxx  1028 T32CLOSEMUTEXSEM 
xxx  1029 T32REQUESTMUTEXSEM 
xxx  1030 T32RELEASEMUTEXSEM 
xxx  1031 T32QUERYMUTEXSEM 
xxx  1032 T32CREATEMUXWAITSEM 
xxx  1033 T32OPENMUXWAITSEM 
xxx  1034 T32CLOSEMUXWAITSEM 
xxx  1035 T32WAITMUXWAITSEM 
xxx  1036 T32ADDMUXWAITSEM 
xxx  1037 T32DELETEMUXWAITSEM 
xxx  1038 T32QUERYMUXWAITSEM 
xxx  1039 T32QUERYSYSINFO 
xxx  1040 T32WAITTHREAD 
xxx  1041 T32GETRESOURCE 
xxx  1042 T32FREERESOURCE 
xxx  1043 T32EXCEPTIONCALLBACK 
xxx  1044 T32QUERYPAGEUSAGE 
xxx  1045 T32FORCESYSTEMDUMP 
xxx  1046 TI32ASYNCTIMER 
xxx  1047 TI32STARTTIMER 
xxx  1048 T32QUERYABIOSSUPPORT 
xxx  1049 T32QUERYMODFROMEIP 
xxx  1050 T32ALIASPERFCTRS 
xxx  1051 T32CONFIGUREPERF 
xxx  1052 T32DECONPERF 
xxx  1053 T32REGISTERPERFCTRS 
xxx  1054 T32QUERYSYSSTATE 
xxx  1055 T32IREAD 
xxx  1056 T32IWRITE 
xxx  1057 T32TMRQUERYFREQ 
xxx  1058 T32TMRQUERYTIME 
xxx  1059 T32IMONREAD 
xxx  1060 T32IMONWRITE 
xxx  1061 T32QUERYRESOURCESIZE 
xxx  1062 T32PROFILE 
xxx  1063 T32SETSIGNALEXCEPTIONFOCUS 
xxx  1064 T32SENDSIGNALEXCEPTION 
xxx  1065 T32STARTTIMER 
xxx  1066 T32STOPTIMER 
xxx  1067 T32ASYNCTIMER 
xxx  1068 T32INITIALIZEPORTHOLE 
xxx  1069 T32QUERYHEADERINFO 
xxx  1070 T32QUERYPROCTYPE 
xxx  1071 T32IEXITMUSTCOMPLETE 
xxx  1072 T32ICACHEMODULE 
xxx  1073 T32DLLTERM 
xxx  1074 T32IRAISEEXCEPTION 
xxx  1075 T32ACKNOWLEDGESIGNALEXCEPTION 
xxx  1076 T32QUERYDOSPROPERTY 
xxx  1077 T32SETDOSPROPERTY 
xxx  1078 T32SETFILELOCKS 
xxx  1079 T32CANCELLOCKREQUEST 
xxx  1080 T32KILLTHREAD 
xxx  1081 TQUERYRASINFO 
xxx  1082 T32DUMPPROCESS 
xxx  1083 T32SUPPRESSPOPUPS 
xxx  1084 T32IPROTECTWRITE 
xxx  1085 T32PROTECTSETFILELOCKS 
xxx  1086 T32IPROTECTREAD 
xxx  1087 T32PMPOSTEVENTSEM 
xxx  1088 T32PMWAITEVENTSEM 
xxx  1089 T32PMREQUESTMUTEXSEM 
xxx  1090 T32PMWAITMUXWAITSEM 
xxx  1091 T32PM16SEMCHK 
xxx  1092 T32ALLOCTHREADLOCALMEMORY 
xxx  1093 T32FREETHREADLOCALMEMORY 
xxx  1094 T32SETEXTLIBPATH 
xxx  1095 T32QUERYEXTLIBPATH 
xxx  1096 T32PM16SEMRST 
xxx  1097 T32VERIFYPIDTID 
xxx  1098 T32SYSCTL 
xxx  1099 T32QUERYTHREADCONTEXT 
xxx  1101 T32PERFSYSCALL 
xxx  1102 T32LISTIO 
xxx  1103 T32PMR3WAITEVENTSEM 
xxx  1104 T32PMR3POSTEVENTSEM 
xxx  1105 T32CREATETHREAD2 
xxx  1106 T32PROTECTSETFILELOCKSL 
xxx  1107 T32IPROTECTSETFILEPTRL 
xxx  1108 T32SETFILELOCKSL 
xxx  1109 T32CANCELLOCKREQUESTL 
xxx  1110 T32ISETFILEPTRL 
xxx  1111 T32LISTIOL 
xxx  1116 T32ISETFILEPTR 
xxx  1117 T32IPROTECTSETFILEPTR
}
(* PMGPI.DLL *)
       Ord_DevOPENDC = 1;
       Ord_DevCLOSEDC = 2;
       Ord_DevPOSTDEVICEMODES = 3;
       Ord_DevESCAPE = 4;
       Ord_DevQUERYHARDCOPYCAPS = 5;
       Ord_DevQUERYCAPS = 6;
       Ord_GpiCREATEPS = 7;
       Ord_GpiQUERYPS = 9;
       Ord_GpiDESTROYPS = 10;
       Ord_GpiRESETPS = 11;
       Ord_GpiSAVEPS = 12;
       Ord_GpiRESTOREPS = 13;
       Ord_GpiASSOCIATE = 14;
       Ord_GpiERRORSEGMENTDATA = 15;
       Ord_GpiERASE = 16;
       Ord_GpiSETDRAWCONTROL = 17;
       Ord_GpiQUERYDRAWCONTROL = 18;
       Ord_GpiDRAWCHAIN = 19;
       Ord_GpiDRAWFROM = 20;
       Ord_GpiDRAWSEGMENT = 21;
       Ord_GpiSETSTOPDRAW = 22;
       Ord_GpiQUERYSTOPDRAW = 23;
       Ord_GpiREMOVEDYNAMICS = 24;
       Ord_GpiDRAWDYNAMICS = 25;
       Ord_GpiSETDRAWINGMODE = 26;
       Ord_GpiQUERYDRAWINGMODE = 27;
       Ord_GpiGETDATA = 28;
       Ord_GpiPUTDATA = 29;
       Ord_GpiSETPICKAPERTURESIZE = 30;
       Ord_GpiQUERYPICKAPERTURESIZE = 31;
       Ord_GpiSETPICKAPERTUREPOSITION = 32;
       Ord_GpiQUERYPICKAPERTUREPOSITION = 33;
       Ord_GpiSETTAG = 34;
       Ord_GpiQUERYTAG = 35;
       Ord_GpiCORRELATECHAIN = 36;
       Ord_GpiCORRELATEFROM = 37;
       Ord_GpiCORRELATESEGMENT = 38;
       Ord_GpiRESETBOUNDARYDATA = 42;
       Ord_GpiQUERYBOUNDARYDATA = 43;
       Ord_GpiOPENSEGMENT = 44;
       Ord_GpiCLOSESEGMENT = 46;
       Ord_GpiDELETESEGMENT = 47;
       Ord_GpiDELETESEGMENTS = 48;
       Ord_GpiQUERYSEGMENTNAMES = 49;
       Ord_GpiSETINITIALSEGMENTATTRS = 51;
       Ord_GpiQUERYINITIALSEGMENTATTRS = 52;
       Ord_GpiSETSEGMENTATTRS = 53;
       Ord_GpiQUERYSEGMENTATTRS = 54;
       Ord_GpiSETSEGMENTPRIORITY = 55;
       Ord_GpiQUERYSEGMENTPRIORITY = 56;
       Ord_GpiSETEDITMODE = 57;
       Ord_GpiQUERYEDITMODE = 58;
       Ord_GpiSETELEMENTPOINTER = 59;
       Ord_GpiQUERYELEMENTPOINTER = 60;
       Ord_GpiOFFSETELEMENTPOINTER = 61;
       Ord_GpiDELETEELEMENT = 62;
       Ord_GpiDELETEELEMENTRANGE = 63;
       Ord_GpiLABEL = 64;
       Ord_GpiSETELEMENTPOINTERATLABEL = 65;
       Ord_GpiDELETEELEMENTSBETWEENLABELS = 66;
       Ord_GpiQUERYELEMENTTYPE = 67;
       Ord_GpiQUERYELEMENT = 68;
       Ord_GpiELEMENT = 69;
       Ord_GpiBEGINELEMENT = 70;
       Ord_GpiENDELEMENT = 71;
       Ord_GpiSETSEGMENTTRANSFORMMATRIX = 78;
       Ord_GpiQUERYSEGMENTTRANSFORMMATRIX = 79;
       Ord_GpiSETMODELTRANSFORMMATRIX = 80;
       Ord_GpiQUERYMODELTRANSFORMMATRIX = 81;
       Ord_GpiCALLSEGMENTMATRIX = 82;
       Ord_GpiSETDEFAULTVIEWMATRIX = 83;
       Ord_GpiQUERYDEFAULTVIEWMATRIX = 84;
       Ord_GpiSETPAGEVIEWPORT = 85;
       Ord_GpiQUERYPAGEVIEWPORT = 86;
       Ord_GpiSETVIEWINGTRANSFORMMATRIX = 87;
       Ord_GpiQUERYVIEWINGTRANSFORMMATRIX = 88;
       Ord_GpiSETGRAPHICSFIELD = 89;
       Ord_GpiQUERYGRAPHICSFIELD = 90;
       Ord_GpiSETVIEWINGLIMITS = 91;
       Ord_GpiQUERYVIEWINGLIMITS = 92;
       Ord_GpiCONVERT = 93;
       Ord_GpiSETATTRMODE = 94;
       Ord_GpiQUERYATTRMODE = 95;
       Ord_GpiPOP = 96;
       Ord_GpiSETATTRS = 97;
       Ord_GpiQUERYATTRS = 98;
       Ord_GpiCREATELOGCOLORTABLE = 99;
       Ord_GpiREALIZECOLORTABLE = 100;
       Ord_GpiUNREALIZECOLORTABLE = 101;
       Ord_GpiQUERYCOLORDATA = 102;
       Ord_GpiQUERYLOGCOLORTABLE = 103;
       Ord_GpiQUERYREALCOLORS = 104;
       Ord_GpiQUERYNEARESTCOLOR = 105;
       Ord_GpiQUERYCOLORINDEX = 106;
       Ord_GpiQUERYRGBCOLOR = 107;
       Ord_GpiSETCOLOR = 108;
       Ord_GpiQUERYCOLOR = 109;
       Ord_GpiSETBACKCOLOR = 110;
       Ord_GpiQUERYBACKCOLOR = 111;
       Ord_GpiSETMIX = 112;
       Ord_GpiQUERYMIX = 113;
       Ord_GpiSETBACKMIX = 114;
       Ord_GpiQUERYBACKMIX = 115;
       Ord_GpiSETLINETYPE = 116;
       Ord_GpiQUERYLINETYPE = 117;
       Ord_GpiSETLINEWIDTH = 118;
       Ord_GpiQUERYLINEWIDTH = 119;
       Ord_GpiSETLINEWIDTHGEOM = 120;
       Ord_GpiQUERYLINEWIDTHGEOM = 121;
       Ord_GpiSETLINEEND = 122;
       Ord_GpiQUERYLINEEND = 123;
       Ord_GpiSETLINEJOIN = 124;
       Ord_GpiQUERYLINEJOIN = 125;
       Ord_GpiSETCURRENTPOSITION = 126;
       Ord_GpiQUERYCURRENTPOSITION = 127;
       Ord_GpiMOVE = 128;
       Ord_GpiLINE = 129;
       Ord_GpiPOLYLINE = 130;
       Ord_GpiBOX = 131;
       Ord_GpiPTVISIBLE = 132;
       Ord_GpiRECTVISIBLE = 133;
       Ord_GpiSETARCPARAMS = 134;
       Ord_GpiQUERYARCPARAMS = 135;
       Ord_GpiPOINTARC = 136;
       Ord_GpiFULLARC = 137;
       Ord_GpiPARTIALARC = 138;
       Ord_GpiPOLYFILLET = 139;
       Ord_GpiPOLYFILLETSHARP = 140;
       Ord_GpiPOLYSPLINE = 141;
       Ord_GpiSETBITMAPID = 142;
       Ord_GpiQUERYBITMAPHANDLE = 143;
       Ord_GpiSETPATTERNSET = 144;
       Ord_GpiQUERYPATTERNSET = 145;
       Ord_GpiSETPATTERN = 146;
       Ord_GpiQUERYPATTERN = 147;
       Ord_GpiSETPATTERNREFPOINT = 148;
       Ord_GpiQUERYPATTERNREFPOINT = 149;
       Ord_GpiBEGINAREA = 150;
       Ord_GpiENDAREA = 151;
       Ord_GpiLOADFONTS = 152;
       Ord_GpiUNLOADFONTS = 153;
       Ord_GpiCREATELOGFONT = 154;
       Ord_GpiDELETESETID = 155;
       Ord_GpiQUERYNUMBERSETIDS = 156;
       Ord_GpiQUERYSETIDS = 157;
       Ord_GpiQUERYFONTS = 158;
       Ord_GpiQUERYFONTMETRICS = 159;
       Ord_GpiQUERYKERNINGPAIRS = 160;
       Ord_GpiQUERYWIDTHTABLE = 161;
       Ord_GpiSETCP = 162;
       Ord_GpiQUERYCP = 163;
       Ord_GpiQUERYTEXTBOX = 164;
       Ord_DevQUERYDEVICENAMES = 165;
       Ord_GpiQUERYDEFCHARBOX = 166;
       Ord_GpiQUERYFONTFILEDESCRIPTIONS = 167;
       Ord_GpiSETCHARSET = 168;
       Ord_GpiQUERYCHARSET = 169;
       Ord_GpiSETCHARBOX = 170;
       Ord_GpiQUERYCHARBOX = 171;
       Ord_GpiSETCHARANGLE = 172;
       Ord_GpiQUERYCHARANGLE = 173;
       Ord_GpiSETCHARSHEAR = 174;
       Ord_GpiQUERYCHARSHEAR = 175;
       Ord_GpiSETCHARDIRECTION = 176;
       Ord_GpiQUERYCHARDIRECTION = 177;
       Ord_GpiSETCHARMODE = 178;
       Ord_GpiQUERYCHARMODE = 179;
       Ord_GpiCHARSTRING = 180;
       Ord_GpiCHARSTRINGAT = 181;
       Ord_GpiCHARSTRINGPOS = 182;
       Ord_GpiCHARSTRINGPOSAT = 183;
       Ord_GpiSETMARKERSET = 184;
       Ord_GpiQUERYMARKERSET = 185;
       Ord_GpiSETMARKER = 186;
       Ord_GpiQUERYMARKER = 187;
       Ord_GpiSETMARKERBOX = 188;
       Ord_GpiQUERYMARKERBOX = 189;
       Ord_GpiMARKER = 190;
       Ord_GpiPOLYMARKER = 191;
       Ord_GpiIMAGE = 192;
       Ord_GpiCREATEBITMAP = 196;
       Ord_GpiDELETEBITMAP = 197;
       Ord_GpiSETBITMAP = 198;
       Ord_GpiSETBITMAPDIMENSION = 199;
       Ord_GpiQUERYBITMAPDIMENSION = 200;
       Ord_GpiQUERYDEVICEBITMAPFORMATS = 201;
       Ord_GpiQUERYBITMAPPARAMETERS = 202;
       Ord_GpiSETBITMAPBITS = 203;
       Ord_GpiQUERYBITMAPBITS = 204;
       Ord_GpiBITBLT = 205;
       Ord_GpiSETPEL = 206;
       Ord_GpiQUERYPEL = 207;
       Ord_GpiCREATEREGION = 208;
       Ord_GpiSETREGION = 209;
       Ord_GpiDESTROYREGION = 210;
       Ord_GpiCOMBINEREGION = 211;
       Ord_GpiEQUALREGION = 212;
       Ord_GpiOFFSETREGION = 213;
       Ord_GpiPTINREGION = 214;
       Ord_GpiRECTINREGION = 215;
       Ord_GpiQUERYREGIONBOX = 216;
       Ord_GpiQUERYREGIONRECTS = 217;
       Ord_GpiSETCLIPREGION = 218;
       Ord_GpiQUERYCLIPREGION = 219;
       Ord_GpiQUERYCLIPBOX = 220;
       Ord_GpiINTERSECTCLIPRECTANGLE = 221;
       Ord_GpiEXCLUDECLIPRECTANGLE = 222;
       Ord_GpiOFFSETCLIPREGION = 223;
       Ord_GpiPAINTREGION = 224;
       Ord_GpiLOADMETAFILE = 225;
       Ord_GpiCOPYMETAFILE = 226;
       Ord_GpiPLAYMETAFILE = 227;
       Ord_GpiSAVEMETAFILE = 228;
       Ord_GpiDELETEMETAFILE = 229;
       Ord_GpiQUERYMETAFILEBITS = 230;
       Ord_GpiSETMETAFILEBITS = 231;
       Ord_GpiQUERYMETAFILELENGTH = 232;
       Ord_GpiSETCLIPPATH = 233;
       Ord_GpiBEGINPATH = 234;
       Ord_GpiENDPATH = 235;
       Ord_GpiCLOSEFIGURE = 236;
       Ord_GpiMODIFYPATH = 237;
       Ord_GpiFILLPATH = 238;
       Ord_GpiSTARTREQUEST = 239;
       Ord_GpiQUERYDEVICE = 240;
       Ord_DevSTDOPEN = 244;
       Ord_GpiQUERYCHARSTRINGPOS = 245;
       Ord_GpiQUERYCHARSTRINGPOSAT = 246;
       Ord_GpiSETPS = 248;
       Ord_SEGSGWOPENSEGMENTWINDOW = 249;
       Ord_MTENDREADREQUEST = 250;
       Ord_MTGETDESCRIPTION = 251;
       Ord_FMTORDERTABLE = 252;
       Ord_MTGETCODEPAGE = 253;
       Ord_MTGETLCT = 254;
       Ord_MTGETGDDINFO = 255;
       Ord_FMTCONVERTGOCAPOLY = 256;
       Ord_MTGETFIRSTFONT = 257;
       Ord_SEGSGWNEWPARTDATA = 258;
       Ord_MTSTARTREADREQUEST = 259;
       Ord_MTGETFIRSTGRAPHICSDATA = 260;
       Ord_MTGETNEXTFONT = 261;
       Ord_MTGETNEXTGRAPHICSDATA = 262;
       Ord_GpiLOADPUBLICFONTS = 263;
       Ord_GpiUNLOADPUBLICFONTS = 264;
       Ord_GpiCOMMENT = 268;
       Ord_GpiWCBITBLT = 269;
       Ord_GpiSTROKEPATH = 270;
       Ord_SEGSGWNEXTORDERF = 271;
       Ord_GpiOUTLINEPATH = 274;
       Ord_GpiSETDEFTAG = 275;
       Ord_GpiQUERYDEFTAG = 276;
       Ord_GpiSETDEFATTRS = 277;
       Ord_GpiQUERYDEFATTRS = 278;
       Ord_GpiSETDEFVIEWINGLIMITS = 279;
       Ord_GpiQUERYDEFVIEWINGLIMITS = 280;
       Ord_GpiSETDEFARCPARAMS = 281;
       Ord_GpiQUERYDEFARCPARAMS = 282;
       Ord_GpiTRANSLATE = 283;
       Ord_GpiSCALE = 284;
       Ord_GpiROTATE = 285;
       Ord_GpiPOLYLINEDISJOINT = 286;
       Ord_GpiPATHTOREGION = 287;
       Ord_GpiFLOODFILL = 289;
       Ord_GpiDRAWBITS = 290;
       Ord_GpiQUERYBITMAPINFOHEADER = 291;
       Ord_GpiQUERYLOGICALFONT = 292;
       Ord_GpiQUERYFACESTRING = 293;
       Ord_GpiQUERYFONTACTION = 294;
       Ord_GpiCREATEPALETTE = 295;
       Ord_GpiDELETEPALETTE = 296;
       Ord_GpiSELECTPALETTE = 297;
       Ord_GpiANIMATEPALETTE = 298;
       Ord_GpiSETPALETTEENTRIES = 299;
       Ord_GpiQUERYPALETTE = 300;
       Ord_GpiQUERYPALETTEINFO = 301;
       Ord_GpiQUERYCHAREXTRA = 302;
       Ord_GpiSETCHAREXTRA = 303;
       Ord_GpiQUERYCHARBREAKEXTRA = 304;
       Ord_GpiSETCHARBREAKEXTRA = 305;
       Ord_GpiFRAMEREGION = 306;
       Ord_GpiCONVERTWITHMATRIX = 307;
       Ord_Gpi32ASSOCIATE = 351;
       Ord_Gpi32BEGINAREA = 352;
       Ord_Gpi32BEGINELEMENT = 353;
       Ord_Gpi32BEGINPATH = 354;
       Ord_Gpi32BITBLT = 355;
       Ord_Gpi32BOX = 356;
       Ord_Gpi32CALLSEGMENTMATRIX = 357;
       Ord_Gpi32CHARSTRING = 358;
       Ord_Gpi32CHARSTRINGAT = 359;
       Ord_Gpi32CLOSEFIGURE = 360;
       Ord_Gpi32CLOSESEGMENT = 361;
       Ord_Gpi32COMBINEREGION = 362;
       Ord_Gpi32COMMENT = 363;
       Ord_Gpi32CONVERT = 364;
       Ord_Gpi32COPYMETAFILE = 365;
       Ord_Gpi32CORRELATECHAIN = 366;
       Ord_Gpi32CORRELATEFROM = 367;
       Ord_Gpi32CREATELOGFONT = 368;
       Ord_Gpi32CREATEPS = 369;
       Ord_Gpi32CREATEREGION = 370;
       Ord_Gpi32DELETEBITMAP = 371;
       Ord_Gpi32DELETEELEMENT = 372;
       Ord_Gpi32DELETEELEMENTRANGE = 373;
       Ord_Gpi32DELETEELEMENTSBETWEENLABE = 374;
       Ord_Gpi32DELETEMETAFILE = 375;
       Ord_Gpi32DELETESEGMENT = 376;
       Ord_Gpi32DELETESEGMENTS = 377;
       Ord_Gpi32DELETESETID = 378;
       Ord_Gpi32DESTROYPS = 379;
       Ord_Gpi32DRAWCHAIN = 380;
       Ord_Gpi32DRAWDYNAMICS = 381;
       Ord_Gpi32DRAWFROM = 382;
       Ord_Gpi32DRAWSEGMENT = 383;
       Ord_Gpi32ELEMENT = 384;
       Ord_Gpi32ENDAREA = 385;
       Ord_Gpi32ENDELEMENT = 386;
       Ord_Gpi32ENDPATH = 387;
       Ord_Gpi32EQUALREGION = 388;
       Ord_Gpi32ERASE = 389;
       Ord_Gpi32ERRORSEGMENTDATA = 390;
       Ord_Gpi32EXCLUDECLIPRECTANGLE = 391;
       Ord_Gpi32FILLPATH = 392;
       Ord_Gpi32FULLARC = 393;
       Ord_Gpi32GETDATA = 394;
       Ord_Gpi32IMAGE = 395;
       Ord_Gpi32INTERSECTCLIPRECTANGLE = 396;
       Ord_Gpi32LABEL = 397;
       Ord_Gpi32LINE = 398;
       Ord_Gpi32LOADBITMAP = 399;
       Ord_Gpi32LOADFONTS = 400;
       Ord_Gpi32LOADMETAFILE = 401;
       Ord_Gpi32MARKER = 402;
       Ord_Gpi32MODIFYPATH = 403;
       Ord_Gpi32MOVE = 404;
       Ord_Gpi32OFFSETCLIPREGION = 405;
       Ord_Gpi32OFFSETELEMENTPOINTER = 406;
       Ord_Gpi32OFFSETREGION = 407;
       Ord_Gpi32OPENSEGMENT = 408;
       Ord_Gpi32PAINTREGION = 409;
       Ord_Gpi32PLAYMETAFILE = 411;
       Ord_Gpi32POINTARC = 412;
       Ord_Gpi32POLYFILLET = 413;
       Ord_Gpi32POLYFILLETSHARP = 414;
       Ord_Gpi32POLYLINE = 415;
       Ord_Gpi32POLYMARKER = 416;
       Ord_Gpi32POLYSPLINE = 417;
       Ord_Gpi32POP = 418;
       Ord_Gpi32PTINREGION = 419;
       Ord_Gpi32PTVISIBLE = 420;
       Ord_Gpi32PUTDATA = 421;
       Ord_Gpi32QUERYARCPARAMS = 422;
       Ord_Gpi32QUERYATTRMODE = 423;
       Ord_Gpi32QUERYBACKCOLOR = 424;
       Ord_Gpi32QUERYBACKMIX = 425;
       Ord_Gpi32QUERYBITMAPDIMENSION = 426;
       Ord_Gpi32QUERYBITMAPHANDLE = 427;
       Ord_Gpi32QUERYBOUNDARYDATA = 428;
       Ord_Gpi32QUERYCHARANGLE = 429;
       Ord_Gpi32QUERYCHARBOX = 430;
       Ord_Gpi32QUERYCHARDIRECTION = 431;
       Ord_Gpi32QUERYCHARMODE = 432;
       Ord_Gpi32QUERYCHARSET = 433;
       Ord_Gpi32QUERYCHARSHEAR = 434;
       Ord_Gpi32QUERYCLIPBOX = 435;
       Ord_Gpi32QUERYCLIPREGION = 436;
       Ord_Gpi32QUERYCOLOR = 437;
       Ord_Gpi32QUERYCOLORDATA = 438;
       Ord_Gpi32QUERYCOLORINDEX = 439;
       Ord_Gpi32QUERYCP = 440;
       Ord_Gpi32QUERYCURRENTPOSITION = 441;
       Ord_Gpi32QUERYDEFCHARBOX = 442;
       Ord_Gpi32QUERYDEFAULTVIEWMATRIX = 443;
       Ord_Gpi32QUERYDEVICE = 444;
       Ord_Gpi32QUERYDEVICEBITMAPFORMATS = 445;
       Ord_Gpi32QUERYDRAWCONTROL = 446;
       Ord_Gpi32QUERYDRAWINGMODE = 447;
       Ord_Gpi32QUERYEDITMODE = 448;
       Ord_Gpi32QUERYELEMENT = 449;
       Ord_Gpi32QUERYELEMENTPOINTER = 450;
       Ord_Gpi32QUERYELEMENTTYPE = 451;
       Ord_Gpi32QUERYFONTFILEDESCRIPTIONS = 452;
       Ord_Gpi32QUERYFONTMETRICS = 453;
       Ord_Gpi32QUERYGRAPHICSFIELD = 454;
       Ord_Gpi32QUERYINITIALSEGMENTATTRS = 455;
       Ord_Gpi32QUERYKERNINGPAIRS = 456;
       Ord_Gpi32QUERYLINEEND = 457;
       Ord_Gpi32QUERYLINEJOIN = 458;
       Ord_Gpi32QUERYLINETYPE = 459;
       Ord_Gpi32QUERYLINEWIDTH = 460;
       Ord_Gpi32QUERYLINEWIDTHGEOM = 461;
       Ord_Gpi32QUERYMARKER = 462;
       Ord_Gpi32QUERYMARKERBOX = 463;
       Ord_Gpi32QUERYMARKERSET = 464;
       Ord_Gpi32QUERYMETAFILEBITS = 465;
       Ord_Gpi32QUERYMETAFILELENGTH = 466;
       Ord_Gpi32QUERYMIX = 467;
       Ord_Gpi32QUERYMODELTRANSFORMMATRIX = 468;
       Ord_Gpi32QUERYNEARESTCOLOR = 469;
       Ord_Gpi32QUERYNUMBERSETIDS = 470;
       Ord_Gpi32QUERYPS = 471;
       Ord_Gpi32QUERYPAGEVIEWPORT = 472;
       Ord_Gpi32QUERYPATTERN = 473;
       Ord_Gpi32QUERYPATTERNREFPOINT = 474;
       Ord_Gpi32QUERYPATTERNSET = 475;
       Ord_Gpi32QUERYPEL = 476;
       Ord_Gpi32QUERYPICKAPERTUREPOSITION = 477;
       Ord_Gpi32QUERYPICKAPERTURESIZE = 478;
       Ord_Gpi32QUERYRGBCOLOR = 479;
       Ord_Gpi32QUERYREALCOLORS = 480;
       Ord_Gpi32QUERYREGIONBOX = 481;
       Ord_Gpi32QUERYSEGMENTATTRS = 482;
       Ord_Gpi32QUERYSEGMENTNAMES = 483;
       Ord_Gpi32QUERYSEGMENTPRIORITY = 484;
       Ord_Gpi32QUERYSEGMENTTRANSFORMMATR = 485;
       Ord_Gpi32QUERYSETIDS = 486;
       Ord_Gpi32QUERYSTOPDRAW = 487;
       Ord_Gpi32QUERYTAG = 488;
       Ord_Gpi32QUERYTEXTBOX = 489;
       Ord_Gpi32QUERYVIEWINGLIMITS = 490;
       Ord_Gpi32QUERYVIEWINGTRANSFORMMATR = 491;
       Ord_Gpi32QUERYWIDTHTABLE = 492;
       Ord_Gpi32RECTINREGION = 494;
       Ord_Gpi32RECTVISIBLE = 495;
       Ord_Gpi32REMOVEDYNAMICS = 496;
       Ord_Gpi32RESETBOUNDARYDATA = 497;
       Ord_Gpi32RESETPS = 498;
       Ord_Gpi32RESTOREPS = 499;
       Ord_Gpi32SAVEMETAFILE = 500;
       Ord_Gpi32SAVEPS = 501;
       Ord_Gpi32SETARCPARAMS = 502;
       Ord_Gpi32SETATTRMODE = 503;
       Ord_Gpi32SETBACKCOLOR = 504;
       Ord_Gpi32SETBACKMIX = 505;
       Ord_Gpi32SETBITMAP = 506;
       Ord_Gpi32SETBITMAPDIMENSION = 507;
       Ord_Gpi32SETBITMAPID = 508;
       Ord_Gpi32SETCHARANGLE = 509;
       Ord_Gpi32SETCHARBOX = 510;
       Ord_Gpi32SETCHARDIRECTION = 511;
       Ord_Gpi32SETCHARMODE = 512;
       Ord_Gpi32SETCHARSET = 513;
       Ord_Gpi32SETCHARSHEAR = 514;
       Ord_Gpi32SETCLIPPATH = 515;
       Ord_Gpi32SETCLIPREGION = 516;
       Ord_Gpi32SETCOLOR = 517;
       Ord_Gpi32SETCP = 518;
       Ord_Gpi32SETCURRENTPOSITION = 519;
       Ord_Gpi32SETDEFAULTVIEWMATRIX = 520;
       Ord_Gpi32SETDRAWCONTROL = 521;
       Ord_Gpi32SETDRAWINGMODE = 522;
       Ord_Gpi32SETEDITMODE = 523;
       Ord_Gpi32SETELEMENTPOINTER = 524;
       Ord_Gpi32SETELEMENTPOINTERATLABEL = 525;
       Ord_Gpi32SETGRAPHICSFIELD = 526;
       Ord_Gpi32SETINITIALSEGMENTATTRS = 527;
       Ord_Gpi32SETLINEEND = 528;
       Ord_Gpi32SETLINEJOIN = 529;
       Ord_Gpi32SETLINETYPE = 530;
       Ord_Gpi32SETLINEWIDTH = 531;
       Ord_Gpi32SETLINEWIDTHGEOM = 532;
       Ord_Gpi32SETMARKER = 533;
       Ord_Gpi32SETMARKERBOX = 534;
       Ord_Gpi32SETMARKERSET = 535;
       Ord_Gpi32SETMETAFILEBITS = 536;
       Ord_Gpi32SETMIX = 537;
       Ord_Gpi32SETMODELTRANSFORMMATRIX = 538;
       Ord_Gpi32SETPS = 539;
       Ord_Gpi32SETPAGEVIEWPORT = 540;
       Ord_Gpi32SETPATTERN = 541;
       Ord_Gpi32SETPATTERNREFPOINT = 542;
       Ord_Gpi32SETPATTERNSET = 543;
       Ord_Gpi32SETPEL = 544;
       Ord_Gpi32SETPICKAPERTUREPOSITION = 545;
       Ord_Gpi32SETREGION = 546;
       Ord_Gpi32SETSEGMENTATTRS = 547;
       Ord_Gpi32SETSEGMENTPRIORITY = 548;
       Ord_Gpi32SETSEGMENTTRANSFORMMATRIX = 549;
       Ord_Gpi32SETSTOPDRAW = 550;
       Ord_Gpi32SETTAG = 551;
       Ord_Gpi32SETVIEWINGLIMITS = 552;
       Ord_Gpi32SETVIEWINGTRANSFORMMATRIX = 553;
       Ord_Gpi32STROKEPATH = 554;
       Ord_Gpi32UNLOADFONTS = 555;
       Ord_Gpi32WCBITBLT = 557;
       Ord_Gpi32POLYLINEDISJOINT = 558;
       Ord_Gpi32PATHTOREGION = 559;
       Ord_Gpi32FLOODFILL = 560;
       Ord_Gpi32SUSPENDPLAY = 561;
       Ord_Gpi32RESUMEPLAY = 562;
       Ord_Gpi32OUTLINEPATH = 563;
       Ord_Gpi32TRANSLATE = 564;
       Ord_Gpi32SCALE = 565;
       Ord_Gpi32ROTATE = 566;
       Ord_Gpi32QUERYDEFARCPARAMS = 567;
       Ord_Gpi32QUERYDEFTAG = 568;
       Ord_Gpi32QUERYDEFVIEWINGLIMITS = 569;
       Ord_Gpi32SETDEFARCPARAMS = 570;
       Ord_Gpi32SETDEFTAG = 571;
       Ord_Gpi32SETDEFVIEWINGLIMITS = 572;
       Ord_Gpi32QUERYBITMAPPARAMETERS = 573;
       Ord_Gpi32QUERYLOGICALFONT = 574;
       Ord_Gpi32QUERYFACESTRING = 575;
       Ord_Gpi32QUERYFONTACTION = 576;
       Ord_Gpi32DELETEPALETTE = 577;
       Ord_Gpi32SELECTPALETTE = 578;
       Ord_Gpi32QUERYPALETTE = 579;
       Ord_Gpi32CHARSTRINGPOS = 580;
       Ord_Gpi32CHARSTRINGPOSAT = 581;
       Ord_Gpi32CORRELATESEGMENT = 582;
       Ord_Gpi32QUERYATTRS = 583;
       Ord_Gpi32QUERYCHARSTRINGPOS = 584;
       Ord_Gpi32QUERYCHARSTRINGPOSAT = 585;
       Ord_Gpi32QUERYFONTS = 586;
       Ord_Gpi32QUERYREGIONRECTS = 587;
       Ord_Gpi32SETATTRS = 588;
       Ord_Gpi32SETPICKAPERTURESIZE = 589;
       Ord_Gpi32QUERYDEFATTRS = 590;
       Ord_Gpi32SETDEFATTRS = 591;
       Ord_Gpi32CREATELOGCOLORTABLE = 592;
       Ord_Gpi32QUERYLOGCOLORTABLE = 593;
       Ord_Gpi32CREATEPALETTE = 594;
       Ord_Gpi32ANIMATEPALETTE = 595;
       Ord_Gpi32SETPALETTEENTRIES = 596;
       Ord_Gpi32QUERYPALETTEINFO = 597;
       Ord_Gpi32CREATEBITMAP = 598;
       Ord_Gpi32QUERYBITMAPBITS = 599;
       Ord_Gpi32QUERYBITMAPINFOHEADER = 601;
       Ord_Gpi32SETBITMAPBITS = 602;
       Ord_Gpi32DRAWBITS = 603;
       Ord_Dev32CLOSEDC = 604;
       Ord_Dev32ESCAPE = 605;
       Ord_Dev32QUERYCAPS = 606;
       Ord_Dev32QUERYDEVICENAMES = 607;
       Ord_Dev32QUERYHARDCOPYCAPS = 608;
       Ord_Dev32POSTDEVICEMODES = 609;
       Ord_Dev32OPENDC = 610;
       Ord_Gpi32DESTROYREGION = 611;
       Ord_Gpi32PARTIALARC = 612;
       Ord_Gpi32QUERYCHAREXTRA = 613;
       Ord_Gpi32SETCHAREXTRA = 614;
       Ord_Gpi32QUERYCHARBREAKEXTRA = 615;
       Ord_Gpi32SETCHARBREAKEXTRA = 616;
       Ord_Gpi32FRAMEREGION = 617;
       Ord_Gpi32CONVERTWITHMATRIX = 618;
       Ord_Gpi32LOADPUBLICFONTS = 622;
       Ord_Gpi32UNLOADPUBLICFONTS = 623;
(* PMWIN.DLL *)
       Ord_WinQUERYDESKTOPWINDOW = 1;
       Ord_WinQUERYOBJECTWINDOW = 2;
       Ord_WinREGISTERCLASS = 3;
       Ord_WinQUERYCLASSNAME = 4;
       Ord_WinQUERYCLASSINFO = 5;
       Ord_WinCREATEWINDOW = 6;
       Ord_WinDESTROYWINDOW = 7;
       Ord_WinSETWINDOWPOS = 8;
       Ord_WinSETMULTWINDOWPOS = 9;
       Ord_WinQUERYWINDOWPOS = 10;
       Ord_WinENABLEWINDOW = 11;
       Ord_WinISWINDOWENABLED = 12;
       Ord_WinSHOWWINDOW = 13;
       Ord_WinENABLEWINDOWUPDATE = 14;
       Ord_WinISWINDOWVISIBLE = 15;
       Ord_WinSETWINDOWTEXT = 16;
       Ord_WinQUERYWINDOWTEXT = 17;
       Ord_WinQUERYWINDOWTEXTLENGTH = 18;
       Ord_WinWINDOWFROMID = 19;
       Ord_WinMULTWINDOWFROMIDS = 20;
       Ord_WinISWINDOW = 21;
       Ord_WinISCHILD = 22;
       Ord_WinSETPARENT = 23;
       Ord_WinSETOWNER = 24;
       Ord_WinQUERYWINDOW = 25;
       Ord_WinQUERYWINDOWRECT = 26;
       Ord_WinQUERYWINDOWPROCESS = 27;
       Ord_WinSETWINDOWUSHORT = 28;
       Ord_WinQUERYWINDOWUSHORT = 29;
       Ord_WinSETWINDOWULONG = 30;
       Ord_WinQUERYWINDOWULONG = 31;
       Ord_WinBEGINENUMWINDOWS = 32;
       Ord_WinGETNEXTWINDOW = 33;
       Ord_WinENDENUMWINDOWS = 34;
       Ord_WinWINDOWFROMPOINT = 35;
       Ord_WinMAPWINDOWPOINTS = 36;
       Ord_WinSUBCLASSWINDOW = 37;
       Ord_WinLOCKWINDOW = 38;
       Ord_WinQUERYWINDOWLOCKCOUNT = 39;
       Ord_WinREGISTERWINDOWDESTROY = 40;
       Ord_WinOPENWINDOWDC = 41;
       Ord_WinGETSCREENPS = 42;
       Ord_WinGETPS = 43;
       Ord_WinRELEASEPS = 44;
       Ord_WinBEGINPAINT = 45;
       Ord_WinENDPAINT = 46;
       Ord_WinINVALIDATERECT = 47;
       Ord_WinINVALIDATEREGION = 48;
       Ord_WinVALIDATERECT = 49;
       Ord_WinVALIDATEREGION = 50;
       Ord_WinQUERYUPDATERECT = 51;
       Ord_WinQUERYUPDATEREGION = 52;
       Ord_WinUPDATEWINDOW = 53;
       Ord_WinEXCLUDEUPDATEREGION = 54;
       Ord_WinLOCKWINDOWUPDATE = 55;
       Ord_WinLOCKVISREGIONS = 56;
       Ord_WinWINDOWFROMDC = 57;
       Ord_WinCREATEMSGQUEUE = 58;
       Ord_WinDESTROYMSGQUEUE = 59;
       Ord_WinQUERYQUEUESTATUS = 60;
       Ord_WinSENDMSG = 61;
       Ord_WinBROADCASTMSG = 63;
       Ord_WinINSENDMSG = 64;
       Ord_WinGETMSG = 65;
       Ord_WinPEEKMSG = 66;
       Ord_WinWAITMSG = 67;
       Ord_WinDISPATCHMSG = 68;
       Ord_WinPOSTMSG = 69;
       Ord_WinPOSTQUEUEMSG = 70;
       Ord_WinQUERYMSGPOS = 71;
       Ord_WinQUERYMSGTIME = 72;
       Ord_WinCALLMSGFILTER = 73;
       Ord_WinSETMSGINTEREST = 74;
       Ord_WinSETCAPTURE = 75;
       Ord_WinQUERYCAPTURE = 76;
       Ord_WinSETFOCUS = 77;
       Ord_WinQUERYFOCUS = 78;
       Ord_WinSETACTIVEWINDOW = 79;
       Ord_WinQUERYACTIVEWINDOW = 80;
       Ord_WinSETSYSMODALWINDOW = 81;
       Ord_WinQUERYSYSMODALWINDOW = 82;
       Ord_WinISTHREADACTIVE = 83;
       Ord_WinSTARTTIMER = 84;
       Ord_WinSTOPTIMER = 85;
       Ord_WinGETCURRENTTIME = 86;
       Ord_WinLOADPOINTER = 87;
       Ord_WinCREATEPOINTER = 88;
       Ord_WinDESTROYPOINTER = 89;
       Ord_OldWinQUERYPOINTERINFO = 90;
       Ord_WinQUERYSYSPOINTER = 91;
       Ord_WinSETPOINTER = 92;
       Ord_WinQUERYPOINTER = 93;
       Ord_WinSHOWPOINTER = 94;
       Ord_WinSETPOINTERPOS = 95;
       Ord_WinQUERYPOINTERPOS = 96;
       Ord_WinGETSYSBITMAP = 97;
       Ord_WinCREATECURSOR = 98;
       Ord_WinDESTROYCURSOR = 99;
       Ord_WinSHOWCURSOR = 100;
       Ord_WinQUERYCURSORINFO = 101;
       Ord_WinLOADACCELTABLE = 102;
       Ord_WinCREATEACCELTABLE = 103;
       Ord_WinDESTROYACCELTABLE = 104;
       Ord_WinTRANSLATEACCEL = 105;
       Ord_WinSETACCELTABLE = 106;
       Ord_WinQUERYACCELTABLE = 107;
       Ord_WinCOPYACCELTABLE = 108;
       Ord_WinSETHOOK = 109;
       Ord_WinRELEASEHOOK = 110;
       Ord_WinOPENCLIPBRD = 111;
       Ord_WinCLOSECLIPBRD = 112;
       Ord_WinEMPTYCLIPBRD = 113;
       Ord_WinSETCLIPBRDOWNER = 114;
       Ord_WinQUERYCLIPBRDOWNER = 115;
       Ord_WinSETCLIPBRDDATA = 116;
       Ord_WinQUERYCLIPBRDDATA = 117;
       Ord_WinENUMCLIPBRDFMTS = 118;
       Ord_WinQUERYCLIPBRDFMTINFO = 119;
       Ord_WinSETCLIPBRDVIEWER = 120;
       Ord_WinQUERYCLIPBRDVIEWER = 121;
       Ord_WinLOADDLG = 122;
       Ord_WinCREATEDLG = 123;
       Ord_WinPROCESSDLG = 124;
       Ord_WinDLGBOX = 125;
       Ord_WinDISMISSDLG = 126;
       Ord_WinSENDDLGITEMMSG = 127;
       Ord_WinSETDLGITEMSHORT = 128;
       Ord_WinQUERYDLGITEMSHORT = 129;
       Ord_WinMAPDLGPOINTS = 130;
       Ord_WinSUBSTITUTESTRINGS = 132;
       Ord_WinENUMDLGITEM = 133;
       Ord_WinSETDLGITEMTEXT = 134;
       Ord_WinQUERYDLGITEMTEXT = 135;
       Ord_WinLOADMENU = 136;
       Ord_WinCREATEMENU = 137;
       Ord_WinALARM = 138;
       Ord_WinMESSAGEBOX = 139;
       Ord_WinCREATESTDWINDOW = 140;
       Ord_WinCREATEFRAMECONTROLS = 141;
       Ord_WinCALCFRAMERECT = 143;
       Ord_WinFLASHWINDOW = 144;
       Ord_WinGETMINPOSITION = 146;
       Ord_WinGETMAXPOSITION = 147;
       Ord_WinQUERYSYSVALUE = 149;
       Ord_WinSETSYSVALUE = 150;
       Ord_WinSETSYSCOLORS = 151;
       Ord_WinQUERYSYSCOLOR = 152;
       Ord_WinSCROLLWINDOW = 153;
       Ord_WinTRACKRECT = 154;
       Ord_WinSHOWTRACKRECT = 155;
       Ord_GpiLOADBITMAP = 156;
       Ord_WinLOADSTRING = 157;
       Ord_WinLOADMESSAGE = 158;
       Ord_WinSETRECTEMPTY = 159;
       Ord_WinSETRECT = 160;
       Ord_WinCOPYRECT = 161;
       Ord_WinISRECTEMPTY = 162;
       Ord_WinEQUALRECT = 163;
       Ord_WinPTINRECT = 164;
       Ord_WinOFFSETRECT = 165;
       Ord_WinINFLATERECT = 166;
       Ord_WinINTERSECTRECT = 167;
       Ord_WinUNIONRECT = 168;
       Ord_WinSUBTRACTRECT = 169;
       Ord_WinMAKERECT = 170;
       Ord_WinMAKEPOINTS = 171;
       Ord_WinINVERTRECT = 172;
       Ord_WinFILLRECT = 173;
       Ord_KbdPACKET = 174;
       Ord_WinDRAWPOINTER = 177;
       Ord_WinDEFWINDOWPROC = 178;
       Ord_WinDEFDLGPROC = 179;
       Ord_WinGETKEYSTATE = 211;
       Ord_WinGETPHYSKEYSTATE = 212;
       Ord_WinSETKEYBOARDSTATETABLE = 213;
       Ord_WinENABLEPHYSINPUT = 214;
       Ord_WinSETCP = 215;
       Ord_WinQUERYCP = 216;
       Ord_WinQUERYCPLIST = 217;
       Ord_WinCPTRANSLATESTRING = 218;
       Ord_WinCPTRANSLATECHAR = 219;
       Ord_WinCOMPARESTRINGS = 220;
       Ord_WinUPPER = 221;
       Ord_WinUPPERCHAR = 222;
       Ord_WinNEXTCHAR = 223;
       Ord_WinPREVCHAR = 224;
       Ord_WinCREATEHEAP = 225;
       Ord_WinDESTROYHEAP = 226;
       Ord_WinAVAILMEM = 227;
       Ord_WinALLOCMEM = 228;
       Ord_WinREALLOCMEM = 229;
       Ord_WinFREEMEM = 230;
       Ord_WinLOCKHEAP = 231;
       Ord_WinCREATEATOMTABLE = 233;
       Ord_WinDESTROYATOMTABLE = 234;
       Ord_WinADDATOM = 235;
       Ord_WinFINDATOM = 236;
       Ord_WinDELETEATOM = 237;
       Ord_WinQUERYATOMUSAGE = 238;
       Ord_WinQUERYATOMLENGTH = 239;
       Ord_WinQUERYATOMNAME = 240;
       Ord_WinQUERYSYSTEMATOMTABLE = 241;
       Ord_WinGETLASTERROR = 243;
       Ord_WinGETERRORINFO = 244;
       Ord_WinFREEERRORINFO = 245;
       Ord_WinINITIALIZE = 246;
       Ord_WinTERMINATE = 247;
       Ord_WinCATCH = 248;
       Ord_WinTHROW = 249;
       Ord_WinQUERYVERSION = 250;
       Ord__WinSETERRORINFO = 263;
       Ord_WinISPHYSINPUTENABLED = 264;
       Ord_WinQUERYWINDOWDC = 265;
       Ord_WinDRAWBORDER = 266;
       Ord_WinDRAWTEXT = 267;
       Ord_WinDRAWBITMAP = 268;
       Ord_WinQUERYWINDOWPTR = 269;
       Ord_WinSETWINDOWPTR = 270;
       Ord_WinMSGSEMWAIT = 274;
       Ord_WinMSGMUXSEMWAIT = 275;
       Ord_WinCANCELSHUTDOWN = 277;
       Ord_WinSETWINDOWBITS = 278;
       Ord_WinGETCLIPPS = 279;
       Ord_WinSAVEWINDOWPOS = 285;
       Ord_WinFOCUSCHANGE = 286;
       Ord_WinQUERYQUEUEINFO = 287;
       Ord_WinSETCLASSMSGINTEREST = 292;
       Ord_WinQUERYDLGITEMTEXTLENGTH = 294;
       Ord_WinDDEINITIATE = 297;
       Ord_WinDDERESPOND = 298;
       Ord_WinDDEPOSTMSG = 299;
       Ord_WinSETPRESPARAM = 301;
       Ord_WinQUERYPRESPARAM = 302;
       Ord_WinREMOVEPRESPARAM = 303;
       Ord_DumWinCREATEHELPINSTANCE = 311;
       Ord_DumWinDESTROYHELPINSTANCE = 312;
       Ord_DumWinASSOCIATEHELPINSTANCE = 313;
       Ord_DumWinCREATEHELPTABLE = 314;
       Ord_DumWinLOADHELPTABLE = 315;
       Ord_DumWinQUERYHELPINSTANCE = 316;
       Ord_WinQUERYWINDOWMODEL = 317;
       Ord_WinSETDESKTOPBKGND = 318;
       Ord_WinQUERYDESKTOPBKGND = 319;
       Ord_WinPOPUPMENU = 320;
       Ord_WinREALIZEPALETTE = 321;
       Ord_DummyHelpEntry = 322;
       Ord_WinDELETELIBRARY = 602;
       Ord_WinLOADPROCEDURE = 603;
       Ord_WinDELETEPROCEDURE = 604;
       Ord_WinSETMSGMODE = 605;
       Ord_WinSETSYNCHROMODE = 606;
       Ord_WinGETDLGMSG = 607;
       Ord_WinREGISTERUSERMSG = 608;
       Ord_WinQUERYANCHORBLOCK = 609;
       Ord_WinREGISTERUSERDATATYPE = 612;
       Ord_WinISWINDOWSHOWING = 614;
       Ord_WinLOADLIBRARY = 615;
       Ord_WinCREATEPOINTERINDIRECT = 616;
       Ord_WinQUERYPOINTERINFO = 617;
       Ord_WinGETERASEPS = 624;
       Ord_WinRELEASEERASEPS = 625;
       Ord_WinSTRETCHPOINTER = 632;
       Ord_WinSETPOINTEROWNER = 633;
       Ord_Win32ADDATOM = 700;
       Ord_Win32ALARM = 701;
       Ord_Win32BEGINENUMWINDOWS = 702;
       Ord_Win32BEGINPAINT = 703;
       Ord_Win32CALCFRAMERECT = 704;
       Ord_Win32CANCELSHUTDOWN = 705;
       Ord_Win32CLOSECLIPBRD = 707;
       Ord_Win32COMPARESTRINGS = 708;
       Ord_Win32COPYACCELTABLE = 709;
       Ord_Win32COPYRECT = 710;
       Ord_Win32CPTRANSLATECHAR = 711;
       Ord_Win32CPTRANSLATESTRING = 712;
       Ord_Win32CREATEACCELTABLE = 713;
       Ord_Win32CREATEATOMTABLE = 714;
       Ord_Win32CREATECURSOR = 715;
       Ord_Win32CREATEMSGQUEUE = 716;
       Ord_Win32CREATEPOINTER = 717;
       Ord_Win32DDEINITIATE = 718;
       Ord_Win32DDEPOSTMSG = 719;
       Ord_Win32DDERESPOND = 720;
       Ord_Win32DELETEATOM = 721;
       Ord_Win32DELETELIBRARY = 722;
       Ord_Win32DESTROYACCELTABLE = 723;
       Ord_Win32DESTROYATOMTABLE = 724;
       Ord_Win32DESTROYCURSOR = 725;
       Ord_Win32DESTROYMSGQUEUE = 726;
       Ord_Win32DESTROYPOINTER = 727;
       Ord_Win32DESTROYWINDOW = 728;
       Ord_Win32DISMISSDLG = 729;
       Ord_Win32DRAWBITMAP = 730;
       Ord_Win32DRAWBORDER = 731;
       Ord_Win32DRAWPOINTER = 732;
       Ord_Win32EMPTYCLIPBRD = 733;
       Ord_Win32ENABLEPHYSINPUT = 734;
       Ord_Win32ENABLEWINDOW = 735;
       Ord_Win32ENABLEWINDOWUPDATE = 736;
       Ord_Win32ENDENUMWINDOWS = 737;
       Ord_Win32ENDPAINT = 738;
       Ord_Win32ENUMCLIPBRDFMTS = 739;
       Ord_Win32ENUMDLGITEM = 740;
       Ord_Win32EQUALRECT = 741;
       Ord_Win32EXCLUDEUPDATEREGION = 742;
       Ord_Win32FILLRECT = 743;
       Ord_Win32FINDATOM = 744;
       Ord_Win32FLASHWINDOW = 745;
       Ord_Win32FOCUSCHANGE = 746;
       Ord_Win32FREEERRORINFO = 748;
       Ord_Win32GETCLIPPS = 749;
       Ord_Win32GETCURRENTTIME = 750;
       Ord_Win32GETERRORINFO = 751;
       Ord_Win32GETKEYSTATE = 752;
       Ord_Win32GETLASTERROR = 753;
       Ord_Win32GETMAXPOSITION = 754;
       Ord_Win32GETMINPOSITION = 755;
       Ord_Win32GETNEXTWINDOW = 756;
       Ord_Win32GETPS = 757;
       Ord_Win32GETPHYSKEYSTATE = 758;
       Ord_Win32GETSCREENPS = 759;
       Ord_Win32GETSYSBITMAP = 760;
       Ord_Win32INSENDMSG = 761;
       Ord_Win32INFLATERECT = 762;
       Ord_Win32INITIALIZE = 763;
       Ord_Win32INTERSECTRECT = 764;
       Ord_Win32INVALIDATERECT = 765;
       Ord_Win32INVALIDATEREGION = 766;
       Ord_Win32INVERTRECT = 767;
       Ord_Win32ISCHILD = 768;
       Ord_Win32ISPHYSINPUTENABLED = 769;
       Ord_Win32ISRECTEMPTY = 770;
       Ord_Win32ISTHREADACTIVE = 771;
       Ord_Win32ISWINDOW = 772;
       Ord_Win32ISWINDOWENABLED = 773;
       Ord_Win32ISWINDOWSHOWING = 774;
       Ord_Win32ISWINDOWVISIBLE = 775;
       Ord_Win32LOADACCELTABLE = 776;
       Ord_Win32LOADLIBRARY = 777;
       Ord_Win32LOADMENU = 778;
       Ord_Win32LOADMESSAGE = 779;
       Ord_Win32LOADPOINTER = 780;
       Ord_Win32LOADSTRING = 781;
       Ord_Win32LOCKVISREGIONS = 782;
       Ord_Win32LOCKWINDOWUPDATE = 784;
       Ord_Win32MAKEPOINTS = 785;
       Ord_Win32MAKERECT = 786;
       Ord_Win32MAPDLGPOINTS = 787;
       Ord_Win32MAPWINDOWPOINTS = 788;
       Ord_Win32MESSAGEBOX = 789;
       Ord_Win32MSGSEMWAIT = 790;
       Ord_Win32NEXTCHAR = 791;
       Ord_Win32OFFSETRECT = 792;
       Ord_Win32OPENCLIPBRD = 793;
       Ord_Win32OPENWINDOWDC = 794;
       Ord_Win32PREVCHAR = 795;
       Ord_Win32PROCESSDLG = 796;
       Ord_Win32PTINRECT = 797;
       Ord_Win32QUERYACCELTABLE = 798;
       Ord_Win32QUERYACTIVEWINDOW = 799;
       Ord_Win32QUERYANCHORBLOCK = 800;
       Ord_Win32QUERYATOMLENGTH = 801;
       Ord_Win32QUERYATOMNAME = 802;
       Ord_Win32QUERYATOMUSAGE = 803;
       Ord_Win32QUERYCAPTURE = 804;
       Ord_Win32QUERYCLASSNAME = 805;
       Ord_Win32QUERYCLIPBRDDATA = 806;
       Ord_Win32QUERYCLIPBRDFMTINFO = 807;
       Ord_Win32QUERYCLIPBRDOWNER = 808;
       Ord_Win32QUERYCLIPBRDVIEWER = 809;
       Ord_Win32QUERYCP = 810;
       Ord_Win32QUERYCPLIST = 811;
       Ord_Win32QUERYCURSORINFO = 812;
       Ord_Win32QUERYDESKTOPWINDOW = 813;
       Ord_Win32QUERYDLGITEMSHORT = 814;
       Ord_Win32QUERYDLGITEMTEXT = 815;
       Ord_Win32QUERYDLGITEMTEXTLENGTH = 816;
       Ord_Win32QUERYFOCUS = 817;
       Ord_Win32QUERYMSGPOS = 818;
       Ord_Win32QUERYMSGTIME = 819;
       Ord_Win32QUERYOBJECTWINDOW = 820;
       Ord_Win32QUERYPOINTER = 821;
       Ord_Win32QUERYPOINTERINFO = 822;
       Ord_Win32QUERYPOINTERPOS = 823;
       Ord_Win32QUERYQUEUEINFO = 824;
       Ord_Win32QUERYQUEUESTATUS = 825;
       Ord_Win32QUERYSYSCOLOR = 826;
       Ord_Win32QUERYSYSMODALWINDOW = 827;
       Ord_Win32QUERYSYSPOINTER = 828;
       Ord_Win32QUERYSYSVALUE = 829;
       Ord_Win32QUERYSYSTEMATOMTABLE = 830;
       Ord_Win32QUERYUPDATERECT = 831;
       Ord_Win32QUERYUPDATEREGION = 832;
       Ord_Win32QUERYVERSION = 833;
       Ord_Win32QUERYWINDOW = 834;
       Ord_Win32QUERYWINDOWDC = 835;
       Ord_Win32QUERYWINDOWPOS = 837;
       Ord_Win32QUERYWINDOWPROCESS = 838;
       Ord_Win32QUERYWINDOWPTR = 839;
       Ord_Win32QUERYWINDOWRECT = 840;
       Ord_Win32QUERYWINDOWTEXT = 841;
       Ord_Win32QUERYWINDOWTEXTLENGTH = 842;
       Ord_Win32QUERYWINDOWULONG = 843;
       Ord_Win32QUERYWINDOWUSHORT = 844;
       Ord_Win32REGISTERUSERDATATYPE = 845;
       Ord_Win32REGISTERUSERMSG = 846;
       Ord_Win32RELEASEPS = 848;
       Ord_Win32SCROLLWINDOW = 849;
       Ord_Win32SETACCELTABLE = 850;
       Ord_Win32SETACTIVEWINDOW = 851;
       Ord_Win32SETCAPTURE = 852;
       Ord_Win32SETCLASSMSGINTEREST = 853;
       Ord_Win32SETCLIPBRDDATA = 854;
       Ord_Win32SETCLIPBRDOWNER = 855;
       Ord_Win32SETCLIPBRDVIEWER = 856;
       Ord_Win32SETCP = 857;
       Ord_Win32SETDLGITEMSHORT = 858;
       Ord_Win32SETDLGITEMTEXT = 859;
       Ord_Win32SETFOCUS = 860;
       Ord_Win32SETMSGINTEREST = 861;
       Ord_Win32SETMSGMODE = 862;
       Ord_Win32SETMULTWINDOWPOS = 863;
       Ord_Win32SETOWNER = 864;
       Ord_Win32SETPARENT = 865;
       Ord_Win32SETPOINTER = 866;
       Ord_Win32SETPOINTERPOS = 867;
       Ord_Win32SETRECT = 868;
       Ord_Win32SETRECTEMPTY = 869;
       Ord_Win32SETSYNCHROMODE = 870;
       Ord_Win32SETSYSCOLORS = 871;
       Ord_Win32SETSYSMODALWINDOW = 872;
       Ord_Win32SETSYSVALUE = 873;
       Ord_Win32SETWINDOWBITS = 874;
       Ord_Win32SETWINDOWPOS = 875;
       Ord_Win32SETWINDOWPTR = 876;
       Ord_Win32SETWINDOWTEXT = 877;
       Ord_Win32SETWINDOWULONG = 878;
       Ord_Win32SETWINDOWUSHORT = 879;
       Ord_Win32SHOWCURSOR = 880;
       Ord_Win32SHOWPOINTER = 881;
       Ord_Win32SHOWTRACKRECT = 882;
       Ord_Win32SHOWWINDOW = 883;
       Ord_Win32STARTTIMER = 884;
       Ord_Win32STOPTIMER = 885;
       Ord_Win32SUBSTITUTESTRINGS = 886;
       Ord_Win32SUBTRACTRECT = 887;
       Ord_Win32TERMINATE = 888;
       Ord_Win32TRACKRECT = 890;
       Ord_Win32UNIONRECT = 891;
       Ord_Win32UPDATEWINDOW = 892;
       Ord_Win32UPPER = 893;
       Ord_Win32UPPERCHAR = 894;
       Ord_Win32VALIDATERECT = 895;
       Ord_Win32VALIDATEREGION = 896;
       Ord_Win32WAITMSG = 897;
       Ord_Win32WINDOWFROMDC = 898;
       Ord_Win32WINDOWFROMID = 899;
       Ord_Win32WINDOWFROMPOINT = 900;
       Ord_Win32BROADCASTMSG = 901;
       Ord_Win32POSTQUEUEMSG = 902;
       Ord_Win32SENDDLGITEMMSG = 903;
       Ord_Win32TRANSLATEACCEL = 904;
       Ord_Win32CALLMSGFILTER = 905;
       Ord_Win32CREATEFRAMECONTROLS = 906;
       Ord_Win32CREATEMENU = 907;
       Ord_Win32CREATESTDWINDOW = 908;
       Ord_Win32CREATEWINDOW = 909;
       Ord_Win32DEFDLGPROC = 910;
       Ord_Win32DEFWINDOWPROC = 911;
       Ord_Win32DISPATCHMSG = 912;
       Ord_Win32DRAWTEXT = 913;
       Ord_Win32GETDLGMSG = 914;
       Ord_Win32GETMSG = 915;
       Ord_Win32MSGMUXSEMWAIT = 916;
       Ord_Win32MULTWINDOWFROMIDS = 917;
       Ord_Win32PEEKMSG = 918;
       Ord_Win32POSTMSG = 919;
       Ord_Win32SENDMSG = 920;
       Ord_Win32SETKEYBOARDSTATETABLE = 921;
       Ord_Win32CREATEDLG = 922;
       Ord_Win32DLGBOX = 923;
       Ord_Win32LOADDLG = 924;
       Ord_Win32QUERYCLASSINFO = 925;
       Ord_Win32REGISTERCLASS = 926;
       Ord_Win32RELEASEHOOK = 927;
       Ord_Win32SETHOOK = 928;
       Ord_Win32SUBCLASSWINDOW = 929;
       Ord_Win32SETCLASSTHUNKPROC = 930;
       Ord_Win32QUERYCLASSTHUNKPROC = 931;
       Ord_Win32SETWINDOWTHUNKPROC = 932;
       Ord_Win32QUERYWINDOWTHUNKPROC = 933;
       Ord_Win32QUERYWINDOWMODEL = 934;
       Ord_Win32SETDESKTOPBKGND = 935;
       Ord_Win32QUERYDESKTOPBKGND = 936;
       Ord_Win32POPUPMENU = 937;
       Ord_Win32SETPRESPARAM = 938;
       Ord_Win32QUERYPRESPARAM = 939;
       Ord_Win32REMOVEPRESPARAM = 940;
       Ord_Win32REALIZEPALETTE = 941;
       Ord_Win32CREATEPOINTERINDIRECT = 942;
       Ord_Win32SAVEWINDOWPOS = 943;
       Ord_Win32GETERASEPS = 952;
       Ord_Win32RELEASEERASEPS = 953;
       Ord_WinSETCLASSTHUNKPROC = 959;
       Ord_WinQUERYCLASSTHUNKPROC = 960;
       Ord_WinSETWINDOWTHUNKPROC = 961;
       Ord_WinQUERYWINDOWTHUNKPROC = 962;
       Ord_Win32STRETCHPOINTER = 968;
       Ord_Win32SETPOINTEROWNER = 971;
       Ord_Win32SETERRORINFO = 977;
       Ord_Win32WAITEVENTSEM = 978;
       Ord_Win32REQUESTMUTEXSEM = 979;
       Ord_Win32WAITMUXWAITSEM = 980;
(* PMPIC.DLL *)
       Ord_PicPRINT = 1;
       Ord_PicICHG = 2;
       Ord_Pic32PRINT = 11;
       Ord_Pic32ICHG = 12;
       Ord_Prf32PIF2MET = 13;
(* PMSHAPI.DLL *)
       Ord_WinQUERYPROFILEINT = 2;
       Ord_WinQUERYPROFILESTRING = 3;
       Ord_WinWRITEPROFILESTRING = 4;
       Ord_WinQUERYPROFILESIZE = 5;
       Ord_WinQUERYPROFILEDATA = 6;
       Ord_WinWRITEPROFILEDATA = 7;
       Ord_WinINITSESSIONMGR = 8;
       Ord_WinSETFGNDWINDOW = 9;
       Ord_WinADDPROGRAM = 12;
       Ord_WinREMOVEPROGRAM = 13;
       Ord_WinCHANGEPROGRAM = 14;
       Ord_WinQUERYDEFINITION = 15;
       Ord_WinQUERYPROGRAMTITLES = 16;
       Ord_WinCREATEGROUP = 17;
       Ord_WinADDTOGROUP = 19;
       Ord_WinQUERYPROGRAMUSE = 20;
       Ord_WinREMOVEFROMGROUP = 21;
       Ord_WinDESTROYGROUP = 23;
       Ord_WinQUERYFILEEXTOPTS = 24;
       Ord_WinSETFILEEXTOPTS = 25;
       Ord_WinQUERYPROGRAMTYPE = 26;
       Ord_PrfQUERYPROFILEINT = 32;
       Ord_PrfQUERYPROFILESTRING = 33;
       Ord_PrfWRITEPROFILESTRING = 34;
       Ord_PrfQUERYPROFILESIZE = 35;
       Ord_PrfQUERYPROFILEDATA = 36;
       Ord_PrfWRITEPROFILEDATA = 37;
       Ord_PrfOPENPROFILE = 38;
       Ord_PrfCLOSEPROFILE = 39;
       Ord_PrfRESET = 42;
       Ord_PrfQUERYPROFILE = 43;
       Ord_WinINSTSTARTAPP = 44;
       Ord_WinTERMINATEAPP = 45;
       Ord_WinCREATESWITCHENTRY = 46;
       Ord_WinQUERYSESSIONTITLE = 47;
       Ord_WinADDSWITCHENTRY = 48;
       Ord_WinCHANGESWITCHENTRY = 49;
       Ord_PrfADDPROGRAM = 50;
       Ord_PrfREMOVEPROGRAM = 51;
       Ord_PrfCHANGEPROGRAM = 52;
       Ord_PrfQUERYDEFINITION = 53;
       Ord_PrfQUERYPROGRAMTITLES = 54;
       Ord_PrfCREATEGROUP = 55;
       Ord_WinQUERYSWITCHENTRY = 56;
       Ord_WinQUERYSWITCHHANDLE = 57;
       Ord_PrfQUERYPROGRAMHANDLE = 58;
       Ord_PrfQUERYPROGRAMCATEGORY = 59;
       Ord_PrfDESTROYGROUP = 60;
       Ord_WinQUERYTASKTITLE = 65;
       Ord_WinQUERYTASKSIZEPOS = 66;
       Ord_WinQUERYSWITCHLIST = 67;
       Ord_WinREMOVESWITCHENTRY = 68;
       Ord_WinSWITCHTOPROGRAM = 69;
       Ord_WinSWITCHPROGRAMREGISTER = 70;
       Ord_WinENDPROGRAM = 73;
       Ord_WinSTOPPROGRAM = 74;
       Ord_WinENDWINDOWSESSION = 75;
       Ord_WinSWITCHTOTASKMANAGER = 78;
       Ord_WinSWITCHTOPROGRAM2 = 80;
       Ord_WinPROCESSHOTKEY = 81;
       Ord_WinINITSESSION = 82;
       Ord_WinENDSESSION = 83;
       Ord_WinINITSWENTRY = 84;
       Ord_WinSETSWENTRY = 85;
       Ord_WinQUERYEXTIDFOCUS = 86;
       Ord_WinSETEXTIDFOCUS = 87;
       Ord_WinNOSHUTDOWN = 91;
       Ord_WinSETTITLE = 93;
       Ord_WinSETTITLEANDICON = 97;
       Ord_Prf32QUERYPROFILESIZE = 101;
       Ord_Prf32OPENPROFILE = 102;
       Ord_Prf32CLOSEPROFILE = 103;
       Ord_Prf32REMOVEPROGRAM = 104;
       Ord_Prf32DESTROYGROUP = 106;
       Ord_Prf32QUERYPROFILE = 107;
       Ord_Prf32RESET = 108;
       Ord_Prf32ADDPROGRAM = 109;
       Ord_Prf32CHANGEPROGRAM = 110;
       Ord_Prf32QUERYDEFINITION = 111;
       Ord_Prf32QUERYPROGRAMTITLES = 113;
       Ord_Prf32QUERYPROFILEINT = 114;
       Ord_Prf32QUERYPROFILESTRING = 115;
       Ord_Prf32WRITEPROFILESTRING = 116;
       Ord_Prf32QUERYPROFILEDATA = 117;
       Ord_Prf32WRITEPROFILEDATA = 118;
       Ord_Win32STARTAPP = 119;
       Ord_Win32ADDSWITCHENTRY = 120;
       Ord_Win32CREATESWITCHENTRY = 121;
       Ord_Win32QUERYSESSIONTITLE = 122;
       Ord_Win32CHANGESWITCHENTRY = 123;
       Ord_Win32QUERYSWITCHENTRY = 124;
       Ord_Win32QUERYSWITCHHANDLE = 125;
       Ord_Win32QUERYSWITCHLIST = 126;
       Ord_Win32QUERYTASKSIZEPOS = 127;
       Ord_Win32QUERYTASKTITLE = 128;
       Ord_Win32REMOVESWITCHENTRY = 129;
       Ord_Win32TERMINATEAPP = 130;
       Ord_Win32SWITCHTOPROGRAM = 131;
       Ord_Win32SWITCHPROGRAMREGISTER = 156;
       Ord_WinSTARTAPP = 201;
       Ord_WinHAPPFROMPID = 208;
       Ord_WinHSWITCHFROMHAPP = 209;
(* QUECALLS.DLL *)
       Ord_DosREADQUEUE = 1;
       Ord_DosPURGEQUEUE = 2;
       Ord_DosCLOSEQUEUE = 3;
       Ord_DosQUERYQUEUE = 4;
       Ord_DosPEEKQUEUE = 5;
       Ord_DosWRITEQUEUE = 6;
       Ord_DosOPENQUEUE = 7;
       Ord_DosCREATEQUEUE = 8;
       Ord_Dos32READQUEUE = 9;
       Ord_Dos32PURGEQUEUE = 10;
       Ord_Dos32CLOSEQUEUE = 11;
       Ord_Dos32QUERYQUEUE = 12;
       Ord_Dos32PEEKQUEUE = 13;
       Ord_Dos32WRITEQUEUE = 14;
       Ord_Dos32OPENQUEUE = 15;
       Ord_Dos32CREATEQUEUE = 16;
(* SESMGR.DLL *)
       Ord_DosSTOPSESSION = 8;
       Ord_DosSELECTSESSION = 9;
       Ord_DosSETSESSION = 14;
       Ord_DosSTARTSESSION = 17;
       Ord_DosSMREGISTERDD = 29;
       Ord_Dos32STARTSESSION = 37;
       Ord_Dos32SELECTSESSION = 38;
       Ord_Dos32SETSESSION = 39;
       Ord_Dos32STOPSESSION = 40;

       Ord_WinFONTDLG = 2;
       Ord_WinDEFFONTDLGPROC = 3;
       Ord_WinFILEDLG = 4;
       Ord_WinDEFFILEDLGPROC = 5;
       Ord_WinFREEFILEDLGLIST = 6;
(* VIOCALLS.DLL *)
       Ord_VioENDPOPUP = 1;
       Ord_VioGETPHYSBUF = 2;
       Ord_VioGETANSI = 3;
       Ord_VioSETANSI = 5;
       Ord_VioDEREGISTER = 6;
       Ord_VioSCROLLUP = 7;
       Ord_VioPRTSC = 8;
       Ord_VioGETCURPOS = 9;
       Ord_VioWRTCELLSTR = 10;
       Ord_VioPOPUP = 11;
       Ord_VioSCROLLRT = 12;
       Ord_VioWRTCHARSTR = 13;
       Ord_VioSETCURPOS = 15;
       Ord_VioSCRUNLOCK = 18;
       Ord_VioWRTTTY = 19;
       Ord_VioGETMODE = 21;
       Ord_VioSETMODE = 22;
       Ord_VioSCRLOCK = 23;
       Ord_VioREADCELLSTR = 24;
       Ord_VioSAVREDRAWWAIT = 25;
       Ord_VioWRTNATTR = 26;
       Ord_VioGETCURTYPE = 27;
       Ord_VioSAVREDRAWUNDO = 28;
       Ord_VioGETFONT = 29;
       Ord_VioREADCHARSTR = 30;
       Ord_VioGETBUF = 31;
       Ord_VioSETCURTYPE = 32;
       Ord_VioSETFONT = 33;
       Ord_VioMODEUNDO = 35;
       Ord_VioMODEWAIT = 37;
       Ord_VioGETCP = 40;
       Ord_VioSETCP = 42;
       Ord_VioSHOWBUF = 43;
       Ord_VioSCROLLLF = 44;
       Ord_VioREGISTER = 45;
       Ord_VioGETCONFIG = 46;
       Ord_VioSCROLLDN = 47;
       Ord_VioWRTCHARSTRATT = 48;
       Ord_VioGETSTATE = 49;
       Ord_VioPRTSCTOGGLE = 50;
       Ord_VioSETSTATE = 51;
       Ord_VioWRTNCELL = 52;
       Ord_VioWRTNCHAR = 53;
       Ord_VioASSOCIATE = 55;
       Ord_VioCREATEPS = 56;
       Ord_VioDELETESETID = 57;
       Ord_VioGETDEVICECELLSIZE = 58;
       Ord_VioGETORG = 59;
       Ord_VioCREATELOGFONT = 60;
       Ord_VioDESTROYPS = 61;
       Ord_VioQUERYSETIDS = 62;
       Ord_VioSETORG = 63;
       Ord_VioQUERYFONTS = 64;
       Ord_VioSETDEVICECELLSIZE = 65;
       Ord_VioSHOWPS = 66;
       Ord_VioGETPSADDRESS = 67;
       Ord_VioGLOBALREG = 70;
       Ord_XVioSETCASTATE = 71;
       Ord_XVioCHECKCHARTYPE = 72;
       Ord_XVioDESTROYCA = 73;
       Ord_XVioCREATECA = 74;
       Ord_VioCHECKCHARTYPE = 75;
       Ord_XVioGETCASTATE = 76;
(* PMVIOP.DLL *)
       Ord_WinDefAVioWindowProc = 30;

{ Declarations from bsedev.h header file (low-level device access) }
    const
       IOCTL_ASYNC = $0001;
       IOCTL_SCR_AND_PTRDRAW = $0003;
       IOCTL_KEYBOARD = $0004;
       IOCTL_PRINTER = $0005;
       IOCTL_LIGHTPEN = $0006;
       IOCTL_POINTINGDEVICE = $0007;
       IOCTL_DISK = $0008;
       IOCTL_PHYSICALDISK = $0009;
       IOCTL_MONITOR = $000A;
       IOCTL_GENERAL = $000B;
       ASYNC_SETBAUDRATE = $0041;
       ASYNC_SETLINECTRL = $0042;
       ASYNC_EXTSETBAUDRATE = $0043;
       ASYNC_SETEXTBAUDRATE = $0043;
       ASYNC_TRANSMITIMM = $0044;
       ASYNC_SETBREAKOFF = $0045;
       ASYNC_SETMODEMCTRL = $0046;
       ASYNC_SETBREAKON = $004B;
       ASYNC_STOPTRANSMIT = $0047;
       ASYNC_STARTTRANSMIT = $0048;
       ASYNC_SETDCBINFO = $0053;
       ASYNC_GETBAUDRATE = $0061;
       ASYNC_GETLINECTRL = $0062;
       ASYNC_EXTGETBAUDRATE = $0063;
       ASYNC_GETEXTBAUDRATE = $0063;
       ASYNC_GETCOMMSTATUS = $0064;
       ASYNC_GETLINESTATUS = $0065;
       ASYNC_GETMODEMOUTPUT = $0066;
       ASYNC_GETMODEMINPUT = $0067;
       ASYNC_GETINQUECOUNT = $0068;
       ASYNC_GETOUTQUECOUNT = $0069;
       ASYNC_GETCOMMERROR = $006D;
       ASYNC_GETCOMMEVENT = $0072;
       ASYNC_GETDCBINFO = $0073;
       SCR_ALLOCLDT = $0070;
       SCR_DEALLOCLDT = $0071;
       PTR_GETPTRDRAWADDRESS = $0072;
       SCR_ALLOCLDTOFF = $0075;
       KBD_SETTRANSTABLE = $0050;
       KBD_SETINPUTMODE = $0051;
       KBD_SETINTERIMFLAG = $0052;
       KBD_SETSHIFTSTATE = $0053;
       KBD_SETTYPAMATICRATE = $0054;
       KBD_SETFGNDSCREENGRP = $0055;
       KBD_SETSESMGRHOTKEY = $0056;
       KBD_SETFOCUS = $0057;
       KBD_SETKCB = $0058;
       KBD_SETNLS = $005C;
       KBD_CREATE = $005D;
       KBD_DESTROY = $005E;
       KBD_GETINPUTMODE = $0071;
       KBD_GETINTERIMFLAG = $0072;
       KBD_GETSHIFTSTATE = $0073;
       KBD_READCHAR = $0074;
       KBD_PEEKCHAR = $0075;
       KBD_GETSESMGRHOTKEY = $0076;
       KBD_GETKEYBDTYPE = $0077;
       KBD_GETCODEPAGEID = $0078;
       KBD_XLATESCAN = $0079;
       PRT_QUERYJOBHANDLE = $0021;
       PRT_SETFRAMECTL = $0042;
       PRT_SETINFINITERETRY = $0044;
       PRT_INITPRINTER = $0046;
       PRT_ACTIVATEFONT = $0048;
       PRT_GETFRAMECTL = $0062;
       PRT_GETINFINITERETRY = $0064;
       PRT_GETPRINTERSTATUS = $0066;
       PRT_QUERYACTIVEFONT = $0069;
       PRT_VERIFYFONT = $006A;
       MOU_ALLOWPTRDRAW = $0050;
       MOU_UPDATEDISPLAYMODE = $0051;
       MOU_SCREENSWITCH = $0052;
       MOU_SETSCALEFACTORS = $0053;
       MOU_SETEVENTMASK = $0054;
       MOU_SETHOTKEYBUTTON = $0055;
       MOU_SETPTRSHAPE = $0056;
       MOU_DRAWPTR = $0057;
       MOU_REMOVEPTR = $0058;
       MOU_SETPTRPOS = $0059;
       MOU_SETPROTDRAWADDRESS = $005A;
       MOU_SETREALDRAWADDRESS = $005B;
       MOU_SETMOUSTATUS = $005C;
       MOU_DISPLAYMODECHANGE = $005D;
       MOU_GETBUTTONCOUNT = $0060;
       MOU_GETMICKEYCOUNT = $0061;
       MOU_GETMOUSTATUS = $0062;
       MOU_READEVENTQUE = $0063;
       MOU_GETQUESTATUS = $0064;
       MOU_GETEVENTMASK = $0065;
       MOU_GETSCALEFACTORS = $0066;
       MOU_GETPTRPOS = $0067;
       MOU_GETPTRSHAPE = $0068;
       MOU_GETHOTKEYBUTTON = $0069;
       MOU_VER = $006A;
       DSK_LOCKDRIVE = $0000;
       DSK_UNLOCKDRIVE = $0001;
       DSK_REDETERMINEMEDIA = $0002;
       DSK_SETLOGICALMAP = $0003;
       DSK_BLOCKREMOVABLE = $0020;
       DSK_GETLOGICALMAP = $0021;
       DSK_SETDEVICEPARAMS = $0043;
       DSK_WRITETRACK = $0044;
       DSK_FORMATVERIFY = $0045;
       DSK_GETDEVICEPARAMS = $0063;
       DSK_READTRACK = $0064;
       DSK_VERIFYTRACK = $0065;
       PDSK_LOCKPHYSDRIVE = $0000;
       PDSK_UNLOCKPHYSDRIVE = $0001;
       PDSK_WRITEPHYSTRACK = $0044;
       PDSK_GETPHYSDEVICEPARAMS = $0063;
       PDSK_READPHYSTRACK = $0064;
       PDSK_VERIFYPHYSTRACK = $0065;
       MON_REGISTERMONITOR = $0040;
       DEV_FLUSHINPUT = $0001;
       DEV_FLUSHOUTPUT = $0002;
       DEV_QUERYMONSUPPORT = $0060;
       RX_QUE_OVERRUN = $0001;
       RX_HARDWARE_OVERRUN = $0002;
       PARITY_ERROR = $0004;
       FRAMING_ERROR = $0008;
       CHAR_RECEIVED = $0001;
       LAST_CHAR_SENT = $0004;
       CTS_CHANGED = $0008;
       DSR_CHANGED = $0010;
       DCD_CHANGED = $0020;
       BREAK_DETECTED = $0040;
       ERROR_OCCURRED = $0080;
       RI_DETECTED = $0100;
       TX_WAITING_FOR_CTS = $0001;
       TX_WAITING_FOR_DSR = $0002;
       TX_WAITING_FOR_DCD = $0004;
       TX_WAITING_FOR_XON = $0008;
       TX_WAITING_TO_SEND_XON = $0010;
       TX_WAITING_WHILE_BREAK_ON = $0020;
       TX_WAITING_TO_SEND_IMM = $0040;
       RX_WAITING_FOR_DSR = $0080;
       WRITE_REQUEST_QUEUED = $0001;
       DATA_IN_TX_QUE = $0002;
       HARDWARE_TRANSMITTING = $0004;
       CHAR_READY_TO_SEND_IMM = $0008;
       WAITING_TO_SEND_XON = $0010;
       WAITING_TO_SEND_XOFF = $0020;
       CTS_ON = $10;
       DSR_ON = $20;
       RI_ON = $40;
       DCD_ON = $80;
       BUILD_BPB_FROM_MEDIUM = $00;
       REPLACE_BPB_FOR_DEVICE = $01;
       REPLACE_BPB_FOR_MEDIUM = $02;
       ASCII_MODE = $00;
       BINARY_MODE = $80;
       CONVERSION_REQUEST = $20;
       INTERIM_CHAR = $80;
       HOTKEY_MAX_COUNT = $0000;
       HOTKEY_CURRENT_COUNT = $0001;
       KBD_DATA_RECEIVED = $0001;
       KBD_DATA_BINARY = $8000;
       KBD_READ_WAIT = $0000;
       KBD_READ_NOWAIT = $8000;
       SHIFT_REPORT_MODE = $01;
       MOUSE_MOTION = $0001;
       MOUSE_MOTION_WITH_BN1_DOWN = $0002;
       MOUSE_BN1_DOWN = $0004;
       MOUSE_MOTION_WITH_BN2_DOWN = $0008;
       MOUSE_BN2_DOWN = $0010;
       MOUSE_MOTION_WITH_BN3_DOWN = $0020;
       MOUSE_BN3_DOWN = $0040;
       MHK_BUTTON1 = $0001;
       MHK_BUTTON2 = $0002;
       MHK_BUTTON3 = $0004;
       MOU_NOWAIT = $0000;
       MOU_WAIT = $0001;
       MHK_NO_HOTKEY = $0000;
       MOUSE_QUEUEBUSY = $0001;
       MOUSE_BLOCKREAD = $0002;
       MOUSE_FLUSH = $0004;
       MOUSE_UNSUPPORTED_MODE = $0008;
       MOUSE_DISABLED = $0100;
       MOUSE_MICKEYS = $0200;
       PRINTER_TIMEOUT = $0001;
       PRINTER_IO_ERROR = $0008;
       PRINTER_SELECTED = $0010;
       PRINTER_OUT_OF_PAPER = $0020;
       PRINTER_ACKNOWLEDGED = $0040;
       PRINTER_NOT_BUSY = $0080;
       MODE_DTR_CONTROL = $01;
       MODE_DTR_HANDSHAKE = $02;
       MODE_CTS_HANDSHAKE = $08;
       MODE_DSR_HANDSHAKE = $10;
       MODE_DCD_HANDSHAKE = $20;
       MODE_DSR_SENSITIVITY = $40;
       MODE_AUTO_TRANSMIT = $01;
       MODE_AUTO_RECEIVE = $02;
       MODE_ERROR_CHAR = $04;
       MODE_NULL_STRIPPING = $08;
       MODE_BREAK_CHAR = $10;
       MODE_RTS_CONTROL = $40;
       MODE_RTS_HANDSHAKE = $80;
       MODE_TRANSMIT_TOGGLE = $C0;
       MODE_NO_WRITE_TIMEOUT = $01;
       MODE_READ_TIMEOUT = $02;
       MODE_WAIT_READ_TIMEOUT = $04;
       MODE_NOWAIT_READ_TIMEOUT = $06;

    type
       DCBINFO = record
          usWriteTimeout : word;
          usReadTimeout : word;
          fbCtlHndShake : byte;
          fbFlowReplace : byte;
          fbTimeout : byte;
          bErrorReplacementChar : byte;
          bBreakReplacementChar : byte;
          bXONChar : byte;
          bXOFFChar : byte;
       end;

       PDCBINFO = ^DCBINFO;
       TDCBInfo = DCBInfo;

    const
       DEVTYPE_48TPI = $0000;
       DEVTYPE_96TPI = $0001;
       DEVTYPE_35 = $0002;
       DEVTYPE_8SD = $0003;
       DEVTYPE_8DD = $0004;
       DEVTYPE_FIXED = $0005;
       DEVTYPE_TAPE = $0006;
       DEVTYPE_UNKNOWN = $0007;

{$PACKRECORDS 1}

    type
       BIOSPARAMETERBLOCK = record
          usBytesPerSector : word;
          bSectorsPerCluster : byte;
          usReservedSectors : word;
          cFATs : byte;
          cRootEntries : word;
          cSectors : word;
          bMedia : byte;
          usSectorsPerFAT : word;
          usSectorsPerTrack : word;
          cHeads : word;
          cHiddenSectors : cardinal;
          cLargeSectors : cardinal;
          abReserved : array[0..6-1] of byte;
          cCylinders : word;
          bDeviceType : byte;
          fsDeviceAttr : word;
       end;

       PBIOSPARAMETERBLOCK = ^BIOSPARAMETERBLOCK;
       TBIOSParameterBlock = BiosParameterBlock;

       SCREENGROUP = record
          idScreenGrp : word;
          fTerminate : word;
       end;

       PSCREENGROUP = ^SCREENGROUP;
       TScreenGroup = ScreenGroup;

       FRAME = record
          bCharsPerLine : byte;
          bLinesPerInch : byte;
       end;

       PFRAME = ^FRAME;
       TFrame = Frame;

       KBDTYPE = record
          usType : word;
          reserved1 : word;
          reserved2 : word;
       end;

       PKBDTYPE = ^KBDTYPE;
       TKbdType = KbdType;

       LINECONTROL = record
          bDataBits : byte;
          bParity : byte;
          bStopBits : byte;
          fTransBreak : byte;
       end;

       PLINECONTROL = ^LINECONTROL;
       TLineControl = LineControl;

    const
       DTR_ON = $01;
       RTS_ON = $02;
       DTR_OFF = $FE;
       RTS_OFF = $FD;

    type
       MODEMSTATUS = record
          fbModemOn : byte;
          fbModemOff : byte;
       end;

       PMODEMSTATUS = ^MODEMSTATUS;
       TModemStatus = ModemStatus;

       RXQUEUE = record
          cch : word;
          cb : word;
       end;

       PRXQUEUE = ^RXQUEUE;
       TRxQueue = RxQueue;

       DEVICEPARAMETERBLOCK = record
          reserved1 : word;
          cCylinders : word;
          cHeads : word;
          cSectorsPerTrack : word;
          reserved2 : word;
          reserved3 : word;
          reserved4 : word;
          reserved5 : word;
       end;

       PDEVICEPARAMETERBLOCK = ^DEVICEPARAMETERBLOCK;
       TDeviceParameterBlock = DeviceParameterBlock;

{$PACKRECORDS 2}

       PTRDRAWFUNCTION = record
          usReturnCode : word;
          pfnDraw : pointer;
          {!!!!!!!! pfnDraw : PFN; }
          pchDataSeg : pointer;
       end;

       PPTRDRAWFUNCTION = ^PTRDRAWFUNCTION;
       TPtrDrawFunction = PtrDrawFunction;

       PTRDRAWADDRESS = record
          reserved : word;
          ptrdfnc : PTRDRAWFUNCTION;
       end;

       PPTRDRAWADDRESS = ^PTRDRAWADDRESS;
       TPtrDrawAddress = PtrDrawAddress;

       SHIFTSTATE = record
          fsState : word;
          fNLS : byte;
       end;

       PSHIFTSTATE = ^SHIFTSTATE;
       TShiftState = ShiftState;

    const
       RIGHTSHIFT = $0001;
       LEFTSHIFT = $0002;
       CONTROL = $0004;
       ALT = $0008;
       SCROLLLOCK_ON = $0010;
       NUMLOCK_ON = $0020;
       CAPSLOCK_ON = $0040;
       INSERT_ON = $0080;
       LEFTCONTROL = $0100;
       LEFTALT = $0200;
       RIGHTCONTROL = $0400;
       RIGHTALT = $0800;
       SCROLLLOCK = $1000;
       NUMLOCK = $2000;
       CAPSLOCK = $4000;
       SYSREQ = $8000;

    type
       HOTKEY = record
          fsHotKey : word;
          uchScancodeMake : byte;
          uchScancodeBreak : byte;
          idHotKey : word;
       end;

       PHOTKEY = ^HOTKEY;
       THotKey = HotKey;

       MONITORPOSITION = record
          fPosition : word;
          index : word;
          pbInBuf : cardinal;
          offOutBuf : word;
       end;

       PMONITORPOSITION = ^MONITORPOSITION;
       TMonitorPosition = MonitorPosition;

       RATEDELAY = record
          usDelay : word;
          usRate : word;
       end;

       PRATEDELAY = ^RATEDELAY;
       TRateDelay = RateDelay;

       CODEPAGEINFO = record
          pbTransTable : PByte;
          idCodePage : word;
          idTable : word;
       end;

       PCODEPAGEINFO = ^CODEPAGEINFO;
       TCodePageInfo = CodePageInfo;

       CPID = record
          idCodePage : word;
          Reserved : word;
       end;

       PCPID = ^CPID;
       TCPID = CPID;

       LDTADDRINFO = record
          pulPhysAddr : PCardinal;
          cb : word;
       end;

       PLDTADDRINFO = ^LDTADDRINFO;
       TLDTAddrInfo = LDTAddrInfo;

       PTRDRAWDATA = record
          cb : word;
          usConfig : word;
          usFlag : word;
       end;

       PPTRDRAWDATA = ^PTRDRAWDATA;
       TPtrDrawData = PtrDrawData;

{$PACKRECORDS NORMAL}

Type
       ICONINFO=record
          cb: Cardinal;         // size of ICONINFO structure
          fFormat: Cardinal;
          pszFileName: PChar;   //use when fFormat = ICON_FILE
          hmod: Cardinal;       // use when fFormat = ICON_RESOURCE
          resid: Cardinal;      // use when fFormat = ICON_RESOURCE
          cbIconData: Cardinal; // use when fFormat = ICON_DATA
          pIconData: Pointer;   // use when fFormat = ICON_DATA
       end;
       PIconInfo=^IconInfo;

const
      ICON_FILE     =1;         // flags for fFormat
      ICON_RESOURCE =2;
      ICON_DATA     =3;
      ICON_CLEAR    =4;

{ Error constants from bseerr.h header file }
CONST
      NO_ERROR                =0;      { MSG%RESPONSE_DATA }
      ERROR_INVALID_FUNCTION  =1;      { MSG%INVALID_FUNCTION }
      ERROR_FILE_NOT_FOUND    =2;      { MSG%FILE_NOT_FOUND }
      ERROR_PATH_NOT_FOUND    =3;      { MSG%PATH_NOT_FOUND }
      ERROR_TOO_MANY_OPEN_FILES=4;     { MSG%OUT_OF_HANDLES }
      ERROR_ACCESS_DENIED     =5;      { MSG%ACCESS_DENIED }
      ERROR_INVALID_HANDLE    =6;      { MSG%INVALID_HANDLE }
      ERROR_ARENA_TRASHED     =7;      { MSG%MEMORY_BLOCKS_BAD }
      ERROR_NOT_ENOUGH_MEMORY =8;      { MSG%NO_MEMORY }
      ERROR_INVALID_BLOCK     =9;      { MSG%INVALID_MEM_ADDR }
      ERROR_BAD_ENVIRONMENT   =10;     { MSG%INVALID_ENVIRON }
      ERROR_BAD_FORMAT        =11;     { MSG%INVALID_FORMAT }
      ERROR_INVALID_ACCESS    =12;     { MSG%INVALID_ACC_CODE }
      ERROR_INVALID_DATA      =13;     { MSG%INVALID_DATA }
      ERROR_INVALID_DRIVE     =15;     { MSG%INVALID_DRIVE }
      ERROR_CURRENT_DIRECTORY =16;     { MSG%ATT_RD_CURDIR }
      ERROR_NOT_SAME_DEVICE   =17;     { MSG%NOT_SAME_DEVICE }
      ERROR_NO_MORE_FILES     =18;     { MSG%NO_MORE_FILES }
      ERROR_WRITE_PROTECT     =19;     { MSG%ATT_WRITE_PROT }
      ERROR_BAD_UNIT          =20;     { MSG%UNKNOWN_UNIT }
      ERROR_NOT_READY         =21;     { MSG%DRIVE_NOT_READY }
      ERROR_BAD_COMMAND       =22;     { MSG%UNKNOWN_COMMAND }
      ERROR_CRC               =23;     { MSG%DATA_ERROR }
      ERROR_BAD_LENGTH        =24;     { MSG%BAD_REQ_STRUCTURE }
      ERROR_SEEK              =25;     { MSG%SEEK_ERROR }
      ERROR_NOT_DOS_DISK      =26;     { MSG%UNKNOWN_MEDIA }
      ERROR_SECTOR_NOT_FOUND  =27;     { MSG%SECTOR_NOT_FOUND }
      ERROR_OUT_OF_PAPER      =28;     { MSG%OUT_OF_PAPER }
      ERROR_WRITE_FAULT       =29;     { MSG%WRITE_FAULT }
      ERROR_READ_FAULT        =30;     { MSG%READ_FAULT }
      ERROR_GEN_FAILURE       =31;     { MSG%GENERAL_FAILURE }
      ERROR_SHARING_VIOLATION =32;     { MSG%SHARING_VIOLATION }
{                                      =32%msg%SHAR_VIOLAT_FIND }
      ERROR_LOCK_VIOLATION    =33;     { MSG%LOCK_VIOLATION }
      ERROR_WRONG_DISK        =34;     { MSG%INVALID_DISK_CHANGE }
      ERROR_FCB_UNAVAILABLE   =35;     { MSG%35;}
      ERROR_SHARING_BUFFER_EXCEEDED=36;{ MSG%SHARING_BUFF_OFLOW }
      ERROR_CODE_PAGE_MISMATCHED=37;   { MSG%ERROR_WRITE_PROTECT }
      ERROR_HANDLE_EOF        =38;     { MSG%ERROR_BAD_UNIT }
      ERROR_HANDLE_DISK_FULL  =39;     { MSG%ERROR_NOT_READY }
{                                      =40%msg%ERROR_BAD_COMMAND }
{                                      =41%msg%ERROR_CRC }
{                                      =42%msg%ERROR_BAD_LENGTH }
{                                      =43%msg%ERROR_SEEK }
{                                      =44%msg%ERROR_NOT_DOS_DISK }
{                                      =45%msg%ERROR_SECTOR_NOT_FOUND }
{                                      =46%msg%ERROR_OUT_OF_PAPER }
{                                      =47%msg%ERROR_WRITE_FAULT }
{                                      =48%msg%ERROR_READ_FAULT }
{                                      =49%msg%ERROR_GEN_FAILURE }
      ERROR_NOT_SUPPORTED     =50;     { MSG%NET_REQ_NOT_SUPPORT }
      ERROR_REM_NOT_LIST      =51;     { MSG%NET_REMOTE_NOT_ONLINE }
      ERROR_DUP_NAME          =52;     { MSG%NET_DUP_FILENAME }
      ERROR_BAD_NETPATH       =53;     { MSG%NET_PATH_NOT_FOUND }
      ERROR_NETWORK_BUSY      =54;     { MSG%NET_BUSY }
      ERROR_DEV_NOT_EXIST     =55;     { MSG%NET_DEV_NOT_INSTALLED }
      ERROR_TOO_MANY_CMDS     =56;     { MSG%NET_BIOS_LIMIT_REACHED }
      ERROR_ADAP_HDW_ERR      =57;     { MSG%NET_ADAPT_HRDW_ERROR }
      ERROR_BAD_NET_RESP      =58;     { MSG%NET_INCORRECT_RESPONSE }
      ERROR_UNEXP_NET_ERR     =59;     { MSG%NET_UNEXPECT_ERROR }
      ERROR_BAD_REM_ADAP      =60;     { MSG%NET_REMOT_ADPT_INCOMP }
      ERROR_PRINTQ_FULL       =61;     { MSG%NET_PRINT_Q_FULL }
      ERROR_NO_SPOOL_SPACE    =62;     { MSG%NET_NO_SPACE_TO_PRINT_FL }
      ERROR_PRINT_CANCELLED   =63;     { MSG%NET_PRINT_FILE_DELETED }
      ERROR_NETNAME_DELETED   =64;     { MSG%NET_NAME_DELETED }
      ERROR_NETWORK_ACCESS_DENIED=65;  { MSG%NET_ACCESS_DENIED }
      ERROR_BAD_DEV_TYPE      =66;     { MSG%NET_DEV_TYPE_INVALID }
      ERROR_BAD_NET_NAME      =67;     { MSG%NET_NAME_NOT_FOUND }
      ERROR_TOO_MANY_NAMES    =68;     { MSG%NET_NAME_LIMIT_EXCEED }
      ERROR_TOO_MANY_SESS     =69;     { MSG%NET_BIOS_LIMIT_EXCEED }
      ERROR_SHARING_PAUSED    =70;     { MSG%NET_TEMP_PAUSED }
      ERROR_REQ_NOT_ACCEP     =71;     { MSG%NET_REQUEST_DENIED }
      ERROR_REDIR_PAUSED      =72;     { MSG%NET_PRT_DSK_REDIR_PAUSE }
      ERROR_SBCS_ATT_WRITE_PROT=73;    { Attempted write on protected disk }
      ERROR_SBCS_GENERAL_FAILURE=74;   { General failure }
      ERROR_XGA_OUT_MEMORY    =75;     { MSG%XGA_OUT_MEMORY }
      ERROR_FILE_EXISTS       =80;     { MSG%FILE_EXISTS }
      ERROR_DUP_FCB           =81;     { MSG%none }
      ERROR_CANNOT_MAKE       =82;     { MSG%CANNOT_MAKE }
      ERROR_FAIL_I24          =83;     { MSG%NET_FAIL_INT_TWO_FOUR }
      ERROR_OUT_OF_STRUCTURES =84;     { MSG%NET_TOO_MANY_REDIRECT }
      ERROR_ALREADY_ASSIGNED  =85;     { MSG%NET_DUP_REDIRECTION }
      ERROR_INVALID_PASSWORD  =86;     { MSG%NET_INVALID_PASSWORD }
      ERROR_INVALID_PARAMETER =87;     { MSG%NET_INCORR_PARAMETER }
      ERROR_NET_WRITE_FAULT   =88;     { MSG%NET_DATA_FAULT }
      ERROR_NO_PROC_SLOTS     =89;     { MSG%NO_PROC_SLOTS }
      ERROR_NOT_FROZEN        =90;     { MSG%none }
      ERROR_SYS_COMP_NOT_LOADED=ERROR_NOT_FROZEN;
      ERR_TSTOVFL             =91;     { MSG%none }
      ERR_TSTDUP              =92;     { MSG%none }
      ERROR_NO_ITEMS          =93;     { MSG%none }
      ERROR_INTERRUPT         =95;     { MSG%none }
      ERROR_DEVICE_IN_USE     =99;     { MSG%DEVICE_IN_USE }
      ERROR_TOO_MANY_SEMAPHORES=100;   { MSG%TOO_MANY_SEMAPHORES }
      ERROR_EXCL_SEM_ALREADY_OWNED=101;{ MSG%EXCL_SEM_ALREADY_OWNED }
      ERROR_SEM_IS_SET        =102;    { MSG%SEM_IS_SET }
      ERROR_TOO_MANY_SEM_REQUESTS=103; { MSG%TOO_MANY_SEM_REQUESTS }
      ERROR_INVALID_AT_INTERRUPT_TIME=104; { MSG%INVALID_AT_INTERRUPT_TIME }
      ERROR_SEM_OWNER_DIED    =105;    { MSG%SEM_OWNER_DIED }
      ERROR_SEM_USER_LIMIT    =106;    { MSG%ERROR_DISK_CHANGE }
      ERROR_DISK_CHANGE       =107;    { MSG%DISK_CHANGE }
      ERROR_DRIVE_LOCKED      =108;    { MSG%DRIVE_LOCKED }
      ERROR_BROKEN_PIPE       =109;    { MSG%BROKEN_PIPE }
      ERROR_OPEN_FAILED       =110;    { MSG%ERROR_OPEN_FAILED }
      ERROR_BUFFER_OVERFLOW   =111;    { MSG%ERROR_FILENAME_LONG }
      ERROR_DISK_FULL         =112;    { MSG%DISK_FULL }
      ERROR_NO_MORE_SEARCH_HANDLES=113;{ MSG%NO_SEARCH_HANDLES }
      ERROR_INVALID_TARGET_HANDLE=114; { MSG%ERR_INV_TAR_HANDLE }
      ERROR_PROTECTION_VIOLATION=115;  { MSG%none }
      ERROR_VIOKBD_REQUEST    =116;    { MSG%none }
      ERROR_INVALID_CATEGORY  =117;    { MSG%INVALID_CATEGORY }
      ERROR_INVALID_VERIFY_SWITCH=118; { MSG%INVALID_VERIFY_SWITCH }
      ERROR_BAD_DRIVER_LEVEL  =119;    { MSG%BAD_DRIVER_LEVEL }
      ERROR_CALL_NOT_IMPLEMENTED=120;  { MSG%BAD_DYNALINK }
      ERROR_SEM_TIMEOUT       =121;    { MSG%SEM_TIMEOUT }
      ERROR_INSUFFICIENT_BUFFER=122;   { MSG%INSUFFICIENT_BUFFER }
      ERROR_INVALID_NAME      =123;    { MSG%INVALID_NAME }
{                                      =123%msg%HPFS_INVALID_VOLUME_CHAR }
      ERROR_INVALID_LEVEL     =124;    { MSG%INVALID_LEVEL }
      ERROR_NO_VOLUME_LABEL   =125;    { MSG%NO_VOLUME_LABEL }
      ERROR_MOD_NOT_FOUND     =126;    { MSG%MOD_NOT_FOUND }
      ERROR_PROC_NOT_FOUND    =127;    { MSG%PROC_NOT_FOUND }
      ERROR_WAIT_NO_CHILDREN  =128;    { MSG%none }
      ERROR_CHILD_NOT_COMPLETE=129;    { MSG%PROT_MODE_ONLY }
      ERROR_DIRECT_ACCESS_HANDLE=130;  { MSG%APPL_SINGLEFRAMECHAR }
      ERROR_NEGATIVE_SEEK     =131;    { MSG%APPL_DOUBLEFRAMECHAR }
      ERROR_SEEK_ON_DEVICE    =132;    { MSG%APPL_ARROWCHAR }
      ERROR_IS_JOIN_TARGET    =133;    { MSG%JOIN_ON_DRIV_IS_TAR }
      ERROR_IS_JOINED         =134;    { MSG%JOIN_DRIVE_IS }
      ERROR_IS_SUBSTED        =135;    { MSG%SUB_DRIVE_IS }
      ERROR_NOT_JOINED        =136;    { MSG%DRIVE_IS_NOT_JOINED }
      ERROR_NOT_SUBSTED       =137;    { MSG%DRIVE_NOT_SUBSTED }
      ERROR_JOIN_TO_JOIN      =138;    { MSG%JOIN_CANNOT_JOIN_DRIVE }
      ERROR_SUBST_TO_SUBST    =139;    { MSG%SUB_CANNOT_SUBST_DRIVE }
      ERROR_JOIN_TO_SUBST     =140;    { MSG%JOIN_CANNOT_SUB_DRIVE }
      ERROR_SUBST_TO_JOIN     =141;    { MSG%SUB_CANNOT_JOIN_DRIVE }
      ERROR_BUSY_DRIVE        =142;    { MSG%DRIVE_IS_BUSY }
      ERROR_SAME_DRIVE        =143;    { MSG%JOIN_SUB_SAME_DRIVE }
      ERROR_DIR_NOT_ROOT      =144;    { MSG%DIRECT_IS_NOT_SUBDIR }
      ERROR_DIR_NOT_EMPTY     =145;    { MSG%DIRECT_IS_NOT_EMPTY }
      ERROR_IS_SUBST_PATH     =146;    { MSG%PATH_USED_SUBST_JOIN }
      ERROR_IS_JOIN_PATH      =147;    { MSG%NO_NEEDED_RESOURCES }
      ERROR_PATH_BUSY         =148;    { MSG%PATH_BUSY }
      ERROR_IS_SUBST_TARGET   =149;    { MSG%SUB_ON_DRIVE_IS_JOIN }
      ERROR_SYSTEM_TRACE      =150;    { MSG%SYSTEM_TRACE }
      ERROR_INVALID_EVENT_COUNT=151;   { MSG%INVALID_EVENT_COUNT }
      ERROR_TOO_MANY_MUXWAITERS=152;   { MSG%TOO_MANY_MUXWAITERS }
      ERROR_INVALID_LIST_FORMAT=153;   { MSG%INVALID_LIST_FORMAT }
      ERROR_LABEL_TOO_LONG    =154;    { MSG%VOLUME_TOO_LONG }
{                                      =154%msg%HPFS_VOL_LABEL_LONG }
      ERROR_TOO_MANY_TCBS     =155;    { MSG%TOO_MANY_TCBS }
      ERROR_SIGNAL_REFUSED    =156;    { MSG%SIGNAL_REFUSED }
      ERROR_DISCARDED         =157;    { MSG%DISCARDED }
      ERROR_NOT_LOCKED        =158;    { MSG%NOT_LOCKED }
      ERROR_BAD_THREADID_ADDR =159;    { MSG%BAD_THREADID_ADDR }
      ERROR_BAD_ARGUMENTS     =160;    { MSG%BAD_ARGUMENTS }
      ERROR_BAD_PATHNAME      =161;    { MSG%none }
      ERROR_SIGNAL_PENDING    =162;    { MSG%SIGNAL_PENDING }
      ERROR_UNCERTAIN_MEDIA   =163;    { MSG%none }
      ERROR_MAX_THRDS_REACHED =164;    { MSG%MAX_THRDS_REACHED }
      ERROR_MONITORS_NOT_SUPPORTED=165;{ MSG%none }
      ERROR_UNC_DRIVER_NOT_INSTALLED=166;{ MSG%UNC_DRIVER_NOT_INSTALLED }
      ERROR_LOCK_FAILED       =167;    { MSG%LOCK_FAILED }
      ERROR_SWAPIO_FAILED     =168;    { MSG%SWAPIO_FAILED }
      ERROR_SWAPIN_FAILED     =169;    { MSG%SWAPIN_ATTEMPT_FAILED }
      ERROR_BUSY              =170;    { MSG%SEGMENT_BUSY }
{                                      =171%msg%INT_TOO_LONG }
      ERROR_CANCEL_VIOLATION  =173;    { MSG%UNLOCK_VIOLATION }
      ERROR_ATOMIC_LOCK_NOT_SUPPORTED=174;{ MSG%none }
      ERROR_READ_LOCKS_NOT_SUPPORTED=175;{ MSG%none }
      ERROR_INVALID_SEGMENT_NUMBER=180;{ MSG%INVALID_SEGMENT_NUM }
      ERROR_INVALID_CALLGATE  =181;    { MSG%none }
      ERROR_INVALID_ORDINAL   =182;    { MSG%INVALID_ORDINAL }
      ERROR_ALREADY_EXISTS    =183;    { MSG%none }
      ERROR_NO_CHILD_PROCESS  =184;    { MSG%none }
      ERROR_CHILD_ALIVE_NOWAIT=185;    { MSG%none }
      ERROR_INVALID_FLAG_NUMBER=186;   { MSG%INVALID_FLAG_NUMBER }
      ERROR_SEM_NOT_FOUND     =187;    { MSG%SEM_NOT_FOUND }
      ERROR_INVALID_STARTING_CODESEG=188;{ MSG%INVALID_STARTING_CODESEG }
      ERROR_INVALID_STACKSEG  =189;    { MSG%INVALID_STACKSEG }
      ERROR_INVALID_MODULETYPE=190;    { MSG%INVALID_MODULETYPE }
      ERROR_INVALID_EXE_SIGNATURE=191; { MSG%INVALID_EXE_SIGNATURE }
      ERROR_EXE_MARKED_INVALID=192;    { MSG%EXE_MARKED_INVALID }
      ERROR_BAD_EXE_FORMAT    =193;    { MSG%BAD_EXE_FORMAT }
      ERROR_ITERATED_DATA_EXCEEDS_64k=194;{ MSG%ITERATED_DATA_EXCEEDS_64K }
      ERROR_INVALID_MINALLOCSIZE=195;  { MSG%INVALID_MINALLOCSIZE }
      ERROR_DYNLINK_FROM_INVALID_RING=196;{ MSG%DYNLINK_FROM_INVALID_RING }
      ERROR_IOPL_NOT_ENABLED  =197;    { MSG%IOPL_NOT_ENABLED }
      ERROR_INVALID_SEGDPL    =198;    { MSG%INVALID_SEGDPL }
      ERROR_AUTODATASEG_EXCEEDS_64k=199;{ MSG%AUTODATASEG_EXCEEDS_64K }
      ERROR_RING2SEG_MUST_BE_MOVABLE=200;{ MSG%CODESEG_CANNOT_BE_64K }
      ERROR_RELOC_CHAIN_XEEDS_SEGLIM=201;{ MSG%RELOC_CHAIN_XEEDS_SEGMENT }
      ERROR_INFLOOP_IN_RELOC_CHAIN=202;    { MSG%INFLOOP_IN_RELOC_CHAIN }
      ERROR_ENVVAR_NOT_FOUND  =203;    { MSG%ENVVAR_NOT_FOUND }
      ERROR_NOT_CURRENT_CTRY  =204;    { MSG%none }
      ERROR_NO_SIGNAL_SENT    =205;    { MSG%SIGNAL_NOT_SENT }
      ERROR_FILENAME_EXCED_RANGE=206;  { MSG%NAME_TOO_LONG }
      ERROR_RING2_STACK_IN_USE=207;    { MSG%RING2_STACK_IN_USE }
      ERROR_META_EXPANSION_TOO_LONG=208;    { MSG%WILD_CARD_NAME }
      ERROR_INVALID_SIGNAL_NUMBER=209;    { MSG%INVALID_SIGNAL_NUMBER }
      ERROR_THREAD_1_INACTIVE =210;    { MSG%THREAD_1_INACTIVE }
      ERROR_INFO_NOT_AVAIL    =211;    { MSG%none }
      ERROR_LOCKED            =212;    { MSG%LOCKED }
      ERROR_BAD_DYNALINK      =213;    { MSG%none }
      ERROR_TOO_MANY_MODULES  =214;    { MSG%TOO_MANY_MODULES }
      ERROR_NESTING_NOT_ALLOWED=215;   { MSG%none }
      ERROR_CANNOT_SHRINK     =216;    { MSG%CANNOT_SHRINK }
      ERROR_ZOMBIE_PROCESS    =217;    { MSG%none }
      ERROR_STACK_IN_HIGH_MEMORY=218;  { MSG%none }
      ERROR_INVALID_EXITROUTINE_RING=219;    { MSG%INVALID_EXITROUTINE_RING }
      ERROR_GETBUF_FAILED     =220;    { MSG%none }
      ERROR_FLUSHBUF_FAILED   =221;    { MSG%none }
      ERROR_TRANSFER_TOO_LONG =222;    { MSG%none }
      ERROR_FORCENOSWAP_FAILED=223;    { MSG%none }
      ERROR_SMG_NO_TARGET_WINDOW=224;  { PM ID can't be selected }
      ERROR_NO_CHILDREN       =228;    { MSG%NO_CHILDREN }
      ERROR_INVALID_SCREEN_GROUP=229;  { MSG%none }
      ERROR_BAD_PIPE          =230;    { MSG%ERROR_BAD_PIPE }
      ERROR_PIPE_BUSY         =231;    { MSG%ERROR_PIPE_BUSY }
      ERROR_NO_DATA           =232;    { MSG%ERROR_NO_DATA }
      ERROR_PIPE_NOT_CONNECTED=233;    { MSG%ERROR_PIPE_NOT_CONNECTED }
      ERROR_MORE_DATA         =234;    { MSG%ERROR_MORE_DATA }
      ERROR_VC_DISCONNECTED   =240;    { MSG%ERROR_VC_DISCONNECTED }
      ERROR_CIRCULARITY_REQUESTED=250; { MSG%CIRCULARITY_REQUESTED }
      ERROR_DIRECTORY_IN_CDS  =251;    { MSG%DIRECTORY_IN_CDS }
      ERROR_INVALID_FSD_NAME  =252;    { MSG%INVALID_FSD_NAME }
      ERROR_INVALID_PATH      =253;    { MSG%INVALID_PATH }
      ERROR_INVALID_EA_NAME   =254;    { MSG%INVALID_EA_NAME }
      ERROR_EA_LIST_INCONSISTENT=255;  { MSG%EA_LIST_INCONSISTENT }
      ERROR_EA_LIST_TOO_LONG  =256;    { MSG%EA_LIST_TOO_LONG }
      ERROR_NO_META_MATCH     =257;    { MSG%NO_META_MATCH }
      ERROR_FINDNOTIFY_TIMEOUT=258;    { MSG%FINDNOTIFY_TIMEOUT }
      ERROR_NO_MORE_ITEMS     =259;    { MSG%NO_MORE_ITEMS }
      ERROR_SEARCH_STRUC_REUSED=260;   { MSG%SEARCH_STRUC_REUSED }
      ERROR_CHAR_NOT_FOUND    =261;    { MSG%CHAR_NOT_FOUND }
      ERROR_TOO_MUCH_STACK    =262;    { MSG%TOO_MUCH_STACK }
      ERROR_INVALID_ATTR      =263;    { MSG%INVALID_ATTR }
      ERROR_INVALID_STARTING_RING=264; { MSG%INVALID_STARTING_RING }
      ERROR_INVALID_DLL_INIT_RING=265; { MSG%INVALID_DLL_INIT_RING }
      ERROR_CANNOT_COPY       =266;    { MSG%CANNOT_COPY }
      ERROR_DIRECTORY         =267;    { MSG%DIRECTORY }
      ERROR_OPLOCKED_FILE     =268;    { MSG%OPLOCKED_FILE }
      ERROR_OPLOCK_THREAD_EXISTS=269;  { MSG%OPLOCK_THREAD_EXISTS }
      ERROR_VOLUME_CHANGED    =270;    { MSG%none }
      ERROR_FINDNOTIFY_HANDLE_IN_USE=271;    { MSG%none }
      ERROR_FINDNOTIFY_HANDLE_CLOSED=272;    { MSG%none }
      ERROR_NOTIFY_OBJECT_REMOVED=273; { MSG%none }
      ERROR_ALREADY_SHUTDOWN  =274;    { MSG%none }
      ERROR_EAS_DIDNT_FIT     =275;    { MSG%none }
      ERROR_EA_FILE_CORRUPT   =276;    { MSG%ERROR_EAS_CORRUPT }
      ERROR_EA_TABLE_FULL     =277;    { MSG%EA_TABLE_FULL }
      ERROR_INVALID_EA_HANDLE =278;    { MSG%INVALID_EA_HANDLE }
      ERROR_NO_CLUSTER        =279;    { MSG%NO_CLUSTER }
      ERROR_CREATE_EA_FILE    =280;    { MSG%ERROR_CREATE_EA_FILE }
      ERROR_CANNOT_OPEN_EA_FILE=281;   { MSG%CANNOT_OPEN_FILE }
      ERROR_EAS_NOT_SUPPORTED =282;    { MSG%EAS_NOT_SUPPORTED }
      ERROR_NEED_EAS_FOUND    =283;    { MSG%NEED_EAS_FOUND }
      ERROR_DUPLICATE_HANDLE  =284;    { MSG%EAS_DISCARDED }
      ERROR_DUPLICATE_NAME    =285;    { MSG%DUPLICATE_SEM_NAME }
      ERROR_EMPTY_MUXWAIT     =286;    { MSG%EMPTY_MUXWAIT_SEM }
      ERROR_MUTEX_OWNED       =287;    { MSG%MUTEX_SEM_OWNED }
      ERROR_NOT_OWNER         =288;    { MSG%NOT_MUTEX_SEM_OWNER }
      ERROR_PARAM_TOO_SMALL   =289;    { MSG%QUERY_MUX_PARAM_TOO_SMALL }
      ERROR_TOO_MANY_HANDLES  =290;    { MSG%TOO_MANY_SEM_HANDLES }
      ERROR_TOO_MANY_OPENS    =291;    { MSG%TOO_MANY_SEM_OPENS }
      ERROR_WRONG_TYPE        =292;    { MSG%SEM_WRONG_TYPE }
      ERROR_UNUSED_CODE       =293;    { MSG%none }
      ERROR_THREAD_NOT_TERMINATED=294; { MSG%none }
      ERROR_INIT_ROUTINE_FAILED=295;   { MSG%none }
      ERROR_MODULE_IN_USE     =296;    { MSG%none }
      ERROR_NOT_ENOUGH_WATCHPOINTS=297;{ MSG%none }
      ERROR_TOO_MANY_POSTS    =298;    { MSG%TOO_MANY_EVENT_SEM_POSTS }
      ERROR_ALREADY_POSTED    =299;    { MSG%EVENT_SEM_ALREADY_POSTED }
      ERROR_ALREADY_RESET     =300;    { MSG%EVENT_SEM_ALREADY_RESET }
      ERROR_SEM_BUSY          =301;    { MSG%SEM_BUSY }

{ end of set 0;- 302;}

      ERROR_USER_DEFINED_BASE =$FF00;

      ERROR_I24_WRITE_PROTECT         =0;
      ERROR_I24_BAD_UNIT              =1;
      ERROR_I24_NOT_READY             =2;
      ERROR_I24_BAD_COMMAND           =3;
      ERROR_I24_CRC                   =4;
      ERROR_I24_BAD_LENGTH            =5;
      ERROR_I24_SEEK                  =6;
      ERROR_I24_NOT_DOS_DISK          =7;
      ERROR_I24_SECTOR_NOT_FOUND      =8;
      ERROR_I24_OUT_OF_PAPER          =9;
      ERROR_I24_WRITE_FAULT           =10;
      ERROR_I24_READ_FAULT            =11;
      ERROR_I24_GEN_FAILURE           =12;
      ERROR_I24_DISK_CHANGE           =13;
      ERROR_I24_WRONG_DISK            =15;
      ERROR_I24_UNCERTAIN_MEDIA       =16;
      ERROR_I24_CHAR_CALL_INTERRUPTED =17;
      ERROR_I24_NO_MONITOR_SUPPORT    =18;
      ERROR_I24_INVALID_PARAMETER     =19;
      ERROR_I24_DEVICE_IN_USE         =20;
      ERROR_I24_QUIET_INIT_FAIL       =21;

      ALLOWED_FAIL                    =$0001;
      ALLOWED_ABORT                   =$0002;
      ALLOWED_RETRY                   =$0004;
      ALLOWED_IGNORE                  =$0008;
      ALLOWED_ACKNOWLEDGE             =$0010;
      ALLOWED_DISPATCH                =$8000;
      ALLOWED_REGDUMP                 =$0020;
      ALLOWED_DETACHED                =ALLOWED_DISPATCH;
      ALLOWED_RESERVED                =NOT (ALLOWED_FAIL OR ALLOWED_ABORT OR
                                            ALLOWED_RETRY OR ALLOWED_IGNORE OR
                                            ALLOWED_ACKNOWLEDGE);

      I24_OPERATION                   =$01;
      I24_AREA                        =$06;
      I24_CLASS                       =$80;

{ Values for error CLASS }
      ERRCLASS_OUTRES                 =1;  { Out of Resource                }
      ERRCLASS_TEMPSIT                =2;  { Temporary Situation            }
      ERRCLASS_AUTH                   =3;  { Permission problem             }
      ERRCLASS_INTRN                  =4;  { Internal System Error          }
      ERRCLASS_HRDFAIL                =5;  { Hardware Failure               }
      ERRCLASS_SYSFAIL                =6;  { System Failure                 }
      ERRCLASS_APPERR                 =7;  { Application Error              }
      ERRCLASS_NOTFND                 =8;  { Not Found                      }
      ERRCLASS_BADFMT                 =9;  { Bad Format                     }
      ERRCLASS_LOCKED                 =10; { Locked                         }
      ERRCLASS_MEDIA                  =11; { Media Failure                  }
      ERRCLASS_ALREADY                =12; { Collision with Existing Item   }
      ERRCLASS_UNK                    =13; { Unknown/other                  }
      ERRCLASS_CANT                   =14;
      ERRCLASS_TIME                   =15;

{ Values for error ACTION }
      ERRACT_RETRY                    =1;  { Retry                          }
      ERRACT_DLYRET                   =2;  { Delay Retry, retry after pause }
      ERRACT_USER                     =3;  { Ask user to regive information }
      ERRACT_ABORT                    =4;  { abort with clean up            }
      ERRACT_PANIC                    =5;  { abort immediately              }
      ERRACT_IGNORE                   =6;  { ignore                         }
      ERRACT_INTRET                   =7;  { Retry after User Intervention  }

{ Values for error LOCUS }
      ERRLOC_UNK                      =1;  { No appropriate value           }
      ERRLOC_DISK                     =2;  { Random Access Mass Storage     }
      ERRLOC_NET                      =3;  { Network                        }
      ERRLOC_SERDEV                   =4;  { Serial Device                  }
      ERRLOC_MEM                      =5;  { Memory                         }

{ Abnormal termination codes }
      TC_NORMAL                       =0;
      TC_HARDERR                      =1;
      TC_GP_TRAP                      =2;
      TC_SIGNAL                       =3;
      TC_XCPT                         =4;


      ERROR_INVALID_PROCID    =303;    { MSG%none }
      ERROR_INVALID_PDELTA    =304;    { MSG%none }
      ERROR_NOT_DESCENDANT    =305;    { MSG%none }
      ERROR_NOT_SESSION_MANAGER=306;   { MSG%none }
      ERROR_INVALID_PCLASS    =307;    { MSG%none }
      ERROR_INVALID_SCOPE     =308;    { MSG%none }
      ERROR_INVALID_THREADID  =309;    { MSG%none }
      ERROR_DOSSUB_SHRINK     =310;    { MSG%none }
      ERROR_DOSSUB_NOMEM      =311;    { MSG%none }
      ERROR_DOSSUB_OVERLAP    =312;    { MSG%none }
      ERROR_DOSSUB_BADSIZE    =313;    { MSG%none }
      ERROR_DOSSUB_BADFLAG    =314;    { MSG%none }
      ERROR_DOSSUB_BADSELECTOR=315;    { MSG%none }
      ERROR_MR_MSG_TOO_LONG   =316;    { MSG%MR_MSG_TOO_LONG }
      MGS_MR_MSG_TOO_LONG     =316;
      ERROR_MR_MID_NOT_FOUND  =317;    { MSG%MR_CANT_FORMAT }
      ERROR_MR_UN_ACC_MSGF    =318;    { MSG%MR_NOT_FOUND }
      ERROR_MR_INV_MSGF_FORMAT=319;    { MSG%MR_READ_ERROR }
      ERROR_MR_INV_IVCOUNT    =320;    { MSG%MR_IVCOUNT_ERROR }
      ERROR_MR_UN_PERFORM     =321;    { MSG%MR_UN_PERFORM }
      ERROR_TS_WAKEUP         =322;    { MSG%none }
      ERROR_TS_SEMHANDLE      =323;    { MSG%none }
      ERROR_TS_NOTIMER        =324;    { MSG%none }
      ERROR_TS_HANDLE         =326;    { MSG%none }
      ERROR_TS_DATETIME       =327;    { MSG%none }
      ERROR_SYS_INTERNAL      =328;    { MSG%none }
      ERROR_QUE_CURRENT_NAME  =329;    { MSG%none }
      ERROR_QUE_PROC_NOT_OWNED=330;    { MSG%QUE_PROC_NOT_OWNED }
      ERROR_QUE_PROC_OWNED    =331;    { MSG%none }
      ERROR_QUE_DUPLICATE     =332;    { MSG%QUE_DUPLICATE }
      ERROR_QUE_ELEMENT_NOT_EXIST=333; { MSG%QUE_ELEMENT_NOT_EXIST }
      ERROR_QUE_NO_MEMORY     =334;    { MSG%QUE_NO_MEMORY }
      ERROR_QUE_INVALID_NAME  =335;    { MSG%none }
      ERROR_QUE_INVALID_PRIORITY=336;  { MSG%none }
      ERROR_QUE_INVALID_HANDLE=337;    { MSG%none }
      ERROR_QUE_LINK_NOT_FOUND=338;    { MSG%none }
      ERROR_QUE_MEMORY_ERROR  =339;    { MSG%none }
      ERROR_QUE_PREV_AT_END   =340;    { MSG%none }
      ERROR_QUE_PROC_NO_ACCESS=341;    { MSG%none }
      ERROR_QUE_EMPTY         =342;    { MSG%none }
      ERROR_QUE_NAME_NOT_EXIST=343;    { MSG%none }
      ERROR_QUE_NOT_INITIALIZED=344;   { MSG%none }
      ERROR_QUE_UNABLE_TO_ACCESS=345;  { MSG%none }
      ERROR_QUE_UNABLE_TO_ADD =346;    { MSG%none }
      ERROR_QUE_UNABLE_TO_INIT=347;    { MSG%none }
      ERROR_VIO_INVALID_MASK  =349;    { MSG%VIO_INVALID_MASK }
      ERROR_VIO_PTR           =350;    { MSG%VIO_PTR }
      ERROR_VIO_APTR          =351;    { MSG%none }
      ERROR_VIO_RPTR          =352;    { MSG%none }
      ERROR_VIO_CPTR          =353;    { MSG%none }
      ERROR_VIO_LPTR          =354;    { MSG%none }
      ERROR_VIO_MODE          =355;    { MSG%DIS_ERROR }
      ERROR_VIO_WIDTH         =356;    { MSG%VIO_WIDTH }
      ERROR_VIO_ATTR          =357;    { MSG%none }
      ERROR_VIO_ROW           =358;    { MSG%VIO_ROW }
      ERROR_VIO_COL           =359;    { MSG%VIO_COL }
      ERROR_VIO_TOPROW        =360;    { MSG%none }
      ERROR_VIO_BOTROW        =361;    { MSG%none }
      ERROR_VIO_RIGHTCOL      =362;    { MSG%none }
      ERROR_VIO_LEFTCOL       =363;    { MSG%none }
      ERROR_SCS_CALL          =364;    { MSG%none }
      ERROR_SCS_VALUE         =365;    { MSG%none }
      ERROR_VIO_WAIT_FLAG     =366;    { MSG%VIO_WAIT_FLAG }
      ERROR_VIO_UNLOCK        =367;    { MSG%VIO_UNLOCK }
      ERROR_SGS_NOT_SESSION_MGR=368;   { MSG%none }
      ERROR_SMG_INVALID_SGID  =369;    { MSG%SMG_INVALID_SESSION_ID }
      ERROR_SMG_INVALID_SESSION_ID=ERROR_SMG_INVALID_SGID;
      ERROR_SMG_NOSG          =370;    { MSG%none }
      ERROR_SMG_NO_SESSIONS   =370;    { MSG%none }
      ERROR_SMG_GRP_NOT_FOUND =371;    { MSG%SMG_GRP_NOT_FOUND }
      ERROR_SMG_SESSION_NOT_FOUND=ERROR_SMG_GRP_NOT_FOUND;
{                                      =371%msg%SMG_SESSION_NOT_FOUND }
      ERROR_SMG_SET_TITLE     =372;    { MSG%SMG_SET_TITLE }
      ERROR_KBD_PARAMETER     =373;    { MSG%KBD_PARAMETER }
      ERROR_KBD_NO_DEVICE     =374;    { MSG%none }
      ERROR_KBD_INVALID_IOWAIT=375;    { MSG%KBD_INVALID_IOWAIT }
      ERROR_KBD_INVALID_LENGTH=376;    { MSG%KBD_INVALID_LENGTH }
      ERROR_KBD_INVALID_ECHO_MASK=377; { MSG%KBD_INVALID_ECHO_MASK }
{                                      =377%msg%KBD_INVALID_INPUT_MASK }
      ERROR_KBD_INVALID_INPUT_MASK=378;{ MSG%none }
      ERROR_MON_INVALID_PARMS =379;    { MSG%MON_INVALID_PARMS }
      ERROR_MON_INVALID_DEVNAME=380;   { MSG%MON_INVALID_DEVNAME }
      ERROR_MON_INVALID_HANDLE=381;    { MSG%MON_INVALID_HANDLE }
      ERROR_MON_BUFFER_TOO_SMALL=382;  { MSG%MON_BUFFER_TOO_SMALL }
      ERROR_MON_BUFFER_EMPTY  =383;    { MSG%MON_BUFFER_EMPTY }
      ERROR_MON_DATA_TOO_LARGE=384;    { MSG%MON_DATA_TOO_LARGE }
      ERROR_MOUSE_NO_DEVICE   =385;    { MSG%MOUSE_NO_DEVICE }
      ERROR_MOUSE_INV_HANDLE  =386;    { MSG%MOUSE_INV_HANDLE }
      ERROR_MOUSE_INV_PARMS   =387;    { MSG%MOUSE_CALLER_NOT_SYBSYS }
      ERROR_MOUSE_CANT_RESET  =388;    { MSG%none }
      ERROR_MOUSE_DISPLAY_PARMS=389;   { MSG%none }
      ERROR_MOUSE_INV_MODULE  =390;    { MSG%none }
      ERROR_MOUSE_INV_ENTRY_PT=391;    { MSG%none }
      ERROR_MOUSE_INV_MASK    =392;    { MSG%none }
      NO_ERROR_MOUSE_NO_DATA  =393;    { MSG%none }
      NO_ERROR_MOUSE_PTR_DRAWN=394;    { MSG%none }
      ERROR_INVALID_FREQUENCY =395;    { MSG%none }
      ERROR_NLS_NO_COUNTRY_FILE=396;   { MSG%NLS_NO_COUNTRY_FILE }
{                                      =396%msg%NO_COUNTRY_SYS }
      ERROR_NLS_OPEN_FAILED   =397;    { MSG%NLS_OPEN_FAILED }
{                                      =397%msg%OPEN_COUNTRY_SYS }
      ERROR_NLS_NO_CTRY_CODE  =398;    { MSG%NLS_NO_CTRY_CODE }
      ERROR_NO_COUNTRY_OR_CODEPAGE=398;{ MSG%NO_COUNTRY_OR_CODEPAGE }
      ERROR_NLS_TABLE_TRUNCATED=399;   { MSG%NLS_TABLE_TRUNCATED }
      ERROR_NLS_BAD_TYPE      =400;    { MSG%NLS_BAD_TYPE }
      ERROR_NLS_TYPE_NOT_FOUND=401;    { MSG%NLS_TYPE_NOT_FOUND }
{                                      =401%msg%COUNTRY_NO_TYPE }
      ERROR_VIO_SMG_ONLY      =402;    { MSG%SWAPIN_FAILED }
      ERROR_VIO_INVALID_ASCIIZ=403;    { MSG%SEGVALIDATE_FAILURE }
      ERROR_VIO_DEREGISTER    =404;    { MSG%VIO_DEREGISTER }
      ERROR_VIO_NO_POPUP      =405;    { MSG%VIO_NO_POPUP }
      ERROR_VIO_EXISTING_POPUP=406;    { MSG%VIO_EXISTING_POPUP }
      ERROR_KBD_SMG_ONLY      =407;    { MSG%KBD_SMG_ONLY }
      ERROR_KBD_INVALID_ASCIIZ=408;    { MSG%KBD_INVALID_ASCIIZ }
      ERROR_KBD_INVALID_MASK  =409;    { MSG%KBD_INVALID_MASK }
      ERROR_KBD_REGISTER      =410;    { MSG%KBD_REGISTER }
      ERROR_KBD_DEREGISTER    =411;    { MSG%KBD_DEREGISTER }
      ERROR_MOUSE_SMG_ONLY    =412;    { MSG%MOUSE_SMG_ONLY }
      ERROR_MOUSE_INVALID_ASCIIZ=413;  { MSG%MOUSE_INVALID_ASCIIZ }
      ERROR_MOUSE_INVALID_MASK=414;    { MSG%MOUSE_INVALID_MASK }
      ERROR_MOUSE_REGISTER    =415;    { MSG%MOUSE_REGISTER }
      ERROR_MOUSE_DEREGISTER  =416;    { MSG%MOUSE_DEREGISTER }
      ERROR_SMG_BAD_ACTION    =417;    { MSG%SMG_BAD_ACTION }
      ERROR_SMG_INVALID_CALL  =418;    { MSG%SMG_INVALID_CALL }
      ERROR_SCS_SG_NOTFOUND   =419;    { MSG%none }
      ERROR_SCS_NOT_SHELL     =420;    { MSG%none }
      ERROR_VIO_INVALID_PARMS =421;    { MSG%VIO_INVALID_PARMS }
      ERROR_VIO_FUNCTION_OWNED=422;    { MSG%VIO_FUNCTION_OWNED }
      ERROR_VIO_RETURN        =423;    { MSG%none }
      ERROR_SCS_INVALID_FUNCTION=424;  { MSG%none }
      ERROR_SCS_NOT_SESSION_MGR=425;   { MSG%none }
      ERROR_VIO_REGISTER      =426;    { MSG%VIO_REGISTER }
      ERROR_VIO_NO_MODE_THREAD=427;    { MSG%none }
      ERROR_VIO_NO_SAVE_RESTORE_THD=428;{ MSG%VIO_NO_SAVE_RESTORE_THD }
      ERROR_VIO_IN_BG         =429;    { MSG%VIO_IN_BG }
      ERROR_VIO_ILLEGAL_DURING_POPUP=430;    { MSG%VIO_ILLEGAL_DURING_POPUP }
      ERROR_SMG_NOT_BASESHELL =431;    { MSG%SMG_NOT_BASESHELL }
      ERROR_SMG_BAD_STATUSREQ =432;    { MSG%SMG_BAD_STATUSREQ }
      ERROR_QUE_INVALID_WAIT  =433;    { MSG%none }
      ERROR_VIO_LOCK          =434;    { MSG%VIO_LOCK }
      ERROR_MOUSE_INVALID_IOWAIT=435;  { MSG%MOUSE_INVALID_IOWAIT }
      ERROR_VIO_INVALID_HANDLE=436;    { MSG%VIO_INVALID_HANDLE }
      ERROR_VIO_ILLEGAL_DURING_LOCK=437;    { MSG%none }
      ERROR_VIO_INVALID_LENGTH=438;    { MSG%VIO_INVALID_LENGTH }
      ERROR_KBD_INVALID_HANDLE=439;    { MSG%KBD_INVALID_HANDLE }
      ERROR_KBD_NO_MORE_HANDLE=440;    { MSG%KBD_NO_MORE_HANDLE }
      ERROR_KBD_CANNOT_CREATE_KCB=441; { MSG%KBD_CANNOT_CREATE_KCB }
      ERROR_KBD_CODEPAGE_LOAD_INCOMPL=442;    { MSG%KBD_CODEPAGE_LOAD_INCOMPL }
      ERROR_KBD_INVALID_CODEPAGE_ID=443;    { MSG%KBD_INVALID_CODEPAGE_ID }
      ERROR_KBD_NO_CODEPAGE_SUPPORT=444;    { MSG%KBD_NO_CODEPAGE_SUPPORT }
      ERROR_KBD_FOCUS_REQUIRED=445;    { MSG%KBD_FOCUS_REQUIRED }
      ERROR_KBD_FOCUS_ALREADY_ACTIVE=446;    { MSG%KBD_FOCUS_ALREADY_ACTIVE }
      ERROR_KBD_KEYBOARD_BUSY =447;    { MSG%KBD_KEYBOARD_BUSY }
      ERROR_KBD_INVALID_CODEPAGE=448;  { MSG%KBD_INVALID_CODEPAGE }
      ERROR_KBD_UNABLE_TO_FOCUS=449;   { MSG%KBD_UNABLE_TO_FOCUS }
      ERROR_SMG_SESSION_NON_SELECT=450;{ MSG%SMG_SESSION_NON_SELECT }
      ERROR_SMG_SESSION_NOT_FOREGRND=451;    { MSG%SMG_SESSION_NOT_FOREGRND }
      ERROR_SMG_SESSION_NOT_PARENT=452;    { MSG%SMG_SESSION_NOT_PARENT }
      ERROR_SMG_INVALID_START_MODE=453;    { MSG%SMG_INVALID_START_MODE }
      ERROR_SMG_INVALID_RELATED_OPT=454;{ MSG%SMG_INVALID_RELATED_OPT }
      ERROR_SMG_INVALID_BOND_OPTION=455;    { MSG%SMG_INVALID_BOND_OPTION }
      ERROR_SMG_INVALID_SELECT_OPT=456;{ MSG%SMG_INVALID_SELECT_OPT }
      ERROR_SMG_START_IN_BACKGROUND=457;{ MSG%SMG_START_IN_BACKGROUND }
      ERROR_SMG_INVALID_STOP_OPTION=458;{ MSG%SMG_INVALID_STOP_OPTION }
      ERROR_SMG_BAD_RESERVE   =459;    { MSG%SMG_BAD_RESERVE }
      ERROR_SMG_PROCESS_NOT_PARENT=460;{ MSG%SMG_PROCESS_NOT_PARENT }
      ERROR_SMG_INVALID_DATA_LENGTH=461;    { MSG%SMG_INVALID_DATA_LENGTH }
      ERROR_SMG_NOT_BOUND     =462;    { MSG%SMG_NOT_BOUND }
      ERROR_SMG_RETRY_SUB_ALLOC=463;   { MSG%SMG_RETRY_SUB_ALLOC }
      ERROR_KBD_DETACHED      =464;    { MSG%KBD_DETACHED }
      ERROR_VIO_DETACHED      =465;    { MSG%VIO_DETACHED }
      ERROR_MOU_DETACHED      =466;    { MSG%MOU_DETACHED }
      ERROR_VIO_FONT          =467;    { MSG%VIO_FONT }
      ERROR_VIO_USER_FONT     =468;    { MSG%VIO_USER_FONT }
      ERROR_VIO_BAD_CP        =469;    { MSG%VIO_BAD_CP }
      ERROR_VIO_NO_CP         =470;    { MSG%none }
      ERROR_VIO_NA_CP         =471;    { MSG%VIO_NA_CP }
      ERROR_INVALID_CODE_PAGE =472;    { MSG%none }
      ERROR_CPLIST_TOO_SMALL  =473;    { MSG%none }
      ERROR_CP_NOT_MOVED      =474;    { MSG%none }
      ERROR_MODE_SWITCH_INIT  =475;    { MSG%none }
      ERROR_CODE_PAGE_NOT_FOUND=476;   { MSG%none }
      ERROR_UNEXPECTED_SLOT_RETURNED=477;    { MSG%none }
      ERROR_SMG_INVALID_TRACE_OPTION=478;    { MSG%SMG_INVALID_TRACE_OPTION }
      ERROR_VIO_INTERNAL_RESOURCE=479; { MSG%none }
      ERROR_VIO_SHELL_INIT    =480;    { MSG%VIO_SHELL_INIT }
      ERROR_SMG_NO_HARD_ERRORS=481;    { MSG%SMG_NO_HARD_ERRORS }
      ERROR_CP_SWITCH_INCOMPLETE=482;  { MSG%none }
      ERROR_VIO_TRANSPARENT_POPUP=483; { MSG%VIO_TRANSPARENT_POPUP }
      ERROR_CRITSEC_OVERFLOW  =484;    { MSG%none }
      ERROR_CRITSEC_UNDERFLOW =485;    { MSG%none }
      ERROR_VIO_BAD_RESERVE   =486;    { MSG%VIO_BAD_RESERVE }
      ERROR_INVALID_ADDRESS   =487;    { MSG%INVALID_ADDRESS }
      ERROR_ZERO_SELECTORS_REQUESTED=488;    { MSG%ZERO_SELECTORS_REQUESTED }
      ERROR_NOT_ENOUGH_SELECTORS_AVA=489;    { MSG%NOT_ENOUGH_SELECTORS_AVA }
      ERROR_INVALID_SELECTOR  =490;    { MSG%INVALID_SELECTOR }
      ERROR_SMG_INVALID_PROGRAM_TYPE=491;    { MSG%SMG_INVALID_PROGRAM_TYPE }
      ERROR_SMG_INVALID_PGM_CONTROL=492;    { MSG%SMG_INVALID_PGM_CONTROL }
      ERROR_SMG_INVALID_INHERIT_OPT=493;    { MSG%SMG_INVALID_INHERIT_OPT }
      ERROR_VIO_EXTENDED_SG   =494;    { MSG%VIO_EXTENDED_SG }
      ERROR_VIO_NOT_PRES_MGR_SG=495;   { MSG%VIO_NOT_PRES_MGR_SG }
      ERROR_VIO_SHIELD_OWNED  =496;    { MSG%VIO_SHIELD_OWNED }
      ERROR_VIO_NO_MORE_HANDLES=497;   { MSG%VIO_NO_MORE_HANDLES }
      ERROR_VIO_SEE_ERROR_LOG =498;    { MSG%none }
      ERROR_VIO_ASSOCIATED_DC =499;    { MSG%none }
      ERROR_KBD_NO_CONSOLE    =500;    { MSG%KBD_NO_CONSOLE }
      ERROR_MOUSE_NO_CONSOLE  =501;    { MSG%DOS_STOPPED }
      ERROR_MOUSE_INVALID_HANDLE=502;  { MSG%MOUSE_INVALID_HANDLE }
      ERROR_SMG_INVALID_DEBUG_PARMS=503;{ MSG%SMG_INVALID_DEBUG_PARMS }
      ERROR_KBD_EXTENDED_SG   =504;    { MSG%KBD_EXTENDED_SG }
      ERROR_MOU_EXTENDED_SG   =505;    { MSG%MOU_EXTENDED_SG }
      ERROR_SMG_INVALID_ICON_FILE=506; { MSG%none }
      ERROR_TRC_PID_NON_EXISTENT=507;  { MSG%TRC_PID_NON_EXISTENT }
      ERROR_TRC_COUNT_ACTIVE  =508;    { MSG%TRC_COUNT_ACTIVE }
      ERROR_TRC_SUSPENDED_BY_COUNT=509;{ MSG%TRC_SUSPENDED_BY_COUNT }
      ERROR_TRC_COUNT_INACTIVE=510;    { MSG%TRC_COUNT_INACTIVE }
      ERROR_TRC_COUNT_REACHED =511;    { MSG%TRC_COUNT_REACHED }
      ERROR_NO_MC_TRACE       =512;    { MSG%NO_MC_TRACE }
      ERROR_MC_TRACE          =513;    { MSG%MC_TRACE }
      ERROR_TRC_COUNT_ZERO    =514;    { MSG%TRC_COUNT_ZERO }
      ERROR_SMG_TOO_MANY_DDS  =515;    { MSG%SMG_TOO_MANY_DDS }
      ERROR_SMG_INVALID_NOTIFICATION=516;    { MSG%SMG_INVALID_NOTIFICATION }
      ERROR_LF_INVALID_FUNCTION=517;   { MSG%LF_INVALID_FUNCTION }
      ERROR_LF_NOT_AVAIL      =518;    { MSG%LF_NOT_AVAIL }
      ERROR_LF_SUSPENDED      =519;    { MSG%LF_SUSPENDED }
      ERROR_LF_BUF_TOO_SMALL  =520;    { MSG%LF_BUF_TOO_SMALL }
      ERROR_LF_BUFFER_CORRUPTED=521;   { MSG%none }
      ERROR_LF_BUFFER_FULL    =521;    { MSG%LF_BUF_FULL }
      ERROR_LF_INVALID_DAEMON =522;    { MSG%none }
      ERROR_LF_INVALID_RECORD =522;    { MSG%LF_INVAL_RECORD }
      ERROR_LF_INVALID_TEMPL  =523;    { MSG%none }
      ERROR_LF_INVALID_SERVICE=523;    { MSG%LF_INVAL_SERVICE }
      ERROR_LF_GENERAL_FAILURE=524;    { MSG%LF_GENERAL_FAILURE }
      ERROR_LF_INVALID_ID     =525;    { MSG%HPFS_DISK_ALREADY_INUSE }
      ERROR_LF_INVALID_HANDLE =526;    { MSG%HPFS_CANNOT_FORMAT_DISK }
      ERROR_LF_NO_ID_AVAIL    =527;    { MSG%HPFS_CANNOT_COPY_SYS_DATA }
      ERROR_LF_TEMPLATE_AREA_FULL=528; { MSG%HPFS_FORMAT_NOT_DONE }
      ERROR_LF_ID_IN_USE      =529;    { MSG%HPFS_FMT_NOT_ENOUGH_MEM }
      ERROR_MOU_NOT_INITIALIZED=530;   { MSG%HPFS_SPECIFY_FIXDSK }
      ERROR_MOUINITREAL_DONE  =531;    { MSG%HPFS_SPECIFY_ONE_DRIVE }
      ERROR_DOSSUB_CORRUPTED  =532;    { MSG%HPFS_UNKNOWN_ERR_NO_FORMAT }
      ERROR_MOUSE_CALLER_NOT_SUBSYS=533;    { MSG%HPFS_SYNTAX_HELP }
      ERROR_ARITHMETIC_OVERFLOW=534;   { MSG%HPFS_DISK_FORMATING }
      ERROR_TMR_NO_DEVICE     =535;    { MSG%HPFS_AVAIL_DISK_SPACE }
      ERROR_TMR_INVALID_TIME  =536;    { MSG%HPFS_BAD_BLOCKS }
      ERROR_PVW_INVALID_ENTITY=537;    { MSG%HPFS_DISK_SPACE_AVAIL }
      ERROR_PVW_INVALID_ENTITY_TYPE=538;    { MSG%HPFS_SPACE_FORMATTED }
      ERROR_PVW_INVALID_SPEC  =539;    { MSG%HPFS_TYPE_CUR_VOLUME_LABEL }
      ERROR_PVW_INVALID_RANGE_TYPE=540;{ MSG%HPFS_DRIVER_NOT_LOADED }
      ERROR_PVW_INVALID_COUNTER_BLK=541;    { MSG%HPFS_DRIVER_LOADER }
      ERROR_PVW_INVALID_TEXT_BLK=542;  { MSG%HPFS_CACHE_BUF_SPECIFIED }
      ERROR_PRF_NOT_INITIALIZED=543;   { MSG%HPFS_CHKDSK_PARM_ERROR }
      ERROR_PRF_ALREADY_INITIALIZED=544;    { MSG%HPFS_CHKDSK_NOACCESS_DRIVE }
      ERROR_PRF_NOT_STARTED   =545;    { MSG%HPFS_UNKNOWN_ERR_NO_CHKDSK }
      ERROR_PRF_ALREADY_STARTED=546;   { MSG%HPFS_CHKDSK_NOT_ENOUGH_MEM }
      ERROR_PRF_TIMER_OUT_OF_RANGE=547;{ MSG%HPFS_CHKDSK_NOWRITEODATA }
      ERROR_PRF_TIMER_RESET   =548;    { MSG%HPFS_CHKDSK_NORECOVER_DATA }
{                                      =549%msg%HPFS_CHKDSK_NO_PARM_SPACE }
{                                      =550%msg%HPFS_CHKDSK_NORECOGNIZE }
{                                      =551%msg%HPFS_CHKDSK_NOROOT_FIND }
{                                      =552%msg%HPFS_CHKDSK_NOFIX_FS_ERROR }
{                                      =553%msg%HPFS_CHKDSK_CORRECT_FS_ERR }
{                                      =554%msg%HPFS_CHKDSK_ORGAN_FIX }
{                                      =555%msg%HPFS_CHKDSK_RELOC_BBPDATA }
{                                      =556%msg%HPFS_CHKDSK_REM_CORRU_BLOC }
{                                      =557%msg%HPFS_CHKDSK_REM_CORRUP_FIL }
{                                      =558%msg%HPFS_CHKDSK_FIX_SPACE_ALLO }
{                                      =559%msg%HPFS_NOT_FORMATTED_DISK }
{                                      =560%msg%HPFS_CHKDSK_COR_ALLOC }
{                                      =561%msg%HPFS_CHKDSK_SEARC_UNALLOC }
{                                      =562%msg%HPFS_CHKDSK_DET_LOST_DATA }
{                                      =563%msg%HPFS_CHKDSK_PERCENT_SEARC }
{                                      =564%msg%HPFS_CHKDSK_LOST_DATASEARC }
{                                      =565%msg%HPFS_CHKDSK_CRIT_NOREAD }
{                                      =566%msg%HPFS_CHKDSK_DISK_INUSE }
{                                      =567%msg%HPFS_CHKDSK_RECOVTEMP_RELOC }
{                                      =568%msg%HPFS_TOTAL_DISK_SPACE }
{                                      =569%msg%HPFS_DIR_KBYTES }
{                                      =570%msg%HPFS_FILE_KBYTES }
{                                      =571%msg%HPFS_KBYTES_AVAILABLE }
{                                      =572%msg%HPFS_CHKDSK_PLACE_REC_FILE }
{                                      =573%msg%HPFS_CHKDSK_RECO_DIR_AS }
{                                      =574%msg%HPFS_CHKDSK_PLACEED_DATA }
{                                      =575%msg%HPFS_CHKDSK_RECOV_EA }
{                                      =576%msg%HPFS_CHKDSK_FIND_EA_INTEM }
{                                      =577%msg%HPFS_CHKDSK_RELOC_TEMP_EA }
{                                      =578%msg%HPFS_CHKDSK_RELOC_AC_LIST }
{                                      =579%msg%HPFS_CHKDSK_LIST_NORELOC }
{                                      =580%msg%HPFS_CHKDSK_TRUN_EA_LIST }
{                                      =581%msg%HPFS_CHKDSK_TRUN_EA_NAME }
{                                      =582%msg%HPFS_CHKDSK_TRUN_EA_BBLOCK }
{                                      =583%msg%HPFS_CHKDSK_REM_INVALID_EA }
{                                      =584%msg%HPFS_CHKDSK_FIX_EA_ALLOC }
{                                      =585%msg%HPFS_CHKDSK_FIX_ALACCCTRL }
{                                      =586%msg%HPFS_CHKDSK_ACCTR_LIST_BBL }
{                                      =587%msg%HPFS_CHKDSK_REM_ACLIST }
{                                      =588%msg%HPFS_CHKDSK_FOUND_DATANORL }
{                                      =589%msg%HPFS_WRONG_VERSION }
{                                      =590%msg%HPFS_CHKDSK_FOUND_DATATEMP }
{                                      =591%msg%HPFS_CHKDSK_FIX_TEMPSTATUS }
{                                      =592%msg%HPFS_CHKDSK_FIX_NEEDEADATA }
{                                      =593%msg%HPFS_RECOVER_PARM_ERROR }
{                                      =594%msg%HPFS_RECOV_FILE_NOT_FOUND }
{                                      =595%msg%HPFS_RECOV_UNKNOWN_ERROR }
{                                      =596%msg%HPFS_RECOV_NOT_ENOUGH_MEM }
{                                      =597%msg%HPFS_RECOV_NOWRITE_DATA }
{                                      =598%msg%HPFS_RECOV_NOTEMP_CREATE }
{                                      =599%msg%HPFS_RECOV_EA_NOREAD }
{                                      =600%msg%HPFS_RECOV_FILE_BYTES }
{                                      =601%msg%HPFS_RECOV_BAD_BYTES_RECOV }
{                                      =602%msg%HPFS_RECOV_FILEBYTES_NOREC }
{                                      =603%msg%HPFS_RECOV_DISK_INUSE }
{                                      =604%msg%HPFS_RECOV_FILE_NODELETE }
{                                      =605%msg%HPFS_RECOV_NOCREATE_NEWFILE }
{                                      =606%msg%HPFS_RECOV_SYSTEM_ERROR }
{                                      =607%msg%HPFS_SYS_PARM_ERROR }
{                                      =608%msg%HPFS_SYS_CANNOT_INSTALL }
{                                      =609%msg%HPFS_SYS_DRIVE_NOTFORMATED }
{                                      =610%msg%HPFS_SYS_FILE_NOCREATE }
{                                      =611%msg%HPFS_SIZE_EXCEED }
{                                      =612%msg%HPFS_SYNTAX_ERR }
{                                      =613%msg%HPFS_NOTENOUGH_MEM }
{                                      =614%msg%HPFS_WANT_MEM }
{                                      =615%msg%HPFS_GET_RETURNED }
{                                      =616%msg%HPFS_SET_RETURNED }
{                                      =617%msg%HPFS_BOTH_RETURNED }
{                                      =618%msg%HPFS_STOP_RETURNED }
{                                      =619%msg%HPFS_SETPRTYRETURNED }
{                                      =620%msg%HPFS_ALCSG_RETURNED }
{                                      =621%msg%HPFS_MSEC_SET }
{                                      =622%msg%HPFS_OPTIONS }
{                                      =623%msg%HPFS_POS_NUM_VALUE }
{                                      =624%msg%HPFS_VALUE_TOO_LARGE }
{                                      =625%msg%HPFS_LAZY_NOT_VALID }
{                                      =626%msg%HPFS_VOLUME_ERROR }
{                                      =627%msg%HPFS_VOLUME_DIRTY }
{                                      =628%msg%HPFS_NEW_SECTOR }
{                                      =629%msg%HPFS_FORMAT_PARM_ERROR }
{                                      =630%msg%HPFS_CANNOT_ACCESS_CONFIG }
{                                      =631%msg%HPFS_RECOV_FILE }
{                                      =632%msg%HPFS_CHKDSK_KBYTES_RESERVE }
{                                      =633%msg%HPFS_CHKDSK_KBYTES_IN_EA }
{                                      =634%msg%HPFS_BYTEBUF_SET }
{                                      =635%msg%HPFS_FORMATTING_COMPLETE }
{                                      =636%msg%HPFS_WRONG_VOLUME_LABEL }
{                                      =637%msg%HPFS_FMAT_TOO_MANY_DRS }
{                                      =638%msg%VDD_UNSUPPORTED_ACCESS }
      ERROR_VDD_LOCK_USEAGE_DENIED=639;{ KP.COM not supported in DOS }
      ERROR_TIMEOUT           =640;    { MSG%none }
      ERROR_VDM_DOWN          =641;    { MSG%none }
      ERROR_VDM_LIMIT         =642;    { MSG%none }
      ERROR_VDD_NOT_FOUND     =643;    { MSG%none }
      ERROR_INVALID_CALLER    =644;    { MSG%none }
      ERROR_PID_MISMATCH      =645;    { MSG%none }
      ERROR_INVALID_VDD_HANDLE=646;    { MSG%none }
      ERROR_VLPT_NO_SPOOLER   =647;    { MSG%none }
      ERROR_VCOM_DEVICE_BUSY  =648;    { MSG%none }
      ERROR_VLPT_DEVICE_BUSY  =649;    { MSG%none }
      ERROR_NESTING_TOO_DEEP  =650;    { MSG%none }
      ERROR_VDD_MISSING       =651;    { MSG%VDD_MISSING }

{ INVALID BIDI API PARAMETERS 671;- 684;no msg's required }

      ERROR_BIDI_INVALID_LENGTH       =671;   { MSG%none }
      ERROR_BIDI_INVALID_INCREMENT    =672;   { MSG%none }
      ERROR_BIDI_INVALID_COMBINATION  =673;   { MSG%none }
      ERROR_BIDI_INVALID_RESERVED     =674;   { MSG%none }
      ERROR_BIDI_INVALID_EFFECT       =675;   { MSG%none }
      ERROR_BIDI_INVALID_CSDREC       =676;   { MSG%none }
      ERROR_BIDI_INVALID_CSDSTATE     =677;   { MSG%none }
      ERROR_BIDI_INVALID_LEVEL        =678;   { MSG%none }
      ERROR_BIDI_INVALID_TYPE_SUPPORT =679;   { MSG%none }
      ERROR_BIDI_INVALID_ORIENTATION  =680;   { MSG%none }
      ERROR_BIDI_INVALID_NUM_SHAPE    =681;   { MSG%none }
      ERROR_BIDI_INVALID_CSD          =682;   { MSG%none }
      ERROR_BIDI_NO_SUPPORT           =683;   { MSG%none }
      NO_ERROR_BIDI_RW_INCOMPLETE     =684;   { MSG%none }

{                                             =689%msg%HPFS_LAZY_ON }
{                                             =690%msg%HPFS_LAZY_OFF }
      ERROR_IMP_INVALID_PARM          =691;    { MSG%none }
      ERROR_IMP_INVALID_LENGTH        =692;    { MSG%none }
      MSG_HPFS_DISK_ERROR_WARN        =693;    { MSG%HPFS_DISK_ERROR_WARN }
      ERROR_MON_BAD_BUFFER            =730;    { MSG%BAD_MON_BUFFER }

      ERROR_MODULE_CORRUPTED           =731;    { MSG%MODULE_CORRUPTED }

      ERROR_SM_OUTOF_SWAPFILE          =1477;  { MSG%SM_OUT_OF_SWAFILE }

      ERROR_LF_TIMEOUT                 =2055;  { MSG%LF_TIMEOUT }
      ERROR_LF_SUSPEND_SUCCESS         =2057;  { MSG%LF_SUSP_SUCCESS }
      ERROR_LF_RESUME_SUCCESS          =2058;  { MSG%LF_RESUM_SUCCESS }
      ERROR_LF_REDIRECT_SUCCESS        =2059;  { MSG%LF_REDIR_SUCCESS }
      ERROR_LF_REDIRECT_FAILURE        =2060;  { MSG%LF_REDIR_FAILURE }


      ERROR_SWAPPER_NOT_ACTIVE        =32768;
      ERROR_INVALID_SWAPID            =32769;
      ERROR_IOERR_SWAP_FILE           =32770;
      ERROR_SWAP_TABLE_FULL           =32771;
      ERROR_SWAP_FILE_FULL            =32772;
      ERROR_CANT_INIT_SWAPPER         =32773;
      ERROR_SWAPPER_ALREADY_INIT      =32774;
      ERROR_PMM_INSUFFICIENT_MEMORY   =32775;
      ERROR_PMM_INVALID_FLAGS         =32776;
      ERROR_PMM_INVALID_ADDRESS       =32777;
      ERROR_PMM_LOCK_FAILED           =32778;
      ERROR_PMM_UNLOCK_FAILED         =32779;
      ERROR_PMM_MOVE_INCOMPLETE       =32780;
      ERROR_UCOM_DRIVE_RENAMED        =32781;
      ERROR_UCOM_FILENAME_TRUNCATED   =32782;
      ERROR_UCOM_BUFFER_LENGTH        =32783;
      ERROR_MON_CHAIN_HANDLE          =32784;
      ERROR_MON_NOT_REGISTERED        =32785;
      ERROR_SMG_ALREADY_TOP           =32786;
      ERROR_PMM_ARENA_MODIFIED        =32787;
      ERROR_SMG_PRINTER_OPEN          =32788;
      ERROR_PMM_SET_FLAGS_FAILED      =32789;
      ERROR_INVALID_DOS_DD            =32790;
      ERROR_BLOCKED                   =32791;
      ERROR_NOBLOCK                   =32792;
      ERROR_INSTANCE_SHARED           =32793;
      ERROR_NO_OBJECT                 =32794;
      ERROR_PARTIAL_ATTACH            =32795;
      ERROR_INCACHE                   =32796;
      ERROR_SWAP_IO_PROBLEMS          =32797;
      ERROR_CROSSES_OBJECT_BOUNDARY   =32798;
      ERROR_LONGLOCK                  =32799;
      ERROR_SHORTLOCK                 =32800;
      ERROR_UVIRTLOCK                 =32801;
      ERROR_ALIASLOCK                 =32802;
      ERROR_ALIAS                     =32803;
      ERROR_NO_MORE_HANDLES           =32804;
      ERROR_SCAN_TERMINATED           =32805;
      ERROR_TERMINATOR_NOT_FOUND      =32806;
      ERROR_NOT_DIRECT_CHILD          =32807;
      ERROR_DELAY_FREE                =32808;
      ERROR_GUARDPAGE                 =32809;
      ERROR_SWAPERROR                 =32900;
      ERROR_LDRERROR                  =32901;
      ERROR_NOMEMORY                  =32902;
      ERROR_NOACCESS                  =32903;
      ERROR_NO_DLL_TERM               =32904;
      ERROR_CPSIO_CODE_PAGE_INVALID   =65026;
      ERROR_CPSIO_NO_SPOOLER          =65027;
      ERROR_CPSIO_FONT_ID_INVALID     =65028;
      ERROR_CPSIO_INTERNAL_ERROR      =65033;
      ERROR_CPSIO_INVALID_PTR_NAME    =65034;
      ERROR_CPSIO_NOT_ACTIVE          =65037;
      ERROR_CPSIO_PID_FULL            =65039;
      ERROR_CPSIO_PID_NOT_FOUND       =65040;
      ERROR_CPSIO_READ_CTL_SEQ        =65043;
      ERROR_CPSIO_READ_FNT_DEF        =65045;
      ERROR_CPSIO_WRITE_ERROR         =65047;
      ERROR_CPSIO_WRITE_FULL_ERROR    =65048;
      ERROR_CPSIO_WRITE_HANDLE_BAD    =65049;
      ERROR_CPSIO_SWIT_LOAD           =65074;
      ERROR_CPSIO_INV_COMMAND         =65077;
      ERROR_CPSIO_NO_FONT_SWIT        =65078;
      ERROR_ENTRY_IS_CALLGATE         =65079;

{ Constants from bsememf.h header file (memory management) }

    const
       PAG_READ         = $00000001;
       PAG_WRITE        = $00000002;
       PAG_EXECUTE      = $00000004;
       PAG_GUARD        = $00000008;
       PAG_COMMIT       = $00000010;
       PAG_DECOMMIT     = $00000020;
       OBJ_TILE         = $00000040;
       OBJ_PROTECTED    = $00000080;
       OBJ_GETTABLE     = $00000100;
       OBJ_GIVEABLE     = $00000200;
       PAG_DEFAULT      = $00000400;
       PAG_SHARED       = $00002000;
       PAG_FREE         = $00004000;
       PAG_BASE         = $00010000;

       fPERM = (PAG_EXECUTE or PAG_READ or PAG_WRITE);
       fSHARE = (OBJ_GETTABLE or OBJ_GIVEABLE);
       fALLOC = (OBJ_TILE or PAG_COMMIT or fPERM);
       fALLOCSHR = (OBJ_TILE or PAG_COMMIT or fSHARE or fPERM);
       fGETNMSHR = (fPERM);
       fGETSHR = (fPERM);
       fGIVESHR = (fPERM);
       fSET = (PAG_COMMIT+PAG_DECOMMIT+PAG_DEFAULT+fPERM);

       DOSSUB_INIT          = $01;
       DOSSUB_GROW          = $02;
       DOSSUB_SPARSE_OBJ    = $04;
       DOSSUB_SERIALIZE     = $08;


  implementation

Function LOUSHORT(var l): Word;
Begin
  LOUSHORT:=Lo(Cardinal(l));
End;

end.
