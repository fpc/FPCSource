{****************************************************************************

                   Copyright (c) 1993,94 by Florian Kl„mpfl
                  
 ****************************************************************************}
unit os2def;

  interface
  
    type
       APIRET = longint;
       APIRET16 = word;
       APIRET32 = longint;
       
       SHANDLE = word;
       LHANDLE = longint;
       
       CHAR = char;
       SHORT = integer;
       LONG = longint;
       INT = longint;
       UCHAR = char;
       USHORT = word;
       ULONG = longint;
       UINT = longint;
       
{!!!!!!! typedef UCHAR     * _Seg16 PUCHAR16;
typedef CHAR      * _Seg16 PCHAR16; }

       BYTE = byte;
       PSZ = ^char;
       NPSZ = ^char;
       PCH = ^char;
       NPCH = ^char;
{       typedef int ( APIENTRY _PFN)  ();
typedef _PFN    *PFN;
typedef int ( APIENTRY _NPFN)  ();
typedef _NPFN   *NPFN;	}

       PBYTE = ^BYTE;
       NPBYTE = ^BYTE;
       PCHAR = ^CHAR;
       PSHORT = ^SHORT;
       PLONG = ^LONG;
       PINT = ^INT;
       PUCHAR = ^UCHAR;
       PUSHORT = ^USHORT;
       PULONG = ^ULONG;
       PUINT = ^UINT;
      
       PVOID = pointer;
       PPVOID = ^PVOID;

      { typedef VOID   * _Seg16  PVOID16; }

       BOOL = longint;
       PBOOL = ^BOOL;
     
       BOOL16 = word;
{ typedef BOOL16     * _Seg16 PBOOL16;  }

       BOOL32 = BOOL;
       PBOOL32 = PBOOL;
      
       QWORD = record
          ulLo : ULONG;
          ulHi : ULONG;
       end;
      
       PQWORD = ^QWORD;
      
       SEL = WORD;
       PSEL = ^SEL;
      
       ERRORID = ULONG;
       PERRORID = ^ERRORID;
      
    const
       SEVERITY_NOERROR = $0;
       SEVERITY_WARNING = $4;
       SEVERITY_ERROR = $8;
       SEVERITY_SEVERE = $c;
       SEVERITY_UNRECOVERABLE = $10;
      
       WINERR_BASE = $1000;
       GPIERR_BASE = $2000;
       DEVERR_BASE = $3000;
       SPLERR_BASE = $4000;
      
    type
       HMODULE = LHANDLE;
       PID = LHANDLE;
       TID = LHANDLE;
      
       SGID = USHORT;
      
       PHMODULE = ^HMODULE;
       PPID = ^PID;
       PTID = ^TID;
      
       HSEM = pointer;
       PHSEM = ^HSEM;
      
       HAB = LHANDLE;
       PHAB = ^HAB;
      
       HPS = LHANDLE;
       PHPS = ^HPS;
      
       HDC = LHANDLE;
       PHDC = ^HDC;
      
       HRGN = LHANDLE;
       PHRGN = ^HRGN;
      
       HBITMAP = LHANDLE;
       PHBITMAP = ^HBITMAP;
      
       HMF = LHANDLE;
       PHMF = ^HMF;
      
       HPAL = LHANDLE;
       PHPAL = HPAL;
      
       COLOR = LONG;
       PCOLOR = ^COLOR;
      
       POINTL = record
          x : LONG;
          y : LONG;
       end;
      
       PPOINTL = ^POINTL;
       NPPOINTL = ^POINTL;
      
       POINTS = record
          x : SHORT;
          y : SHORT;
       end;
      
       PPOINTS = ^POINTS;
      
       RECTL = record
          xLeft : LONG;
          yBottom : LONG;
          xRight : LONG;
          yTop : LONG;
       end;

       PRECTL = ^RECTL;

       NPRECTL = ^RECTL;
       
       STR8 = array[0..7] of CHAR;

       PSTR8 = ^STR8;

       DRIVDATA = record
          cb : LONG;
          lVersion : LONG;
          szDeviceName : array[0..32-1] of CHAR;
          abGeneralData : array[0..1-1] of CHAR;
       end;

       PDRIVDATA = ^DRIVDATA;

       PDEVOPENDATA = ^PSZ;

    const
       ADDRESS = 0;
       DRIVER_NAME = 1;
       DRIVER_DATA = 2;
       DATA_TYPE = 3;
       COMMENT = 4;
       PROC_NAME = 5;
       PROC_PARAMS = 6;
       SPL_PARAMS = 7;
       NETWORK_PARAMS = 8;

    type
       DEVOPENSTRUC = record
          pszLogAddress : PSZ;
          pszDriverName : PSZ;
          pdriv : PDRIVDATA;
          pszDataType : PSZ;
          pszComment : PSZ;
          pszQueueProcName : PSZ;
          pszQueueProcParams : PSZ;
          pszSpoolerParams : PSZ;
          pszNetworkParams : PSZ;
       end;

       PDEVOPENSTRUC = ^DEVOPENSTRUC;

       PRINTDEST = record
          cb : ULONG;
          lType : LONG;
          pszToken : PSZ;
          lCount : LONG;
          pdopData : PDEVOPENDATA;
          fl : ULONG;
          pszPrinter : PSZ;
       end;

       PPRINTDEST = ^PRINTDEST;

    const
       PD_JOB_PROPERTY = $0001;
       FATTR_SEL_ITALIC = $0001;
       FATTR_SEL_UNDERSCORE = $0002;
       FATTR_SEL_OUTLINE = $0008;
       FATTR_SEL_STRIKEOUT = $0010;
       FATTR_SEL_BOLD = $0020;
       FATTR_TYPE_KERNING = $0004;
       FATTR_TYPE_MBCS = $0008;
       FATTR_TYPE_DBCS = $0010;
       FATTR_TYPE_ANTIALIASED = $0020;
       FATTR_FONTUSE_NOMIX = $0002;
       FATTR_FONTUSE_OUTLINE = $0004;
       FATTR_FONTUSE_TRANSFORMABLE = $0008;
       FACESIZE = 32;

    type
       FATTRS = record
          usRecordLength : USHORT;
          fsSelection : USHORT;
          lMatch : LONG;
          szFacename : array[0..FACESIZE-1] of CHAR;
          idRegistry : USHORT;
          usCodePage : USHORT;
          lMaxBaselineExt : LONG;
          lAveCharWidth : LONG;
          fsType : USHORT;
          fsFontUse : USHORT;
       end;

       PFATTRS = ^FATTRS;

    const
       FM_TYPE_FIXED = $0001;
       FM_TYPE_LICENSED = $0002;
       FM_TYPE_KERNING = $0004;
       FM_TYPE_DBCS = $0010;
       FM_TYPE_MBCS = $0018;
       FM_TYPE_64K = $8000;
       FM_TYPE_ATOMS = $4000;
       FM_TYPE_FAMTRUNC = $2000;
       FM_TYPE_FACETRUNC = $1000;
       FM_DEFN_OUTLINE = $0001;
       FM_DEFN_IFI = $0002;
       FM_DEFN_WIN = $0004;
       FM_DEFN_GENERIC = $8000;
       FM_SEL_ITALIC = $0001;
       FM_SEL_UNDERSCORE = $0002;
       FM_SEL_NEGATIVE = $0004;
       FM_SEL_OUTLINE = $0008;
       FM_SEL_STRIKEOUT = $0010;
       FM_SEL_BOLD = $0020;
       FM_CAP_NOMIX = $0001;

    type
       PANOSE = record
          bFamilyType : BYTE;
          bSerifStyle : BYTE;
          bWeight : BYTE;
          bProportion : BYTE;
          bContrast : BYTE;
          bStrokeVariation : BYTE;
          bArmStyle : BYTE;
          bLetterform : BYTE;
          bMidline : BYTE;
          bXHeight : BYTE;
          abReserved : array[0..2-1] of BYTE;
       end;

       FONTMETRICS = record
          szFamilyname : array[0..FACESIZE-1] of CHAR;
          szFacename : array[0..FACESIZE-1] of CHAR;
          idRegistry : USHORT;
          usCodePage : USHORT;
          lEmHeight : LONG;
          lXHeight : LONG;
          lMaxAscender : LONG;
          lMaxDescender : LONG;
          lLowerCaseAscent : LONG;
          lLowerCaseDescent : LONG;
          lInternalLeading : LONG;
          lExternalLeading : LONG;
          lAveCharWidth : LONG;
          lMaxCharInc : LONG;
          lEmInc : LONG;
          lMaxBaselineExt : LONG;
          sCharSlope : SHORT;
          sInlineDir : SHORT;
          sCharRot : SHORT;
          usWeightClass : USHORT;
          usWidthClass : USHORT;
          sXDeviceRes : SHORT;
          sYDeviceRes : SHORT;
          sFirstChar : SHORT;
          sLastChar : SHORT;
          sDefaultChar : SHORT;
          sBreakChar : SHORT;
          sNominalPointSize : SHORT;
          sMinimumPointSize : SHORT;
          sMaximumPointSize : SHORT;
          fsType : USHORT;
          fsDefn : USHORT;
          fsSelection : USHORT;
          fsCapabilities : USHORT;
          lSubscriptXSize : LONG;
          lSubscriptYSize : LONG;
          lSubscriptXOffset : LONG;
          lSubscriptYOffset : LONG;
          lSuperscriptXSize : LONG;
          lSuperscriptYSize : LONG;
          lSuperscriptXOffset : LONG;
          lSuperscriptYOffset : LONG;
          lUnderscoreSize : LONG;
          lUnderscorePosition : LONG;
          lStrikeoutSize : LONG;
          lStrikeoutPosition : LONG;
          sKerningPairs : SHORT;
          sFamilyClass : SHORT;
          lMatch : LONG;
          FamilyNameAtom : LONG;
          FaceNameAtom : LONG;
          panose : PANOSE;
       end;

       PFONTMETRICS = ^FONTMETRICS;
       
       HWND = LHANDLE;
       HMQ = LHANDLE;
       PHWND = ^HWND;
       PHMQ = ^LHANDLE;
       
       WRECTL = RECTL;
       PWRECT = PRECTL;
       NPWRECT = NPRECTL;
       WPOINT = POINTL;
       PWPOINT = PPOINTL;
       NPWPOINT = NPPOINTL;
       
  { Nun folgen einige FPKPascal-spezifische Typen: }
  
      { null. term. Strings sind in den Header Dateien oft als }
      { array[0..0] of byte deklariert, der folgende Typ er-   }
      { m”glich eine Typkonvertierung			       }
      CHARARRAY = array[0..0] of char;
     
  implementation
  
end.
