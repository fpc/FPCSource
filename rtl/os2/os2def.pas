{****************************************************************************

    $Id$

              Copyright (c) 1999-2000 by Florian Klaempfl
                  Copyright (c) 1999-2000 by Ramon Bosque

 The Free Pascal runtime library is distributed under the Library GNU Public
 License v2. So is this unit. The Library GNU Public License requires you to
 distribute the source code of this unit with any product that uses it.
 Because the EMX library isn't under the LGPL, we grant you an exception to
 this, and that is, when you compile a program with the Free Pascal compiler,
 you do not need to ship source code with that program, AS LONG AS YOU ARE
 USING UNMODIFIED CODE! If you modify this code, you MUST change the next
 line:

 <This an official, unmodified Free Pascal source code file.>

 Send us your modified files, we can work together if you want!

 Free Pascal is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 Library GNU General Public License for more details.

 You should have received a copy of the Library GNU General Public License
 along with Free Pascal; see the file COPYING.LIB.  If not, write to
 the Free Software Foundation, 59 Temple Place - Suite 330,
 Boston, MA 02111-1307, USA.

 ****************************************************************************}
unit os2def;

{Warning: This code is alfa. Future versions of this unit will propably
 not be compatible.}

interface

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

       ADDRESS = 0;
       DRIVER_NAME = 1;
       DRIVER_DATA = 2;
       DATA_TYPE = 3;
       COMMENT = 4;
       PROC_NAME = 5;
       PROC_PARAMS = 6;
       SPL_PARAMS = 7;
       NETWORK_PARAMS = 8;

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
       pcardinal = ^cardinal;
       plongint = ^longint;
       pinteger = ^integer;
       pshortint = ^shortint;
       ppointer = ^pointer;
       pbyte = ^byte;
       
       POINTL = record
	  x : cardinal;
	  y : cardinal;
       end;

       PPOINTL = ^POINTL;

       POINTS = record
	  x : integer;
	  y : integer;
       end;
      
       PPOINTS = ^POINTS;
      
       RECTL = record
	  xLeft : longint;
	  yBottom : longint;
	  xRight : longint;
	  yTop : longint;
       end;

       PRECTL = ^RECTL;

       NPRECTL = ^RECTL;
       
       STR8 = array[0..7] of CHAR;

       PSTR8 = ^STR8;

       DRIVDATA = record
	  cb : longint;
	  lVersion : longint;
	  szDeviceName : array[0..32-1] of CHAR;
	  abGeneralData : array[0..1-1] of CHAR;
       end;

       PDRIVDATA = ^DRIVDATA;

       DEVOPENSTRUC = record
	  pszLogAddress : pchar;
	  pszDriverName : pchar;
	  pdriv : PDRIVDATA;
	  pszDataType : pchar;
	  pszComment : pchar;
	  pszQueueProcName : pchar;
	  pszQueueProcParams : pchar;
	  pszSpoolerParams : pchar;
	  pszNetworkParams : pchar;
       end;

       PDEVOPENSTRUC = ^DEVOPENSTRUC;

       PDEVOPENDATA = PDevOpenStruc;

       PRINTDEST = record
	  cb : cardinal;
	  lType : longint;
	  pszToken : pchar;
	  lCount : longint;
	  pdopData : PDEVOPENDATA;
	  fl : cardinal;
	  pszPrinter : pchar;
       end;

       PPRINTDEST = ^PRINTDEST;
       
       FATTRS = record
	  usRecordLength : word;
	  fsSelection : word;
	  lMatch : longint;
	  szFacename : array[0..FACESIZE-1] of CHAR;
	  idRegistry : word;
	  usCodePage : word;
	  lMaxBaselineExt : longint;
	  lAveCharWidth : longint;
	  fsType : word;
	  fsFontUse : word;
       end;

       PFATTRS = ^FATTRS;
       
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
	  idRegistry : word;
	  usCodePage : word;
	  lEmHeight : longint;
	  lXHeight : longint;
	  lMaxAscender : longint;
	  lMaxDescender : longint;
	  lLowerCaseAscent : longint;
	  lLowerCaseDescent : longint;
	  lInternalLeading : longint;
	  lExternalLeading : longint;
	  lAveCharWidth : longint;
	  lMaxCharInc : longint;
	  lEmInc : longint;
	  lMaxBaselineExt : longint;
	  sCharSlope : integer;
	  sInlineDir : integer;
	  sCharRot : integer;
	  usWeightClass : word;
	  usWidthClass : word;
	  sXDeviceRes : integer;
	  sYDeviceRes : integer;
	  sFirstChar : integer;
	  sLastChar : integer;
	  sDefaultChar : integer;
	  sBreakChar : integer;
	  sNominalPointSize : integer;
	  sMinimumPointSize : integer;
	  sMaximumPointSize : integer;
	  fsType : word;
	  fsDefn : word;
	  fsSelection : word;
	  fsCapabilities : word;
	  lSubscriptXSize : longint;
	  lSubscriptYSize : longint;
	  lSubscriptXOffset : longint;
	  lSubscriptYOffset : longint;
	  lSuperscriptXSize : longint;
	  lSuperscriptYSize : longint;
	  lSuperscriptXOffset : longint;
	  lSuperscriptYOffset : longint;
	  lUnderscoreSize : longint;
	  lUnderscorePosition : longint;
	  lStrikeoutSize : longint;
	  lStrikeoutPosition : longint;
	  sKerningPairs : integer;
	  sFamilyClass : integer;
	  lMatch : longint;
	  FamilyNameAtom : longint;
	  FaceNameAtom : longint;
	  panose : PANOSE;
       end;

       PFONTMETRICS = ^FONTMETRICS;
       
  { Nun folgen einige Free Pascal-spezifische Typen: }
  
      { null. term. Strings sind in den Header Dateien oft als }
      { array[0..0] of byte deklariert, der folgende Typ er-   }
      { m”glich eine Typkonvertierung                          }
      CHARARRAY = array[0..0] of char;
     
{Names beginning with T for compatibility}
	TPOINTL = POINTL;
	TPOINTS = POINTS;
	TRECTL = RECTL;
	TSTR8 = STR8;
	TDRIVDATA = DRIVDATA;
	TDEVOPENSTRUC = DEVOPENSTRUC;
	TPRINTDEST = PRINTDEST;
	TFATTRS = FATTRS;
	TPANOSE = PANOSE;
	TFONTMETRICS = FONTMETRICS;
	TCHARARRAY = CHARARRAY;

{Another bunch of compatibility things}
        HWND = cardinal;
        HAB = cardinal;
        HMQ = cardinal;
        HPS = cardinal;
        HRGN = cardinal;
        ULONG = cardinal;
        MParam = pointer;
        MResult = pointer;

  implementation
  
end.
{
  $Log$
  Revision 1.1  2000-07-13 06:31:06  michael
  + Initial import

  Revision 1.10  2000/01/09 20:48:04  hajny
    * FPK changed to FPC

  Revision 1.9  2000/01/07 16:41:48  daniel
    * copyright 2000

  Revision 1.8  2000/01/07 16:32:32  daniel
    * copyright 2000 added

  Revision 1.7  1999/08/10 14:20:54  hajny
    * compatibility updates

  Revision 1.6  1999/08/05 07:37:27  hajny
    * 'solution' for a compiler problem

}
