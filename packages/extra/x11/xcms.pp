unit xcms;
interface
uses
  x,xlib;

{$LinkLib c}
{$LinkLib X11}

{
  Automatically converted by H2Pas 0.99.15 from xcms.h
  The following command line parameters were used:
    -p
    -T
    -S
    -d
    -c
    xcms.h
}


{$PACKRECORDS C}


const
   XcmsFailure = 0;
   XcmsSuccess = 1;
   XcmsSuccessWithCompression = 2;
type
   PXcmsColorFormat = ^TXcmsColorFormat;
   TXcmsColorFormat = dword;
function XcmsUndefinedFormat : TXcmsColorFormat;

function XcmsCIEXYZFormat : TXcmsColorFormat;

function XcmsCIEuvYFormat : TXcmsColorFormat;

function XcmsCIExyYFormat : TXcmsColorFormat;

function XcmsCIELabFormat : TXcmsColorFormat;

function XcmsCIELuvFormat : TXcmsColorFormat;

function XcmsTekHVCFormat : TXcmsColorFormat;

function XcmsRGBFormat : TXcmsColorFormat;

function XcmsRGBiFormat : TXcmsColorFormat;


const
   XcmsInitNone = $00;
   XcmsInitSuccess = $01;
   XcmsInitFailure = $ff;
{$ifdef MACROS}
function DisplayOfCCC(ccc : longint) : longint;

function ScreenNumberOfCCC(ccc : longint) : longint;

function VisualOfCCC(ccc : longint) : longint;

function ClientWhitePointOfCCC(ccc : longint) : longint;

function ScreenWhitePointOfCCC(ccc : longint) : longint;

function FunctionSetOfCCC(ccc : longint) : longint;
{$endif MACROS}

type

   PXcmsFloat = ^TXcmsFloat;
   TXcmsFloat = double;

   PXcmsRGB = ^TXcmsRGB;
   TXcmsRGB = record
        red : word;
        green : word;
        blue : word;
     end;

   PXcmsRGBi = ^TXcmsRGBi;
   TXcmsRGBi = record
        red : TXcmsFloat;
        green : TXcmsFloat;
        blue : TXcmsFloat;
     end;

   PXcmsCIEXYZ = ^TXcmsCIEXYZ;
   TXcmsCIEXYZ = record
        X : TXcmsFloat;
        Y : TXcmsFloat;
        Z : TXcmsFloat;
     end;

   PXcmsCIEuvY = ^TXcmsCIEuvY;
   TXcmsCIEuvY = record
        u_prime : TXcmsFloat;
        v_prime : TXcmsFloat;
        Y : TXcmsFloat;
     end;

   PXcmsCIExyY = ^TXcmsCIExyY;
   TXcmsCIExyY = record
        x : TXcmsFloat;
        y : TXcmsFloat;
        _Y : TXcmsFloat;
     end;

   PXcmsCIELab = ^TXcmsCIELab;
   TXcmsCIELab = record
        L_star : TXcmsFloat;
        a_star : TXcmsFloat;
        b_star : TXcmsFloat;
     end;

   PXcmsCIELuv = ^TXcmsCIELuv;
   TXcmsCIELuv = record
        L_star : TXcmsFloat;
        u_star : TXcmsFloat;
        v_star : TXcmsFloat;
     end;

   PXcmsTekHVC = ^TXcmsTekHVC;
   TXcmsTekHVC = record
        H : TXcmsFloat;
        V : TXcmsFloat;
        C : TXcmsFloat;
     end;

   PXcmsPad = ^TXcmsPad;
   TXcmsPad = record
        pad0 : TXcmsFloat;
        pad1 : TXcmsFloat;
        pad2 : TXcmsFloat;
        pad3 : TXcmsFloat;
     end;

   PXcmsColor = ^TXcmsColor;
   TXcmsColor = record
        spec : record
            case longint of
               0 : ( RGB : TXcmsRGB );
               1 : ( RGBi : TXcmsRGBi );
               2 : ( CIEXYZ : TXcmsCIEXYZ );
               3 : ( CIEuvY : TXcmsCIEuvY );
               4 : ( CIExyY : TXcmsCIExyY );
               5 : ( CIELab : TXcmsCIELab );
               6 : ( CIELuv : TXcmsCIELuv );
               7 : ( TekHVC : TXcmsTekHVC );
               8 : ( Pad : TXcmsPad );
            end;
        pixel : dword;
        format : TXcmsColorFormat;
     end;

   PXcmsPerScrnInfo = ^TXcmsPerScrnInfo;
   TXcmsPerScrnInfo = record
        screenWhitePt : TXcmsColor;
        functionSet : TXPointer;
        screenData : TXPointer;
        state : byte;
        pad : array[0..2] of char;
     end;

   PXcmsCCC = ^TXcmsCCC;

   TXcmsCompressionProc = function (para1:PXcmsCCC; para2:PXcmsColor; para3:dword; para4:dword; para5:PBool):TStatus;cdecl;

   TXcmsWhiteAdjustProc = function (para1:PXcmsCCC; para2:PXcmsColor; para3:PXcmsColor; para4:TXcmsColorFormat; para5:PXcmsColor;
                para6:dword; para7:PBool):TStatus;cdecl;

   TXcmsCCC = record
        dpy : PDisplay;
        screenNumber : longint;
        visual : PVisual;
        clientWhitePt : TXcmsColor;
        gamutCompProc : TXcmsCompressionProc;
        gamutCompClientData : TXPointer;
        whitePtAdjProc : TXcmsWhiteAdjustProc;
        whitePtAdjClientData : TXPointer;
        pPerScrnInfo : PXcmsPerScrnInfo;
     end;
   TXcmsCCCRec = TXcmsCCC;
   PXcmsCCCRec = ^TXcmsCCCRec;

   TXcmsScreenInitProc = function (para1:PDisplay; para2:longint; para3:PXcmsPerScrnInfo):TStatus;cdecl;

   TXcmsScreenFreeProc = procedure (para1:TXPointer);cdecl;

   TXcmsConversionProc = procedure;cdecl;

       PXcmsFuncListPtr = ^TXcmsFuncListPtr;
       TXcmsFuncListPtr = TXcmsConversionProc;

       TXcmsParseStringProc = function (para1:Pchar; para2:PXcmsColor):longint;cdecl;

       PXcmsColorSpace = ^TXcmsColorSpace;
       TXcmsColorSpace = record
            prefix : Pchar;
            id : TXcmsColorFormat;
            parseString : TXcmsParseStringProc;
            to_CIEXYZ : TXcmsFuncListPtr;
            from_CIEXYZ : TXcmsFuncListPtr;
            inverse_flag : longint;
         end;

       PXcmsFunctionSet = ^TXcmsFunctionSet;
       TXcmsFunctionSet = record
            DDColorSpaces : ^PXcmsColorSpace;
            screenInitProc : TXcmsScreenInitProc;
            screenFreeProc : TXcmsScreenFreeProc;
         end;
(* error
extern Status XcmsAddColorSpace (
in declaration at line 323 *)

function XcmsAddFunctionSet(para1:PXcmsFunctionSet):TStatus;cdecl;external;
function XcmsAllocColor(para1:PDisplay; para2:TColormap; para3:PXcmsColor; para4:TXcmsColorFormat):TStatus;cdecl;external;
function XcmsAllocNamedColor(para1:PDisplay; para2:TColormap; para3:Pchar; para4:PXcmsColor; para5:PXcmsColor;
               para6:TXcmsColorFormat):TStatus;cdecl;external;
function XcmsCCCOfColormap(para1:PDisplay; para2:TColormap):TXcmsCCC;cdecl;external;
function XcmsCIELabClipab(para1:TXcmsCCC; para2:PXcmsColor; para3:dword; para4:dword; para5:PBool):TStatus;cdecl;external;
function XcmsCIELabClipL(para1:TXcmsCCC; para2:PXcmsColor; para3:dword; para4:dword; para5:PBool):TStatus;cdecl;external;
function XcmsCIELabClipLab(para1:TXcmsCCC; para2:PXcmsColor; para3:dword; para4:dword; para5:PBool):TStatus;cdecl;external;
function XcmsCIELabQueryMaxC(para1:TXcmsCCC; para2:TXcmsFloat; para3:TXcmsFloat; para4:PXcmsColor):TStatus;cdecl;external;
function XcmsCIELabQueryMaxL(para1:TXcmsCCC; para2:TXcmsFloat; para3:TXcmsFloat; para4:PXcmsColor):TStatus;cdecl;external;
function XcmsCIELabQueryMaxLC(para1:TXcmsCCC; para2:TXcmsFloat; para3:PXcmsColor):TStatus;cdecl;external;
function XcmsCIELabQueryMinL(para1:TXcmsCCC; para2:TXcmsFloat; para3:TXcmsFloat; para4:PXcmsColor):TStatus;cdecl;external;
function XcmsCIELabToCIEXYZ(para1:TXcmsCCC; para2:PXcmsColor; para3:PXcmsColor; para4:dword):TStatus;cdecl;external;
function XcmsCIELabWhiteShiftColors(para1:TXcmsCCC; para2:PXcmsColor; para3:PXcmsColor; para4:TXcmsColorFormat; para5:PXcmsColor;
               para6:dword; para7:PBool):TStatus;cdecl;external;
function XcmsCIELuvClipL(para1:TXcmsCCC; para2:PXcmsColor; para3:dword; para4:dword; para5:PBool):TStatus;cdecl;external;
function XcmsCIELuvClipLuv(para1:TXcmsCCC; para2:PXcmsColor; para3:dword; para4:dword; para5:PBool):TStatus;cdecl;external;
function XcmsCIELuvClipuv(para1:TXcmsCCC; para2:PXcmsColor; para3:dword; para4:dword; para5:PBool):TStatus;cdecl;external;
function XcmsCIELuvQueryMaxC(para1:TXcmsCCC; para2:TXcmsFloat; para3:TXcmsFloat; para4:PXcmsColor):TStatus;cdecl;external;
function XcmsCIELuvQueryMaxL(para1:TXcmsCCC; para2:TXcmsFloat; para3:TXcmsFloat; para4:PXcmsColor):TStatus;cdecl;external;
function XcmsCIELuvQueryMaxLC(para1:TXcmsCCC; para2:TXcmsFloat; para3:PXcmsColor):TStatus;cdecl;external;
function XcmsCIELuvQueryMinL(para1:TXcmsCCC; para2:TXcmsFloat; para3:TXcmsFloat; para4:PXcmsColor):TStatus;cdecl;external;
function XcmsCIELuvToCIEuvY(para1:TXcmsCCC; para2:PXcmsColor; para3:PXcmsColor; para4:dword):TStatus;cdecl;external;
function XcmsCIELuvWhiteShiftColors(para1:TXcmsCCC; para2:PXcmsColor; para3:PXcmsColor; para4:TXcmsColorFormat; para5:PXcmsColor;
               para6:dword; para7:PBool):TStatus;cdecl;external;
function XcmsCIEXYZToCIELab(para1:TXcmsCCC; para2:PXcmsColor; para3:PXcmsColor; para4:dword):TStatus;cdecl;external;
function XcmsCIEXYZToCIEuvY(para1:TXcmsCCC; para2:PXcmsColor; para3:PXcmsColor; para4:dword):TStatus;cdecl;external;
function XcmsCIEXYZToCIExyY(para1:TXcmsCCC; para2:PXcmsColor; para3:PXcmsColor; para4:dword):TStatus;cdecl;external;
function XcmsCIEXYZToRGBi(para1:TXcmsCCC; para2:PXcmsColor; para3:dword; para4:PBool):TStatus;cdecl;external;
function XcmsCIEuvYToCIELuv(para1:TXcmsCCC; para2:PXcmsColor; para3:PXcmsColor; para4:dword):TStatus;cdecl;external;
function XcmsCIEuvYToCIEXYZ(para1:TXcmsCCC; para2:PXcmsColor; para3:PXcmsColor; para4:dword):TStatus;cdecl;external;
function XcmsCIEuvYToTekHVC(para1:TXcmsCCC; para2:PXcmsColor; para3:PXcmsColor; para4:dword):TStatus;cdecl;external;
function XcmsCIExyYToCIEXYZ(para1:TXcmsCCC; para2:PXcmsColor; para3:PXcmsColor; para4:dword):TStatus;cdecl;external;
function XcmsClientWhitePointOfCCC(para1:TXcmsCCC):PXcmsColor;cdecl;external;
function XcmsConvertColors(para1:TXcmsCCC; para2:PXcmsColor; para3:dword; para4:TXcmsColorFormat; para5:PBool):TStatus;cdecl;external;
function XcmsCreateCCC(para1:PDisplay; para2:longint; para3:PVisual; para4:PXcmsColor; para5:TXcmsCompressionProc;
               para6:TXPointer; para7:TXcmsWhiteAdjustProc; para8:TXPointer):TXcmsCCC;cdecl;external;
function XcmsDefaultCCC(para1:PDisplay; para2:longint):TXcmsCCC;cdecl;external;
function XcmsDisplayOfCCC(para1:TXcmsCCC):PDisplay;cdecl;external;
function XcmsFormatOfPrefix(para1:Pchar):TXcmsColorFormat;cdecl;external;
procedure XcmsFreeCCC(para1:TXcmsCCC);cdecl;external;
function XcmsLookupColor(para1:PDisplay; para2:TColormap; para3:Pchar; para4:PXcmsColor; para5:PXcmsColor;
               para6:TXcmsColorFormat):TStatus;cdecl;external;
function XcmsPrefixOfFormat(para1:TXcmsColorFormat):Pchar;cdecl;external;
function XcmsQueryBlack(para1:TXcmsCCC; para2:TXcmsColorFormat; para3:PXcmsColor):TStatus;cdecl;external;
function XcmsQueryBlue(para1:TXcmsCCC; para2:TXcmsColorFormat; para3:PXcmsColor):TStatus;cdecl;external;
function XcmsQueryColor(para1:PDisplay; para2:TColormap; para3:PXcmsColor; para4:TXcmsColorFormat):TStatus;cdecl;external;
function XcmsQueryColors(para1:PDisplay; para2:TColormap; para3:PXcmsColor; para4:dword; para5:TXcmsColorFormat):TStatus;cdecl;external;
function XcmsQueryGreen(para1:TXcmsCCC; para2:TXcmsColorFormat; para3:PXcmsColor):TStatus;cdecl;external;
function XcmsQueryRed(para1:TXcmsCCC; para2:TXcmsColorFormat; para3:PXcmsColor):TStatus;cdecl;external;
function XcmsQueryWhite(para1:TXcmsCCC; para2:TXcmsColorFormat; para3:PXcmsColor):TStatus;cdecl;external;
function XcmsRGBiToCIEXYZ(para1:TXcmsCCC; para2:PXcmsColor; para3:dword; para4:PBool):TStatus;cdecl;external;
function XcmsRGBiToRGB(para1:TXcmsCCC; para2:PXcmsColor; para3:dword; para4:PBool):TStatus;cdecl;external;
function XcmsRGBToRGBi(para1:TXcmsCCC; para2:PXcmsColor; para3:dword; para4:PBool):TStatus;cdecl;external;
function XcmsScreenNumberOfCCC(para1:TXcmsCCC):longint;cdecl;external;
function XcmsScreenWhitePointOfCCC(para1:TXcmsCCC):PXcmsColor;cdecl;external;
function XcmsSetCCCOfColormap(para1:PDisplay; para2:TColormap; para3:TXcmsCCC):TXcmsCCC;cdecl;external;
function XcmsSetCompressionProc(para1:TXcmsCCC; para2:TXcmsCompressionProc; para3:TXPointer):TXcmsCompressionProc;cdecl;external;
function XcmsSetWhiteAdjustProc(para1:TXcmsCCC; para2:TXcmsWhiteAdjustProc; para3:TXPointer):TXcmsWhiteAdjustProc;cdecl;external;
function XcmsSetWhitePoint(para1:TXcmsCCC; para2:PXcmsColor):TStatus;cdecl;external;
function XcmsStoreColor(para1:PDisplay; para2:TColormap; para3:PXcmsColor):TStatus;cdecl;external;
function XcmsStoreColors(para1:PDisplay; para2:TColormap; para3:PXcmsColor; para4:dword; para5:PBool):TStatus;cdecl;external;
function XcmsTekHVCClipC(para1:TXcmsCCC; para2:PXcmsColor; para3:dword; para4:dword; para5:PBool):TStatus;cdecl;external;
function XcmsTekHVCClipV(para1:TXcmsCCC; para2:PXcmsColor; para3:dword; para4:dword; para5:PBool):TStatus;cdecl;external;
function XcmsTekHVCClipVC(para1:TXcmsCCC; para2:PXcmsColor; para3:dword; para4:dword; para5:PBool):TStatus;cdecl;external;
function XcmsTekHVCQueryMaxC(para1:TXcmsCCC; para2:TXcmsFloat; para3:TXcmsFloat; para4:PXcmsColor):TStatus;cdecl;external;
function XcmsTekHVCQueryMaxV(para1:TXcmsCCC; para2:TXcmsFloat; para3:TXcmsFloat; para4:PXcmsColor):TStatus;cdecl;external;
function XcmsTekHVCQueryMaxVC(para1:TXcmsCCC; para2:TXcmsFloat; para3:PXcmsColor):TStatus;cdecl;external;
function XcmsTekHVCQueryMaxVSamples(para1:TXcmsCCC; para2:TXcmsFloat; para3:PXcmsColor; para4:dword):TStatus;cdecl;external;
function XcmsTekHVCQueryMinV(para1:TXcmsCCC; para2:TXcmsFloat; para3:TXcmsFloat; para4:PXcmsColor):TStatus;cdecl;external;
function XcmsTekHVCToCIEuvY(para1:TXcmsCCC; para2:PXcmsColor; para3:PXcmsColor; para4:dword):TStatus;cdecl;external;
function XcmsTekHVCWhiteShiftColors(para1:TXcmsCCC; para2:PXcmsColor; para3:PXcmsColor; para4:TXcmsColorFormat; para5:PXcmsColor;
               para6:dword; para7:PBool):TStatus;cdecl;external;
function XcmsVisualOfCCC(para1:TXcmsCCC):PVisual;cdecl;external;


implementation

function XcmsUndefinedFormat : TXcmsColorFormat;
  begin
     XcmsUndefinedFormat:=TXcmsColorFormat($00000000);
  end;

function XcmsCIEXYZFormat : TXcmsColorFormat;
  begin
     XcmsCIEXYZFormat:=TXcmsColorFormat($00000001);
  end;

function XcmsCIEuvYFormat : TXcmsColorFormat;
  begin
     XcmsCIEuvYFormat:=TXcmsColorFormat($00000002);
  end;

function XcmsCIExyYFormat : TXcmsColorFormat;
  begin
     XcmsCIExyYFormat:=TXcmsColorFormat($00000003);
  end;

function XcmsCIELabFormat : TXcmsColorFormat;
  begin
     XcmsCIELabFormat:=TXcmsColorFormat($00000004);
  end;

function XcmsCIELuvFormat : TXcmsColorFormat;
  begin
     XcmsCIELuvFormat:=TXcmsColorFormat($00000005);
  end;

function XcmsTekHVCFormat : TXcmsColorFormat;
  begin
     XcmsTekHVCFormat:=TXcmsColorFormat($00000006);
  end;

function XcmsRGBFormat : TXcmsColorFormat;
  begin
     XcmsRGBFormat:=TXcmsColorFormat($80000000);
  end;

function XcmsRGBiFormat : TXcmsColorFormat;
  begin
     XcmsRGBiFormat:=TXcmsColorFormat($80000001);
  end;

{$ifdef MACROS}
function DisplayOfCCC(ccc : longint) : longint;
begin
   DisplayOfCCC:=ccc^.dpy;
end;

function ScreenNumberOfCCC(ccc : longint) : longint;
begin
   ScreenNumberOfCCC:=ccc^.screenNumber;
end;

function VisualOfCCC(ccc : longint) : longint;
begin
   VisualOfCCC:=ccc^.visual;
end;

function ClientWhitePointOfCCC(ccc : longint) : longint;
begin
   ClientWhitePointOfCCC:=@(ccc^.clientWhitePt);
end;

function ScreenWhitePointOfCCC(ccc : longint) : longint;
begin
   ScreenWhitePointOfCCC:=@(ccc^.(pPerScrnInfo^.screenWhitePt));
end;

function FunctionSetOfCCC(ccc : longint) : longint;
begin
   FunctionSetOfCCC:=ccc^.(pPerScrnInfo^.functionSet);
end;
{$endif MACROS}

end.
