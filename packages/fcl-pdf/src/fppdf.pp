{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2014 by Michael Van Canneyt

    This unit generates PDF files, without dependencies on GUI libraries.
    (Based on original ideas from the fpGUI pdf generator by Jean-Marc Levecque
     <jmarc.levecque@jmlesite.web4me.fr>)

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.


    LOCALISATION NOTICE:
       Most of the string constants in this unit should NOT be localised,
       as they are specific constants used in the PDF Specification document.
       If you do localise anything, make sure you know what you are doing.

 **********************************************************************}
unit fpPDF;

{$mode objfpc}{$H+}

{ enable compiler define for extra console debug output }
{.$define gdebug}

interface

uses
  Classes,
  SysUtils,
  StrUtils,
  contnrs,
  fpImage,
  FPReadJPEG, FPReadPNG, FPReadBMP, // these are required for auto image-handler functionality
  zstream,
  fpparsettf,
  fpTTFSubsetter,
  FPFontTextMapping;

Const
  { Some popular predefined colors. Channel format is: RRGGBB }
  clBlack   = $000000;
  clWhite   = $FFFFFF;
  clBlue    = $0000FF;
  clGreen   = $008000;
  clRed     = $FF0000;
  clAqua    = $00FFFF;
  clMagenta = $FF00FF;
  clYellow  = $FFFF00;
  clLtGray  = $C0C0C0;
  clMaroon  = $800000;
  clOlive   = $808000;
  clDkGray  = $808080;
  clTeal    = $008080;
  clNavy    = $000080;
  clPurple  = $800080;
  clLime    = $00FF00;
  clWaterMark = $F0F0F0;

type
  TPDFPaperType = (ptCustom, ptA4, ptA5, ptLetter, ptLegal, ptExecutive, ptComm10, ptMonarch, ptDL, ptC5, ptB5);
  TPDFPaperOrientation = (ppoPortrait,ppoLandscape);
  TPDFPenStyle = (ppsSolid,ppsDash,ppsDot,ppsDashDot,ppsDashDotDot);
  TPDFPageLayout = (lSingle, lTwo, lContinuous);
  TPDFUnitOfMeasure = (uomInches, uomMillimeters, uomCentimeters, uomPixels);

  TPDFOption = (poOutLine, poCompressText, poCompressFonts, poCompressImages, poUseRawJPEG, poNoEmbeddedFonts, poPageOriginAtTop, poSubsetFont);
  TPDFOptions = set of TPDFOption;

  EPDF = Class(Exception);

  // forward declarations
  TPDFDocument = class;
  TPDFAnnotList = class;
  TPDFLineStyleDef = class;
  TPDFPage = class;

  TARGBColor = Cardinal;
  TPDFFloat = Single;

  {$IF FPC_FULLVERSION < 30000}
  RawByteString = type AnsiString;
  {$ENDIF}

  TPDFDimensions = record
    T,L,R,B: TPDFFloat;
  end;


  TPDFPaper = record
    H, W: integer;
    Printable: TPDFDimensions;
  end;


  TPDFCoord = record
    X,Y: TPDFFloat;
  end;


  TPDFCoordArray = array of TPDFCoord;

  { We use a special 3x3 matrix for transformations of coordinates. As the
    only allowed transformations are translations and scalations, we need a
    matrix with the following content ([x,y] is a variable):
        [0,0]   0   [2,0]
          0   [1,1] [2,1]
          0     0     1
    [0,0]: X scalation
    [2,0]: X translation
    [1,1]: Y scalation
    [2,1]: Y translation
  }
  TPDFMatrix = object
    _00, _20, _11, _21: TPDFFloat;
    function Transform(APoint: TPDFCoord): TPDFCoord; overload;
    function Transform(X, Y: TPDFFloat): TPDFCoord; overload;
    function ReverseTransform(APoint: TPDFCoord): TPDFCoord;
    procedure SetXScalation(const AValue: TPDFFloat);
    procedure SetYScalation(const AValue: TPDFFloat);
    procedure SetXTranslation(const AValue: TPDFFloat);
    procedure SetYTranslation(const AValue: TPDFFloat);
  end;

  // CharWidth array of standard PDF fonts
  TPDFFontWidthArray = array[0..255] of integer;


  TPDFObject = class(TObject)
  Protected
    Class Function FloatStr(F: TPDFFloat) : String;
    procedure Write(const AStream: TStream); virtual;
    Class procedure WriteString(const AValue: RawByteString; AStream: TStream);
  public
    Constructor Create(Const ADocument : TPDFDocument); virtual; overload;
  end;


  TPDFDocumentObject = Class(TPDFObject)
  Private
    FDocument : TPDFDocument;
  Public
    Constructor Create(Const ADocument : TPDFDocument); override; overload;
    Procedure SetWidth(AWidth : TPDFFloat; AStream : TStream);
    Property Document : TPDFDocument Read FDocument ;
  end;


  TPDFBoolean = class(TPDFDocumentObject)
  private
    FValue: Boolean;
  protected
    procedure Write(const AStream: TStream); override;
  public
    constructor Create(Const ADocument : TPDFDocument; const AValue: Boolean);overload;
  end;


  TPDFMoveTo = class(TPDFDocumentObject)
  private
    FPos : TPDFCoord;
  protected
    procedure Write(const AStream: TStream); override;
  public
    class function Command(APos: TPDFCoord): String;
    class function Command(AX,AY: TPDFFloat): String;
    constructor Create(Const ADocument : TPDFDocument; const AX,AY : TPDFFloat);overload;
    constructor Create(Const ADocument : TPDFDocument; const APos : TPDFCoord);overload;
  end;


  TPDFResetPath = class(TPDFDocumentObject)
  protected
    procedure   Write(const AStream: TStream); override;
  public
    class function Command: string;
  end;


  TPDFClosePath = class(TPDFDocumentObject)
  protected
    procedure   Write(const AStream: TStream); override;
  public
    class function Command: string;
  end;


  TPDFStrokePath = class(TPDFDocumentObject)
  protected
    procedure   Write(const AStream: TStream); override;
  public
    class function Command: string;
  end;


  TPDFPushGraphicsStack = class(TPDFDocumentObject)
  protected
    procedure   Write(const AStream: TStream); override;
  public
    class function Command: string;
  end;


  TPDFPopGraphicsStack = class(TPDFDocumentObject)
  protected
    procedure   Write(const AStream: TStream); override;
  public
    class function Command: string;
  end;


  TPDFInteger = class(TPDFDocumentObject)
  private
    FInt: integer;
  protected
    procedure Inc;
    procedure Write(const AStream: TStream); override;
  public
    constructor Create(Const ADocument : TPDFDocument; const AValue: integer);overload;
    property Value: integer read FInt write FInt;
  end;


  TPDFReference = class(TPDFDocumentObject)
  private
    FValue: integer;
  protected
    procedure Write(const AStream: TStream); override;
  public
    constructor Create(Const ADocument : TPDFDocument; Const AValue: integer);overload;
    property Value: integer read FValue write FValue;
  end;


  TPDFName = class(TPDFDocumentObject)
  private
    FName : string;
    FMustEscape: boolean;
    function  ConvertCharsToHex: string;
  protected
    procedure Write(const AStream: TStream); override;
  public
    constructor Create(Const ADocument : TPDFDocument; const AValue: string; const AMustEscape: boolean = True); overload;
    property Name : String read FName;
    property MustScape: boolean read FMustEscape;
  end;


  TPDFAbstractString = class(TPDFDocumentObject)
  protected
    FFontIndex: integer;
    // These symbols must be preceded by a backslash:  "(", ")", "\"
    function InsertEscape(const AValue: string): string;
  public
    property FontIndex: integer read FFontIndex;
  end;


  TPDFString = class(TPDFAbstractString)
  private
    FValue: AnsiString;
  protected
    procedure Write(const AStream: TStream); override;
  public
    constructor Create(Const ADocument : TPDFDocument; const AValue: AnsiString); overload;
    property    Value: AnsiString read FValue;
  end;


  TPDFUTF8String = class(TPDFAbstractString)
  private
    FValue: UTF8String;
    { Remap each character to the equivalant dictionary character code }
    function RemapedText: AnsiString;
  protected
    procedure Write(const AStream: TStream); override;
  public
    constructor Create(Const ADocument : TPDFDocument; const AValue: UTF8String; const AFontIndex: integer); overload;
    property    Value: UTF8String read FValue;
  end;

  { Is useful to populate an array with free-form space separated values. This
    class is similar to TPDFString, except it doesn't wrap the string content with
    '(' and ')' symbols and doesn't escape the content. }
  TPDFFreeFormString = class(TPDFAbstractString)
  private
    FValue: string;
  protected
    procedure   Write(const AStream: TStream); override;
  public
    constructor Create(Const ADocument: TPDFDocument; const AValue: string); overload;
    property    Value: string read FValue;
  end;


  TPDFArray = class(TPDFDocumentObject)
  private
    FArray: TFPObjectList;
  protected
    procedure Write(const AStream: TStream); override;
    procedure AddItem(const AValue: TPDFObject);
    // Add integers in S as TPDFInteger elements to the array
    Procedure AddIntArray(S : String);
    procedure AddFreeFormArrayValues(S: string);
  public
    constructor Create(Const ADocument : TPDFDocument); override;
    destructor Destroy; override;
  end;


  TPDFStream = class(TPDFDocumentObject)
  private
    FItems: TFPObjectList;
  protected
    procedure Write(const AStream: TStream); override;
    procedure AddItem(const AValue: TPDFObject);
  public
    constructor Create(Const ADocument : TPDFDocument; OwnsObjects : Boolean = True); overload;
    destructor Destroy; override;
  end;


  TPDFEmbeddedFont = class(TPDFDocumentObject)
  private
    FTxtFont: integer;
    FTxtSize: string;
    FPage: TPDFPage;
    function    GetPointSize: integer;
  protected
    procedure Write(const AStream: TStream); override;
    class function WriteEmbeddedFont(const ADocument: TPDFDocument; const Src: TMemoryStream; const AStream: TStream): int64;
    class function WriteEmbeddedSubsetFont(const ADocument: TPDFDocument; const AFontNum: integer; const AOutStream: TStream): int64;
  public
    constructor Create(const ADocument: TPDFDocument;const APage: TPDFPage; const AFont: integer; const ASize: string); overload;
    property    FontIndex: integer read FTxtFont;
    property    PointSize: integer read GetPointSize;
    property    Page: TPDFPage read FPage;
  end;


  TPDFBaseText = class(TPDFDocumentObject)
  private
    FX: TPDFFloat;
    FY: TPDFFloat;
    FFont: TPDFEmbeddedFont;
    FDegrees: single;
    FUnderline: boolean;
    FColor: TARGBColor;
    FStrikeThrough: boolean;
  public
    constructor Create(const ADocument: TPDFDocument); override;
    property    X: TPDFFloat read FX write FX;
    property    Y: TPDFFloat read FY write FY;
    property    Font: TPDFEmbeddedFont read FFont write FFont;
    property    Degrees: single read FDegrees write FDegrees;
    property    Underline: boolean read FUnderline write FUnderline;
    property    Color: TARGBColor read FColor write FColor;
    property    StrikeThrough: boolean read FStrikeThrough write FStrikeThrough;
  end;


  TPDFText = class(TPDFBaseText)
  private
    FString: TPDFString;
    function    GetTextWidth: single;
    function    GetTextHeight: single;
  protected
    procedure   Write(const AStream: TStream); override;
  public
    constructor Create(const ADocument: TPDFDocument; const AX, AY: TPDFFloat; const AText: AnsiString; const AFont: TPDFEmbeddedFont; const ADegrees: single; const AUnderline: boolean; const AStrikethrough: boolean); overload;
    destructor  Destroy; override;
    property    Text: TPDFString read FString;
  end;


  TPDFUTF8Text = class(TPDFBaseText)
  private
    FString: TPDFUTF8String;
  protected
    procedure   Write(const AStream: TStream); override;
  public
    constructor Create(const ADocument: TPDFDocument; const AX, AY: TPDFFloat; const AText: UTF8String; const AFont: TPDFEmbeddedFont; const ADegrees: single; const AUnderline: boolean; const AStrikethrough: boolean); overload;
    destructor  Destroy; override;
    property    Text: TPDFUTF8String read FString;
  end;


  TPDFLineSegment = class(TPDFDocumentObject)
  private
    FWidth: TPDFFloat;
    FStroke: boolean;
    P1, p2: TPDFCoord;
  protected
    procedure Write(const AStream: TStream); override;
  public
    Class Function Command(APos : TPDFCoord) : String; overload;
    Class Function Command(x1, y1 : TPDFFloat) : String; overload;
    Class Function Command(APos1, APos2 : TPDFCoord) : String; overload;
    constructor Create(Const ADocument : TPDFDocument; const AWidth, X1,Y1, X2,Y2: TPDFFloat; const AStroke: Boolean = True); overload;
  end;


  TPDFRectangle = class(TPDFDocumentObject)
  private
    FWidth: TPDFFloat;
    FTopLeft: TPDFCoord;
    FDimensions: TPDFCoord;
    FFill: Boolean;
    FStroke: Boolean;
  protected
    procedure Write(const AStream: TStream); override;
  public
    constructor Create(const ADocument: TPDFDocument; const APosX, APosY, AWidth, AHeight, ALineWidth: TPDFFloat; const AFill, AStroke: Boolean);overload;
  end;


  TPDFRoundedRectangle = class(TPDFDocumentObject)
  private
    FWidth: TPDFFloat;
    FBottomLeft: TPDFCoord;
    FDimensions: TPDFCoord;
    FFill: Boolean;
    FStroke: Boolean;
    FRadius: TPDFFloat;
  protected
    procedure   Write(const AStream: TStream); override;
  public
    constructor Create(const ADocument: TPDFDocument; const APosX, APosY, AWidth, AHeight, ARadius, ALineWidth: TPDFFloat; const AFill, AStroke: Boolean);overload;
  end;


  TPDFCurveC = class(TPDFDocumentObject)
  private
    FCtrl1, FCtrl2, FTo: TPDFCoord;
    FWidth: TPDFFloat;
    FStroke: Boolean;
  protected
    procedure Write(const AStream: TStream); override;
  public
    Class Function Command(Const xCtrl1, yCtrl1, xCtrl2, yCtrl2, xTo, yTo: TPDFFloat): String; overload;
    Class Function Command(Const ACtrl1, ACtrl2, ATo3: TPDFCoord): String; overload;
    constructor Create(Const ADocument : TPDFDocument; const xCtrl1, yCtrl1, xCtrl2, yCtrl2, xTo, yTo, AWidth: TPDFFloat; AStroke: Boolean = True);overload;
    constructor Create(Const ADocument : TPDFDocument; const ACtrl1, ACtrl2, ATo3: TPDFCoord; AWidth: TPDFFloat; AStroke: Boolean = True);overload;
  end;


  TPDFCurveV = class(TPDFDocumentObject)
  private
    FP2,FP3: TPDFCoord;
    FWidth: TPDFFloat;
    FStroke : Boolean;
  protected
    procedure Write(const AStream: TStream); override;
  public
    constructor Create(Const ADocument : TPDFDocument; const X2,Y2,X3,Y3,AWidth : TPDFFloat;AStroke: Boolean = True);overload;
    constructor Create(Const ADocument : TPDFDocument; const AP2,AP3 : TPDFCoord; AWidth: TPDFFloat;AStroke: Boolean = True);overload;
  end;


  TPDFCurveY = class(TPDFDocumentObject)
  private
    FP1,FP3: TPDFCoord;
    FWidth: TPDFFloat;
    FStroke : Boolean;
  protected
    procedure Write(const AStream: TStream); override;
  public
    constructor Create(Const ADocument : TPDFDocument; const X1,Y1,X3,Y3,AWidth : TPDFFloat;AStroke: Boolean = True);overload;
    constructor Create(Const ADocument : TPDFDocument; const AP1,AP3 : TPDFCoord; AWidth: TPDFFloat;AStroke: Boolean = True);overload;
  end;


  TPDFEllipse = class(TPDFDocumentObject)
  private
    FCenter,
    FDimensions: TPDFCoord;
    FFill : Boolean;
    FStroke : Boolean;
    FLineWidth : TPDFFloat;
  protected
    procedure Write(const AStream: TStream); override;
  public
    constructor Create(Const ADocument : TPDFDocument; const APosX, APosY, AWidth, AHeight, ALineWidth: TPDFFloat; const AFill : Boolean = True; AStroke: Boolean = True);overload;
  end;


  TPDFSurface = class(TPDFDocumentObject)
  private
    FPoints: TPDFCoordArray;
    FFill : Boolean;
    FClose : Boolean;
  protected
    procedure Write(const AStream: TStream); override;
  public
    constructor Create(Const ADocument : TPDFDocument; const APoints: TPDFCoordArray; AClose : Boolean; AFill : Boolean = True); overload;
  end;


  TPDFImage = class(TPDFDocumentObject)
  private
    FNumber: integer;
    FPos: TPDFCoord;
    FSize: TPDFCoord;
  protected
    procedure Write(const AStream: TStream); override;
  public
    constructor Create(Const ADocument : TPDFDocument; const ALeft, ABottom, AWidth, AHeight: TPDFFloat; ANumber: integer); overload;
  end;


  TPDFLineStyle = class(TPDFDocumentObject)
  private
    FStyle: TPDFPenStyle;
    FPhase: integer;
    FLineWidth: TPDFFloat;
  protected
    procedure Write(const AStream: TStream);override;
  public
    constructor Create(Const ADocument : TPDFDocument; AStyle: TPDFPenStyle; APhase: integer; ALineWidth: TPDFFloat); overload;
  end;


  TPDFColor = class(TPDFDocumentObject)
  private
    FRed: string;
    FGreen: string;
    FBlue: string;
    FStroke: Boolean;
    FColor: TARGBColor;
  protected
    procedure Write(const AStream: TStream);override;
  public
    class function Command(const AStroke: boolean; const AColor: TARGBColor): string;
    constructor Create(Const ADocument : TPDFDocument; const AStroke: Boolean; AColor: TARGBColor); overload;
    property    Color: TARGBColor read FColor;
  end;


  TPDFDictionaryItem = class(TPDFDocumentObject)
  private
    FKey: TPDFName;
    FObj: TPDFObject;
  protected
    procedure Write(const AStream: TStream);override;
  public
    constructor Create(Const ADocument : TPDFDocument; const AKey: string; const AValue: TPDFObject);
    destructor Destroy; override;
    Property Value : TPDFObject Read FObj;
  end;


  TPDFDictionary = class(TPDFDocumentObject)
  private
    FElements: TFPObjectList; // list of TPDFDictionaryItem
    function GetE(AIndex : Integer): TPDFDictionaryItem;
    function GetEC: Integer;
    function GetV(AIndex : Integer): TPDFObject;
  protected
    procedure AddElement(const AKey: string; const AValue: TPDFObject);
    procedure AddName(const AKey,AName : String; const AMustEscape: boolean = True);
    procedure AddInteger(const AKey : String; AInteger : Integer);
    procedure AddReference(const AKey : String; AReference : Integer);
    procedure AddString(const AKey, AString : String);
    function IndexOfKey(const AValue: string): integer;
    procedure Write(const AStream: TStream); override;
    procedure WriteDictionary(const AObject: integer; const AStream: TStream);
  public
    constructor Create(Const ADocument : TPDFDocument); override;
    destructor Destroy; override;
    Function LastElement : TPDFDictionaryItem;
    Function LastValue : TPDFObject;
    Function FindElement(Const AKey : String) : TPDFDictionaryItem;
    Function FindValue(Const AKey : String) : TPDFObject;
    Function ElementByName(Const AKey : String) : TPDFDictionaryItem;
    Function ValueByName(Const AKey : String) : TPDFObject;
    Property Elements[AIndex : Integer] : TPDFDictionaryItem Read GetE;
    Property Values[AIndex : Integer] : TPDFObject Read GetV;
    Property ElementCount : Integer Read GetEC;
  end;


  TPDFXRef = class(TPDFDocumentObject)
  private
    FOffset: integer;
    FDict: TPDFDictionary;
    FStream: TPDFStream;
  protected
    procedure Write(const AStream: TStream);override;
  public
    constructor Create(Const ADocument : TPDFDocument); override;
    destructor Destroy; override;
    property Offset: integer read FOffset write FOffset;
    Property Dict : TPDFDictionary Read FDict;
  end;


  TPDFInfos = Class(TPersistent)
  private
    FApplicationName: String;
    FAuthor: String;
    FCreationDate: TDateTime;
    FProducer: String;
    FTitle: String;
  public
    constructor Create; virtual;
    Property Author : String Read FAuthor Write FAuthor;
    Property Title : String Read FTitle Write FTitle;
    Property ApplicationName : String Read FApplicationName Write FApplicationName;
    Property Producer : String Read FProducer Write FProducer;
    Property CreationDate : TDateTime Read FCreationDate Write FCreationDate;
  end;


  { When the WriteXXX() and DrawXXX() methods specify coordinates, they do it as
    per the PDF specification, from the bottom-left. }
  TPDFPage = Class(TPDFDocumentObject)
  private
    FObjects : TObjectList;
    FOrientation: TPDFPaperOrientation;
    FPaper: TPDFPaper;
    FPaperType: TPDFPaperType;
    FUnitOfMeasure: TPDFUnitOfMeasure;
    FMatrix: TPDFMatrix;
    FAnnots: TPDFAnnotList;
    FLastFont: TPDFEmbeddedFont;
    FLastFontColor: TARGBColor;
    procedure CalcPaperSize;
    function GetO(AIndex : Integer): TPDFObject;
    function GetObjectCount: Integer;
    function CreateAnnotList: TPDFAnnotList; virtual;
    procedure SetOrientation(AValue: TPDFPaperOrientation);
    procedure SetPaperType(AValue: TPDFPaperType);
    procedure AddTextToLookupLists(AText: UTF8String);
    procedure SetUnitOfMeasure(AValue: TPDFUnitOfMeasure);
  protected
    procedure AdjustMatrix; virtual;
    procedure DoUnitConversion(var APoint: TPDFCoord); virtual;
    procedure CreateStdFontText(X, Y: TPDFFloat; AText: AnsiString; const AFont: TPDFEmbeddedFont; const ADegrees: single; const AUnderline: boolean; const AStrikethrough: boolean); virtual;
    procedure CreateTTFFontText(X, Y: TPDFFloat; AText: UTF8String; const AFont: TPDFEmbeddedFont; const ADegrees: single; const AUnderline: boolean; const AStrikethrough: boolean); virtual;
  Public
    Constructor Create(Const ADocument : TPDFDocument); override;
    Destructor Destroy; override;
    Procedure AddObject(AObject : TPDFObject);
    // Commands. These will create objects in the objects list of the page.
    Procedure SetFont(AFontIndex : Integer; AFontSize : Integer); virtual;
    // used for stroking and nonstroking colors - purpose determined by the AStroke parameter
    Procedure SetColor(AColor : TARGBColor; AStroke : Boolean = True); virtual;
    Procedure SetPenStyle(AStyle : TPDFPenStyle; const ALineWidth: TPDFFloat = 1.0); virtual;
    // Set color and pen style from line style
    Procedure SetLineStyle(AIndex : Integer; AStroke : Boolean = True); overload;
    Procedure SetLineStyle(S : TPDFLineStyleDef; AStroke : Boolean = True); overload;
    { output coordinate is the font baseline. }
    Procedure WriteText(X, Y: TPDFFloat; AText : UTF8String; const ADegrees: single = 0.0; const AUnderline: boolean = false; const AStrikethrough: boolean = false); overload;
    Procedure WriteText(APos: TPDFCoord; AText : UTF8String; const ADegrees: single = 0.0; const AUnderline: boolean = false; const AStrikethrough: boolean = false); overload;
    procedure DrawLine(X1, Y1, X2, Y2, ALineWidth : TPDFFloat; const AStroke: Boolean = True); overload;
    procedure DrawLine(APos1, APos2: TPDFCoord; ALineWidth: TPDFFloat; const AStroke: Boolean = True); overload;
    Procedure DrawLineStyle(X1, Y1, X2, Y2: TPDFFloat; AStyle: Integer); overload;
    Procedure DrawLineStyle(APos1, APos2: TPDFCoord; AStyle: Integer); overload;
    { X, Y coordinates are the bottom-left coordinate of the rectangle. The W and H parameters are in the UnitOfMeasure units. }
    Procedure DrawRect(const X, Y, W, H, ALineWidth: TPDFFloat; const AFill, AStroke : Boolean; const ADegrees: single = 0.0); overload;
    Procedure DrawRect(const APos: TPDFCoord; const W, H, ALineWidth: TPDFFloat; const AFill, AStroke : Boolean; const ADegrees: single = 0.0); overload;
    { X, Y coordinates are the bottom-left coordinate of the rectangle. The W and H parameters are in the UnitOfMeasure units. }
    procedure DrawRoundedRect(const X, Y, W, H, ARadius, ALineWidth: TPDFFloat; const AFill, AStroke : Boolean; const ADegrees: single = 0.0);
    { X, Y coordinates are the bottom-left coordinate of the image. AWidth and AHeight are in image pixels. }
    Procedure DrawImageRawSize(const X, Y: TPDFFloat; const APixelWidth, APixelHeight, ANumber: integer; const ADegrees: single = 0.0); overload;
    Procedure DrawImageRawSize(const APos: TPDFCoord; const APixelWidth, APixelHeight, ANumber: integer; const ADegrees: single = 0.0); overload;
    { X, Y coordinates are the bottom-left coordinate of the image. AWidth and AHeight are in UnitOfMeasure units. }
    Procedure DrawImage(const X, Y: TPDFFloat; const AWidth, AHeight: TPDFFloat; const ANumber: integer; const ADegrees: single = 0.0); overload;
    Procedure DrawImage(const APos: TPDFCoord; const AWidth, AHeight: TPDFFloat; const ANumber: integer; const ADegrees: single = 0.0); overload;
    { X, Y coordinates are the bottom-left coordinate of the boundry rectangle.
      The W and H parameters are in the UnitOfMeasure units. A negative AWidth will
      cause the ellpise to draw to the left of the origin point. }
    Procedure DrawEllipse(const APosX, APosY, AWidth, AHeight, ALineWidth: TPDFFloat; const AFill: Boolean = True; AStroke: Boolean = True; const ADegrees: single = 0.0); overload;
    Procedure DrawEllipse(const APos: TPDFCoord; const AWidth, AHeight, ALineWidth: TPDFFloat; const AFill: Boolean = True; AStroke: Boolean = True; const ADegrees: single = 0.0); overload;
    procedure DrawPolygon(const APoints: array of TPDFCoord; const ALineWidth: TPDFFloat);
    procedure DrawPolyLine(const APoints: array of TPDFCoord; const ALineWidth: TPDFFloat);
    { start a new subpath }
    procedure ResetPath;
    { Close the current subpath by appending a straight line segment from the current point to the starting point of the subpath. }
    procedure ClosePath;
    procedure ClosePathStroke;
    { render the actual path }
    procedure StrokePath;
    { Fill using the nonzero winding number rule. }
    procedure FillStrokePath;
    { Fill using the Even-Odd rule. }
    procedure FillEvenOddStrokePath;
    { Move the current drawing position to (x, y) }
    procedure MoveTo(x, y: TPDFFloat); overload;
    procedure MoveTo(APos: TPDFCoord); overload;
    { Append a cubic Bezier curve to the current path
      - The curve extends from the current point to the point (xTo, yTo),
        using (xCtrl1, yCtrl1) and (xCtrl2, yCtrl2) as the Bezier control points
      - The new current point is (xTo, yTo) }
    procedure CubicCurveTo(const xCtrl1, yCtrl1, xCtrl2, yCtrl2, xTo, yTo, ALineWidth: TPDFFloat; AStroke: Boolean = True); overload;
    procedure CubicCurveTo(ACtrl1, ACtrl2, ATo: TPDFCoord; const ALineWidth: TPDFFloat; AStroke: Boolean = True); overload;
    { Append a cubic Bezier curve to the current path
      - The curve extends from the current point to the point (xTo, yTo),
        using the current point and (xCtrl2, yCtrl2) as the Bezier control points
      - The new current point is (xTo, yTo) }
    procedure CubicCurveToV(xCtrl2, yCtrl2, xTo, yTo: TPDFFloat; const ALineWidth: TPDFFloat; AStroke: Boolean = True); overload;
    procedure CubicCurveToV(ACtrl2, ATo: TPDFCoord; const ALineWidth: TPDFFloat; AStroke: Boolean = True); overload;
    { Append a cubic Bezier curve to the current path
      - The curve extends from the current point to the point (xTo, yTo),
        using (xCtrl1, yCtrl1) and (xTo, yTo) as the Bezier control points
      - The new current point is (xTo, yTo) }
    procedure CubicCurveToY(xCtrl1, yCtrl1, xTo, yTo: TPDFFloat; const ALineWidth: TPDFFloat; AStroke: Boolean = True); overload;
    procedure CubicCurveToY(ACtrl1, ATo: TPDFCoord; const ALineWidth: TPDFFloat; AStroke: Boolean = True); overload;
    { Define a rectangle that becomes a clickable hotspot, referencing the URI argument. }
    Procedure AddExternalLink(const APosX, APosY, AWidth, AHeight: TPDFFloat; const AURI: string; ABorder: boolean = false);
    { This returns the paper height, converted to whatever UnitOfMeasure is set too }
    function GetPaperHeight: TPDFFloat;
    Function HasImages : Boolean;
    // Quick settings for Paper.
    Property PaperType : TPDFPaperType Read FPaperType Write SetPaperType default ptA4;
    Property Orientation : TPDFPaperOrientation Read FOrientation Write SetOrientation;
    // Set this if you want custom paper size. You must set this before setting PaperType = ptCustom.
    Property Paper : TPDFPaper Read FPaper Write FPaper;
    // Unit of Measure - how the PDF Page should convert the coordinates and dimensions
    property UnitOfMeasure: TPDFUnitOfMeasure read FUnitOfMeasure write SetUnitOfMeasure default uomMillimeters;
    Property ObjectCount: Integer Read GetObjectCount;
    Property Objects[AIndex : Integer] : TPDFObject Read GetO; default;
    // returns the last font object created by SetFont()
    property LastFont: TPDFEmbeddedFont read FLastFont;
    { A 3x3 matrix used to translate the PDF Cartesian coordinate system to an Image coordinate system. }
    property Matrix: TPDFMatrix read FMatrix write FMatrix;
    property Annots: TPDFAnnotList read FAnnots;
  end;


  TPDFPageClass = class of TPDFPage;


  TPDFSection = Class(TCollectionItem)
  private
    FTitle: String;
    FPages : TFPList; // not owned
    function GetP(AIndex : Integer): TPDFPage;
    function GetP: Integer;
  Public
    Destructor Destroy; override;
    Procedure AddPage(APage : TPDFPage);
    Property Title : String Read FTitle Write FTitle;
    Property Pages[AIndex : Integer] : TPDFPage Read GetP;
    Property PageCount : Integer Read GetP;
  end;


  TPDFSectionList = Class(TCollection)
  private
    function GetS(AIndex : Integer): TPDFSection;
  Public
    Function AddSection : TPDFSection;
    Property Section[AIndex : Integer] : TPDFSection Read GetS; Default;
  end;


  TPDFFont = class(TCollectionItem)
  private
    FIsStdFont: boolean;
    FName: String;
    FFontFilename: String;
    FTrueTypeFile: TTFFileInfo;
    { stores mapping of Char IDs to font Glyph IDs }
    FTextMappingList: TTextMappingList;
    FSubsetFont: TStream;
    procedure   PrepareTextMapping;
    procedure   SetFontFilename(AValue: string);
    procedure   GenerateSubsetFont;
  public
    constructor Create(ACollection: TCollection); override;
    destructor  Destroy; override;
    { Returns a string where each character is replaced with a glyph index value instead. }
    function    GetGlyphIndices(const AText: UnicodeString): AnsiString;
    procedure   AddTextToMappingList(const AText: UnicodeString);
    Property    FontFile: string read FFontFilename write SetFontFilename;
    Property    Name: String Read FName Write FName;
    property    TextMapping: TTextMappingList read FTextMappingList;
    property    IsStdFont: boolean read FIsStdFont write FIsStdFont;
    property    SubsetFont: TStream read FSubsetFont;
  end;


  TPDFTrueTypeCharWidths = class(TPDFDocumentObject)
  private
    FEmbeddedFontNum: integer;
    FFontIndex: integer;
  protected
    procedure Write(const AStream: TStream);override;
  public
    constructor Create(const ADocument: TPDFDocument; const AEmbeddedFontNum: integer); overload;
    property EmbeddedFontNum: integer read FEmbeddedFontNum;
    property FontIndex: integer read FFontIndex write FFontIndex;
  end;


  TPDFFontDefs = Class(TCollection)
  private
    function GetF(AIndex : Integer): TPDFFont;
  Public
    Function AddFontDef : TPDFFont;
    Property FontDefs[AIndex : Integer] : TPDFFont Read GetF; Default;
  end;


  TPDFPages = Class(TPDFDocumentObject)
  private
    FList: TFPObjectList;
    FPageClass: TPDFPageClass;
    function    GetP(AIndex: Integer): TPDFPage;
    function    GetPageCount: integer;
  public
    constructor Create(const ADocument: TPDFDocument); override; overload;
    destructor  Destroy; override;
    function    AddPage: TPDFPage;
    procedure   Add(APage: TPDFPage);
    property    Count: integer read GetPageCount;
    property    Pages[AIndex: Integer]: TPDFPage read GetP; default;
    property    PageClass: TPDFPageClass read FPageClass write FPageClass;
  end;


  TPDFAnnot = class(TPDFObject)
  private
    FLeft: TPDFFloat;
    FBottom: TPDFFloat;
    FWidth: TPDFFloat;
    FHeight: TPDFFloat;
    FURI: string;
    FBorder: boolean;
  public
    constructor Create(const ADocument: TPDFDocument); override; overload;
    constructor Create(const ADocument: TPDFDocument; const ALeft, ABottom, AWidth, AHeight: TPDFFloat; const AURI: String; const ABorder: Boolean = false); overload;
  end;


  TPDFAnnotList = class(TPDFDocumentObject)
  private
    FList: TFPObjectList;
    procedure   CheckList;
    function    GetAnnot(AIndex: integer): TPDFAnnot;
  public
    destructor  Destroy; override;
    function    AddAnnot: TPDFAnnot;
    function    Count: integer;
    procedure   Add(AAnnot: TPDFAnnot);
    property    Annots[AIndex: integer]: TPDFAnnot read GetAnnot; default;
  end;


  TPDFImageCompression = (icNone, icDeflate, icJPEG);


  TPDFImageItem = Class(TCollectionItem)
  private
    FImage: TFPCustomImage;
    FOwnsImage: Boolean;
    FStreamed: TBytes;
    FCompression: TPDFImageCompression;
    FWidth,FHeight : Integer;
    function GetHeight: Integer;
    function GetStreamed: TBytes;
    function GetWidth: Integer;
    procedure SetImage(AValue: TFPCustomImage);
    procedure SetStreamed(AValue: TBytes);
  Public
    Destructor Destroy; override;
    Procedure CreateStreamedData(AUseCompression: Boolean);
    Function WriteImageStream(AStream: TStream): int64; virtual;
    function Equals(AImage: TFPCustomImage): boolean; reintroduce;
    Property Image : TFPCustomImage Read FImage Write SetImage;
    Property StreamedData : TBytes Read GetStreamed Write SetStreamed;
    Property OwnsImage : Boolean Read FOwnsImage Write FOwnsImage;
    Property Width : Integer Read GetWidth;
    Property Height : Integer Read GetHeight;
  end;


  TPDFImages = Class(TCollection)
  Private
    FOwner: TPDFDocument;
    function GetI(AIndex : Integer): TPDFImageItem;
  Protected
    function GetOwner: TPersistent; override;
  Public
    Constructor Create(AOwner: TPDFDocument; AItemClass : TCollectionItemClass);
    Function AddImageItem : TPDFImageItem;
    Function AddJPEGStream(Const AStream : TStream; Width,Height : Integer): Integer;
    Function AddFromStream(Const AStream : TStream; Handler : TFPCustomImageReaderClass;
      KeepImage : Boolean = False): Integer;
    Function AddFromFile(Const AFileName : String; KeepImage : Boolean = False): Integer;
    Property Images[AIndex : Integer] : TPDFImageItem Read GetI; default;
    Property Owner: TPDFDocument read FOwner;
  end;


  TPDFFontNumBaseObject = class(TPDFDocumentObject)
  protected
    FFontNum: integer;
  public
    constructor Create(const ADocument: TPDFDocument; const AFontNum: integer); overload;
    property FontNum: integer read FFontNum;
  end;


  TPDFToUnicode = class(TPDFFontNumBaseObject)
  protected
    procedure Write(const AStream: TStream); override;
  end;


  TCIDToGIDMap = class(TPDFFontNumBaseObject)
  protected
    procedure Write(const AStream: TStream); override;
  end;


  TPDFCIDSet = class(TPDFFontNumBaseObject)
  protected
    procedure Write(const AStream: TStream); override;
  end;


  TPDFLineStyleDef = Class(TCollectionItem)
  private
    FColor: TARGBColor;
    FLineWidth: TPDFFloat;
    FPenStyle: TPDFPenStyle;
  Public
    Procedure Assign(Source : TPersistent); override;
  Published
    Property LineWidth : TPDFFloat Read FLineWidth Write FLineWidth;
    Property Color : TARGBColor Read FColor Write FColor Default clBlack;
    Property PenStyle : TPDFPenStyle Read FPenStyle Write FPenStyle Default ppsSolid;
  end;


  TPDFLineStyleDefs = Class(TCollection)
  private
    function GetI(AIndex : Integer): TPDFLineStyleDef;
  Public
    Function AddLineStyleDef : TPDFLineStyleDef;
    Property Defs[AIndex : Integer] : TPDFLineStyleDef Read GetI; Default;
  end;


  TPDFDocument = class(TComponent)
  private
    FCatalogue: integer;
    FCurrentColor: string;
    FCurrentWidth: string;
    FDefaultOrientation: TPDFPaperOrientation;
    FDefaultPaperType: TPDFPaperType;
    FFontDirectory: string;
    FFontFiles: TStrings;
    FFonts: TPDFFontDefs;
    FImages: TPDFImages;
    FInfos: TPDFInfos;
    FLineStyleDefs: TPDFLineStyleDefs;
    FObjectCount: Integer;
    FOptions: TPDFOptions;
    FPages: TPDFPages;
    FPreferences: Boolean;
    FPageLayout: TPDFPageLayout;
    FSections: TPDFSectionList;
    FTrailer: TPDFDictionary;
    FZoomValue: string;
    FGlobalXRefs: TFPObjectList; // list of TPDFXRef
    FUnitOfMeasure: TPDFUnitOfMeasure;
    function GetStdFontCharWidthsArray(const AFontName: string): TPDFFontWidthArray;
    function GetX(AIndex : Integer): TPDFXRef;
    function GetXC: Integer;
    function GetTotalAnnotsCount: integer;
    function GetFontNamePrefix(const AFontNum: Integer): string;
    procedure SetFontFiles(AValue: TStrings);
    procedure SetFonts(AValue: TPDFFontDefs);
    procedure SetInfos(AValue: TPDFInfos);
    procedure SetLineStyles(AValue: TPDFLineStyleDefs);
  protected
    // Create all kinds of things, virtual so they can be overridden to create descendents instead
    function CreatePDFPages: TPDFPages; virtual;
    function CreateLineStyles: TPDFLineStyleDefs; virtual;
    function CreateFontDefs: TPDFFontDefs; virtual;
    function CreatePDFImages: TPDFImages; virtual;
    function CreatePDFInfos: TPDFInfos; virtual;
    function CreateSectionList: TPDFSectionList; virtual;
    // Returns next prevoutline
    function CreateSectionOutLine(Const SectionIndex,OutLineRoot,ParentOutLine,NextSect,PrevSect : Integer): Integer; virtual;
    Function CreateSectionsOutLine : Integer; virtual;
    Function CreateSectionPageOutLine(Const S: TPDFSection; Const PageOutLine, PageIndex, NewPage,  ParentOutline, NextOutline, PrevOutLine : Integer) : Integer;virtual;
    procedure AddFontNameToPages(const AName: String; ANum : Integer);
    procedure WriteXRefTable(const AStream: TStream);
    procedure WriteObject(const AObject: integer; const AStream: TStream);
    procedure CreateRefTable;virtual;
    procedure CreateTrailer;virtual;
    procedure CreateFontEntries; virtual;
    procedure CreateImageEntries; virtual;
    procedure CreateAnnotEntries(const APageNum: integer; const APageDict: TPDFDictionary); virtual;
    function CreateContentsEntry(const APageNum: integer): integer;virtual;
    function CreateCatalogEntry: integer;virtual;
    procedure CreateInfoEntry;virtual;
    procedure CreatePreferencesEntry;virtual;
    function CreatePagesEntry(Parent: integer): integer;virtual;
    function CreatePageEntry(Parent, PageNum: integer): integer;virtual;
    function CreateOutlines: integer;virtual;
    function CreateOutlineEntry(Parent, SectNo, PageNo: integer; ATitle: string): integer;virtual;
    function LoadFont(AFont: TPDFFont): boolean;
    procedure CreateStdFont(EmbeddedFontName: string; EmbeddedFontNum: integer);virtual;
    procedure CreateTTFFont(const EmbeddedFontNum: integer);virtual;
    procedure CreateTTFDescendantFont(const EmbeddedFontNum: integer);virtual;
    procedure CreateTTFCIDSystemInfo;virtual;
    procedure CreateTp1Font(const EmbeddedFontNum: integer);virtual;
    procedure CreateFontDescriptor(const EmbeddedFontNum: integer);virtual;
    procedure CreateToUnicode(const AFontNum: integer);virtual;
    procedure CreateFontFileEntry(const AFontNum: integer);virtual;
    procedure CreateCIDSet(const AFontNum: integer); virtual;
    procedure CreateImageEntry(ImgWidth, ImgHeight, NumImg: integer);virtual;
    function CreateAnnotEntry(const APageNum, AnnotNum: integer): integer; virtual;
    function CreateCIDToGIDMap(const AFontNum: integer): integer; virtual;
    procedure CreatePageStream(APage : TPDFPage; PageNum: integer);
    Function CreateString(Const AValue : String) : TPDFString;
    Function CreateUTF8String(Const AValue : UTF8String; const AFontIndex: integer) : TPDFUTF8String;
    Function CreateGlobalXRef: TPDFXRef;
    Function AddGlobalXRef(AXRef : TPDFXRef) : Integer;
    function IndexOfGlobalXRef(const AValue: string): integer;
    Function FindGlobalXRef(Const AName : String) : TPDFXRef;
    Function GlobalXRefByName(Const AName : String) : TPDFXRef;
    Property GlobalXRefs[AIndex : Integer] : TPDFXRef Read GetX;
    Property GlobalXRefCount : Integer Read GetXC;
    Property CurrentColor: string Read FCurrentColor Write FCurrentColor;
    Property CurrentWidth: string Read FCurrentWidth Write FCurrentWidth;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    procedure StartDocument;
    procedure Reset;
    procedure SaveToStream(const AStream: TStream); virtual;
    Procedure SaveToFile(Const AFileName : String);
    function  IsStandardPDFFont(AFontName: string): boolean;
    // Create objects, owned by this document.
    Function CreateEmbeddedFont(const APage: TPDFPage; AFontIndex, AFontSize : Integer) : TPDFEmbeddedFont;
    Function CreateText(X,Y : TPDFFloat; AText : AnsiString; const AFont: TPDFEmbeddedFont; const ADegrees: single; const AUnderline: boolean; const AStrikethrough: boolean) : TPDFText; overload;
    Function CreateText(X,Y : TPDFFloat; AText : UTF8String; const AFont: TPDFEmbeddedFont; const ADegrees: single; const AUnderline: boolean; const AStrikethrough: boolean) : TPDFUTF8Text; overload;
    Function CreateRectangle(const X,Y,W,H, ALineWidth: TPDFFloat; const AFill, AStroke: Boolean) : TPDFRectangle;
    function CreateRoundedRectangle(const X, Y, W, H, ARadius, ALineWidth: TPDFFloat; const AFill, AStroke: Boolean): TPDFRoundedRectangle;
    Function CreateColor(AColor : TARGBColor; AStroke : Boolean) : TPDFColor;
    Function CreateBoolean(AValue : Boolean) : TPDFBoolean;
    Function CreateInteger(AValue : Integer) : TPDFInteger;
    Function CreateReference(AValue : Integer) : TPDFReference;
    Function CreateLineStyle(APenStyle: TPDFPenStyle; const ALineWidth: TPDFFloat) : TPDFLineStyle;
    Function CreateName(AValue : String; const AMustEscape: boolean = True) : TPDFName;
    Function CreateStream(OwnsObjects : Boolean = True) : TPDFStream;
    Function CreateDictionary : TPDFDictionary;
    Function CreateXRef : TPDFXRef;
    Function CreateArray : TPDFArray;
    Function CreateImage(const ALeft, ABottom, AWidth, AHeight: TPDFFloat; ANumber: integer) : TPDFImage;
    Function AddFont(AName : String) : Integer; overload;
    Function AddFont(AFontFile: String; AName : String) : Integer; overload;
    Function AddLineStyleDef(ALineWidth : TPDFFloat; AColor : TARGBColor = clBlack; APenStyle : TPDFPenStyle = ppsSolid) : Integer;
    Property Fonts : TPDFFontDefs Read FFonts Write SetFonts;
    Property Pages : TPDFPages Read FPages;
    Property Images : TPDFImages Read FImages;
    Property Catalogue: integer Read FCatalogue;
    Property Trailer: TPDFDictionary Read FTrailer;
    Property FontFiles : TStrings Read FFontFiles Write SetFontFiles;
    Property FontDirectory: string Read FFontDirectory Write FFontDirectory;
    Property Sections : TPDFSectionList Read FSections;
    Property ObjectCount : Integer Read FObjectCount;
  Published
    Property Options : TPDFOptions Read FOptions Write FOPtions;
    Property LineStyles : TPDFLineStyleDefs Read FLineStyleDefs Write SetLineStyles;
    property PageLayout: TPDFPageLayout read FPageLayout write FPageLayout default lSingle;
    Property Infos : TPDFInfos Read FInfos Write SetInfos;
    Property DefaultPaperType : TPDFPaperTYpe Read FDefaultPaperType Write FDefaultPaperType;
    Property DefaultOrientation : TPDFPaperOrientation Read FDefaultOrientation Write FDefaultOrientation;
    property DefaultUnitOfMeasure: TPDFUnitOfMeasure read FUnitOfMeasure write FUnitOfMeasure default uomMillimeters;

  end;


const
  CRLF = #13#10;
  PDF_VERSION = '%PDF-1.3';
  PDF_BINARY_BLOB = '%'#$C3#$A4#$C3#$BC#$C3#$B6#$C3#$9F;
  PDF_FILE_END = '%%EOF';
  PDF_MAX_GEN_NUM = 65535;
  PDF_UNICODE_HEADER = 'FEFF001B%s001B';
  PDF_LANG_STRING = 'en';

  { Info from http://www.papersizes.org/a-sizes-all-units.htm }
  PDFPaperSizes : Array[TPDFPaperType,0..1] of Integer = (
    // Height,Width (units in pixels (or Points))
      (0,0),          // ptCustom
      (842,595),      // ptA4
      (595,420),      // ptA5
      (792,612),      // ptLetter
      (1008,612),     // ptLegal
      (756,522),      // ptExecutive
      (684,297),      // ptComm10
      (540,279),      // ptMonarch
      (624,312),      // ptDL
      (649,459),      // ptC5
      (709,499)       // ptB5
    );

  PDFPaperPrintables : Array[TPDFPaperType,0..3] of Integer = (
     // Top,Left,Right,Bottom (units in pixels)
      (0,0,0,0),          // ptCustom
      (10,11,586,822),    // ptA4
      (10,11,407,588),    // ptA5
      (13,13,599,780),    // ptLetter
      (13,13,599,996),    // ptLegal
      (14,13,508,744),    // ptExecutive
      (13,13,284,672),    // ptComm10
      (13,13,266,528),    // ptMonarch
      (14,13,297,611),    // ptDL
      (13,13,446,637),    // ptC5
      (14,13,485,696)     // ptB5
    );

  PageLayoutNames : Array[TPDFPageLayout] of String
                  = ('SinglePage','TwoColumnLeft','OneColumn');


// Helper procedures - made them global for unit testing purposes
procedure CompressStream(AFrom: TStream; ATo: TStream; ACompressLevel: TCompressionLevel = clDefault; ASkipHeader: boolean = False);
procedure CompressString(const AFrom: string; var ATo: string);
procedure DecompressStream(AFrom: TStream; ATo: TStream);

function mmToPDF(mm: single): TPDFFloat;
function PDFTomm(APixels : TPDFFloat) : Single;
function cmToPDF(cm: single): TPDFFloat;
function PDFtoCM(APixels: TPDFFloat): single;
function InchesToPDF(Inches: single): TPDFFloat;
function PDFtoInches(APixels: TPDFFloat): single;

function PDFCoord(x, y: TPDFFloat): TPDFCoord;

implementation

uses
  math,
  fpttf;


resourcestring
  rsErrReportFontFileMissing = 'Font File "%s" does not exist.';
  rsErrDictElementNotFound = 'Error: Dictionary element "%s" not found.';
  rsErrInvalidSectionPage = 'Error: Invalid section page index.';
  rsErrNoGlobalDict = 'Error: no global XRef named "%s".';
  rsErrInvalidPageIndex = 'Invalid page index: %d';
  rsErrInvalidAnnotIndex = 'Invalid annot index: %d';
  rsErrNoFontDefined = 'No Font was set - please use SetFont() first.';
  rsErrNoImageReader = 'Unsupported image format - no image reader available.';
  rsErrUnknownStdFont = 'Unknown standard PDF font name <%s>.';

{ Includes font metrics constant arrays for the standard PDF fonts. They are
  not used at the moment, but in future we might want to do something with
  them. }
{$I fontmetrics_stdpdf.inc }

type
  // to get access to protected methods
  TTTFFriendClass = class(TTFFileInfo)
  end;


const
  cInchToMM = 25.4;
  cInchToCM = 2.54;
  cDefaultDPI = 72;
  // mm = (pixels * 25.4) / dpi
  // pixels = (mm * dpi) / 25.4
  // cm = ((pixels * 25.4) / dpi) / 10

  // see http://paste.lisp.org/display/1105
  BEZIER: single = 0.5522847498; // = 4/3 * (sqrt(2) - 1);


function DateToPdfDate(const ADate: TDateTime): string;
begin
  Result:=FormatDateTime('"D:"yyyymmddhhnnss', ADate);
end;

function FormatPDFInt(const Value: integer; PadLen: integer): string;
begin
  Result:=IntToStr(Value);
  Dec(PadLen,Length(Result));
  if PadLen>0 then
    Result:=StringOfChar('0',Padlen)+Result;
end;

procedure CompressStream(AFrom: TStream; ATo: TStream; ACompressLevel: TCompressionLevel = clDefault; ASkipHeader: boolean = False);
var
  c: TCompressionStream;
begin
  if AFrom.Size = 0 then
  begin
    ATo.Size := 0;
    Exit; //==>
  end;

  c := TCompressionStream.Create(ACompressLevel, ATo, ASkipHeader);
  try
    AFrom.Position := 0;
    c.CopyFrom(AFrom, AFrom.Size);
    c.Flush;
  finally
    c.Free;
  end;
end;

procedure CompressString(const AFrom: string; var ATo: string);
var
  lStreamFrom : TStringStream;
  lStreamTo  : TStringStream;
begin
  { TODO : Possible improvement would be to perform this compression directly on
           the string as a buffer, and not go through the stream stage. }
  lStreamFrom := TStringStream.Create(AFrom);
  try
    lStreamTo  := TStringStream.Create('');
    try
      lStreamFrom.Position := 0;
      lStreamTo.Size := 0;
      CompressStream(lStreamFrom, lStreamTo);
      ATo  := lStreamTo.DataString;
    finally
      lStreamTo.Free;
    end;
  finally
    lStreamFrom.Free;
  end;
end;

procedure DecompressStream(AFrom: TStream; ATo: TStream);

{$IFDEF VER2_6}
{$DEFINE NOHEADERWORKADOUND}
{$ENDIF}
{$IFDEF VER3_0}
{$DEFINE NOHEADERWORKADOUND}
{$ENDIF}

Const
  BufSize = 1024; // 1K

Type
  TBuffer = Array[0..BufSize-1] of byte;

var
  d: TDecompressionStream;
  {$IFDEF NOHEADERWORKADOUND}
  I: integer;
  {$ENDIF}
  Count : Integer;
  Buffer : TBuffer;

begin
  if AFrom.Size = 0 then
  begin
    ATo.Size := 0;
    Exit; //==>
  end;
  FillMem(@Buffer, SizeOf(TBuffer), 0);

  AFrom.Position := 0;
  AFrom.Seek(0,soFromEnd);
{$IFDEF NOHEADERWORKADOUND}
  // Work around a paszlib bug, FPC bugtracker 26827
  I:=0;
  AFrom.Write(I,SizeOf(I));
  AFrom.Position:=0;
{$ENDIF}
  D:=TDecompressionStream.Create(AFrom, False);
  try
    repeat
       Count:=D.Read(Buffer,BufSize);
       ATo.WriteBuffer(Buffer,Count);
     until (Count<BufSize);
  finally
    d.Free;
  end;
end;

function mmToPDF(mm: single): TPDFFloat;
begin
  Result := mm * (cDefaultDPI / cInchToMM);
end;

function PDFTomm(APixels: TPDFFloat): Single;
begin
  Result := (APixels * cInchToMM) / cDefaultDPI;
end;

function cmToPDF(cm: single): TPDFFloat;
begin
  Result := cm *(cDefaultDPI / cInchToCM);
end;

function PDFtoCM(APixels: TPDFFloat): single;
begin
  Result := (APixels * cInchToCM) / cDefaultDPI;
end;

function InchesToPDF(Inches: single): TPDFFloat;
begin
  Result := Inches * cDefaultDPI;
end;

function PDFCoord(x, y: TPDFFloat): TPDFCoord;
begin
  Result.x := x;
  Result.y := y;
end;

function PDFtoInches(APixels: TPDFFloat): single;
begin
  Result := APixels / cDefaultDPI;
end;

{ TPDFMatrix }

function TPDFMatrix.Transform(APoint: TPDFCoord): TPDFCoord;
begin
  Result.x := _00 * APoint.x + _20;
  Result.y := _11 * APoint.y + _21;
end;

function TPDFMatrix.Transform(X, Y: TPDFFloat): TPDFCoord;
begin
  Result.x := _00 * X + _20;
  Result.y := _11 * Y + _21;
end;

function TPDFMatrix.ReverseTransform(APoint: TPDFCoord): TPDFCoord;
begin
  Result.x := (APoint.x - _20) / _00;
  Result.y := (APoint.y - _21) / _11;
end;

procedure TPDFMatrix.SetXScalation(const AValue: TPDFFloat);
begin
  _00 := AValue;
end;

procedure TPDFMatrix.SetYScalation(const AValue: TPDFFloat);
begin
  _11 := AValue;
end;

procedure TPDFMatrix.SetXTranslation(const AValue: TPDFFloat);
begin
  _20 := AValue;
end;

procedure TPDFMatrix.SetYTranslation(const AValue: TPDFFloat);
begin
  _21 := AValue;
end;

{ TPDFFont }

procedure TPDFFont.PrepareTextMapping;
begin
  if FFontFilename <> '' then
  begin
    // only create objects when needed
    FTextMappingList := TTextMappingList.Create;
    FTrueTypeFile := TTFFileInfo.Create;
    FTrueTypeFile.LoadFromFile(FFontFilename);
    FTrueTypeFile.PrepareFontDefinition('cp1252', True);
  end;
end;

procedure TPDFFont.SetFontFilename(AValue: string);
begin
  if FFontFilename = AValue then
    Exit;
  FFontFilename := AValue;
  PrepareTextMapping;
end;

procedure TPDFFont.GenerateSubsetFont;
var
  f: TFontSubsetter;
  {$ifdef gdebug}
  fs: TFileStream;
  {$endif}
begin
  if Assigned(FSubsetFont) then
    FreeAndNil(FSubSetFont);
  f := TFontSubsetter.Create(FTrueTypeFile, FTextMappingList);
  try
    FSubSetFont := TMemoryStream.Create;
    f.SaveToStream(FSubsetFont);
    {$ifdef gdebug}
    fs := TFileStream.Create(FTrueTypeFile.PostScriptName + '-subset.ttf', fmCreate);
    FSubSetFont.Position := 0;
    TMemoryStream(FSubsetFont).SaveToStream(fs);
    fs.Free;
    {$endif}
  finally
    f.Free;
  end;
end;

constructor TPDFFont.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FSubsetFont := nil;
end;

destructor TPDFFont.Destroy;
begin
  FTextMappingList.Free;
  FTrueTypeFile.Free;
  FSubSetFont.Free;
  inherited Destroy;
end;

function TPDFFont.GetGlyphIndices(const AText: UnicodeString): AnsiString;
var
  i: integer;
  c: word;
  n: integer;
begin
  Result := '';
  if Length(AText) = 0 then
    Exit;
  for i := 1 to Length(AText) do
  begin
    c := Word(AText[i]);
    for n := 0 to FTextMappingList.Count-1 do
    begin
      if FTextMappingList[n].CharID = c then
      begin
        result := Result + IntToHex(FTextMappingList[n].GlyphID, 4);
        break;
      end;
    end;
  end;
end;

procedure TPDFFont.AddTextToMappingList(const AText: UnicodeString);
var
  i: integer;
  c: uint16; // Unicode codepoint
  gid: uint16;
begin
  if AText = '' then
    Exit;
  for i := 1 to Length(AText) do
  begin
    c := uint16(AText[i]);
    gid := FTrueTypeFile.GetGlyphIndex(c);
    FTextMappingList.Add(c, gid);
  end;
end;

{ TPDFTrueTypeCharWidths }

// TODO: (optional improvement) CID -> Unicode mappings, use ranges to generate a smaller CMap
//       See pdfbox's writeTo() method in ToUnicodeWriter.java
procedure TPDFTrueTypeCharWidths.Write(const AStream: TStream);
var
  i: integer;
  s: string;
  lst: TTextMappingList;
  lFont: TTFFileInfo;
begin
  s := '';
  lst := Document.Fonts[EmbeddedFontNum].TextMapping;
  lst.Sort;
  lFont := Document.Fonts[EmbeddedFontNum].FTrueTypeFile;
  // use decimal values for the output
  for i := 0 to lst.Count-1 do
    s :=  s + Format(' %d [%d]', [ lst[i].GlyphID, TTTFFriendClass(lFont).ToNatural(lFont.Widths[lst[i].GlyphID].AdvanceWidth)]);
  WriteString(s, AStream);
end;

constructor TPDFTrueTypeCharWidths.Create(const ADocument: TPDFDocument; const AEmbeddedFontNum: integer);
begin
  inherited Create(ADocument);
  FEmbeddedFontNum := AEmbeddedFontNum;
end;

{ TPDFMoveTo }

class function TPDFMoveTo.Command(APos: TPDFCoord): String;

begin
  Result:=Command(APos.X,APos.Y);
end;

class function TPDFMoveTo.Command(AX, AY: TPDFFloat): String;
begin
  Result:=FloatStr(AX)+' '+FloatStr(AY)+' m'+CRLF;
end;

procedure TPDFMoveTo.Write(const AStream: TStream);
begin
  WriteString(Command(FPos),AStream);
end;

constructor TPDFMoveTo.Create(const ADocument: TPDFDocument; const AX,
  AY: TPDFFloat);
begin
  Inherited Create(ADocument);
  FPos.X:=AX;
  FPos.Y:=AY;
end;

constructor TPDFMoveTo.Create(const ADocument: TPDFDocument;
  const APos: TPDFCoord);
begin
  Inherited Create(ADocument);
  FPos:=APos;
end;

{ TPDFResetPath }

procedure TPDFResetPath.Write(const AStream: TStream);
begin
  WriteString(Command, AStream);
end;

class function TPDFResetPath.Command: string;
begin
  Result := 'n' + CRLF;
end;

{ TPDFClosePath }

procedure TPDFClosePath.Write(const AStream: TStream);
begin
  WriteString(Command, AStream);
end;

class function TPDFClosePath.Command: string;
begin
  Result := 'h' + CRLF;
end;

{ TPDFStrokePath }

procedure TPDFStrokePath.Write(const AStream: TStream);
begin
  WriteString(Command, AStream);
end;

class function TPDFStrokePath.Command: string;
begin
  Result := 'S' + CRLF;
end;

{ TPDFPushGraphicsStack }

procedure TPDFPushGraphicsStack.Write(const AStream: TStream);
begin
  WriteString(Command, AStream);
end;

class function TPDFPushGraphicsStack.Command: string;
begin
  Result := 'q'+CRLF;
end;

{ TPDFPopGraphicsStack }

procedure TPDFPopGraphicsStack.Write(const AStream: TStream);
begin
  WriteString(Command, AStream);
end;

class function TPDFPopGraphicsStack.Command: string;
begin
  Result := 'Q' + CRLF;
end;

{ TPDFEllipse }

procedure TPDFEllipse.Write(const AStream: TStream);
Var
  X,Y,W2,H2,WS,HS : TPDFFloat;
begin
  if FStroke then
    SetWidth(FLineWidth, AStream);

  X:=FCenter.X;
  Y:=FCenter.Y;
  W2:=FDimensions.X/2;
  H2:=FDimensions.Y/2;
  WS:=W2*BEZIER;
  HS:=H2*BEZIER;
  // Starting point
  WriteString(TPDFMoveTo.Command(X,Y+H2),AStream);
  WriteString(TPDFCurveC.Command(X, Y+H2-HS, X+W2-WS, Y, X+W2, Y),AStream);
  WriteString(TPDFCurveC.Command(X+W2+WS, Y, X+W2*2, Y+H2-HS, X+W2*2, Y+H2),AStream);
  WriteString(TPDFCurveC.Command(X+W2*2, Y+H2+HS, X+W2+WS, Y+H2*2, X+W2, Y+H2*2),AStream);
  WriteString(TPDFCurveC.Command(X+W2-WS, Y+H2*2, X, Y+H2+HS, X, Y+H2),AStream);

  if FStroke and FFill then
    WriteString('b'+CRLF, AStream)
  else if FFill then
    WriteString('f'+CRLF, AStream)
  else if FStroke then
    WriteString('S'+CRLF, AStream);
  (*
  // should we default to this if no stroking or filling is required?
  else
    WriteString('n'+CRLF, AStream); // see PDF 1.3 Specification document on page 152
  *)
end;

constructor TPDFEllipse.Create(const ADocument: TPDFDocument; const APosX, APosY, AWidth, AHeight,
  ALineWidth: TPDFFloat; const AFill: Boolean; AStroke: Boolean);
begin
  Inherited Create(ADocument);
  FLineWidth:=ALineWidth;
  FCenter.X:=APosX;
  FCenter.Y:=APosY;
  FDimensions.X:=AWidth;
  FDimensions.Y:=AHeight;
  FFill:=AFill;
  FStroke:=AStroke;
end;

{ TPDFCurveY }

procedure TPDFCurveY.Write(const AStream: TStream);

begin
  if FStroke then
    SetWidth(FWidth,AStream);
  WriteString(FloatStr(FP1.X)+' '+FloatStr(FP1.Y)+' '+
              FloatStr(FP3.X)+' '+FloatStr(FP3.Y)+' y'+CRLF,AStream);
  if FStroke then
    WriteString('S'+CRLF, AStream);
end;

constructor TPDFCurveY.Create(const ADocument: TPDFDocument; const X1, Y1, X3,
  Y3, AWidth: TPDFFloat; AStroke: Boolean);
begin
  Inherited Create(ADocument);
  FP1.X:=X1;
  FP1.Y:=Y1;
  FP3.X:=X3;
  FP3.Y:=Y3;
  FWidth:=AWidth;
  FStroke:=AStroke;
end;

constructor TPDFCurveY.Create(const ADocument: TPDFDocument; const AP1,
  AP3: TPDFCoord; AWidth: TPDFFloat; AStroke: Boolean);
begin
  Inherited Create(ADocument);
  FP1:=AP1;
  FP3:=AP3;
  FWidth:=AWidth;
  FStroke:=AStroke;
end;


{ TPDFCurveV }

procedure TPDFCurveV.Write(const AStream: TStream);

begin
  if FStroke then
    SetWidth(FWidth,AStream);
  WriteString(FloatStr(FP2.X)+' '+FloatStr(FP2.Y)+' '+
              FloatStr(FP3.X)+' '+FloatStr(FP3.Y)+' v'+CRLF,AStream);
  if FStroke then
    WriteString('S'+CRLF, AStream);
end;

constructor TPDFCurveV.Create(const ADocument: TPDFDocument; const X2, Y2, X3,
  Y3, AWidth: TPDFFloat;AStroke: Boolean = True);
begin
  Inherited Create(ADocument);
  FP2.X:=X2;
  FP2.Y:=Y2;
  FP3.X:=X3;
  FP3.Y:=Y3;
  FWidth:=AWidth;
  FStroke:=AStroke;
end;

constructor TPDFCurveV.Create(const ADocument: TPDFDocument; const AP2,
  AP3: TPDFCoord; AWidth: TPDFFloat;AStroke: Boolean = True);
begin
  Inherited Create(ADocument);
  FP2:=AP2;
  FP3:=AP3;
  FWidth:=AWidth;
  FStroke:=AStroke;
end;

{ TPDFCurveC }

class function TPDFCurveC.Command(const xCtrl1, yCtrl1, xCtrl2, yCtrl2, xTo, yTo: TPDFFloat): String;
begin
  Result:=FloatStr(xCtrl1)+' '+FloatStr(yCtrl1)+' '+
          FloatStr(xCtrl2)+' '+FloatStr(yCtrl2)+' '+
          FloatStr(xTo)+' '+FloatStr(yTo)+' c'+CRLF
end;

class function TPDFCurveC.Command(const ACtrl1, ACtrl2, ATo3: TPDFCoord): String;
begin
  Result := Command(ACtrl1.X, ACtrl1.Y, ACtrl2.X, ACtrl2.Y, ATo3.X, ATo3.Y);
end;

procedure TPDFCurveC.Write(const AStream: TStream);
begin
  if FStroke then
    SetWidth(FWidth, AStream);
  WriteString(Command(FCtrl1, FCtrl2, FTo), AStream);
  if FStroke then
    WriteString('S'+CRLF, AStream);
end;

constructor TPDFCurveC.Create(const ADocument: TPDFDocument; const xCtrl1, yCtrl1, xCtrl2, yCtrl2, xTo, yTo,
  AWidth: TPDFFloat; AStroke: Boolean);
begin
  Inherited Create(ADocument);
  FCtrl1.X := xCtrl1;
  FCtrl1.Y := yCtrl1;
  FCtrl2.X := xCtrl2;
  FCtrl2.Y := yCtrl2;
  FTo.X := xTo;
  FTo.Y := yTo;
  FWidth := AWidth;
  FStroke := AStroke;
end;

constructor TPDFCurveC.Create(const ADocument: TPDFDocument; const ACtrl1, ACtrl2, ATo3: TPDFCoord;
    AWidth: TPDFFloat; AStroke: Boolean);
begin
  Inherited Create(ADocument);
  FCtrl1 := ACtrl1;
  FCtrl2 := ACtrl2;
  FTo := ATo3;
  FWidth := AWidth;
  FStroke := AStroke;
end;

{ TPDFLineStyleDef }

Procedure TPDFLineStyleDef.Assign(Source : TPersistent);

Var
 L : TPDFLineStyleDef;

begin
  if Source is TPDFLineStyleDef then
    begin
    L:=Source as TPDFLineStyleDef;
    LineWidth:=L.LineWidth;
    Color:=L.Color;
    PenStyle:=L.PenStyle;
    end
  else
    Inherited;
end;


{ TPDFLineStyleDefs }

function TPDFLineStyleDefs.GetI(AIndex : Integer): TPDFLineStyleDef;
begin
  Result:=TPDFLineStyleDef(Items[AIndex]);
end;

function TPDFLineStyleDefs.AddLineStyleDef: TPDFLineStyleDef;
begin
  Result:=Add as TPDFLineStyleDef;
end;

{ TPDFPages }

function TPDFPages.GetP(AIndex : Integer): TPDFPage;
begin
  if Assigned(Flist) then
    Result:=TPDFPage(FList[Aindex])
  else
    Raise EListError.CreateFmt(rsErrInvalidPageIndex,[AIndex]);
end;

function TPDFPages.GetPageCount: integer;
begin
  result := FList.Count;
end;

constructor TPDFPages.Create(const ADocument: TPDFDocument);
begin
  inherited Create(ADocument);
  FPageClass := TPDFPage;
end;

destructor TPDFPages.Destroy;
begin
  FreeAndNil(FList);
  inherited Destroy;
end;

function TPDFPages.AddPage: TPDFPage;
begin
  if (FList=Nil) then
    FList:=TFPObjectList.Create;
  Result := PageClass.Create(Document);
  FList.Add(Result);
end;

procedure TPDFPages.Add(APage: TPDFPage);
begin
  if (FList = nil) then
    FList := TFPObjectList.Create;
  FList.Add(APage);
end;

{ TPDFAnnot }

constructor TPDFAnnot.Create(const ADocument: TPDFDocument);
begin
  inherited Create(ADocument);
end;

constructor TPDFAnnot.Create(const ADocument: TPDFDocument; const ALeft, ABottom, AWidth, AHeight: TPDFFloat;
  const AURI: String; const ABorder: Boolean);
begin
  Create(ADocument);
  FLeft := ALeft;
  FBottom := ABottom;
  FWidth := AWidth;
  FHeight := AHeight;
  FURI := AURI;
  FBorder := ABorder;
end;

{ TPDFAnnotList }

procedure TPDFAnnotList.CheckList;
begin
  if (FList = nil) then
    FList := TFPObjectList.Create;
end;

function TPDFAnnotList.GetAnnot(AIndex: integer): TPDFAnnot;
begin
  if Assigned(FList) then
    Result := TPDFAnnot(FList[AIndex])
  else
    raise EListError.CreateFmt(rsErrInvalidAnnotIndex, [AIndex]);
end;

destructor TPDFAnnotList.Destroy;
begin
  FreeAndNil(FList);
  inherited Destroy;
end;

function TPDFAnnotList.AddAnnot: TPDFAnnot;
begin
  CheckList;
  Result := TPDFAnnot.Create(Document);
  FList.Add(Result);
end;

function TPDFAnnotList.Count: integer;
begin
  if Assigned(FList) then
    result := FList.Count
  else
    result := 0;
end;

procedure TPDFAnnotList.Add(AAnnot: TPDFAnnot);
begin
  CheckList;
  FList.Add(AAnnot);
end;


{ TPDFPage }

function TPDFPage.GetO(AIndex : Integer): TPDFObject;
begin
  Result:=TPDFObject(FObjects[AIndex]);
end;

function TPDFPage.GetObjectCount: Integer;
begin
  if FObjects=Nil then
    Result:=0
  else
    Result:=FObjects.Count;
end;

function TPDFPage.CreateAnnotList: TPDFAnnotList;
begin
  result := TPDFAnnotList.Create(Document);
end;

procedure TPDFPage.SetOrientation(AValue: TPDFPaperOrientation);
begin
  if FOrientation=AValue then Exit;
  FOrientation:=AValue;
  CalcPaperSize;
  AdjustMatrix;
end;

procedure TPDFPage.CalcPaperSize;
var
  PP: TPDFPaper;
  O1, O2: Integer;
begin
  if PaperType = ptCustom then
    Exit;
  O1 := 0;
  O2 := 1;
  if Orientation = ppoLandScape then
  begin
    O1 := 1;
    O2 := 0;
  end;
  PP.H:=PDFPaperSizes[PaperType][O1];
  PP.W:=PDFPaperSizes[PaperType][O2];
  PP.Printable.T:=PDFPaperPrintables[PaperType][O1];
  PP.Printable.L:=PDFPaperPrintables[PaperType][O2];
  PP.Printable.R:=PDFPaperPrintables[PaperType][2+O1];
  PP.Printable.B:=PDFPaperPrintables[PaperType][2+O2];
  Paper:=PP;
end;

procedure TPDFPage.SetPaperType(AValue: TPDFPaperType);
begin
  if FPaperType=AValue then Exit;
  FPaperType:=AValue;
  CalcPaperSize;
  AdjustMatrix;
end;

procedure TPDFPage.AddTextToLookupLists(AText: UTF8String);
var
  str: UnicodeString;
begin
  if AText = '' then
    Exit;
  str := UTF8Decode(AText);
  Document.Fonts[FLastFont.FontIndex].AddTextToMappingList(str);
end;

procedure TPDFPage.DoUnitConversion(var APoint: TPDFCoord);
begin
  case FUnitOfMeasure of
    uomMillimeters:
      begin
        APoint.X := mmToPDF(APoint.X);
        APoint.Y := mmToPDF(APoint.Y);
      end;
    uomCentimeters:
      begin
        APoint.X := cmToPDF(APoint.X);
        APoint.Y := cmToPDF(APoint.Y);
      end;
    uomInches:
      begin
        APoint.X := InchesToPDF(APoint.X);
        APoint.Y := InchesToPDF(APoint.Y);
      end;
  end;
end;

procedure TPDFPage.CreateStdFontText(X, Y: TPDFFloat; AText: AnsiString; const AFont: TPDFEmbeddedFont;
  const ADegrees: single; const AUnderline: boolean; const AStrikethrough: boolean);
var
  T: TPDFText;
begin
  T := Document.CreateText(X, Y, AText, AFont, ADegrees, AUnderline, AStrikeThrough);
  AddObject(T);
end;

procedure TPDFPage.CreateTTFFontText(X, Y: TPDFFloat; AText: UTF8String; const AFont: TPDFEmbeddedFont;
  const ADegrees: single; const AUnderline: boolean; const AStrikethrough: boolean);
var
  T: TPDFUTF8Text;
begin
  AddTextToLookupLists(AText);
  T := Document.CreateText(X, Y, AText, AFont, ADegrees, AUnderline, AStrikeThrough);
  AddObject(T);
end;

procedure TPDFPage.SetUnitOfMeasure(AValue: TPDFUnitOfMeasure);
begin
  if FUnitOfMeasure = AValue then
    Exit;
  FUnitOfMeasure := AValue;
  AdjustMatrix;
end;

procedure TPDFPage.AdjustMatrix;
begin
  if poPageOriginAtTop in Document.Options then
  begin
    FMatrix._11 := -1;
    FMatrix._21 := GetPaperHeight;
  end
  else
  begin
    FMatrix._11 := 1;
    FMatrix._21 := 0;
  end;
end;

constructor TPDFPage.Create(const ADocument: TPDFDocument);
begin
  inherited Create(ADocument);
  FLastFont := nil;
  FLastFontColor := clBlack;
  FPaperType := ptA4;
  FUnitOfMeasure := uomMillimeters;
  CalcPaperSize;
  If Assigned(ADocument) then
  begin
    PaperType := ADocument.DefaultPaperType;
    Orientation := ADocument.DefaultOrientation;
    FUnitOfMeasure:=ADocument.DefaultUnitOfMeasure;
  end;

  FMatrix._00 := 1;
  FMatrix._20 := 0;
  AdjustMatrix;

  FAnnots := CreateAnnotList;
end;

destructor TPDFPage.Destroy;
begin
  FreeAndNil(FObjects);
  FreeAndNil(FAnnots);
  inherited Destroy;
end;

procedure TPDFPage.AddObject(AObject: TPDFObject);
begin
  if FObjects=Nil then
    FObjects:=TObjectList.Create;
  FObjects.Add(AObject);
end;

procedure TPDFPage.SetFont(AFontIndex: Integer; AFontSize: Integer);
Var
  F : TPDFEmbeddedFont;
begin
  F:=Document.CreateEmbeddedFont(self, AFontIndex, AFontSize);
  AddObject(F);
  FLastFont := F;
end;

procedure TPDFPage.SetColor(AColor: TARGBColor; AStroke : Boolean = True);
Var
  C : TPDFColor;
begin
  C:=Document.CreateColor(AColor,AStroke);
  if not AStroke then
    FLastFontColor := AColor;
  AddObject(C);
end;

procedure TPDFPage.SetPenStyle(AStyle: TPDFPenStyle; const ALineWidth: TPDFFloat);
Var
  L : TPDFLineStyle;
begin
  L:=Document.CreateLineStyle(AStyle, ALineWidth);
  AddObject(L);
end;

procedure TPDFPage.SetLineStyle(AIndex: Integer; AStroke : Boolean = True);
begin
  SetLineStyle(Document.LineStyles[Aindex],AStroke);
end;

procedure TPDFPage.SetLineStyle(S: TPDFLineStyleDef; AStroke: Boolean = True);
begin
  SetColor(S.Color,AStroke);
  SetPenStyle(S.PenStyle,S.LineWidth);
end;

procedure TPDFPage.WriteText(X, Y: TPDFFloat; AText: UTF8String; const ADegrees: single;
    const AUnderline: boolean; const AStrikethrough: boolean);
var
  p: TPDFCoord;
begin
  if not Assigned(FLastFont) then
    raise EPDF.Create(rsErrNoFontDefined);
  p := Matrix.Transform(X, Y);
  DoUnitConversion(p);
  if Document.Fonts[FLastFont.FontIndex].IsStdFont then
    CreateStdFontText(p.X, p.Y, AText, FLastFont, ADegrees, AUnderline, AStrikeThrough)
  else
    CreateTTFFontText(p.X, p.Y, AText, FLastFont, ADegrees, AUnderline, AStrikeThrough);
end;

procedure TPDFPage.WriteText(APos: TPDFCoord; AText: UTF8String; const ADegrees: single;
    const AUnderline: boolean; const AStrikethrough: boolean);
begin
  WriteText(APos.X, APos.Y, AText, ADegrees, AUnderline, AStrikeThrough);
end;

procedure TPDFPage.DrawLine(X1, Y1, X2, Y2, ALineWidth: TPDFFloat; const AStroke: Boolean = True);
var
  L : TPDFLineSegment;
  p1, p2: TPDFCoord;
begin
  p1 := Matrix.Transform(X1, Y1);
  p2 := Matrix.Transform(X2, Y2);
  DoUnitConversion(p1);
  DoUnitConversion(p2);
  L := TPDFLineSegment.Create(Document, ALineWidth, p1.X, p1.Y, p2.X, p2.Y, AStroke);
  AddObject(L);
end;

procedure TPDFPage.DrawLine(APos1, APos2: TPDFCoord; ALineWidth: TPDFFloat;
  const AStroke: Boolean);
begin
  DrawLine(APos1.X, APos1.Y, APos2.X, APos2.Y, ALineWidth, AStroke);
end;

procedure TPDFPage.DrawLineStyle(X1, Y1, X2, Y2: TPDFFloat; AStyle: Integer);
var
  S: TPDFLineStyleDef;
begin
  S := Document.LineStyles[AStyle];
  SetLineStyle(S);
  DrawLine(X1, Y1, X2, Y2, S.LineWidth);
end;

procedure TPDFPage.DrawLineStyle(APos1, APos2: TPDFCoord; AStyle: Integer);
begin
  DrawLineStyle(APos1.X, APos1.Y, APos2.X, APos2.Y, AStyle);
end;

procedure TPDFPage.DrawRect(const X, Y, W, H, ALineWidth: TPDFFloat; const AFill, AStroke: Boolean;
  const ADegrees: single);
var
  R: TPDFRectangle;
  p1, p2: TPDFCoord;
  t1, t2, t3: string;
  rad: single;
begin
  p1 := Matrix.Transform(X, Y);
  DoUnitConversion(p1);
  p2.X := W;
  p2.Y := H;
  DoUnitConversion(p2);

  if ADegrees <> 0.0 then
  begin
    rad := DegToRad(-ADegrees);
    t1 := FormatFloat('0.###;;0', Cos(rad));
    t2 := FormatFloat('0.###;;0', -Sin(rad));
    t3 := FormatFloat('0.###;;0', Sin(rad));
    AddObject(TPDFPushGraphicsStack.Create(Document));
    // PDF v1.3 page 132 & 143
    AddObject(TPDFFreeFormString.Create(Document, Format('%s %s %s %s %.4f %.4f cm', [t1, t2, t3, t1, p1.X, p1.Y]) + CRLF));
    // co-ordinates are now based on the newly transformed matrix co-ordinates.
    R := Document.CreateRectangle(0, 0, p2.X, p2.Y, ALineWidth, AFill, AStroke);
  end
  else
    R := Document.CreateRectangle(p1.X, p1.Y, p2.X, p2.Y, ALineWidth, AFill, AStroke);

  AddObject(R);

  if ADegrees <> 0.0 then
    AddObject(TPDFPopGraphicsStack.Create(Document));
end;

procedure TPDFPage.DrawRect(const APos: TPDFCoord; const W, H, ALineWidth: TPDFFloat; const AFill, AStroke: Boolean;
  const ADegrees: single);
begin
  DrawRect(APos.X, APos.Y, W, H, ALineWidth, AFill, AStroke, ADegrees);
end;

procedure TPDFPage.DrawRoundedRect(const X, Y, W, H, ARadius, ALineWidth: TPDFFloat; const AFill, AStroke: Boolean;
  const ADegrees: single);
var
  R: TPDFRoundedRectangle;
  p1, p2, p3: TPDFCoord;
  t1, t2, t3: string;
  rad: single;
begin
  p1 := Matrix.Transform(X, Y);
  DoUnitConversion(p1);
  p2.X := W;
  p2.Y := H;
  DoUnitConversion(p2);
  p3.X := ARadius;
  p3.Y := 0;
  DoUnitConversion(p3);
  if ADegrees <> 0.0 then
  begin
    rad := DegToRad(-ADegrees);
    t1 := FormatFloat('0.###;;0', Cos(rad));
    t2 := FormatFloat('0.###;;0', -Sin(rad));
    t3 := FormatFloat('0.###;;0', Sin(rad));
    AddObject(TPDFPushGraphicsStack.Create(Document));
    // PDF v1.3 page 132 & 143
    AddObject(TPDFFreeFormString.Create(Document, Format('%s %s %s %s %.4f %.4f cm', [t1, t2, t3, t1, p1.X, p1.Y]) + CRLF));
    // co-ordinates are now based on the newly transformed matrix co-ordinates.
    R := Document.CreateRoundedRectangle(0, 0, p2.X, p2.Y, p3.X, ALineWidth, AFill, AStroke);
  end
  else
    R := Document.CreateRoundedRectangle(p1.X, p1.Y, p2.X, p2.Y, p3.X, ALineWidth, AFill, AStroke);

  AddObject(R);

  if ADegrees <> 0.0 then
    AddObject(TPDFPopGraphicsStack.Create(Document));
end;

procedure TPDFPage.DrawImageRawSize(const X, Y: TPDFFloat; const APixelWidth, APixelHeight, ANumber: integer;
  const ADegrees: single);
var
  p1: TPDFCoord;
  t1, t2, t3: string;
  rad: single;
begin
  p1 := Matrix.Transform(X, Y);
  DoUnitConversion(p1);
  if ADegrees <> 0.0 then
  begin
    rad := DegToRad(-ADegrees);
    t1 := FormatFloat('0.###;;0', Cos(rad));
    t2 := FormatFloat('0.###;;0', -Sin(rad));
    t3 := FormatFloat('0.###;;0', Sin(rad));
    AddObject(TPDFPushGraphicsStack.Create(Document));
    // PDF v1.3 page 132 & 143
    AddObject(TPDFFreeFormString.Create(Document, Format('%s %s %s %s %.4f %.4f cm', [t1, t2, t3, t1, p1.X, p1.Y]) + CRLF));
    // co-ordinates are now based on the newly transformed matrix co-ordinates.
    AddObject(Document.CreateImage(0, 0, APixelWidth, APixelHeight, ANumber));
  end
  else
    AddObject(Document.CreateImage(p1.X, p1.Y, APixelWidth, APixelHeight, ANumber));

  if ADegrees <> 0.0 then
    AddObject(TPDFPopGraphicsStack.Create(Document));
end;

procedure TPDFPage.DrawImageRawSize(const APos: TPDFCoord; const APixelWidth, APixelHeight, ANumber: integer;
  const ADegrees: single);
begin
  DrawImage(APos.X, APos.Y, APixelWidth, APixelHeight, ANumber, ADegrees);
end;

procedure TPDFPage.DrawImage(const X, Y: TPDFFloat; const AWidth, AHeight: TPDFFloat; const ANumber: integer;
  const ADegrees: single);
var
  p1, p2: TPDFCoord;
  t1, t2, t3: string;
  rad: single;
begin
  p1 := Matrix.Transform(X, Y);
  DoUnitConversion(p1);
  p2.X := AWidth;
  p2.Y := AHeight;
  DoUnitConversion(p2);

  if ADegrees <> 0.0 then
  begin
    rad := DegToRad(-ADegrees);
    t1 := FormatFloat('0.###;;0', Cos(rad));
    t2 := FormatFloat('0.###;;0', -Sin(rad));
    t3 := FormatFloat('0.###;;0', Sin(rad));
    AddObject(TPDFPushGraphicsStack.Create(Document));
    // PDF v1.3 page 132 & 143
    AddObject(TPDFFreeFormString.Create(Document, Format('%s %s %s %s %.4f %.4f cm', [t1, t2, t3, t1, p1.X, p1.Y]) + CRLF));
    // co-ordinates are now based on the newly transformed matrix co-ordinates.
    AddObject(Document.CreateImage(0, 0, p2.X, p2.Y, ANumber));
  end
  else
    AddObject(Document.CreateImage(p1.X, p1.Y, p2.X, p2.Y, ANumber));

  if ADegrees <> 0.0 then
    AddObject(TPDFPopGraphicsStack.Create(Document));
end;

procedure TPDFPage.DrawImage(const APos: TPDFCoord; const AWidth, AHeight: TPDFFloat; const ANumber: integer;
  const ADegrees: single);
begin
  DrawImage(APos.X, APos.Y, AWidth, AHeight, ANumber, ADegrees);
end;

procedure TPDFPage.DrawEllipse(const APosX, APosY, AWidth, AHeight, ALineWidth: TPDFFloat; const AFill: Boolean;
  AStroke: Boolean; const ADegrees: single);
var
  p1, p2: TPDFCoord;
  t1, t2, t3: string;
  rad: single;
begin
  p1 := Matrix.Transform(APosX, APosY);
  DoUnitConversion(p1);
  p2.X := AWidth;
  p2.Y := AHeight;
  DoUnitConversion(p2);

  if ADegrees <> 0.0 then
  begin
    rad := DegToRad(-ADegrees);
    t1 := FormatFloat('0.###;;0', Cos(rad));
    t2 := FormatFloat('0.###;;0', -Sin(rad));
    t3 := FormatFloat('0.###;;0', Sin(rad));
    AddObject(TPDFPushGraphicsStack.Create(Document));
    // PDF v1.3 page 132 & 143
    AddObject(TPDFFreeFormString.Create(Document, Format('%s %s %s %s %.4f %.4f cm', [t1, t2, t3, t1, p1.X, p1.Y]) + CRLF));
    // co-ordinates are now based on the newly transformed matrix co-ordinates.
    AddObject(TPDFEllipse.Create(Document, 0, 0, p2.X, p2.Y, ALineWidth, AFill, AStroke));
  end
  else
    AddObject(TPDFEllipse.Create(Document, p1.X, p1.Y, p2.X, p2.Y, ALineWidth, AFill, AStroke));

  if ADegrees <> 0.0 then
    AddObject(TPDFPopGraphicsStack.Create(Document));
end;

procedure TPDFPage.DrawEllipse(const APos: TPDFCoord; const AWidth, AHeight, ALineWidth: TPDFFloat;
  const AFill: Boolean; AStroke: Boolean; const ADegrees: single);
begin
  DrawEllipse(APos.X, APos.Y, AWidth, AHeight, ALineWidth, AFill, AStroke, ADegrees);
end;

procedure TPDFPage.DrawPolygon(const APoints: array of TPDFCoord; const ALineWidth: TPDFFloat);
begin
  DrawPolyLine(APoints, ALineWidth);
  ClosePath;
end;

procedure TPDFPage.DrawPolyLine(const APoints: array of TPDFCoord; const ALineWidth: TPDFFloat);
var
  i: integer;
begin
  if Length(APoints) < 2 then
    Exit; { not enough points to draw a line. Should this raise an exception? }
  MoveTo(APoints[0].X, APoints[0].Y);
  for i := Low(APoints)+1 to High(APoints) do
    DrawLine(APoints[i-1].X, APoints[i-1].Y, APoints[i].X, APoints[i].Y, ALineWidth, False);
end;

procedure TPDFPage.ResetPath;
begin
  AddObject(TPDFResetPath.Create(Document));
end;

procedure TPDFPage.ClosePath;
begin
  AddObject(TPDFClosePath.Create(Document));
end;

procedure TPDFPage.ClosePathStroke;
begin
  AddObject(TPDFFreeFormString.Create(Document, 's'+CRLF));
end;

procedure TPDFPage.StrokePath;
begin
  AddObject(TPDFStrokePath.Create(Document));
end;

procedure TPDFPage.FillStrokePath;
begin
  AddObject(TPDFFreeFormString.Create(Document, 'B'+CRLF));
end;

procedure TPDFPage.FillEvenOddStrokePath;
begin
  AddObject(TPDFFreeFormString.Create(Document, 'B*'+CRLF));
end;

procedure TPDFPage.MoveTo(x, y: TPDFFloat);
var
  p1: TPDFCoord;
begin
  p1 := Matrix.Transform(x, y);
  DoUnitConversion(p1);
  AddObject(TPDFMoveTo.Create(Document, p1.x, p1.y));
end;

procedure TPDFPage.MoveTo(APos: TPDFCoord);
begin
  MoveTo(APos.X, APos.Y);
end;

procedure TPDFPage.CubicCurveTo(const xCtrl1, yCtrl1, xCtrl2, yCtrl2, xTo, yTo, ALineWidth: TPDFFloat; AStroke: Boolean);
var
  p1, p2, p3: TPDFCoord;
begin
  p1 := Matrix.Transform(xCtrl1, yCtrl1);
  DoUnitConversion(p1);
  p2 := Matrix.Transform(xCtrl2, yCtrl2);
  DoUnitConversion(p2);
  p3 := Matrix.Transform(xTo, yTo);
  DoUnitConversion(p3);
  AddObject(TPDFCurveC.Create(Document, p1.x, p1.y, p2.x, p2.y, p3.x, p3.y, ALineWidth, AStroke));
end;

procedure TPDFPage.CubicCurveTo(ACtrl1, ACtrl2, ATo: TPDFCoord; const ALineWidth: TPDFFloat; AStroke: Boolean);
begin
  CubicCurveTo(ACtrl1.X, ACtrl1.Y, ACtrl2.X, ACtrl2.Y, ATo.X, ATo.Y, ALineWidth, AStroke);
end;

procedure TPDFPage.CubicCurveToV(xCtrl2, yCtrl2, xTo, yTo: TPDFFloat; const ALineWidth: TPDFFloat; AStroke: Boolean);
var
  p2, p3: TPDFCoord;
begin
  p2 := Matrix.Transform(xCtrl2, yCtrl2);
  DoUnitConversion(p2);
  p3 := Matrix.Transform(xTo, yTo);
  DoUnitConversion(p3);
  AddObject(TPDFCurveV.Create(Document, p2.x, p2.y, p3.x, p3.y, ALineWidth, AStroke));
end;

procedure TPDFPage.CubicCurveToV(ACtrl2, ATo: TPDFCoord; const ALineWidth: TPDFFloat; AStroke: Boolean);
begin
  CubicCurveToV(ACtrl2.X, ACtrl2.Y, ATo.X, ATo.Y, ALineWidth, AStroke);
end;

procedure TPDFPage.CubicCurveToY(xCtrl1, yCtrl1, xTo, yTo: TPDFFloat; const ALineWidth: TPDFFloat; AStroke: Boolean);
var
  p1, p3: TPDFCoord;
begin
  p1 := Matrix.Transform(xCtrl1, yCtrl1);
  DoUnitConversion(p1);
  p3 := Matrix.Transform(xTo, yTo);
  DoUnitConversion(p3);
  AddObject(TPDFCurveY.Create(Document, p1.x, p1.y, p3.x, p3.y, ALineWidth, AStroke));
end;

procedure TPDFPage.CubicCurveToY(ACtrl1, ATo: TPDFCoord; const ALineWidth: TPDFFloat; AStroke: Boolean);
begin
  CubicCurveToY(ACtrl1.X, ACtrl1.Y, ATo.X, ATo.Y, ALineWidth, AStroke);
end;

procedure TPDFPage.AddExternalLink(const APosX, APosY, AWidth, AHeight: TPDFFloat;
    const AURI: string; ABorder: boolean);
var
  an: TPDFAnnot;
  p1, p2: TPDFCoord;
begin
  p1 := Matrix.Transform(APosX, APosY);
  DoUnitConversion(p1);
  p2.X := AWidth;
  p2.Y := AHeight;
  DoUnitConversion(p2);
  an := TPDFAnnot.Create(Document, p1.X, p1.Y, p2.X, p2.Y, AURI, ABorder);
  Annots.Add(an);
end;

function TPDFPage.GetPaperHeight: TPDFFloat;
begin
  case FUnitOfMeasure of
    uomMillimeters:
      begin
        Result := PDFtoMM(Paper.H);
      end;
    uomCentimeters:
      begin
        Result := PDFtoCM(Paper.H);
      end;
    uomInches:
      begin
        Result := PDFtoInches(Paper.H);
      end;
    uomPixels:
      begin
        Result := Paper.H;
      end;
  end;
end;

function TPDFPage.HasImages: Boolean;

Var
  I,M : Integer;
begin
  Result:=False;
  M:=ObjectCount;
  I:=0;
  While (Not Result) and (I<M) do
    begin
    Result:=FObjects[i] is TPDFImage;
    Inc(I);
    end;
end;

{ TPDFFontDefs }

function TPDFFontDefs.GetF(AIndex : Integer): TPDFFont;
begin
  Result:=Items[AIndex] as TPDFFont;
end;

function TPDFFontDefs.AddFontDef: TPDFFont;
begin
  Result:=Add as TPDFFont;
end;

{ TPDFSection }

function TPDFSection.GetP(AIndex : Integer): TPDFPage;
begin
  If Assigned(FPages) then
    Result:=TPDFPage(FPages[Aindex])
  else
    Raise EPDF.CreateFmt(rsErrInvalidSectionPage,[AIndex]);
end;

function TPDFSection.GetP: INteger;
begin
  if Assigned(FPages) then
    Result:=FPages.Count
  else
    Result:=0;
end;

destructor TPDFSection.Destroy;
begin
  FreeAndNil(FPages);
  inherited Destroy;
end;

procedure TPDFSection.AddPage(APage: TPDFPage);
begin
  if Not Assigned(FPages) then
    FPages:=TFPList.Create;
  FPages.Add(APage);
end;

{ TPDFSectionList }

function TPDFSectionList.GetS(AIndex : Integer): TPDFSection;
begin
  Result:=Items[AIndex] as TPDFSection
end;

function TPDFSectionList.AddSection: TPDFSection;
begin
  Result:=Add as TPDFSection;
end;

{ TPDFDocumentObject }

constructor TPDFDocumentObject.Create(const ADocument: TPDFDocument);
begin
  inherited Create(ADocument);
  FDocument:=ADocument;
end;

procedure TPDFDocumentObject.SetWidth(AWidth: TPDFFloat; AStream : TStream);

Var
  S : String;
begin
  S:=FloatStr(AWidth)+' w'; // stroke width
  if (S<>Document.CurrentWidth) then
    begin
    WriteString('1 J'+CRLF, AStream); // line cap set to rounded edge
    WriteString(S+CRLF, AStream);
    Document.CurrentWidth:=S;
    end;
end;

class procedure TPDFObject.WriteString(const AValue: RawByteString; AStream: TStream);

Var
  L : Integer;

begin
  L:=Length(AValue);
  if L>0 then
    AStream.Write(AValue[1],L);
end;

// Font=Name-Size:x:y
function ExtractBaseFontName(const AValue: string): string;
var
  FontName, S1, S2: string;
  P : Integer;

begin
  P:=RPos('-', AValue);
  if (P>0) then
    FontName:=Copy(AValue,1,P-1)
  else
    FontName:='';
  P:=Pos(':',AValue); // First attribute
  if (P>0) then
    begin
    S1:=Copy(AValue,P+1,Length(AValue)-P);
    S1:=Upcase(S1[1])+Copy(S1,2,Pred(Length(S1)));
    P:=Pos(':',S1);
    if (P>0) then
      begin
      S2:=Copy(S1,P+1,Length(S1)-P);
      if Length(S2)>0 then
        S2[1]:=Upcase(S2[1]);
      S1:=Copy(S1,1,P-1);
      if Length(S1)>0 then
        S1[1]:=Upcase(S1[1]);
      S1:=S1+S2;
      end;
    S1:='-'+S1;
    end;
  Result:=FontName+S1;
end;

{ TPDFImageItem }

procedure TPDFImageItem.SetImage(AValue: TFPCustomImage);
begin
  if FImage=AValue then Exit;
  FImage:=AValue;
  SetLength(FStreamed,0);
end;

function TPDFImageItem.GetStreamed: TBytes;
begin
  if Length(FStreamed)=0 then
  begin
    if Collection.Owner is TPDFDocument then
      CreateStreamedData(poCompressImages in TPDFDocument(Collection.Owner).Options)
    else
      CreateStreamedData(True);
  end;
  Result:=FStreamed;
end;

function TPDFImageItem.GetHeight: Integer;
begin
  If Assigned(FImage) then
    Result:=FImage.Height
  else
    Result:=FHeight;
end;

function TPDFImageItem.GetWidth: Integer;
begin
  If Assigned(FImage) then
    Result:=FImage.Width
  else
    Result:=FWidth;
end;

procedure TPDFImageItem.SetStreamed(AValue: TBytes);
begin
  If AValue=FStreamed then exit;
  SetLength(FStreamed,0);
  FStreamed:=AValue;
end;

destructor TPDFImageItem.Destroy;
begin
  if FOwnsImage then
    FreeAndNil(FImage);
  inherited Destroy;
end;

procedure TPDFImageItem.CreateStreamedData(AUseCompression: Boolean);
Var
  X,Y : Integer;
  C : TFPColor;
  MS : TMemoryStream;
  Str : TStream;
  CWhite : TFPColor; // white color
begin
  FillMem(@CWhite, SizeOf(CWhite), $FF);
  FWidth:=Image.Width;
  FHeight:=Image.Height;
  Str := nil;
  MS := TMemoryStream.Create;
  try
    if AUseCompression then
      begin
      FCompression := icDeflate;
      Str := Tcompressionstream.create(cldefault, MS)
      end
    else
      begin
      FCompression := icNone;
      Str := MS;
      end;
    for Y:=0 to FHeight-1 do
      for X:=0 to FWidth-1 do
        begin
        C:=Image.Colors[x,y];
        if C.alpha < $FFFF then // remove alpha channel - assume white background
          C := AlphaBlend(CWhite, C);

        Str.WriteByte(C.Red shr 8);
        Str.WriteByte(C.Green shr 8);
        Str.WriteByte(C.Blue shr 8);
        end;
    if Str<>MS then
      Str.Free;
    Str := nil;
    SetLength(FStreamed, MS.Size);
    MS.Position := 0;
    if MS.Size>0 then
      MS.ReadBuffer(FStreamed[0], MS.Size);
  finally
    Str.Free;
    MS.Free;
  end;
end;

function TPDFImageItem.WriteImageStream(AStream: TStream): int64;
var
  Img : TBytes;
begin
  TPDFObject.WriteString(CRLF+'stream'+CRLF,AStream);
  Img:=StreamedData;
  Result:=Length(Img);
  AStream.WriteBuffer(Img[0],Result);
  TPDFObject.WriteString(CRLF, AStream);
  TPDFObject.WriteString('endstream', AStream);
end;

function TPDFImageItem.Equals(AImage: TFPCustomImage): boolean;
var
  x, y: Integer;
begin
  if AImage = nil then
  begin
    Result := False;
    exit;
  end;

  { if dimensions don't match, we know we can exit early }
  Result := (Image.Width = AImage.Width) and (Image.Height = AImage.Height);
  if not Result then
    Exit;

  for x := 0 to Image.Width-1 do
    for y := 0 to Image.Height-1 do
      if Image.Colors[x, y] <> AImage.Colors[x, y] then
      begin
        Result := False;
        Exit;
      end;
end;

{ TPDFImages }

function TPDFImages.GetI(AIndex : Integer): TPDFImageItem;
begin
  Result:=Items[AIndex] as TPDFImageItem;
end;

function TPDFImages.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

function TPDFImages.AddImageItem: TPDFImageItem;
begin
  Result:=Add as TPDFImageItem;
end;

function TPDFImages.AddJPEGStream(const AStream: TStream; Width, Height: Integer
  ): Integer;
Var
  IP : TPDFImageItem;

begin
  IP:=AddImageItem;
  IP.FWidth := Width;
  IP.FHeight := Height;
  IP.FCompression := icJPEG;
  SetLength(IP.FStreamed, AStream.Size-AStream.Position);
  if Length(IP.FStreamed)>0 then
    AStream.ReadBuffer(IP.FStreamed[0], Length(IP.FStreamed));
  Result:=Count-1;
end;

constructor TPDFImages.Create(AOwner: TPDFDocument;
  AItemClass: TCollectionItemClass);
begin
  inherited Create(AItemClass);
  FOwner := AOwner;
end;

function TPDFImages.AddFromFile(const AFileName: String; KeepImage: Boolean): Integer;

  {$IF NOT (FPC_FULLVERSION >= 30101)}
  function FindReaderFromExtension(extension: String): TFPCustomImageReaderClass;
  var
    s: string;
    r: integer;
  begin
    extension := lowercase (extension);
    if (extension <> '') and (extension[1] = '.') then
      system.delete (extension,1,1);
    with ImageHandlers do
      begin
        r := count-1;
        s := extension + ';';
        while (r >= 0) do
          begin
          Result := ImageReader[TypeNames[r]];
          if (pos(s,{$if (FPC_FULLVERSION = 20604)}Extentions{$else}Extensions{$endif}[TypeNames[r]]+';') <> 0) then
            Exit;
          dec (r);
          end;
      end;
    Result := nil;
  end;

  function FindReaderFromFileName(const filename: String): TFPCustomImageReaderClass;
  begin
    Result := FindReaderFromExtension(ExtractFileExt(filename));
  end;
  {$ENDIF}

var
  FS: TFileStream;

begin
  FS := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyNone);
  try
    Result := AddFromStream(FS,
      {$IF (FPC_FULLVERSION >= 30101)}TFPCustomImage.{$ENDIF}FindReaderFromFileName(AFileName), KeepImage);
  finally
    FS.Free;
  end;
end;

function TPDFImages.AddFromStream(const AStream: TStream;
  Handler: TFPCustomImageReaderClass; KeepImage: Boolean): Integer;

Var
  I : TFPMemoryImage;
  IP : TPDFImageItem;
  JPEG : TFPReaderJPEG;
  Reader: TFPCustomImageReader;
  {$IF (FPC_FULLVERSION >= 30101)}
  Size : TPoint;
  {$ELSE}
  startPos: Int64;
  {$ENDIF}

begin
  if (poUseRawJPEG in Owner.Options) and Handler.InheritsFrom(TFPReaderJPEG) then
  begin
    JPEG := TFPReaderJPEG.Create;
    try
      {$IF (FPC_FULLVERSION >= 30101)}
      Size := JPEG.ImageSize(AStream);
      Result := AddJPEGStream(AStream, Size.X, Size.Y);
      {$ELSE}
      I:=TFPMemoryImage.Create(0,0);
      try
        startPos := AStream.Position;
        I.LoadFromStream(AStream, JPEG);
        AStream.Position := startPos;
        Result := AddJPEGStream(AStream, I.Width, I.Height);
      finally
        I.Free;
      end;
      {$ENDIF}
    finally
      JPEG.Free;
    end;
  end else
  begin
    IP:=AddImageItem;
    I:=TFPMemoryImage.Create(0,0);
    if not Assigned(Handler) then
      raise EPDF.Create(rsErrNoImageReader);
    Reader := Handler.Create;
    try
      I.LoadFromStream(AStream, Reader);
    finally
      Reader.Free;
    end;
    IP.Image:=I;
    if Not KeepImage then
      begin
      IP.CreateStreamedData(poCompressImages in Owner.Options);
      IP.FImage:=Nil; // not through property, that would clear the image
      i.Free;
      end;
  end;
  Result:=Count-1;
end;

{ TPDFObject }

constructor TPDFObject.Create(const ADocument: TPDFDocument);
begin
  If Assigned(ADocument) then
    Inc(ADocument.FObjectCount);
end;

{ We opted to use the Str() function instead of FormatFloat(), because it is
  considerably faster. This also works around the problem of locale specific
  DecimalSeparator causing float formatting problems in the generated PDF. }
class function TPDFObject.FloatStr(F: TPDFFloat): String;
begin
  if ((Round(F*100) mod 100)=0) then
    Str(F:4:0,Result)
  else
    Str(F:4:2,Result);
  result := trim(Result);
end;

procedure TPDFObject.Write(const AStream: TStream);
begin
  Assert(AStream<>Nil);
end;

procedure TPDFBoolean.Write(const AStream: TStream);
begin
  if FValue then
    WriteString('true', AStream)
  else
    WriteString('false', AStream);
end;

constructor TPDFBoolean.Create(Const ADocument : TPDFDocument; const AValue: Boolean);
begin
  inherited Create(ADocument);
  FValue:=AValue;
end;

procedure TPDFInteger.Write(const AStream: TStream);
begin
  WriteString(IntToStr(FInt), AStream);
end;

procedure TPDFInteger.Inc;
begin
  system.Inc(FInt);
end;

constructor TPDFInteger.Create(Const ADocument : TPDFDocument; const AValue: integer);
begin
  inherited Create(ADocument);
  FInt:=AValue;
end;


procedure TPDFReference.Write(const AStream: TStream);
begin
  WriteString(IntToStr(FValue)+' 0 R', AStream);
end;

constructor TPDFReference.Create(Const ADocument : TPDFDocument; const AValue: integer);
begin
  inherited Create(ADocument);
  FValue:=AValue;
end;

procedure TPDFName.Write(const AStream: TStream);
begin
  if FName <> '' then
    if Pos('Length1', FName) > 0 then
      WriteString('/Length1', AStream)
    else
    begin
      if FMustEscape then
        WriteString('/'+ConvertCharsToHex, AStream)
      else
        WriteString('/'+FName, AStream);
    end;
end;

constructor TPDFName.Create(const ADocument: TPDFDocument; const AValue: string; const AMustEscape: boolean = True);
begin
  inherited Create(ADocument);
  FName:=AValue;
  FMustEscape := AMustEscape;
end;

function TPDFName.ConvertCharsToHex: string;
var
  s: string;
  i: integer;
  d: integer;
begin
  s := '';
  for i := 1 to Length(Name) do
  begin
    d := Ord(Name[i]);
    if (d < 33) or (d > 126) then
      s := s + '#' + IntToHex(d, 2)
    else
      s := s + Name[i];
  end;
  Result := s;
end;

{ TPDFAbstractString }

function TPDFAbstractString.InsertEscape(const AValue: string): string;
var
  S: string;
begin
  Result:='';
  S:=AValue;
  if Pos('\', S) > 0 then
    S:=AnsiReplaceStr(S, '\', '\\');
  if Pos('(', S) > 0 then
    S:=AnsiReplaceStr(S, '(', '\(');
  if Pos(')', S) > 0 then
    S:=AnsiReplaceStr(S, ')', '\)');
  Result:=S;
end;

{ TPDFString }

procedure TPDFString.Write(const AStream: TStream);
var
  s: AnsiString;
begin
  s := Utf8ToAnsi(FValue);
  WriteString('('+s+')', AStream);
end;

constructor TPDFString.Create(Const ADocument : TPDFDocument; const AValue: string);
begin
  inherited Create(ADocument);
  FValue := AValue;
  if (Pos('(', FValue) > 0) or (Pos(')', FValue) > 0) or (Pos('\', FValue) > 0) then
    FValue := InsertEscape(FValue);
end;

{ TPDFUTF8String }

function TPDFUTF8String.RemapedText: AnsiString;
var
  s: UnicodeString;
begin
  s := UTF8Decode(FValue);
  Result := Document.Fonts[FontIndex].GetGlyphIndices(s);
end;

procedure TPDFUTF8String.Write(const AStream: TStream);
begin
  WriteString('<'+RemapedText+'>', AStream);
end;

constructor TPDFUTF8String.Create(const ADocument: TPDFDocument; const AValue: UTF8String; const AFontIndex: integer);
begin
  inherited Create(ADocument);
  FValue := AValue;
  FFontIndex := AFontIndex;
end;

{ TPDFFreeFormString }

procedure TPDFFreeFormString.Write(const AStream: TStream);
var
  s: AnsiString;
begin
  s := Utf8ToAnsi(FValue);
  WriteString(s, AStream);
end;

constructor TPDFFreeFormString.Create(const ADocument: TPDFDocument; const AValue: string);
begin
  inherited Create(ADocument);
  FValue := AValue;
end;


{ TPDFArray }

procedure TPDFArray.Write(const AStream: TStream);
var
  i: integer;
begin
  WriteString('[', AStream);
  for i:=0 to Pred(FArray.Count) do
    begin
    if i > 0 then
      WriteString(' ', AStream);
    TPDFObject(FArray[i]).Write(AStream);
    end;
  WriteString(']', AStream);
end;

procedure TPDFArray.AddItem(const AValue: TPDFObject);
begin
  FArray.Add(AValue);
end;

procedure TPDFArray.AddIntArray(S: String);

Var
  P : Integer;

begin
  P:=Pos(' ',S);
  while (P>0) do
  begin
    AddItem(Document.CreateInteger(StrToInt(Copy(S,1,Pred(P)))));
    Delete(S,1,P);
    P:=Pos(' ',S);
  end;
  if S <> '' then
    AddItem(Document.CreateInteger(StrToInt(S)));
end;

procedure TPDFArray.AddFreeFormArrayValues(S: string);
begin
  AddItem(TPDFFreeFormString.Create(nil, S));
end;

constructor TPDFArray.Create(const ADocument: TPDFDocument);
begin
  inherited Create(ADocument);
  FArray:=TFPObjectList.Create;
end;

destructor TPDFArray.Destroy;

begin
  // TPDFInteger, TPDFReference, TPDFName
  FreeAndNil(FArray);
  inherited;
end;

procedure TPDFStream.Write(const AStream: TStream);
var
  i: integer;
begin
  for i:=0 to FItems.Count-1 do
    TPDFObject(FItems[i]).Write(AStream);
end;

procedure TPDFStream.AddItem(const AValue: TPDFObject);
begin
  FItems.Add(AValue);
end;

constructor TPDFStream.Create(Const ADocument : TPDFDocument; OwnsObjects : Boolean = True);
begin
  inherited Create(ADocument);
  FItems:=TFPObjectList.Create(OwnsObjects);
end;

destructor TPDFStream.Destroy;
begin
  FreeAndNil(FItems);
  inherited;
end;

function TPDFEmbeddedFont.GetPointSize: integer;
begin
  Result := StrToInt(FTxtSize);
end;

procedure TPDFEmbeddedFont.Write(const AStream: TStream);
begin
  WriteString('/F'+IntToStr(FTxtFont)+' '+FTxtSize+' Tf'+CRLF, AStream);
end;

Class function TPDFEmbeddedFont.WriteEmbeddedFont(const ADocument: TPDFDocument; const Src: TMemoryStream; const AStream: TStream): int64;
var
  PS: int64;
  CompressedStream: TMemoryStream;
begin
  WriteString(CRLF+'stream'+CRLF, AStream);
  PS:=AStream.Position;
  if poCompressFonts in ADocument.Options then
  begin
    CompressedStream := TMemoryStream.Create;
    CompressStream(Src, CompressedStream);
    CompressedStream.Position := 0;
    CompressedStream.SaveToStream(AStream);
    CompressedStream.Free;
  end
  else
  begin
    Src.Position := 0;
    Src.SaveToStream(AStream);
  end;
  Result:=AStream.Position-PS;
  WriteString(CRLF, AStream);
  WriteString('endstream', AStream);
end;

class function TPDFEmbeddedFont.WriteEmbeddedSubsetFont(const ADocument: TPDFDocument;
    const AFontNum: integer; const AOutStream: TStream): int64;
var
  PS: int64;
  CompressedStream: TMemoryStream;
begin
  if ADocument.Fonts[AFontNum].SubsetFont = nil then
    raise Exception.Create('WriteEmbeddedSubsetFont: SubsetFont stream was not initialised.');
  WriteString(CRLF+'stream'+CRLF, AOutStream);
  PS := AOutStream.Position;
  if poCompressFonts in ADocument.Options then
  begin
    CompressedStream := TMemoryStream.Create;
    CompressStream(ADocument.Fonts[AFontNum].SubsetFont, CompressedStream);
    CompressedStream.Position := 0;
    CompressedStream.SaveToStream(AOutStream);
    CompressedStream.Free;
  end
  else
  begin
    ADocument.Fonts[AFontNum].SubsetFont.Position := 0;
    TMemoryStream(ADocument.Fonts[AFontNum].SubsetFont).SaveToStream(AOutStream);
  end;
  Result := AOutStream.Position-PS;

  WriteString(CRLF, AOutStream);
  WriteString('endstream', AOutStream);
end;

constructor TPDFEmbeddedFont.Create(const ADocument: TPDFDocument; const APage: TPDFPage; const AFont: integer;
  const ASize: string);
begin
  inherited Create(ADocument);
  FTxtFont := AFont;
  FTxtSize := ASize;
  FPage := APage;
end;

{ TPDFBaseText }

constructor TPDFBaseText.Create(const ADocument: TPDFDocument);
begin
  inherited Create(ADocument);
  FX := 0.0;
  FY := 0.0;
  FFont := nil;
  FDegrees := 0.0;
  FUnderline := False;
  FColor := clBlack;
  FStrikeThrough := False;
end;

{ TPDFText }

function TPDFText.GetTextWidth: single;
var
  i: integer;
  lWidth: double;
  lFontName: string;
begin
  lFontName := Document.Fonts[Font.FontIndex].Name;
  if not Document.IsStandardPDFFont(lFontName) then
    raise EPDF.CreateFmt(rsErrUnknownStdFont, [lFontName]);

  lWidth := 0;
  for i := 1 to Length(FString.Value) do
    lWidth := lWidth + Document.GetStdFontCharWidthsArray(lFontName)[Ord(FString.Value[i])];
  Result := lWidth * Font.PointSize / 1540;
end;

function TPDFText.GetTextHeight: single;
var
  lFontName: string;
begin
  lFontName := Document.Fonts[Font.FontIndex].Name;
  Result := 0;
  case lFontName of
    'Courier':                 result := FONT_TIMES_COURIER_CAPHEIGHT;
    'Courier-Bold':            result := FONT_TIMES_COURIER_CAPHEIGHT;
    'Courier-Oblique':         result := FONT_TIMES_COURIER_CAPHEIGHT;
    'Courier-BoldOblique':     result := FONT_TIMES_COURIER_CAPHEIGHT;
    'Helvetica':               result := FONT_HELVETICA_ARIAL_CAPHEIGHT;
    'Helvetica-Bold':          result := FONT_HELVETICA_ARIAL_BOLD_CAPHEIGHT;
    'Helvetica-Oblique':       result := FONT_HELVETICA_ARIAL_ITALIC_CAPHEIGHT;
    'Helvetica-BoldOblique':   result := FONT_HELVETICA_ARIAL_BOLD_ITALIC_CAPHEIGHT;
    'Times-Roman':             result := FONT_TIMES_CAPHEIGHT;
    'Times-Bold':              result := FONT_TIMES_BOLD_CAPHEIGHT;
    'Times-Italic':            result := FONT_TIMES_ITALIC_CAPHEIGHT;
    'Times-BoldItalic':        result := FONT_TIMES_BOLD_ITALIC_CAPHEIGHT;
    'Symbol':                  result := 300;
    'ZapfDingbats':            result := 300;
    else
      raise EPDF.CreateFmt(rsErrUnknownStdFont, [lFontName]);
  end;
  Result := Result * Font.PointSize / 1540;
end;

procedure TPDFText.Write(const AStream: TStream);
var
  t1, t2, t3: string;
  rad: single;
  lWidth: single;
  lTextWidthInMM: single;
  lHeight: single;
  lTextHeightInMM: single;
  lColor: string;
  lLineWidth: string;
begin
  inherited Write(AStream);
  WriteString('BT'+CRLF, AStream);
  if Degrees <> 0.0 then
  begin
    rad := DegToRad(-Degrees);
    t1 := FloatStr(Cos(rad));
    t2 := FloatStr(-Sin(rad));
    t3 := FloatStr(Sin(rad));
    WriteString(Format('%s %s %s %s %s %s Tm', [t1, t2, t3, t1, FloatStr(X), FloatStr(Y)]) + CRLF, AStream);
  end
  else
  begin
    WriteString(FloatStr(X)+' '+FloatStr(Y)+' TD'+CRLF, AStream);
  end;
  FString.Write(AStream);
  WriteString(' Tj'+CRLF, AStream);
  WriteString('ET'+CRLF, AStream);

  if (not Underline) and (not StrikeThrough) then
    Exit;

  // result is in Font Units
  lWidth := GetTextWidth;
  lHeight := GetTextHeight;
  { convert the Font Units to Millimeters. This is also because fontcache DPI (default 96) could differ from PDF DPI (72). }
  lTextWidthInMM := (lWidth * cInchToMM) / gTTFontCache.DPI;
  lTextHeightInMM := (lHeight * cInchToMM) / gTTFontCache.DPI;

  if Degrees <> 0.0 then
    // angled text
    WriteString(Format('q %s %s %s %s %s %s cm', [t1, t2, t3, t1, FloatStr(X), FloatStr(Y)]) + CRLF, AStream)
  else
    // horizontal text
    WriteString(Format('q 1 0 0 1 %s %s cm', [FloatStr(X), FloatStr(Y)]) + CRLF, AStream);

  { set up a pen width and stroke color }
  lColor := TPDFColor.Command(True, Color);
  lLineWidth := FloatStr(mmToPDF(lTextHeightInMM / 12)) + ' w ';
  WriteString(lLineWidth + lColor + CRLF, AStream);

  { line segment is relative to matrix translation coordinate, set above }
  if Underline then
    WriteString(Format('0 -1.5 m %s -1.5 l S', [FloatStr(mmToPDF(lTextWidthInMM))]) + CRLF, AStream)
  else
    WriteString(Format('0 %s m %s %0:s l S', [FloatStr(mmToPDF(lTextHeightInMM) / 2), FloatStr(mmToPDF(lTextWidthInMM))]) + CRLF, AStream);

  { restore graphics state to before the translation matrix adjustment }
  WriteString('Q' + CRLF, AStream);
end;

constructor TPDFText.Create(const ADocument: TPDFDocument; const AX, AY: TPDFFloat; const AText: AnsiString;
  const AFont: TPDFEmbeddedFont; const ADegrees: single; const AUnderline: boolean; const AStrikethrough: boolean);
begin
  inherited Create(ADocument);
  X := AX;
  Y := AY;
  Font := AFont;
  Degrees := ADegrees;
  Underline := AUnderline;
  StrikeThrough := AStrikeThrough;
  if Assigned(AFont) and Assigned(AFont.Page) then
    Color := AFont.Page.FLastFontColor;
  FString := ADocument.CreateString(AText);
end;

destructor TPDFText.Destroy;
begin
  FreeAndNil(FString);
  inherited;
end;

{ TPDFUTF8Text }

procedure TPDFUTF8Text.Write(const AStream: TStream);
var
  t1, t2, t3: string;
  rad: single;
  lFC: TFPFontCacheItem;
  lWidth: single;
  lTextWidthInMM: single;
  lHeight: single;
  lTextHeightInMM: single;
  lColor: string;
  lLineWidth: string;
  lDescender: single;
begin
  inherited Write(AStream);
  WriteString('BT'+CRLF, AStream);
  if Degrees <> 0.0 then
  begin
    rad := DegToRad(-Degrees);
    t1 := FloatStr(Cos(rad));
    t2 := FloatStr(-Sin(rad));
    t3 := FloatStr(Sin(rad));
    WriteString(Format('%s %s %s %s %s %s Tm', [t1, t2, t3, t1, FloatStr(X), FloatStr(Y)]) + CRLF, AStream);
  end
  else
  begin
    WriteString(FloatStr(X)+' '+FloatStr(Y)+' TD'+CRLF, AStream);
  end;
  FString.Write(AStream);
  WriteString(' Tj'+CRLF, AStream);
  WriteString('ET'+CRLF, AStream);

  if (not Underline) and (not StrikeThrough) then
    Exit;

  // implement Underline and Strikethrough here
  lFC := gTTFontCache.Find(Document.Fonts[Font.FontIndex].Name);
  if not Assigned(lFC) then
    Exit;  // we can't do anything further

  // result is in Font Units
  lWidth := lFC.TextWidth(FString.Value, Font.PointSize);
  lHeight := lFC.TextHeight(FString.Value, Font.PointSize, lDescender);
  { convert the Font Units to Millimeters. This is also because fontcache DPI (default 96) could differ from PDF DPI (72). }
  lTextWidthInMM := (lWidth * cInchToMM) / gTTFontCache.DPI;
  lTextHeightInMM := (lHeight * cInchToMM) / gTTFontCache.DPI;

  if Degrees <> 0.0 then
    // angled text
    WriteString(Format('q %s %s %s %s %s %s cm', [t1, t2, t3, t1, FloatStr(X), FloatStr(Y)]) + CRLF, AStream)
  else
    // horizontal text
    WriteString(Format('q 1 0 0 1 %s %s cm', [FloatStr(X), FloatStr(Y)]) + CRLF, AStream);

  { set up a pen width and stroke color }
  lColor := TPDFColor.Command(True, Color);
  lLineWidth := FloatStr(mmToPDF(lTextHeightInMM / 12)) + ' w ';
  WriteString(lLineWidth + lColor + CRLF, AStream);

  { line segment is relative to matrix translation coordinate, set above }
  if Underline then
    WriteString(Format('0 -1.5 m %s -1.5 l S', [FloatStr(mmToPDF(lTextWidthInMM))]) + CRLF, AStream)
  else
    WriteString(Format('0 %s m %s %0:s l S', [FloatStr(mmToPDF(lTextHeightInMM) / 2), FloatStr(mmToPDF(lTextWidthInMM))]) + CRLF, AStream);

  { restore graphics state to before the translation matrix adjustment }
  WriteString('Q' + CRLF, AStream);

end;

constructor TPDFUTF8Text.Create(const ADocument: TPDFDocument; const AX, AY: TPDFFloat; const AText: UTF8String;
  const AFont: TPDFEmbeddedFont; const ADegrees: single; const AUnderline: boolean; const AStrikethrough: boolean);
begin
  inherited Create(ADocument);
  X := AX;
  Y := AY;
  Font := AFont;
  Degrees := ADegrees;
  Underline := AUnderline;
  if Assigned(AFont) and Assigned(AFont.Page) then
    Color := AFont.Page.FLastFontColor;
  StrikeThrough := AStrikeThrough;
  FString := ADocument.CreateUTF8String(AText, AFont.FontIndex);
end;

destructor TPDFUTF8Text.Destroy;
begin
  FreeAndNil(FString);
  inherited Destroy;
end;

{ TPDFLineSegment  }

procedure TPDFLineSegment.Write(const AStream: TStream);

begin
  SetWidth(FWidth,AStream);
  if FStroke then
    WriteString(TPDFMoveTo.Command(P1), AStream);
  WriteString(Command(P2),AStream);
  if FStroke then
    WriteString('S'+CRLF, AStream);
end;

class function TPDFLineSegment.Command(APos: TPDFCoord): String;
begin
  Result:=FloatStr(APos.X)+' '+FloatStr(APos.Y)+' l'+CRLF
end;

class function TPDFLineSegment.Command(x1, y1: TPDFFloat): String;
begin
  Result := FloatStr(x1)+' '+FloatStr(y1)+' l'+CRLF
end;

class function TPDFLineSegment.Command(APos1, APos2: TPDFCoord): String;
begin
  Result:=TPDFMoveTo.Command(APos1)+Command(APos2);
end;

constructor TPDFLineSegment.Create(const ADocument: TPDFDocument; const AWidth, X1, Y1, X2, Y2: TPDFFloat;
  const AStroke: Boolean);
begin
  inherited Create(ADocument);
  FWidth:=AWidth;
  P1.X:=X1;
  P1.Y:=Y1;
  P2.X:=X2;
  P2.Y:=Y2;
  FStroke := AStroke;
end;

{ TPDFRectangle }

procedure TPDFRectangle.Write(const AStream: TStream);
begin
  if FStroke then
    SetWidth(FWidth, AStream);
  WriteString(FloatStr(FTopLeft.X)+' '+FloatStr(FTopLeft.Y)+' '+FloatStr(FDimensions.X)+' '+FloatStr(FDimensions.Y)+' re'+CRLF, AStream);
  if FStroke and FFill then
    WriteString('b'+CRLF, AStream)
  else if FFill then
    WriteString('f'+CRLF, AStream)
  else if FStroke then
    WriteString('S'+CRLF, AStream);
  (*
  // should we default to this if no stroking or filling is required?
  else
    WriteString('n'+CRLF, AStream); // see PDF 1.3 Specification document on page 152
  *)
end;

constructor TPDFRectangle.Create(const ADocument: TPDFDocument; const APosX, APosY, AWidth, AHeight,
    ALineWidth: TPDFFloat; const AFill, AStroke: Boolean);
begin
  inherited Create(ADocument);
  FTopLeft.X := APosX;
  FTopLeft.Y := APosY;
  FDimensions.X := AWidth;
  FDimensions.Y := AHeight;
  FWidth := ALineWidth;
  FFill := AFill;
  FStroke := AStroke;
end;

{ TPDFRoundedRectangle }

procedure TPDFRoundedRectangle.Write(const AStream: TStream);
var
  c: TPDFFloat;
  x1, y1, x2, y2: TPDFFloat;
begin
  if FStroke then
    SetWidth(FWidth, AStream);

  // bottom left
  x1 := FBottomLeft.X;
  y1 := FBottomLeft.Y;

  // top right
  x2 := FBottomLeft.X + FDimensions.X;
  y2 := FBottomLeft.Y + FDimensions.Y;

  // radius
  c := FRadius;

  // Starting point is bottom left, then drawing anti-clockwise
  WriteString(TPDFMoveTo.Command(x1+c, y1), AStream);
  WriteString(TPDFLineSegment.Command(x2-c, y1), AStream);

  WriteString(TPDFCurveC.Command(x2-c+BEZIER*c, y1,   x2, y1+c-BEZIER*c,   x2, y1+c), AStream);
  WriteString(TPDFLineSegment.Command(x2, y2-c), AStream);

  WriteString(TPDFCurveC.Command(x2, y2-c+BEZIER*c, x2-c+BEZIER*c, y2, x2-c, y2), AStream);
  WriteString(TPDFLineSegment.Command(x1+c, y2), AStream);

  WriteString(TPDFCurveC.Command(x1+c-BEZIER*c, y2, x1, y2-c+BEZIER*c, x1, y2-c), AStream);
  WriteString(TPDFLineSegment.Command(x1, y1+c), AStream);

  WriteString(TPDFCurveC.Command(x1, y1+c-BEZIER*c, x1+c-BEZIER*c, y1, x1+c, y1), AStream);
  WriteString('h'+CRLF, AStream);

  if FStroke and FFill then
    WriteString('b'+CRLF, AStream)
  else if FFill then
    WriteString('f'+CRLF, AStream)
  else if FStroke then
    WriteString('S'+CRLF, AStream);
end;

constructor TPDFRoundedRectangle.Create(const ADocument: TPDFDocument; const APosX, APosY, AWidth, AHeight, ARadius,
  ALineWidth: TPDFFloat; const AFill, AStroke: Boolean);
begin
  inherited Create(ADocument);
  FBottomLeft.X := APosX;
  FBottomLeft.Y := APosY;
  FDimensions.X := AWidth;
  FDimensions.Y := AHeight;
  FWidth := ALineWidth;
  FFill := AFill;
  FStroke := AStroke;
  FRadius := ARadius;
end;

{ TPDFSurface }

procedure TPDFSurface.Write(const AStream: TStream);
var
  i: integer;
begin
  WriteString(TPDFMoveTo.Command(FPoints[0].X,FPoints[0].Y),AStream);
  for i:=1 to Pred(Length(FPoints)) do
    WriteString(FloatStr( FPoints[i].X)+' '+FloatStr( FPoints[i].Y)+' l'+CRLF, AStream);
  if FClose then
    WriteString('h'+CRLF, AStream);
  if FFill then
    WriteString('f'+CRLF, AStream);
end;

constructor TPDFSurface.Create(Const ADocument : TPDFDocument; const APoints: TPDFCoordArray; AClose : Boolean; AFill : Boolean = True);
begin
  inherited Create(ADocument);
  FPoints:=APoints;
  FClose:=AClose;
  FFill:=AFill;
end;

procedure TPDFImage.Write(const AStream: TStream);
begin
  WriteString(TPDFPushGraphicsStack.Command, AStream);   // save graphics state
  WriteString(FloatStr(FSize.X)+' 0 0 '+FloatStr(FSize.Y)+' '+FloatStr( FPos.X)+' '+FloatStr( FPos.Y)+' cm'+CRLF, AStream);
  WriteString('/I'+IntToStr(FNumber)+' Do'+CRLF, AStream);
  WriteString(TPDFPopGraphicsStack.Command, AStream);   // restore graphics state
end;

constructor TPDFImage.Create(const ADocument: TPDFDocument; const ALeft, ABottom, AWidth, AHeight: TPDFFloat; ANumber: integer);
begin
  inherited Create(ADocument);
  FNumber:=ANumber;
  FPos.X:=ALeft;
  FPos.Y:=ABottom;
  FSize.X:=AWidth;
  FSize.Y:=AHeight;
end;

// Dot = linewidth; Dash = (5 x linewidth); Gap = (3 x linewidth);
procedure TPDFLineStyle.Write(const AStream: TStream);
var
  lMask: string;
  w: TPDFFloat;
begin
  w := FLineWidth;
  case FStyle of
    ppsSolid:
      begin
        lMask := '';
      end;
    ppsDash:
      begin
        lMask := FloatStr(5*w) + ' ' + FloatStr(5*w);
      end;
    ppsDot:
      begin
        lMask := FloatStr(0.8*w) + ' ' + FloatStr(4*w)
      end;
    ppsDashDot:
      begin
        lMask := FloatStr(5*w) + ' ' + FloatStr(3*w) + ' ' + FloatStr(0.8*w) + ' ' + FloatStr(3*w)
      end;
    ppsDashDotDot:
      begin
        lMask := FloatStr(5*w) + ' ' + FloatStr(3*w) + ' ' + FloatStr(0.8*w) + ' ' + FloatStr(3*w) + ' ' + FloatStr(0.8*w) + ' ' + FloatStr(3*w)
      end;
  end;
  WriteString(Format('[%s] %d d'+CRLF,[lMask, FPhase]), AStream);
end;

constructor TPDFLineStyle.Create(const ADocument: TPDFDocument; AStyle: TPDFPenStyle; APhase: integer;
  ALineWidth: TPDFFloat);
begin
  inherited Create(ADocument);
  FStyle := AStyle;
  FPhase := APhase;
  FLineWidth := ALineWidth;
end;

Function ARGBGetRed(AColor : TARGBColor) : Byte;

begin
  Result:=((AColor shr 16) and $FF)
end;

Function ARGBGetGreen(AColor : TARGBColor) : Byte;

begin
  Result:=((AColor shr 8) and $FF)
end;

Function ARGBGetBlue(AColor : TARGBColor) : Byte;

begin
  Result:=AColor and $FF;
end;

Function ARGBGetAlpha(AColor : TARGBColor) : Byte;

begin
  Result:=((AColor shr 24) and $FF)
end;

procedure TPDFColor.Write(const AStream: TStream);
var
  S : String;
begin
  S:=FRed+' '+FGreen+' '+FBlue;
  if FStroke then
    S:=S+' RG'
  else
    S:=S+' rg';
  if (S<>Document.CurrentColor) then
  begin
    WriteString(S+CRLF, AStream);
    Document.CurrentColor:=S;
  end;
end;

class function TPDFColor.Command(const AStroke: boolean; const AColor: TARGBColor): string;
var
  lR, lG, lB: string;
begin
  lR := FloatStr(ARGBGetRed(AColor)/256);
  lG := FloatStr(ARGBGetGreen(AColor)/256);
  lB := FloatStr(ARGBGetBlue(AColor)/256);
  result := lR+' '+lG+' '+lB+' ';
  if AStroke then
    result := result + 'RG'
  else
    result := result + 'rg'
end;

constructor TPDFColor.Create(Const ADocument : TPDFDocument; const AStroke: Boolean; AColor: TARGBColor);
begin
  inherited Create(ADocument);
  FColor := AColor;
  FRed:=FloatStr( ARGBGetRed(AColor)/256);
  FGreen:=FloatStr( ARGBGetGreen(AColor)/256);
  FBlue:=FloatStr( ARGBGetBlue(AColor)/256);
  FStroke:=AStroke;
end;

procedure TPDFDictionaryItem.Write(const AStream: TStream);
begin
  FKey.Write(AStream);
  TPDFObject.WriteString(' ', AStream);
  FObj.Write(AStream);
  TPDFObject.WriteString(CRLF, AStream);
end;

constructor TPDFDictionaryItem.Create(Const ADocument : TPDFDocument;const AKey: string; const AValue: TPDFObject);
begin
  inherited Create(ADocument);
  FKey:=ADocument.CreateName(AKey);
  FObj:=AValue;
end;

destructor TPDFDictionaryItem.Destroy;
begin
  FreeAndNil(FKey);
  // TPDFBoolean,TPDFDictionary,TPDFInteger,TPDFName,TPDFReference,TPDFString,TPDFArray
  FreeAndNil(FObj);
  inherited;
end;

function TPDFDictionary.GetE(AIndex : Integer): TPDFDictionaryItem;
begin
  Result:=TPDFDictionaryItem(FElements[AIndex]);
end;

function TPDFDictionary.GetEC: Integer;
begin
  Result:=FElements.Count;
end;

function TPDFDictionary.GetV(AIndex : Integer): TPDFObject;
begin
  Result:=Elements[AIndex].Value;
end;

procedure TPDFDictionary.AddElement(const AKey: string; const AValue: TPDFObject);
var
  DicElement: TPDFDictionaryItem;
begin
  DicElement:=TPDFDictionaryItem.Create(Document,AKey, AValue);
  FElements.Add(DicElement);
end;

procedure TPDFDictionary.AddName(const AKey, AName: String; const AMustEscape: boolean = True);
begin
  AddElement(AKey,Document.CreateName(AName, AMustEscape));
end;

procedure TPDFDictionary.AddInteger(const AKey: String; AInteger: Integer);
begin
  AddElement(AKey,Document.CreateInteger(AInteger));
end;

procedure TPDFDictionary.AddReference(const AKey: String; AReference: Integer);
begin
  AddElement(AKey,Document.CreateReference(AReference));
end;

procedure TPDFDictionary.AddString(const AKey, AString: String);
begin
  AddElement(AKey,Document.CreateString(AString));
end;

function TPDFDictionary.IndexOfKey(const AValue: string): integer;
var
  i: integer;
begin
  Result:=-1;
  I:=0;
  While (Result=-1) and (I<ElementCount) do
    begin
    if GetE(I).FKey.Name=AValue then
      Result:=I;
    Inc(I);
    end;
end;

procedure TPDFDictionary.Write(const AStream: TStream);
begin
  WriteDictionary(-1,AStream);
end;

procedure TPDFDictionary.WriteDictionary(const AObject: integer; const AStream: TStream);
var
  ISize,i, NumImg, NumFnt, BufSize: integer;
  Value: string;
  M, Buf : TMemoryStream;
  E : TPDFDictionaryItem;
  D : TPDFDictionary;
begin
  if GetE(0).FKey.Name='' then
    GetE(0).Write(AStream)  // write a charwidth array of a font
  else
  begin
    WriteString('<<'+CRLF, AStream);
    for i:=0 to ElementCount-1 do
      GetE(I).Write(AStream);
    NumImg:=-1;
    NumFnt:=-1;
    for i:=0 to ElementCount-1 do
      begin
      E:=GetE(i);
      if AObject > -1 then
        begin
          if (E.FKey.Name='Name') then
          begin
            if (TPDFObject(E.Value) is TPDFName) and (TPDFName(E.Value).Name[1]='I') then
            begin
              NumImg:=StrToInt(Copy(TPDFName(E.Value).Name, 2, Length(TPDFName(E.Value).Name) - 1));
              // write image stream length in xobject dictionary
              ISize:=Length(Document.Images[NumImg].StreamedData);
              D:=Document.GlobalXRefs[AObject].Dict;
              D.AddInteger('Length',ISize);
              LastElement.Write(AStream);
              case Document.Images[NumImg].FCompression of
                icJPEG: WriteString('/Filter /DCTDecode'+CRLF, AStream);
                icDeflate: WriteString('/Filter /FlateDecode'+CRLF, AStream);
              end;
              WriteString('>>', AStream);
              // write image stream in xobject dictionary
              Document.Images[NumImg].WriteImageStream(AStream);
            end;
          end;
          if Pos('Length1', E.FKey.Name) > 0 then
          begin
            Value:=E.FKey.Name;
            NumFnt:=StrToInt(Copy(Value, Succ(Pos(' ', Value)), Length(Value) - Pos(' ', Value)));
            if poSubsetFont in Document.Options then
            begin

              buf := TMemoryStream.Create;
              try
                // write fontfile stream (could be compressed or not) to a temporary buffer so we can get the size
                BufSize := TPDFEmbeddedFont.WriteEmbeddedSubsetFont(Document, NumFnt, Buf);
                Buf.Position := 0;
                // write fontfile stream length in xobject dictionary
                D := Document.GlobalXRefs[AObject].Dict;
                D.AddInteger('Length', BufSize);
                LastElement.Write(AStream);
                WriteString('>>', AStream);
                // write fontfile buffer stream in xobject dictionary
                Buf.SaveToStream(AStream);
              finally
                Buf.Free;
              end;

            end
            else
            begin
              M:=TMemoryStream.Create;
              try
                m.LoadFromFile(Document.FontFiles[NumFnt]);
                Buf := TMemoryStream.Create;
                try
                  // write fontfile stream (could be compressed or not) to a temporary buffer so we can get the size
                  BufSize := TPDFEmbeddedFont.WriteEmbeddedFont(Document, M, Buf);
                  Buf.Position := 0;
                  // write fontfile stream length in xobject dictionary
                  D := Document.GlobalXRefs[AObject].Dict;
                  D.AddInteger('Length', BufSize);
                  LastElement.Write(AStream);
                  WriteString('>>', AStream);
                  // write fontfile buffer stream in xobject dictionary
                  Buf.SaveToStream(AStream);
                finally
                  Buf.Free;
                end;
              finally
                M.Free;
              end;
            end;
          end;
        end;
      end; { for i... }
    end; { if FElement.Count... }
    if (NumImg = -1) and (NumFnt = -1) then
      WriteString('>>', AStream);
end; { if/else }


function TPDFDictionary.LastElement: TPDFDictionaryItem;
begin
  if (ElementCount=0) then
    Result:=Nil
  else
    Result:=GetE(ElementCount-1);
end;

function TPDFDictionary.LastValue: TPDFObject;
Var
  DE : TPDFDictionaryItem;
begin
  DE:=LastElement;
  If Assigned(DE) then
    Result:=DE.Value
  else
    Result:=Nil;
end;

function TPDFDictionary.FindElement(const AKey: String): TPDFDictionaryItem;

Var
  I : integer;

begin
  I:=IndexOfKey(AKey);
  if I=-1 then
    Result:=Nil
  else
    Result:=GetE(I);
end;

function TPDFDictionary.FindValue(const AKey: String): TPDFObject;

Var
  DI : TPDFDictionaryItem;

begin
  DI:=FindElement(AKey);
  if Assigned(DI) then
    Result:=DI.Value
  else
    Result:=Nil;
end;

function TPDFDictionary.ElementByName(const AKey: String): TPDFDictionaryItem;
begin
  Result:=FindElement(AKey);
  If (Result=Nil) then
    Raise EPDF.CreateFmt(rsErrDictElementNotFound,[AKey]);
end;

function TPDFDictionary.ValueByName(const AKey: String): TPDFObject;
begin
  Result:=ElementByName(AKey).Value;
end;

constructor TPDFDictionary.Create(const ADocument: TPDFDocument);
begin
  inherited Create(ADocument);
  FElements:=TFPObjectList.Create;
end;

destructor TPDFDictionary.Destroy;

begin
  FreeAndNil(FElements);
  inherited;
end;

procedure TPDFXRef.Write(const AStream: TStream);
begin
  TPDFObject.WriteString(FormatPDFInt(FOffset, 10)+' '+FormatPDFInt(0, 5)+' n'+CRLF, AStream);
end;

constructor TPDFXRef.Create(Const ADocument : TPDFDocument);
begin
  inherited Create;
  FOffset:=0;
  FDict:=ADocument.CreateDictionary;
  FStream:=nil;
end;

destructor TPDFXRef.Destroy;
begin
  FreeAndNil(FDict);
  FreeAndNil(FStream);
  inherited;
end;

{ TPDFInfos }

constructor TPDFInfos.Create;
begin
  inherited Create;
  FProducer := 'fpGUI Toolkit 1.4';
end;

{ TPDFFontNumBaseObject }

constructor TPDFFontNumBaseObject.Create(const ADocument: TPDFDocument; const AFontNum: integer);
begin
  inherited Create(ADocument);
  FFontNum := AFontNum;
end;

{ TPDFToUnicode }

procedure TPDFToUnicode.Write(const AStream: TStream);
var
  lst: TTextMappingList;
  i: integer;
begin
  lst := Document.Fonts[FontNum].TextMapping;

  WriteString('/CIDInit /ProcSet findresource begin'+CRLF, AStream);
  WriteString('12 dict begin'+CRLF, AStream);
  WriteString('begincmap'+CRLF, AStream);
  WriteString('/CIDSystemInfo'+CRLF, AStream);
  WriteString('<</Registry (Adobe)'+CRLF, AStream);

  if poSubsetFont in Document.Options then
    WriteString('/Ordering (UCS)'+CRLF, AStream)
  else
    WriteString('/Ordering (Identity)'+CRLF, AStream);

  WriteString('/Supplement 0'+CRLF, AStream);
  WriteString('>> def'+CRLF, AStream);

  if poSubsetFont in Document.Options then
    WriteString('/CMapName /Adobe-Identity-UCS def'+CRLF, AStream)
  else
    WriteString(Format('/CMapName /%s def', [Document.Fonts[FontNum].FTrueTypeFile.PostScriptName])+CRLF, AStream);

  WriteString('/CMapType 2 def'+CRLF, AStream);  // 2 = ToUnicode

  // ToUnicode always uses 16-bit CIDs
  WriteString('1 begincodespacerange'+CRLF, AStream);
  WriteString('<0000> <FFFF>'+CRLF, AStream);
  WriteString('endcodespacerange'+CRLF, AStream);

  if poSubsetFont in Document.Options then
  begin
    { TODO: Future Improvement - We can reduce the entries in the beginbfrange
            by actually using ranges for consecutive numbers.
            eg:
                 <0051> <0053> <006E>
              vs
                 <0051> <0051> <006E>
                 <0052> <0052> <006F>
                 <0053> <0053> <0070>
    }
    // use hex values in the output
    WriteString(Format('%d beginbfrange', [lst.Count-1])+CRLF, AStream);
    for i := 1 to lst.Count-1 do
      WriteString(Format('<%s> <%0:s> <%s>', [IntToHex(lst[i].GlyphID, 4), IntToHex(lst[i].CharID, 4)])+CRLF, AStream);
    WriteString('endbfrange'+CRLF, AStream);
  end
  else
  begin
    WriteString(Format('%d beginbfchar', [lst.Count])+CRLF, AStream);
    for i := 0 to lst.Count-1 do
      WriteString(Format('<%s> <%s>', [IntToHex(lst[i].GlyphID, 4), IntToHex(lst[i].CharID, 4)])+CRLF, AStream);
    WriteString('endbfchar'+CRLF, AStream);
  end;
  WriteString('endcmap'+CRLF, AStream);
  WriteString('CMapName currentdict /CMap defineresource pop'+CRLF, AStream);
  WriteString('end'+CRLF, AStream);
  WriteString('end'+CRLF, AStream);
end;


{ TCIDToGIDMap }

procedure TCIDToGIDMap.Write(const AStream: TStream);
var
  lst: TTextMappingList;
  i: integer;
  cid, gid: uint16;
  ba: TBytes;
  lMaxCharID: integer;
begin
  lst := Document.Fonts[FontNum].TextMapping;
  lst.Sort;
  lMaxCharID := lst.GetMaxCharID;
  SetLength(ba, (lMaxCharID * 2)+1);
  // initialize array to 0's
  for i := 0 to Length(ba)-1 do
    ba[i] := 0;
  for i := 0 to lst.Count-1 do
  begin
    cid := lst[i].GlyphID;
    gid := lst[i].NewGlyphID;

    ba[2*cid] := Hi(gid); // Byte((gid shr 8) and $FF); //Hi(gid);
    ba[(2*cid)+1] := Lo(gid); //Byte(gid and $FF); //Lo(gid);
  end;

  AStream.WriteBuffer(ba[0], Length(ba));
  WriteString(CRLF, AStream);
  SetLength(ba, 0);
end;

{ TPDFCIDSet }

{ CIDSet uses the bits of each byte for optimised storage. }
procedure TPDFCIDSet.Write(const AStream: TStream);
var
  lst: TTextMappingList;
  i: integer;
  cid, gid: uint16;
  ba: TBytes;
  mask: uint8;
  lSize: integer;
begin
  lst := Document.Fonts[FontNum].TextMapping;
  lst.Sort;
  lSize := (lst.GetMaxCharID div 8) + 1;
  SetLength(ba, lSize);
  for i := 0 to lst.Count-1 do
  begin
    cid := lst[i].CharID;
    mask := 1 shl (7 - (cid mod 8));
    if cid = 0 then
      gid := 0
    else
      gid := cid div 8;
    ba[gid] := ba[gid] or mask;
  end;
  AStream.WriteBuffer(ba[0], Length(ba));
  WriteString(CRLF, AStream);
  SetLength(ba, 0);
end;

{ TPDFDocument }

procedure TPDFDocument.SetInfos(AValue: TPDFInfos);
begin
  if FInfos=AValue then Exit;
  FInfos.Assign(AValue);
end;

procedure TPDFDocument.SetLineStyles(AValue: TPDFLineStyleDefs);
begin
  if FLineStyleDefs=AValue then Exit;
  FLineStyleDefs.Assign(AValue);
end;

procedure TPDFDocument.SetFonts(AValue: TPDFFontDefs);
begin
  if FFonts=AValue then Exit;
  FFonts:=AValue;
end;

procedure TPDFDocument.SetFontFiles(AValue: TStrings);
begin
  if FFontFiles=AValue then Exit;
  FFontFiles.Assign(AValue);
end;

function TPDFDocument.GetStdFontCharWidthsArray(const AFontName: string): TPDFFontWidthArray;
begin
  case AFontName of
    'Courier':                 result := FONT_COURIER_FULL;
    'Courier-Bold':            result := FONT_COURIER_FULL;
    'Courier-Oblique':         result := FONT_COURIER_FULL;
    'Courier-BoldOblique':     result := FONT_COURIER_FULL;
    'Helvetica':               result := FONT_HELVETICA_ARIAL;
    'Helvetica-Bold':          result := FONT_HELVETICA_ARIAL_BOLD;
    'Helvetica-Oblique':       result := FONT_HELVETICA_ARIAL_ITALIC;
    'Helvetica-BoldOblique':   result := FONT_HELVETICA_ARIAL_BOLD_ITALIC;
    'Times-Roman':             result := FONT_TIMES;
    'Times-Bold':              result := FONT_TIMES_BOLD;
    'Times-Italic':            result := FONT_TIMES_ITALIC;
    'Times-BoldItalic':        result := FONT_TIMES_BOLD_ITALIC;
    'Symbol':                  result := FONT_SYMBOL;
    'ZapfDingbats':            result := FONT_ZAPFDINGBATS;
    else
      raise EPDF.CreateFmt(rsErrUnknownStdFont, [AFontName]);
  end;
end;

function TPDFDocument.GetX(AIndex : Integer): TPDFXRef;
begin
  Result:=FGlobalXRefs[Aindex] as TPDFXRef;
end;

function TPDFDocument.GetXC: Integer;
begin
  Result:=FGlobalXRefs.Count;
end;

function TPDFDocument.GetTotalAnnotsCount: integer;
var
  i: integer;
begin
  Result := 0;
  for i := 0 to Pages.Count-1 do
    Result := Result + Pages[i].Annots.Count;
end;

function TPDFDocument.GetFontNamePrefix(const AFontNum: Integer): string;
begin
  // TODO: it must be 6 uppercase characters - no numbers!
  Result := 'GRAEA' + Char(65+AFontNum) + '+';
end;

function TPDFDocument.IndexOfGlobalXRef(const AValue: string): integer;
var
  i: integer;
  p : TPDFObject;
begin
  Result:=-1;
  I:=1;
  While (Result=-1) and (I<FGlobalXRefs.Count) do
    begin
    p:=GetX(i).Dict.Elements[0].Value;
    if (p is TPDFName) and (TPDFName(p).Name=AValue) then
      Result:=i;
    Inc(I);
    end;
end;

function TPDFDocument.FindGlobalXRef(const AName: String): TPDFXRef;

Var
  I : Integer;

begin
  I:=IndexOfGlobalXRef(AName);
  if I=-1 then
    Result:=Nil
  else
    Result:=GlobalXRefs[i];
end;

procedure TPDFDocument.WriteXRefTable(const AStream: TStream);
var
  i: integer;
begin
  if FGlobalXRefs.Count > 1 then
    for i:=1 to FGlobalXRefs.Count-1 do
      GetX(i).Write(AStream);
end;

procedure TPDFDocument.WriteObject(const AObject: integer; const AStream: TStream);
var
  M : TMemoryStream;
  MCompressed: TMemoryStream;
  X : TPDFXRef;
  d: integer;
begin
  TPDFObject.WriteString(IntToStr(AObject)+' 0 obj'+CRLF, AStream);
  X:=GlobalXRefs[AObject];
  if X.FStream = nil then
    X.Dict.WriteDictionary(AObject, AStream)
  else
  begin
    CurrentColor := '';
    CurrentWidth := '';

    M := TMemoryStream.Create;
    X.FStream.Write(M);
    d := M.Size;
    X.Dict.AddInteger('Length', M.Size);

    if poCompressText in Options then
    begin
      MCompressed := TMemoryStream.Create;
      CompressStream(M, MCompressed);
      X.Dict.AddName('Filter', 'FlateDecode');
      X.Dict.AddInteger('Length1', MCompressed.Size);
    end;

    X.Dict.Write(AStream);

    // write stream in contents dictionary
    CurrentColor:='';
    CurrentWidth:='';
    TPDFObject.WriteString(CRLF+'stream'+CRLF, AStream);
    if poCompressText in Options then
    begin
      MCompressed.Position := 0;
      MCompressed.SaveToStream(AStream);
      MCompressed.Free;
    end
    else
    begin
      M.Position := 0;
      m.SaveToStream(AStream);
//      X.FStream.Write(AStream);
    end;

    M.Free;
    TPDFObject.WriteString('endstream', AStream);
  end;
  TPDFObject.WriteString(CRLF+'endobj'+CRLF+CRLF, AStream);
end;

procedure TPDFDocument.CreateRefTable;

begin
  FGlobalXRefs:=TFPObjectList.Create;
  FGlobalXRefs.Add(CreateXRef);
end;


procedure TPDFDocument.CreateTrailer;

begin
  FTrailer:=CreateDictionary;
  Trailer.AddInteger('Size',GlobalXRefCount);
end;

function TPDFDocument.CreateCatalogEntry: integer;
var
  CDict: TPDFDictionary;
begin
  CDict:=CreateGlobalXRef.Dict;
  Trailer.AddReference('Root',GlobalXRefCount-1);
  CDict.AddName('Type','Catalog');
  CDict.AddName('PageLayout', PageLayoutNames[FPageLayout]);
  CDict.AddElement('OpenAction', CreateArray);
  Result:=GlobalXRefCount-1;
end;

procedure TPDFDocument.CreateInfoEntry;

var
  IDict: TPDFDictionary;

begin
  IDict:=CreateGlobalXRef.Dict;
  Trailer.AddReference('Info', GLobalXRefCount-1);
  (Trailer.ValueByName('Size') as TPDFInteger).Value:=GLobalXRefCount;
  if Infos.Title <> '' then
    IDict.AddString('Title',Infos.Title);
  if Infos.Author <> '' then
    IDict.AddString('Author',Infos.Author);
  if Infos.ApplicationName <> '' then
    IDict.AddString('Creator',Infos.ApplicationName);
  IDict.AddString('Producer',Infos.Producer);
  IDict.AddString('CreationDate',DateToPdfDate(Infos.CreationDate));
end;

procedure TPDFDocument.CreatePreferencesEntry;

var
  VDict: TPDFDictionary;

begin
  VDict:=CreateGlobalXRef.Dict;
  VDict.AddName('Type', 'ViewerPreferences');
  VDict.AddElement('FitWindow', CreateBoolean(True));
  VDict:=GlobalXRefByName('Catalog').Dict;
  VDict.AddReference('ViewerPreferences',GLobalXRefCount-1);
end;

function TPDFDocument.CreatePagesEntry(Parent: integer): integer;

var
  EDict,ADict: TPDFDictionary;

begin
  EDict:=CreateGlobalXRef.Dict;
  Result:=GLobalXRefCount-1;
  EDict.AddName('Type','Pages');
  EDict.AddElement('Kids',CreateArray);
  EDict.AddInteger('Count',0);
  if Parent=0 then
    GlobalXRefByName('Catalog').Dict.AddReference('Pages',Result)
  else
    begin
    EDict.AddReference('Parent',Parent);
    ADict:=GlobalXRefs[Parent].Dict;
    (ADict.ValueByName('Count') as TPDFInteger).Inc;
    (ADict.ValueByName('Kids') as TPDFArray).AddItem(CreateReference(Result));
    end;
end;

function TPDFDocument.CreatePageEntry(Parent, PageNum: integer): integer;
var
  PDict,ADict: TPDFDictionary;
  Arr : TPDFArray;
  PP : TPDFPage;
begin
  // add xref entry
  PP:=Pages[PageNum];
  PDict:=CreateGlobalXRef.Dict;

  PDict.AddName('Type','Page');
  PDict.AddReference('Parent',Parent);
  ADict:=GlobalXRefs[Parent].Dict;
  (ADict.ValueByName('Count') as TPDFInteger).Inc;
  (ADict.ValueByName('Kids') as TPDFArray).AddItem(CreateReference(GlobalXRefCount-1));
  Arr:=CreateArray;
  Arr.AddItem(CreateInteger(0));
  Arr.AddItem(CreateInteger(0));
  Arr.AddItem(CreateInteger(PP.Paper.W));
  Arr.AddItem(CreateInteger(PP.Paper.H));
  PDict.AddElement('MediaBox',Arr);
  CreateAnnotEntries(PageNum, PDict);
  ADict:=CreateDictionary;
  PDict.AddElement('Resources',ADict);
  Arr:=CreateArray; // procset
  ADict.AddElement('ProcSet',Arr);
  Arr.AddItem(CreateName('PDF'));
  Arr.AddItem(CreateName('Text'));
  Arr.AddItem(CreateName('ImageC'));
  if (Fonts.Count>0) then
    ADict.AddElement('Font',CreateDictionary);
  if PP.HasImages then
    ADict.AddElement('XObject', CreateDictionary);

  Result:=GlobalXRefCount-1;
end;

function TPDFDocument.CreateOutlines: integer;
var
  ODict: TPDFDictionary;

begin
  ODict:=CreateGlobalXRef.Dict;
  ODict.AddName('Type','Outlines');
  ODict.AddInteger('Count',0);
  Result:=GLobalXRefCount-1;
end;

function TPDFDocument.CreateOutlineEntry(Parent, SectNo, PageNo: integer; ATitle: string): integer;
var
  ODict: TPDFDictionary;
  S: String;

begin
  ODict:=CreateGlobalXRef.Dict;
  S:=ATitle;
  if (S='') then
    S:='Section '+IntToStr(SectNo);
  if (PageNo>-1) then
    S:=S+' Page '+IntToStr(PageNo);
  ODict.AddString('Title',S);
  ODict.AddReference('Parent',Parent);
  ODict.AddInteger('Count',0);
  ODict.AddElement('Dest', CreateArray);
  Result:=GLobalXRefCount-1;
end;

procedure TPDFDocument.AddFontNameToPages(const AName: String; ANum: Integer);

Var
  i: integer;
  ADict: TPDFDictionary;

begin
  for i:=1 to GLobalXRefCount-1 do
    begin
    ADict:=GlobalXRefs[i].Dict;
    if (ADict.ElementCount>0) then
      if (ADict.Values[0] is TPDFName) and ((ADict.Values[0] as TPDFName).Name= 'Page') then
        begin
        ADict:=ADict.ValueByName('Resources') as TPDFDictionary;
        ADict:=ADict.ValueByName('Font') as TPDFDictionary;
        ADict.AddReference(AName,ANum);
        end;
    end;
end;

procedure TPDFDocument.CreateStdFont(EmbeddedFontName: string; EmbeddedFontNum: integer);
var
  FDict: TPDFDictionary;
  N: TPDFName;
  lFontXRef: integer;
begin
  lFontXRef := GlobalXRefCount; // will be used a few lines down in AddFontNameToPages()
  // add xref entry
  FDict := CreateGlobalXRef.Dict;
  FDict.AddName('Type', 'Font');
  FDict.AddName('Subtype', 'Type1');
  FDict.AddName('Encoding', 'WinAnsiEncoding');
  FDict.AddInteger('FirstChar', 32);
  FDict.AddInteger('LastChar', 255);
  FDict.AddName('BaseFont', EmbeddedFontName);
  N := CreateName('F'+IntToStr(EmbeddedFontNum));
  FDict.AddElement('Name',N);
  // add font reference to global page dictionary
  AddFontNameToPages(N.Name, lFontXRef);

  FontFiles.Add('');
end;

function TPDFDocument.LoadFont(AFont: TPDFFont): boolean;
var
  lFName: string;
  s: string;
begin
  Result := False;
  if ExtractFilePath(AFont.FontFile) <> '' then
    // assume AFont.FontFile is the full path to the TTF file
    lFName := AFont.FontFile
  else
    // assume it's just a TTF filename
    lFName := IncludeTrailingPathDelimiter(FontDirectory)+AFont.FontFile;

  if FileExists(lFName) then
  begin
    s := LowerCase(ExtractFileExt(lFName));
    Result := (s = '.ttf') or (s = '.otf');
  end
  else
    Raise EPDF.CreateFmt(rsErrReportFontFileMissing, [lFName]);
end;

procedure TPDFDocument.CreateTTFFont(const EmbeddedFontNum: integer);
var
  FDict: TPDFDictionary;
  N: TPDFName;
  Arr: TPDFArray;
  lFontXRef: integer;
begin
  lFontXRef := GlobalXRefCount; // will be used a few lines down in AddFontNameToPages()

  // add xref entry
  FDict := CreateGlobalXRef.Dict;
  FDict.AddName('Type', 'Font');
  FDict.AddName('Subtype', 'Type0');

  if poSubsetFont in Options then
    FDict.AddName('BaseFont', GetFontNamePrefix(EmbeddedFontNum) + Fonts[EmbeddedFontNum].Name)
  else
    FDict.AddName('BaseFont', Fonts[EmbeddedFontNum].Name);

  FDict.AddName('Encoding', 'Identity-H');

  // add name element to font dictionary
  N:=CreateName('F'+IntToStr(EmbeddedFontNum));
  FDict.AddElement('Name',N);
  AddFontNameToPages(N.Name, lFontXRef);

  Arr := CreateArray;
  Arr.AddItem(TPDFReference.Create(self, GlobalXRefCount));
  FDict.AddElement('DescendantFonts', Arr);
  CreateTTFDescendantFont(EmbeddedFontNum);

  if not (poNoEmbeddedFonts in Options) then
  begin
    FDict.AddReference('ToUnicode', GlobalXRefCount);
    CreateToUnicode(EmbeddedFontNum);
  end;
  FontFiles.Add(Fonts[EmbeddedFontNum].FTrueTypeFile.Filename);
end;

procedure TPDFDocument.CreateTTFDescendantFont(const EmbeddedFontNum: integer);
var
  FDict: TPDFDictionary;
  Arr: TPDFArray;
begin
  // add xref entry
  FDict := CreateGlobalXRef.Dict;
  FDict.AddName('Type', 'Font');
  FDict.AddName('Subtype', 'CIDFontType2');
  if poSubsetFont in Options then
    FDict.AddName('BaseFont', GetFontNamePrefix(EmbeddedFontNum) + Fonts[EmbeddedFontNum].Name)
  else
    FDict.AddName('BaseFont', Fonts[EmbeddedFontNum].Name);

  FDict.AddReference('CIDSystemInfo', GlobalXRefCount);
  CreateTTFCIDSystemInfo;

  // add fontdescriptor reference to font dictionary
  FDict.AddReference('FontDescriptor',GlobalXRefCount);
  CreateFontDescriptor(EmbeddedFontNum);

  Arr := CreateArray;
  FDict.AddElement('W',Arr);
  Arr.AddItem(TPDFTrueTypeCharWidths.Create(self, EmbeddedFontNum));

  // TODO: Implement CIDToGIDMap here
  { It's an array of 256*256*2, loop through the CID values (from <xxx> Tj) and if
    CID matches the loop variable, then populate the 2-byte data, otherwise write
    $0 to the two bytes. Then stream the array as a PDF Reference Object and
    use compression (if defined in PDFDocument.Options. }
  if (poSubsetFont in Options) then
  begin
    FDict.AddReference('CIDToGIDMap', CreateCIDToGIDMap(EmbeddedFontNum));
  end;
end;

procedure TPDFDocument.CreateTTFCIDSystemInfo;
var
  FDict: TPDFDictionary;
begin
  FDict := CreateGlobalXRef.Dict;
  FDict.AddString('Registry', 'Adobe');
  FDict.AddString('Ordering', 'Identity');
  FDict.AddInteger('Supplement', 0);
end;

procedure TPDFDocument.CreateTp1Font(const EmbeddedFontNum: integer);
begin
  Assert(EmbeddedFontNum<>-1);
end;

procedure TPDFDocument.CreateFontDescriptor(const EmbeddedFontNum: integer);
var
  Arr: TPDFArray;
  FDict: TPDFDictionary;
begin
  FDict:=CreateGlobalXRef.Dict;
  FDict.AddName('Type', 'FontDescriptor');

  if poSubsetFont in Options then
  begin
    FDict.AddName('FontName', GetFontNamePrefix(EmbeddedFontNum) + Fonts[EmbeddedFontNum].Name);
    FDict.AddInteger('Flags', 4);
  end
  else
  begin
    FDict.AddName('FontName', Fonts[EmbeddedFontNum].Name);
    FDict.AddName('FontFamily', Fonts[EmbeddedFontNum].FTrueTypeFile.FamilyName);
    FDict.AddInteger('Flags', 32);
  end;

  FDict.AddInteger('Ascent', Fonts[EmbeddedFontNum].FTrueTypeFile.Ascender);
  FDict.AddInteger('Descent', Fonts[EmbeddedFontNum].FTrueTypeFile.Descender);
  FDict.AddInteger('CapHeight', Fonts[EmbeddedFontNum].FTrueTypeFile.CapHeight);
  Arr:=CreateArray;
  FDict.AddElement('FontBBox',Arr);
  Arr.AddIntArray(Fonts[EmbeddedFontNum].FTrueTypeFile.BBox);
  FDict.AddInteger('ItalicAngle', trunc(Fonts[EmbeddedFontNum].FTrueTypeFile.ItalicAngle));
  FDict.AddInteger('StemV', Fonts[EmbeddedFontNum].FTrueTypeFile.StemV);
  FDict.AddInteger('MissingWidth', Fonts[EmbeddedFontNum].FTrueTypeFile.MissingWidth);
  if not (poNoEmbeddedFonts in Options) then
  begin
    FDict.AddReference('FontFile2', GlobalXRefCount);
    CreateFontFileEntry(EmbeddedFontNum);

    if poSubsetFont in Options then
    begin
      // todo /CIDSet reference
      FDict.AddReference('CIDSet', GlobalXRefCount);
      CreateCIDSet(EmbeddedFontNum);
    end;
  end;
end;

procedure TPDFDocument.CreateToUnicode(const AFontNum: integer);
var
  lXRef: TPDFXRef;
begin
  lXRef := CreateGlobalXRef;
  lXRef.FStream := CreateStream(True);
  lXRef.FStream.AddItem(TPDFToUnicode.Create(self, AFontNum));
end;

procedure TPDFDocument.CreateFontFileEntry(const AFontNum: integer);
var
  FDict: TPDFDictionary;
begin
  FDict:=CreateGlobalXRef.Dict;
  if poCompressFonts in Options then
    FDict.AddName('Filter','FlateDecode');
  FDict.AddInteger('Length1 '+IntToStr(AFontNum), Fonts[AFontNum].FTrueTypeFile.OriginalSize);
end;

procedure TPDFDocument.CreateCIDSet(const AFontNum: integer);
var
  lXRef: TPDFXRef;
begin
  lXRef := CreateGlobalXRef;
  lXRef.FStream := CreateStream(True);
  lXRef.FStream.AddItem(TPDFCIDSet.Create(self, AFontNum));
end;

procedure TPDFDocument.CreateImageEntry(ImgWidth, ImgHeight, NumImg: integer);
var
  N: TPDFName;
  IDict,ADict: TPDFDictionary;
  i: integer;
  lXRef: integer;
begin
  lXRef := GlobalXRefCount; // reference to be used later

  IDict:=CreateGlobalXRef.Dict;
  IDict.AddName('Type','XObject');
  IDict.AddName('Subtype','Image');
  IDict.AddInteger('Width',ImgWidth);
  IDict.AddInteger('Height',ImgHeight);
  IDict.AddName('ColorSpace','DeviceRGB');
  IDict.AddInteger('BitsPerComponent',8);
  N:=CreateName('I'+IntToStr(NumImg)); // Needed later
  IDict.AddElement('Name',N);

  // now find where we must add the image xref - we are looking for "Resources"
  for i := 1 to GlobalXRefCount-1 do
  begin
    ADict:=GlobalXRefs[i].Dict;
    if ADict.ElementCount > 0 then
    begin
      if (ADict.Values[0] is TPDFName) and ((ADict.Values[0] as TPDFName).Name='Page') then
      begin
        ADict:=ADict.ValueByName('Resources') as TPDFDictionary;
        ADict:=TPDFDictionary(ADict.FindValue('XObject'));
        if Assigned(ADict) then
        begin
          ADict.AddReference(N.Name, lXRef);
        end;
      end;
    end;
  end;
end;

function TPDFDocument.CreateAnnotEntry(const APageNum, AnnotNum: integer): integer;
var
  lDict, ADict: TPDFDictionary;
  an: TPDFAnnot;
  ar: TPDFArray;
  lXRef: TPDFXRef;
  s: string;
begin
  an := Pages[APageNum].Annots[AnnotNum];
  lXRef := CreateGlobalXRef;
  lDict := lXRef.Dict;
  lDict.AddName('Type', 'Annot');
  lDict.AddName('Subtype', 'Link');
  { Invert link on click - PDF 1.3 spec pg.410. It is the default value, but
    some PDF viewers don't apply that if not explicity specified. }
  lDict.AddName('H', 'I');

  { Border array consists of 3 or 4 values. The first three elements describe
    the horizontal corner radius, the vertical corner radius and the border
    width. A 0 border width means no border is drawn. The optional 4th element
    is an array defining a dash pattern. For example:  /Border [16 16 2 [2 1]] }
  ar := CreateArray;
  lDict.AddElement('Border', ar);
  if an.FBorder then
    s := '1'
  else
    s := '0';
  ar.AddFreeFormArrayValues('0 0 ' + s);

  ar := CreateArray;
  lDict.AddElement('Rect', ar);
  s := ar.FloatStr(an.FLeft);
  s := s + ' ' + ar.FloatStr(an.FBottom);
  s := s + ' ' + ar.FloatStr(an.FLeft + an.FWidth);
  s := s + ' ' + ar.FloatStr(an.FBottom + an.FHeight);
  ar.AddFreeFormArrayValues(s);

  ADict := CreateDictionary;
  lDict.AddElement('A', ADict);
  ADict.AddName('Type', 'Action');
  ADict.AddName('S', 'URI');
  ADict.AddString('URI', an.FURI);

  result := GlobalXRefCount-1;
end;

function TPDFDocument.CreateCIDToGIDMap(const AFontNum: integer): integer;
var
  lXRef: TPDFXRef;
begin
  lXRef := CreateGlobalXRef;
  result := GlobalXRefCount-1;

  lXRef.FStream := CreateStream(True);
  lXRef.FStream.AddItem(TCIDToGIDMap.Create(self, AFontNum));
end;

function TPDFDocument.CreateContentsEntry(const APageNum: integer): integer;
var
  Contents: TPDFXRef;
  i: integer;
begin
  Contents:=CreateGlobalXRef;
  Contents.FStream:=CreateStream(False);
  Result:=GlobalXRefCount-1;
  { TODO: This is terrible code. See if we can make a better plan getting hold
          of the reference to the Page dictionary. }
  i := 2 + Pages[APageNum].Annots.Count; // + GetTotalAnnotsCount;
  GlobalXrefs[GlobalXRefCount-i].Dict.AddReference('Contents',Result);
end;

procedure TPDFDocument.CreatePageStream(APage : TPDFPage; PageNum: integer);

var
  i: integer;
  PageStream : TPDFStream;

begin
  PageStream:=GlobalXRefs[PageNum].FStream;
  for i:=0 to APage.ObjectCount-1 do
    begin
    PageStream.AddItem(APage.Objects[i]);
    end;
end;

function TPDFDocument.CreateGlobalXRef: TPDFXRef;
begin
  Result:=Self.CreateXRef;
  AddGlobalXRef(Result);
end;

function TPDFDocument.AddGlobalXRef(AXRef: TPDFXRef): Integer;
begin
  Result:=FGlobalXRefs.Add(AXRef);
end;

function TPDFDocument.GlobalXRefByName(const AName: String): TPDFXRef;
begin
  Result:=FindGlobalXRef(AName);
  if Result=Nil then
    Raise EPDF.CreateFmt(rsErrNoGlobalDict,[AName]);
end;

function TPDFDocument.CreateLineStyles: TPDFLineStyleDefs;
begin
  Result:=TPDFLineStyleDefs.Create(TPDFLineStyleDef);
end;

function TPDFDocument.CreateSectionList: TPDFSectionList;
begin
  Result:=TPDFSectionList.Create(TPDFSection)
end;

function TPDFDocument.CreateFontDefs: TPDFFontDefs;
begin
  Result := TPDFFontDefs.Create(TPDFFont);
end;

function TPDFDocument.CreatePDFInfos: TPDFInfos;
begin
  Result:=TPDFInfos.Create;
end;

function TPDFDocument.CreatePDFImages: TPDFImages;
begin
  Result:=TPDFImages.Create(Self,TPDFImageItem);
end;

function TPDFDocument.CreatePDFPages: TPDFPages;
begin
  Result:=TPDFPages.Create(Self);
end;

constructor TPDFDocument.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  FFontFiles:=TStringList.Create;
  FLineStyleDefs:=CreateLineStyles;
  FSections:=CreateSectionList;
  FFonts:=CreateFontDefs;
  FInfos:=CreatePDFInfos;
  FImages:=CreatePDFImages;
  FPages:=CreatePDFPages;
  FPreferences:=True;
  FPageLayout:=lSingle;
  FDefaultPaperType:=ptA4;
  FDefaultOrientation:=ppoPortrait;
  FZoomValue:='100';
  FOptions := [poCompressFonts, poCompressImages];
  FUnitOfMeasure:=uomMillimeters;
end;

procedure TPDFDocument.StartDocument;

begin
  Reset;
  CreateRefTable;
  CreateTrailer;
  FCatalogue:=CreateCatalogEntry;
  CreateInfoEntry;
  CreatePreferencesEntry;
  if (FontDirectory = '') then
    FontDirectory:=ExtractFilePath(ParamStr(0));
end;

procedure TPDFDocument.Reset;
begin
  FLineStyleDefs.Clear;
  FFonts.Clear;
  FImages.Clear;
  FFontFiles.Clear;
  FreeAndNil(FPages);
  FPages:=CreatePDFPages;
  FreeAndNil(FSections);
  FSections:=CreateSectionList;
end;

destructor TPDFDocument.Destroy;

begin
  FreeAndNil(FLineStyleDefs);
  FreeAndNil(FInfos);
  FreeAndNil(FFonts);
  FreeAndNil(FImages);
  FreeAndNil(FFontFiles);
  FreeAndNil(FPages);
  FreeAndNil(FSections);
  Trailer.Free;
  FGlobalXRefs.Free;
  inherited;
end;

function TPDFDocument.CreateSectionPageOutLine(const S: TPDFSection;
  const PageOutLine, PageIndex, NewPage, ParentOutline, NextOutline,
  PrevOutLine: Integer): Integer; // Return pages ID

Var
  ADict: TPDFDictionary;
  Arr : TPDFArray;

begin
  ADict:=GlobalXRefs[ParentOutline].Dict;
  (ADict.ValueByName('Count') as TPDFInteger).Inc;
  // add page reference to outline destination
  ADict:=GlobalXRefs[PageOutLine].Dict;
  Arr:=(ADict.ValueByName('Dest') as TPDFArray);
  Arr.AddItem(CreateReference(NewPage));
  Arr.AddItem(CreateName('Fit'));
  Result:=PrevOutline;
  if PageIndex=0 then
    begin
    GlobalXRefs[ParentOutline].Dict.AddReference('First',GLobalXRefCount-1);
    Result:=GLobalXRefCount-1;
    // add page reference to parent outline destination
    ADict:=GlobalXRefs[ParentOutline].Dict;
    Arr:=(ADict.ValueByName('Dest') as TPDFArray);
    Arr.AddItem(CreateReference(NewPage));
    Arr.AddItem(CreateName('Fit'));
    end
  else
    begin
    GlobalXRefs[NextOutline].Dict.AddReference('Next',GLobalXRefCount-1);
    GlobalXRefs[PageOutLine].Dict.AddReference('Prev',PrevOutline);
    if (PageIndex<S.PageCount) then
      Result:=GLobalXRefCount-1;
    end;
  if PageIndex=S.PageCount-1 then
    GlobalXRefs[ParentOutline].Dict.AddReference('Last',GLobalXRefCount-1);
end;

function TPDFDocument.CreateSectionOutLine(const SectionIndex, OutLineRoot,
  ParentOutLine, NextSect, PrevSect: Integer): Integer; // Previous section

Var
  ADict: TPDFDictionary;

begin
  Result:=PrevSect;
  ADict:=GlobalXRefs[OutlineRoot].Dict;
  (ADict.ValueByName('Count') as TPDFInteger).Inc;
  if (SectionIndex=0) then
    begin
    GlobalXRefs[OutlineRoot].Dict.AddReference('First',GLobalXRefCount-1);
    Result:=GLobalXRefCount-1;
    end
  else
    begin
    GlobalXRefs[NextSect].Dict.AddReference('Next',GLobalXRefCount-1);
    GlobalXRefs[ParentOutline].Dict.AddReference('Prev', PrevSect);
    if (SectionIndex<Sections.Count-1) then
      Result:=GLobalXRefCount-1;
    end;
  if SectionIndex=Sections.Count-1 then
    GlobalXRefs[OutlineRoot].Dict.AddReference('Last',GLobalXRefCount-1);
end;

function TPDFDocument.CreateSectionsOutLine: Integer; // Parent page ID

var
  pc,j: integer;
  ParentOutline,TreeRoot,OutlineRoot : Integer;
  K,PageNum: integer;
  PageOutline, NextOutline, NextSect, NewPage, PrevOutline, PrevSect: integer;
  ADict: TPDFDictionary;
  Arr : TPDFArray;
  S : TPDFSection;

begin
  Result:=0;
  TreeRoot:=0;
  if (Sections.Count>1) then
    begin
    If (poOutline in Options) then
      begin
      OutlineRoot:=CreateOutlines;
      // add outline reference to catalog dictionary
      GlobalXRefs[Catalogue].Dict.AddReference('Outlines',GLobalXRefCount-1);
      // add useoutline element to catalog dictionary
      GlobalXRefs[Catalogue].Dict.AddName('PageMode','UseOutlines');
      end;
    TreeRoot:=CreatePagesEntry(Result);
    end
  else
    begin
    Result:=CreatePagesEntry(Result);
    TreeRoot:=Result;
    end;

  NextSect:=0;
  PrevSect:=0;
  PrevOutLine:=0;
  NextOutLine:=0;
  for J:=0 to Sections.Count-1 do
    begin
    S:=Sections[J];
    if (poOutline in options) then
      begin
      ParentOutline:=CreateOutlineEntry(OutlineRoot,j+1,-1,S.Title);
      PrevSect:=CreateSectionOutline(J,OutlineRoot,ParentOutLine,NextSect,PrevSect);
      NextSect:=ParentOutline;
      Result:=CreatePagesEntry(TreeRoot);
      end;
    for k:=0 to S.PageCount-1 do
      begin
      with S do
        NewPage:=CreatePageEntry(Result,K);
      // add zoom factor to catalog dictionary
      if (j=0) and (k=0) then
        begin
        ADict:=GlobalXRefByName('Catalog').Dict;
        Arr:=ADict.ValueByName('OpenAction') as TPDFArray;
        Arr.AddItem(CreateReference(GLobalXRefCount-1));
        Arr.AddItem(CreateName('XYZ null null '+TPDFObject.FloatStr(StrToInt(FZoomValue) / 100), False));
        end;
      PageNum:=CreateContentsEntry(k); // pagenum = object number in the pdf file
      CreatePageStream(S.Pages[k],PageNum);
      if (Sections.Count>1) and (poOutline in Options) then
        begin
        PageOutLine:=CreateOutlineEntry(ParentOutline,j+1,k+1,S.Title);
        CreateSectionPageOutLine(S,PageOutLine,K,NewPage,ParentOutLine,NextOutLine,PrevOutLine);
        NextOutline:=PageOutLine;
        end;
    end;
  end;
  // update count in root parent pages dictionary
  ADict:=GlobalXRefs[TreeRoot].Dict;
  Pc:=0;
  For J:=0 to Sections.Count-1 do
    Inc(Pc,Sections[J].PageCount);
  (ADict.ValueByName('Count') as TPDFInteger).Value:=PC;
end;

procedure TPDFDocument.CreateFontEntries;
var
  i: integer;
  NumFont: integer;
  FontName: string;
begin
  // select the font type
  NumFont:=0;
  for i:=0 to Fonts.Count-1 do
  begin
    FontName := Fonts[i].Name;

    if IsStandardPDFFont(FontName) then
      CreateStdFont(FontName, NumFont)
    else if LoadFont(Fonts[i]) then
    begin
      if poSubsetFont in Options then
        Fonts[i].GenerateSubsetFont;
      CreateTtfFont(NumFont);
    end
    else
      CreateTp1Font(NumFont);  // not implemented yet

    Inc(NumFont);
  end;
end;

procedure TPDFDocument.CreateImageEntries;
Var
  I : Integer;
begin
  for i:=0 to Images.Count-1 do
    CreateImageEntry(Images[i].Width,Images[i].Height,i);
end;

procedure TPDFDocument.CreateAnnotEntries(const APageNum: integer; const APageDict: TPDFDictionary);
var
  i: integer;
  refnum: integer;
  ar: TPDFArray;
begin
  if GetTotalAnnotsCount = 0 then
    Exit;
  ar := CreateArray;
  APageDict.AddElement('Annots', ar);
  for i := 0 to Pages[APageNum].Annots.Count-1 do
  begin
    refnum := CreateAnnotEntry(APageNum, i);
    ar.AddItem(CreateReference(refnum));
  end;
end;

procedure TPDFDocument.SaveToStream(const AStream: TStream);
var
  i, XRefPos: integer;
begin
  CreateSectionsOutLine;
  CreateFontEntries;
  CreateImageEntries;
  (Trailer.ValueByName('Size') as TPDFInteger).Value:=GlobalXRefCount;
  AStream.Position:=0;
  TPDFObject.WriteString(PDF_VERSION+CRLF, AStream);
  TPDFObject.WriteString(PDF_BINARY_BLOB+CRLF, AStream); // write some binary data as recommended in PDF Spec section 3.4.1 (File Header)
  // write numbered indirect objects
  for i:=1 to GlobalXRefCount-1 do
    begin
    XRefPos:=AStream.Position;
    WriteObject(i, AStream);
    GlobalXRefs[i].Offset:=XRefPos;
    end;
  XRefPos:=AStream.Position;
  // write xref table
  TPDFObject.WriteString('xref'+CRLF+'0 '+IntToStr(GlobalXRefCount)+CRLF, AStream);
  with GlobalXRefs[0] do
    TPDFObject.WriteString(FormatPDFInt(Offset, 10)+' '+FormatPDFInt(PDF_MAX_GEN_NUM, 5)+' f'+CRLF, AStream);
  WriteXRefTable(AStream);
  // write trailer
  TPDFObject.WriteString('trailer'+CRLF, AStream);
  Trailer.Write(AStream);
  // write offset of last xref table
  TPDFObject.WriteString(CRLF+'startxref'+CRLF+IntToStr(XRefPos)+CRLF, AStream);
  TPDFObject.WriteString(PDF_FILE_END, AStream);
end;

procedure TPDFDocument.SaveToFile(const AFileName: String);

Var
  F : TFileStream;

begin
  F:=TFileStream.Create(AFileName,fmCreate);
  try
    SaveToStream(F);
  finally
    F.Free;
  end;
end;

function TPDFDocument.IsStandardPDFFont(AFontName: string): boolean;
begin
  { Acrobat Reader expects us to be case sensitive. Other PDF viewers are case-insensitive. }
  if (AFontName='Courier') or (AFontName='Courier-Bold') or (AFontName='Courier-Oblique') or (AFontName='Courier-BoldOblique')
      or (AFontName='Helvetica') or (AFontName='Helvetica-Bold') or (AFontName='Helvetica-Oblique') or (AFontName='Helvetica-BoldOblique')
      or (AFontName='Times-Roman') or (AFontName='Times-Bold') or (AFontName='Times-Italic') or (AFontName='Times-BoldItalic')
      or (AFontName='Symbol')
      or (AFontName='ZapfDingbats') then
    Result := True
  else
    Result := False;
end;

function TPDFDocument.CreateEmbeddedFont(const APage: TPDFPage; AFontIndex, AFontSize: Integer): TPDFEmbeddedFont;
begin
  Result:=TPDFEmbeddedFont.Create(Self, APage, AFontIndex, IntToStr(AFontSize))
end;

function TPDFDocument.CreateText(X, Y: TPDFFloat; AText: AnsiString; const AFont: TPDFEmbeddedFont;
  const ADegrees: single; const AUnderline: boolean; const AStrikethrough: boolean): TPDFText;
begin
  Result:=TPDFText.Create(Self, X, Y, AText, AFont, ADegrees, AUnderline, AStrikeThrough);
end;

function TPDFDocument.CreateText(X, Y: TPDFFloat; AText: UTF8String; const AFont: TPDFEmbeddedFont;
  const ADegrees: single; const AUnderline: boolean; const AStrikethrough: boolean): TPDFUTF8Text;
begin
  Result := TPDFUTF8Text.Create(Self, X, Y, AText, AFont, ADegrees, AUnderline, AStrikeThrough);
end;

function TPDFDocument.CreateRectangle(const X,Y,W,H, ALineWidth: TPDFFloat; const AFill, AStroke: Boolean): TPDFRectangle;
begin
  Result:=TPDFRectangle.Create(Self,X,Y,W,H,ALineWidth,AFill, AStroke);
end;

function TPDFDocument.CreateRoundedRectangle(const X, Y, W, H, ARadius, ALineWidth: TPDFFloat;
    const AFill, AStroke: Boolean): TPDFRoundedRectangle;
begin
  Result := TPDFRoundedRectangle.Create(Self, X, Y, W, H, ARadius, ALineWidth, AFill, AStroke);
end;

function TPDFDocument.CreateColor(AColor: TARGBColor; AStroke: Boolean): TPDFColor;
begin
  Result:=TPDFColor.Create(Self,AStroke,AColor);
end;

function TPDFDocument.CreateBoolean(AValue: Boolean): TPDFBoolean;
begin
  Result:=TPDFBoolean.Create(Self,AValue);
end;

function TPDFDocument.CreateInteger(AValue: Integer): TPDFInteger;
begin
  Result:=TPDFInteger.Create(Self,AValue);
end;

function TPDFDocument.CreateReference(AValue: Integer): TPDFReference;
begin
  Result:=TPDFReference.Create(Self,AValue);
end;

function TPDFDocument.CreateString(const AValue: String): TPDFString;
begin
  Result:=TPDFString.Create(Self,AValue);
end;

function TPDFDocument.CreateUTF8String(const AValue: UTF8String; const AFontIndex: integer): TPDFUTF8String;
begin
  Result := TPDFUTF8String.Create(self, AValue, AFontIndex);
end;

function TPDFDocument.CreateLineStyle(APenStyle: TPDFPenStyle; const ALineWidth: TPDFFloat): TPDFLineStyle;
begin
  Result := TPDFLineStyle.Create(Self, APenStyle, 0, ALineWidth);
end;

function TPDFDocument.CreateName(AValue: String; const AMustEscape: boolean = True): TPDFName;
begin
  Result:=TPDFName.Create(Self,AValue,AMustEscape);
end;

function TPDFDocument.CreateStream(OwnsObjects : Boolean = True): TPDFStream;
begin
  Result:=TPDFStream.Create(Self,OwnsObjects);
end;

function TPDFDocument.CreateDictionary: TPDFDictionary;
begin
  Result:=TPDFDictionary.Create(Self);
end;

function TPDFDocument.CreateXRef: TPDFXRef;
begin
  Result:=TPDFXRef.Create(Self);
end;

function TPDFDocument.CreateArray: TPDFArray;
begin
  Result:=TPDFArray.Create(Self);
end;

function TPDFDocument.CreateImage(const ALeft, ABottom, AWidth,
  AHeight: TPDFFloat; ANumber: integer): TPDFImage;
begin
  Result:=TPDFImage.Create(Self,ALeft,ABottom,AWidth,AHeight,ANumber);
end;

function TPDFDocument.AddFont(AName: String): Integer;
var
  F: TPDFFont;
  i: integer;
begin
  { reuse existing font definition if it exists }
  for i := 0 to Fonts.Count-1 do
  begin
    if Fonts[i].Name = AName then
    begin
      Result := i;
      Exit;
    end;
  end;
  F := Fonts.AddFontDef;
  F.Name := AName;
  F.IsStdFont := True;
  Result := Fonts.Count-1;
end;

function TPDFDocument.AddFont(AFontFile: String; AName: String): Integer;
var
  F: TPDFFont;
  i: integer;
  lFName: string;
begin
  { reuse existing font definition if it exists }
  for i := 0 to Fonts.Count-1 do
  begin
    if Fonts[i].Name = AName then
    begin
      Result := i;
      Exit;
    end;
  end;
  F := Fonts.AddFontDef;
  if ExtractFilePath(AFontFile) <> '' then
    // assume AFontFile is the full path to the TTF file
    lFName := AFontFile
  else
    // assume it's just the TTF filename
    lFName := IncludeTrailingPathDelimiter(FontDirectory)+AFontFile;
  F.FontFile := lFName;
  F.Name := AName;
  F.IsStdFont := False;
  Result := Fonts.Count-1;
end;

function TPDFDocument.AddLineStyleDef(ALineWidth: TPDFFloat; AColor: TARGBColor;
  APenStyle: TPDFPenStyle): Integer;

Var
  F : TPDFLineStyleDef;

begin
  F:=FLineStyleDefs.AddLineStyleDef;
  F.LineWidth:=ALineWidth;
  F.Color:=AColor;
  F.PenStyle:=APenStyle;
  Result:=FLineStyleDefs.Count-1;
end;



end.

