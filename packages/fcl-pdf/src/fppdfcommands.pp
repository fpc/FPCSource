unit fppdfcommands;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fppdfconsts, types, fppdfobjects;

Type

{ ----------------------------------------------------------------------
  General types, to be used as parents for specific commands
  ----------------------------------------------------------------------}

 // Do not register these classes !!

  { TPDFBTCommand }

  TPDFTextStateCommand = class(TPDFCommand)
    class function CommandType : TPDFCommandType; override;
  end;

  { TPDFGraphicStateCommand }

  TPDFGraphicStateCommand = class(TPDFCommand)
    class function CommandType : TPDFCommandType; override;
  end;

  { TPDFNameCommand }

  TPDFNameCommand = class(TPDFCommand)
  protected
    function GetValue: String;
  Public
    Property Name : String Read GetValue;
  end;

  { TPDFMarkedContentCommand }

  TPDFMarkedContentCommand = class(TPDFNameCommand)
  Public
    Class function CommandType: TPDFCommandType; override;
    Property Tag : String Read GetValue;
  end;


  { TPDFTextCommand }

  TPDFTextCommand = Class(TPDFCommand)
  Public
    class function CommandType : TPDFCommandType; override;
    Function GetFullText(aUnicodeMap : TPDFCMap) : RawByteString; virtual; overload;
    Function GetFullText : RawByteString; virtual; abstract; overload;
  end;

  // Some parents that allow easy access to the arguments

  { TPDFNumberCommand }

  TPDFNumberCommand = class(TPDFCommand)
  private
    function GetAsArray: TSingleDynArray;
    function GetValues(Index: Integer): Single;
  protected
    function GetValue: Single;
  Public
    Property Value : Single Read GetValue;
    Property Values[Index: Integer] : Single Read GetValues;
    Property AsArray : TSingleDynArray Read GetAsArray;
  end;

  { TPDFIntegerNumberCommand }

  TPDFIntegerNumberCommand = class(TPDFCommand)
  protected
    function GetValue: Integer;
  Public
    Property Value : Integer Read GetValue;
  end;

  { TPDFStringCommand }

  TPDFStringCommand = class(TPDFCommand)
  protected
    function GetValue: String;
  Public
    Property Value : String Read GetValue;
  end;


  { Transformation matrix :
    [ a b 0 ]  [ hscale skew1   0]
    [ c d 0 ]  [ skew2  vscale  0]
    [ e f 1 ]  [ hoff   voff    1]
  }

  { TPDFTransformCommand }

  TPDFTransformCommand = class(TPDFCommand)
  private
    function GetMatrixPos(aIndex: Integer): double;
    function GetTransFormation: TPDFTransFormation;
  public
    property a : double Index 0 Read GetMatrixPos; // Horz scale
    property b : double Index 1 Read GetMatrixPos; // Skew 1
    property c : double Index 2 Read GetMatrixPos; // Skew 2
    property d : double Index 3 Read GetMatrixPos; // Vert scale
    property e : double Index 4 Read GetMatrixPos; // Horz offset
    property f : double Index 5 Read GetMatrixPos; // Vert offset
    Property TransFormation : TPDFTransFormation read GetTransFormation;
  end;

  { TPDFTranslateCommand }

  TPDFTranslateCommand = class(TPDFCommand)
  private
    function GetPos(AIndex: Integer): double;
  public
    property x : double Index 0 Read GetPos;
    property y : double Index 1 Read GetPos;
  end;

  { TPDFDrawcommand }

  TPDFDrawcommand = class(TPDFCommand)
    class function CommandType : TPDFCommandType; override;
  end;

  { TPDFPointcommand }

  TPDFPointcommand = class(TPDFDrawcommand)
  protected
    function GetCoord(AIndex: Integer): Single;
    function GetPoint(AIndex: Integer): TPDFPoint;
  end;

  TPDFOnePointcommand = class(TPDFPointCommand)
  protected
  Public
    property X : Single Index 0 Read GetCoord;
    property Y : Single Index 1 Read GetCoord;
    Property AsPoint : TPDFPoint Index 0 Read GetPoint;
  end;

  TPDFTwoPointscommand = class(TPDFPointCommand)
  Public
    property X1 : Single Index 0 Read GetCoord;
    property Y1 : Single Index 1 Read GetCoord;
    property X2 : Single Index 2 Read GetCoord;
    property Y2 : Single Index 3 Read GetCoord;
    Property Point1 : TPDFPoint Index 0 Read GetPoint;
    Property Point2 : TPDFPoint Index 1 Read GetPoint;
  end;

  { TPDFThreePointscommand }

  TPDFThreePointscommand = class(TPDFTwoPointsCommand)
  Public
    property X3 : Single Index 4 Read GetCoord;
    property Y3 : Single Index 5 Read GetCoord;
    Property Point3 : TPDFPoint Index 2 Read GetPoint;
  end;

  { TPDFStrokeCommand }

  TPDFStrokeCommand = class(TPDFCommand)
    class function CommandType : TPDFCommandType; override;
    class Function StrokeOptions : TPDFStrokeOptions; virtual; abstract;
  end;

{ ----------------------------------------------------------------------
  Other commands
  ----------------------------------------------------------------------}


  { TPDFBXCommand }

  // begin compatibility section
  TPDFBXCommand = class(TPDFCommand)
    Class Function RegisterCommandName : String;override;
  end;

  { TPDFEXCommand }

  // end compatibility section
  TPDFEXCommand = class(TPDFCommand)
    Class Function RegisterCommandName : String; override;
  end;

  { TPDFTextStateCommand }

  // begin text
  TPDFBTCommand = class(TPDFTextStateCommand)
    Class Function RegisterCommandName : String;override;
  end;

  { TPDFETCommand }

  // end text
  TPDFETCommand = class(TPDFTextStateCommand)
    Class Function RegisterCommandName : String; override;
  end;


  { TPDFBDCCommand }

  // Begin Marked Content with properties
  TPDFBDCCommand = class(TPDFMarkedContentCommand)
    Class Function RegisterCommandName : String;override;
  end;


  { TPDFBMCCommand }

  // Begin Marked Content
  TPDFBMCCommand = class(TPDFMarkedContentCommand)
    Class Function RegisterCommandName : String;override;
  end;


  { TPDFEMCCommand }

  // End Marked Content
  TPDFEMCCommand = class(TPDFMarkedContentCommand)
    Class Function RegisterCommandName : String; override;
  end;


  { TPDFMPCommand }

  // Marked content Point
  TPDFMPCommand = class(TPDFMarkedContentCommand)
    Class Function RegisterCommandName : String; override;
  end;

  { TPDFDPCommand }

  // Marked content Point with properties
  TPDFDPCommand = class(TPDFMarkedContentCommand)
    Class Function RegisterCommandName : String; override;
  end;


  { TPDFDoCommand }

  // Invoke named object
  TPDFDoCommand = class(TPDFCommand)
    Class Function RegisterCommandName : String;override;
  end;

{ ----------------------------------------------------------------------
  Graphics state
  ----------------------------------------------------------------------}

  { TPDFcmCommand }

  // concat CMT matrix
  TPDFcmCommand = class(TPDFGraphicStateCommand)
    Class Function RegisterCommandName : String;override;
  end;


  { TPDFcm_Command }
  // set current transformation matrix
  TPDFcm_Command = class(TPDFTransformCommand)
    Class Function RegisterCommandName : String; override;
    class function CommandType : TPDFCommandType; override;
  end;



  { TPDFq_Command }

  // Push graphics drawing state
  TPDFq_Command = class(TPDFGraphicStateCommand)
    Class Function RegisterCommandName : String; override;
  end;

  { TPDFQCommand }

  // Pop graphics drawing state
  TPDFQCommand = class(TPDFGraphicStateCommand)
    Class Function RegisterCommandName : String; override;
  end;

  { TPDFw_Command }
  // Set line width
  TPDFw_Command = class(TPDFNumberCommand)
  public
    class function CommandType : TPDFCommandType; override;
    Class Function RegisterCommandName : String; override;
    Property LineWidth : Single Read GetValue;
  end;

  { TPDFJCommand }
  // Set line cap
  TPDFJCommand = class(TPDFintegerNumberCommand)
  private
    function GetLineCap: TPDFLineCapStyle;
  public
    class function CommandType : TPDFCommandType; override;
    Class Function RegisterCommandName : String; override;
    Property LineCap : TPDFLineCapStyle Read GetLineCap;
  end;

  { TPDFj_Command }
  // Set line join
  TPDFj_Command = class(TPDFintegerNumberCommand)
  private
    function GetLineJoin: TPDFLineJoinStyle;
  public
    class function CommandType : TPDFCommandType; override;
    Class Function RegisterCommandName : String; override;
    Property LineJoin : TPDFLineJoinStyle Read GetLineJoin;
  end;

  { TPDFMCommand }
  // Set Miter limit
  TPDFMCommand = class(TPDFNumberCommand)
  public
    class function CommandType : TPDFCommandType; override;
    Class Function RegisterCommandName : String; override;
    Property MiterLimit : Single Read GetValue;
  end;

  { TPDFd_Command }
  // Set dash style
  TPDFd_Command = class(TPDFCommand)
  private
    function GetDash: TPDFDashPattern;
    function GetPattern: TIntegerDynArray;
    function GetPhase: Integer;
  public
    class function CommandType : TPDFCommandType; override;
    Class Function RegisterCommandName : String; override;
    Property Pattern : TIntegerDynArray Read GetPattern;
    Property Phase : Integer Read GetPhase;
    Property AsDash : TPDFDashPattern Read GetDash;
  end;


  { TPDFi_Command }
  // set flatness limit
  TPDFi_Command = class(TPDFintegerNumberCommand)
  public
    class function CommandType : TPDFCommandType; override;
    Class Function RegisterCommandName : String; override;
    Property Flatness : Integer Read GetValue;
  end;

  { TPDFri_Command }
  // Set color rendering intent
  TPDFri_Command = class(TPDFStringCommand)
  public
    class function CommandType : TPDFCommandType; override;
    Class Function RegisterCommandName : String; override;
    Property Intent : string Read GetValue;
  end;

  { TPDFgs_Command }
  // Set graphics state from dictionary
  TPDFgs_Command = class(TPDFNameCommand)
  public
    class function CommandType: TPDFCommandType; override;
    Class Function RegisterCommandName : String; override;
    Property DictName : string Read GetValue;
  end;

{ ----------------------------------------------------------------------
  Color commands
  ----------------------------------------------------------------------}

  { TPDFCSCommand }
  // Set stroke color space

  TPDFCSCommand = class(TPDFNameCommand)
    class function CommandType: TPDFCommandType; override;
    Class Function RegisterCommandName : String; override;
    function ColorSpace : TPDFColorSpace;
  end;

  // Set fill color space

  { TPDFcs_Command }

  TPDFcs_Command = class(TPDFNameCommand)
    class function CommandType: TPDFCommandType; override;
    Class Function RegisterCommandName : String; override;
    function ColorSpace : TPDFColorSpace;
  end;


  { TPDFSCCommand }

  // Set stroke color
  TPDFSCCommand = class(TPDFNumberCommand)
    class function CommandType: TPDFCommandType; override;
    Class Function RegisterCommandName : String; override;
  end;

  { TPDFsc_Command }

  // Set non-stroke color
  TPDFsc_Command = class(TPDFNumberCommand)
    class function CommandType: TPDFCommandType; override;
    Class Function RegisterCommandName : String; override;
  end;

  { TPDFSCNCommand }

  { TPDFColorNamespaceCommand }

  TPDFColorNamespaceCommand = class(TPDFCommand)
  private
    function GetAsArray: TSingleDynArray;
    function HasName : Boolean;
  Public
    class function CommandType: TPDFCommandType; override;
    Function ColorSpaceName : String;
    Property AsArray : TSingleDynArray Read GetAsArray;
  end;

  TPDFSCNCommand = class(TPDFColorNamespaceCommand)
    Class Function RegisterCommandName : String; override;
  end;

  { TPDFscn_Command }

  TPDFscn_Command = class(TPDFColorNamespaceCommand)
    Class Function RegisterCommandName : String; override;
  end;

  { TPDFscn_Command }

  // Set devicegray and gray level for stroke operations

  { TPDFGCommand }

  TPDFGCommand = class(TPDFNumberCommand)
  private
    function GetColor: TPDFColor;
  public
    class function CommandType: TPDFCommandType; override;
    Class Function RegisterCommandName : String; override;
    property gray : single Read GetValue;
    function ColorSpace : TPDFColorSpace;
    property AsColor : TPDFColor Read GetColor;
  end;

  // Set devicegray and gray level for non-stroke operations

  { TPDFg_Command }

  TPDFg_Command = class(TPDFNumberCommand)
  private
    function GetColor: TPDFColor;
  public
    class function CommandType: TPDFCommandType; override;
    Class Function RegisterCommandName : String; override;
    property gray : single Read GetValue;
    function ColorSpace : TPDFColorSpace;
    property AsColor : TPDFColor Read GetColor;
  end;


  { TPDFRGBCommand }

  // Set deviceRGB color space and color - parent
  TPDFRGBCommand = class(TPDFNumberCommand)
  private
    function GetColor: TPDFColor;
  Public
    class function CommandType: TPDFCommandType; override;
    function ColorSpace : TPDFColorSpace;
    property R : single read GetValue;
    property G : single index 1 read GetValues;
    property B : single index 2 read GetValues;
    Property AsColor : TPDFColor Read GetColor;
  end;

  { TPDFRGCommand }

  // Set deviceRGB color space and color for stroke operations
  TPDFRGCommand = class(TPDFRGBCommand)
    Class Function RegisterCommandName : String; override;
  end;


  { TPDFrg_Command }

  // Set deviceRGB color space and color for non-stroke operations
  TPDFrg_Command = class(TPDFRGBCommand)
    Class Function RegisterCommandName : String; override;
  end;

  { TPDFCMYKCommand }

  // Select CMYK color space and color - parent
  TPDFCMYKCommand = class(TPDFNumberCommand)
  private
    function GetAsColor: TPDFColor;
  Public
    class function CommandType: TPDFCommandType; override;
    function ColorSpace : TPDFColorSpace;
    property C : single read GetValue;
    property M : single index 1 read GetValues;
    property Y : single index 2 read GetValues;
    property K : single index 3 read GetValues;
    property AsColor : TPDFColor Read GetAsColor;
  end;

  { TPDFKCommand }

  // Set stroke color in CMYK color space
  TPDFKCommand = class(TPDFCMYKCommand)
    Class Function RegisterCommandName : String; override;
  end;

  { TPDFk_Command }

  // Set non-stroke color in CMYK color space
  TPDFk_Command = class(TPDFCMYKCommand)
    Class Function RegisterCommandName : String; override;
  end;


{ ----------------------------------------------------------------------
  Text drawing
  ----------------------------------------------------------------------}


  { TPDFTJCommand }
  // Draw text segments
  TPDFTJCommand = class(TPDFTextCommand)
    Class Function RegisterCommandName : String; override;
    Function GetFullText(aUnicodeMap : TPDFCMap) : RawByteString; override; overload;
    Function GetFullText : RawByteString; override;
  end;

  { TPDFTj_Command }

  // Draw text
  TPDFTj_Command = class(TPDFTextCommand)
    Class Function RegisterCommandName : String; override;
    Function GetFullText : RawByteString; override;
  end;

{ ----------------------------------------------------------------------
  Text state commands
  ----------------------------------------------------------------------}

  { TPDFTf_Command }

  // Set font
  TPDFTf_Command = class(TPDFTextStateCommand)
  private
    function GetFontName: String;
    function GetFontSize: Integer;
  public
    Class Function RegisterCommandName : String; override;
    property FontName : String Read GetFontName;
    Property FontSize : Integer Read GetFontSize;
  end;


  { TPDFTm_Command }

  // Set text matrix
  TPDFTm_Command = class(TPDFTransformCommand)
    class function CommandType : TPDFCommandType; override;
    Class Function RegisterCommandName : String; override;
  end;

  { TPDFTAsterixCommand }

  // Move to start of next line
  TPDFTAsterixCommand = class(TPDFTransformCommand)
    class function CommandType : TPDFCommandType; override;
    Class Function RegisterCommandName : String; override;
  end;

  { TPDFTc_Command }

  // set text character spacing
  TPDFTc_Command = class(TPDFNumberCommand)
    class function CommandType : TPDFCommandType; override;
    Class Function RegisterCommandName : String; override;
  end;

  { TPDFTw_Command }

  // Set text word spacing
  TPDFTw_Command = class(TPDFNumberCommand)
    class function CommandType : TPDFCommandType; override;
    Class Function RegisterCommandName : String; override;
  end;

  { TPDFTz_Command }

  // set text horizontal scaling
  TPDFTz_Command = class(TPDFNumberCommand)
    class function CommandType : TPDFCommandType; override;
    Class Function RegisterCommandName : String; override;
  end;

  { TPDFTLCommand }

  // Set text leading
  TPDFTLCommand = class(TPDFNumberCommand)
    class function CommandType : TPDFCommandType; override;
    Class Function RegisterCommandName : String; override;
  end;


  { TPDFTr_Command }

  // set text rendering mode
  TPDFTr_Command = class(TPDFIntegerNumberCommand)
    class Function CommandType: TPDFCommandType; override;
    Class Function RegisterCommandName : String; override;
    Function RenderMode : TPDFTextRenderMode;
  end;

  { TPDFTs_Command }

  // set text rise
  TPDFTs_Command = class(TPDFNumberCommand)
    class Function CommandType: TPDFCommandType; override;
    Class Function RegisterCommandName : String; override;
  end;


  //  Move text position
  TPDFTd_Command = class(TPDFTranslateCommand)
    class function CommandType : TPDFCommandType; override;
    Class Function RegisterCommandName : String; override;
  end;

  { TPDFTDCommand }

  // Move text position and leading
  TPDFTDCommand = class(TPDFTranslateCommand)
    class function CommandType : TPDFCommandType; override;
    Class Function RegisterCommandName : String; override;
  end;

{ ----------------------------------------------------------------------
  Drawing (path creation)
  ----------------------------------------------------------------------}

  { TPDFm_command }

  TPDFm_command = Class(TPDFOnePointcommand)
  public
    Class Function RegisterCommandName : String; override;
    Function PathSegment : TPDFPathSegment;
  end;

  { TPDFl_command }

  TPDFl_command = Class(TPDFOnePointcommand)
  public
    Class Function RegisterCommandName : String; override;
  end;

  { TPDFy_command }

  TPDFy_command = Class(TPDFTwoPointsCommand)
  public
    Class Function RegisterCommandName : String; override;
  end;

  { TPDFv_command }

  TPDFv_command = Class(TPDFTwoPointsCommand)
  public
    Class Function RegisterCommandName : String; override;
  end;


  { TPDFc_command }

  TPDFc_command = Class(TPDFThreePointsCommand)
  public
    Class Function RegisterCommandName : String; override;
  end;

  { TPDFh_command }

  TPDFh_command = Class(TPDFDrawcommand)
  public
    Class Function RegisterCommandName : String; override;
  end;

  { TPDFre_command }

  TPDFre_command = class(TPDFPointCommand)
  Public
    Class Function RegisterCommandName : String; override;
    property X : Single Index 0 Read GetCoord;
    property Y : Single Index 1 Read GetCoord;
    property Width : Single Index 2 Read GetCoord;
    property Height : Single Index 3 Read GetCoord;
  end;

{ ----------------------------------------------------------------------
  Stroke (path closing) commands
  ----------------------------------------------------------------------}

  { TPDFScommand }

  TPDFScommand = class(TPDFStrokeCommand)
  public
    Class Function RegisterCommandName : String; override;
    class Function StrokeOptions : TPDFStrokeOptions; override;
  end;

  { TPDFs_command }

  TPDFs_command = class(TPDFStrokeCommand)
  public
    Class Function RegisterCommandName : String; override;
    class Function StrokeOptions : TPDFStrokeOptions; override;
  end;

  { TPDFf_command }

  TPDFf_command = class(TPDFStrokeCommand)
  public
    Class Function RegisterCommandName : String; override;
    class Function StrokeOptions : TPDFStrokeOptions; override;
  end;

  { TPDFFcommand }

  TPDFFcommand = class(TPDFStrokeCommand)
  public
    Class Function RegisterCommandName : String; override;
    class Function StrokeOptions : TPDFStrokeOptions; override;
  end;

  { TPDfAsterisk_command }

  { TPDFfAsterisk_command }

  TPDFfAsterisk_command = class(TPDFStrokeCommand)
  public
    Class Function RegisterCommandName : String; override;
    class Function StrokeOptions : TPDFStrokeOptions; override;
  end;

  { TPDFBcommand }

  TPDFBcommand = class(TPDFStrokeCommand)
  public
    Class Function RegisterCommandName : String; override;
    class Function StrokeOptions : TPDFStrokeOptions; override;
  end;

  { TPDFBAsteriskcommand }

  TPDFBAsteriskcommand = class(TPDFStrokeCommand)
  public
    Class Function RegisterCommandName : String; override;
    class Function StrokeOptions : TPDFStrokeOptions; override;
  end;

  { TPDFb_command }

  TPDFb_command = class(TPDFStrokeCommand)
  public
    Class Function RegisterCommandName : String; override;
    class Function StrokeOptions : TPDFStrokeOptions; override;
  end;

  { TPDFbAsterisk_command }

  TPDFbAsterisk_command = class(TPDFStrokeCommand)
  public
    Class Function RegisterCommandName : String; override;
    class Function StrokeOptions : TPDFStrokeOptions; override;
  end;

  { TPDFn_command }

  TPDFn_command = class(TPDFStrokeCommand)
  public
    Class Function RegisterCommandName : String; override;
    class Function StrokeOptions : TPDFStrokeOptions; override;
  end;

{ ----------------------------------------------------------------------
  Image commands
  ----------------------------------------------------------------------}

  TPDFImageData = record
    Width,
    height,
    BitsPerComponent : Integer;
    ColorSpace : String;
    ColorSpaceComponents : Integer;
    Filters : Array of String;
  end;

  { TPDFImageDataCommand }

  TPDFImageDataCommand = Class(TPDFCommand)
    Class Procedure ParseImageOperands(aOperands : TPDFTokenArray; Out aImageData : TPDFImageData);
    Class Function RegisterCommandName : String; override;
  end;

Function StringToColorSpace(aName : string) : TPDFColorSpace;

implementation

Procedure RegisterStandardCommands;

begin
  // general
  TPDFBXCommand.Register;
  TPDFEXCommand.Register;
  TPDFcm_Command.Register;
  TPDFDoCommand.Register;
  TPDFBTCommand.Register;
  TPDFETCommand.Register;
  // Graphics state
  TPDFgs_Command.Register;
  TPDFQCommand.Register;
  TPDFq_Command.Register;
  // text state
  TPDFTDCommand.Register;
  TPDFTd_Command.Register;
  TPDFTf_Command.Register;
  TPDFTm_Command.Register;
  TPDFTc_Command.Register;
  TPDFTw_Command.Register;
  TPDFTz_Command.Register;
  TPDFTLCommand.Register;
  TPDFTr_Command.Register;
  TPDFTs_Command.Register;
  TPDFTAsterixCommand.Register;
  // Text
  TPDFTJCommand.Register;
  TPDFTj_Command.Register;
  // Graphics state
  TPDFw_Command.Register;
  TPDFJCommand.Register;
  TPDFj_Command.Register;
  TPDFMCommand.Register;
  TPDFd_Command.Register;
  TPDFi_Command.Register;
  TPDFri_Command.Register;
  // Color space

  // Draw
  TPDFm_command.Register;
  TPDFl_command.Register;
  TPDFy_command.Register;
  TPDFv_command.Register;
  TPDFc_command.Register;
  TPDFh_command.Register;
  // Stroke
  TPDFScommand.Register;
  TPDFs_command.Register;
  TPDFf_command.Register;
  TPDFFcommand.Register;
  TPDFfAsterisk_command.Register;
  TPDFBcommand.Register;
  TPDFBAsteriskcommand.Register;
  TPDFb_command.Register;
  TPDFbAsterisk_command.Register;
  TPDFn_command.Register;
  TPDFre_command.Register;
  // Marked content
  TPDFBDCCommand.Register;
  TPDFBMCCommand.Register;
  TPDFEMCCommand.Register;
  TPDFMPCommand.Register;
  TPDFDPCommand.Register;
  // Color
  TPDFCSCommand.Register;
  TPDFcs_Command.Register;
  TPDFSCCommand.Register;
  TPDFsc_Command.Register;
  TPDFSCNCommand.Register;
  TPDFGCommand.Register;
  TPDFg_Command.Register;
  TPDFRGCommand.Register;
  TPDFrg_Command.Register;
  TPDFKCommand.Register;
  TPDFk_Command.Register;
end;


{ TPDFDrawcommand }

class function TPDFDrawcommand.CommandType: TPDFCommandType;
begin
  Result:=cmtPath;
end;

{ TPDFPointcommand }

function TPDFPointcommand.GetCoord(AIndex: Integer): Single;
begin
  if (aIndex>=0) and  (aIndex<Length(Tokens)) then
    Result:=Tokens[aIndex].AsSingle
  else
    Result:=0;
end;

function TPDFPointcommand.GetPoint(AIndex: Integer): TPDFPoint;
begin
  Result.X:=GetCoord(aIndex * 2);
  Result.Y:=GetCoord((aIndex * 2)+1);
end;

function TPDFTransformCommand.GetMatrixPos(aIndex: Integer): double;
begin
  if (aIndex<Length(Tokens)) then
    Result:=Tokens[aIndex].AsDouble
  else
    Result:=0.0;
end;

function TPDFTransformCommand.GetTransFormation: TPDFTransFormation;
begin
  Result:=TPDFTransFormation.Create(a,b,c,d,e,f);
end;


{ TPDFNumberCommand }

function TPDFNumberCommand.GetAsArray: TSingleDynArray;

var
  I : Integer;

begin
  SetLength(Result,Length(Tokens));
  For I:=0 to Length(Result)-1 do
    Result[i]:=Values[I];
end;

function TPDFNumberCommand.GetValues(Index: Integer): Single;
begin
  if (Index>=0) and (Index<Length(Tokens)) then
    Result:=Tokens[Index].AsSingle
  else
    Result:=0;
end;

function TPDFNumberCommand.GetValue: Single;
begin
  if Length(Tokens)>0 then
    Result:=Tokens[0].AsSingle
  else
    Result:=0;
end;

{ TPDFIntegerNumberCommand }

function TPDFIntegerNumberCommand.GetValue: Integer;

begin
  if Length(Tokens)>0 then
    Result:=Tokens[0].AsInteger
  else
    Result:=0;
end;

{ TPDFStringCommand }

function TPDFStringCommand.GetValue: String;
begin
  if (Length(Tokens)>0) and Tokens[0].IsString then
    Result:=Tokens[0].AsString
  else
    Result:='';
end;

{ TPDFNameCommand }

function TPDFNameCommand.GetValue: String;
begin
  if (Length(Tokens)>0) and Tokens[0].IsName then
    Result:=Tokens[0].AsName
  else
    Result:='';
end;


{ TPDFMarkedContentCommand }

class function TPDFMarkedContentCommand.CommandType: TPDFCommandType;
begin
  Result:=cmtMarkedContent;
end;

{ TPDFGraphicStateCommand }

class function TPDFGraphicStateCommand.CommandType: TPDFCommandType;
begin
  Result:=cmtGraphicState;
end;
{ TPDFTextStateCommand }

class function TPDFTextStateCommand.CommandType: TPDFCommandType;
begin
  Result:=cmtTextState;
end;



{ TPDFTextCommand }

class function TPDFTextCommand.CommandType: TPDFCommandType;
begin
  Result:=cmtText;
end;

function TPDFTextCommand.GetFullText(aUnicodeMap: TPDFCMap): RawByteString;

Var
  aRaw : RawByteString;

begin
  aRaw:=GetFullText();
  if not Assigned(aUnicodeMap) then
    Result:=aRaw
  else
    Result:=aUnicodeMap.InterPret(aRaw);
end;

{ TPDFImageDataCommand }

class procedure TPDFImageDataCommand.ParseImageOperands(
  aOperands: TPDFTokenArray; out aImageData: TPDFImageData);

Var
  I,J : Integer;


begin
  aImageData:=Default(TPDFImageData);
  I:=0;
  While (I<Length(aOperands)-1) do
    begin
    if aOperands[i].IsName then
      begin
      Case Copy(aOperands[i].TokenData,2,Length(aOperands[i].TokenData)-1) of
        SPDFImageKeyW :
          begin
          Inc(I);
          aImageData.Width:=aOperands[i].AsInteger;
          end;
        SPDFImageKeyH :
          begin
          Inc(I);
          aImageData.Height:=aOperands[i].AsInteger;
          end;
        SPDFImageKeyBPC:
          begin
          Inc(I);
          aImageData.BitsPerComponent:=aOperands[i].AsInteger;
          end;
        SPDFImageKeyCS:
          begin
          Inc(I);
          aImageData.ColorSpace:=aOperands[i].TokenData;
          end;
        SPDFImageKeyF:
          begin
          Inc(i);
          If aOperands[i].TokenType<>ptSquareOpen then
            begin
            Inc(i);
            aImageData.Filters:=[aOperands[i].TokenData];
            end
          else
            begin
            Inc(I);
            J:=I;
            While (J<Length(aOperands)) and (aOperands[J].TokenType<>ptSquareClose) do
              Inc(J);
            SetLength(aImageData.Filters,J);
            J:=I;
            While (J<Length(aOperands)) and (aOperands[J].TokenType<>ptSquareClose) do
              begin
              aImageData.Filters[J-I]:=aOperands[J].TokenData;
              Inc(J);
              end
            end;
          end;
      end;
      end;
    inc(I);
    end;
  Case Copy(aImageData.ColorSpace,2,Length(aImageData.ColorSpace)-1) of
    SPDFImageKeyCMYK : aImageData.ColorSpaceComponents:=4;
    SPDFImageKeyRGB : aImageData.ColorSpaceComponents:=3;
    SPDFImageKeyG : aImageData.ColorSpaceComponents:=1;
  end;
end;

class function TPDFImageDataCommand.RegisterCommandName: String;
begin
  Result:='ID';
end;

{ TPDFTTranslateCommand }

function TPDFTranslateCommand.GetPos(AIndex: Integer): double;
begin
  if aIndex<Length(Tokens) then
    Result:=Tokens[aIndex].AsDouble
  else
    Result:=0;
end;

{ TPDFTd_Command }

class function TPDFTd_Command.CommandType: TPDFCommandType;
begin
  Result:=cmtTextState;
end;

class function TPDFTd_Command.RegisterCommandName: String;
begin
  Result:='Td';
end;

{ TPDFq_Command }

class function TPDFq_Command.RegisterCommandName: String;
begin
  Result:='q';
end;

{ TPDFQCommand }

class function TPDFQCommand.RegisterCommandName: String;
begin
  Result:='Q';
end;

{ TPDFw_Command }

class function TPDFw_Command.CommandType: TPDFCommandType;
begin
  Result:=cmtGraphicState;
end;

class function TPDFw_Command.RegisterCommandName: String;
begin
  Result:='w';
end;

{ TPDFJCommand }

function TPDFJCommand.GetLineCap: TPDFLineCapStyle;

Var
  i : integer;

begin
  Result:=lcsbutt;
  I:=Value;
  if (I>=0) and (I<Ord(High(TPDFLineCapStyle))) then
    Result:=TPDFLineCapStyle(I);
end;

class function TPDFJCommand.CommandType: TPDFCommandType;
begin
  Result:=cmtGraphicState;
end;

class function TPDFJCommand.RegisterCommandName: String;
begin
  Result:='J'
end;

{ TPDFj_Command }

function TPDFj_Command.GetLineJoin: TPDFLineJoinStyle;

Var
  i : integer;

begin
  Result:=ljsMiter;
  I:=Value;
  if (I>=0) and (I<Ord(High(TPDFLineJoinStyle))) then
    Result:=TPDFLineJoinStyle(I);
end;

class function TPDFj_Command.CommandType: TPDFCommandType;
begin
  Result:=cmtGraphicState;
end;

class function TPDFj_Command.RegisterCommandName: String;
begin
  Result:='j';
end;

{ TPDFMCommand }

class function TPDFMCommand.CommandType: TPDFCommandType;
begin
  Result:=cmtGraphicState;
end;

class function TPDFMCommand.RegisterCommandName: String;
begin
  Result:='M';
end;

{ TPDFd_Command }

function TPDFd_Command.GetDash: TPDFDashPattern;
begin
  Result.Pattern:=Pattern;
  Result.Phase:=Phase;
end;

function TPDFd_Command.GetPattern: TIntegerDynArray;

var
  i,len : Integer;

begin
  Result:=[];
  Len:=Length(Tokens)-2; // Index of ]
  if (Len<2) then
    exit;
  if (Tokens[0].AsString='[') and (Tokens[Len].AsString=']') then
    begin
    SetLength(Result,Len-1);
    for I:=1 to Len-1 do
      if Tokens[I].IsInteger then
        Result[i-1]:=Tokens[I].AsInteger
      else
        Result[i-1]:=0;
    end;
end;

function TPDFd_Command.GetPhase: Integer;

var
  len : Integer;

begin
  Len:=Length(Tokens)-1; // Index of phase
  if (Len>2) and (Tokens[len].IsInteger) then
    Result:=Tokens[Len].asInteger
end;

class function TPDFd_Command.CommandType: TPDFCommandType;
begin
  Result:=cmtGraphicState;
end;

class function TPDFd_Command.RegisterCommandName: String;
begin
  Result:='d';
end;

{ TPDFi_Command }

class function TPDFi_Command.CommandType: TPDFCommandType;
begin
  Result:=cmtGraphicState;
end;

class function TPDFi_Command.RegisterCommandName: String;
begin
  Result:='i';
end;

{ TPDFri_Command }

class function TPDFri_Command.CommandType: TPDFCommandType;
begin
  Result:=cmtGraphicState;
end;

class function TPDFri_Command.RegisterCommandName: String;
begin
  Result:='ri';
end;

{ TPDFgs_Command }

class function TPDFgs_Command.CommandType: TPDFCommandType;
begin
  Result:=cmtGraphicState;
end;

class function TPDFgs_Command.RegisterCommandName: String;
begin
  Result:='gs';
end;

{ ----------------------------------------------------------------------
  Color commands
  ----------------------------------------------------------------------}


{ TPDFCSCommand }

class function TPDFCSCommand.CommandType: TPDFCommandType;
begin
  Result:=cmtColor;
end;

class function TPDFCSCommand.RegisterCommandName: String;
begin
  Result:='CS';
end;

Function StringToColorSpace(aName : string) : TPDFColorSpace;

begin
  if (aName<>'') and (aName[1]='/') then
    Delete(aName,1,1);
  case aName of
    SPDFColorSpaceDeviceRGB  : result:=csDeviceRGB;
    SPDFColorSpaceDeviceGray : result:=csDeviceGray;
    SPDFColorSpaceDeviceCMYK : result:=csDeviceCMYK;
  else
    Result:=csDict;
  end;
end;

function TPDFCSCommand.ColorSpace: TPDFColorSpace;
begin
  StringToColorSpace(Name);
end;

{ TPDFcs_Command }

class function TPDFcs_Command.CommandType: TPDFCommandType;
begin
  Result:=cmtColor;
end;

class function TPDFcs_Command.RegisterCommandName: String;
begin
  Result:='cs';
end;

function TPDFcs_Command.ColorSpace: TPDFColorSpace;
begin
  Result:=StringToColorSpace(Name);
end;

{ TPDFSCCommand }

class function TPDFSCCommand.CommandType: TPDFCommandType;
begin
  Result:=cmtColor;
end;

class function TPDFSCCommand.RegisterCommandName: String;
begin
  Result:='SC';
end;

{ TPDFsc_Command }

class function TPDFsc_Command.CommandType: TPDFCommandType;
begin
  Result:=cmtColor;
end;

class function TPDFsc_Command.RegisterCommandName: String;
begin
  Result:='sc';
end;

{ TPDFColorNamespaceCommand }

function TPDFColorNamespaceCommand.GetAsArray: TSingleDynArray;

var
  I,aCount : Integer;

begin
  aCount:=Length(Tokens)-Ord(HasName);
  SetLength(Result,aCount);
  For I:=0 to aCount-1 do
    Result[i]:=Tokens[i].AsSingle;
end;

function TPDFColorNamespaceCommand.HasName: Boolean;
begin
  Result:=(Length(Tokens)>0) and (Tokens[Length(Tokens)].TokenType=ptName);
end;

class function TPDFColorNamespaceCommand.CommandType: TPDFCommandType;
begin
  Result:=cmtColor;
end;

function TPDFColorNamespaceCommand.ColorSpaceName: String;
begin
  if HasName then
    Result:=Tokens[Length(Tokens)-1].AsName;
end;

{ TPDFSCNCommand }


class function TPDFSCNCommand.RegisterCommandName: String;
begin
  Result:='SCN';
end;

{ TPDFscn_Command }

class function TPDFscn_Command.RegisterCommandName: String;
begin
  Result:='scn';
end;

{ TPDFg_Command }

function TPDFg_Command.GetColor: TPDFColor;
begin
  Result:=Default(TPDFColor);
  Result[0]:=Gray;
end;

class function TPDFg_Command.CommandType: TPDFCommandType;
begin
  Result:=cmtColor
end;

class function TPDFg_Command.RegisterCommandName: String;
begin
  Result:='g';
end;

function TPDFg_Command.ColorSpace: TPDFColorSpace;
begin
  Result:=csDeviceGray;
end;

{ TPDFRGBCommand }

function TPDFRGBCommand.GetColor: TPDFColor;
begin
  Result:=Default(TPDFColor);
  Result[0]:=R;
  Result[1]:=G;
  Result[2]:=B;
end;

class function TPDFRGBCommand.CommandType: TPDFCommandType;
begin
  Result:=cmtColor;
end;

function TPDFRGBCommand.ColorSpace: TPDFColorSpace;
begin
  Result:=csDeviceRGB;
end;

{ TPDFRGCommand }

class function TPDFRGCommand.RegisterCommandName: String;
begin
  Result:='RG';
end;

{ TPDFrg_Command }

class function TPDFrg_Command.RegisterCommandName: String;
begin
  Result:='rg';
end;

{ TPDFCMYKCommand }

function TPDFCMYKCommand.GetAsColor: TPDFColor;
begin
  Result:=Default(TPDFColor);
  Result[0]:=C;
  Result[1]:=M;
  Result[2]:=Y;
  Result[3]:=K;
end;

class function TPDFCMYKCommand.CommandType: TPDFCommandType;
begin
  Result:=cmtColor;
end;

function TPDFCMYKCommand.ColorSpace: TPDFColorSpace;
begin
  result:=csDeviceCMYK;
end;

{ TPDFKCommand }

class function TPDFKCommand.RegisterCommandName: String;
begin
  Result:='K';
end;

{ TPDFk_Command }

class function TPDFk_Command.RegisterCommandName: String;
begin
  Result:='k';
end;

{ TPDFGCommand }

function TPDFGCommand.GetColor: TPDFColor;
begin
  Result:=Default(TPDFColor);
  Result[0]:=Gray;
end;

class function TPDFGCommand.CommandType: TPDFCommandType;
begin
  Result:=cmtColor;
end;

class function TPDFGCommand.RegisterCommandName: String;
begin
  Result:='G';
end;

function TPDFGCommand.ColorSpace: TPDFColorSpace;
begin
  Result:=csDeviceGray;
end;

{ ----------------------------------------------------------------------
  Drawing commands
  ----------------------------------------------------------------------}


{ TPDFm_command }

class function TPDFm_command.RegisterCommandName: String;
begin
  Result:='m'
end;

function TPDFm_command.PathSegment: TPDFPathSegment;
begin
  Result:=Default(TPDFPathSegment);
  Result.SegmentType:=stStart;
  Result.P1:=asPoint;
end;

{ TPDFl_command }

class function TPDFl_command.RegisterCommandName: String;
begin
  Result:='l';
end;

{ TPDFy_command }

class function TPDFy_command.RegisterCommandName: String;
begin
  Result:='y';
end;

{ TPDFv_command }

class function TPDFv_command.RegisterCommandName: String;
begin
  Result:='v';
end;

{ TPDFc_command }

class function TPDFc_command.RegisterCommandName: String;
begin
  Result:='c';
end;

{ TPDFh_command }

class function TPDFh_command.RegisterCommandName: String;
begin
  Result:='h';
end;

{ TPDFTre_command }

class function TPDFre_command.RegisterCommandName: String;
begin
  Result:='re';
end;

{ TPDFStrokeCommand }

class function TPDFStrokeCommand.CommandType: TPDFCommandType;
begin
  Result:=cmtStroke;
end;

{ TPDFScommand }

class function TPDFScommand.RegisterCommandName: String;
begin
  Result:='S';
end;

class function TPDFScommand.StrokeOptions: TPDFStrokeOptions;
begin
  Result:=[soStroke]
end;

{ TPDFs_command }

class function TPDFs_command.RegisterCommandName: String;
begin
  Result:='s';
end;

class function TPDFs_command.StrokeOptions: TPDFStrokeOptions;
begin
  Result:=[soClose,soStroke]
end;

{ TPDFf_command }

class function TPDFf_command.RegisterCommandName: String;
begin
  Result:='f';
end;

class function TPDFf_command.StrokeOptions: TPDFStrokeOptions;
begin
  Result:=[soFill,soNonZeroWinding];
end;

{ TPDFFcommand }

class function TPDFFcommand.RegisterCommandName: String;
begin
  Result:='F';
end;

class function TPDFFcommand.StrokeOptions: TPDFStrokeOptions;
begin
  Result:=[soFill,soNonZeroWinding];
end;

{ TPDFfAsterisk_command }

class function TPDFfAsterisk_command.RegisterCommandName: String;
begin
  Result:='f*';
end;

class function TPDFfAsterisk_command.StrokeOptions: TPDFStrokeOptions;
begin
  Result:=[soFill,soEvenOdd];
end;

{ TPDFBcommand }

class function TPDFBcommand.RegisterCommandName: String;
begin
  Result:='B';
end;

class function TPDFBcommand.StrokeOptions: TPDFStrokeOptions;
begin
  Result:=[soStroke,soFill,soNonZeroWinding];
end;

{ TPDFBAsteriskcommand }

class function TPDFBAsteriskcommand.RegisterCommandName: String;
begin
  Result:='B*';
end;

class function TPDFBAsteriskcommand.StrokeOptions: TPDFStrokeOptions;
begin
  Result:=[soStroke,soFill,soEvenOdd];
end;

{ TPDFb_command }

class function TPDFb_command.RegisterCommandName: String;
begin
  Result:='b';
end;

class function TPDFb_command.StrokeOptions: TPDFStrokeOptions;
begin
  Result:=[soClose,soStroke,soFill,soNonZeroWinding];
end;

{ TPDFbAsterisk_command }

class function TPDFbAsterisk_command.RegisterCommandName: String;
begin
  Result:='b*';
end;

class function TPDFbAsterisk_command.StrokeOptions: TPDFStrokeOptions;
begin
  Result:=[soStroke,soFill,soEvenOdd];
end;

{ TPDFn_command }

class function TPDFn_command.RegisterCommandName: String;
begin
  Result:='n';
end;

class function TPDFn_command.StrokeOptions: TPDFStrokeOptions;
begin
  Result:=[soClose];
end;

{ TPDFTj_Command }

class function TPDFTj_Command.RegisterCommandName: String;
begin
  Result:='Tj';
end;

function TPDFTj_Command.GetFullText: RawByteString;
begin
  Result:='';
  if Length(Self.Tokens)>0 then
    try
      Result:=Tokens[0].AsString;
    except
      on E : exception do
        begin
        Writeln('Exception ',E.ClassName,'getting text for token: "',E.Message,'". Token data :',GetDescription);
        Raise;
        end;

    end;
end;

{ TPDFTfCommand }

function TPDFTf_Command.GetFontName: String;
begin
  Result:='';
  If (Length(Tokens)>0) then
    if Tokens[0].IsString then
      Result:=Tokens[0].AsString
    else if Tokens[0].IsName then
      Result:=Tokens[0].AsName;
end;

function TPDFTf_Command.GetFontSize: Integer;

begin
  Result:=0;
  If (Length(Tokens)>1) and Tokens[1].IsInteger then
    Result:=Tokens[1].AsInteger
end;

class function TPDFTf_Command.RegisterCommandName: String;
begin
  Result:='Tf';
end;

{ TPDFTdCommand }

class function TPDFTDCommand.CommandType: TPDFCommandType;
begin
  Result:=cmtTextState;
end;

class function TPDFTDCommand.RegisterCommandName: String;
begin
  Result:='TD';
end;

{ TPDFTm_Command }


class function TPDFTm_Command.CommandType: TPDFCommandType;
begin
  Result:=cmtTextState;
end;

class function TPDFTm_Command.RegisterCommandName: String;
begin
  Result:='Tm';
end;

{ TPDFTAsterixCommand }

class function TPDFTAsterixCommand.CommandType: TPDFCommandType;
begin
  Result:=cmtTextState;
end;

class function TPDFTAsterixCommand.RegisterCommandName: String;
begin
  Result:='T*';
end;

{ TPDFTc_Command }

class function TPDFTc_Command.CommandType: TPDFCommandType;
begin
  Result:=cmtTextState;
end;

class function TPDFTc_Command.RegisterCommandName: String;
begin
  Result:='Tc';
end;

{ TPDFTw_Command }

class function TPDFTw_Command.CommandType: TPDFCommandType;
begin
  Result:=cmtTextState;
end;

class function TPDFTw_Command.RegisterCommandName: String;
begin
  Result:='Tw';
end;

{ TPDFTz_Command }

class function TPDFTz_Command.CommandType: TPDFCommandType;
begin
  Result:=cmtTextState;
end;

class function TPDFTz_Command.RegisterCommandName: String;
begin
  Result:='Tz';
end;

{ TPDFTLCommand }

class function TPDFTLCommand.CommandType: TPDFCommandType;
begin
  Result:=cmtTextState;
end;

class function TPDFTLCommand.RegisterCommandName: String;
begin
  Result:='TL';
end;

{ TPDFTrCommand }

class function TPDFTr_Command.CommandType: TPDFCommandType;
begin
  Result:=cmtTextState;
end;

class function TPDFTr_Command.RegisterCommandName: String;
begin
  Result:='Tr';
end;

function TPDFTr_Command.RenderMode: TPDFTextRenderMode;
begin
  Case Value of
    0 : Result:=[trmFill];
    1 : Result:=[trmStroke];
    2 : Result:=[trmFill,trmStroke];
    3 : Result:=[];
    4 : Result:=[trmFill,trmAddToClippingPath];
    5 : Result:=[trmStroke,trmAddToClippingPath];
    6 : Result:=[trmFill,trmStroke,trmAddToClippingPath];
    7 : Result:=[trmAddToClippingPath];
  else
    Result:=[trmFill];
  end;
end;

{ TPDFTsCommand }

class function TPDFTs_Command.CommandType: TPDFCommandType;
begin
  Result:=cmtTextState;
end;

class function TPDFTs_Command.RegisterCommandName: String;
begin
  Result:='Ts';
end;

{ TPDFcm_Command }

class function TPDFcm_Command.RegisterCommandName: String;
begin
  Result:='cm';
end;

class function TPDFcm_Command.CommandType: TPDFCommandType;
begin
  Result:=cmtGraphicState;
end;

{ TPDFTJCommand }

class function TPDFTJCommand.RegisterCommandName: String;
begin
  Result:='TJ';
end;

function TPDFTJCommand.GetFullText(aUnicodeMap: TPDFCMap): RawByteString;
Var
  i : integer;

begin
  if aUnicodeMap=Nil then
    Exit(GetFullText());
  Result:='';
  if Length(Tokens)>=2 then
    For I:=1 to Length(Tokens)-2 do
      case Tokens[I].TokenType of
      ptString,ptHexString:
        Result:=Result+aUnicodeMap.InterPret(Tokens[I].TokenData);
      ptNumber:
        if Abs(Tokens[i].AsDouble)>PDFTextArraySpaceTreshold then
          Result:=Result+' ';
      else
        Raise EConvertError.Create('Unexpected char');
      end;
end;

function TPDFTJCommand.GetFullText: RawByteString;

Var
  i : integer;

begin
  Result:='';
  if Length(Tokens)>=2 then
    For I:=1 to Length(Tokens)-2 do
      begin
      if Tokens[I].TokenType=ptString then
        Result:=Result+Tokens[I].TokenData
      else if Tokens[i].IsNumber then
        begin
        if Abs(Tokens[i].AsDouble)>PDFTextArraySpaceTreshold then
          Result:=Result+' '
        end
      else if Tokens[I].TokenType=ptHexString then
        Result:='' // Tokens[I].AsHexInteger
      else
        Raise EConvertError.Create('Unexpected char');
      end;
end;

{ TPDFETCommand }

class function TPDFETCommand.RegisterCommandName: String;
begin
  Result:='ET';
end;

{ TPDFEMCCommand }

class function TPDFEMCCommand.RegisterCommandName: String;
begin
  Result:='EMC';
end;

{ TPDFMPCommand }

class function TPDFMPCommand.RegisterCommandName: String;
begin
  Result:='MP';
end;

{ TPDFDPCommand }

class function TPDFDPCommand.RegisterCommandName: String;
begin
  Result:='DP';
end;


{ TPDFBTCommand }

class function TPDFBTCommand.RegisterCommandName: String;
begin
  Result:='BT';
end;

{ TPDFBDCCommand }

class function TPDFBDCCommand.RegisterCommandName: String;
begin
  Result:='BDC';
end;

{ TPDFBMCCommand }

class function TPDFBMCCommand.RegisterCommandName: String;
begin
  Result:='BMC';
end;

{ TPDFBXCommand }

class function TPDFBXCommand.RegisterCommandName: String;
begin
  Result:='BX';
end;

{ TPDFEXCommand }

class function TPDFEXCommand.RegisterCommandName: String;
begin
  Result:='EX';
end;

{ TPDFcmCommand }

class function TPDFcmCommand.RegisterCommandName: String;
begin
  Result:='cm';
end;

{ TPDFDCommand }

class function TPDFDoCommand.RegisterCommandName: String;
begin
  Result:='Do';
end;


initialization
  RegisterStandardCommands;
end.

