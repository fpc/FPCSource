{
Reads DXF files

License: The same modified LGPL as the Free Pascal RTL
         See the file COPYING.modifiedLGPL for more details

AUTHORS: Felipe Monteiro de Carvalho

DXF is composed by records written in ASCII with the following structure:

0
SECTION
section_number
SECTION_NAME
<data>
0
ENDSEC
0

after all sections there is:

EOF

}
unit dxfvectorialreader;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  fpvectorial;

type

  { Used by tcutils.SeparateString }
  T10Strings = array[0..9] of shortstring;

  TDXFToken = class;

  TDXFTokens = TFPList;// TDXFToken;

  TDXFToken = class
    GroupCode: Integer;
    StrValue: string;
    FloatValue: double;
    IntValue: Integer;
    Childs: TDXFTokens;
    constructor Create;
    Destructor Destroy; override;
  end;

  { TDXFTokenizer }

  TDXFTokenizer = class
  public
    Tokens: TDXFTokens;
    constructor Create;
    Destructor Destroy; override;
    procedure ReadFromStrings(AStrings: TStrings);
    function  IsENTITIES_Subsection(AStr: string): Boolean;
  end;

  { TvDXFVectorialReader }

  TvDXFVectorialReader = class(TvCustomVectorialReader)
  private
    //
    function  SeparateString(AString: string; ASeparator: Char): T10Strings;
    procedure ReadENTITIES(ATokens: TDXFTokens; AData: TvVectorialDocument);
    procedure ReadENTITIES_LINE(ATokens: TDXFTokens; AData: TvVectorialDocument);
    procedure ReadENTITIES_ARC(ATokens: TDXFTokens; AData: TvVectorialDocument);
    procedure ReadENTITIES_CIRCLE(ATokens: TDXFTokens; AData: TvVectorialDocument);
    procedure ReadENTITIES_ELLIPSE(ATokens: TDXFTokens; AData: TvVectorialDocument);
    procedure ReadENTITIES_TEXT(ATokens: TDXFTokens; AData: TvVectorialDocument);
    function  GetCoordinateValue(AStr: shortstring): Double;
  public
    { General reading methods }
    Tokenizer: TDXFTokenizer;
    constructor Create; override;
    Destructor Destroy; override;
    procedure ReadFromStrings(AStrings: TStrings; AData: TvVectorialDocument); override;
  end;

implementation

{$define FPVECTORIALDEBUG}

const
  // Group Codes for ENTITIES
  DXF_ENTITIES_TYPE = 0;
  DXF_ENTITIES_HANDLE = 5;
  DXF_ENTITIES_APPLICATION_GROUP = 102;
  DXF_ENTITIES_AcDbEntity = 100;
  DXF_ENTITIES_MODEL_OR_PAPER_SPACE = 67; // default=0=model, 1=paper
  DXF_ENTITIES_VISIBILITY = 60; // default=0 = Visible, 1 = Invisible

{ TDXFToken }

constructor TDXFToken.Create;
begin
  inherited Create;

  Childs := TDXFTokens.Create;
end;

destructor TDXFToken.Destroy;
begin
  Childs.Free;

  inherited Destroy;
end;

{ TDXFTokenizer }

constructor TDXFTokenizer.Create;
begin
  inherited Create;

  Tokens := TDXFTokens.Create;
end;

destructor TDXFTokenizer.Destroy;
begin
  Tokens.Free;

  inherited Destroy;
end;

procedure TDXFTokenizer.ReadFromStrings(AStrings: TStrings);
var
  i: Integer;
  StrSectionGroupCode, StrSectionName: string;
  IntSectionGroupCode: Integer;
  CurTokenBase, NextTokenBase, SectionTokenBase: TDXFTokens;
  NewToken: TDXFToken;
  ParserState: Integer;
begin
  //  Tokens.ForEachCall(); deletecallback
  Tokens.Clear;

  CurTokenBase := Tokens;
  NextTokenBase := Tokens;
  i := 0;
  ParserState := 0;

  while i < AStrings.Count - 1 do
  begin
    CurTokenBase := NextTokenBase;

    // Now read and process the section name
    StrSectionGroupCode := AStrings.Strings[i];
    IntSectionGroupCode := StrToInt(Trim(StrSectionGroupCode));
    StrSectionName := AStrings.Strings[i+1];

    NewToken := TDXFToken.Create;
    NewToken.GroupCode := IntSectionGroupCode;
    NewToken.StrValue := StrSectionName;

    // Waiting for a section
    if ParserState = 0 then
    begin
      if (StrSectionName = 'SECTION') then
      begin
        ParserState := 1;
        NextTokenBase := NewToken.Childs;
      end
      else if (StrSectionName = 'EOF') then
      begin
        Exit;
      end
      else
      begin
        raise Exception.Create(Format(
          'TDXFTokenizer.ReadFromStrings: Expected SECTION, but got: %s', [StrSectionname]));
      end;
    end
    // Processing the section name
    else if ParserState = 1 then
    begin
      if (StrSectionName = 'HEADER') or
        (StrSectionName = 'CLASSES') or
        (StrSectionName = 'TABLES') or
        (StrSectionName = 'BLOCKS') or
        (StrSectionName = 'OBJECTS') or
        (StrSectionName = 'THUMBNAILIMAGE') then
      begin
        ParserState := 2;
        SectionTokenBase := CurTokenBase;
      end
      else if (StrSectionName = 'ENTITIES') then
      begin
        ParserState := 3;
        SectionTokenBase := CurTokenBase;
      end
      else
      begin
        raise Exception.Create(Format(
          'TDXFTokenizer.ReadFromStrings: Invalid section name: %s', [StrSectionname]));
      end;
    end
    // Reading a generic section
    else if ParserState = 2 then
    begin
      if StrSectionName = 'ENDSEC' then
      begin
        ParserState := 0;
        CurTokenBase := SectionTokenBase;
        NextTokenBase := Tokens;
      end;
    end
    // Reading the ENTITIES section
    else if ParserState = 3 then
    begin
      if IsENTITIES_Subsection(StrSectionName) then
      begin
        CurTokenBase := SectionTokenBase;
        NextTokenBase := NewToken.Childs;
      end
      else if StrSectionName = 'ENDSEC' then
      begin
        ParserState := 0;
        CurTokenBase := SectionTokenBase;
        NextTokenBase := Tokens;
      end;
    end;

    CurTokenBase.Add(NewToken);

    Inc(i, 2);
  end;
end;

function TDXFTokenizer.IsENTITIES_Subsection(AStr: string): Boolean;
begin
  Result :=
    (AStr = '3DFACE') or
    (AStr = '3DSOLID') or
    (AStr = 'ACAD_PROXY_ENTITY') or
    (AStr = 'ARC') or
    (AStr = 'ATTDEF') or
    (AStr = 'ATTRIB') or
    (AStr = 'BODY') or
    (AStr = 'CIRCLE') or
    (AStr = 'DIMENSION') or
    (AStr = 'ELLIPSE') or
    (AStr = 'HATCH') or
    (AStr = 'IMAGE') or
    (AStr = 'INSERT') or
    (AStr = 'LEADER') or
    (AStr = 'LINE') or
    (AStr = 'LWPOLYLINE') or
    (AStr = 'MLINE') or
    (AStr = 'MTEXT') or
    (AStr = 'OLEFRAME') or
    (AStr = 'OLE2FRAME') or
    (AStr = 'POINT') or
    (AStr = 'POLYLINE') or
    (AStr = 'RAY') or
    (AStr = 'REGION') or
    (AStr = 'SEQEND') or
    (AStr = 'SHAPE') or
    (AStr = 'SOLID') or
    (AStr = 'SPLINE') or
    (AStr = 'TEXT') or
    (AStr = 'TOLERANCE') or
    (AStr = 'TRACE') or
    (AStr = 'VERTEX') or
    (AStr = 'VIEWPORT') or
    (AStr = 'XLINE');
end;

{ TvDXFVectorialReader }

{@@
  Reads a string and separates it in substring
  using ASeparator to delimite them.

  Limits:

  Number of substrings: 10 (indexed 0 to 9)
  Length of each substring: 255 (they are shortstrings)
}
function TvDXFVectorialReader.SeparateString(AString: string; ASeparator: Char): T10Strings;
var
  i, CurrentPart: Integer;
begin
  CurrentPart := 0;

  { Clears the result }
  for i := 0 to 9 do Result[i] := '';

  { Iterates througth the string, filling strings }
  for i := 1 to Length(AString) do
  begin
    if Copy(AString, i, 1) = ASeparator then
    begin
      Inc(CurrentPart);

      { Verifies if the string capacity wasn't exceeded }
      if CurrentPart > 9 then Exit;
    end
    else
      Result[CurrentPart] := Result[CurrentPart] + Copy(AString, i, 1);
  end;
end;

procedure TvDXFVectorialReader.ReadENTITIES(ATokens: TDXFTokens; AData: TvVectorialDocument);
var
  i: Integer;
  CurToken: TDXFToken;
begin
  for i := 0 to ATokens.Count - 1 do
  begin
    CurToken := TDXFToken(ATokens.Items[i]);
    if CurToken.StrValue = 'CIRCLE' then ReadENTITIES_CIRCLE(CurToken.Childs, AData)
    else if CurToken.StrValue = 'ELLIPSE' then ReadENTITIES_ELLIPSE(CurToken.Childs, AData)
    else if CurToken.StrValue = 'LINE' then ReadENTITIES_LINE(CurToken.Childs, AData)
    else if CurToken.StrValue = 'TEXT' then
    begin
      // ...
    end;
  end;
end;

procedure TvDXFVectorialReader.ReadENTITIES_LINE(ATokens: TDXFTokens; AData: TvVectorialDocument);
var
  CurToken: TDXFToken;
  i: Integer;
  // LINE
  LineStartX, LineStartY, LineStartZ: Double;
  LineEndX, LineEndY, LineEndZ: Double;
begin
  // Initial values
  LineStartX := 0;
  LineStartY := 0;
  LineStartZ := 0;
  LineEndX := 0;
  LineEndY := 0;
  LineEndZ := 0;

  for i := 0 to ATokens.Count - 1 do
  begin
    // Now read and process the item name
    CurToken := TDXFToken(ATokens.Items[i]);

    // Avoid an exception by previously checking if the conversion can be made
    if (CurToken.GroupCode = DXF_ENTITIES_HANDLE) or
      (CurToken.GroupCode = DXF_ENTITIES_AcDbEntity) then Continue;

    CurToken.FloatValue :=  StrToFloat(Trim(CurToken.StrValue));

    case CurToken.GroupCode of
      10: LineStartX := CurToken.FloatValue;
      20: LineStartY := CurToken.FloatValue;
      30: LineStartZ := CurToken.FloatValue;
      11: LineEndX := CurToken.FloatValue;
      21: LineEndY := CurToken.FloatValue;
      31: LineEndZ := CurToken.FloatValue;
    end;
  end;

  // And now write it
  {$ifdef FPVECTORIALDEBUG}
  WriteLn(Format('Adding Line from %f,%f to %f,%f', [LineStartX, LineStartY, LineEndX, LineEndY]));
  {$endif}
  AData.StartPath(LineStartX, LineStartY);
  AData.AddLineToPath(LineEndX, LineEndY);
  AData.EndPath();
end;

{
100 Subclass marker (AcDbCircle)
39 Thickness (optional; default = 0)
10 Center point (in OCS) DXF: X value; APP: 3D point
20, 30 DXF: Y and Z values of center point (in OCS)
40 Radius
100 Subclass marker (AcDbArc)
50 Start angle
51 End angle
210 Extrusion direction. (optional; default = 0, 0, 1) DXF: X value; APP: 3D vector
220, 230 DXF: Y and Z values of extrusion direction (optional)
}
procedure TvDXFVectorialReader.ReadENTITIES_ARC(ATokens: TDXFTokens;
  AData: TvVectorialDocument);
var
  CurToken: TDXFToken;
  i: Integer;
  CenterX, CenterY, CenterZ, Radius, StartAngle, EndAngle: Double;
begin
  CenterX := 0.0;
  CenterY := 0.0;
  CenterZ := 0.0;
  Radius := 0.0;
  StartAngle := 0.0;
  EndAngle := 0.0;

  for i := 0 to ATokens.Count - 1 do
  begin
    // Now read and process the item name
    CurToken := TDXFToken(ATokens.Items[i]);

    // Avoid an exception by previously checking if the conversion can be made
    if (CurToken.GroupCode = DXF_ENTITIES_HANDLE) or
      (CurToken.GroupCode = DXF_ENTITIES_AcDbEntity) then Continue;

    CurToken.FloatValue :=  StrToFloat(Trim(CurToken.StrValue));

    case CurToken.GroupCode of
      10: CenterX := CurToken.FloatValue;
      20: CenterY := CurToken.FloatValue;
      30: CenterZ := CurToken.FloatValue;
      40: Radius := CurToken.FloatValue;
    end;
  end;

  AData.AddCircularArc(CenterX, CenterY, CenterZ, Radius, StartAngle, EndAngle);
end;

{
Group codes	Description
100 Subclass marker (AcDbCircle)
39 Thickness (optional; default = 0)
10 Center point (in OCS) DXF: X value; APP: 3D point
20, 30 DXF: Y and Z values of center point (in OCS)
40 Radius
210 Extrusion direction (optional; default = 0, 0, 1) DXF: X value; APP: 3D vector
220, 230 DXF: Y and Z values of extrusion direction  (optional)
}
procedure TvDXFVectorialReader.ReadENTITIES_CIRCLE(ATokens: TDXFTokens;
  AData: TvVectorialDocument);
var
  CurToken: TDXFToken;
  i: Integer;
  CircleCenterX, CircleCenterY, CircleCenterZ, CircleRadius: Double;
begin
  CircleCenterX := 0.0;
  CircleCenterY := 0.0;
  CircleCenterZ := 0.0;
  CircleRadius := 0.0;

  for i := 0 to ATokens.Count - 1 do
  begin
    // Now read and process the item name
    CurToken := TDXFToken(ATokens.Items[i]);

    // Avoid an exception by previously checking if the conversion can be made
    if (CurToken.GroupCode = DXF_ENTITIES_HANDLE) or
      (CurToken.GroupCode = DXF_ENTITIES_AcDbEntity) then Continue;

    CurToken.FloatValue :=  StrToFloat(Trim(CurToken.StrValue));

    case CurToken.GroupCode of
      10: CircleCenterX := CurToken.FloatValue;
      20: CircleCenterY := CurToken.FloatValue;
      30: CircleCenterZ := CurToken.FloatValue;
      40: CircleRadius := CurToken.FloatValue;
    end;
  end;

  AData.AddCircle(CircleCenterX, CircleCenterY,
    CircleCenterZ, CircleRadius);
end;

{
100 Subclass marker (AcDbEllipse)
10 Center point (in WCS) DXF: X value; APP: 3D point
20, 30 DXF: Y and Z values of center point (in WCS)
11 Endpoint of major axis, relative to the center (in WCS) DXF: X value; APP: 3D point
21, 31 DXF: Y and Z values of endpoint of major axis, relative to the center (in WCS)
210 Extrusion direction (optional; default = 0, 0, 1) DXF: X value; APP: 3D vector
220, 230 DXF: Y and Z values of extrusion direction  (optional)
40 Ratio of minor axis to major axis
41 Start parameter (this value is 0.0 for a full ellipse)
42 End parameter (this value is 2pi for a full ellipse)
}
procedure TvDXFVectorialReader.ReadENTITIES_ELLIPSE(ATokens: TDXFTokens;
  AData: TvVectorialDocument);
var
  CurToken: TDXFToken;
  i: Integer;
  CenterX, CenterY, CenterZ, MajorHalfAxis, MinorHalfAxis, Angle: Double;
begin
  for i := 0 to ATokens.Count - 1 do
  begin
    // Now read and process the item name
    CurToken := TDXFToken(ATokens.Items[i]);

    // Avoid an exception by previously checking if the conversion can be made
    if (CurToken.GroupCode = DXF_ENTITIES_HANDLE) or
      (CurToken.GroupCode = DXF_ENTITIES_AcDbEntity) then Continue;

    CurToken.FloatValue :=  StrToFloat(Trim(CurToken.StrValue));

    case CurToken.GroupCode of
      10: CenterX := CurToken.FloatValue;
      20: CenterY := CurToken.FloatValue;
      30: CenterZ := CurToken.FloatValue;
    end;

  end;

  //
  AData.AddEllipse(CenterX, CenterY, CenterZ, MajorHalfAxis, MinorHalfAxis, Angle);
end;

{
100 Subclass marker (AcDbText)
39 Thickness (optional; default = 0)
10 First alignment point (in OCS) DXF: X value; APP: 3D point
20, 30 DXF: Y and Z values of first alignment point (in OCS)
40 Text height
1 Default value (the string itself)
50 Text rotation (optional; default = 0)
41 Relative X scale factor-width (optional; default = 1)
  This value is also adjusted when fit-type text is used.
51 Oblique angle (optional; default = 0)
7 Text style name (optional, default = STANDARD)
71 Text generation flags (optional, default = 0):
  2 = Text is backward (mirrored in X).
  4 = Text is upside down (mirrored in Y).
72 Horizontal text justification type (optional, default = 0) integer codes (not bit-coded)
  0 = Left; 1= Center; 2 = Right
  3 = Aligned (if vertical alignment = 0)
  4 = Middle (if vertical alignment = 0)
  5 = Fit (if vertical alignment = 0)
  See the Group 72 and 73 integer codes table for clarification.
11 Second alignment point (in OCS) (optional)
  DXF: X value; APP: 3D point
  This value is meaningful only if the value of a 72 or 73 group is nonzero (if the justification is anything other than baseline/left).
21, 31 DXF: Y and Z values of second alignment point (in OCS) (optional)
210 Extrusion direction (optional; default = 0, 0, 1)
  DXF: X value; APP: 3D vector
220, 230 DXF: Y and Z values of extrusion direction (optional)
73 Vertical text justification type (optional, default = 0): integer codes (not bit- coded):
  0 = Baseline; 1 = Bottom; 2 = Middle; 3 = Top
  See the Group 72 and 73 integer codes table for clarification.
}
procedure TvDXFVectorialReader.ReadENTITIES_TEXT(ATokens: TDXFTokens;
  AData: TvVectorialDocument);
var
  CurToken: TDXFToken;
  i: Integer;
  PosX, PosY, PosZ: Double;
  Str: string;
begin
  for i := 0 to ATokens.Count - 1 do
  begin
    // Now read and process the item name
    CurToken := TDXFToken(ATokens.Items[i]);

    // Avoid an exception by previously checking if the conversion can be made
    if (CurToken.GroupCode = DXF_ENTITIES_HANDLE) or
      (CurToken.GroupCode = 1) or
      (CurToken.GroupCode = DXF_ENTITIES_AcDbEntity) then Continue;

    CurToken.FloatValue :=  StrToFloat(Trim(CurToken.StrValue));

    case CurToken.GroupCode of
      1:  Str := CurToken.StrValue;
      10: PosX := CurToken.FloatValue;
      20: PosY := CurToken.FloatValue;
      30: PosZ := CurToken.FloatValue;
    end;

  end;

  //
//  AData.AddEllipse(CenterX, CenterY, CenterZ, MajorHalfAxis, MinorHalfAxis, Angle);
end;

function TvDXFVectorialReader.GetCoordinateValue(AStr: shortstring): Double;
begin
  Result := 0.0;

{  if Length(AStr) <= 1 then Exit;

  Result := StrToFloat(Copy(AStr, 2, Length(AStr) - 1));}
end;

constructor TvDXFVectorialReader.Create;
begin
  inherited Create;

  Tokenizer := TDXFTokenizer.Create;
end;

destructor TvDXFVectorialReader.Destroy;
begin
  Tokenizer.Free;

  inherited Destroy;
end;

{@@
  The information of each separate path is lost in G-Code files
  Only one path uniting all of them is created when reading G-Code
}
procedure TvDXFVectorialReader.ReadFromStrings(AStrings: TStrings;
  AData: TvVectorialDocument);
var
  i: Integer;
  CurToken, CurTokenFirstChild: TDXFToken;
begin
  Tokenizer.ReadFromStrings(AStrings);

  for i := 0 to Tokenizer.Tokens.Count - 1 do
  begin
    CurToken := TDXFToken(Tokenizer.Tokens.Items[i]);
    CurTokenFirstChild := TDXFToken(CurToken.Childs.Items[0]);

    if CurTokenFirstChild.StrValue = 'ENTITIES' then
      ReadENTITIES(CurToken.Childs, AData);
  end;
end;

initialization

  RegisterVectorialReader(TvDXFVectorialReader, vfDXF);

end.

