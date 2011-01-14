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

after all section end there is:

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

  { TvDXFVectorialReader }

  TvDXFVectorialReader = class(TvCustomVectorialReader)
  private
    LineStartX, LineStartY, LineStartZ: Double;
    LineEndX, LineEndY, LineEndZ: Double;
    function  SeparateString(AString: string; ASeparator: Char): T10Strings;
    function ReadSection(AStrings: TStrings; var AIndex: Integer; AData: TvVectorialDocument): Boolean;
    function ReadENTITIES(AStrings: TStrings; var AIndex: Integer; AData: TvVectorialDocument): Boolean;
    function ReadENTITIES_LINE(AStrings: TStrings; var AIndex: Integer; AData: TvVectorialDocument): Boolean;
    function  GetCoordinate(AStr: shortstring): Integer;
    function  GetCoordinateValue(AStr: shortstring): Double;
  public
    { General reading methods }
    procedure ReadFromStrings(AStrings: TStrings; AData: TvVectorialDocument); override;
  end;

implementation

{$define FPVECTORIALDEBUG}

const
  { Coordinate constants }

  INT_COORDINATE_NONE = 0;
  INT_COORDINATE_X = 1;
  INT_COORDINATE_Y = 2;
  INT_COORDINATE_Z = 3;

  { GCode constants }

  STR_GCODE_LINEAR_MOVE = 'G01';
  STR_GCODE_STEPPER_MOVE = 'S01';
  STR_GCODE_2DBEZIER_MOVE = 'B02';
  STR_GCODE_3DBEZIER_MOVE = 'B03';
  STR_GCODE_DRILL_UP = 'P01';
  STR_GCODE_DRILL_DOWN = 'P02';

{ TvAvisoCNCGCodeReader }

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

{@@
  returns   If an end of file marker was found
}
function TvDXFVectorialReader.ReadSection(
  AStrings: TStrings; var AIndex: Integer; AData: TvVectorialDocument): Boolean;
var
  DestX, DestY, DestZ: Double;
  StrSectionNum, StrSectionName: string;
  IntSectionNum, i: Integer;
begin
  Result := False;

  // Check if there is minimal space for a section
  if AIndex+5 > AStrings.Count then
  begin
    {$ifdef FPVECTORIALDEBUG}
    WriteLn('Not enough space for a section');
    {$endif}
    Exit(True);
  end;

  // Check of the EOF marker
  StrSectionName := Trim(AStrings.Strings[AIndex+1]);
  if StrSectionName = 'EOF' then
  begin
    {$ifdef FPVECTORIALDEBUG}
    WriteLn('EOF found');
    {$endif}
    Exit(True);
  end;

  // Now read and process the section name
  StrSectionNum := AStrings.Strings[AIndex+2];
  IntSectionNum := StrToInt(Trim(StrSectionNum));
  StrSectionName := AStrings.Strings[AIndex+3];

  {$ifdef FPVECTORIALDEBUG}
  WriteLn('TvDXFVectorialReader.ReadSection ' + StrSectionName);
  {$endif}

  if (StrSectionName = 'HEADER') or
     (StrSectionName = 'CLASSES') or
     (StrSectionName = 'TABLES') or
     (StrSectionName = 'BLOCKS') or
     (StrSectionName = 'OBJECTS') or
     (StrSectionName = 'THUMBNAILIMAGE') then
  begin
    // We don't care about contents here, so let's just find the last section and get out of here.
    for i := AIndex + 4 to AStrings.Count - 1 do
    begin
      if AStrings.Strings[i] = 'ENDSEC' then
      begin
        AIndex := i + 1;
        Exit;
      end;
    end;
    // If we reached here, the section in incomplete
    raise Exception.Create('TvDXFVectorialReader.ReadSection: ENDSEC was not found in the SECTION');
  end
  else if StrSectionName = 'ENTITIES' then
  begin
    AIndex := AIndex + 4;
    while not ReadENTITIES(AStrings, AIndex, AData) do ;
  end;
  {else
  begin
  end;}
end;

function TvDXFVectorialReader.ReadENTITIES(AStrings: TStrings;
  var AIndex: Integer; AData: TvVectorialDocument): Boolean;
var
  StrSectionNum, StrSectionName: string;
  IntSectionNum, i: Integer;
begin
  Result := False;

  // Now read and process the item name
  StrSectionName := AStrings.Strings[AIndex+1];

  {$ifdef FPVECTORIALDEBUG}
  WriteLn('TvDXFVectorialReader.ReadENTITIES ', StrSectionName);
  {$endif}

  if StrSectionName = 'ENDSEC' then
  begin
    Inc(AIndex, 2);
    Exit(True);
  end
  else if StrSectionName = 'LINE' then
  begin
    // Initial values
    LineStartX := 0;
    LineStartY := 0;
    LineStartZ := 0;
    LineEndX := 0;
    LineEndY := 0;
    LineEndZ := 0;

    // Read the data of the line
    Inc(AIndex, 2);
    while not ReadENTITIES_LINE(AStrings, AIndex, AData) do ;

    // And now write it
    {$ifdef FPVECTORIALDEBUG}
    WriteLn(Format('Adding Line from %f,%f to %f,%f', [LineStartX, LineStartY, LineEndX, LineEndY]));
    {$endif}
    AData.StartPath(LineStartX, LineStartY);
    AData.AddLineToPath(LineEndX, LineEndY);
    AData.EndPath();
  end;
end;

function TvDXFVectorialReader.ReadENTITIES_LINE(AStrings: TStrings;
  var AIndex: Integer; AData: TvVectorialDocument): Boolean;
var
  StrSectionNum, StrSectionValue: string;
  IntSectionNum: Integer;
  FloatSectionValue: double;
begin
  Result := False;

  // Now read and process the item name
  StrSectionNum := AStrings.Strings[AIndex];
  StrSectionValue := AStrings.Strings[AIndex+1];

  if (StrSectionValue = 'LINE') or
     (StrSectionValue = 'ENDSEC') then
  begin
    Exit(True);
  end
  else
  begin
    Inc(AIndex, 2);

    IntSectionNum := StrToInt(Trim(StrSectionNum));
    FloatSectionValue := StrToFloat(Trim(StrSectionValue));

    case IntSectionNum of
      10: LineStartX := FloatSectionValue;
      20: LineStartY := FloatSectionValue;
      30: LineStartZ := FloatSectionValue;
      11: LineEndX := FloatSectionValue;
      21: LineEndY := FloatSectionValue;
      31: LineEndZ := FloatSectionValue;
    end;
  end;
end;

function TvDXFVectorialReader.GetCoordinate(AStr: shortstring): Integer;
begin
  Result := INT_COORDINATE_NONE;

  if AStr = '' then Exit
  else if AStr[1] = 'X' then Result := INT_COORDINATE_X
  else if AStr[1] = 'Y' then Result := INT_COORDINATE_Y
  else if AStr[1] = 'Z' then Result := INT_COORDINATE_Z;
end;

function TvDXFVectorialReader.GetCoordinateValue(AStr: shortstring): Double;
begin
  Result := 0.0;

  if Length(AStr) <= 1 then Exit;

  Result := StrToFloat(Copy(AStr, 2, Length(AStr) - 1));
end;

{@@
  The information of each separate path is lost in G-Code files
  Only one path uniting all of them is created when reading G-Code
}
procedure TvDXFVectorialReader.ReadFromStrings(AStrings: TStrings;
  AData: TvVectorialDocument);
var
  i: Integer;
begin
  i := 0;
  while i < AStrings.Count - 1 do
    if ReadSection(AStrings, i, AData) then Break;
end;

initialization

  RegisterVectorialReader(TvDXFVectorialReader, vfDXF);

end.

