unit fppdf_test;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils
  {$ifdef fptest}
  ,TestFramework
  {$else}
  ,fpcunit, testutils, testregistry
  {$endif}
  ,fppdf
  ;

type

  TBasePDFTest = class(TTestCase)
  private
    FPDF: TPDFDocument;
    FStream: TStringStream;
  protected
    procedure   SetUp; override;
    procedure   TearDown; override;
  public
    property    PDF: TPDFDocument read FPDF;
    property    S: TStringStream read FStream;
  end;


  TTestPDFObject = class(TBasePDFTest)
  published
    procedure   TestFloatStr;
    procedure   TestWriteString;
  end;


  TTestTPDFDocumentObject = class(TBasePDFTest)
  published
    procedure   TestSetWidth;
  end;


  TTestPDFBoolean = class(TBasePDFTest)
  published
    procedure   TestWriteTrue;
    procedure   TestWriteFalse;
  end;


  TTestPDFMoveTo = class(TBasePDFTest)
  published
    procedure   TestCommandPDFFloat;
    procedure   TestCommandPDFCoord;
  end;


  TTestPDFInteger = class(TBasePDFTest)
  published
    procedure   TestWrite;
  end;


  TTestPDFReference = class(TBasePDFTest)
  published
    procedure   TestWrite;
  end;


  TTestPDFName = class(TBasePDFTest)
  published
    procedure   TestWrite;
    procedure   TestValidNames1;
    procedure   TestValidNames2;
  end;


  TTestPDFAbstractString = class(TBasePDFTest)
  published
    procedure   TestInsertEscape;
  end;

  TTestPDFString = class(TBasePDFTest)
  published
    procedure   TestWrite;
    procedure   TestWriteEscaped;
    procedure   TestWriteEscaped2;
  end;


  TTestPDFArray = class(TBasePDFTest)
  published
    procedure   TestWrite;
  end;


  TTestPDFStream = class(TBasePDFTest)
  published
    procedure   TestWrite;
  end;


  TTestPDFEmbeddedFont = class(TBasePDFTest)
  published
    procedure   TestWrite;
    procedure   TestWriteEmbeddedFont;
  end;


  TTestPDFText = class(TBasePDFTest)
  published
    procedure   TestWrite;
  end;


  TTestPDFLineSegment = class(TBasePDFTest)
  published
    procedure   TestCommand;
    procedure   TestWrite;
  end;


  TTestTPDFRectangle = class(TBasePDFTest)
  published
    procedure   TestWrite_NoFill_NoStroke;
    procedure   TestWrite_Fill_Stroke;
    procedure   TestWrite_NoFill_Stroke;
    procedure   TestWrite_Fill_NoStroke;
  end;


  TTestPDFCurveC = class(TBasePDFTest)
  published
    procedure   TestCommand;
    procedure   TestWrite_Stroke;
    procedure   TestWrite_NoStroke;
  end;


  TTestPDFCurveV = class(TBasePDFTest)
  published
    procedure   TestWrite_Stroke;
    procedure   TestWrite_NoStroke;
  end;


  TTestPDFCurveY = class(TBasePDFTest)
  published
    procedure   TestWrite_Stroke;
    procedure   TestWrite_NoStroke;
  end;


  TTestPDFEllipse = class(TBasePDFTest)
  published
    procedure   TestWrite_NoFill_NoStroke;
    procedure   TestWrite_Fill_NoStroke;
    procedure   TestWrite_NoFill_Stroke;
    procedure   TestWrite_Fill_Stroke;
  end;


  TTestPDFSurface = class(TBasePDFTest)
  published
    procedure   TestWrite;
    procedure   TestWrite_noFill;
    procedure   TestWrite_noClose;
  end;


  TTestPDFImage = class(TBasePDFTest)
  published
    procedure   TestWrite;
  end;


  TTestPDFLineStyle = class(TBasePDFTest)
  published
    procedure   TestWrite_ppsSolid;
    procedure   TestWrite_ppsDash;
    procedure   TestWrite_ppsDot;
    procedure   TestWrite_ppsDashDot;
    procedure   TestWrite_ppsDashDotDot;
  end;


  TTestPDFColor = class(TBasePDFTest)
  published
    procedure   TestWrite_Stroke;
    procedure   TestWrite_noStroke;
  end;


  TTestPDFDictionaryItem = class(TBasePDFTest)
  published
    procedure   TestWrite;
  end;


  TTestPDFDictionary = class(TBasePDFTest)
  published
    procedure   TestWrite;
  end;


  TTestPDFXRef = class(TBasePDFTest)
  published
    procedure   TestWrite;
  end;


  TTestPDFPage = class(TBasePDFTest)
  published
    procedure   TestPageDocument;
    procedure   TestPageDefaultUnitOfMeasure;
    procedure   TestMatrix;
    procedure   TestUnitOfMeasure_MM;
    procedure   TestUnitOfMeasure_Inches;
    procedure   TestUnitOfMeasure_CM;
  end;


  TTestCompressionDecompression = class(TTestCase)
  private
    function  GetTestString: string;
  published
    procedure TestStreamCompressionDecompression;
    procedure TestStringCompressionDecompression;
  end;


  TTestTPDFImageItem = class(TTestCase)
  published
    procedure TestCreateStreamedData;
  end;

implementation

uses
  FPImage;

type
  // so we can access Protected methods in the tests
  TMockPDFObject = class(TPDFObject);
  TMockPDFDocumentObject = class(TPDFDocumentObject);
  TMockPDFBoolean = class(TPDFBoolean);
  TMockPDFMoveTo = class(TPDFMoveTo);
  TMockPDFInteger = class(TPDFInteger);
  TMockPDFReference = class(TPDFReference);
  TMockPDFName = class(TPDFName);
  TMockPDFString = class(TPDFString);
  TMockPDFArray = class(TPDFArray);
  TMockPDFStream = class(TPDFStream);
  TMockPDFEmbeddedFont = class(TPDFEmbeddedFont);
  TMockPDFText = class(TPDFText);
  TMockPDFLineSegment = class(TPDFLineSegment);
  TMockPDFRectangle = class(TPDFRectangle);
  TMockPDFCurveC = class(TPDFCurveC);
  TMockPDFCurveV = class(TPDFCurveV);
  TMockPDFCurveY = class(TPDFCurveY);
  TMockPDFEllipse = class(TPDFEllipse);
  TMockPDFSurface = class(TPDFSurface);
  TMockPDFImage = class(TPDFImage);
  TMockPDFLineStyle = class(TPDFLineStyle);
  TMockPDFColor = class(TPDFColor);
  TMockPDFDictionaryItem = class(TPDFDictionaryItem);
  TMockPDFDictionary = class(TPDFDictionary);
  TMockPDFXRef = class(TPDFXRef);
  TMockPDFPage = class(TPDFPage);


{ TBasePDFTest }

procedure TBasePDFTest.SetUp;
begin
  inherited SetUp;
  FPDF := TPDFDocument.Create(nil);
  FStream := TStringStream.Create('');
end;

procedure TBasePDFTest.TearDown;
begin
  FStream.Free;
  FPDF.Free;
  inherited TearDown;
end;

{ TTestPDFObject }

procedure TTestPDFObject.TestFloatStr;

Var
  C : Char;

begin
  AssertEquals('Failed on 1', '0.12', TMockPDFObject.FloatStr(TPDFFLoat(0.12)));
  AssertEquals('Failed on 2', '  12', TMockPDFObject.FloatStr(TPDFFLoat(12.00)));
  AssertEquals('Failed on 3', '12.30', TMockPDFObject.FloatStr(TPDFFLoat(12.30)));
  AssertEquals('Failed on 4', '12.34', TMockPDFObject.FloatStr(TPDFFLoat(12.34)));
  AssertEquals('Failed on 5', '123.45', TMockPDFObject.FloatStr(TPDFFLoat(123.45)));
  AssertEquals('Failed on 6', '123.46', TMockPDFObject.FloatStr(TPDFFLoat(123.455)));
  AssertEquals('Failed on 7', '123.46', TMockPDFObject.FloatStr(TPDFFLoat(123.456)));
  AssertEquals('Failed on 8', '1234567.00', TMockPDFObject.FloatStr(TPDFFLoat(1234567)));
  // Set DecimalSeparator
  C:=FormatSettings.DecimalSeparator;
  FormatSettings.DecimalSeparator:=',';
  try
    AssertEquals('Failed on 9', '12.34', TMockPDFObject.FloatStr(TPDFFLoat(12.34)));
  finally
    FormatSettings.DecimalSeparator:=C;
  end;
  // Set ThousandSeparator
  C:=FormatSettings.ThousandSeparator;
  FormatSettings.ThousandSeparator:=' ';
  try
    AssertEquals('Failed on 10', '1234567.00', TMockPDFObject.FloatStr(TPDFFLoat(1234567)));
  finally
    FormatSettings.ThousandSeparator:=C;
  end;
end;

procedure TTestPDFObject.TestWriteString;
var
  o: TMockPDFObject;
begin
  o := TMockPDFObject.Create(PDF);
  try
    o.WriteString('Hello', S);
    AssertEquals('Failed on 1', 'Hello', s.DataString);
  finally
    o.Free;
  end;
end;

{ TTestTPDFDocumentObject }

procedure TTestTPDFDocumentObject.TestSetWidth;
var
  o: TMockPDFDocumentObject;
begin
  o := TMockPDFDocumentObject.Create(PDF);
  try
    o.SetWidth(TPDFFloat(300.5), S);
    AssertEquals('Failed on 1',
      '1 J'+CRLF+
      '300.50 w'+CRLF,             // line width
      s.DataString);

    // this shouldn't cause any change
    o.SetWidth(TPDFFloat(300.5), S);
    AssertEquals('Failed on 2',
      '1 J'+CRLF+
      '300.50 w'+CRLF,             // line width
      s.DataString);

    // but this will
    o.SetWidth(TPDFFloat(123), S);
    AssertEquals('Failed on 3',
      '1 J'+CRLF+
      '300.50 w'+CRLF+           // line width 300.5
      '1 J'+CRLF+
      ' 123 w'+CRLF,             // line width 123
      s.DataString);
  finally
    o.Free;
  end;
end;

{ TTestPDFBoolean }

procedure TTestPDFBoolean.TestWriteTrue;
var
  o: TPDFBoolean;
begin
  o := TPDFBoolean.Create(PDF, True);
  try
    AssertEquals('Failed on 1', '', S.DataString);
    TMockPDFBoolean(o).Write(S);
    AssertEquals('Failed on 2', 'true', S.DataString);
  finally
    o.Free;
  end;
end;

procedure TTestPDFBoolean.TestWriteFalse;
var
  o: TPDFBoolean;
begin
  o := TPDFBoolean.Create(PDF, False);
  try
    AssertEquals('Failed on 1', '', S.DataString);
    TMockPDFBoolean(o).Write(S);
    AssertEquals('Failed on 2', 'false', S.DataString);
  finally
    o.Free;
  end;
end;

{ TTestPDFMoveTo }

procedure TTestPDFMoveTo.TestCommandPDFFloat;
var
  o: TPDFMoveTo;
begin
  o := TPDFMoveTo.Create(PDF, 10, 20);
  try
    AssertEquals('Failed on 1', '', S.DataString);
    TMockPDFMoveTo(o).Write(S);
    AssertEquals('Failed on 2', '  10   20 m'+CRLF, S.DataString);
  finally
    o.Free;
  end;
end;

procedure TTestPDFMoveTo.TestCommandPDFCoord;
var
  c: TPDFCoord;
  o: TPDFMoveTo;
begin
  c.X := 10;
  c.Y := 20;
  o := TPDFMoveTo.Create(PDF, c);
  try
    AssertEquals('Failed on 1', '', S.DataString);
    TMockPDFMoveTo(o).Write(S);
    AssertEquals('Failed on 2', '  10   20 m'+CRLF, S.DataString);
  finally
    o.Free;
  end;
end;

{ TTestPDFInteger }

procedure TTestPDFInteger.TestWrite;
var
  o: TPDFInteger;
begin
  o := TPDFInteger.Create(PDF, 15);
  try
    AssertEquals('Failed on 1', '', S.DataString);
    TMockPDFInteger(o).Write(S);
    AssertEquals('Failed on 2', '15', S.DataString);
    TMockPDFInteger(o).inc;
    TMockPDFInteger(o).Write(S);
    AssertEquals('Failed on 3', '1516', S.DataString);
  finally
    o.Free;
  end;
end;

{ TTestPDFReference }

procedure TTestPDFReference.TestWrite;
var
  o: TPDFReference;
begin
  o := TPDFReference.Create(PDF, 10);
  try
    AssertEquals('Failed on 1', '', S.DataString);
    TMockPDFReference(o).Write(S);
    AssertEquals('Failed on 2', '10 0 R', S.DataString);
  finally
    o.Free;
  end;
end;

{ TTestPDFName }

procedure TTestPDFName.TestWrite;
var
  o: TPDFName;
begin
  o := TPDFName.Create(PDF, 'Test');
  try
    AssertEquals('Failed on 1', '', S.DataString);
    TMockPDFName(o).Write(S);
    AssertEquals('Failed on 2', '/Test', S.DataString);
  finally
    o.Free;
  end;

  { Length1 seems to be a special case? }
  o := TPDFName.Create(PDF, 'Length1');
  try
    TMockPDFName(o).Write(S);
    AssertEquals('Failed on 2', '/Test/Length1', S.DataString);
  finally
    o.Free;
  end;
end;

procedure TTestPDFName.TestValidNames1;
var
  o: TPDFName;
begin
  o := TPDFName.Create(PDF, 'paired()parentheses');
  try
    AssertEquals('Failed on 1', '', S.DataString);
    TMockPDFName(o).Write(S);
    AssertEquals('Failed on 2', '/paired()parentheses', S.DataString);
  finally
    o.Free;
  end;
end;

procedure TTestPDFName.TestValidNames2;
var
  o: TPDFName;
begin
  o := TPDFName.Create(PDF, 'Adobe Green');
  try
    AssertEquals('Failed on 1', '', S.DataString);
    TMockPDFName(o).Write(S);
    AssertEquals('Failed on 2', '/Adobe Green', S.DataString);
  finally
    o.Free;
  end;
end;


{ TTestPDFAbstractString }

procedure TTestPDFAbstractString.TestInsertEscape;
var
  o: TPDFAbstractString;
begin
  o := TPDFAbstractString.Create(PDF);
  try
    AssertEquals('Failed on 1', 'abcdefg', TMockPDFString(o).InsertEscape('abcdefg'));
    AssertEquals('Failed on 2', 'a\\b/cdefg', TMockPDFString(o).InsertEscape('a\b/cdefg'));
    AssertEquals('Failed on 3', 'a\(b\)cdefg', TMockPDFString(o).InsertEscape('a(b)cdefg'));
    AssertEquals('Failed on 4', 'a\(b\)c\\def/g', TMockPDFString(o).InsertEscape('a(b)c\def/g'));
  finally
    o.Free;
  end;
end;

{ TTestPDFString }

procedure TTestPDFString.TestWrite;
var
  o: TPDFString;
begin
  PDF.Options := []; // disable all compression
  o := TPDFString.Create(PDF, 'Test');
  try
    AssertEquals('Failed on 1', '', S.DataString);
    TMockPDFString(o).Write(S);
    AssertEquals('Failed on 2', '(Test)', S.DataString);
  finally
    o.Free;
  end;

  { Length1 seems to be a special case? }
  o := TPDFString.Create(PDF, #$C2#$A3+#$C2#$BB); //  UTF-8 text of "£»"
  try
    TMockPDFString(o).Write(S);  // write will convert UTF-8 to ANSI
    AssertEquals('Failed on 3', '(Test)('+#163#187+')', S.DataString);
  finally
    o.Free;
  end;
end;

{ The symbols ( ) and \ get escaped before written to PDF }
procedure TTestPDFString.TestWriteEscaped;
var
  o: TPDFString;
begin
  o := TPDFString.Create(PDF, 'a(b)c\def/g');
  try
    AssertEquals('Failed on 1', '', S.DataString);
    TMockPDFString(o).Write(S);
    AssertEquals('Failed on 2', '(a\(b\)c\\def/g)', S.DataString);
  finally
    o.Free;
  end;
end;

procedure TTestPDFString.TestWriteEscaped2;
var
  o: TPDFString;
begin
  o := TPDFString.Create(PDF, 'Special characters (*!&}^% and so on).');
  try
    AssertEquals('Failed on 1', '', S.DataString);
    TMockPDFString(o).Write(S);
    AssertEquals('Failed on 2', '(Special characters \(*!&}^% and so on\).)', S.DataString);
  finally
    o.Free;
  end;
end;

{ TTestPDFArray }

procedure TTestPDFArray.TestWrite;
var
  o: TPDFArray;
begin
  o := TPDFArray.Create(PDF);
  try
    AssertEquals('Failed on 1', '', S.DataString);
    TMockPDFArray(o).AddIntArray('1 2 3 4');  // no trailing space in string
    TMockPDFArray(o).Write(S);
    AssertEquals('Failed on 2', '[1 2 3 4]', S.DataString);

    TMockPDFArray(o).AddIntArray('1 2 3 4 ');  // now we have a trailing space
    TMockPDFArray(o).Write(S);
    AssertEquals('Failed on 3', '[1 2 3 4][1 2 3 4 1 2 3 4]', S.DataString);
  finally
    o.Free;
  end;
end;

{ TTestPDFStream }

procedure TTestPDFStream.TestWrite;
var
  o: TPDFStream;
begin
  o := TPDFStream.Create(PDF, True);
  try
    TMockPDFStream(o).AddItem(TPDFString.Create(PDF, 'Hello World'));
    AssertEquals('Failed on 1', '', S.DataString);
    TMockPDFStream(o).Write(S);
    AssertEquals('Failed on 2', '(Hello World)', S.DataString);
    TMockPDFStream(o).AddItem(TPDFString.Create(PDF, '12345'));
    TMockPDFStream(o).Write(S);
    AssertEquals('Failed on 3', '(Hello World)(Hello World)(12345)', S.DataString);
  finally
    o.Free;
  end;
end;

{ TTestPDFEmbeddedFont }

procedure TTestPDFEmbeddedFont.TestWrite;
var
  o: TPDFEmbeddedFont;
begin
  o := TPDFEmbeddedFont.Create(PDF, 1, '16');
  try
    AssertEquals('Failed on 1', '', S.DataString);
    TMockPDFEmbeddedFont(o).Write(S);
    AssertEquals('Failed on 2', '/F1 16 Tf'+CRLF, S.DataString);  // DON't change CRLF to anything else
  finally
    o.Free;
  end;
end;

procedure TTestPDFEmbeddedFont.TestWriteEmbeddedFont;
var
  o: TPDFEmbeddedFont;
  lStream: TMemoryStream;
  str: String;
begin
  PDF.Options := []; // disable compressed fonts
  str := 'Hello World';
  o := TPDFEmbeddedFont.Create(PDF, 1, '16');
  try
    AssertEquals('Failed on 1', '', S.DataString);
    lStream := TMemoryStream.Create;
    lStream.Write(str[1], Length(str));
    TMockPDFEmbeddedFont(o).WriteEmbeddedFont(PDF, lStream, S);
    lStream.Free;
    // DON't change CRLF to anything else
    AssertEquals('Failed on 2', CRLF+'stream'+CRLF+'Hello World'+CRLF+'endstream', S.DataString);
  finally
    o.Free;
  end;
end;

{ TTestPDFText }

procedure TTestPDFText.TestWrite;
var
  o: TPDFText;
  x, y: TPDFFloat;
begin
  x := 10.5;
  y := 20.0;
  o := TPDFText.Create(PDF, x, y, 'Hello World!');
  try
    AssertEquals('Failed on 1', '', S.DataString);
    TMockPDFText(o).Write(S);
    AssertEquals('Failed on 2',
      'BT'+CRLF+
      '10.50   20 TD'+CRLF+
      '(Hello World!) Tj'+CRLF+
      'ET'+CRLF,
      S.DataString);
  finally
    o.Free;
  end;
end;

{ TTestPDFLineSegment }

procedure TTestPDFLineSegment.TestCommand;
var
  pos: TPDFCoord;
begin
  pos.X := 10.0;
  pos.Y := 55.5;
  AssertEquals('Failed on 1', '  10 55.50 l'+CRLF, TPDFLineSegment.Command(pos));
end;

procedure TTestPDFLineSegment.TestWrite;
var
  o: TPDFLineSegment;
  Width, X1,Y1, X2,Y2: TPDFFLoat;
begin
  Width := 2.0;
  X1 := 10.0;
  Y1 := 15.5;
  X2 := 50.0;
  Y2 := 55.5;
  o := TPDFLineSegment.Create(PDF, Width, X1, Y1, X2, Y2);
  try
    AssertEquals('Failed on 1', '', S.DataString);
    TMockPDFLineSegment(o).Write(S);
    AssertEquals('Failed on 2',
      '1 J'+CRLF+
      '   2 w'+CRLF+             // line width
      '  10 15.50 m'+CRLF+       // moveto command
      '  50 55.50 l'+CRLF+       // line segment
      'S'+CRLF,               // end line segment
      S.DataString);
  finally
    o.Free;
  end;
end;

{ TTestTPDFRectangle }

procedure TTestTPDFRectangle.TestWrite_NoFill_NoStroke;
var
  o: TMockPDFRectangle;
  lPosX, lPosY, lWidth, lHeight, lLineWidth: TPDFFLoat;
begin
  lPosX := 10;
  lPosY := 11;
  lWidth := 100;
  lHeight := 200;
  lLineWidth := 1;
  o := TMockPDFRectangle.Create(PDF, lPosX, lPosY, lWidth, lHeight, lLineWidth, False, False);
  try
    AssertEquals('Failed on 1', '', S.DataString);
    o.Write(S);
    AssertEquals('Failed on 2',
      '  10   11  100  200 re'+CRLF,
      S.DataString);
  finally
    o.Free;
  end;
end;

procedure TTestTPDFRectangle.TestWrite_Fill_Stroke;
var
  o: TMockPDFRectangle;
  lPosX, lPosY, lWidth, lHeight, lLineWidth: TPDFFLoat;
begin
  lPosX := 10;
  lPosY := 11;
  lWidth := 100;
  lHeight := 200;
  lLineWidth := 2;
  o := TMockPDFRectangle.Create(PDF, lPosX, lPosY, lWidth, lHeight, lLineWidth, True, True);
  try
    AssertEquals('Failed on 1', '', S.DataString);
    o.Write(S);
    AssertEquals('Failed on 2',
      '1 J'+CRLF+
      '   2 w'+CRLF+
      '  10   11  100  200 re'+CRLF+
      'b'+CRLF,
      S.DataString);
  finally
    o.Free;
  end;
end;

procedure TTestTPDFRectangle.TestWrite_NoFill_Stroke;
var
  o: TMockPDFRectangle;
  lPosX, lPosY, lWidth, lHeight, lLineWidth: TPDFFLoat;
begin
  lPosX := 10;
  lPosY := 11;
  lWidth := 100;
  lHeight := 200;
  lLineWidth := 2;
  o := TMockPDFRectangle.Create(PDF, lPosX, lPosY, lWidth, lHeight, lLineWidth, False, True);
  try
    AssertEquals('Failed on 1', '', S.DataString);
    o.Write(S);
    AssertEquals('Failed on 2',
      '1 J'+CRLF+
      '   2 w'+CRLF+
      '  10   11  100  200 re'+CRLF+
      'S'+CRLF,
      S.DataString);
  finally
    o.Free;
  end;
end;

procedure TTestTPDFRectangle.TestWrite_Fill_NoStroke;
var
  o: TMockPDFRectangle;
  lPosX, lPosY, lWidth, lHeight, lLineWidth: TPDFFLoat;
begin
  lPosX := 10;
  lPosY := 11;
  lWidth := 100;
  lHeight := 200;
  lLineWidth := 2;
  o := TMockPDFRectangle.Create(PDF, lPosX, lPosY, lWidth, lHeight, lLineWidth, True, False);
  try
    AssertEquals('Failed on 1', '', S.DataString);
    o.Write(S);
    AssertEquals('Failed on 2',
      '  10   11  100  200 re'+CRLF+
      'f'+CRLF,
      S.DataString);
  finally
    o.Free;
  end;
end;

{ TTestPDFCurveC }

procedure TTestPDFCurveC.TestCommand;
var
  X1,Y1: TPDFFloat;
  X2,Y2: TPDFFloat;
  X3,Y3: TPDFFloat;
  s1: string;
begin
  X1 := 10;
  Y1 := 11;
  X2 := 100;
  Y2 := 9;
  X3 := 200;
  Y3 := 250;
  s1 := TMockPDFCurveC.Command(x1, y1, x2, y2, x3, y3);
  AssertEquals('Failed on 1', '  10   11  100    9  200  250 c'+CRLF, s1);
end;

procedure TTestPDFCurveC.TestWrite_Stroke;
var
  o: TMockPDFCurveC;
  X1,Y1: TPDFFloat;
  X2,Y2: TPDFFloat;
  X3,Y3: TPDFFloat;
  lLineWidth: TPDFFLoat;
begin
  X1 := 10;
  Y1 := 11;
  X2 := 100;
  Y2 := 9;
  X3 := 200;
  Y3 := 250;
  lLineWidth := 2;
  o := TMockPDFCurveC.Create(PDF, x1, y1, x2, y2, x3, y3, lLineWidth, True);
  try
    AssertEquals('Failed on 1', '', S.DataString);
    o.Write(S);
    AssertEquals('Failed on 2',
      '1 J'+CRLF+
      '   2 w'+CRLF+
      '  10   11  100    9  200  250 c'+CRLF+
      'S'+CRLF,
      S.DataString);
  finally
    o.Free;
  end;
end;

procedure TTestPDFCurveC.TestWrite_NoStroke;
var
  o: TMockPDFCurveC;
  X1,Y1: TPDFFloat;
  X2,Y2: TPDFFloat;
  X3,Y3: TPDFFloat;
  lLineWidth: TPDFFLoat;
begin
  X1 := 10;
  Y1 := 11;
  X2 := 100;
  Y2 := 9;
  X3 := 200;
  Y3 := 250;
  lLineWidth := 2;
  o := TMockPDFCurveC.Create(PDF, x1, y1, x2, y2, x3, y3, lLineWidth, False);
  try
    AssertEquals('Failed on 1', '', S.DataString);
    o.Write(S);
    AssertEquals('Failed on 2',
      '  10   11  100    9  200  250 c'+CRLF,
      S.DataString);
  finally
    o.Free;
  end;
end;

{ TTestPDFCurveV }

procedure TTestPDFCurveV.TestWrite_Stroke;
var
  o: TMockPDFCurveV;
  X2,Y2: TPDFFloat;
  X3,Y3: TPDFFloat;
  lLineWidth: TPDFFLoat;
begin
  X2 := 100;
  Y2 := 9;
  X3 := 200;
  Y3 := 250;
  lLineWidth := 2;
  o := TMockPDFCurveV.Create(PDF, x2, y2, x3, y3, lLineWidth, True);
  try
    AssertEquals('Failed on 1', '', S.DataString);
    o.Write(S);
    AssertEquals('Failed on 2',
      '1 J'+CRLF+
      '   2 w'+CRLF+
      ' 100    9  200  250 v'+CRLF+
      'S'+CRLF,
      S.DataString);
  finally
    o.Free;
  end;
end;

procedure TTestPDFCurveV.TestWrite_NoStroke;
var
  o: TMockPDFCurveV;
  X2,Y2: TPDFFloat;
  X3,Y3: TPDFFloat;
  lLineWidth: TPDFFLoat;
begin
  X2 := 100;
  Y2 := 9;
  X3 := 200;
  Y3 := 250;
  lLineWidth := 2;
  o := TMockPDFCurveV.Create(PDF, x2, y2, x3, y3, lLineWidth, False);
  try
    AssertEquals('Failed on 1', '', S.DataString);
    o.Write(S);
    AssertEquals('Failed on 2',
      ' 100    9  200  250 v'+CRLF,
      S.DataString);
  finally
    o.Free;
  end;
end;

{ TTestPDFCurveY }

procedure TTestPDFCurveY.TestWrite_Stroke;
var
  o: TMockPDFCurveY;
  X2,Y2: TPDFFloat;
  X3,Y3: TPDFFloat;
  lLineWidth: TPDFFLoat;
begin
  X2 := 100;
  Y2 := 9;
  X3 := 200;
  Y3 := 250;
  lLineWidth := 2;
  o := TMockPDFCurveY.Create(PDF, x2, y2, x3, y3, lLineWidth, True);
  try
    AssertEquals('Failed on 1', '', S.DataString);
    o.Write(S);
    AssertEquals('Failed on 2',
      '1 J'+CRLF+
      '   2 w'+CRLF+
      ' 100    9  200  250 y'+CRLF+
      'S'+CRLF,
      S.DataString);
  finally
    o.Free;
  end;
end;

procedure TTestPDFCurveY.TestWrite_NoStroke;
var
  o: TMockPDFCurveY;
  X2,Y2: TPDFFloat;
  X3,Y3: TPDFFloat;
  lLineWidth: TPDFFLoat;
begin
  X2 := 100;
  Y2 := 9;
  X3 := 200;
  Y3 := 250;
  lLineWidth := 2;
  o := TMockPDFCurveY.Create(PDF, x2, y2, x3, y3, lLineWidth, False);
  try
    AssertEquals('Failed on 1', '', S.DataString);
    o.Write(S);
    AssertEquals('Failed on 2',
      ' 100    9  200  250 y'+CRLF,
      S.DataString);
  finally
    o.Free;
  end;
end;

{ TTestPDFEllipse }

procedure TTestPDFEllipse.TestWrite_NoFill_NoStroke;
var
  o: TMockPDFEllipse;
  lPosX, lPosY, lWidth, lHeight, lLineWidth: TPDFFloat;
begin
  lPosX := 10;
  lPosY := 20;
  lWidth := 200;
  lHeight := 250;
  lLineWidth := 2;
  o := TMockPDFEllipse.Create(PDF, lPosX, lPosY, lWidth, lHeight, lLineWidth, False, False);
  try
    AssertEquals('Failed on 1', '', S.DataString);
    o.Write(S);
    AssertEquals('Failed on 2',
      // move to
      '  10  145 m'+CRLF+
      // curveC 1
      '  10 76.25   55   20  110   20 c'+CRLF+
      // curveC 2
      ' 165   20  210 76.25  210  145 c'+CRLF+
      // curveC 3
      ' 210 213.75  165  270  110  270 c'+CRLF+
      // curveC 4
      '  55  270   10 213.75   10  145 c'+CRLF,
      S.DataString);
  finally
    o.Free;
  end;
end;

procedure TTestPDFEllipse.TestWrite_Fill_NoStroke;
var
  o: TMockPDFEllipse;
  lPosX, lPosY, lWidth, lHeight, lLineWidth: TPDFFloat;
begin
  lPosX := 10;
  lPosY := 20;
  lWidth := 200;
  lHeight := 250;
  lLineWidth := 2;
  o := TMockPDFEllipse.Create(PDF, lPosX, lPosY, lWidth, lHeight, lLineWidth, True, False);
  try
    AssertEquals('Failed on 1', '', S.DataString);
    o.Write(S);
    AssertEquals('Failed on 2',
      // move to
      '  10  145 m'+CRLF+
      // curveC 1
      '  10 76.25   55   20  110   20 c'+CRLF+
      // curveC 2
      ' 165   20  210 76.25  210  145 c'+CRLF+
      // curveC 3
      ' 210 213.75  165  270  110  270 c'+CRLF+
      // curveC 4
      '  55  270   10 213.75   10  145 c'+CRLF+
      'f'+CRLF,
      S.DataString);
  finally
    o.Free;
  end;
end;

procedure TTestPDFEllipse.TestWrite_NoFill_Stroke;
var
  o: TMockPDFEllipse;
  lPosX, lPosY, lWidth, lHeight, lLineWidth: TPDFFloat;
begin
  lPosX := 10;
  lPosY := 20;
  lWidth := 200;
  lHeight := 250;
  lLineWidth := 2;
  o := TMockPDFEllipse.Create(PDF, lPosX, lPosY, lWidth, lHeight, lLineWidth, False, True);
  try
    AssertEquals('Failed on 1', '', S.DataString);
    o.Write(S);
    AssertEquals('Failed on 2',
      '1 J'+CRLF+
      '   2 w'+CRLF+
      // move to
      '  10  145 m'+CRLF+
      // curveC 1
      '  10 76.25   55   20  110   20 c'+CRLF+
      // curveC 2
      ' 165   20  210 76.25  210  145 c'+CRLF+
      // curveC 3
      ' 210 213.75  165  270  110  270 c'+CRLF+
      // curveC 4
      '  55  270   10 213.75   10  145 c'+CRLF+
      'S'+CRLF,
      S.DataString);
  finally
    o.Free;
  end;
end;

procedure TTestPDFEllipse.TestWrite_Fill_Stroke;
var
  o: TMockPDFEllipse;
  lPosX, lPosY, lWidth, lHeight, lLineWidth: TPDFFloat;
begin
  lPosX := 10;
  lPosY := 20;
  lWidth := 200;
  lHeight := 250;
  lLineWidth := 2;
  o := TMockPDFEllipse.Create(PDF, lPosX, lPosY, lWidth, lHeight, lLineWidth, True, True);
  try
    AssertEquals('Failed on 1', '', S.DataString);
    o.Write(S);
    AssertEquals('Failed on 2',
      '1 J'+CRLF+
      '   2 w'+CRLF+
      // move to
      '  10  145 m'+CRLF+
      // curveC 1
      '  10 76.25   55   20  110   20 c'+CRLF+
      // curveC 2
      ' 165   20  210 76.25  210  145 c'+CRLF+
      // curveC 3
      ' 210 213.75  165  270  110  270 c'+CRLF+
      // curveC 4
      '  55  270   10 213.75   10  145 c'+CRLF+
      'b'+CRLF,
      S.DataString);
  finally
    o.Free;
  end;
end;

{ TTestPDFSurface }

procedure TTestPDFSurface.TestWrite;
var
  o: TMockPDFSurface;
  ar: TPDFCoordArray;
  p1, p2, p3: TPDFCoord;
begin
  SetLength(ar, 3);
  p1.X := 10; p1.Y := 20;
  p2.X := 30; p2.Y := 40;
  p3.X := 50; p3.Y := 60;
  ar[0] := p1;
  ar[1] := p2;
  ar[2] := p3;
  o := TMockPDFSurface.Create(PDF, ar, True, True);
  try
    AssertEquals('Failed on 1', '', S.DataString);
    o.Write(S);
    AssertEquals('Failed on 2',
      // move to - p0
      '  10   20 m'+CRLF+
      // line segment - p1
      '  30   40 l'+CRLF+
      // line segment - p2
      '  50   60 l'+CRLF+
      'h'+CRLF+   // close
      'f'+CRLF,   // fill
      S.DataString);
  finally
    SetLength(ar, 0);
    o.Free;
  end;
end;

procedure TTestPDFSurface.TestWrite_noFill;
var
  o: TMockPDFSurface;
  ar: TPDFCoordArray;
  p1, p2, p3: TPDFCoord;
begin
  SetLength(ar, 3);
  p1.X := 10; p1.Y := 20;
  p2.X := 30; p2.Y := 40;
  p3.X := 50; p3.Y := 60;
  ar[0] := p1;
  ar[1] := p2;
  ar[2] := p3;
  o := TMockPDFSurface.Create(PDF, ar, True, False);
  try
    AssertEquals('Failed on 1', '', S.DataString);
    o.Write(S);
    AssertEquals('Failed on 2',
      // move to - p0
      '  10   20 m'+CRLF+
      // line segment - p1
      '  30   40 l'+CRLF+
      // line segment - p2
      '  50   60 l'+CRLF+
      'h'+CRLF,   // close
      S.DataString);
  finally
    SetLength(ar, 0);
    o.Free;
  end;
end;

procedure TTestPDFSurface.TestWrite_noClose;
var
  o: TMockPDFSurface;
  ar: TPDFCoordArray;
  p1, p2, p3: TPDFCoord;
begin
  SetLength(ar, 3);
  p1.X := 10; p1.Y := 20;
  p2.X := 30; p2.Y := 40;
  p3.X := 50; p3.Y := 60;
  ar[0] := p1;
  ar[1] := p2;
  ar[2] := p3;
  o := TMockPDFSurface.Create(PDF, ar, False, True);
  try
    AssertEquals('Failed on 1', '', S.DataString);
    o.Write(S);
    AssertEquals('Failed on 2',
      // move to - p0
      '  10   20 m'+CRLF+
      // line segment - p1
      '  30   40 l'+CRLF+
      // line segment - p2
      '  50   60 l'+CRLF+
      'f'+CRLF,   // fill
      S.DataString);
  finally
    SetLength(ar, 0);
    o.Free;
  end;
end;

{ TTestPDFImage }

procedure TTestPDFImage.TestWrite;
var
  o: TMockPDFImage;
  x, y: TPDFFLoat;
begin
  x := 100;
  y := 200;
  o := TMockPDFImage.Create(PDF, x, y, 150, 75, 1);
  try
    AssertEquals('Failed on 1', '', S.DataString);
    o.Write(S);
    AssertEquals('Failed on 2',
      // save graphics state
      'q'+CRLF+
      '150 0 0 75  100  200 cm'+CRLF+
      '/I1 Do'+CRLF+
      // restore graphics state
      'Q'+CRLF,
      S.DataString);
  finally
    o.Free;
  end;
end;

{ TTestPDFLineStyle }

procedure TTestPDFLineStyle.TestWrite_ppsSolid;
var
  o: TMockPDFLineStyle;
begin
  o := TMockPDFLineStyle.Create(PDF, ppsSolid, 1);
  try
    AssertEquals('Failed on 1', '', S.DataString);
    o.Write(S);
    AssertEquals('Failed on 2',
      '[] 1 d'+CRLF,
      S.DataString);
  finally
    o.Free;
  end;
end;

procedure TTestPDFLineStyle.TestWrite_ppsDash;
var
  o: TMockPDFLineStyle;
begin
  o := TMockPDFLineStyle.Create(PDF, ppsDash, 2);
  try
    AssertEquals('Failed on 1', '', S.DataString);
    o.Write(S);
    AssertEquals('Failed on 2',
      '[5 3] 2 d'+CRLF,
      S.DataString);
  finally
    o.Free;
  end;
end;

procedure TTestPDFLineStyle.TestWrite_ppsDot;
var
  o: TMockPDFLineStyle;
begin
  o := TMockPDFLineStyle.Create(PDF, ppsDot, 3);
  try
    AssertEquals('Failed on 1', '', S.DataString);
    o.Write(S);
    AssertEquals('Failed on 2',
      '[1 3] 3 d'+CRLF,
      S.DataString);
  finally
    o.Free;
  end;
end;

procedure TTestPDFLineStyle.TestWrite_ppsDashDot;
var
  o: TMockPDFLineStyle;
begin
  o := TMockPDFLineStyle.Create(PDF, ppsDashDot, 4);
  try
    AssertEquals('Failed on 1', '', S.DataString);
    o.Write(S);
    AssertEquals('Failed on 2',
      '[5 3 1 3] 4 d'+CRLF,
      S.DataString);
  finally
    o.Free;
  end;
end;

procedure TTestPDFLineStyle.TestWrite_ppsDashDotDot;
var
  o: TMockPDFLineStyle;
begin
  o := TMockPDFLineStyle.Create(PDF, ppsDashDotDot, 1);
  try
    AssertEquals('Failed on 1', '', S.DataString);
    o.Write(S);
    AssertEquals('Failed on 2',
      '[5 3 1 3 1 3] 1 d'+CRLF,
      S.DataString);
  finally
    o.Free;
  end;
end;

{ TTestPDFColor }

procedure TTestPDFColor.TestWrite_Stroke;
var
  o: TMockPDFColor;
begin
  o := TMockPDFColor.Create(PDF, True, $AABBCC);
  try
    AssertEquals('Failed on 1', '', S.DataString);
    o.Write(S);
    AssertEquals('Failed on 2',
      '0.66 0.73 0.80 RG'+CRLF,
      S.DataString);
  finally
    o.Free;
  end;
end;

procedure TTestPDFColor.TestWrite_noStroke;
var
  o: TMockPDFColor;
begin
  o := TMockPDFColor.Create(PDF, False, $AABBCC);
  try
    AssertEquals('Failed on 1', '', S.DataString);
    o.Write(S);
    AssertEquals('Failed on 2',
      '0.66 0.73 0.80 rg'+CRLF,
      S.DataString);
  finally
    o.Free;
  end;
end;

{ TTestPDFDictionaryItem }

procedure TTestPDFDictionaryItem.TestWrite;
var
  o: TMockPDFDictionaryItem;
  v: TPDFString;
begin
  v := TPDFString.Create(PDF, 'TestValue');
  o := TMockPDFDictionaryItem.Create(PDF, 'tv', v);
  try
    AssertEquals('Failed on 1', '', S.DataString);
    o.Write(S);
    AssertEquals('Failed on 2',
      '/tv (TestValue)'+CRLF,
      S.DataString);
  finally
    o.Free;
  end;
end;

{ TTestPDFDictionary }

procedure TTestPDFDictionary.TestWrite;
var
  o: TMockPDFDictionary;
  v: TPDFString;
begin
  v := TPDFString.Create(PDF, 'TestValue');
  o := TMockPDFDictionary.Create(PDF);
  o.AddName('key1','value1');
  o.AddElement('key2', v);
  o.AddInteger('key3', 1234);
  o.AddString('key4', 'string4');
  o.AddReference('key5', 987);
  try
    AssertEquals('Failed on 1', '', S.DataString);
    o.Write(S);
    AssertEquals('Failed on 2',
      '<<'+CRLF+
      '/key1 /value1'+CRLF+
      '/key2 (TestValue)'+CRLF+
      '/key3 1234'+CRLF+
      '/key4 (string4)'+CRLF+
      '/key5 987 0 R'+CRLF+
      '>>',
      S.DataString);
  finally
    o.Free;
  end;
end;

{ TTestPDFXRef }

procedure TTestPDFXRef.TestWrite;
var
  o: TMockPDFXRef;
begin
  o := TMockPDFXRef.Create(PDF);
  try
    AssertEquals('Failed on 1', '', S.DataString);
    o.Write(S);
    AssertEquals('Failed on 2',
      '0000000000 00000 n'+CRLF,
      S.DataString);

    o.Offset := 234;
    o.Write(S);
    AssertEquals('Failed on 3',
      '0000000000 00000 n'+CRLF+
      '0000000234 00000 n'+CRLF,
      S.DataString);
  finally
    o.Free;
  end;
end;

{ TTestPDFPage }

procedure TTestPDFPage.TestPageDocument;
var
  p: TPDFPage;
begin
  p := PDF.Pages.AddPage;
  AssertTrue('Failed on 1', p.Document = PDF);
  AssertTrue('Failed on 2', p.UnitOfMeasure = uomMillimeters);
end;

procedure TTestPDFPage.TestPageDefaultUnitOfMeasure;
var
  p: TPDFPage;
begin
  p := PDF.Pages.AddPage;
  AssertTrue('Failed on 1', p.UnitOfMeasure = uomMillimeters);
end;

procedure TTestPDFPage.TestMatrix;
var
  p: TPDFPage;
  pt1, pt2: TPDFCoord;
begin
  p := PDF.Pages.AddPage;
  AssertTrue('Failed on 1', p.UnitOfMeasure = uomMillimeters);
  AssertEquals('Failed on 2', mmToPDF(p.Matrix._21), p.Paper.H);

  pt1.X := 10;
  pt1.Y := 20;
  pt2 := p.Matrix.Transform(pt1);
  AssertEquals('Failed on 3', 10, pt2.X);
  AssertEquals('Failed on 4', 297-20, pt2.Y, 0.1);

  pt1 := p.Matrix.ReverseTransform(pt2);
  AssertEquals('Failed on 5', 10, pt1.X);
  AssertEquals('Failed on 6', 20, pt1.Y, 0.1);
end;

procedure TTestPDFPage.TestUnitOfMeasure_MM;
var
  p: TPDFPage;
  pt: TPDFCoord;
begin
  p := PDF.Pages.AddPage;
  p.UnitOfMeasure := uomMillimeters;

  pt.X := 20;
  pt.Y := 35;
  TMockPDFPage(p).doUnitConversion(pt);
  AssertEquals('Failed on 1', 56.69, pt.X, 0.01);
  AssertEquals('Failed on 2', 99.21, pt.Y, 0.01);

  pt.X := 40;
  pt.Y := 20;
  TMockPDFPage(p).doUnitConversion(pt);
  AssertEquals('Failed on 3', 113.38, pt.X, 0.01);
  AssertEquals('Failed on 4', 56.69, pt.Y, 0.01);
end;

procedure TTestPDFPage.TestUnitOfMeasure_Inches;
var
  p: TPDFPage;
  pt: TPDFCoord;
begin
  p := PDF.Pages.AddPage;
  p.UnitOfMeasure := uomInches;

  pt.X := 1;
  pt.Y := 1.5;
  TMockPDFPage(p).doUnitConversion(pt);
  AssertEquals('Failed on 1', 72.0, pt.X, 0.01);
  AssertEquals('Failed on 2', 108.0, pt.Y, 0.01);

  pt.X := 2;
  pt.Y := 1;
  TMockPDFPage(p).doUnitConversion(pt);
  AssertEquals('Failed on 3', 144.0, pt.X, 0.01);
  AssertEquals('Failed on 4', 72.0, pt.Y, 0.01);
end;

procedure TTestPDFPage.TestUnitOfMeasure_CM;
var
  p: TPDFPage;
  pt: TPDFCoord;
begin
  p := PDF.Pages.AddPage;
  p.UnitOfMeasure := uomMillimeters;

  pt.X := 2.0;
  pt.Y := 3.5;
  TMockPDFPage(p).doUnitConversion(pt);
  AssertEquals('Failed on 1', 5.669, pt.X, 0.01);
  AssertEquals('Failed on 2', 9.921, pt.Y, 0.01);

  pt.X := 4.0;
  pt.Y := 2.0;
  TMockPDFPage(p).doUnitConversion(pt);
  AssertEquals('Failed on 3', 11.338, pt.X, 0.01);
  AssertEquals('Failed on 4', 5.669, pt.Y, 0.01);
end;


{ TTestCompressionDecompression }

function TTestCompressionDecompression.GetTestString: string;
var
  i: integer;
  lsLine: string;
begin
  result := '';
  lsLine := '';
  for i := 1 to 1000 do
    lsLine := lsLine + Chr(ord('A')+Random(ord('z')-ord('A')));
  for i := 1 to 200 do
    result := result + lsLine + LineEnding;
  Result := 'Hello World';
end;

procedure TTestCompressionDecompression.TestStreamCompressionDecompression;
var
  lSBefore: TStringStream;
  lSAfter: TStringStream;
  lCompressed: TMemoryStream;
  lBefore: string;
  lAfter: string;
begin
  lBefore := GetTestString;
  lSBefore := TStringStream.Create(lBefore);
  lCompressed := TMemoryStream.Create;
  CompressStream(lSBefore, lCompressed);
  try
    lSAfter := TStringStream.Create('');
    DecompressStream(lCompressed, lSAfter);
    lAfter := lSAfter.DataString;
    AssertTrue('Compression failed. Strings are not the same. ' +IntToStr(Length(lBefore)) + ' vs ' + IntToStr(Length(lAfter)), lBefore = lAfter);
  finally
    lSBefore.Free;
    lCompressed.Free;
    lSAfter.Free;
  end;
end;

procedure TTestCompressionDecompression.TestStringCompressionDecompression;
var
  lBefore: string;
  lCompressed: string;
  lAfter: string;
  s: TStringStream;
  e: TStringStream;
begin
  lBefore := GetTestString;
  lCompressed := '';
  CompressString(lBefore, lCompressed);
  s := TStringStream.Create(lCompressed);
  try
    e := TStringStream.Create('');
    s.Position := 0;
    DecompressStream(s, e);
    lAfter := e.DataString;
  finally
    e.Free;
    s.Free;
  end;

  AssertTrue('Compression failed. Strings are not the same. ' +IntToStr(Length(lBefore)) + ' vs ' + IntToStr(Length(lAfter)), lBefore = lAfter);
end;

{ TTestTPDFImageItem }

procedure TTestTPDFImageItem.TestCreateStreamedData;
var
  itm: TPDFImageItem;
  img: TFPMemoryImage;
  b: TBytes;
begin
  itm := TPDFImageItem.Create(nil);
  try
    itm.OwnsImage := True;
    img := TFPMemoryImage.Create(5, 5);
    itm.Image := img;
    b := itm.StreamedData;
    AssertEquals('Failed on 1', 75 {5*5*3}, Length(b));
  finally
    itm.Free;
  end;

  itm := TPDFImageItem.Create(nil);
  try
    itm.OwnsImage := True;
    img := TFPMemoryImage.Create(10, 20);
    itm.Image := img;
    { this try..except as to prove that we had a bug before we fixed it. }
    try
      b := itm.StreamedData;
    except
      Fail('Failed on 2 - itm.StreamedData raised an exception');
    end;
    AssertEquals('Failed on 3', 600 {10*20*3}, Length(b));
  finally
    itm.Free;
  end;
end;


initialization
  RegisterTest({$ifdef fptest}'fpPDF',{$endif}TTestPDFObject{$ifdef fptest}.Suite{$endif});
  RegisterTest({$ifdef fptest}'fpPDF',{$endif}TTestTPDFDocumentObject{$ifdef fptest}.Suite{$endif});
  RegisterTest({$ifdef fptest}'fpPDF',{$endif}TTestPDFBoolean{$ifdef fptest}.Suite{$endif});
  RegisterTest({$ifdef fptest}'fpPDF',{$endif}TTestPDFMoveTo{$ifdef fptest}.Suite{$endif});
  RegisterTest({$ifdef fptest}'fpPDF',{$endif}TTestPDFInteger{$ifdef fptest}.Suite{$endif});
  RegisterTest({$ifdef fptest}'fpPDF',{$endif}TTestPDFReference{$ifdef fptest}.Suite{$endif});
  RegisterTest({$ifdef fptest}'fpPDF',{$endif}TTestPDFName{$ifdef fptest}.Suite{$endif});
  RegisterTest({$ifdef fptest}'fpPDF',{$endif}TTestPDFAbstractString{$ifdef fptest}.Suite{$endif});
  RegisterTest({$ifdef fptest}'fpPDF',{$endif}TTestPDFString{$ifdef fptest}.Suite{$endif});
  RegisterTest({$ifdef fptest}'fpPDF',{$endif}TTestPDFArray{$ifdef fptest}.Suite{$endif});
  RegisterTest({$ifdef fptest}'fpPDF',{$endif}TTestPDFStream{$ifdef fptest}.Suite{$endif});
  RegisterTest({$ifdef fptest}'fpPDF',{$endif}TTestPDFEmbeddedFont{$ifdef fptest}.Suite{$endif});
  RegisterTest({$ifdef fptest}'fpPDF',{$endif}TTestPDFText{$ifdef fptest}.Suite{$endif});
  RegisterTest({$ifdef fptest}'fpPDF',{$endif}TTestPDFLineSegment{$ifdef fptest}.Suite{$endif});
  RegisterTest({$ifdef fptest}'fpPDF',{$endif}TTestTPDFRectangle{$ifdef fptest}.Suite{$endif});
  RegisterTest({$ifdef fptest}'fpPDF',{$endif}TTestPDFCurveC{$ifdef fptest}.Suite{$endif});
  RegisterTest({$ifdef fptest}'fpPDF',{$endif}TTestPDFCurveV{$ifdef fptest}.Suite{$endif});
  RegisterTest({$ifdef fptest}'fpPDF',{$endif}TTestPDFCurveY{$ifdef fptest}.Suite{$endif});
  RegisterTest({$ifdef fptest}'fpPDF',{$endif}TTestPDFEllipse{$ifdef fptest}.Suite{$endif});
  RegisterTest({$ifdef fptest}'fpPDF',{$endif}TTestPDFSurface{$ifdef fptest}.Suite{$endif});
  RegisterTest({$ifdef fptest}'fpPDF',{$endif}TTestPDFImage{$ifdef fptest}.Suite{$endif});
  RegisterTest({$ifdef fptest}'fpPDF',{$endif}TTestPDFLineStyle{$ifdef fptest}.Suite{$endif});
  RegisterTest({$ifdef fptest}'fpPDF',{$endif}TTestPDFColor{$ifdef fptest}.Suite{$endif});
  RegisterTest({$ifdef fptest}'fpPDF',{$endif}TTestPDFDictionaryItem{$ifdef fptest}.Suite{$endif});
  RegisterTest({$ifdef fptest}'fpPDF',{$endif}TTestPDFDictionary{$ifdef fptest}.Suite{$endif});
  RegisterTest({$ifdef fptest}'fpPDF',{$endif}TTestPDFXRef{$ifdef fptest}.Suite{$endif});
  RegisterTest({$ifdef fptest}'fpPDF',{$endif}TTestPDFPage{$ifdef fptest}.Suite{$endif});
  RegisterTest({$ifdef fptest}'fpPDF',{$endif}TTestCompressionDecompression{$ifdef fptest}.Suite{$endif});
  RegisterTest({$ifdef fptest}'fpPDF',{$endif}TTestTPDFImageItem{$ifdef fptest}.Suite{$endif});

end.

