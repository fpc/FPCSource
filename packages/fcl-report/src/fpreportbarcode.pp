{
    This file is part of the Free Component Library.
    Copyright (c) 2017 Michael Van Canneyt, member of the Free Pascal development team

    Barcode report element.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit fpreportbarcode;

{$mode objfpc}
{$H+}

interface

uses
  Classes, fpimage, fpexprpars, fpimgbarcode, fpbarcode, fpreport, fpreportstreamer;

Type

  { TFPReportBarcode }

  TFPReportBarcode = Class(TFPReportElement)
  private
    FEncoding: TBarcodeEncoding;
    FExpression: String;
    FPadLength: Integer;
    FUnitWidth: Integer;
    FValue: String;
    FExprValue : String;
    FWeight: Double;
  Protected
    procedure BeforePrint;  override;
    procedure RecalcLayout; override;
    Procedure DoWriteLocalProperties(AWriter: TFPReportStreamer; AOriginal: TFPReportElement=nil); override;
  Public
    procedure   Assign(Source: TPersistent); override;
    Constructor Create(AOwner: TComponent); override;
    Class Function ElementType : String; override;
    // Will calculate the value to display. Either Value or evaluated expression.
    Function BarcodeValue : String;
    Procedure ReadElement(AReader: TFPReportStreamer); override;
  Published
    Property Encoding : TBarcodeEncoding Read FEncoding Write FEncoding;
    Property UnitWidth : Integer Read FUnitWidth Write FUnitWidth;
    Property Weight : Double Read FWeight Write FWeight;
    Property PadLength : Integer Read FPadLength Write FPadLength;
    // Expression takes precedence
    Property Value : String Read FValue Write FValue;
    Property Expression : String Read FExpression Write FExpression;
  end;

Procedure RegisterReportBarcode;
Procedure UnRegisterReportBarcode;
  
implementation

uses typinfo, strutils;


{ TFPReportBarcode }

procedure TFPReportBarcode.RecalcLayout;
begin
  // Do nothing for the moment.
  // We may consider adding a Boolean property FitWidth and calculating width based on value/expression when it is set to true
end;

procedure TFPReportBarcode.DoWriteLocalProperties(AWriter: TFPReportStreamer; AOriginal: TFPReportElement);


begin
  inherited DoWriteLocalProperties(AWriter, AOriginal);
  AWriter.WriteString('Encoding',GetEnumName(TypeInfo(TBarcodeEncoding),Ord(FEncoding)));
  AWriter.WriteInteger('UnitWidth',UnitWidth);
  AWriter.WriteInteger('PadLength',PadLength);
  AWriter.WriteFloat('Weight',Weight);
  AWriter.WriteString('Value',Value);
  AWriter.WriteString('Expression',Expression);
end;

procedure TFPReportBarcode.Assign(Source: TPersistent);

Var
  BC : TFPReportBarcode;

begin
  if (Source is TFPReportBarcode) then
    begin
    BC:=TFPReportBarcode(Source);
    FValue:=BC.Value;
    FPadlength:=BC.PadLength;
    FExpression:=BC.Expression;
    FWeight:=BC.Weight;
    FUnitWidth:=BC.UnitWidth;
    FEncoding:=BC.Encoding;
    end;
  inherited Assign(Source);
end;

constructor TFPReportBarcode.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FEncoding:=be128A;
  FUnitWidth:=1;
  FWeight:=2.0;
end;

class function TFPReportBarcode.ElementType: String;
begin
  Result:='BarCode';
end;

procedure TFPReportBarcode.BeforePrint;

begin
  Inherited;
  if (FExpression<>'') then
  FExprValue:=EvaluateExpressionAsText(FExpression)
end;

function TFPReportBarcode.BarcodeValue: String;

begin
  if (FExpression<>'') then
    Result:=FExprValue // Calculated in beforeprint
  else
    Result:=FValue;
  Result:=AddChar('0',Result,PadLength);
end;

procedure TFPReportBarcode.ReadElement(AReader: TFPReportStreamer);

Var
  S : String;
  I : Integer;

begin
  inherited ReadElement(AReader);
  S:=AReader.ReadString('Encoding','beEan8');
  I:=GetEnumValue(TypeInfo(TBarcodeEncoding),S);
  if I<>-1 then
    FEncoding:=TBarcodeEncoding(I);
  UnitWidth:=AReader.ReadInteger('UnitWidth',UnitWidth);
  PadLength:=AReader.ReadInteger('UnitWidth',PadLength);
  Weight:=AReader.ReadFloat('Weight',Weight);
  Value:=AReader.ReadString('Value',Value);
  Expression:=AReader.ReadString('Expression',Expression);
end;

procedure RenderBarcode(aElement: TFPReportElement; aImage: TFPCustomImage);

Var
  D : TFPDrawBarcode;
  B : TFPReportBarcode;

begin
  B:=TFPReportBarcode(aElement);
  D:=TFPDrawBarcode.Create;
  try
    D.Image:=aImage;
    D.Weight:=B.Weight;
    D.UnitWidth:=B.UnitWidth;
    D.Rect:=Rect(0,0,aImage.Width-1,aImage.Height-1);
    D.Text:=B.BarcodeValue;
    // Writeln('Weight: ',D.Weight,' unitwidth:',D.UnitWidth,' ',aImage.Width-1,'x',aImage.Height-1,' Text: ',D.Text);
    D.Encoding:=B.Encoding;
    D.Clipping:=True;
    D.Draw;
  finally
    D.Free;
  end;
end;


Procedure RegisterReportBarcode;

Const
  icon : Array[0..687] of byte = (
     137, 80, 78, 71, 13, 10, 26, 10,  0,  0,  0, 13, 73, 72, 68, 82,  0,
       0,  0, 16,  0,  0,  0, 16,  8,  6,  0,  0,  0, 31,243,255, 97,  0,
       0,  2,119, 73, 68, 65, 84,120,156,141,147,189, 74, 92, 81, 20,133,
     191,243,115,255,207,140,137,131, 78, 52, 97, 16, 11, 13, 72, 82, 72,
       8, 24, 27, 11,155,188,131,149,181,146,202,135,  8, 41,132,188,129,
      32,216,  8,118,226,  3,164, 21,196, 46, 54,153, 66, 13, 99, 38,113,
     254,238,157,153,123,199,123, 82, 56,115, 73, 37, 89,176,224,176, 89,
     103,237,195,222,103,  9,198, 56, 56, 56, 88, 95, 92, 92,124, 45,165,
      20, 60,  1,107,173,173,215,235,223,183,182,182,190, 21,197,163,163,
     163,207, 73,146,228, 73,146,216,255,100,126,120,120,248,  5, 64,108,
     111,111,191, 92, 93, 93,173,135, 97,168,141, 49, 52, 26, 13,142,143,
     143, 57, 61, 61,101,125,125,157, 82,169,196,206,206, 14,215,215,215,
     156,156,156,176,187,187,203,205,205, 13,211,211,211, 15,151,151,151,
      11,186, 82,169, 84,227, 56,214,214, 90,164,148,116,187, 93,126,253,
     106,  2,112,119,119,199,112, 56, 36, 73, 18,226, 56,166,217,108,210,
     235,245,136,227, 24,223,247, 85,181, 90,125, 33,133, 16, 76, 40,165,
      68,107,141,235,186, 40,165,240, 60, 15,207,243, 80, 74,161,181,198,
     113,156, 66,  7, 32,165, 20, 18, 40,138, 82,170,177,129, 83, 24,185,
     174,139,214,186, 48,208, 90, 35,165, 66, 74,137,181, 22, 41, 30,157,
     198, 93, 84,209, 77,  8, 81, 92, 82,106, 82,159,156,229,248, 21, 22,
     141, 16,204,164, 41,230,247,111, 76,154,162, 90, 45,222,102, 25,226,
     231, 79,222,141, 70,148, 71, 35,102, 91, 45,242, 78,135, 55, 89, 74,
     165,217,100,116,127,143,137, 34,218,  8,180, 16,130, 56,138, 40, 55,
     155, 68, 74, 97,  7,  3, 94,229, 57,162,223,103,193, 90, 76,158, 83,
      74, 83,134, 89,198,124,158, 19,245,251,136, 52, 37, 14, 67, 72,146,
      71,  3,171, 53,237, 90,141, 40,203,200,178,140,196,243,160, 82,161,
     231,121,  8,207, 99, 84, 46,147,117,187,244, 93,151,254,212, 51,254,
     184, 46,211, 74, 33,192, 74, 38, 51,240,125,226,106,149, 81,185, 76,
      26,134, 48, 53,197, 32,  8, 24,134, 33,185, 49, 60, 24, 67, 28, 69,
     244,102,103,208,227, 45, 33,  4, 18, 40,134,164, 28,135,158, 49, 12,
     130,  0,164,100, 16,  4,164, 97,200,131,235, 50,116, 93, 26,229, 50,
      74,235, 66,111,173, 69,118, 58,157,252,113,133,143,204,181,166, 21,
      69,  0,164, 65, 64, 26,  4,228,142, 67,223,247, 17,142,195,191,218,
     118,187,253,160, 47, 46, 46,126,172,172,172, 52,150,151,151,171, 65,
      16, 96,140,225,253,135, 15,  0,172,110,108, 96, 74, 37,158,207,207,
      99,125,159,181,181, 53,230,230,230,240, 60,143,219,219,219,198,249,
     249,249, 15,  1,176,185,185,249,113,111,111,239,235,210,210,210,252,
      83, 73,156,224,234,234,234,118,127,127,255,211,217,217,217,105, 17,
     221, 90,173, 22,229,121,254,124,242, 77,159,136, 51, 74,169,251,122,
     189, 30,  3,252,  5,131, 89, 18, 55,201,190,160,207,  0,  0,  0,  0,
      73, 69, 78, 68,174, 66, 96,130);


begin
  TFPReportBarcode.RegisterElement.SetIconFromBytes(Icon);
  // Fallback renderer
  gElementFactory.RegisterImageRenderer(TFPReportBarcode,@RenderBarcode);
end;

Procedure UnRegisterReportBarcode;

begin
  TFPReportBarcode.UnRegisterElement;
end;

initialization
  RegisterReportBarcode;
end.  
