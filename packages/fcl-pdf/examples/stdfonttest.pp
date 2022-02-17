{
  Demo program to demonstrate standard font strange character support, with strikethrough and underline.
}
program stdfonttest;

{$mode objfpc}{$H+}
{$codepage UTF8}

uses
  {$ifdef unix}cwstring,{$endif}SysUtils, fpTTF, fpPDF;


var
  PDF: TPDFDocument;
  StdFtHelvetica: Integer;
  P: TPDFPage;

begin
   PDF := TPDFDocument.Create(nil);
   PDF.Infos.Producer := 'Test';
   PDF.Infos.CreationDate := Now;
   PDF.Options := [poPageOriginAtTop, {poNoEmbeddedFonts,} poSubsetFont, poCompressFonts, poCompressImages];
   PDF.DefaultOrientation := ppoPortrait;
   PDF.DefaultPaperType := ptA4;
   PDF.DefaultUnitOfMeasure := uomMillimeters;
   PDF.StartDocument;
   PDF.Sections.AddSection;
   PDF.Sections[0].AddPage(PDF.Pages.AddPage);
   StdFtHelvetica := PDF.AddFont('Helvetica');
   P:=PDF.Pages[0];
   P.SetFont(StdFtHelvetica, 14);
   P.WriteText(10,10,'FPC Demo: PDF öäü ÖÄÜ Test',0,true,true);
   PDF.SaveToFile('test-stdfont.pdf');
   PDF.Free;
end.
