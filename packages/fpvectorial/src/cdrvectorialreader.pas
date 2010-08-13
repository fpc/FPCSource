{
cdrvectorialreader.pas

Reads a Corel Draw vectorial file

CDR file format specification obtained from:

ADOBE SYSTEMS INCORPORATED. PDF Reference: Adobe®
Portable Document Format. San Jose, 2006. (Sixth edition).

AUTHORS: Felipe Monteiro de Carvalho

License: The same modified LGPL as the Free Pascal RTL
         See the file COPYING.modifiedLGPL for more details
}
unit cdrvectorialreader;

{$ifdef fpc}
  {$mode delphi}
{$endif}

interface

uses
  Classes, SysUtils,
  pdfvrlexico, pdfvrsintatico, pdfvrsemantico, avisozlib,
  fpvectorial;

type

  { TvCDRVectorialReader }

  TvCDRVectorialReader = class(TvCustomVectorialReader)
  public
    { General reading methods }
    procedure ReadFromStream(AStream: TStream; AData: TvVectorialDocument); override;
  end;

implementation

{ TvPDFVectorialReader }

procedure TvCDRVectorialReader.ReadFromStream(AStream: TStream;
  AData: TvVectorialDocument);
begin
end;

initialization

  RegisterVectorialReader(TvCDRVectorialReader, vfCorelDrawCDR);

end.

