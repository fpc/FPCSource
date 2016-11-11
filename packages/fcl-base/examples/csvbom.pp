program csvbom;

{$APPTYPE Console}
{$mode objfpc}{$H+}

uses
  sysutils, classes, dateutils, csvreadwrite;

type
  TDataRec = record
    FDate: TDate;
    FNumber: Integer;
    FText: String;
  end;

const
  FILENAME = 'databom.txt';

var
  parser: TCSVParser;
  stream: TFileStream;
  data: array of TDataRec;
  s: String;
  i: Integer;
begin
  parser := TCSVParser.Create;
  try
    parser.Delimiter := ',';
    parser.DetectBOM := true;     // uncomment for running with patched version
    stream := TFileStream.Create(FILENAME, fmOpenRead);
    parser.SetSource(stream);
    SetLength(data, 0);
    while parser.ParseNextCell do begin
      if parser.CurrentRow > High(data) then
        SetLength(data, parser.CurrentRow + 1);
      s := parser.CurrentCellText;
      case parser.CurrentCol of
        0: data[High(data)].FDate := ScanDateTime('yyyy-mm-dd', s);
        1: data[High(data)].FNumber := StrToInt(s);
        2: data[High(data)].FText := s;
      end;
    end;

    for i:=0 to High(data) do
      WriteLn(DateToStr(data[i].FDate), '; ', data[i].FNumber, '; ', data[i].FText);
    Writeln('Press enter to quit program');
    Readln;
  finally
    stream.Free;
    parser.Free;
  end;
end.

