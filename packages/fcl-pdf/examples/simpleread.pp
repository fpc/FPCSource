{$mode objfpc}
{$h+}
uses fppdfobjects, fppdfparser, sysutils, classes;

procedure ReadPDF(const aStream: TStream; aDoc: TPDFDocument);

var
  aParser : TPDFParser;

begin
  aParser:=TPDFParser.Create(aStream);
  try
    aParser.ResolveToUnicodeCMaps:=True;
    aParser.ParseDocument(aDoc);
  finally
    aParser.Free;
  end;
end;

var
  F : TFileStream;
  Doc : TPDFDocument;
  
begin
  if ParamCount<1 then
    begin
    Writeln('Usage : simpleread filename');
    Halt(1);
    end;
  F:=TFileStream.Create(paramstr(1),fmCreate or fmShareDenyNone);
  try
    Doc:=TPDFDocument.Create;
    ReadPDF(F,Doc);
  finally
    doc.free;
    f.free;
  end;  
end.