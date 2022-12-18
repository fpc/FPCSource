{$mode objfpc}
{$h+}
uses fppdfobjects, fppdfparser;

var
  Doc : TPDFDocument;
  
begin
  if ParamCount<1 then
    begin
    Writeln('Usage : simpleread filename');
    Halt(1);
    end;
  Doc:=TPDFDocument.Create;
  try
    Doc.LoadFromFile(paramstr(1));
  finally
    doc.free;
  end;  
end.