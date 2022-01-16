{$mode objfpc}{$h+}
{$inline on}
program fpjsmin;

uses jsminifier;


begin
  if ParamCount<>2 then
    begin
    Writeln('Usage: fpjsmin infile outfile');
    halt(1);
    end;
  With TJSONMinifier.Create(Nil) do
    try
       FileHeader.Add(paramstr(1));
       Execute(ParamStr(1),ParamStr(2));
    finally
      Free
    end;
end.
