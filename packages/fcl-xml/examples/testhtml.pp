program testhtml;
{
  simple demo to demonstrate rewriting a HTML file
}
uses sysutils, dom_html,sax_html, XMLWrite;

Var
  H : THTMLDocument;

begin
  if ParamCount<>2 then
    begin
    Writeln('Usage: ',ExtractFileName(Paramstr(0)),' inputfile outputfile');
    Halt(1);
    end;
  ReadHTMLFile(H,ParamStr(1));
  WriteXMLFile(H,Paramstr(2));
end.

