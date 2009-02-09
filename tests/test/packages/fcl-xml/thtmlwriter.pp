program TestHtmlWriter;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, htmlwriter, htmlelements;

var hwriter: THtmlWriter;
    hdoc : THTMLDocument;
begin
  hdoc := THTMLDocument.Create;
  hwriter := THTMLwriter.create(hdoc);
  hwriter.Starthtml;
  hwriter.Starthead;
  hwriter.title('Test website');
  hwriter.Endhead;
  hwriter.Startbody;
  hwriter.paragraph('test line 1');
  hwriter.paragraph('test line 2');
  hwriter.Endbody;
  hwriter.Endhtml;
  hwriter.Free;
  writeln(hdoc.Asstring);
  hdoc.Free;
end.

