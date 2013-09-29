{$mode objfpc}
{$H+}
program testhttp;

uses
  SysUtils, fphttpapp, fpwebfile;

Procedure Usage;

begin
  Writeln('Usage : testhttp DocumentRoot [Port]');
  Writeln('Where');
  Writeln(' Documentroot   location to serve files from. It is mapped to location /files');
  Writeln(' Port           port to listen on (default 8080)');
  Halt(1);
end;

begin
  if (ParamCount<1) or (ParamCount>2) then
    usage;
  if (ParamCount=2) and (StrToIntDef(ParamStr(2),-1)=-1) then
    usage;
  RegisterFileLocation('files',ParamStr(1));
{$ifdef unix}
  MimeTypesFile:='/etc/mime.types';
{$endif}
  Application.Initialize;
  Application.Port:=StrTointDef(ParamStr(2),8080);
  Application.Title:='HTTP Demo application';
  Application.Run;
end.
