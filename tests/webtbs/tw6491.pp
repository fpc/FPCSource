program TestExtractDrive;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ELSE}
  {$APPTYPE CONSOLE}
{$ENDIF}

uses
  SysUtils;


function tiRemoveDrive(pStrPath : string) : string;
var
  sDrive : string;
begin
  sDrive := extractFileDrive(pStrPath);
  if sDrive <> '' then begin
    result := copy(pStrPath, length(sDrive)+1, length(pStrPath) - length(sDrive));
  end else begin
    result := pStrPath;
  end;
end;


procedure CheckEquals(expected, actual, msg: string);
const
  c = '%s: Expected <%s> but found <%s>';
begin
  if expected <> actual then
    begin
      Writeln(Format(c, [msg, expected, actual]));
      halt(1);
    end
  else
    Writeln('...test passed.');
end;


begin
  Writeln('Start tests...');
  { What I use in my application }
  CheckEquals('', tiRemoveDrive('c:'), 'Failed on 1');
  CheckEquals('\temp', tiRemoveDrive('c:\temp'), 'Failed on 2');
  CheckEquals('\temp\hos.txt', tiRemoveDrive('c:\temp\hos.txt'), 'Failed on 3');
  CheckEquals('\Program Files\My Program\run.bat', tiRemoveDrive('c:\Program Files\My Program\run.bat'), 'Failed on 4');
  { To put the previous four test in a different way, calling ExtractFileDrive directly. }
  CheckEquals('c:', ExtractFileDrive('c:'), 'Failed on 5');
  CheckEquals('c:', ExtractFileDrive('c:\temp'), 'Failed on 6');
  CheckEquals('c:', ExtractFileDrive('c:\temp\hos.txt'), 'Failed on 6');
  CheckEquals('c:', ExtractFileDrive('c:\Program Files\My Program\run.bat'), 'Failed on 7');
  Writeln('Done.');
end.

