{ %target=win32,win64 }
program project1;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, JwaWindows
  { you can add units after this };

begin
end.

