{$ifdef fpc}{$mode objfpc}{$endif}

uses
  sysutils;

var ErrorFileHandle : Text;

begin
  //No Assignfile
  Writeln('----');
  try
    CloseFile(ErrorFileHandle);
  except
    on E: Exception
        do Writeln('ExceptionMessage: '+e.Message);
  end;
  Writeln('----');
end.
