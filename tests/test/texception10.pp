{$ifdef fpc}{$mode objfpc}{$H+}{$endif}

uses
  Classes, SysUtils;

type
  { TMyObject }

  TMyObject = class(TObject)
  public
    constructor Create(TheOwner: TObject);
  end;

{ TMyObject }

constructor TMyObject.Create(TheOwner: TObject);
begin
  // create AV
  if TheOwner.ClassName='' then;
end;

var
  i : integer;
begin
  i:=0;
  writeln('Creating the first time');
  try
    TMyObject.Create(nil);
  except
    on E: Exception do begin
      writeln('E='+E.Message);
      inc(i);
    end;
  end;
  writeln('Creating the second time');
  try
    TMyObject.Create(nil);
  except
    on E: Exception do begin
      writeln('E='+E.Message);
      inc(i);
    end;
  end;
  writeln('Ending ..');
  if i<>2 then
    halt(1);
end.
