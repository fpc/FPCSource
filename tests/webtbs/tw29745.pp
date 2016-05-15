{ %NORUN }

program tw29745;

{$apptype console}
{$ifdef fpc}
{$mode objfpc}
{$h+}
{$codepage utf8}
{$endif}

uses Classes;

type
  TFoo = class helper for TStream
  public
    procedure Bar;
  end;

  procedure TFoo.Bar;
  begin
  end;

var
  s: string = '';
  m: TStream;
begin
  m := TMemoryStream.Create;
  try
    m.Bar;
  finally
    m.Free;
  end;
  writeln(defaultsystemcodepage);
end.

