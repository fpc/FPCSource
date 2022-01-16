program tres6;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes;

type
  TMyRes = class(TComponent)
  private
    fStr1: String;
  published
    property Str1: String read fStr1 write fStr1;
  end;

{$R tres6.lfm}

var
  r: TMyRes;
begin
  r := TMyRes.Create(Nil);
  try
    try
      ReadComponentRes('TMyRes', r);
    except
      on e: Exception do begin
        Writeln('Exception (', e.ClassName, '): ', e.Message);
        DumpExceptionBacktrace(Output);
        Halt(1);
      end;
    end;
    Writeln(r.Str1);
    if r.Str1 <> 'Foobar' then
      Halt(2);
  finally
    r.Free;
  end;
end.
