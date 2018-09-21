unit uw34287b;

{$IFDEF FPC}
 {$MODE OBJFPC}{$H+}
{$ENDIF}

interface

uses
  Classes;

type
  TBase = class(TComponent)
  public
    generic function Bar<T: TComponent>(const P1: string; out P2: T): Boolean;
  end;

  TFoo = class(TBase)
  public
    function Bar(const P1: string): Boolean;
  end;

implementation

generic function TBase.Bar<T>(const P1: string; out P2: T): Boolean;
begin
  Result := False;
end;

function TFoo.Bar(const P1: string): Boolean;
var
  C: TComponent;
begin
  Result := inherited specialize Bar<TComponent>(P1, C);
end;

end.
