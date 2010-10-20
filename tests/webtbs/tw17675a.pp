{ %fail }

program project1;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes;

type

  TBase = class
  public
    function Count: Integer; overload;
  end;

  TSub = class(TBase)
  private
    function GetCount: Integer;
  public
    property Count: Integer read GetCount;
  end;

function TSub.Count: Integer;
begin
  Result := 0;
end;

{ TBase }

function TBase.GetCount: Integer;
begin
  Result := 0;
end;

var
  MySub: TSub;
  i : Integer;
begin
  MySub := TSub.Create;
// uncomment the next line for Fatal Internal error 200111022:
 for i := 0 to MySub.Count do begin end;
end.
