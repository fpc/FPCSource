program project1;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes;

type

  TBase = class
  private
    function GetCount: Integer;
  public
    property Count: Integer read GetCount;
  end;

  TSub = class(TBase)
  public
    function Count: Integer; overload;
  end;

function TSub.Count: Integer;
begin
  Result := 2;
end;

{ TBase }

function TBase.GetCount: Integer;
begin
  Result := 1;
end;

var
  MySub: TSub;
  i : Integer;
begin
  MySub := TSub.Create;
// uncomment the next line for Fatal Internal error 200111022:
 if MySub.Count <> 2 then
   halt(1);
end.
