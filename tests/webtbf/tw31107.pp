{ %FAIL }

program tw31107;

{$MODE DELPHI}

uses RTTI;

type
  TFoo = class
  private
    FBar: string;
  public
    property Bar: string read FBar;
  end;

begin
  Writeln(Assigned(TRttiContext.GetType(TFoo).GetProperty('Bar')));
end.
