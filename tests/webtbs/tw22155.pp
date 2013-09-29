{ %opt=-vn -Sen }

{$MODE DELPHI}
{.$DEFINE ALT_MODE}

type
  TWrapper = class
  strict private
    type
      TInternals = record
        class procedure Z; static;
      end;
    class var
      FInternals: TInternals;
  private
    class property Internals: TInternals read FInternals;
  public
    procedure Z;
  end;

class procedure TWrapper.TInternals.Z;
begin
end;

procedure TWrapper.Z;
begin
{$IFNDEF ALT_MODE}
  Internals.Z;   { Class variable FInternals “unused” }
{$ELSE}
  FInternals.Z;  { Class property Internals “unused” }
{$ENDIF}
end;


begin
  with TWrapper.Create do begin
    Z;
    Free;
  end;
end.
