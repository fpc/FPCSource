{ %NORUN }

program tw20871;
{$MODE delphi}
{$DEFINE CAUSE_ERROR}

type
  TInterior<TValue> = class end;

  TExterior<TValue> = class
  strict private
    FInterior: TInterior<TValue>;
  public
    constructor Create;
    destructor Destroy; override;
  end;

constructor TExterior<TValue>.Create;
{$IFDEF CAUSE_ERROR}
begin
  FInterior := TInterior<TValue>.Create;
                             { ^ Compiler reports here that “<” is expected }
{$ELSE}
type
  TSpecializedInterior = TInterior<TValue>;
begin
  FInterior := TSpecializedInterior.Create;
{$ENDIF}
end;

destructor TExterior<TValue>.Destroy;
begin
  FInterior.Free;
end;

begin
  TExterior<Integer>.Create.Free;
end.
