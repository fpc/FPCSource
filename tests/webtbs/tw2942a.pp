unit tw2942a;


interface
{$mode Delphi}

Type
  TIdStack = class
  public
    constructor Create; reintroduce; virtual;
    destructor Destroy; override;
    end;

implementation

constructor TIdStack.Create;
begin
inherited destroy;
end;

destructor TIdStack.Destroy;
begin
  inherited Destroy;
end;


end.
