{ Source provided for Free Pascal Bug Report 2942 }
{ Submitted by "Marco (Gory Bugs Department)" on  2004-02-06 }
{ e-mail:  }
unit tw2942b;

interface
{$mode Delphi}

Uses tw2942a;
type
  TIdStackWindows = class(TIdStack)
  public
    constructor Create; override;
    destructor Destroy; override;
     end;

implementation

constructor TIdStackWindows.Create;
begin
  inherited destroy;
end;

destructor TIdStackWindows.Destroy;
begin
  inherited Destroy;
end;
end.
