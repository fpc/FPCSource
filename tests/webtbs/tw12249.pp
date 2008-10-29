unit tw12249;

{$mode objfpc}{$H+}

interface

type

  { TG2 }

  generic TG2<T> = class
  public
    destructor Destroy; override;
  end;

implementation

{ TG2 }

destructor TG2.Destroy;
begin
  inherited Destroy;
end;

procedure P;
type 
  T = specialize TG2<Integer>;
begin
end;

end.

