{$ifdef fpc}
{$mode objfpc}
{$endif fpc}
type
  TAbstractClass = class abstract
  public
    procedure Test; virtual;
  end;

  TAbstractClassDescendant = class(TAbstractClass)
  public
    procedure Test; override;
  end;

  TSealedClass = class sealed
  public
    procedure Test;
  end;

procedure TAbstractClass.Test;
begin
end;

procedure TAbstractClassDescendant.Test;
begin
end;

procedure TSealedClass.Test;
begin
end;

var
  AClass: TAbstractClass;
  AClassDesc: TAbstractClassDescendant;
  SClass: TSealedClass;
begin
  AClass := TAbstractClass.Create;
  AClass.Test;
  AClass.Free;

  AClassDesc:= TAbstractClassDescendant.Create;
  AClassDesc.Test;
  AClassDesc.Free;

  SClass := TSealedClass.Create;
  SClass.Test;
  SClass.Free;
end.

