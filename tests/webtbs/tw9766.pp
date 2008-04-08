{ %interactive }

{ instructions: set a breakpoint on PASCALMAIN, then step into }
{ TChild.Create(nil). This shouldn't crash gdb                 }

{$mode delphi}

type
  // swap the order of these declarations (TParent & TChild) and the problem is fixed.
  TParent = class;
  TChild = class;

  TParent = class
  private
    FChild : TChild; // remove me and the problem is fixed.
  public
    constructor Create ( AOwner : pointer ); virtual;
  end;

  TChild = class(TParent)
  private
    FField : Integer; // remove me and the problem is fixed.
  public
    constructor Create ( AOwner : pointer ); override;
  end;

{ TParent }

constructor TParent.Create(AOwner: pointer);
begin
  Inherited Create;
end;

{ TChild }

constructor TChild.Create(AOwner: pointer);
begin
  Inherited;
end;


begin
  TChild.Create(nil); // break-point here and try to step into constructor (gdb/stabs)
end.
