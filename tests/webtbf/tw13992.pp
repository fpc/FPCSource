{ %fail }

{ new(property) should fail }

program test_prop;

{$mode delphi}

type
  TMyRec = record
    s: string;
  end;
  PMyRec = ^TMyRec;

  TSomeClass = class
  private
    FMyRec: PMyRec;
  public
    constructor Create;
    destructor Destroy; override;
    property MyRec: PMyRec read FMyRec write FMyRec;
  end;

{ TSomeClass }

constructor TSomeClass.Create;
begin
  New(MyRec);
end;

destructor TSomeClass.Destroy;
begin
//  Dispose(MyRec);
  inherited;
end;

begin
end.

