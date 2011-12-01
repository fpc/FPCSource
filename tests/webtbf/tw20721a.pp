{%norun}
{%fail}
program tw20721a;
{$mode delphi}
{$apptype console}

type
  TFrame = class
    type
    TNested = class
      procedure ProcN;
    end;

  var
    fField: integer;
    FNested: TNested;

    procedure ProcF;
    constructor Create;
    destructor Destroy; override;
    property Field: integer read fField write fField;
  end;

var
  Frame: TFrame;

  procedure TFrame.TNested.ProcN;
  begin
    ProcF;
  end;

  procedure TFrame.ProcF;
  begin
    WriteLn(Self.ClassName);
    WriteLn(NativeInt(Self));
    WriteLn(fField);
  end;

  constructor TFrame.Create;
  begin
    inherited;
    fField := 23;
    FNested := TNested.Create;
  end;

  destructor TFrame.Destroy;
  begin
    FNested.Free;
  end;

begin
  Frame := TFrame.Create;
  try
    Frame.ProcF; { results:
      TFrame
      <address of Frame variable>
      23
    }
    Frame.FNested.ProcN; { results:
      TFrame.TNested
      <address of field Frame.FNested>
      <unpredictable: garbage or AV>
    }
  finally
    Frame.Free
  end;

end.

