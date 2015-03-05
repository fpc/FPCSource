{ %NORUN }

program tw27348;

{$mode objfpc}

type
  TRect = record
    xyz: LongInt;
  end;

  TControl = class
  end;

  TWinControl = class(TControl)
    procedure AlignControls(AControl: TControl; var RemainingClientRect: TRect);
  end;

  TAlign = (
    alNone
  );

{ TWinControl }

procedure TWinControl.AlignControls(AControl: TControl;
  var RemainingClientRect: TRect);

  procedure DoPosition(Control: TControl; AAlign: TAlign; AControlIndex: Integer);

    function ConstraintHeight(NewHeight: integer): Integer;
    begin
      Result:=NewHeight;
    end;

    procedure ConstraintHeight(var NewTop, NewHeight: integer);
    begin
      NewHeight:=ConstraintHeight(NewHeight);
    end;

  begin

  end;

begin

end;


begin
end.
