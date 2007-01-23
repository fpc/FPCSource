{ %cpu=i386 }

program FieldDoesntResolve;

{$IFDEF FPC}
  {$mode delphi}
{$ENDIF}

type
  TMyRecord = record
    rField: Integer;
  end;

  TMyObject = class
  private
    oField: TMyRecord;
  public
    procedure Test;
  end;

{ TMyObject }

procedure TMyObject.Test;
asm
//  mov [eax + TMyObject.oField.rField], 0 // works in Delphi and FPC
  mov [eax + oField.rField], 5 // works only in Delphi
end;

begin
  with TMyObject.Create do try
    Test;
  finally
    if (ofield.rfield <> 5) then
      halt(1);
    Free;
  end;
end.
