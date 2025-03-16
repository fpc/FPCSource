{ %NORUN }
{ Test for internal error on line 17 }

program tw41175;
{$pointermath on}

type
  PRec = ^TRec;
  TRec = record
    X: NativeInt;
  end;

var
  R: TRec;

begin
  R:=PRec(nil)[0];
  WriteLn('ok');
end.
