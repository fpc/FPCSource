program tmultilinestring17;

{$mode ObjFPC}
{$modeswitch MultiLineStrings}
{$MultiLineStringTrimLeft 6}

type
  TMessage = record
    Msg: String;
  end;

  TMyClass = class
  public
    procedure MyMessage(var Msg: TMessage); message `
      Multi
      Line
      Message!
    `;
  end;

  procedure TMyClass.MyMessage(var Msg: TMessage);
  begin
    WriteLn('Ok!');
  end;

  const M: TMessage = (
    Msg: `
      Multi
      Line
      Message!
    `
  );

begin
  with TMyClass.Create() do begin
    DispatchStr(M);
    Free();
  end;
end.
