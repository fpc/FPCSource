{ %norun }

program tw41184;

{$mode delphi}
{$warn 6058 off}

type
  TMyClass = class;
  TMyClassClass = class of TMyClass;
  TMyClass = class
    class function Func: TMyClassClass; inline;
  end;

class function TMyClass.Func: TMyClassClass;
begin
  Func().Func;
end;

begin
end.
