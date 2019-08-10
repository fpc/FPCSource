{ %NORUN }

program tw35735;

{$Mode objfpc}

uses
  Classes, SysUtils;

type

  { TObjectHelper }

  TObjectHelper = class helper for TObject
  public
    generic function Test<T>(): String;
  end;

{ TComponentHelper }

generic function TObjectHelper.Test<T>: String;
begin
  Result := T.ClassName
end;

var
  O: TObject;
begin
  O := TObject.Create;
  WriteLn(O.specialize Test<TPersistent>);
  O.Free;
end.

