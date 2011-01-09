program tw18433;
{$mode objfpc}
type
  TBase = class
    function Print: String; virtual;
  end;

  TDesc1 = class(TBase)
    function Print: String; override;
  end;

  TDesc2 = class(TBase)
    function Print: String; override;
  end;

function TBase.Print: String;
begin
  Result := 'Base';
end;

function TDesc1.Print: String;
begin
  Result := inherited + '-Desc1';
end;

function TDesc2.Print: String;
begin
  Result := inherited Print + '-Desc2';
end;

begin
end.
