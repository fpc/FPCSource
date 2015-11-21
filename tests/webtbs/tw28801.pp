program project1;

{$mode delphi}

type
  TTest = class
    public
      destructor Destroy; override;
      class destructor Destroy;
  end;

{ TTest }


class destructor TTest.Destroy;
begin
  exitcode:=0;
end;

destructor TTest.Destroy;
begin

end;

begin
  halt(1);
end.
