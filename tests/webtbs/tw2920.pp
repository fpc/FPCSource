unit tw2920;

interface
{$mode Delphi}

Uses uw2920;

type myclass2= class(myclass)
                function bb:string; override;
                end;

implementation

function myclass2.bb:string;

begin
  bb:='b';
end;

begin
end.
