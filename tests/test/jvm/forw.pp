{ %norun }

program forw;

{$mode delphi}

type
 TC = class
 public
    procedure execute;
 end;

procedure tc.execute;

       procedure nested1; forward;

       procedure nested2;
       begin

       end;

       procedure nested1;
       begin

       end;

begin

end;


begin
end.
