{ %VERSION=1.1 }
{$mode delphi}
type
   tc1 = class
      procedure a;overload;virtual;
   end;

   tc2 = class(tc1)
      procedure a;override;
   end;

procedure tc1.a;

  begin
  end;

procedure tc2.a;

  begin
  end;

begin
end.
