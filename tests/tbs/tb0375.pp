{ %VERSION=1.1 }
{$mode objfpc}

type
   i1 = interface
      procedure intfp;
   end;

   tc1 = class(tobject,i1)
      procedure i1.intfp = p;
      procedure p;
   end;

procedure tc1.p;

  begin
  end;

begin
end.
