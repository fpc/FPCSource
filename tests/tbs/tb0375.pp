{ %VERSION=1.1 }
{$ifdef fpc}{$mode objfpc}{$endif}

type
   i1 = interface
      procedure intfp;
   end;

   tc1 = class(tinterfacedobject,i1)
      procedure i1.intfp = p;
      procedure p;
   end;

procedure tc1.p;

  begin
  end;

begin
end.
