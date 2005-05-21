{ %VERSION=1.1 }
{ %FAIL }
{ %OPT=-Sew -vw }

{$MODE OBJFPC}

{ This tests that non-implemented abstract methods which are
  overloaded (but not in all cases) will still give out a
  warning
 }
type
  tmyclass = class
   procedure myabstract(x: integer); virtual; abstract;
   procedure myabstract(z: byte); virtual; abstract;
  end;

  tmyclass2 = class(tmyclass)
   procedure myabstract(x: integer) ; override;
  end;


  procedure tmyclass2.myabstract(x: integer);
   begin
   end;


var
 cla : tmyclass2;
Begin
 cla := tmyclass2.create;
end.
