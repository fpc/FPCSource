{ %FAIL }

program tw40621;
{$mode delphi}{$H+}
uses uw40621;

var
  X: TRecord< int32 >; // declared in unitFault

begin
  X.PrivateMember := 'Should not be able to assign this.';
  //Writeln( X.PrivateMember );
  //Readln;
end.


