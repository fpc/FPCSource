{ %target=darwin }
{ %fail }
{ %opt=-Sew }

{$mode objfpc}
{$modeswitch objectivec2}

uses
  uobjc41;

type
	NSDictionaryUtilities = objccategory (NSSubject)
           { the "key" paramter should give a warning because there's already a "key"
             message in a category for NSObject }
		function containsKey (key: NSString): boolean; message 'containsKey:';
	end;

function NSDictionaryUtilities.containsKey (key: NSString): boolean;
begin
  result:=false;
end;

begin
end.
