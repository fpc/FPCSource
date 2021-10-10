{ %norun }
{$mode objfpc}
type
	Collide29685295 = type uint32;
	Collide32060020 = type uint32;

	SomeType = class
	type
		SomeNestedType = class
		type
			YetAnotherNestedType = class
				class procedure Hello(arg: YetAnotherNestedType; arg2: Collide29685295); static;
				class procedure Hello(arg: YetAnotherNestedType; arg2: Collide32060020); static;
			end;
		end;
	end;

	class procedure SomeType.SomeNestedType.YetAnotherNestedType.Hello(arg: YetAnotherNestedType; arg2: Collide29685295);
	begin
	end;

	class procedure SomeType.SomeNestedType.YetAnotherNestedType.Hello(arg: YetAnotherNestedType; arg2: Collide32060020);
	begin
	end;

begin
end.
