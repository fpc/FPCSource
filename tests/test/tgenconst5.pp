{ %NORUN }
{$mode objfpc}
{
	test nested generic records with constants
}
program tgenconst5;

type
	generic THelperA<const U:integer> = record
		list: array[0..U-1] of byte;
	end;

type
	generic THelperB<T> = record
		value: T;
	end;

type
	generic TList<T; const U:integer> = record
		helperA: specialize THelperA<U>;
		helperB: specialize THelperB<T>;
	end;

var
	list: specialize TList<integer,32>;
begin
	writeln('sizeof:',sizeof(list));
end.
