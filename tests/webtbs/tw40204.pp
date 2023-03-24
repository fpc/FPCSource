{ %wpoparas=devirtcalls,optvmts }
{ %wpopasses=1 }

{$mode objfpc} {$longstrings on}
uses
	Objects;

type
	MyObjBase = object
		constructor Create;
		function GetVirt: string; virtual; abstract;
	end;

	MyObjA = object(MyObjBase)
		constructor Create;
		function GetVirt: string; virtual;
	end;

	MyObjB = object(MyObjBase)
		constructor Create;
		function GetVirt: string; virtual;
	end;

	constructor MyObjBase.Create; begin end;
	constructor MyObjA.Create; begin end;
	function MyObjA.GetVirt: string; begin result := 'MyObjA.GetVirt'; end;
	constructor MyObjB.Create; begin end;
	function MyObjB.GetVirt: string; begin result := 'MyObjB.GetVirt'; end;

type
	MyObjFactory = record
		ctr: CodePointer;
		vmt: pointer;
	end;

const
	MyObjFactories: array[0 .. 1] of MyObjFactory =
	(
		(ctr: @MyObjA.Create; vmt: TypeOf(MyObjA)),
		(ctr: @MyObjB.Create; vmt: TypeOf(MyObjB))
	);

var
	o: MyObjBase;
	fact: MyObjFactory;

begin
	for fact in MyObjFactories do
	begin
		CallVoidConstructor(fact.ctr, @o, fact.vmt);
		writeln(o.GetVirt);
	end;
end.

