{$mode objfpc} {$modeswitch advancedrecords}
var
	initialized: boolean = false;

type
	InnerRec = record
		class operator Initialize(var self: InnerRec);
	end;

	OuterRec = record
		inner: InnerRec;
		class operator Initialize(var self: OuterRec);
	end;

	class operator InnerRec.Initialize(var self: InnerRec);
	begin
		initialized := true;
	end;

	class operator OuterRec.Initialize(var self: OuterRec);
	begin
	end;

begin
	Initialize(OuterRec(nil^));
	if not initialized then
	begin
		writeln('Inner record not initialized.');
		halt(1);
	end;
	writeln('ok');
end.
