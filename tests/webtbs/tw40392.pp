{$mode objfpc} {$modeswitch advancedrecords}

{ define doublefree}
var
	InitCount: int32 = 0;

type
	ManRec = record
		x: int32;
		class operator Initialize(var self: ManRec);
		class operator Finalize(var self: ManRec);
		class operator Copy(constref b: ManRec; var self: ManRec);
		class operator AddRef(var self: ManRec);
	end;

	class operator ManRec.Initialize(var self: ManRec);
	begin
		inc(InitCount);
	end;

	class operator ManRec.Finalize(var self: ManRec);
	begin
		dec(InitCount);
	end;

	class operator ManRec.Copy(constref b: ManRec; var self: ManRec);
	begin
		writeln('shouldn''t happen');
		halt(1);
	end;

	class operator ManRec.AddRef(var self: ManRec);
	begin
		writeln('shouldn''t happen');
		halt(2);
	end;

	function GetManRec: ManRec;
	begin
		result.x := 1;
	end;

	procedure Use(const mr: ManRec);
	begin
	end;

	procedure Main;
	var
		mr: ManRec;
	begin
	{$ifdef doublefree}
		GetManRec; // imperfect, double Finalize()
	{$else}
		mr := GetManRec; // ok
		Use(GetManRec); // ok as well
	{$endif}
	end;

begin
	Main;
	writeln('InitCount = ', InitCount, ' (should ideally be 0).');
        if InitCount <> 0 then
          halt(1);
end.
