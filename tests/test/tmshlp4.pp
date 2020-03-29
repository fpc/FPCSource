{ %NORUN }

{$mode objfpc}
{$modeswitch multihelpers}

program tmshlp4;

type
	TMyObject = class
		procedure DoThis_1;
	end;
	THelperBase = class helper for TMyObject
		procedure DoThis_4;
	end;
	THelper1 = class helper(THelperBase) for TMyObject
		procedure DoThis_2;
	end;
	THelper2 = class helper(THelperBase) for TMyObject
		procedure DoThis_3;
	end;

procedure THelperBase.DoThis_4;
begin
	writeln('DoThis_4');
end;

procedure TMyObject.DoThis_1;
begin
	writeln('DoThis_1');
end;

procedure THelper1.DoThis_2;
begin
	writeln('DoThis_2');
end;

procedure THelper2.DoThis_3;
begin
	writeln('DoThis_3');
end;

var
	obj: TMyObject;
begin
	obj := TMyObject.Create;
	obj.DoThis_1;
	obj.DoThis_2;
	obj.DoThis_3;
	obj.DoThis_4;
end.
