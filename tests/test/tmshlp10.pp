{ %NORUN }

{$mode objfpc}
{$modeswitch multihelpers}

program tmshlp10;

type
	TMyObject = class
		procedure DoThis(param: integer); overload;
	end;
	THelper1 = class helper for TMyObject
		procedure DoThis(param: string); overload;
	end;
	THelper2 = class helper for TMyObject
		procedure DoThis(param: pointer); overload;
	end;

procedure TMyObject.DoThis(param: integer);
begin
end;

procedure THelper1.DoThis(param: string);
begin
end;

procedure THelper2.DoThis(param: pointer);
begin
end;

var
	obj: TMyObject;
begin
	obj := TMyObject.Create;
	obj.DoThis(1);
	obj.DoThis('string');
	obj.DoThis(nil);
end.
