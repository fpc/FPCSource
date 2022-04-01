{%NORUN}
{$mode objfpc}
{$modeswitch implicitfunctionspecialization}
{$modeswitch advancedrecords}
{
	Test generic record methods
}

program timpfuncspez8;
	
type
	TMyRecord = record
		generic class procedure Call<T>(msg: T); static;
		generic procedure DoThis<T>(msg: T);
	end;

generic class procedure TMyRecord.Call<T>(msg:T);
begin
end;

generic procedure TMyRecord.DoThis<T>(msg:T);
begin
end;

var
	rec: TMyRecord;
begin
	TMyRecord.Call('Hello World');
	rec.DoThis(1);
end.