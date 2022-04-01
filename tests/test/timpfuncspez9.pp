{%NORUN}
{$mode objfpc}
{$modeswitch implicitfunctionspecialization}
{
	Test generic object methods
}

program timpfuncspez9;
	
type
	TMyObject = object
		generic class procedure Call<T>(msg: T); static;
		generic procedure DoThis<T>(msg: T);
	end;

generic class procedure TMyObject.Call<T>(msg:T);
begin
end;

generic procedure TMyObject.DoThis<T>(msg:T);
begin
end;

var
	rec: TMyObject;
begin
	TMyObject.Call('Hello World');
	rec.DoThis(1);
end.