{%NORUN}
{$mode objfpc}
{$modeswitch implicitfunctionspecialization}
{
	Test generic methods
}

program timpfuncspez7;
	
type
	TMyClass = class
		generic class procedure Call<T>(msg: T);
		generic procedure DoThis<T>(msg: T);
	end;

generic class procedure TMyClass.Call<T>(msg:T);
begin
end;

generic procedure TMyClass.DoThis<T>(msg:T);
begin
end;

var
	obj:TMyClass;
begin
	TMyClass.Call('Hello World');
	obj := TMyClass.create;
	obj.DoThis(1);
end.