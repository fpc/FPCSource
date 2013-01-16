{ %target=darwin }
program test;
uses
	MacOSAll;

procedure Callback (data: ptr; context: ptr); mwpascal;
begin
end;

var
	arr: CFMutableArrayRef;
begin
	arr := CFArrayCreateMutable(nil, 0, @kCFTypeArrayCallBacks);
	CFArrayApplyFunction(arr, CFRangeMake(0, CFArrayGetCount(arr)), CFArrayApplierFunction(@Callback), nil);
end.

