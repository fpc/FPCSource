{ %FAIL }
{ Old file: tbf0300.pp }
{ crash if method on non existing object is parsed (form bugs 651) OK 0.99.13 (PFV) }

 procedure nonexistent_class_or_object.method; begin end;
begin
end.
