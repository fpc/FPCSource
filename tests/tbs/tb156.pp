{ Old file: tbs0181.pp }
{ shows a problem with name mangling                    OK 0.99.9 (PM) }

{ shows a problem of name mangling  }
Program tb156;

  Uses tb157;

  var l : mylongint;
begin
  dummy(l);
end.
