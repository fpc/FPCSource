{ Should give the same error as /tbf/tb0125.pp }
type

  tinterface = interface
      procedure x;
  end;
  
  tderivedinterface = interface(tinterface)
      procedure x;
  end;

  procedure testintparam(var i  : tinterface);
   begin
   end;

var
 t1 : tderivedinterface;
begin
  testintparam(t1);
end.
  
  
  
  