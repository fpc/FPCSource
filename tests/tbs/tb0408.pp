{ This passes under Delphi and Borland pascal      }
{ for objects, classes don't pass, cf. /tbf/tb0125 }
type

  tobjsymbol = object
  end;

  tobjderivedsymbol = object(tobjsymbol)
  end;



procedure testobject(var t: tobjsymbol);
begin
end;


var
 myobject : tobjderivedsymbol;
begin
 testobject(myobject);
end.
