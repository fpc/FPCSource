{ Source provided for Free Pascal Bug Report 2956 }
{ Submitted by "Marco (Gory Bugs Department)" on  2004-02-08 }
{ e-mail:  }

program something;
{$mode Delphi}
Uses uw2956;

type
     localclassfields = class(aclasswithfields)
                        end;
     classwithmethod = class
                        procedure xx;
                        end;

procedure classwithmethod.xx;

var ll : aclasswithfields;

begin
 writeln(localclassfields(ll).field1);  // goes ok.
 with localclassfields(ll) do           // typecast.
   writeln(field1);
end;

begin
end.
