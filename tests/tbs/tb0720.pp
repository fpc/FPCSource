{$apptype console}
{$mode objfpc}
type
	Cls = class(TInterfacedObject)
		destructor Destroy; override;
	end;

var
	ClsInstance: IUnknown;

	destructor Cls.Destroy;
	begin
		writeln(Assigned(ClsInstance));
        if Assigned(ClsInstance) then
          halt(1);
	end;

begin
	ClsInstance := Cls.Create;
end.
