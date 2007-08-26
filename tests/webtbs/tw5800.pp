{$IFDEF FPC}{$mode objfpc}{$ENDIF}

uses
	sysutils;

type
{$INTERFACES CORBA}
	IAny1 = interface
		//['{949041BD-BEC9-468A-93AA-96B158EF97E0}']
		procedure x;
	end;

	IAny2 = interface
        //['{4743E9F5-74B2-411D-94CE-AAADDB8F45E0}']
		procedure y;
	end;

	TAny = class(TInterfacedObject, IAny1, IAny2)
		procedure x;
		procedure y;
	end;


procedure TAny.x;
begin
	WriteLn('x');
end;

procedure TAny.y;
begin
	WriteLn('y');
end;

procedure any(const z : IAny1); overload;
begin
	z.x;
end;

procedure any(const z : IAny2); overload;
begin
	z.y;
end;


var
	a : TAny;

begin
	a := TAny.Create();

	if (supports(a, IAny1)) then begin end; // remove this line to get it compile

	any(a as IAny1);
	any(a as IAny2);

	//a.Free();
end.
