program texception17;

{$MODE OBJFPC} // This and {$MODE OBJFPC} fails, {$MODE DELPHI} succeeds

function GetIntf: IInterface;
begin
  Result := nil
end;

function GetString: string;
begin
  GetIntf;
  try
	Result := 'Caboom';
  finally
	GetIntf;
  end;
end;

begin
  writeln('before');
  writeln(GetString);
  writeln('after');
end.

