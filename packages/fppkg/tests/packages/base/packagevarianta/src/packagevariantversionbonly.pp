unit PackageVariantVersionBOnly;

{$mode objfpc}{$H+}

interface

function FuncPackageVariantVersionBOnly: string;

implementation

function FuncPackageVariantVersionBOnly: string;
begin
  Result := 'Now with extra unit!';
end;

end.
