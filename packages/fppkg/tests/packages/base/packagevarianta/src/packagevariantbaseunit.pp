unit PackageVariantBaseUnit;

{$mode objfpc}{$H+}

interface

function GetPackageVariantMessage: string;

implementation

function GetPackageVariantMessage: string;
begin
  {$ifdef Hello}
  Result := 'Hello ';
  {$else}
  Result := 'Bye ';
  {$endif}

  {$ifdef versiona}
  Result := Result + 'version A';
  {$endif}

  {$ifdef versionb}
  Result := Result + 'version B';
  {$endif}
end;

end.
