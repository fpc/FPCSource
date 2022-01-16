program PackageVariantP;

{$mode objfpc}{$H+}

{$ifdef versiona}
{$fatal VersionA should only be defined in the PackageVariantA package}
{$endif}

uses
  {$ifdef PROCVersionB}
  PackageVariantVersionBOnly,
  {$endif}
  PackageVariantBaseUnit;

begin
  writeln(GetPackageVariantMessage);
  {$ifdef PROCVersionB}
  writeln(FuncPackageVariantVersionBOnly);
  {$endif}

end.
