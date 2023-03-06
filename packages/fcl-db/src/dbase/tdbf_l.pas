{  This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install
  the package tdbf_l 0.0.
}

{$IFNDEF FPC_DOTTEDUNITS}
unit tdbf_l; 
{$ENDIF FPC_DOTTEDUNITS}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  Data.Dbf.Dbf, Data.Dbf.Reg, LazarusPackageIntf; 
{$ELSE FPC_DOTTEDUNITS}
uses
  dbf, dbf_reg, LazarusPackageIntf; 
{$ENDIF FPC_DOTTEDUNITS}

implementation

procedure Register; 
begin
  RegisterUnit('Dbf', @dbf_reg.Register); 
end; 

initialization
  RegisterPackage('tdbf_l', @Register)
end.
