{ %fail }
unit tw11970;
{$mode objfpc}
{$interfaces corba}
interface

type
  ID3D10EffectScalarVariable = class;

  ID3D10EffectVariable = interface
  end;

  ID3D10EffectVariable_FPC = class(ID3D10EffectVariable)
  end;

  ID3D10EffectScalarVariable = interface(ID3D10EffectVariable)
  end;

  ID3D10EffectScalarVariable_FPC = class(ID3D10EffectVariable_FPC, ID3D10EffectScalarVariable)
  end;

implementation

end.
