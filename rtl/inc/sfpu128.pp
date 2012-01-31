unit sfpu128;
{$mode objfpc}

{ Overflow checking must be disabled,
  since some operations expect overflow!
}
{$Q-}
{$goto on}


{ define FPC_SOFTFLOAT_FLOATX80}
{$define FPC_SOFTFLOAT_FLOAT128}


{$define fpc_softfpu_interface}
interface
{$i softfpu.pp}

{$undef fpc_softfpu_interface}
{$define fpc_softfpu_implementation}
implementation

{$i softfpu.pp}

end.
