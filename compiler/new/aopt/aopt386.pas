unit aopt386;

Interface

uses aopt;

Type TAsmOptimizer386 = Object(TAsmOptimizer)
       Constructor Init(_AsmL: PAasmOutput);
       Destructor done;
     End;

Implementation

Constructor init(_AsmL: PAasmOutput);
begin
  inherited init(_AsmL);
End;

Destructor Done;
Begin
  inherited done
End;

End.