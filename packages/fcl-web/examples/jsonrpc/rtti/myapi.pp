unit myapi;

{$mode ObjFPC}{$H+}

interface

uses sysutils; // for TStringArray

Type
  { enable RTTI for methods! }
  {$M+}
  IMyInterface = interface ['{E4C73198-0831-47B9-944C-E2D7EFAE1C6A}']
   procedure SayHello;
   function Echo(args : Array of string) : String;
   function DoSum(a,b : Integer) : integer;
   function Split(aLine,aSep : string) : TStringArray;
   function DoVarTest(var aArg: String): Boolean;
  end;

  IMyOtherInterface = interface ['{4D52BEE3-F709-44AC-BD31-870CBFF44632}']
    Function SayHello : string;
    function Echo(args : TStringArray) : String;
  end;

implementation

end.

