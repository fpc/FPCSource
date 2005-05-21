{ Source provided for Free Pascal Bug Report 3227 }
{ Submitted by "mickaël leduque" on  2004-08-03 }
{ e-mail: mickael.leduque@laposte.net }
uses Variants;


type
  TGffVarType           = (

    gffBYTE,            // 0

    gffCHAR,            // 1

    gffWORD,            // 2

    gffSHORT,           // 3

    gffDWORD,           // 4

    gffINT,             // 5

    gffDWORD64,         // 6

    gffINT64,           // 7

    gffFLOAT,           // 8

    gffDOUBLE,          // 9

    gffVoid,            // 13

    gffStruct,          // 14

    gffList             // 15

  );

  TGffVarData           = record

    AsVoid              : array of Char;

    case TGffVarType of

      gffBYTE           : (AsByte: Byte);

      gffCHAR           : (AsChar: Shortint);

      gffWORD           : (AsWord: Word);

      gffSHORT          : (AsShort: Smallint);

      gffDWORD          : (AsDWord: Longword);

      gffINT            : (AsInt: Longint);

      gffDWORD64,

      gffINT64          : (AsInt64: Int64);

      gffFLOAT          : (AsFloat: Single);

      gffDOUBLE         : (AsDouble: Double);

  end;






var     FType : TGffVarType;

        Machin : variant;
        Data : TGffVarData;

begin
SetLength(Data.AsVoid,3);
Data.AsVoid[0]:='b';
Data.AsVoid[1]:='c';
Data.asFloat:=0.0;
FType:=gffBYTE;

  case FType of

    gffBYTE:

      Machin            := Data.AsByte;

    gffCHAR:

      Machin            := Data.AsChar;

    gffWORD:

      Machin            := Data.AsWord;

    gffSHORT:

      Machin            := Data.AsShort;

    gffDWORD:

      Machin            := Data.AsDWord;

    gffINT:

      Machin            := Data.AsInt;

    gffDWORD64, gffINT64:

      Machin            := Data.AsInt64;

    gffFLOAT:

      Machin            := Data.AsFloat;

    gffDOUBLE:

      Machin            := Data.AsDouble;

    gffVoid:

      Machin            := Data.AsVoid;

  end;

end.
