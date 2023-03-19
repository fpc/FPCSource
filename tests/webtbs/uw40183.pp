unit uw40183;

{$mode ObjFPC}{$H+}

interface

uses
  Types;

type
  TBuffer = packed record
    Mask: Int64;     
    DataArray: TObjectDynArray;
  end;

var
  GlobalBuffer: TBuffer;

implementation

initialization  
  GlobalBuffer := Default(TBuffer);

end.
