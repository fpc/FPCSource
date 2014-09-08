{ %recompile=-drecompile}
{ %norun }

{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit tw25610;

interface

uses
  uw25610a, uw25610b;

implementation

procedure Register;
var
  arr: array of byte;
begin
  setlength(arr,1);
  DynArraySize(pointer(arr));
end;

end.
