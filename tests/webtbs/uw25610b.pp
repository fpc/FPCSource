unit uw25610b;

interface

function DynArraySize(p : pointer): tdynarrayindex;

implementation

uses
  uw25610a;

function DynArraySize(p : pointer): tdynarrayindex; external name 'FPC_DYNARRAY_LENGTH';

begin
  upcase(Foo);
end.

