unit ub0158a;

interface

uses ub0158b;

implementation

{ should give "duplicate identifier error", because already used in the }
{ interface                                                             }
uses ub0158b;

end.
