{ %fail }

unit tw2357;
interface
 type
 TQ = class(TObject)
 public
 procedure DoSome(const X); virtual; abstract;
 function GetSome : integer; virtual; overload;
 function GetSome : string; virtual; overload; abstract;
 end;
 implementation
 end.
