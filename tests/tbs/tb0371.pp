{ %VERSION=1.1 }
{ %target=win32 }

{$mode delphi}
unit tb0371;

interface

  const
     dllname = 'lalala';

  type
     pinteger = ^integer;

  procedure p1(var i : integer);overload;
  procedure p1(i : pinteger);overload;
  procedure p2(var i : integer);overload;
  procedure p2(i : pinteger);overload;

implementation

  procedure p1(var i : integer);overload;external dllname;
  procedure p1(i : pinteger);overload;external dllname;
  procedure p2(var i : integer);external dllname name 'lalala';
  procedure p2(i : pinteger);external dllname name 'lalala';

begin
end.
