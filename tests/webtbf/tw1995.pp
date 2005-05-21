{ %FAIL }
unit tw1995;

interface

type
  TMemoryManager = record
    FreememSize : Function(p:pointer;Size:Longint):Longint;
  end;

  TMemoryManager2 = record
    FreememSize : Function(var p:pointer;Size:Longint):Longint;
  end;

  Function CFreeMemSize(var p:pointer;Size:Longint):Longint;
  Function CFreeMemSize2(p:pointer;Size:Longint):Longint;


Const
 CMemoryManager : TMemoryManager =
    (
      FreememSize : {$ifdef fpc}@{$endif}CFreememSize;
    );

 CMemoryManager2 : TMemoryManager2 =
    (
      FreememSize : {$ifdef fpc}@{$endif}CFreememSize2;
    );

implementation

Function CFreeMemSize(var p:pointer;Size:Longint):Longint;
begin
  CFreeMemSize:=0;
end;

Function CFreeMemSize2(p:pointer;Size:Longint):Longint;
begin
  CFreeMemSize2:=0;
end;

end.
