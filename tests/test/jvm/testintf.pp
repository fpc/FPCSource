{ %norun }

{$mode objfpc}

{$namespace org.freepascal.test}

unit testintf;

interface

type
  tinterface1 = interface
    function test(l: longint): longint;
  end;

  tinterface2 = interface
    const
      iconst = longint(4);
    function test(b: byte): longint;
  end;

  tinterface3 = interface(tinterface1,tinterface2)
  end;

  tinterface4 = interface
    function intf4test(i: int64): longint;
  end;

  tintfclass = class(tinterface1,tinterface2,tinterface3)
    constructor create;
    function test(l: longint): longint;virtual;final;
    function Test(b: byte): longint;virtual;final;
    destructor destroy; override;
  end;

  tintfclass2 = class(tintfclass,tinterface4)
    constructor create;
    function intf4test(i: int64): longint;virtual;final;
  end;

implementation

  uses
    {$ifdef java}jdk15{$else}androidr14{$endif};

  constructor tintfclass.create;
    begin
    end;

  function tintfclass.Test(l: longint): longint;
    begin
      result:=l+1;
    end;


  function tintfclass.test(b: byte): longint;
    begin
      result:=b+2;
    end;


  destructor tintfclass.destroy;
    begin
      JLSystem.fout.println(555);
    end;

  constructor tintfclass2.create;
    begin
    end;


  function tintfclass2.intf4test(i: int64): longint;
    begin
      result:=i div 12345;
    end;


end.
