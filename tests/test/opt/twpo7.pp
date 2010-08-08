{ %wpoparas=devirtcalls,optvmts }
{ %wpopasses=1 }


{$mode objfpc}

type
  tu1 = class
    procedure u1proccalled; virtual;
    procedure u1proccalledinoverride; virtual;
    procedure u1proccallednotoverridden; virtual;
    procedure u1procnotcalled; virtual;
    procedure u1procaddrtaken; virtual;
  end;

  tu2 = class(tu1)
    procedure u1proccalledinoverride; override;
  end;


  procedure tu1.u1proccalled;
    begin
      writeln('u1proccalled in u1');
    end;

  procedure tu1.u1proccalledinoverride;
    begin
      writeln('u1proccalledinoverride in u1');
      if (self.classtype=tu1) then
        halt(3);
    end;

  procedure tu1.u1proccallednotoverridden;
    begin
      writeln('u1proccallednotoverridden in u1');
      if not(self.classtype = tu1) then
        halt(4);
    end;

  procedure tu1.u1procnotcalled;
    begin
      writeln('u1procnotcalled in u1');
      halt(1);
    end;

  procedure tu1.u1procaddrtaken;
    begin
      writeln('procvar called');
    end;


  procedure tu2.u1proccalledinoverride;
    begin
      writeln('u1proccalledinoverride in u2');
      if (self.classtype <> tu2) then
        halt(10);
    end;

var
  u1: tu1;
  u2: tu2;
  p: procedure of object;
begin
  u1:=tu1.create;
  u1.u1proccalled;
  u1.u1proccallednotoverridden;
  u1.free;
  u2:=tu2.create;
  p:=@u2.u1procaddrtaken;
  p();
  u2.u1proccalled;
  u2.u1proccalledinoverride;
  u2.free;
end.
