{$mode objfpc}

type
   tclass1 = class
      procedure a;virtual;
      procedure b;virtual;
   end;

   tclass2 = class(tclass1)
      procedure a;override;
      procedure b;override;
      procedure c;virtual;
   end;


  procedure tclass1.a;

    begin
    end;

  procedure tclass1.b;

    begin
    end;

  procedure tclass2.a;

    begin
    end;

  procedure tclass2.b;

    begin
    end;


  procedure tclass2.c;

    begin
    end;

begin
end.
