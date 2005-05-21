{$mode delphi}
unit tb0483u;

interface

  type
    tmyclass1 = class
    private
      procedure x(var l : longint);message 1234;
    public
      procedure defaulthandler(var msg);override;
    end;

  const
    testresult : longint = 0;


implementation

  procedure tmyclass1.defaulthandler(var msg);
    begin
      writeln('error; being in tmyclass1.defaulthandler');
      halt(1);
    end;


  procedure tmyclass1.x(var l : longint);
    begin
      testresult:=1;
    end;

end.
