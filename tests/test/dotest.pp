unit dotest;

  interface
{$ifdef go32v2}
    uses
       dpmiexcp;
{$endif go32v2}

    procedure do_error(l : longint);

  implementation

    procedure do_error(l : longint);

      begin
         writeln('Error near: ',l);
         halt(100);
      end;

end.