unit test;

  interface

    procedure x(l : longint);

  implementation

    procedure crash;

      begin
         x(1234); { called with pascal calling conventions }
      end;

   procedure x(l : longint);external;cdecl;

end.
