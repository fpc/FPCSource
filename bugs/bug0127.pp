unit test;

  interface

    procedure x;

  implementation

    procedure crash;

      begin
         x; { called with pascal calling conventions }
      end;

   procedure x;external;cdecl;

end.
