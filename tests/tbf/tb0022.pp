{ %FAIL }
{ Old file: tbf0127.pp }
{ problem with cdecl in implementation part             OK 0.99.7 (PFV) }

unit tbf0127;

  interface

    procedure x(l : longint);

  implementation

    procedure crash;

      begin
         x(1234); { called with pascal calling conventions }
      end;

   procedure x(l : longint);external;cdecl;

end.
