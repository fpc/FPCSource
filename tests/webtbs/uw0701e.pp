unit uw0701e;

  interface

    procedure TestProc(arg: AnsiString);

    var
       s1: array[0..9] of AnsiString;

  implementation

    var
       s2: array[0..9] of AnsiString;

    procedure TestProc(arg: AnsiString);

      begin
         s1[0] := arg + '!s10';
         s1[3] := arg + '!s13';
         s2[4] := arg + '!s24';
         s2[7] := arg + '!s27';
      end;
initialization
finalization
end.
