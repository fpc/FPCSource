unit tbug701c;

  interface  
    procedure TestProc(arg: AnsiString);

  implementation

    var
       s: array[0..9] of AnsiString;
  
    procedure TestProc(arg: AnsiString);

      begin
         s[0] := arg + '!';
      end;

end.
