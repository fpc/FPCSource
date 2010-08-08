{ %opt=-gh }

    program copytest;

    var
    S, D : array of Integer;
 
    begin
      HaltOnNotReleased := true;
      SetLength(S,4000);
      D := Copy(Copy(S));
    end.
