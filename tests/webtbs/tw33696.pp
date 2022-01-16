{$mode objfpc}{$H+}
type
  ternary = (F, U, T);

  operator and (const a,b:ternary):ternary;inline;
    const lookupAnd:array[ternary,ternary] of ternary =
                    ((F,F,F),(F,U,U),(F,U,T));
  begin
    Result:= LookupAnd[a,b];
  end;

  operator or (const a,b:ternary):ternary;inline;
    const lookupOr:array[ternary,ternary] of ternary =
                   ((F,U,T),(U,U,T),(T,T,T));
  begin
    Result := LookUpOr[a,b];
  end;

  operator not (const a:ternary):ternary;inline;
    const LookupNot:array[ternary] of ternary =(T,U,F);
  begin
     Result:= LookUpNot[a];
  end;


begin
  // works as expected
  writeln('AND');write(F and F);write(F and U);
  writeln(F and T);write(U and F);write(U and U);
  writeln(U and T);write(T and F);write(T and U);
  writeln(T and T);
  writeln;
  //works as expected
  writeln('OR');write(F or F);write(F or U);
  writeln(F or T);write(U or F);write(U or U);
  writeln(U or T);write(T or F);write(T or U);
  writeln(T or T);
  writeln;
  // this fails, but compiles and runs w/o error indication.
  // and renders the wrong results
  writeln('NOT');
  writeln(not F);// prints -1 unstead of T, it does not pick the overload!
  writeln(not U);// prints -2 instead of U, it does not pick the overload!
  writeln(not T);// prints -3 instead of F, it does not pick the overload!
  // which makes this next construct impossible while the compiler suggests it's legal.
  {
   writeln(T and not F);
  }
end.
