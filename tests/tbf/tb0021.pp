{ %FAIL }
{ Old file: tbf0117.pp }
{ internalerror 17 (and why is there an automatic float OK 0.99.6 (FK) }

var
 i: word;
 j: integer;
Begin
 i:=65530;
 i:=i+1;     { CF check  }
 i:=i-1;
 i:=i*5;
 i:=i/5;
 i:=i shl 5;
 i:=i shr 5;
 Inc(i);     { no check  }
 j:=32765;   { OV check  }
 j:=j+1;
 inc(j);
 j:=j-1;
 j:=j*5;
 j:=j div 5;
 j:=j shl 5;
 j:=j shr 5;
end.
