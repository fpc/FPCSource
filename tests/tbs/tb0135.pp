{ Old file: tbs0159.pp }
{ Invalid virtual functions - should compile            OK 0.99.7 (FK) }

Type TParent = Object
       Procedure SomeProc;
       end;

     TChild = Object(TParent)
       Procedure SomeProc; virtual;
       end;


       Procedure TParent.someproc;
       Begin
       end;


      procedure TChild.Someproc;
      Begin
      end;



Begin
end.
