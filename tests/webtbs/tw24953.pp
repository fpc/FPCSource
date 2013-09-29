unit tw24953;

interface
{$mode delphi}

Type
   TPoolablePool<t:class,constructor>   =   class
                                        factory : TObject;
                                        constructor create;
                                      end;

   TPoolableFactory<t>= class
   end;

implementation

{ TPoolablePool<t> }

constructor TPoolablePool<t>.create;  // last line visible in -va output before crash
begin
  factory:=TPoolableFactory<t>.create;
end;

end.
