{ original: peter5a.pas from the GNU Pascal testsuite }

{ Mac Pascal objects }

{$mode macpas}

program peter5a;

        type
                Str = String[100];
                BaseObject = object
                        v1: Str;
                        function m1: Str;
                        function m2: Str;
                end;
                SuperObject = object(BaseObject)
                        v2: Str;
                        v3: Str;
                        function m1: Str; override;
                        function m2: Str; override;
                        function m3: Str;
                end;

        var
                good: Boolean;

        function BaseObject.m1: Str;
        begin
                return 'BaseObject.' + v1;
        end;

        function BaseObject.m2: Str;
        begin
                return 'BaseObject.nov2';
        end;

        function SuperObject.m1: Str;
        begin
                return 'SuperObject.' + (inherited m1) + '.' + v1;
        end;

        function SuperObject.m2: Str;
        begin
                return 'SuperObject.' + (inherited m2) + '.' + v2;
        end;

        function SuperObject.m3: Str;
        begin
                return 'SuperObject.' + v3;
        end;

        procedure CheckEqual( const param, s1, s2: Str );
        begin
                if s1 <> s2 then begin
                        good := false;
                        WriteLn( 'Failed: ', param, ' = ', s1, ' is not equal to ', s2 );
                end;
        end;

        var
                base: BaseObject;
                super: SuperObject;
                reallysuper: BaseObject;
begin
        New(base);
        base.v1 := 'basev1';

        New(super);
        super.v1 := 'superv1';
        super.v2 := 'superv2';
        super.v3 := 'superv3';

        reallysuper := super; { reference copy only! }

        good := true;

        CheckEqual( 'base.m1', base.m1, 'BaseObject.basev1' );
        CheckEqual( 'base.m2', base.m2, 'BaseObject.nov2' );

        CheckEqual( 'super.m1', super.m1, 'SuperObject.BaseObject.superv1.superv1' );
        CheckEqual( 'super.m2', super.m2, 'SuperObject.BaseObject.nov2.superv2' );
        CheckEqual( 'super.m3', super.m3, 'SuperObject.superv3' );

        CheckEqual( 'reallysuper.m1', reallysuper.m1, 'SuperObject.BaseObject.superv1.superv1' );
        CheckEqual( 'reallysuper.m2', reallysuper.m2, 'SuperObject.BaseObject.nov2.superv2' );

        if good then begin
                WriteLn( 'OK' );
        end
        else begin
                halt(1);
        end;

        Dispose( base );
        Dispose( super );
end.
