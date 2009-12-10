{ %opt=-Mdelphi -Sa }
program t_testfhtunit;
uses Sysutils, u_testfhtunit;

begin
    Randomize();
    try 
        if not testfhtunit_test_silent() then
        begin
            //WriteLn('*');
            Halt(1);
        end;
    except on E: Exception do 
        begin
            Halt(2);
        end;
    end;
    Halt(0);
end.
