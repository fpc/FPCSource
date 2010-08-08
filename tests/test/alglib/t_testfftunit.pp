{ %opt=-Mdelphi -Sa }
program t_testfftunit;
uses Sysutils, u_testfftunit;

begin
    Randomize();
    try 
        if not testfftunit_test_silent() then
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
