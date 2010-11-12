set FPC=fpcpp
rmdir /s /q units
mkdir units
call %FPC% -FUunits -Fuunit1 -Fuunit2 main/main
call %FPC% -FUunits main/main
