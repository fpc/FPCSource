{ %opt=-O2 -Os -B }
{ %cpu=avr }

program tshift_size;

{ Test correctness of code generated for shifts with compile time constant shift distance.
  This test is compiled to test the size optimized code generator. }

uses
  ushift;

begin
  testSHL8;
  writeln;
  testSHR8;
  writeln;
  testSHL16;
  writeln;
  testSHR16;
  writeln;
  testSHL32;
  writeln;
  testSHR32;
  writeln;
  testSHL64;
  writeln;
  testSHR64;
  Halt(0);
end.

