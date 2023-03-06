{ Test Generics.Collections, adapted from Castle Game Engine testcase
  tests/code/testcases/testgenericscollections.pas
}

{$mode objfpc}{$H+}
{$assertions on}

uses Generics.Collections, Generics.Defaults;
type
  TMyVector = packed array [0..1] of Single;
  TMyVectorList = {$ifdef FPC}specialize{$endif} TList<TMyVector>;
var
  List: TMyVectorList;
  R1, R2, R: TMyVector;
begin
  List := TMyVectorList.Create;
  try
    R1[0] := 11;
    R1[1] := 22;
    List.Add(R1);

    R2[0] := 33;
    R2[1] := 44;
    List.Add(R2);

    R2[0] := 33;
    R2[1] := 44;
    List.Add(R2);

    Assert(3 = List.Count);
    Assert(11 = List[0][0]);
    Assert(22 = List[0][1]);
    Assert(33 = List[1][0]);
    Assert(44 = List[1][1]);
    Assert(33 = List[2][0]);
    Assert(44 = List[2][1]);

    List.Delete(2);

    Assert(2 = List.Count);
    Assert(11 = List[0][0]);
    Assert(22 = List[0][1]);
    Assert(33 = List[1][0]);
    Assert(44 = List[1][1]);

    Assert(0 = List.IndexOf(R1));
    Assert(1 = List.IndexOf(R2));

    // change R1 and R2, to make sure it doesn't matter for tests
    R1[0] := 111111;
    R1[1] := 222222;
    R2[0] := 333333;
    R2[1] := 444444;
    Assert(-1 = List.IndexOf(R1));
    Assert(-1 = List.IndexOf(R2));

    R[0] := 11;
    R[1] := 22;
    Assert(0 = List.IndexOf(R));

    R[0] := 33;
    R[1] := 44;
    Assert(1 = List.IndexOf(R));

    R[0] := 11;
    R[1] := 22;
    List.Remove(R);
    Assert(1 = List.Count);
    Assert(33 = List[0][0]);
    Assert(44 = List[0][1]);

    R[0] := 666;
    R[1] := 22;
    List.Remove(R); // does nothing, no such item
    Assert(1 = List.Count);
    Assert(33 = List[0][0]);
    Assert(44 = List[0][1]);
  finally List.Free end;
end.
