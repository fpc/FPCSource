{ %interactive }

{ check that the contents of xyc and yxd are printed
  correctly by gdb at the end of the program, resp.
  [1..2, 9] and [1..3, 9]
}

program test;
{$R+}
{$coperators on}
const
  empty = [];

type
  menum = (mea = 1, meb, mec);

var
  xyc : set of 1..9;
  yxd : set of 0..9;
  mset: set of menum;
begin
  xyc := empty;
  xyc += [1];
  xyc += [2];
  xyc += [9];
  yxd := [1,2,3];
  yxd := yxd + [9];

  mset:=[meb];
  include(mset,mec);
end.
