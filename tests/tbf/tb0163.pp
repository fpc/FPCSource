{ %fail }
unit Testunit;
{This doesn't fail although it should due to non resolved forward. OR}

interface

implementation

procedure x; forward;

//begin   {If the comment is removed, it fails as it should.}
end.
