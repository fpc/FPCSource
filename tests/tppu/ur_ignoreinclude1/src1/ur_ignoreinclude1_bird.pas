unit ur_ignoreinclude1_bird;

{$mode objfpc}

interface

function Fly(w: word): word;

implementation

{$I ur_ignoreinclude1_bird.inc}

end.
