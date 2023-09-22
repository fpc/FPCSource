{ %cpu=wasm32 }
{ %norun }

library tjspromise1;

var
  state: double;

function init_state: double; external 'js';
function compute_delta: double; external 'js' suspending;

procedure init;
begin
  state := init_state;
end;

function get_state: double;
begin
  get_state := state;
end;

function update_state: double;
begin
  state := state + compute_delta;
  update_state := state;
end;

exports
  get_state,
  update_state promising;

begin
  init;
end.
