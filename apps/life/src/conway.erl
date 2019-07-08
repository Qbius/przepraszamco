-module(conway).
-export([conway/1]).

conway(State) ->
    Neighbours = lists:append(lists:map(fun([X, Y]) -> [[X + XOffset, Y + YOffset] || XOffset <- lists:seq(-1, 1), YOffset <- lists:seq(-1, 1), XOffset =/= 0 orelse YOffset =/= 0] end, State)),
    process_neighbour_map(neighbours_to_map(Neighbours), State).

neighbours_to_map(Neighbours) ->
    neighbours_to_map(Neighbours, #{}).
neighbours_to_map([H | T], Map) ->
    case Map of
	#{H := N} -> neighbours_to_map(T, Map#{H => N + 1});
        _ -> neighbours_to_map(T, Map#{H => 1})
    end;
neighbours_to_map([], Map) ->
    Map.

process_neighbour_map(NeighbourMap, InitialState) ->
    process_neighbour_map(maps:to_list(NeighbourMap), [], sets:from_list(InitialState)).
process_neighbour_map([{Coords, 3} | T], Result, InitialState) ->
    process_neighbour_map(T, [Coords | Result], InitialState);
process_neighbour_map([{Coords, 2} | T], Result, InitialState) ->
    process_neighbour_map(T, case sets:is_element(Coords, InitialState) of true -> [Coords | Result]; false -> Result end, InitialState);
process_neighbour_map([_ | T], Result, InitialState) ->
    process_neighbour_map(T, Result, InitialState);
process_neighbour_map([], Result, _) ->
    Result.
