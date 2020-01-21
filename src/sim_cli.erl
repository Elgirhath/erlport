-module(sim_cli).
-export([
    start/0,
    start_input/0,
    print_table/1
]).
-include_lib("include/airport.hrl").

start() ->
    {ok, Tower} = control_tower:start_link(),
    update(Tower, [], []).

update(Tower, Planes, Lanes) ->
    SpawnRate = 0.5, % Planes per second
    SleepSeconds = 0.5,

    NewPlanes = update_plane_list(Planes, Tower, SpawnRate * SleepSeconds),
    {NewestPlanes, NewLanes} = process_input(NewPlanes, Lanes, Tower),

    ok = land_planes(Planes),

    repaint(NewestPlanes, NewLanes),
    timer:sleep(round(1000 * SleepSeconds)),
    update(Tower, NewestPlanes, NewLanes).

land_planes([]) -> ok;
land_planes([Plane1 | Tail]) ->
    % Get the planes to land
    case plane:get_state(Plane1) of
        prepare_for_landing ->
            %% if we got permission to land -> land the plane
            timer:sleep(2000),
            plane:land(Plane1),
            land_planes(Tail);
        in_air ->
            plane:permission_to_land(Plane1),
            timer:sleep(2000),
            land_planes(Tail)
    end.

update_plane_list(Planes, Tower, SpawnProbability) ->
    Rand = random:uniform(),
    if 
        Rand < SpawnProbability ->
            {ok, PlaneId} = plane:start(Tower),
            Planes ++ [PlaneId];
        true ->
            Planes
    end.

process_input(Planes, Lanes, Tower) ->
    ActionFileContent = readlines(get_connector()),
    file:write_file(get_connector(), ""),
    process_commands(ActionFileContent, Planes, Lanes, Tower).

process_commands([Command], Planes, Lanes, Tower) ->
    case Command of
        "ols" ->
            {Planes, Lanes ++ [control_tower:open_landing_strip(Tower)]};
        _Else ->
            {Planes, Lanes}
    end;

process_commands([Command | Tail], Planes, Lanes, Tower) ->
    {NewPlanes, NewLanes} = process_commands([Command], Planes, Lanes, Tower),
    process_commands(Tail, NewPlanes, NewLanes, Tower).


repaint(Planes, Lanes) ->
    PlaneStrings = lists:map(fun(Plane) ->
        pid_to_list(Plane) end,
        Planes
    ),
    PlaneStateStrings = lists:map(fun(Plane) ->
        plane:get_state(Plane) end,
        Planes
    ),
    LaneStrings = lists:map(fun(Lane) ->
        {_, Id, _} = Lane,
        integer_to_list(Id) end,
        Lanes
    ),
    LaneStateStrings = lists:map(fun(Lane) ->
        {_, _, Free} = Lane,
        atom_to_list(Free) end,
        Lanes
    ),
    PlaneColumn = ["Planes"] ++ PlaneStrings,
    PlaneStateColumn = ["Plane state"] ++ PlaneStateStrings,
    LaneColumn = ["Lanes"] ++ LaneStrings,
    LaneStateColumn = ["Lane empty"] ++ LaneStateStrings,
    
    clear_console(),
    print_table({[PlaneColumn, PlaneStateColumn, LaneColumn, LaneStateColumn], 20}).

clear_console() ->
    io:format("\e[H\e[J").
    % io:format(os:cmd(clear)).

log(Item) ->
    io:format("Log ~p\n", [Item]).

start_input() ->
    file:write_file(get_connector(), ""),
    update_input().

update_input() ->
    % clear_console(),
    Commands = get_command_dict(),
    for(1, length(Commands), fun(I) ->
        {Name, _} = lists:nth(round(I), Commands),
        io:format("~p. ~s\n", [I, Name]) end
    ),
    Index = input_int("enter>"),
    {_, ActionCode} = lists:nth(Index, Commands),
    file:write_file(get_connector(), io_lib:fwrite("~s\n", [ActionCode]), [append]),
    update_input().

get_connector() ->
    Path = "./temp/connector",
    filelib:ensure_dir(Path),
    file:write_file(Path, "", [append]),
    Path.

get_command_dict() ->
    % {Name, code}
    [
        {"Open landing strip", "ols"},
        {"Show land permission", "slp"},
        {"Land plane", "lp"}
    ].

for( Max, Max, F )  -> [ F(Max) ];
for( I, Max, F )    -> [ F(I) | for( I+1, Max, F ) ].

input_int(Prompt) ->
    Input = io:get_line(Prompt),
    {Index, _} = string:to_integer(Input),
    Index.

readlines(FileName) ->
    {ok, Data} = file:read_file(FileName),
    String = binary:bin_to_list(Data),
    string:split(String, "\n", all).

print_table(Table) ->
    {Columns, ColWidth} = Table,
    Width = length(Columns),
    ColHeights = lists:map(fun(Column) ->
        length(Column) end,
        Columns
    ),
    Height = 10,
    for(1, Height, fun(I) ->
        for(1, Width, fun(J) ->
            Column = lists:nth(J, Columns),
            if
                length(Column) > Height , I == Height ->
                    Element = "...";
                length(Column) >= I ->
                    Element = lists:nth(I, Column);
                true ->
                    Element = ""
            end,
            io:fwrite("|~-*.s", [ColWidth, Element])
        end
        ),
        io:fwrite("|~n")
    end
    ).