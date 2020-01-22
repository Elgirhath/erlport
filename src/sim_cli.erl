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

    % NewPlanes = update_plane_list(Planes, Tower, SpawnRate * SleepSeconds),
    {NewestPlanes, NewLanes} = process_input(Planes, Lanes, Tower),

    simulate_planes(Planes),

    repaint(NewestPlanes, NewLanes),
    timer:sleep(round(1000 * SleepSeconds)),
    update(Tower, NewestPlanes, NewLanes).

simulate_planes([]) -> ok;
simulate_planes([PlaneWrapper | Tail]) ->
    % Get the planes to land
    {Pid, _} = PlaneWrapper,
    case plane:get_state(Pid) of
        prepare_for_landing ->
            %% if we got permission to land -> land the plane
            plane:land(Pid);
        in_air ->
            plane:permission_to_land(Pid);
        on_the_ground ->
            plane:permission_to_start(Pid);
        prepare_for_takeoff ->
            plane:fly(Pid)
    end,
    simulate_planes(Tail).

update_plane_list(Planes, Tower, SpawnProbability) ->
    Rand = random:uniform(),
    if 
        Rand < SpawnProbability ->
            Plane = plane:start_and_get_wrapper(Tower),
            Planes ++ [Plane];
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
        "cls" ->
            if
                length(Lanes) =< 0 ->
                    {Planes, Lanes};
                true ->
                    {NewLanes, [OldLane]} = lists:split(length(Lanes) - 1, Lanes),
                    control_tower:close_landing_strip(Tower, OldLane),
                    {Planes, NewLanes}
            end;
        "adp" ->
            {Planes ++ [plane:start_and_get_wrapper(Tower)], Lanes};
        "rmp" ->
            if
                length(Planes) =< 0 ->
                    {Planes, Lanes};
                true ->
                    {NewPlanes, [OldPlane]} = lists:split(length(Planes) - 1, Planes),
                    % {OldPlanePid, OldPlaneObj} = OldPlane,
                    % plane:terminate(normal, plane:get_state(OldPlane), OldPlaneObj),
                    {NewPlanes, Lanes}
            end;
        _Else ->
            {Planes, Lanes}
    end;

process_commands([Command | Tail], Planes, Lanes, Tower) ->
    {NewPlanes, NewLanes} = process_commands([Command], Planes, Lanes, Tower),
    process_commands(Tail, NewPlanes, NewLanes, Tower).


repaint(Planes, Lanes) ->
    PlaneStrings = lists:map(fun(PlaneWrapper) ->
        {_, Plane} = PlaneWrapper,
        FlightNumber = Plane#plane.flight_number,
        FlightNumber end,
        Planes
    ),
    PlaneStateStrings = lists:map(fun(PlaneWrapper) ->
        {Pid, _} = PlaneWrapper,
        plane:get_state(Pid) end,
        Planes
    ),
    LaneStrings = lists:map(fun(Lane) ->
        {_, Id, _} = Lane,
        integer_to_list(Id) end,
        Lanes
    ),
    PlaneColumn = ["Planes"] ++ PlaneStrings,
    PlaneStateColumn = ["Plane state"] ++ PlaneStateStrings,
    LaneColumn = ["All Lanes"] ++ LaneStrings,
    
    clear_console(),
    print_table({[PlaneColumn, PlaneStateColumn, LaneColumn], 20}).

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
        {"Close landing strip", "cls"},
        {"Add plane", "adp"},
        {"Remove plane", "rmp"}
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