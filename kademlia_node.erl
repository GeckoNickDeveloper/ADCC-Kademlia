-module(kademlia_node).

-behaviour(gen_server).

%% Callbacks of gen_server
-export([
    init/1, 
    handle_call/3, 
    handle_cast/2, 
    handle_info/2, 
    terminate/2,    
    code_change/3
]).

%% =================================================================== %%
%%                              Callbacks                              %%
%% =================================================================== %%

%% Initialize the server
init([ID]) ->
    %% State contains: ID, routing table, local storage and bootstrap flag

    RoutingTableRef = ets:new(routingtable, [set, private]),
    KademliaDataRef = ets: new(kademliadata, [set, private]),
    %% Determine if this node is the bootstrap node
    case is_first_node() of
        true ->
            {ok, {ID, RoutingTableRef, KademliaDataRef, true}};
        false ->
            {ok, {ID, RoutingTableRef, KademliaDataRef, false}}
    end
.

%% Checks if the current node is the first node (bootstrap)
is_first_node() ->
    case process_info(self(), dictionary) of
        {_, [{_, []}]} -> true;  % If no nodes exist, this is the first node
        _ -> false  % Otherwise, there are other nodes, not the first one
    end
.

%% =================================================================== %%
%%                              Handle call                            %%
%% =================================================================== %%

handle_call(ping, _From, State) ->
    %% Respond to PING
    {reply, pong, State};

handle_call({find_node, ID}, _From, State = {routingtable = RoutingTableRef}) ->
    %% Find closest nodes to TargetId
    ClosestNodes = get_closest_nodes(ID, RoutingTableRef),
    {reply, ClosestNodes, State};

handle_call({find_value, Key}, _From, {Id, RoutingTable, KademliaData, IsBootstrap}) ->
    %% Check if the Key exists in local storage
    case dict:find(Key, KademliaData) of
        {ok, Value} -> {reply, {ok, Value}, {Id, RoutingTable, KademliaData, IsBootstrap}};
        error -> {reply, not_found, {Id, RoutingTable, KademliaData, IsBootstrap}}
    end;

handle_call(is_bootstrap_node, _From, {ID, RoutingTable, KademliaData, IsBootstrap}) ->
    %% Return whether the node is the bootstrap node
    {reply, IsBootstrap, {ID, RoutingTable, KademliaData, IsBootstrap}};

handle_call(_Request, _From, State) ->
    %% Default case: Unknown request
    {reply, error, State}
.



%% =================================================================== %%
%%                              Handle cast                            %%
%% =================================================================== %%

handle_cast({become_bootstrap}, {ID, RoutingTable, KademliaData, _IsBootstrap}) ->
    %% This node becomes the bootstrap node
    {noreply, {ID, RoutingTable, KademliaData, true}};

%% Handle asynchronous casts
handle_cast(_Msg, State) ->
    {noreply, State}
.

%% Handle unexpected messages
handle_info(_Info, State) ->
    {noreply, State}
.

%% Cleanup on termination
terminate(_Reason, _State) ->
    ok
.

%% Handle hot code updates
code_change(_OldVsn, State, _Extra) ->
    {ok, State}
.

%% =================================================================== %%
%%                              Utils Function                         %%
%% =================================================================== %%


%% Finds the closest nodes to a target ID using XOR distance
get_closest_nodes(TargetID, RoutingTable) ->
    %% Sort nodes by XOR distance to TargetId and return the closest ones
    SortedNodes = lists:sort(
        fun({ID1, _ }, {ID2, _ }) -> 
            compare_distance(ID1,ID2, TargetID) 
        end, RoutingTable),
    lists:sublist(SortedNodes, 5)
.

%% Compares XOR distance between two nodes and a target ID
compare_distance(ID1, ID2, TargetID) ->
    Distance1 = xor_distance(ID1, TargetID),
    Distance2 = xor_distance(ID2, TargetID),
    case Distance1 < Distance2 of
        true -> -1;  % If Distance1 is smaller, return -1 (first is smaller)
        false -> 
            case Distance1 > Distance2 of
                true -> 1;  % If Distance1 is larger, return 1 (first is larger)
                false -> 0   % If both distances are equal, return 0
            end
    end
.

%% Computes XOR distance between two IDs
xor_distance(Id1, Id2) ->
    Id1 bxor Id2
.

