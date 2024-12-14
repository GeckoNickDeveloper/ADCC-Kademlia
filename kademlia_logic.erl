-module(kademlia_logic).

%% API
-export([

    % Interfaces 
    start_link/1,

    % Interfaces
    join/2,
    ping/1,
    store/2,
    find_node/1,
    find_value/1

]).


%% =================================================================== %%
%%                              API Functions                          %%
%% =================================================================== %%

%% Starts a node with a given ID and Pid
start_link(ID) ->
    gen_server:start_link({local, ID}, ?MODULE, ID, [])
.

%% =================================================================== %%
%%                              Call                                   %%
%% =================================================================== %%

ping(ID) ->
   %% Send PING request to NodeId
    gen_server:call(ID, ping)
.

find_node(ID) ->
    %% Finds a node closest to a given ID
    gen_server:call(self(), {find_node, ID})
.

find_value(Key) ->
    %% Finds a value associated with a Key
    gen_server:call(self(), {find_value, Key})
.

%% Joins the network via a known node or becomes the bootstrap node
join(SelfPid, KnownNodePid) ->
    %% Ask the known node if it is the bootstrap node
    case gen_server:call(KnownNodePid, is_bootstrap_node) of
        true ->
            %% This is the bootstrap node, no need to connect to another node
            gen_server:cast(SelfPid, {become_bootstrap}),
            {ok, "Node became the bootstrap node"};

        false ->
            %% This is not the bootstrap node, ask for closest nodes from an existing node
            ClosestNodes = gen_server:call(KnownNodePid, {find_node, SelfPid}),
            %% Update the routing table with the closest nodes
            gen_server:cast(SelfPid, {update_routing, ClosestNodes}),
            {ok, "Node successfully joined the network"}
    end.

%% =================================================================== %%
%%                              Cast                                   %%
%% =================================================================== %%


store(Key, Value) ->
    %% Stores a Key-Value pair
    gen_server:cast(self(), {store, Key, Value})
.

%% Stop the server
stop() ->
    gen_server:call(?MODULE, stop)
.





