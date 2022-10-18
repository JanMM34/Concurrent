-module(server).
-export([start/1,stop/1]).

-record(server_st, {
    nicks,  
    channels
}).

init_server_st() ->
    #server_st{
        nicks = [],
        channels = []
    }.

% Start a new server process with the given name
% Do not change the signature of this function.
start(ServerAtom) ->
    genserver:start(ServerAtom,init_server_st(),fun handle/2).
      

    
    % TODO Implement function
    % - Spawn a new process which waits for a message, handles it, then loops infinitely
    % - Register this process to ServerAtom
    % - Return the process ID


% Stop the server process registered to the given name,
% together with any other associated processes
stop(ServerAtom) ->
    % TODO Implement function
    % Return ok
    genserver:request(ServerAtom, stop_channels),
    genserver:stop(ServerAtom).

%Reply ok if Channel exists and atom channel_no_exist otherwise.
handle(St,{channel_exist, Channel})->
    case lists:member(Channel, St#server_st.channels) of
        true  -> {reply, ok, St};
        false -> {reply, channel_no_exist, St}
    end;
    
    

%function that handles if user joins a Channel    
handle(St, {join, ClientPID, ClientNick, Channel})->
    %if user first join, we add the nick to the nick list
    NewNickList =
    case lists:member(ClientNick, St#server_st.nicks) of
        true  -> St#server_st.nicks;
        false -> [ClientNick | St#server_st.nicks]
    end,
    %if channel doesn't exist we create it and spwan a process for it
    case lists:member(Channel, St#server_st.channels) of
        true  -> UpdatedChannels = St#server_st.channels;
        false -> channel:start(Channel), UpdatedChannels = [Channel | St#server_st.channels]
    end,
    %send request to the channel to join
    Response = genserver:request(list_to_atom(Channel), {join, ClientPID}),
    {reply, Response, St#server_st{nicks=NewNickList,channels = UpdatedChannels}};


%Function that stops each process of channels
handle(St, stop_channels) ->
    lists:foreach((fun(Ch) -> genserver:stop(list_to_atom(Ch)) end), St#server_st.channels),
    {reply, ok, St};


%function that handles the nick change
handle(St, {nick, ClientNick, NewNick}) ->
    case lists:member(NewNick, St#server_st.nicks) of
        %if nick is the same reply ok
        true when ClientNick =:= NewNick ->
            {reply, ok, St};
        %if nick in the list of nicks and not the same reply error
        true ->
            {reply, {error, nick_taken, "Nick "++NewNick++" has been taken!"}, St};
        %if nick not tacken reply ok and update list
        false ->
            UpdatedNickList = [NewNick | lists:delete(ClientNick   , St#server_st.nicks)],
            {reply, ok, St#server_st{nicks=UpdatedNickList}}
        end;


handle(St, _)-> {reply,{error, not_implemented, "Server cannot handle this request!", St}}.