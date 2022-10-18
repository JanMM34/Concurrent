-module(client).
-export([handle/2, initial_state/3]).

% This record defines the structure of the state of a client.
% Add whatever other fields you need.
-record(client_st, {
    gui, % atom of the GUI process
    nick, % nick/username of the client
    server, % atom of the chat server
    channels %list of channels the user joined

}).

% Return an initial state record. This is called from GUI.
% Do not change the signature of this function.
initial_state(Nick, GUIAtom, ServerAtom) ->
    #client_st{
        gui = GUIAtom,
        nick = Nick,
        server = ServerAtom,
        channels = []
    }.


% handle/2 handles each kind of request from GUI
% Parameters:
%   - the current state of the client (St)
%   - request data from GUI
% Must return a tuple {reply, Data, NewState}, where:
%   - Data is what is sent to GUI, either the atom `ok` or a tuple {error, Atom, "Error message"}
%   - NewState is the updated state of the client


% Join channel
handle(St, {join, Channel}) ->

    %Send request to the server to join a channel
    case (catch genserver:request(St#client_st.server, {join, self(), St#client_st.nick, Channel})) of
        ok ->
            %We add the new channel to the users channel list
            NewChannelsList = [Channel | St#client_st.channels],
            {reply, ok, St#client_st{channels=NewChannelsList}};
        
        %catch errors
        timeout_error ->
            {reply, {error,server_not_reached,"Server not reached"}, St};
        {'EXIT', _} ->
            {reply, {error,server_not_reached,"Server not reached"}, St};
        Error -> {reply, Error,St}

        end;



% Leave channel
handle(St, {leave, Channel}) ->

    %Send request to a Channel to leave
    case (catch genserver:request(list_to_atom(Channel), {leave, self()})) of
        ok ->
            %We remove the channel from the list
            {reply, ok, St#client_st{channels=lists:delete(Channel, St#client_st.channels)}};  

        %catch errors
        timeout_error ->
            {reply, {error,server_not_reached,"Server not reached"}, St};
        {'EXIT', _} ->
            {reply, {error,server_not_reached,"Server not reached"}, St};
        Error -> {reply, Error,St}

        
    end;   

    

% Sending message (from GUI, to channel)
handle(St, {message_send, Channel, Msg}) ->

    %We send a request to the server to see if channel exists
    case (catch genserver:request(St#client_st.server, {channel_exist, Channel})) of
        ok ->
            %If the channel exists we check if user has joined it
            case lists:member(Channel, St#client_st.channels) of

                %if user hasn't joined send atom user_not_joined
                false ->
                    {reply, {error, user_not_joined, "You are not in the channel "++Channel}, St};
                %if user is in the channel send request to the Channel process to send message
                true ->
                    Response = genserver:request(list_to_atom(Channel),{message_send, self(), St#client_st.nick,Msg}),
                    {reply, Response, St}
            end;
        
        %If ther is an error it's beacause user hasn't joined the channel or server is down
        Error ->
            %If server is down we can still try to send request to the Channel process
            case (catch genserver:request(list_to_atom(Channel),{message_send, self(), St#client_st.nick,Msg})) of
                ok ->
                    {reply, ok, St};
                
                %catch errors
                Error ->
                    {reply, {error,server_not_reached,"Server not reached"},St};
                timeout_error ->
                    {reply, {error,server_not_reached,"Server not reached"}, St};
                {'EXIT', _} ->
                    {reply, {error,server_not_reached,"Server not reached"}, St}

                end

        end;

    



    

% This case is only relevant for the distinction assignment!
% Change nick (no check, local only)
handle(St, {nick, NewNick}) ->
    %request the server to change nick
    case (catch genserver:request(St#client_st.server, {nick,St#client_st.nick, NewNick})) of
        ok ->
            {reply, ok, St#client_st{nick = NewNick}} ;
        nick_taken ->
            {reply,nick_taken,St};
        Error ->
            {reply, Error, St}
        end;

    

% ---------------------------------------------------------------------------
% The cases below do not need to be changed...
% But you should understand how they work!

% Get current nick
handle(St, whoami) ->
    {reply, St#client_st.nick, St} ;

% Incoming message (from channel, to GUI)
handle(St = #client_st{gui = GUI}, {message_receive, Channel, Nick, Msg}) ->
    gen_server:call(GUI, {message_receive, Channel, Nick++"> "++Msg}),
    {reply, ok, St} ;

% Quit client via GUI
handle(St, quit) ->
    % Any cleanup should happen here, but this is optional
    {reply, ok, St} ;

% Catch-all for any unhandled requests
handle(St, _) ->
    {reply, {error, not_implemented, "Client does not handle this command"}, St} .




