-module(channel).
-export([start/1,stop/1]).

-record(channel_st, {
    name,   % name of the channel
    members % list of members' pids
}).

initial_channel_st(Name) ->
    #channel_st{
        name = Name,
        members = []
    }.


start(Name) ->
    genserver:start(list_to_atom(Name), initial_channel_st(Name), fun handle/2).


stop(Name) ->
    genserver:stop(list_to_atom(Name)).


handle(St, {join, ClientPid}) ->
    case lists:member(ClientPid, St#channel_st.members) of
        true ->
            %If ClientPid already joine send atom user_already_joined
            {reply, {error, user_already_joined, "Already joined "++St#channel_st.name}, St};
        false ->
            % Add new member and reply ok
            UpdatedMembers = [ClientPid | St#channel_st.members],
            {reply, ok, St#channel_st{members=UpdatedMembers}}
        end;
    

% Client request to leave
handle(St, {leave, ClientPid}) ->
    case lists:member(ClientPid, St#channel_st.members) of
        false ->
            % Client hasn't joined
            {reply, {error, user_not_joined, "You are not in the channel  "++St#channel_st.name}, St};
        true ->
            %Delete member and reply ok
            UpdatedMembers = lists:delete(ClientPid, St#channel_st.members),
            {reply, ok, St#channel_st{members=UpdatedMembers}}
    end;

% Client request to send message 
handle(St, {message_send, ClientPid, ClientNick, Msg}) ->
    %OtherMembers contain all clientPids except the sender
    OtherMembers = lists:delete(ClientPid, St#channel_st.members),
    Data = {request, self(), make_ref(), {message_receive, St#channel_st.name, ClientNick, Msg}},
    %Send message to every member
    lists:foreach((fun(Member) -> Member ! Data end), OtherMembers),
    {reply, ok, St};

% Catch-all for any unhandled requests
handle(St, _) ->
    {reply, {error, not_implemented, "Channel cannot handle this request!"}, St} .


