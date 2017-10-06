-module(client).
-export([handle/2, initial_state/3]).

% This record defines the structure of the state of a client.
% Add whatever other fields you need.
-record(client_st, {
    gui, % atom of the GUI process
    nick, % nick/username of the client
    server, % atom of the chat server
    channels % Channels that the client is a member of
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
    Serv = St#client_st.server,
    Nickname = St#client_st.nick,
    Data = {join,self(),Nickname, Channel},
    
    %% In some tests Serv ceased to be an Atom and was a PiD, thus needed this here.
    if is_atom(Serv) == true ->
        % Check to see if server atom is registered, if not means server is unavailable
        Registered = whereis(Serv),
        
        if Registered == undefined ->
            Reply = server_not_reached;
        true ->
            Reply = genserver:request(Serv, Data)
        end;
        
    true ->
        Reply = genserver:request(Serv, Data)
    end,
    case Reply of
    
        ok ->  %if you get an ok reply then add the new channel to the list of channels
            Cha = [Channel|St#client_st.channels],
            NewSt = St#client_st{channels = Cha} ,		
             Response = ok;
        user_already_joined ->
            %if it wants to join a channel which it already joined then send error
            Response = {error,user_already_joined,"user_already_joined"},
            NewSt = St;
        server_not_reached ->
            Response = {error,server_not_reached,"Server unavailable on send msg."},
            NewSt = St;
        _->  %If anything other than recieved then it shows an error which is not thought of so send error
            NewSt = St,
            Response = error
        end, 

    {reply, Response, NewSt} ;

% Leave channel
handle(St, {leave, Channel}) ->
    % TODO: Implement this function
    Data ={leave,self(),Channel},
    Server =St#client_st.server,
    X = genserver:request(Server, Data),

    case X of 

        ok -> 
            %if it recieves okay then delete the chanel from the list of channels 
            Cha = lists:delete(Channel , St#client_st.channels),
            NewSt = St#client_st{channels = Cha},
            Response = ok;
        user_not_joined -> 
            %if user hasn't joined then reply error
            NewSt = St,
            Response = {error,user_not_joined,"user_not_joined"};
        _ -> 
            %If anything other than recieved then it shows an error which is not thought of so send error
            NewSt = St,
            Response = error
    end,
                            
    {reply, Response, NewSt};

% Sending message (from GUI, to channel)
handle(St, {message_send, Channel, Msg}) ->
    Pid = self(),
    Ismemb = lists:member(Channel, St#client_st.channels),
    
    if Ismemb =:= true ->
        try spawn(fun() -> genserver:request(list_to_atom(Channel), {send_message,Channel, St#client_st.nick, Msg, Pid}) end) of
             _ -> 
                {reply, ok, St}
        catch
            _ -> {reply, {error, server_not_reached, "Channel unavailible on send msg."}, St}
        end;
    true ->
        {reply,{error,user_not_joined,"user_not_joined"}, St}
end;

% ---------------------------------------------------------------------------
% The cases below do not need to be changed...
% But you should understand how they work!

% Get current nick
handle(St, whoami) ->
    {reply, St#client_st.nick, St} ;

% Change nick / Check for duplicate is implemented
handle(St, {nick, NewNick}) ->
    Server =St#client_st.server,
    OldNick = St#client_st.nick,
    Data ={nick, OldNick, NewNick},
    X = genserver:request(Server, Data),
    if X =:= ok ->
	NewST = St#client_st{nick = NewNick},
        {reply, ok, NewST} ;
    true ->
        {reply, {error, nick_taken, "nick_is_taken"}, St}
    end;

% Incoming message (from channel, to GUI)
handle(St = #client_st{gui = GUI}, {message_receive, Channel, Nick, Msg}) ->
    gen_server:call(GUI, {message_receive, Channel, Nick++"> "++Msg}),
    {reply, ok, St} ;

% Quit client via GUI
handle(St, quit) ->
    % Any cleanup should happen here, but this is optional
    {reply, ok, St} ;

% Catch-all for any unhandled requests
handle(St, Data) ->
    {reply, {error, not_implemented, "Client does not handle this command"}, St} .
