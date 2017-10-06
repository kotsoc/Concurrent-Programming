-module(server).
-export([start/1,stop/1,handle/2,listen/2,initial_state_channel/2]).

% This record defines the structure of the state of a server.
-record(server_st, {
  server,
  uselist, % list of users
  channelL  % Channel list
}).

% This record defines the structure of the channel.
-record(channel_st, {
  channel,
  users % list of users
  }).

%initial state
initial_state() ->
    #server_st{uselist = [], channelL = []}.

initial_state_channel(Channel, JoinedUser) ->
    #channel_st{channel = Channel, users = [JoinedUser]}.



% Start a new server process with the given name
% Do not change the signature of this function.
start(ServerAtom) ->
    % - Spawn a new process which waits for a message, handles it, then loops infinitely
	Pid = genserver:start(ServerAtom, initial_state(), fun server:listen/2),	
    % - Register this process to ServerAtom
	%register(server, Pid),
	%initial_state(ServerAtom),
    % - Return the process ID
	Pid.

% Stop the server process registered to the given name,
% together with any other associated processes
stop(ServerAtom) ->
    % TODO Implement function
    % Return ok
	genserver:stop(ServerAtom),
	ok.


%% Join channel
listen(St, {join,Pid, Channel}) ->
	Exist = lists:member(Channel, St#server_st.channelL),
	if Exist =:= false ->
		Cha = [Channel|St#server_st.channelL],
		NewSt = St#server_st{channelL = Cha},
		genserver:start(list_to_atom(Channel), initial_state_channel(Channel, Pid),fun handle/2),
		Resp = ok;
	true ->
		NewSt = St,
		Resp = genserver:request(list_to_atom(Channel),{add, Pid})
	end,
{reply, Resp, NewSt};

%% Leave channel
listen(St, {leave,Pid, Channel}) ->
	ChatRoomsList = lists:member(Channel,St#server_st.channelL),
	if ChatRoomsList =:= true ->	
		Response = genserver:request(list_to_atom(Channel),{leave, Pid});
    true ->    
		Response = {error,user_not_joined,"user_not_joined"}
    end,	 
{reply, Response, St}.


handle(St, {leave, Pid}) ->
	CheckPid = lists:member(Pid,St#channel_st.users),
		if CheckPid =:= false ->
			   	Responses =user_not_joined,
			   	NewStatus = St;
	       true ->
				Newclients = lists:delete(Pid, St#channel_st.users),
		      NewStatus = St#channel_st {users = Newclients},
				Responses = ok
		end,
		
	{reply, Responses, NewStatus};

%% Add channel
handle(St, {add, Pid}) ->
%check if the pid is already connected to this channel if
%true then reply user_already_connected message if not just add it to the list
    ChatR = lists:member(Pid,St#channel_st.users),
	if ChatR =:= true ->
		   	Response = user_already_joined,
		   	NewSt = St;
       true ->
			New = [Pid | St#channel_st.users],
			NewSt = St#channel_st {users = New},
			Response = ok
	end,
	{reply, Response, NewSt};

%%% Send a Msg
handle(St, {send_message,Channel, Nick, Msg, Pid}) ->
%%	% Check to see if pid is a member of the channel, he is trying to send to.
%%	ChanMemb = lists:member(Pid,St#channel_st.users),
%%	if ChanMemb == false ->
%%		Response = {error, user_not_joined};	
%%	true ->		
	
	%First remove the sender pid from the list of recieving lists, since we don't want to recieve the message we sent
    	%Then create a process by calling the genserver with Channel,Nick and message as an arguemnt 
		ClientsIntheChannel = lists:delete(Pid, St#channel_st.users), 
		[spawn(fun() -> genserver:request(SendingList, {message_receive,Channel, Nick, Msg}) end) || SendingList <- ClientsIntheChannel],
		Response = ok,
	{reply, Response, St}.


 

	 
