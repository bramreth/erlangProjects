-module(bw308Assessment4).
-export([likeServer/1, numOfLikes/2, isPost/2, database/2, likePost/2, client/2, run/0, intermediateDatabase/2, buffer/1, postClient/4]).

%supplied methods for handling likes and confirming they are posts.
likeServer(DB) ->
    receive
        {like, Post, Client} -> 
            DB!{like, Post, self()},
            receive {dataReply, Data} ->
                case isPost(Data, Post) of
                  true -> L = numOfLikes(Data, Post),
                          Client!{likes, L+1},
                          likeServer(DB);
                  false -> Client!{nopost},
                           likeServer(DB)
                end
            end
    end.

numOfLikes([], _Post)                      -> 0;
numOfLikes([{Post, Likes} | _Posts], Post) -> Likes;
numOfLikes([_ | Posts], Post)              -> numOfLikes(Posts, Post).

isPost([], _Post)                  -> false;
isPost([{Post, _} | _Posts], Post) -> true;
isPost([_ | Posts], Post)          -> isPost(Posts, Post).

%the database is unlocked if it is availiable to have a new post be made
database(Data, unlocked) ->
    receive
        {like, Post, Server} ->
            Server!{dataReply, Data},
            Data2 = likePost(Data,Post),
            io:fwrite("DB: ~w~n", [Data2]),
            database(Data2, unlocked);
        %if a user requests to make post the database is locked, for them to make a post with mutual exclusivity
    	{tryPost, User}->
    		User!{unlocked},
    		database(Data,locked)
    end;
%the locked version of database, which does not respond to attmepts to make posts by other users
database(Data, locked) ->
    receive
        {like, Post, Server} ->
            Server!{dataReply, Data},
            Data2 = likePost(Data,Post),
            io:fwrite("DB: ~w~n", [Data2]),
            database(Data2, locked);
        %if a user makes a post add a new post to the existing data
		{makePost}->
			Data2 = Data++[{length(Data), 0}],
			io:fwrite("New post: {~w, 0}~n", [length(Data)]),
			database(Data2,unlocked)
    end.
%an intermediate database to respond to users that like posts.
intermediateDatabase(Buffer, DataCopy)->
	receive
		{like, Post, Server} ->
			Buffer!{like, Post, self()},
			%sends the user a response with the latest version of the data
            Server!{dataReply, DataCopy},
			intermediateDatabase(Buffer, DataCopy);
		%when it recieves an update from the database, update the local data copy
        {dataReply, Data} ->
			intermediateDatabase(Buffer, Data);
		%pass along tryPost requests and makePost requests.
		{tryPost, User}->
    		Buffer!{tryPost, User},
    		intermediateDatabase(Buffer, DataCopy);
    	{makePost}->
    		Buffer!{makePost},
    		intermediateDatabase(Buffer, DataCopy)
	end.

%forwards requests to the database at most twice per second.
buffer(Database)->
	timer:sleep(500),
	receive
		{like, Post, Server} ->
			Database!{like, Post, Server},
			buffer(Database);
		{tryPost, User}->
    		Database!{tryPost, User},
    		buffer(Database);
		{makePost}->
    		Database!{makePost},
    		buffer(Database)
	end.

likePost([], _Post)                     -> [];
likePost([{Post, Likes} | Posts], Post) -> [{Post, Likes+1} | Posts];
likePost([P | Posts], Post)             -> [P | likePost(Posts, Post)].

%the client process that likes random existing posts
client(Username, Server) ->	
	random:seed(now()),
	PostNum = random:uniform(6)-1,
	timer:sleep(random:uniform(5000)),
	Server!{like,PostNum,self()},
	receive
		{likes, L} ->
			io:fwrite("~p, ~p likes on post ~p~n",[Username,L, PostNum]);
		{nopost} ->
			io:fwrite("~p,post: ~p is not a post~n",[Username, PostNum])
	end,
	client(Username,Server).

%this client is able to create new posts as well as like them
postClient(Username, Server, DB, Choice) when Choice > 9 ->	
	random:seed(now()),
	DB!{tryPost, self()},
	receive
		%if the postClient recieves the database is availiable, send the database a message to make a new post.
		{unlocked}->
			DB!{makePost};
		{locked}->
			timer:sleep(random:uniform(10)),
			postClient(Username,Server, DB, 10)
	end,
	postClient(Username,Server, DB, random:uniform(10));
%the version that will like posts rather than make them
postClient(Username, Server, DB, _) ->	
	random:seed(now()),
	PostNum = random:uniform(6)-1,
	timer:sleep(random:uniform(5000)),
	Server!{like,PostNum,self()},
	receive
		{likes, L} ->
			io:fwrite("~p, ~p likes on post ~p~n",[Username,L, PostNum]);
		{nopost} ->
			io:fwrite("~p,post: ~p is not a post~n",[Username, PostNum])
	end,
	postClient(Username,Server, DB, random:uniform(10)).

%this method runs the processes spawning the databases and clients
run() ->
	Database = spawn(?MODULE, database, [[{0,3},{1,3},{2,1},{3,0},{4,0},{5,1}], unlocked]),
	Buffer = spawn(?MODULE, buffer, [Database]),
	IntermediateDatabase = spawn(?MODULE, intermediateDatabase, [Buffer, [{0,3},{1,3},{2,1},{3,0},{4,0},{5,1}]]),
	LikeServer = spawn(?MODULE, likeServer, [IntermediateDatabase]),
	spawn(?MODULE, client, [arthur,LikeServer]),
	spawn(?MODULE, client, [zaphod,LikeServer]),
	spawn(?MODULE, client, [trillian,LikeServer]),
	spawn(?MODULE, client, [ford,LikeServer]),
	spawn(?MODULE, client, [marvin,LikeServer]),
	spawn(?MODULE, postClient, [postman_pat,LikeServer, IntermediateDatabase,1]).
%3. my approach to task 3 involved setting up an intermediate database
%and a buffer. The intermediate database pushes likes to the buffer and
%holds the latest copy of the data to respond to the like server 
%immediately. The buffer then pushes these updates to the database at 
%most twice per second.This results in the database getting updated and 
%the users getting an immediate response to their like; however the 
%information about how many likes there are can be up to half a second
%out of date.
