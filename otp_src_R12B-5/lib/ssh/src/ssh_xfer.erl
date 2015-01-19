%%<copyright>
%% <year>2005-2007</year>
%% <holder>Ericsson AB, All Rights Reserved</holder>
%%</copyright>
%%<legalnotice>
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% The Initial Developer of the Original Code is Ericsson AB.
%%</legalnotice>
%%

%%% Description: SFTP functions

-module(ssh_xfer).

-export([attach/2, connect/3]).
-export([open/6, opendir/3, readdir/3, close/3, read/5, write/5,
	 rename/5, remove/3, mkdir/4, rmdir/3, realpath/3, extended/4,
	 stat/4, fstat/4, lstat/4, setstat/4,
	 readlink/3, fsetstat/4, symlink/4,
	 xf_reply/2,
	 xf_send_reply/3, xf_send_names/3, xf_send_name/4,
	 xf_send_status/3, xf_send_status/4, xf_send_status/5,
	 xf_send_handle/3, xf_send_attr/3, xf_send_data/3,
	 encode_erlang_status/1,
	 decode_open_flags/2, encode_open_flags/1,
	 decode_ATTR/2, encode_ATTR/2]).

-include("ssh.hrl").
-include("ssh_xfer.hrl").

-import(lists, [foldl/3, reverse/1]).

-define(is_set(F, Bits),
	((F) band (Bits)) == (F)).

-define(XFER_PACKET_SIZE, 32768).
-define(XFER_WINDOW_SIZE, 4*?XFER_PACKET_SIZE).
-define(DEFAULT_TIMEOUT, 5000).

attach(CM, Opts) ->
    case ssh:attach(CM, connect_timeout(Opts)) of
	{ok,CMPid} ->  open_xfer(CMPid, Opts);
	Error ->  Error
    end.

connect(Host, Port, Opts) ->
    case ssh:connect(Host, Port, Opts) of
	{ok, CM} -> open_xfer(CM, Opts);
	Error -> Error
    end.

%% xfer_channel(XF, Channel) ->
%%     XF#ssh_xfer{channel = Channel}.

open_xfer(CM, Opts) ->
    TMO = connect_timeout(Opts),
    case ssh_connection:session_channel(CM, ?XFER_WINDOW_SIZE, ?XFER_PACKET_SIZE, TMO) of
	{ok, Channel} ->
	    case ssh_connection:subsystem(CM, Channel, "sftp", TMO) of
		success ->
		    case init(CM, Channel) of
			{ok, {Vsn,Ext}, Rest} ->
			    ?dbg(true, "open_xfer: Vsn=~p Ext=~p\n", [Vsn,Ext]),
			    {ok, #ssh_xfer{vsn = Vsn,
					   ext = Ext,
					   cm  = CM,
					   channel = Channel}, Rest};
			Error -> Error
		    end;
		Error -> Error
	    end;
	Error -> Error
    end.


init(CM, Channel) ->
    XF = #ssh_xfer { cm = CM, channel = Channel},
    xf_request(XF, ?SSH_FXP_INIT, <<?UINT32(?SSH_SFTP_PROTOCOL_VERSION)>>),
    case reply(CM, Channel) of
	{ok, <<?SSH_FXP_VERSION, ?UINT32(Version), Ext/binary>>, Rest} ->
	    ?dbg(true, "init: Version=~p\n", [Version]),
	    {ok, {Version, decode_ext(Ext)}, Rest};
	Error ->
	    Error
    end.

reply(CM,Channel) ->
    reply(CM,Channel,<<>>).

reply(CM,Channel,RBuf) ->
    receive
	{ssh_cm, CM, {data, Channel, 0, Data}} ->
	    case <<RBuf/binary, Data/binary>> of
		<<?UINT32(Len),Reply:Len/binary,Rest/binary>> ->
		    {ok, Reply, Rest};
		RBuf2 ->
		    reply(CM,Channel,RBuf2)
	    end;
	{ssh_cm, CM, {data, Channel, _, Data}} ->
	    error_logger:format("ssh: STDERR: ~s\n", [binary_to_list(Data)]),
	    reply(CM,Channel,RBuf);
	{ssh_cm, CM, {exit_signal,Channel,_SIG,Err,_Lang}} ->
	    ssh_connection:close(CM, Channel),
	    {error, Err};
	{ssh_cm, CM, {exit_status,Channel,_Status}} ->
	    ssh_connection:close(CM, Channel),
	    eof;
	{ssh_cm, CM, {eof, Channel}} ->
	    eof;
	{ssh_cm, CM, {closed, Channel}} ->
	    {error, closed};
	{ssh_cm, CM, Msg} ->
	    error_logger:format("GOT: ssh_cm ~p\n", [Msg]);
	Msg ->
	    error_logger:format("GOT: ~p\n", [Msg])
    end.


open(XF, ReqID, FileName, Access, Flags, Attrs) -> 
    Vsn = XF#ssh_xfer.vsn,
    FileName1 = list_to_binary(FileName),
    MBits = if Vsn >= 5 -> 
		    M = encode_ace_mask(Access),
		    ?uint32(M);
	       true ->
		    (<<>>)
	    end,
    F = encode_open_flags(Flags),
    xf_request(XF,?SSH_FXP_OPEN, 
	       [?uint32(ReqID),
		?binary(FileName1),
		MBits,
		?uint32(F),
		encode_ATTR(Vsn,Attrs)]).    
    
opendir(XF, ReqID, DirName) ->
    xf_request(XF, ?SSH_FXP_OPENDIR, 
	       [?uint32(ReqID),
		?string(DirName)]).


close(XF, ReqID, Handle) ->
    xf_request(XF, ?SSH_FXP_CLOSE,
	       [?uint32(ReqID),
		?binary(Handle)]).

read(XF, ReqID, Handle, Offset, Length) ->
    xf_request(XF, ?SSH_FXP_READ,
	       [?uint32(ReqID),
		?binary(Handle),
		?uint64(Offset),
		?uint32(Length)]).

readdir(XF, ReqID, Handle) ->
    xf_request(XF, ?SSH_FXP_READDIR,
	       [?uint32(ReqID),
		?binary(Handle)]).    

write(XF,ReqID, Handle, Offset, Data) ->
    Data1 = if binary(Data) -> Data;
	       list(Data) -> list_to_binary(Data)
	    end,
    xf_request(XF,?SSH_FXP_WRITE,
	       [?uint32(ReqID),
		?binary(Handle),
		?uint64(Offset),
		?binary(Data1)]).

%% Remove a file
remove(XF, ReqID, File) ->
    xf_request(XF, ?SSH_FXP_REMOVE, 
	       [?uint32(ReqID),
		?string(File)]).

%% Rename a file/directory
rename(XF, ReqID, Old, New, Flags) ->
    Vsn = XF#ssh_xfer.vsn,
    OldPath = list_to_binary(Old),
    NewPath = list_to_binary(New),
    FlagBits
	= if Vsn >= 5 ->
		  F0 = encode_rename_flags(Flags),
		  ?uint32(F0);
	     true ->
		  (<<>>)
	  end,
    xf_request(XF, ?SSH_FXP_RENAME, 
	       [?uint32(ReqID),
		?binary(OldPath),
		?binary(NewPath),
		FlagBits]).



%% Create directory
mkdir(XF, ReqID, Path, Attrs) ->
    Path1 = list_to_binary(Path),
    xf_request(XF, ?SSH_FXP_MKDIR, 
	       [?uint32(ReqID),
		?binary(Path1),
		encode_ATTR(XF#ssh_xfer.vsn, Attrs)]).

%% Remove a directory
rmdir(XF, ReqID, Dir) ->
    Dir1 = list_to_binary(Dir),
    xf_request(XF, ?SSH_FXP_RMDIR,
	       [?uint32(ReqID),
		?binary(Dir1)]).

%% Stat file
stat(XF, ReqID, Path, Flags) ->
    Path1 = list_to_binary(Path),
    Vsn = XF#ssh_xfer.vsn,
    AttrFlags = if Vsn >= 5 ->
			F = encode_attr_flags(Vsn, Flags),
			?uint32(F);
		   true ->
			[]
		end,
    xf_request(XF, ?SSH_FXP_STAT, 
	       [?uint32(ReqID),
		?binary(Path1),
		AttrFlags]).


%% Stat file - follow symbolic links
lstat(XF, ReqID, Path, Flags) ->
    Path1 = list_to_binary(Path),
    Vsn = XF#ssh_xfer.vsn,
    AttrFlags = if Vsn >= 5 ->
			F = encode_attr_flags(Vsn, Flags),
			?uint32(F);
		   true ->
			[]
		end,
    xf_request(XF, ?SSH_FXP_LSTAT, 
	     [?uint32(ReqID),
	      ?binary(Path1),
	      AttrFlags]).

%% Stat open file
fstat(XF, ReqID, Handle, Flags) ->
    Vsn = XF#ssh_xfer.vsn,
    AttrFlags = if Vsn >= 5 ->
			F = encode_attr_flags(Vsn, Flags),
			?uint32(F);
		   true ->
			[]
		end,
    xf_request(XF, ?SSH_FXP_FSTAT, 
	       [?uint32(ReqID),
		?binary(Handle),
		AttrFlags]).

%% Modify file attributes
setstat(XF, ReqID, Path, Attrs) ->
    Path1 = list_to_binary(Path),
    xf_request(XF, ?SSH_FXP_SETSTAT, 
	       [?uint32(ReqID),
		?binary(Path1),
		encode_ATTR(XF#ssh_xfer.vsn, Attrs)]).


%% Modify file attributes
fsetstat(XF, ReqID, Handle, Attrs) ->
    xf_request(XF, ?SSH_FXP_FSETSTAT, 
	       [?uint32(ReqID),
		?binary(Handle),
		encode_ATTR(XF#ssh_xfer.vsn, Attrs)]).
    
%% Read a symbolic link
readlink(XF, ReqID, Path) ->
    Path1 = list_to_binary(Path),
    xf_request(XF, ?SSH_FXP_READLINK, 
	       [?uint32(ReqID),
		?binary(Path1)]).


%% Create a symbolic link    
symlink(XF, ReqID, LinkPath, TargetPath) ->
    LinkPath1 = list_to_binary(LinkPath),
    TargetPath1 = list_to_binary(TargetPath),
    xf_request(XF, ?SSH_FXP_SYMLINK, 
	       [?uint32(ReqID),
		?binary(LinkPath1),
		?binary(TargetPath1)]).

%% Convert a path into a 'canonical' form
realpath(XF, ReqID, Path) ->
    Path1 = list_to_binary(Path),
    xf_request(XF, ?SSH_FXP_REALPATH,     
	       [?uint32(ReqID),
		?binary(Path1)]).

extended(XF, ReqID, Request, Data) ->
    xf_request(XF, ?SSH_FXP_EXTENDED,
	       [?uint32(ReqID),
		?string(Request),
		?binary(Data)]).


%% Send xfer request to connection manager
xf_request(XF, Op, Arg) ->
    CM = XF#ssh_xfer.cm,
    Channel = XF#ssh_xfer.channel,
    Data = if binary(Arg) -> Arg;
	      list(Arg) -> list_to_binary(Arg)
	   end,
    Size = 1+size(Data),
    ssh_connection:send(CM, Channel, <<?UINT32(Size), Op, Data/binary>>).

xf_send_reply(#ssh_xfer{cm = CM, channel = Channel}, Op, Arg) ->    
    Data = if binary(Arg) -> Arg;
	      list(Arg) -> list_to_binary(Arg)
	   end,
    Size = 1 + size(Data),
    ssh_connection:send(CM, Channel, <<?UINT32(Size), Op, Data/binary>>).

xf_send_name(XF, ReqId, Name, Attr) ->
    xf_send_names(XF, ReqId, [{Name, Attr}]).
					    
%%     ?dbg(true, "xf_send_name: ReqId=~p Name=~p\n", [ReqId, Name]),
    
%%     Count
%%     NameLen = length(Name),
%%     EncAttr = encode_ATTR(Vsn, Attr),
%%     Size = 1 + 4 + 4 + 4 + NameLen + bsize(EncAttr),% op reqid count namelen name attr
%%     ToSend = [<<?UINT32(Size),
%% 	       ?SSH_FXP_NAME, ?UINT32(ReqId), ?UINT32(1), ?UINT32(NameLen)>>,
%% 	      Name, EncAttr],
%%     ssh_connection:send(CM, Channel, ToSend).

xf_send_handle(#ssh_xfer{cm = CM, channel = Channel},
	       ReqId, Handle) ->
    HLen = length(Handle),
    Size = 1 + 4 + 4+HLen,
    ToSend = [<<?UINT32(Size), ?SSH_FXP_HANDLE, ?UINT32(ReqId), ?UINT32(HLen)>>,
	      Handle],
    ssh_connection:send(CM, Channel, ToSend).

xf_send_names(#ssh_xfer{cm = CM, channel = Channel, vsn = Vsn},
	      ReqId, NamesAndAttrs) ->
    Count = length(NamesAndAttrs),
    {Data, Len} = encode_names(Vsn, NamesAndAttrs),
    Size = 1 + 4 + 4 + Len,
    ToSend = [<<?UINT32(Size), ?SSH_FXP_NAME, ?UINT32(ReqId), ?UINT32(Count)>>,
	      Data],
    ?dbg(true, "xf_send_names: Size=~p size(ToSend)=~p\n",
	 [Size, size(list_to_binary(ToSend))]),
    ssh_connection:send(CM, Channel, ToSend).

xf_send_status(XF, ReqId, ErrorCode) ->
    xf_send_status(XF, ReqId, ErrorCode, "").

xf_send_status(XF, ReqId, ErrorCode, ErrorMsg) ->
    xf_send_status(XF, ReqId, ErrorCode, ErrorMsg, <<>>).

xf_send_status(#ssh_xfer{cm = CM, channel = Channel},
	       ReqId, ErrorCode, ErrorMsg, Data) ->
    LangTag = "en",
    ELen = length(ErrorMsg),
    TLen = 2, %% length(LangTag),
    Size = 1 + 4 + 4 + 4+ELen + 4+TLen + size(Data),
    ToSend = [<<?UINT32(Size), ?SSH_FXP_STATUS, ?UINT32(ReqId),
	       ?UINT32(ErrorCode)>>,
	      <<?UINT32(ELen)>>, ErrorMsg,
	      <<?UINT32(TLen)>>, LangTag,
	      Data],
    ssh_connection:send(CM, Channel, ToSend).

xf_send_attr(#ssh_xfer{cm = CM, channel = Channel, vsn = Vsn}, ReqId, Attr) ->
    EncAttr = encode_ATTR(Vsn, Attr),
    ALen = size(EncAttr),
    Size = 1 + 4 + ALen,
    ToSend = [<<?UINT32(Size), ?SSH_FXP_ATTRS, ?UINT32(ReqId)>>, EncAttr],
    ssh_connection:send(CM, Channel, ToSend).

xf_send_data(#ssh_xfer{cm = CM, channel = Channel}, ReqId, Data) ->
    DLen = size(Data),
    Size = 1 + 4 + 4+DLen,
    ToSend = [<<?UINT32(Size), ?SSH_FXP_DATA, ?UINT32(ReqId), ?UINT32(DLen)>>,
	      Data],
    ssh_connection:send(CM, Channel, ToSend).    


%% xf_reply_server(XF, <<?SSH_FXP_INIT, ?UINT32(V)>>) ->
%%     Vers = XF#ssh_xfer.vsn,
%%     Ver = if Vers > V -> V;
%% 	     true -> Vers
%% 	  end,
%%     xf_send_reply(XF, ?SSH_FXP_VERSION, <<?UINT32(Ver)>>),
%%     ok;
%% xf_reply_server(_XF, <<?SSH_FXP_VERSION>> ) ->
%%     ok.

xf_reply(_XF, << ?SSH_FXP_STATUS, ?UINT32(ReqID), ?UINT32(Status), 
	      ?UINT32(ELen), Err:ELen/binary,
	      ?UINT32(LLen), Lang:LLen/binary,
	      Reply/binary >> ) ->
    Stat = decode_status(Status),
    {status, ReqID, {Stat,binary_to_list(Err),binary_to_list(Lang),
		     Reply}};
xf_reply(_XF, << ?SSH_FXP_STATUS, ?UINT32(ReqID), ?UINT32(Status)>> ) ->
    Stat = decode_status(Status),
    {status, ReqID, {Stat,"","",<<>>}};
xf_reply(_XF, <<?SSH_FXP_HANDLE, ?UINT32(ReqID),
	      ?UINT32(HLen), Handle:HLen/binary>>) ->
    {handle, ReqID, Handle};
xf_reply(_XF, <<?SSH_FXP_DATA, ?UINT32(ReqID),
	      ?UINT32(DLen), Data:DLen/binary>>) ->
    {data, ReqID, Data};
xf_reply(XF, <<?SSH_FXP_NAME, ?UINT32(ReqID),
	      ?UINT32(Count), AData/binary>>) ->
    ?dbg(true, "xf_reply ?SSH_FXP_NAME: AData=~p\n", [AData]),
    {name, ReqID, decode_names(XF#ssh_xfer.vsn, Count, AData)};
xf_reply(XF, <<?SSH_FXP_ATTRS, ?UINT32(ReqID),
	      AData/binary>>) ->
    {A, _} = decode_ATTR(XF#ssh_xfer.vsn, AData),
    {attrs, ReqID, A};
xf_reply(_XF, <<?SSH_FXP_EXTENDED_REPLY, ?UINT32(ReqID),
	      RData>>) ->
    {extended_reply, ReqID, RData}.



decode_status(Status) ->
    case Status of
	?SSH_FX_OK -> ok;
	?SSH_FX_EOF -> eof;
	?SSH_FX_NO_SUCH_FILE -> no_such_file;
	?SSH_FX_PERMISSION_DENIED -> permission_denied;
	?SSH_FX_FAILURE -> failure;
	?SSH_FX_BAD_MESSAGE -> bad_message;
	?SSH_FX_NO_CONNECTION -> no_connection;
	?SSH_FX_CONNECTION_LOST -> connection_lost;
	?SSH_FX_OP_UNSUPPORTED -> op_unsupported;
	?SSH_FX_INVALID_HANDLE -> invalid_handle;
	?SSH_FX_NO_SUCH_PATH -> no_such_path;
	?SSH_FX_FILE_ALREADY_EXISTS -> file_already_exists;
	?SSH_FX_WRITE_PROTECT -> write_protect;
	?SSH_FX_NO_MEDIA -> no_media;
	?SSH_FX_NO_SPACE_ON_FILESYSTEM -> no_space_on_filesystem;
	?SSH_FX_QUOTA_EXCEEDED -> quota_exceeded;
	?SSH_FX_UNKNOWN_PRINCIPLE -> unknown_principle;
	?SSH_FX_LOCK_CONFlICT -> lock_conflict;
	?SSH_FX_NOT_A_DIRECTORY -> not_a_directory;
	_ -> {error,Status}
    end.

encode_erlang_status(Status) ->
    case Status of
	ok -> ?SSH_FX_OK;
	eof -> ?SSH_FX_EOF;
	enoent -> ?SSH_FX_NO_SUCH_FILE;
	eacces -> ?SSH_FX_PERMISSION_DENIED;
	_ -> ?SSH_FX_FAILURE
    end.
%% 	?SSH_FX_FAILURE -> failure;
%% 	?SSH_FX_BAD_MESSAGE -> bad_message;
%% 	?SSH_FX_NO_CONNECTION -> no_connection;
%% 	?SSH_FX_CONNECTION_LOST -> connection_lost;
%% 	?SSH_FX_OP_UNSUPPORTED -> op_unsupported;
%% 	?SSH_FX_INVALID_HANDLE -> invalid_handle;
%% 	?SSH_FX_NO_SUCH_PATH -> no_such_path;
%% 	?SSH_FX_FILE_ALREADY_EXISTS -> file_already_exists;
%% 	?SSH_FX_WRITE_PROTECT -> write_protect;
%% 	?SSH_FX_NO_MEDIA -> no_media;
%% 	?SSH_FX_NO_SPACE_ON_FILESYSTEM -> no_space_on_filesystem;
%% 	?SSH_FX_QUOTA_EXCEEDED -> quota_exceeded;
%% 	?SSH_FX_UNKNOWN_PRINCIPLE -> unknown_principle;
%% 	?SSH_FX_LOCK_CONFlICT -> lock_conflict
%%     end.

decode_ext(<<?UINT32(NameLen), Name:NameLen/binary,
	    ?UINT32(DataLen), Data:DataLen/binary,
	    Tail/binary>>) ->
    [{binary_to_list(Name), binary_to_list(Data)}
     | decode_ext(Tail)];
decode_ext(<<>>) ->
    [].

%%
%% Encode rename flags
%%
encode_rename_flags(Flags) ->
    encode_bits(
      fun(overwrite) -> ?SSH_FXP_RENAME_OVERWRITE;
	 (atomic) -> ?SSH_FXP_RENAME_ATOMIC;
	 (native) -> ?SSH_FXP_RENAME_NATIVE
      end, Flags).

%% decode_rename_flags(F) ->
%%     decode_bits(F,
%% 		[{?SSH_FXP_RENAME_OVERWRITE, overwrite},
%% 		 {?SSH_FXP_RENAME_ATOMIC, atomic},
%% 		 {?SSH_FXP_RENAME_NATIVE, native}]).
    

encode_open_flags(Flags) ->
    encode_bits(
      fun (read) -> ?SSH_FXF_READ;
	  (write) -> ?SSH_FXF_WRITE;
	  (append) -> ?SSH_FXF_APPEND;
	  (creat) -> ?SSH_FXF_CREAT;
	  (trunc)  -> ?SSH_FXF_TRUNC;
	  (excl)   -> ?SSH_FXF_EXCL;
	  (create_new) -> ?SSH_FXF_CREATE_NEW;
	  (create_truncate) -> ?SSH_FXF_CREATE_TRUNCATE;
	  (open_existing) -> ?SSH_FXF_OPEN_EXISTING;
	  (open_or_create) -> ?SSH_FXF_OPEN_OR_CREATE;
	  (truncate_existing) -> ?SSH_FXF_TRUNCATE_EXISTING;
	  (append_data) -> ?SSH_FXF_ACCESS_APPEND_DATA;
	  (append_data_atomic) -> ?SSH_FXF_ACCESS_APPEND_DATA_ATOMIC;
	  (text_mode) -> ?SSH_FXF_ACCESS_TEXT_MODE;
	  (read_lock) -> ?SSH_FXF_ACCESS_READ_LOCK;
	  (write_lock) -> ?SSH_FXF_ACCESS_WRITE_LOCK;
	  (delete_lock) -> ?SSH_FXF_ACCESS_DELETE_LOCK
      end, Flags).

encode_ace_mask(Access) ->
    encode_bits(
      fun(read_data) -> ?ACE4_READ_DATA;
	 (list_directory) -> ?ACE4_LIST_DIRECTORY;
	 (write_data) -> ?ACE4_WRITE_DATA;
	 (add_file) -> ?ACE4_ADD_FILE;
	 (append_data) -> ?ACE4_APPEND_DATA;
	 (add_subdirectory) -> ?ACE4_ADD_SUBDIRECTORY;
	 (read_named_attrs) -> ?ACE4_READ_NAMED_ATTRS;
	 (write_named_attrs) -> ?ACE4_WRITE_NAMED_ATTRS;
	 (execute) -> ?ACE4_EXECUTE;
	 (delete_child) -> ?ACE4_DELETE_CHILD;
	 (read_attributes) -> ?ACE4_READ_ATTRIBUTES;
	 (write_attributes) -> ?ACE4_WRITE_ATTRIBUTES;
	 (delete) -> ?ACE4_DELETE;
	 (read_acl) -> ?ACE4_READ_ACL;
	 (write_acl) -> ?ACE4_WRITE_ACL;
	 (write_owner) -> ?ACE4_WRITE_OWNER;
	 (synchronize) -> ?ACE4_SYNCHRONIZE
      end, Access).

decode_ace_mask(F) ->
    decode_bits(F,
		[
		 {?ACE4_READ_DATA, read_data},
		 {?ACE4_LIST_DIRECTORY, list_directory},
		 {?ACE4_WRITE_DATA, write_data},
		 {?ACE4_ADD_FILE, add_file},
		 {?ACE4_APPEND_DATA, append_data},
		 {?ACE4_ADD_SUBDIRECTORY, add_subdirectory},
		 {?ACE4_READ_NAMED_ATTRS, read_named_attrs},
		 {?ACE4_WRITE_NAMED_ATTRS, write_named_attrs},
		 {?ACE4_EXECUTE, execute},
		 {?ACE4_DELETE_CHILD, delete_child},
		 {?ACE4_READ_ATTRIBUTES, read_attributes},
		 {?ACE4_WRITE_ATTRIBUTES, write_attributes},
		 {?ACE4_DELETE, delete},
		 {?ACE4_READ_ACL, read_acl},
		 {?ACE4_WRITE_ACL, write_acl},
		 {?ACE4_WRITE_OWNER, write_owner},
		 {?ACE4_SYNCHRONIZE, synchronize}
		]).

decode_open_flags(Vsn, F) when Vsn =< 3 ->
    decode_bits(F,
		[
		 {?SSH_FXF_READ, read},
		 {?SSH_FXF_WRITE, write},
		 {?SSH_FXF_APPEND, append},
		 {?SSH_FXF_CREAT, creat},
		 {?SSH_FXF_TRUNC, trunc},
		 {?SSH_FXF_EXCL, excl}
		 ]);
decode_open_flags(Vsn, F) when Vsn >= 4 ->
    R = decode_bits(F,
		    [
		     {?SSH_FXF_ACCESS_APPEND_DATA, append_data},
		     {?SSH_FXF_ACCESS_APPEND_DATA_ATOMIC, append_data_atomic},
		     {?SSH_FXF_ACCESS_TEXT_MODE, text_mode},
		     {?SSH_FXF_ACCESS_READ_LOCK, read_lock},
		     {?SSH_FXF_ACCESS_WRITE_LOCK, write_lock},
		     {?SSH_FXF_ACCESS_DELETE_LOCK, delete_lock}
		    ]),
    AD = case F band ?SSH_FXF_ACCESS_DISPOSITION of
	     ?SSH_FXF_CREATE_NEW -> create_new;
	     ?SSH_FXF_CREATE_TRUNCATE -> create_truncate;
	     ?SSH_FXF_OPEN_EXISTING -> open_existing;
	     ?SSH_FXF_OPEN_OR_CREATE -> open_or_create;
	     ?SSH_FXF_TRUNCATE_EXISTING -> truncate_existing
	 end,
    [AD | R].    

encode_ace_type(Type) ->
    case Type of
	access_allowed -> ?ACE4_ACCESS_ALLOWED_ACE_TYPE;
	access_denied  -> ?ACE4_ACCESS_DENIED_ACE_TYPE;
	system_audit   -> ?ACE4_SYSTEM_AUDIT_ACE_TYPE;
	system_alarm   -> ?ACE4_SYSTEM_ALARM_ACE_TYPE
    end.

decode_ace_type(F) ->
    case F of
	?ACE4_ACCESS_ALLOWED_ACE_TYPE -> access_allowed;
	?ACE4_ACCESS_DENIED_ACE_TYPE -> access_denied;
	?ACE4_SYSTEM_AUDIT_ACE_TYPE -> system_audit;
	?ACE4_SYSTEM_ALARM_ACE_TYPE -> system_alarm
    end.

encode_ace_flag(Flag) ->
    encode_bits(
      fun(file_inherit) -> ?ACE4_FILE_INHERIT_ACE;
	 (directory_inherit) -> ?ACE4_DIRECTORY_INHERIT_ACE;
	 (no_propagte_inherit) -> ?ACE4_NO_PROPAGATE_INHERIT_ACE;
	 (inherit_only) -> ?ACE4_INHERIT_ONLY_ACE;
	 (successful_access) -> ?ACE4_SUCCESSFUL_ACCESS_ACE_FLAG;
	 (failed_access) -> ?ACE4_FAILED_ACCESS_ACE_FLAG;
	 (identifier_group) -> ?ACE4_IDENTIFIER_GROUP
      end, Flag).

decode_ace_flag(F) ->
    decode_bits(F,
		[
		 {?ACE4_FILE_INHERIT_ACE, file_inherit},
		 {?ACE4_DIRECTORY_INHERIT_ACE, directory_inherit},
		 {?ACE4_NO_PROPAGATE_INHERIT_ACE, no_propagte_inherit},
		 {?ACE4_INHERIT_ONLY_ACE, inherit_only},
		 {?ACE4_SUCCESSFUL_ACCESS_ACE_FLAG, successful_access},
		 {?ACE4_FAILED_ACCESS_ACE_FLAG, failed_access},
		 {?ACE4_IDENTIFIER_GROUP, identifier_group}
		]).

encode_attr_flags(Vsn, all) ->
    encode_attr_flags(Vsn,
		      [size, uidgid, permissions,
		       acmodtime, accesstime, createtime,
		       modifytime, acl, ownergroup, subsecond_times,
		       bits, extended]);
encode_attr_flags(Vsn, Flags) ->
    encode_bits(
      fun(size) -> ?SSH_FILEXFER_ATTR_SIZE;
	 (uidgid) when Vsn =<3 -> ?SSH_FILEXFER_ATTR_UIDGID;
	 (permissions) -> ?SSH_FILEXFER_ATTR_PERMISSIONS;
	 (acmodtime) when Vsn =< 3 -> ?SSH_FILEXFER_ATTR_ACMODTIME;
	 (accesstime) when Vsn >= 5 -> ?SSH_FILEXFER_ATTR_ACCESSTIME;
	 (createtime) when Vsn >= 5 -> ?SSH_FILEXFER_ATTR_CREATETIME;
	 (modifytime) when Vsn >= 5 -> ?SSH_FILEXFER_ATTR_MODIFYTIME;
	 (acl) when Vsn >= 5 -> ?SSH_FILEXFER_ATTR_ACL;
	 (ownergroup) when  Vsn >= 5 -> ?SSH_FILEXFER_ATTR_OWNERGROUP;
	 (subsecond_times) when Vsn >= 5 -> ?SSH_FILEXFER_ATTR_SUBSECOND_TIMES;
	 (bits) when Vsn >= 5 -> ?SSH_FILEXFER_ATTR_BITS;
	 (extended) when Vsn >= 5 -> ?SSH_FILEXFER_ATTR_EXTENDED;
	 (_) -> 0
      end, Flags).

encode_file_type(Type) ->
    ?dbg(true, "encode_file_type(~p)\n", [Type]),
    case Type of
	regular -> ?SSH_FILEXFER_TYPE_REGULAR;
	directory -> ?SSH_FILEXFER_TYPE_DIRECTORY;
	symlink -> ?SSH_FILEXFER_TYPE_SYMLINK;
	special -> ?SSH_FILEXFER_TYPE_SPECIAL;
	unknown -> ?SSH_FILEXFER_TYPE_UNKNOWN;
	other -> ?SSH_FILEXFER_TYPE_UNKNOWN;
	socket -> ?SSH_FILEXFER_TYPE_SOCKET;
	char_device -> ?SSH_FILEXFER_TYPE_CHAR_DEVICE;
	block_device -> ?SSH_FILEXFER_TYPE_BLOCK_DEVICE;
	fifo -> ?SSH_FILEXFER_TYPE_FIFO;
	undefined -> ?SSH_FILEXFER_TYPE_UNKNOWN
    end.

decode_file_type(Type) ->
    case Type of
	?SSH_FILEXFER_TYPE_REGULAR -> regular;
	?SSH_FILEXFER_TYPE_DIRECTORY -> directory;
	?SSH_FILEXFER_TYPE_SYMLINK -> symlink;
	?SSH_FILEXFER_TYPE_SPECIAL -> special;
	?SSH_FILEXFER_TYPE_UNKNOWN -> other; % unknown
	?SSH_FILEXFER_TYPE_SOCKET -> socket;
	?SSH_FILEXFER_TYPE_CHAR_DEVICE -> char_device;
	?SSH_FILEXFER_TYPE_BLOCK_DEVICE -> block_device;
	?SSH_FILEXFER_TYPE_FIFO -> fifo
    end.

encode_attrib_bits(Bits) ->
    encode_bits(
      fun(readonly) -> ?SSH_FILEXFER_ATTR_FLAGS_READONLY;
	 (system) -> ?SSH_FILEXFER_ATTR_FLAGS_SYSTEM;
	 (hidden) -> ?SSH_FILEXFER_ATTR_FLAGS_HIDDEN;
	 (case_insensitive) -> ?SSH_FILEXFER_ATTR_FLAGS_CASE_INSENSITIVE;
	 (arcive) -> ?SSH_FILEXFER_ATTR_FLAGS_ARCHIVE;
	 (encrypted) -> ?SSH_FILEXFER_ATTR_FLAGS_ENCRYPTED;
	 (compressed) -> ?SSH_FILEXFER_ATTR_FLAGS_COMPRESSED;
	 (sparse) -> ?SSH_FILEXFER_ATTR_FLAGS_SPARSE;
	 (append_only) -> ?SSH_FILEXFER_ATTR_FLAGS_APPEND_ONLY;
	 (immutable) -> ?SSH_FILEXFER_ATTR_FLAGS_IMMUTABLE;
	 (sync) -> ?SSH_FILEXFER_ATTR_FLAGS_SYNC
      end, Bits).

decode_attrib_bits(F) ->
    decode_bits(F,
		[{?SSH_FILEXFER_ATTR_FLAGS_READONLY, readonly},
		 {?SSH_FILEXFER_ATTR_FLAGS_SYSTEM, system},
		 {?SSH_FILEXFER_ATTR_FLAGS_HIDDEN, hidden},
		 {?SSH_FILEXFER_ATTR_FLAGS_CASE_INSENSITIVE, case_insensitive},
		 {?SSH_FILEXFER_ATTR_FLAGS_ARCHIVE, arcive},
		 {?SSH_FILEXFER_ATTR_FLAGS_ENCRYPTED, encrypted},
		 {?SSH_FILEXFER_ATTR_FLAGS_COMPRESSED, compressed},
		 {?SSH_FILEXFER_ATTR_FLAGS_SPARSE, sparse},
		 {?SSH_FILEXFER_ATTR_FLAGS_APPEND_ONLY, append_only},
		 {?SSH_FILEXFER_ATTR_FLAGS_IMMUTABLE, immutable},
		 {?SSH_FILEXFER_ATTR_FLAGS_SYNC, sync}]).


%% 
%% Encode file attributes
%% 
encode_ATTR(Vsn, A) ->
    {Flags,As} =
	encode_As(Vsn, 
		  [{size, A#ssh_xfer_attr.size},
		   {ownergroup, A#ssh_xfer_attr.owner},
		   {ownergroup, A#ssh_xfer_attr.group},
		   {permissions, A#ssh_xfer_attr.permissions},
		   {acmodtime, A#ssh_xfer_attr.atime},
		   {acmodtime, A#ssh_xfer_attr.mtime},
		   {accesstime,  A#ssh_xfer_attr.atime},
		   {subsecond_times, A#ssh_xfer_attr.atime_nseconds},
		   {createtime,  A#ssh_xfer_attr.createtime},
		   {subsecond_times, A#ssh_xfer_attr.createtime_nseconds},
		   {modifytime,  A#ssh_xfer_attr.mtime},
		   {subsecond_times, A#ssh_xfer_attr.mtime_nseconds},
		   {acl, A#ssh_xfer_attr.acl},
		   {bits, A#ssh_xfer_attr.attrib_bits},
		   {extended, A#ssh_xfer_attr.extensions}],
		  0, []),
    Type = encode_file_type(A#ssh_xfer_attr.type),
    ?dbg(true, "encode_ATTR: Vsn=~p A=~p As=~p Flags=~p Type=~p",
    	 [Vsn, A, As, Flags, Type]),
    Result = list_to_binary([?uint32(Flags),
			     if Vsn >= 5 ->
				     ?byte(Type);
				true ->
				     (<<>>)
			     end, As]),
    %% ?dbg(true, " Result=~p\n", [Result]),
    Result.


encode_As(Vsn, [{_AName, undefined}|As], Flags, Acc) ->
    encode_As(Vsn, As, Flags, Acc);
encode_As(Vsn, [{AName, X}|As], Flags, Acc) ->
    case AName of
	size ->
	    encode_As(Vsn, As,Flags bor ?SSH_FILEXFER_ATTR_SIZE,
		      [?uint64(X) | Acc]);
	ownergroup when Vsn=<4 ->
	     encode_As(Vsn, As,Flags bor ?SSH_FILEXFER_ATTR_UIDGID,
		       [?uint32(X) | Acc]);
	ownergroup when Vsn>=5 ->
	    X1 = list_to_binary(integer_to_list(X)), % TODO: check owner and group
	    encode_As(Vsn, As,Flags bor ?SSH_FILEXFER_ATTR_OWNERGROUP,
		      [?binary(X1) | Acc]);
	permissions ->
	    encode_As(Vsn, As,Flags bor ?SSH_FILEXFER_ATTR_PERMISSIONS,
		      [?uint32(X) | Acc]);
	acmodtime when Vsn=<3 ->
	    encode_As(Vsn, As,Flags bor ?SSH_FILEXFER_ATTR_ACMODTIME,
		      [?uint32(X) | Acc]);
	accesstime when Vsn>=5 ->
	    encode_As(Vsn, As, Flags bor ?SSH_FILEXFER_ATTR_ACCESSTIME,
		      [?uint64(X) | Acc]);
	createtime when Vsn>=5->
	    encode_As(Vsn, As, Flags bor ?SSH_FILEXFER_ATTR_CREATETIME,
		      [?uint64(X) | Acc]);
	modifytime when Vsn>=5 ->
	    encode_As(Vsn, As, Flags bor ?SSH_FILEXFER_ATTR_MODIFYTIME,
		      [?uint64(X) | Acc]);
	subsecond_times when Vsn>=5 ->
	    encode_As(Vsn, As, Flags bor ?SSH_FILEXFER_ATTR_SUBSECOND_TIMES,
		      [?uint64(X) | Acc]);
	acl when Vsn >=5 ->
	    encode_As(Vsn, As, Flags bor ?SSH_FILEXFER_ATTR_ACL,
		      [encode_acl(X) | Acc]);
	bits when Vsn>=5 ->
	    F = encode_attrib_bits(X),
	    encode_As(Vsn, As, Flags bor ?SSH_FILEXFER_ATTR_BITS,
		      [?uint32(F) | Acc]);
	extended ->
	    encode_As(Vsn, As, Flags bor ?SSH_FILEXFER_ATTR_EXTENDED,
		      [encode_extensions(X) | Acc]);
	_ ->
	    encode_As(Vsn, As, Flags, Acc)
    end;
encode_As(_Vsn, [], Flags, Acc) ->
    {Flags, reverse(Acc)}.


decode_ATTR(Vsn, <<?UINT32(Flags), Tail/binary>>) ->
    ?dbg(true, "decode_ATTR: Vsn=~p Flags=~p Tail=~p\n", [Vsn, Flags, Tail]),
    {Type,Tail2} =
	if Vsn =< 3 ->
		{?SSH_FILEXFER_TYPE_UNKNOWN, Tail};
	   Vsn >= 5 ->
		<<?BYTE(T), TL/binary>> = Tail,
		{T, TL}
	end,
    decode_As(Vsn, 
	      [{size, #ssh_xfer_attr.size},
	       {ownergroup, #ssh_xfer_attr.owner},
	       {ownergroup, #ssh_xfer_attr.group},
	       {permissions, #ssh_xfer_attr.permissions},
	       {acmodtime, #ssh_xfer_attr.atime},
	       {acmodtime, #ssh_xfer_attr.mtime},
	       {accesstime,  #ssh_xfer_attr.atime},
	       {subsecond_times, #ssh_xfer_attr.atime_nseconds},
	       {createtime,  #ssh_xfer_attr.createtime},
	       {subsecond_times, #ssh_xfer_attr.createtime_nseconds},
	       {modifytime,  #ssh_xfer_attr.mtime},
	       {subsecond_times, #ssh_xfer_attr.mtime_nseconds},
	       {acl, #ssh_xfer_attr.acl},
	       {bits, #ssh_xfer_attr.attrib_bits},
	       {extended, #ssh_xfer_attr.extensions}],
	      #ssh_xfer_attr { type = decode_file_type(Type) },
	      Flags,
	      Tail2).

decode_As(Vsn, [{AName, AField}|As], R, Flags, Tail) ->
    ?dbg(false, "decode_As: Vsn=~p AName=~p AField=~p Flags=~p Tail=~p\n", [Vsn, AName, AField, Flags, Tail]),
    case AName of
	size when ?is_set(?SSH_FILEXFER_ATTR_SIZE, Flags) ->
	    <<?UINT64(X), Tail2/binary>> = Tail,
	    decode_As(Vsn, As, setelement(AField, R, X), Flags, Tail2);
	ownergroup when ?is_set(?SSH_FILEXFER_ATTR_UIDGID, Flags),Vsn=<3 ->
	    <<?UINT32(X), Tail2/binary>> = Tail,
	    decode_As(Vsn, As, setelement(AField, R, X), Flags, Tail2);
	ownergroup when ?is_set(?SSH_FILEXFER_ATTR_OWNERGROUP, Flags),Vsn>=5 ->
	    <<?UINT32(Len), Bin:Len/binary, Tail2/binary>> = Tail,
	    X = binary_to_list(Bin),
	    ?dbg(true, "ownergroup X=~p\n", [X]),
	    decode_As(Vsn, As, setelement(AField, R, X), Flags, Tail2);

	permissions when ?is_set(?SSH_FILEXFER_ATTR_PERMISSIONS,Flags),Vsn>=5->
	    <<?UINT32(X), Tail2/binary>> = Tail,
	    decode_As(Vsn, As, setelement(AField, R, X), Flags, Tail2);

	permissions when ?is_set(?SSH_FILEXFER_ATTR_PERMISSIONS,Flags),Vsn=<3->
	    <<?UINT32(X), Tail2/binary>> = Tail,
	    R1 = setelement(AField, R, X),
	    Type = case X band ?S_IFMT of
		       ?S_IFDIR -> directory;
		       ?S_IFCHR -> char_device;
		       ?S_IFBLK -> block_device;
		       ?S_IFIFO -> fifi;
		       ?S_IFREG -> regular;
		       ?S_IFSOCK -> socket;
		       ?S_IFLNK -> symlink;
		       _ -> unknown
		   end,
	    decode_As(Vsn, As, R1#ssh_xfer_attr { type=Type}, Flags, Tail2);

	acmodtime when ?is_set(?SSH_FILEXFER_ATTR_ACMODTIME,Flags),Vsn=<3 ->
	    <<?UINT32(X), Tail2/binary>> = Tail,
	    decode_As(Vsn, As, setelement(AField, R, X), Flags, Tail2);
	accesstime when ?is_set(?SSH_FILEXFER_ATTR_ACCESSTIME,Flags),Vsn>=5 ->
	    <<?UINT64(X), Tail2/binary>> = Tail,
	    decode_As(Vsn, As, setelement(AField, R, X), Flags, Tail2);
	modifytime when ?is_set(?SSH_FILEXFER_ATTR_MODIFYTIME,Flags),Vsn>=5 ->
	    <<?UINT64(X), Tail2/binary>> = Tail,
	    decode_As(Vsn, As, setelement(AField, R, X), Flags, Tail2);
	createtime when ?is_set(?SSH_FILEXFER_ATTR_CREATETIME,Flags),Vsn>=5 ->
	    <<?UINT64(X), Tail2/binary>> = Tail,
	    decode_As(Vsn, As, setelement(AField, R, X), Flags, Tail2);
	subsecond_times when ?is_set(?SSH_FILEXFER_ATTR_SUBSECOND_TIMES,Flags),Vsn>=5 ->
	    <<?UINT32(X), Tail2/binary>> = Tail,
	    decode_As(Vsn, As, setelement(AField, R, X), Flags, Tail2);
	acl when ?is_set(?SSH_FILEXFER_ATTR_ACL, Flags), Vsn>=5 ->
	    {X,Tail2} = decode_acl(Tail),
	    decode_As(Vsn, As, setelement(AField, R, X), Flags, Tail2);
	bits when ?is_set(?SSH_FILEXFER_ATTR_BITS, Flags), Vsn >=5 ->
	    <<?UINT32(Y), Tail2/binary>> = Tail,
	    X = decode_attrib_bits(Y),
	    decode_As(Vsn, As, setelement(AField, R, X), Flags, Tail2);
	extended when ?is_set(?SSH_FILEXFER_ATTR_EXTENDED, Flags) ->
	    {X,Tail2} = decode_extended(Tail),
	    decode_As(Vsn, As, setelement(AField, R, X), Flags, Tail2);
	_ ->
	    decode_As(Vsn, As, R, Flags, Tail)
    end;
decode_As(_Vsn, [], R, _, Tail) ->
    {R, Tail}.


	

decode_names(_Vsn, 0, _Data) ->
    [];
decode_names(Vsn, I, <<?UINT32(Len), FileName:Len/binary, 
		      ?UINT32(LLen), _LongName:LLen/binary,
		      Tail/binary>>) when Vsn =< 3 ->
    Name = binary_to_list(FileName),
    ?dbg(true, "decode_names: ~p\n", [Name]),
    {A, Tail2} = decode_ATTR(Vsn, Tail),
    [{Name, A} | decode_names(Vsn, I-1, Tail2)];
decode_names(Vsn, I, <<?UINT32(Len), FileName:Len/binary, 
		      Tail/binary>>) when Vsn >= 4 ->
    Name = binary_to_list(FileName),
    ?dbg(true, "decode_names: ~p\n", [Name]),
    {A, Tail2} = decode_ATTR(Vsn, Tail),
    [{Name, A} | decode_names(Vsn, I-1, Tail2)].

encode_names(Vsn, NamesAndAttrs) ->
    lists:mapfoldl(fun(N, L) -> encode_name(Vsn, N, L) end, 0, NamesAndAttrs).

encode_name(Vsn, {Name,Attr}, Len) when Vsn =< 3 ->
    NLen = length(Name),
    %%?dbg(true, "encode_name: Vsn=~p Name=~p Attr=~p\n",
    %% 	 [Vsn, Name, Attr]),
    EncAttr = encode_ATTR(Vsn, Attr),
    ALen = size(EncAttr),
    NewLen = Len + NLen*2 + 4 + 4 + ALen,
    {[<<?UINT32(NLen)>>, Name, <<?UINT32(NLen)>>, Name, EncAttr], NewLen};
encode_name(Vsn, {Name,Attr}, Len) when Vsn >= 4 ->
    NLen = length(Name),
    EncAttr = encode_ATTR(Vsn, Attr),
    ALen = size(EncAttr),
    {[<<?UINT32(NLen)>>, Name, EncAttr],
     Len + 4 + NLen + ALen}.

encode_acl(ACLList) ->
    Count = length(ACLList),
    [?uint32(Count) | encode_acl_items(ACLList)].

encode_acl_items([ACE|As]) ->
    Type = encode_ace_type(ACE#ssh_xfer_ace.type),
    Flag = encode_ace_flag(ACE#ssh_xfer_ace.flag), 
    Mask = encode_ace_mask(ACE#ssh_xfer_ace.mask), 
    Who = list_to_binary(ACE#ssh_xfer_ace.who),
    [?uint32(Type), ?uint32(Flag), ?uint32(Mask), 
     ?binary(Who) | encode_acl_items(As)];
encode_acl_items([]) ->
    [].


decode_acl(<<?UINT32(Count), Tail/binary>>) ->
    decode_acl_items(Count, Tail, []).

decode_acl_items(0, Tail, Acc) -> 
    {reverse(Acc), Tail};
decode_acl_items(I, <<?UINT32(Type), 
	       ?UINT32(Flag),
	       ?UINT32(Mask),
	       ?UINT32(WLen), BWho:WLen/binary,
	       Tail/binary>>, Acc) ->
    decode_acl_items(I-1, Tail,
		     [#ssh_xfer_ace { type = decode_ace_type(Type),
				      flag = decode_ace_flag(Flag),
				      mask = decode_ace_mask(Mask),
				      who = binary_to_list(BWho)} | Acc]).

encode_extensions(Exts) ->
    Count = length(Exts),
    [?uint32(Count) | encode_ext(Exts)].

encode_ext([{Type, Data} | Exts]) ->
    [?string(Type), ?string(Data) | encode_ext(Exts)];
encode_ext([]) ->
    [].


decode_extended(<<?UINT32(Count), Tail/binary>>) ->     
    decode_ext(Count, Tail, []).

decode_ext(0, Tail, Acc) ->
    {reverse(Acc), Tail};
decode_ext(I, <<?UINT32(TLen), Type:TLen/binary,
	       ?UINT32(DLen), Data:DLen/binary,
	       Tail/binary>>,  Acc) ->
    decode_ext(I-1, Tail, [{binary_to_list(Type), Data}|Acc]).



%% Encode bit encoded flags
encode_bits(Fun, BitNames) ->
    encode_bits(Fun, 0, BitNames).

encode_bits(Fun, F, [Bit|BitNames]) ->
    encode_bits(Fun, Fun(Bit) bor F, BitNames);
encode_bits(_Fun, F, []) ->
    F.

%% Decode bit encoded flags
decode_bits(F, [{Bit,BitName}|Bits]) ->
    if F band Bit == Bit ->
	    [BitName | decode_bits(F, Bits)];
       true ->
	    decode_bits(F, Bits)
    end;
decode_bits(_F, []) ->
    [].

%% %% iolist size
%% bsize(B) ->
%%     bsize(B, 0).

%% bsize(B, S) when binary(B) ->
%%     case B of
%% 	<<>> -> S;
%% 	_ -> S + size(B)
%%     end;
%% bsize([], S) ->
%%     S;
%% bsize(I, S) when integer(I) ->
%%     S + 1;
%% bsize([A|B], S) ->
%%     bsize(B, S + bsize(A)).

connect_timeout(Opts) ->
    proplists:get_value(connect_timeout, Opts, ?DEFAULT_TIMEOUT).