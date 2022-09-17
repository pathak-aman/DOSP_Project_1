-module(generating_hash).
-import(crypto, [hash/2]).
-import(constants, [get_prefix_constant/0]).
-export([generate_hash/1]).


get_hash(String_to_hash) ->
  io:format("The string for hashing is: ~p~n", [String_to_hash]),
  SHA_hash = crypto:hash(sha256,String_to_hash),
  String_Hash = integer_to_list(binary:decode_unsigned(SHA_hash),16),
  String_Hash.


generate_hash(Input_String) ->
  PREFIX_CONSTANT = get_prefix_constant(),
  % io:format("The prefix is: ~p~n", [PREFIX_CONSTANT]),
  String_to_hash = PREFIX_CONSTANT ++ Input_String,

  Hash = get_hash(String_to_hash),
  Hash.
