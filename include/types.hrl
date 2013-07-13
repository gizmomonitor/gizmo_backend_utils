-define(I2B(I), list_to_binary(integer_to_list(I))).

-define(B2I(B), try list_to_integer(binary_to_list(B)) catch _:_ -> undefined end).
-define(B2A(Bin), ?L2A(binary_to_list(Bin))).

-define(L2I(L), try list_to_integer(L) of I -> I catch _:_ -> undefined end).
-define(L2A(L), try list_to_existing_atom(L) catch _:_ -> list_to_atom(L) end).