-module(two_7).
-export([maximum/1]).


maximum([A|As]) -> max(A, maximum(As));
maximum([A]) -> A.