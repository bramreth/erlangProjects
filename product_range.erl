-module(product_range).
-export([product_range/2]).

product_range(M,N) when M<N-> M+product_range(M+1,N);
product_range(M,M) -> M.