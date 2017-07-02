library(RTypeInference)
f = function(n)
{
    total = 0L
    for(i in 1:n)
        total = total + i
    total
}

g = function(n) 1:n
    
tt.f = infer_types(f)
tt.g = infer_types(g)

