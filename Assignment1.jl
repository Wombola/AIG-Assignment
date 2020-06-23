#218021925FrancisEmvula
#import Pkg; Pkg.add("DataFrames")
#import Pkg; Pkg.add("Statistics")

using CSV, DataFrames, Statistics;

datbas1 = CSV.read("bank-additional-full.csv")
datbas = convert(Matrix,datbas1)

#standardizing the values
function reg(x)
    t = mean(x, dims=1)
    j = std(x, dims=1)
    xn = (x .- t) ./ j
    return (xn, t, j);
end

function stdiz(x, t, j)
            xn = (x .- t) ./ j
    return xn;
end

xt, t, j = reg(x_train);
xtst = stdiz(x_test, t, j);

#cleaning data
function cleanUp(DST)
    f= DST[:,21]
    y = Array{Int64}(undef,size(DST)[1],1)

    for i in 1:size(DST)[1]
#job
        if(DST[i,2] == "housemaid")
            DST[i,2] = 1
        elseif(DST[i,2] == "services")
            DST[i,2] = 2
        elseif(DST[i,2] == "admin.")
            DST[i,2] = 3
        elseif(DST[i,2] == "blue-sollar")
            DST[i,2] = 4
        elseif(DST[i,2] == "technician")
            DST[i,2] = 5
        elseif(DST[i,2] == "management")
            DST[i,2] = 6
        elseif(DST[i,2] == "self-employed")
            DST[i,2] = 7
        elseif(DST[i,2] == "retired")
            DST[i,2] = 8
        elseif(DST[i,2] == "unemployed")
            DST[i,2] = 9
        elseif(DST[i,2] == "unknown")
            DST[i,2] = 10
        end
#marital
        if(DST[i,3] == "married")
            DST[i,3] = 1
        elseif(DST[i,3] == "single")
            DST[i,3] = 2
        elseif(DST[i,3] == "divorced")
            DST[i,3] = 3
        end
#education
        if(DST[i,4] == "basic.4y")
            DST[i,4] = 1
        elseif(DST[i,4] == "high.school")
            DST[i,4] = 2
        elseif(DST[i,4] == "professional.course")
            DST[i,4] = 3
        elseif(DST[i,4] == "unknown")
            DST[i,4] = 4
        elseif(DST[i,4] == "basic.9y")
            DST[i,4] = 5
        elseif(DST[i,4] == "basic.6y")
            DST[i,4] = 6
        elseif(DST[i,4] == "university.degree")
            DST[i,4] = 7
        end

#default
        if(DST[i,5] == "no")
            DST[i,5] = 1
        elseif(DST[i,5] == "yes")
            DST[i,5] = 2
        elseif(DST[i,5] == "unknown")
            DST[i,5] = 3
        end
#housing
        if(DST[i,6] == "no")
            DST[i,6] = 1
        elseif(DST[i,6] == "yes")
            DST[i,6] = 2
        elseif(DST[i,6] == "unknown")
            DST[i,6] = 3
        end
#loan
        if(DST[i,7] == "no")
            DST[i,7] = 1
        elseif(DST[i,7] == "yes")
            DST[i,7] = 2
        elseif(DST[i,7] == "unknown")
            DST[i,7] = 3
        end
#contact
        if(DST[i,8] == "telephone")
            DST[i,8] = 1
        elseif(DST[i,8] == "cellular")
            DST[i,8] = 2
        end
#month
        if(DST[i,9] == "jan")
            DST[i,9] = 1
        elseif(DST[i,9] == "feb")
            DST[i,9] = 2
        elseif(DST[i,9] == "mar")
            DST[i,9] = 3
        elseif(DST[i,9] == "apr")
            DST[i,9] = 4
        elseif(DST[i,9] == "may")
            DST[i,9] = 5
        elseif(DST[i,9] == "jun")
            DST[i,9] = 6
        elseif(DST[i,9] == "jul")
            DST[i,9] = 7
        elseif(DST[i,9] == "aug")
            DST[i,9] = 8
        elseif(DST[i,9] == "sep")
            DST[i,9] = 9
        elseif(DST[i,9] == "oct")
            DST[i,9] = 10
        elseif(DST[i,9] == "nov")
            DST[i,9] = 11
        elseif(DST[i,9] == "dec")
            DST[i,9] = 12
        end
#Day
        if(DST[i,10] == "mon")
            DST[i,10] = 1
        elseif(DST[i,10] == "tue")
            DST[i,10] = 2
        elseif(DST[i,10] == "wed")
            DST[i,10] = 3
        elseif(DST[i,10] == "thu")
            DST[i,10] = 4
        elseif(DST[i,10] == "fri")
            DST[i,10] = 5
        elseif(DST[i,10] == "sat")
            DST[i,10] = 6
        elseif(DST[i,10] == "sun")
            DST[i,10] = 7
#pout
        if(DST[i,15] == "nonexistent")
            DST[i,15] = 1
        elseif(DST[i,15] == "success")
            DST[i,15] = 2
        elseif(DST[i,15] == "failure")
            DST[i,15] = 3
        end
#y
        if(DST[i,21] == "yes")
            DST[i,21] = 1
        else
            DST[i,21] = 0
        end
            
    x = convert(Array{Float64},DST[:,1:20])
    return (x,y)
end

#hypothesis
function hypo(z)
    return 1 ./ (1 .+ exp.(.-z))
end

function regcst(x, y, θ, λ)
    d = length(y)
    k = hypo(x * θ)
    pcst = ((-y)' * log.(k))
    ncst = ((1 .- y)' * log.(1 .- k))
    lreg = (λ/(2*d) * sum(θ[2 : end] .^ 2))
    q = (1/d) * (pcst - ncst) + lreg
    pq = (1/d) * (x') * (k-y) + ((1/d) * (λ * θ)) 
    pq[1] = (1/d) * (x[:, 1])' * (k-y) 
    return (q, pq)
end

#gradient descent
function loggd(x, y, λ, FI=true, d=0.01, numbr=1000)

    u = length(y); 
    if FI     
        cons = ones(u, 1)
        x = hcat(cons, x)
    else
        x
    end
    b = size(x)[2]
    g = zeros(b)
    v = zeros(numbr)
    for iter in range(1, stop=numbr)
    v[iter], ev = regcst(x, y, θ, λ)
    g = g - (d * ev)
    end
    return (θ, v)
end
#training/testing
cln = cleanUp(datbas)
x = cln[1]
x = reg(x)
f = cln[2]
mtx = ones(size(x)[1])
x = hcat(x,mtx)
rey = zeros(size(x)[2])
i = trunc(Int,(size(x)[1]) * 0.8)
x_train = x[1:i,:]
y_train = f[1:i,:]
x_test = x[i+1:size(x)[1],:]
y_test = f[i+1:size(x)[1],:]
function predi(x, θ, FI=true)
    o = size(x)[1]

    if FI
        cons = ones(o, 1)
        x = hcat(cons, x)
    else
        x
    end

    j = hypo(x * θ)
    return j
end

#
function pclass(proba, threshold=0.5)
    return proba .>= threshold
end

#
trs = mean(y_train .== pclass(predi(xt, θ)));
ts = mean(y_test .== pclass(predi(xtst, θ)));

#
println("Training score:", round(trs, sigdigits=2))
println("Testing score:", round(ts, sigdigits=2))
