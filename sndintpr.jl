module Scheme

export lex, Obj, head, tail, displayobj, tokenize, cons, islist, dsplo, ptol, tokentoobj, ltop, tokenstolist, sread, repl, evaluate

#typestuff
struct Token
    type
    str
end

struct Sym
    name
end

eq(x::Sym, y::Sym) = x.name == y.name
eq(x, y) = x == y

struct Primitiveop
    f
end

struct Compositeop
    arglist
    body
end

const D = Union{Int, Float64, String, SubString{String}, Bool, Primitiveop, Compositeop, Sym, Nothing}

const Obj = Union{D, Tuple}

#list related functions
function islist(x::Obj)
    if typeof(x) <: Tuple
        if x[2] == nothing
            return true
        else
            return islist(x[2])
        end
    else
        return false
    end
end

function ptol(x::Obj)
    if typeof(x) == Nothing
        return []
    elseif typeof(x) <: D
        return x
    elseif islist(x)
        return vcat(x[1], ptol(x[2]))
    else
        print("cannot turn non-list nested pairs into list ")
    end
end

function ltop(x)
    y = reverse(x)
    r = nothing
    for i in y
        if typeof(i) <: Array
            r = cons(ltop(i), r)
        else
            r = cons(i, r)
        end
    end
    return r
end

function fevaluate end

ltop(x::D) = x

#builtins
cons(a, b) = (a, b)
car(x) = x[1]
cdr(x) = x[2]
caar(x) = car(car(x))
cadr(x) = car(cdr(x))
cddr(x) = cdr(cdr(x))
equal(x, y) = x == y
negate(x) = typeof(x) <: Bool ? !(x) : println("Object not boolean")
#ifthenelse(cond, tr, fal) = cond == true ? tr : fal
list(x...) = ltop(x)

function loadcont(x)
    io = open(x, "r")
    str = read(io, String)
    close(io)
    return str
end

function load(x)
    str = loadcont(x)
    objs = map(ltop, tokenstolist(lex(str)))
    map(fevaluate, objs)
    return nothing
end


symbtabel = Dict{Any, Any}(
 "cons" => Primitiveop(cons),
 "car" => Primitiveop(car),
 "cdr" => Primitiveop(cdr),
 "caar" => Primitiveop(caar),
 "cadr" => Primitiveop(cadr),
 "cddr" => Primitiveop(cddr),
 "=" => Primitiveop(equal),
 "!" => Primitiveop(negate),
 #"if" => Primitiveop(ifthenelse),
 "list" => Primitiveop(list),
 "+" => Primitiveop(+),
 "-" => Primitiveop(-),
 "*" => Primitiveop(*),
 "eval" => Primitiveop(fevaluate),
 "args" => Primitiveop(x->ltop(x.arglist)),
 "body" => Primitiveop(x->x.body),
 "load" => Primitiveop(load),
 "loadcont" => Primitiveop(loadcont),
 "type" => Primitiveop(x->print(typeof(x)))
 )

#evaluation logic
function fapply(func, args)
    if typeof(func) <: Primitiveop
        return func.f(args...)

    elseif typeof(func) <: Compositeop
        argbinds = Dict()
        i = 0
        for arg in func.arglist
            i += 1
            merge!(argbinds, Dict(arg.name=>args[i]))
        end

        fevaluate(func.body; exenv = argbinds)
    else
        print("error, first element is not callable")
    end
end

function fevaluate(x; exenv = Dict())
    if typeof(x) <: Sym
        return haskey(exenv, x.name) ? exenv[x.name] : symbtabel[x.name]
    elseif typeof(x) <: D
        return x
    elseif (islist(x))
        #binding symbols to values and creating functions
        if typeof(car(x)) <: Sym && car(x).name == "def"
            if typeof(cadr(x)) <: Sym
                merge!(symbtabel, Dict(car(cdr(x)).name => fevaluate(car(cddr(x)), exenv = exenv)))
                return fevaluate(car(cddr(x)), exenv = exenv)
            elseif islist(cadr(x))
                k = cadr(x)
                name = car(k).name
                args = cdr(k)
                body = car(cdr(cdr(x)))
                merge!(symbtabel, Dict(name => Compositeop(ptol(args), body)))
                return Compositeop(ptol(args), body)
            end

        #creating functions
        elseif typeof(car(x)) <: Sym && car(x).name == "lambda"
            return Compositeop(ptol(car(cdr(x))), car(cddr(x)))

        elseif typeof(car(x)) <: Sym && car(x).name == "quote"
            return cadr(x)

        elseif typeof(car(x)) <: Sym && car(x).name == "if"
            if fevaluate(cadr(x), exenv = exenv) == true
                return fevaluate(car(cddr(x)), exenv = exenv)
            else
                return fevaluate(cadr(cddr(x)), exenv = exenv)
            end
        else
            return fapply(fevaluate(car(x), exenv = exenv), map(i -> fevaluate(i, exenv = exenv), ptol(cdr(x))))
        end
    else
        println("Evaluation requires proper lists")
    end
end


function parenmatch(str)
    counter = 0
    for l in str
        if l == '('
            counter += 1
        elseif l == ')'
            counter -= 1
        end

        if counter < 0
            return false
        end
    end
    counter == 0 ? true : false
end

function tokenize(str)
    tp = ""
    match(r"\".*\"", str) != nothing && match(r"\".*\"", str).match == str && (tp = "string")
    match(r"[0-9]+\.[0-9]+", str) != nothing && match(r"[0-9]+\.?[0-9]*", str).match == str && (tp = "float")
    match(r"[0-9]+", str) != nothing && match(r"[0-9]+", str).match == str && (tp = "int")
    (str == "true" || str == "false") && (tp = "bool")
    str == "(" && (tp = "lparen")
    str == ")" && (tp = "rparen")
    tp == "" ? tp = "id" : tp = tp
    return Token(tp, str)
end

function lex(inp)
    inp = replace(inp, "(" => " ( ")
    inp = replace(inp, ")" => " ) ")
    inp = split(inp, r"[\h, \v]+")
    inp = filter(i -> length(i) > 0, inp)
    tks = map(tokenize, inp)
end

function atom(x::Token)
    if x.type == "string"
        return String(x.str[2:end-1])
    elseif x.type == "float"
        return parse(Float64, x.str)
    elseif x.type == "int"
        return parse(Int, x.str)
    elseif x.type == "bool"
        return parse(Bool, x.str)
    elseif x.type == "id"
        return Sym(x.str)
    end
end


function recurreplacequote(lis)
    newlist = []
    templist = []
    i = 1
    while i != length(lis)+1
        if typeof(lis[i]) <: Sym && lis[i].name == "'"
            push!(templist, Sym("quote"))
            push!(templist, recurreplacequote(lis[i += 1]))
            push!(newlist, templist)
            i += 1
        elseif typeof(lis[i]) <: Array
            push!(newlist, recurreplacequote(lis[i]))
            i += 1
        else
            push!(newlist, lis[i])
            i += 1
        end
    end
    return newlist
end

function tokenstolist(tokens)
    rstack = []
    objstack = []
    for tok in tokens
        if tok.str == "("
            push!(objstack, [])
        elseif tok.str == ")"
            ob = pop!(objstack)
            objstack == [] ? push!(rstack, ob) : push!(objstack[end], ob)
        else
            objstack == [] ? push!(rstack, atom(tok)) : push!(objstack[end], atom(tok))
        end
    end
    return recurreplacequote(rstack)
end





function readexpression()
    buffer = ""

    while true
        buffer = buffer * readline();
        !(parenmatch(buffer)) || break
    end
    return buffer
end

sread() = map(ltop, tokenstolist(lex(readexpression())))


function displayobj(x::Obj)
    if typeof(x) <: Sym
        print(x.name)
    elseif typeof(x) <: Compositeop
        print("args: ")
        displayobj(ltop(x.arglist))
        println()
        print("body: ")
        displayobj(x.body)
    elseif typeof(x) <: D
        x == nothing ? print("()") :  print(x)
    elseif islist(x)
        print("(")
        while x != nothing
            #if islist(x[1])
            #    displayobj(x[1])
            #    x[2] != nothing && print(" ")
            #    x = x[2]
            #else
                displayobj(x[1])
                x[2] != nothing && print(" ")
                x = x[2]
            #end
        end
        print(")")
    elseif typeof(x) <: Tuple
        print("(")
        displayobj(x[1])
        print(" . ")
        displayobj(x[2])
        print(")")
    end
end

dsplo(x::Obj) = begin
    displayobj(x)
    println()
    end

function repl()
    while true
        print(">")
        #objs = map(evaluate, sread())
        #objs = sread()
        objs = map(fevaluate, sread())
        map(dsplo, objs)
        println()
    end
end
end
