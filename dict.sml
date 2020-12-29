(*tomer katz- 212234140 tomerkatz@campus.technion.ac.il, ido magner-212324313 idomagner@campus.technion.ac.il*)
datatype ('a, 'b) dictionary =
    Nil
  | Dict of {key: 'a, value: 'b} list;
exception ItemNotPresent;
(*help functions for the excercise*)
fun list Nil = nil | list (Dict (lst)) = lst;
fun key {key = k, value = v} = k;
fun value {key = k, value = v} = v;
(*q1*)
fun   insert Nil k v = Dict [{key = k, value = v}]
    | insert (Dict (nil)) k v = Dict [{key = k, value = v}]
    | insert (Dict(d :: ds)) k v =
        if k = key(d) then Dict({key = k, value = v} :: ds)
        else Dict(d :: list(insert (Dict ds) k v));
(*q2*)
fun   find Nil k = raise ItemNotPresent
    | find (Dict (nil)) k = raise ItemNotPresent
    | find (Dict({key = a, value = b} :: ds)) k =
        if k = a then b
        else find (Dict ds) k;
(*q3*)
(*recursive method for remove*)
fun   remrec Nil k = raise ItemNotPresent
    | remrec (Dict (nil)) k = raise ItemNotPresent
    | remrec (Dict(d :: ds)) k =
        if k = key(d) then ds
        else  d::(remrec (Dict ds) k);

fun remove dict k=  (Dict (remrec dict k));
(*q4*)
fun   keys Nil = []
    | keys (Dict(nil))=[]
    | keys (Dict({key = a, value = b} :: ds))= a:: (keys (Dict ds));
(*q5*)
fun   values Nil = []
    | values (Dict(nil))=[]
    | values (Dict({key = a, value = b} :: ds))= b:: (values (Dict ds));
