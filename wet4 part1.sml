datatype ('a, 'b) dictionary = 
    Nil
  | Dict of {key: 'a, value: 'b} list;
exception ItemNotPresent;
fun list Nil = nil | list (Dict (lst)) = lst;
fun key {key = k, value = v} = k;
fun value {key = k, value = v} = v;
fun   insert Nil k v = Dict [{key = k, value = v}]
    | insert (Dict (nil)) k v = Dict [{key = k, value = v}]
    | insert (Dict(d :: ds)) k v =
        if k = key(d) then Dict({key = k, value = v} :: ds)
        else Dict(d :: list(insert (Dict ds) k v));
fun   find Nil k = raise ItemNotPresent
    | find (Dict (nil)) k = raise ItemNotPresent
    | find (Dict({key = a, value = b} :: ds)) k =
        if k = a then b
        else find (Dict ds) k;
fun   remrec Nil k = raise ItemNotPresent
    | remrec (Dict (nil)) k = raise ItemNotPresent
    | remrec (Dict(d :: ds)) k =
        if k = key(d) then ds
        else  d::(remrec (Dict ds) k);
fun remove dict k=  (Dict (remrec dict k));
fun   keys Nil = []
    | keys (Dict(nil))=[]
    | keys (Dict({key = a, value = b} :: ds))= a:: (keys (Dict ds));
fun   values Nil = []
    | values (Dict(nil))=[]
    | values (Dict({key = a, value = b} :: ds))= b:: (values (Dict ds));

 val d = Nil;
val d = insert d 2 #"b";
val d = insert d 1 #"a";
val d = insert d 3 #"c";
find d 2;
insert Nil "a" [1,2];
remove it "a";
keys d;
values d;
