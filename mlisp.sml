(*tomer katz- 212234140 tomerkatz@campus.technion.ac.il, ido magner-212324313 idomagner@campus.technion.ac.il*)
datatype Atom =
   SYMBOL of string | NUMBER of int | NIL;

datatype SExp =
   ATOM of Atom | CONS of (SExp * SExp);
local

fun getMatchBraAux ([],counter,res)=res
     |getMatchBraAux (("("::xs), counter,res)= if(counter=0) then getMatchBraAux(xs,(counter+1),res) else getMatchBraAux(xs,(counter+1),res@["("])
     |getMatchBraAux((")"::xs),counter,res)=if(counter=1) then res@[")"] else getMatchBraAux(xs,(counter-1),res@[")"])
     |getMatchBraAux((x::xs),counter,res)=getMatchBraAux(xs,counter,res@[x]);

fun getMatchBra lis=getMatchBraAux(lis,0,[]);

fun getIndexAux ([],i_counter, b_counter)=i_counter
    |getIndexAux(("("::xs),i_counter,b_counter)=getIndexAux(xs,(i_counter+1),(b_counter+1))
    |getIndexAux((")"::xs),i_counter,b_counter)=if(b_counter=1)then i_counter else getIndexAux(xs,(i_counter+1),(b_counter-1))
    |getIndexAux((x::xs),i_counter,b_counter)=getIndexAux(xs,(i_counter+1),b_counter);

    (*this function get a list that starts with "(" and returns the index of the mathing ")" to this from the start*)
fun getIndex(lis)=getIndexAux(lis,0,0);

   (*this get the list without the first bracket*)
   fun parseAux []=ATOM(NIL)
      |parseAux[")"]=ATOM(NIL)
     |parseAux ("("::xs)=CONS(parseAux(getMatchBra ("("::xs)) ,parseAux(List.drop(xs,getIndex("("::xs))))
     |parseAux (x::xs)=if (isNumber(x)) then CONS(ATOM(NUMBER(atoi(x))),parseAux(xs)) else CONS(ATOM(SYMBOL(x)),parseAux(xs));


 in
  fun parse []=ATOM(NIL)
  |parse ["(",")"]=ATOM(NIL)
  |parse [x]=if (isNumber(x)) then ATOM(NUMBER(atoi(x))) else ATOM(SYMBOL(x))
  |parse ("("::xs) =parseAux(xs)


end;
