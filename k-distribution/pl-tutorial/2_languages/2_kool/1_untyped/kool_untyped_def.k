module KOOL-UNTYPED-SYNTAX
  imports DOMAINS-SYNTAX
  syntax Id ::= "Object" [token] | "Main" [token]
  syntax Viz ::= "Pub" [token] | "Private" [token]
  syntax Stmt ::= "var" Exps ";"
                | "method" Id "(" Ids ")" Block  // called "function" in SIMPLE
                | "private" "var" Exps ";"          // new private vars 
                | "private" "method" Id "(" Ids ")" Block       // new private method
                | "class" Id Block               // KOOL
                | "class" Id "extends" Id Block  // KOOL

  syntax Exp ::= Int | Bool | String | Id
               | "this"                                 // KOOL
               | "super"                                // KOOL
               | "(" Exp ")"             [bracket]
               | "++" Exp
               | Exp "instanceOf" Id     [strict(1)]    // KOOL
               | "(" Id ")" Exp          [strict(2)]    // KOOL  cast
               | "new" Id "(" Exps ")"   [strict(2)]    // KOOL
               | Exp "." Id                             // KOOL
               > Exp "[" Exps "]"        [strict]
               > Exp "(" Exps ")"        [strict(2)]    // was strict in SIMPLE
               | "-" Exp                 [strict]
               | "sizeOf" "(" Exp ")"    [strict]
               | "read" "(" ")"
               > left:
                 Exp "*" Exp             [strict, left]
               | Exp "/" Exp             [strict, left]
               | Exp "%" Exp             [strict, left]
               > left:
                 Exp "+" Exp             [strict, left]
               | Exp "-" Exp             [strict, left]
               > non-assoc:
                 Exp "<" Exp             [strict, non-assoc]
               | Exp "<=" Exp            [strict, non-assoc]
               | Exp ">" Exp             [strict, non-assoc]
               | Exp ">=" Exp            [strict, non-assoc]
               | Exp "==" Exp            [strict, non-assoc]
               | Exp "!=" Exp            [strict, non-assoc]
               > "!" Exp                 [strict]
               > left:
                 Exp "&&" Exp            [strict(1), left]
               | Exp "||" Exp            [strict(1), left]
               > "spawn" Block
               > Exp "=" Exp             [strict(2), right]

  syntax Ids  ::= List{Id,","}

  syntax Exps ::= List{Exp,","}          [strict, klabel(exps)]
  syntax Val
  syntax Vals ::= List{Val,","}          [klabel(exps)]

  syntax Block ::= "{" "}"
                | "{" Stmt "}"

  syntax Stmt ::= Block
                | Exp ";"                               [strict]
                | "if" "(" Exp ")" Block "else" Block   [avoid, strict(1)]
                | "if" "(" Exp ")" Block                [macro]
                | "while" "(" Exp ")" Block
                | "for" "(" Stmt Exp ";" Exp ")" Block  [macro]
                | "return" Exp ";"                      [strict]
                | "return" ";"                          [macro]
                | "print" "(" Exps ")" ";"              [strict]
                | "try" Block "catch" "(" Id ")" Block
                | "throw" Exp ";"                       [strict]
                | "join" Exp ";"                        [strict]
                | "acquire" Exp ";"                     [strict]
                | "release" Exp ";"                     [strict]
                | "rendezvous" Exp ";"                  [strict]

  syntax Stmt ::= Stmt Stmt                          [right]
  rule if (E) S => if (E) S else {}
  rule for(Start Cond; Step) {S} => {Start while (Cond) {S Step;}}
  rule var E1::Exp, E2::Exp, Es::Exps; => var E1; var E2, Es;       [anywhere]
  rule var X::Id = E; => var X; X = E;                              [anywhere]

  rule private var E1::Exp, E2::Exp, Es::Exps; => private var E1; private var E2, Es;   // private var desugaring 
  rule private var X::Id = E; => private var X; X = E;                                  // private var desugaring 

  rule class C:Id S => class C extends Object S                     // KOOL

endmodule

module KOOL-UNTYPED
  imports KOOL-UNTYPED-SYNTAX
  imports DOMAINS
  // the syntax declarations below are required because the sorts are
  // referenced directly by a production and, because of the way KIL to KORE
  // is implemented, the configuration syntax is not available yet
  // should simply work once KIL is removed completely
  // check other definitions for this hack as well
  syntax EnvCell
  syntax ControlCell
  syntax EnvStackCell
  syntax CrntObjCellFragment

  configuration <T color="red">
                  <threads color="orange">
                    <thread multiplicity="*" type="Set" color="yellow">
                      <k color="green"> $PGM:Stmt ~> execute </k>
                    //<br/> // TODO(KORE): support latex annotations #1799
                      <control color="cyan">
                        <fstack color="blue"> .List </fstack>
                        <xstack color="purple"> .List </xstack>
                      //<br/> // TODO(KORE): support latex annotations #1799
                        <crntObj color="Fuchsia">  // KOOL
                           <crntClass> Object </crntClass>
                           <visibility>.Map </visibility>
                           <envStack> .List </envStack>
                           <location multiplicity="?"> .K </location>
                        </crntObj>
                      </control>
                    //<br/> // TODO(KORE): support latex annotations #1799
                      <env color="violet"> .Map </env>
                      <holds color="black"> .Map </holds>
                      <id color="pink"> 0 </id>
                    </thread>
                  </threads>
                //<br/> // TODO(KORE): support latex annotations #1799
                  <store color="white"> .Map </store>
                  <busy color="cyan">.Set </busy>
                  <terminated color="red"> .Set </terminated>
                  <input color="magenta" stream="stdin"> .List </input>
                  <output color="brown" stream="stdout"> .List </output>
                  <nextLoc color="gray"> 0 </nextLoc>
                //<br/> // TODO(KORE): support latex annotations #1799
                  <classes color="Fuchsia">        // KOOL
                     <classData multiplicity="*" type="Map" color="Fuchsia">
                        // the Map has as its key the first child of the cell,
                        // in this case the className cell.
                        <className color="Fuchsia"> Main </className>
                        <baseClass color="Fuchsia"> Object </baseClass>
                        <declarations color="Fuchsia"> .K </declarations>
                     </classData>
                  </classes>
                </T>
  syntax Val ::= Int | Bool | String
               | array(Int,Int)
  syntax Exp ::= Val
  syntax Exps ::= Vals
  syntax KResult ::= Val
  syntax KResult ::= Vals
  syntax KItem ::= "undefined"  [latex(\bot)]

  rule <k> var X:Id; => . ...</k>
       <env> Env => Env[X <- L] </env>
       <store>... .Map => L |-> undefined ...</store>
       <visibility> ... .Map => X |-> Pub ...</visibility>
       <nextLoc> L:Int => L +Int 1 </nextLoc>

  rule <k> private var X:Id; => . ...</k>
       <env> Env => Env[X <- L] </env>
       <visibility> ... .Map => X |-> Private ...</visibility>
       <store>... .Map => L |-> undefined ...</store>
       <nextLoc> L:Int => L +Int 1 </nextLoc>

  context var _:Id[HOLE];

  rule <k> var X:Id[N:Int]; => . ...</k>
       <env> Env => Env[X <- L] </env>
       <store>... .Map => L |-> array(L +Int 1, N)
                          (L +Int 1) ... (L +Int N) |-> undefined ...</store>
       <nextLoc> L:Int => L +Int 1 +Int N </nextLoc>
    when N >=Int 0


  syntax Id ::= "$1" [token] | "$2" [token]
  rule var X:Id[N1:Int, N2:Int, Vs:Vals];
    => var X[N1];
       {
         var $1=X;
         for(var $2=0; $2 <= N1 - 1; ++$2) {
           var X[N2,Vs];
           $1[$2] = X;
         }
       }


  rule <k> X:Id => V ...</k>
       <env>... X |-> L ...</env>
       <store>... L |-> V:Val ...</store>  [group(lookup)]


  context ++(HOLE => lvalue(HOLE))
  rule <k> ++loc(L) => I +Int 1 ...</k>
       <store>... L |-> (I:Int => I +Int 1) ...</store>  [group(increment)]


  rule I1 + I2 => I1 +Int I2
  rule Str1 + Str2 => Str1 +String Str2
  rule I1 - I2 => I1 -Int I2
  rule I1 * I2 => I1 *Int I2
  rule I1 / I2 => I1 /Int I2 when I2 =/=K 0
  rule I1 % I2 => I1 %Int I2 when I2 =/=K 0
  rule - I => 0 -Int I
  rule I1 < I2 => I1 <Int I2
  rule I1 <= I2 => I1 <=Int I2
  rule I1 > I2 => I1 >Int I2
  rule I1 >= I2 => I1 >=Int I2

  rule V1:Val == V2:Val => V1 ==K V2
  rule V1:Val != V2:Val => V1 =/=K V2
  rule ! T => notBool(T)
  rule true  && E => E
  rule false && _ => false
  rule true  || _ => true
  rule false || E => E


  rule V:Val[N1:Int, N2:Int, Vs:Vals] => V[N1][N2, Vs]
    [anywhere]

  rule array(L,_)[N:Int] => lookup(L +Int N)
    [anywhere]


  rule sizeOf(array(_,N)) => N
  rule <k> return(V:Val); ~> _ => V ~> K </k>
       <control>
         <fstack> ListItem(fstackFrame(Env,K,XS,<crntObj> CO </crntObj>)) => .List ...</fstack>
         <xstack> _ => XS </xstack>
         <crntObj> _ => CO </crntObj>
       </control>
       <env> _ => Env </env>

  syntax Val ::= "nothing"
  rule return; => return nothing;


  rule <k> read() => I ...</k> <input> ListItem(I:Int) => .List ...</input>  [group(read)]


  context (HOLE => lvalue(HOLE)) = _

  rule <k> loc(L) = V:Val => V ...</k> <store>... L |-> (_ => V) ...</store>
    [group(assignment)]


  rule {} => .
  rule <k> { S } => S ~> setEnv(Env) ...</k>  <env> Env </env>


  rule S1::Stmt S2::Stmt => S1 ~> S2

  rule _:Val; => .

  rule if ( true) S else _ => S
  rule if (false) _ else S => S

  rule while (E) S => if (E) {S while(E)S}

  rule <k> print(V:Val, Es => Es); ...</k> <output>... .List => ListItem(V) </output>
    [group(print)]
  rule print(.Vals); => .


  syntax KItem ::= xstackFrame(Id,Stmt,K,Map,K)
  // TODO(KORE): drop the additional production once parsing issue #1842 is fixed
                 | (Id,Stmt,K,Map,K)

  syntax KItem ::= "popx"

  rule <k> (try S1 catch(X) {S2} => S1 ~> popx) ~> K </k>
       <control>
         <xstack> .List => ListItem(xstackFrame(X, S2, K, Env, C)) ...</xstack>
         C
       </control>
       <env> Env </env>

  rule <k> popx => . ...</k>
       <xstack> ListItem(_) => .List ...</xstack>

  rule <k> throw V:Val; ~> _ => { var X = V; S2 } ~> K </k>
       <control>
         <xstack> ListItem(xstackFrame(X, S2, K, Env, C)) => .List ...</xstack>
         (_ => C)
       </control>
       <env> _ => Env </env>
  // TODO(KORE): ..Bag should be . throughout this definition #1772
  rule (<thread>... <k>.</k> <holds>H</holds> <id>T</id> ...</thread> => .Bag)
  /*
  rule (<thread>... <k>.</k> <holds>H</holds> <id>T</id> ...</thread> => .)
  */
       <busy> Busy => Busy -Set keys(H) </busy>
       <terminated>... .Set => SetItem(T) ...</terminated>

  rule <k> join T:Int; => . ...</k>
       <terminated>... SetItem(T) ...</terminated>

  rule <k> acquire V:Val; => . ...</k>
       <holds>... .Map => V |-> 0 ...</holds>
       <busy> Busy (.Set => SetItem(V)) </busy>
    when (notBool(V in Busy:Set))  [group(acquire)]

  rule <k> acquire V; => . ...</k>
       <holds>... V:Val |-> (N:Int => N +Int 1) ...</holds>

  rule <k> release V:Val; => . ...</k>
       <holds>... V |-> (N => N:Int -Int 1) ...</holds>
    when N >Int 0

  rule <k> release V; => . ...</k> <holds>... V:Val |-> 0 => .Map ...</holds>
       <busy>... SetItem(V) => .Set ...</busy>

  rule <k> rendezvous V:Val; => . ...</k>
       <k> rendezvous V; => . ...</k>  [group(rendezvous)]
  syntax Stmt ::= mkDecls(Ids,Vals)  [function]
  rule mkDecls((X:Id, Xs:Ids), (V:Val, Vs:Vals)) => var X=V; mkDecls(Xs,Vs)
  rule mkDecls(.Ids,.Vals) => {}

  // TODO(KORE): clarify sort inferences #1803
  syntax Exp ::= lookup(Int)
  /*
  syntax KItem ::= lookup(Int)
  */
  rule <k> lookup(L) => V ...</k> <store>... L |-> V:Val ...</store>  [group(lookup)]

  syntax KItem ::= setEnv(Map)
  rule <k> setEnv(Env) => . ...</k>  <env> _ => Env </env>
  rule (setEnv(_) => .) ~> setEnv(_)
  // TODO: How can we make sure that the second rule above applies before the first one?
  //       Probably we'll deal with this using strategies, eventually.

  syntax Exp ::= lvalue(K)
  syntax Val ::= loc(Int)

  rule <k> lvalue(X:Id => loc(L)) ...</k> <env>... X |-> L:Int ...</env>

  context lvalue(_::Exp[HOLE::Exps])
  context lvalue(HOLE::Exp[_::Exps])

  rule lvalue(lookup(L:Int) => loc(L))


  syntax Map ::= Int "..." Int "|->" K
    [function, latex({#1}\ldots{#2}\mapsto{#3})]
  rule N...M |-> _ => .Map  when N >Int M
  rule N...M |-> K => N |-> K (N +Int 1)...M |-> K  when N <=Int M
  syntax KItem ::= "execute"
  rule <k> execute => new Main(.Exps); </k> <env> .Map </env>
  syntax Val ::= objectClosure(Id, List)
               | methodClosure(Id,Int,Ids,Stmt)

  syntax KItem ::= fstackFrame(Map,K,List,K)
  // TODO(KORE): drop the additional production once parsing issue #1842 is fixed
                 | (Map,K,K)

  rule <k> methodClosure(Class,OL,Xs,S)(Vs:Vals) ~> K
           => mkDecls(Xs,Vs) S return; </k>
       <env> Env => .Map </env>
       <store>... OL |-> objectClosure(_, EnvStack)...</store>
     //<br/> // TODO(KORE): support latex annotations #1799
       <control>
          <xstack> XS </xstack>
          <fstack> .List => ListItem(fstackFrame(Env, K, XS, <crntObj> Obj' </crntObj>))
          ...</fstack>
          <crntObj> Obj' => <crntClass> Class </crntClass> 
          // <visibility> _VZ </visibility> 
          <envStack> EnvStack </envStack> </crntObj>
       </control>

  rule <thread>...
         <k> spawn S => !T:Int ...</k>
         <env> Env </env>
         <crntObj> Obj </crntObj>
       ...</thread>
       (.Bag => <thread>...
               <k> S </k>
               <env> Env </env>
               <id> !T </id>
               <crntObj> Obj </crntObj>
             ...</thread>)

  rule <k> class Class1 extends Class2 { S } => . ...</k>
       <classes>... (.Bag => <classData>
                            <className> Class1 </className>
                            <baseClass> Class2 </baseClass>
                            <declarations> S </declarations>
                        </classData>)
       ...</classes>
  rule <k> method F:Id(Xs:Ids) S => . ...</k>
       <crntClass> Class:Id </crntClass>
       <location> OL:Int </location>
       <env> Env => Env[F <- L] </env>
       <store>... .Map => L |-> methodClosure(Class,OL,Xs,S) ...</store>
       <nextLoc> L => L +Int 1 </nextLoc>

  rule <k>private method F:Id(Xs:Ids) S => . ...</k>
       <crntClass> Class:Id </crntClass>
       <location> OL:Int </location>
       <env> Env => Env[F <- L] </env>
       <store>... .Map => L |-> methodClosure(Class,OL,Xs,S) ...</store>
       <nextLoc> L => L +Int 1 </nextLoc>

  syntax KItem ::= "envStackFrame" "(" Id "," Map ")"

  rule <k> new Class:Id(Vs:Vals) ~> K
           => create(Class) ~> storeObj ~> Class(Vs); return this; </k>
       <env> Env => .Map </env>
       <nextLoc> L:Int => L +Int 1 </nextLoc>
     //<br/> // TODO(KORE): support latex annotations #1799
       <control> <xstack> XS </xstack>
         <crntObj> Obj
                   => <crntClass> Object </crntClass>
                      <visibility> .Map </visibility>
                      <envStack> ListItem(envStackFrame(Object, .Map)) </envStack>
                      <location> L </location>
         </crntObj>
         <fstack> .List => ListItem(fstackFrame(Env, K, XS, <crntObj> Obj </crntObj>)) ...</fstack>
       </control>
  syntax KItem ::= create(Id)

  rule <k> create(Class:Id)
           => create(Class1) ~> setCrntClass(Class) ~> S ~> addEnvLayer ...</k>
       <className> Class </className>
       <baseClass> Class1:Id </baseClass>
       <declarations> S </declarations>

  rule <k> create(Object) => . ...</k>

  syntax KItem ::= setCrntClass(Id)

  rule <k> setCrntClass(C) => . ...</k>
       <crntClass> _ => C </crntClass>

  syntax KItem ::= "addEnvLayer"

  rule <k> addEnvLayer => . ...</k>
       <env> Env => .Map </env>
       <crntClass> Class:Id </crntClass>
       <envStack> .List => ListItem(envStackFrame(Class, Env)) ...</envStack>

  syntax KItem ::= "storeObj"

  rule <k> storeObj => . ...</k>
      <crntObj> 
        <crntClass> CC </crntClass> 
        <visibility> _VS </visibility> 
        <envStack> ES </envStack> 
        (<location> L:Int </location> => .Bag) 
      </crntObj>
      <store>... .Map => L |-> objectClosure(CC, ES) ...</store>

  rule <k> this => objectClosure(CC, ES) ...</k>
       <crntObj> 
          <crntClass> CC </crntClass> 
          // <visibility> VZ => VZ </visibility> 
          <envStack> ES </envStack> 
        </crntObj>

  rule <k> X:Id => this . X ...</k> <env> Env:Map </env>
    when notBool(X in keys(Env))

  context HOLE._::Id when (HOLE =/=K super)

// TODO: explain how Assoc matching has been replaced with two rules here.
// Maybe also improve it a bit.

/*  rule objectClosure(<crntClass> Class:Id </crntClass>
                     <envStack>... envStackFrame(Class,EnvC) EStack </envStack>)
       . X:Id
    => lookupMember(envStackFrame(Class,EnvC) EStack, X) */

  rule objectClosure(Class:Id, ListItem(envStackFrame(Class,Env)) EStack)
       . X:Id
    => lookupMember(ListItem(envStackFrame(Class,Env)) EStack, X)
  rule objectClosure(Class:Id, (ListItem(envStackFrame(Class':Id,_)) => .List) _)
       . _X:Id
    when Class =/=K Class'

/*  rule <k> super . X => lookupMember(EStack, X) ...</k>
       <crntClass> Class </crntClass>
       <envStack>... envStackFrame(Class,EnvC) EStack </envStack> */
  rule <k> super . X => lookupMember(EStack, X) ...</k>
       <crntClass> Class:Id </crntClass>
       <envStack> ListItem(envStackFrame(Class,_)) EStack </envStack>
  rule <k> super . _X ...</k>
       <crntClass> Class </crntClass>
       <envStack> ListItem(envStackFrame(Class':Id,_)) => .List ...</envStack>
    when Class =/=K Class'

  rule <k> (X:Id => V)(_:Exps) ...</k>
       <env>... X |-> L ...</env>
       <store>... L |-> V:Val ...</store>  [group(lookup)]

  rule <k> (X:Id => this . X)(_:Exps) ...</k>
       <env> Env </env>
    when notBool(X in keys(Env))

  context HOLE._::Id(_) when HOLE =/=K super

  rule (objectClosure(_, EStack) . X
    => lookupMember(EStack, X:Id))(_:Exps)

/*  rule <k> (super . X
            => lookupMember(EStack,X))(_:Exps)...</k>
       <crntClass> Class </crntClass>
       <envStack>... envStackFrame(Class,_) EStack </envStack> */
  rule <k> (super . X
            => lookupMember(EStack,X))(_:Exps)...</k>
       <crntClass> Class </crntClass>
       <envStack> ListItem(envStackFrame(Class,_)) EStack </envStack>
  rule <k> (super . _X)(_:Exps) ...</k>
       <crntClass> Class </crntClass>
       <envStack> ListItem(envStackFrame(Class':Id,_)) => .List ...</envStack>
    when Class =/=K Class'

  // TODO(KORE): fix getKLabel #1801
  rule (A:Exp(B:Exps))(C:Exps) => A(B) ~> #freezerFunCall(C)
  rule (A:Exp[B:Exps])(C:Exps) => A[B] ~> #freezerFunCall(C)
  rule V:Val ~> #freezerFunCall(C:Exps) => V(C)
  syntax KItem ::= "#freezerFunCall" "(" K ")"
  /*
  context HOLE(_:Exps)
    when getKLabel(HOLE) ==K #klabel(`_(_)`) orBool getKLabel(HOLE) ==K #klabel(`_[_]`)
  */

  rule <k> (lookup(L) => V)(_:Exps) ...</k>  <store>... L |-> V:Val ...</store>
    [group(lookup)]

  rule objectClosure(_, ListItem(envStackFrame(C,_)) _)
       instanceOf C => true

  rule objectClosure(_, (ListItem(envStackFrame(C,_)) => .List) _)
       instanceOf C'  when C =/=K C'
//TODO: remove the sort cast ::Id of C above, when sort inference bug fixed

  rule objectClosure(_, .List) instanceOf _ => false

  rule (C) objectClosure(_ , EnvStack) => objectClosure(C ,EnvStack)

    rule <k> lvalue(X:Id => this . X) ...</k>  <env> Env </env>
    when notBool(X in keys(Env))

  context lvalue((HOLE . _)::Exp)

/*  rule lvalue(objectClosure(<crntClass> C </crntClass>
                            <envStack>... envStackFrame(C,EnvC) EStack </envStack>)
              . X
              => lookupMember(<envStack> envStackFrame(C,EnvC) EStack </envStack>,
                              X))  */
  rule lvalue(objectClosure(Class, ListItem(envStackFrame(Class,Env)) EStack)
              . X
              => lookupMember(ListItem(envStackFrame(Class,Env)) EStack,
                              X))
  rule lvalue(objectClosure(Class, (ListItem(envStackFrame(Class':Id,_)) => .List) _)
              . _X)
    when Class =/=K Class'

  // TODO(KORE): clarify sort inferences #1803
  syntax Exp ::= lookupMember(List, Id)  [function]
  /*
  syntax KItem ::= lookupMember(EnvStackCell,Id)  [function]
  */

//  rule lookupMember(<envStack> envStackFrame(_, <env>... X|->L ...</env>) ...</envStack>, X)
//    => lookup(L)
  rule lookupMember(ListItem(envStackFrame(_, X|->L _)) _, X)
    => lookup(L)

//  rule lookupMember(<envStack> envStackFrame(_, <env> Env </env>) => .List ...</envStack>, X)
//    when notBool(X in keys(Env))
  rule lookupMember(ListItem(envStackFrame(_, Env)) Rest, X) =>
       lookupMember(Rest, X)
    when notBool(X in keys(Env))
//TODO: beautify the above

endmodule
