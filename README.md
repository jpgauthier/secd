# SECD abstract machine

The SECD machine consists of four stacks: S, E, C, and D. These are called, respectively, the Stack, the Environment, the Control, and the Dump. The purpose of each is roughly as follows:

  * the stack holds the results of computations already performed;
  * the environment is a map from program variables to the terms to be substituted for them;
  * the control holds the current and remaining computations to be performed—its contents “direct” the actions of the machine;
  * the dump is a store for surrounding context when evaluating nested expressions.

The notation ⟨S,E,C,D⟩ (denoting the contents of each of the four stacks) will be used to represent a configuration of the machine. Initially, S, E, and D are empty, and C contains the expression to be reduced.
The behaviour of the SECD machine is defined by the following state transitions:

  * ⟨S, E, x : C′, D⟩ → ⟨lookup(x, E) : S, E, C′, D⟩  -  look up a variable in the environment and place it on the stack
  * ⟨S, E, λx.M : C′, D⟩ → ⟨⟨x, M, E⟩ : S, E, C′, D⟩  —  convert an abstraction into a closure, containing the variable and body of the abstraction, and the current environment; place the closure on the stack
  * ⟨S,E,(MN) : C′,D⟩ → ⟨S,E,N : M : @ : C′,D⟩  —  split up the components of an application and put them on the control separately in reverse order (so that N is evaluated first), followed by a special directive, @, meaning “apply”
  * ⟨S,E,prim : C′,D⟩ → ⟨prim : S,E,C′,D⟩  —  if a primitive value is on the control, then transfer the primitive to the stack unchanged
  * ⟨prim : N : S′,E,@ : C′,D⟩ → ⟨S′,E,prim(N) : C′,D⟩  —  if a primitive function is on top of the stack and an “apply” directive is on the control, then apply the primitive function to the next item on the stack, and put the result on the control in place of them
  * ⟨⟨x,M,E1⟩ : N : S′,E,@ : C′,D⟩ → ⟨(),⟨x,N⟩ : E1,M,⟨S′,E,C′⟩ : D⟩  —  if a closure is on top of the stack and an “apply” directive is on the control, then push a triple consisting of the current stack (minus the top two elements), environment, and remaining control, onto the dump
  * ⟨M : (),E,(),⟨S′,E′,C′⟩ : D′⟩ → ⟨M : S′,E′,C′,D′⟩  —  when a computation is exhausted (control is empty and stack has a single element on it), restore the previous context from the dump, and push the current result onto the top of the restored stack
  * ⟨M : (),E,(),()⟩ → done: return M  —  if the dump is empty when the computation is exhausted, then the machine halts
  * all other configurations are erroneous
