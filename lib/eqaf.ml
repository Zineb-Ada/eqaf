let[@inline] get x i = String.unsafe_get x i |> Char.code

(* get:
     sarq $1, %rbx
     movzbq (%rax, %rbx), %rax
     leaq 1(%rax,%rax), %rax
     ret

   XXX(dinosaure): we use [unsafe_get] to avoid jump to exception.
*)

let[@inline] min (a:int) b = if a < b then a else b

(* min:
     cmpq  %rbx, %rax
     jge   .L101
     ret
   .L101
     movq  %rbx, %rax
     ret

   XXX(dinosaure): we rewrite [min] to avoid a call to [camlStdlib__min].
   flambda can inline it by:

     ((min[@inlined]) : int -> int -> int)

     // [String.length a] on %rsi
     movq   %rsi, 16(%rsp)
     // [String.length b] on %rdi
     movq   %rdi, 24(%rsp)
     movq   caml_lessequal@GOTPCREL(%rip), %rax
     call   caml_c_call@PLT
*)

let equal a b =
  let ln = min (String.length a) (String.length b) in
  (* movq   -8(%rbx), %rdi
     shrq   $10, %rdi
     leaq   -1(,%rdi,8), %rdi
     movzbq (%rbx,%rdi), %rsi
     subq   %rsi, %rdi
     leaq   1(%rdi,%rdi),%rsi

     // [String.length a]

     movq   -8(%rax), %rdi
     shrq   $10,%rdi
     leaq   -1(,%rdi,8), %rdi
     movzbq (%rax,%rdi), %rdx
     subq   %rdx,%rdi
     leaq   1(%rdi,%rdi), %rdi

     // [String.length b]

     cmpq   %rsi, %rdi
     jge    .L105
     movq   %rdi, %rsi
     .L105:
     movq   $1, %rdx
     movq   $1, %rdi

     // [min] inlined (without flambda)
  *)
  let r = ref 0 in
  let i = ref 0 in
  (* .L104
     cmpq   %rsi,%rdi
     jge    .L103     // end of loop
     movq   %rdi, %rcx

     sarq   $1,%rcx
     movzbq (%rbx,%rcx), %r8
     leaq   1(%r8,%r8), %r8
     movzbq (%rax,%rcx), %rcx
     leaq   1(%rcx,%rcx), %rcx

     // [get a !i] & [get b !i]

     xorq   %r8, %rcx
     orq    $1,%rcx
     orq    %rcx, %rdx
     addq   $2, %rdi
     jmp   .L104
  *)
  while !i < ln do r := !r lor (get a !i lxor get b !i) ; incr i done ;
  (* // [String.length a] in %rdi
     // [String.lenght b] in %rbx
     // [r] in %rdx

     subq   %rdi, %rbx
     incq   %rbx
     orq    %rdx, %rbx
  *)
  let r = ((String.length a lxor ln) lor (String.length b lxor ln)) lor !r in
  (* cmpq   $1, %rbx
     sete   %al
     movzbq %al, %rax
     leaq   1(%rax,%rax), %rax
     ret
  *)
  r = 0
