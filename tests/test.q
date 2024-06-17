data nat =
  | s : nat -> nat
  | z : nat;

match s (s (s z)) with
| s n -> n
| z -> z
end
