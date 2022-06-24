# TODO: Cannot have return type inference along with recursion.
#fn recursive_infinite() = recursive_infinite()

#fn factorial(n: Int) = {
#  if n == 0 then {
#    return 1;
#  }
#
#  n * factorial(n - 1)
#}

#fn factorial_tail_recursive(n2: Int, acc: Int) = {
#  if n2 == 0 then {
#    return acc;
#  }
#
#  factorial_tail_recursive(n2 - 1, n2 * acc)
#}
