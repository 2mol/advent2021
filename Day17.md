observarions
------------

- takes a sec to realize why we can't increase y arbitrarily high.
- if the shot stagnates (x = 0) before the target, then increasing y won't help.
    - because that just _increases_ the number of steps, so the stagnation happens even earlier.
- maybe we can budget x, i.e. find a _minimum_.
    - it's not fully independent, but yes, for a given number of steps, x will decrease n_steps times.
- maybe we don't need to solve by precise iteration, if only we can narrow down `x_range` and `y_range` to the point where trying all the solutions is fine.
- bla bla automatic differentiation would be cool to know right now.

strategy
--------

```
type FailureMode =
  | StagnateLeft
  | StagnateRight
  | OvershootLeft
  | OvershootRight
```
