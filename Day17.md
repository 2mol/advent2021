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

failure cases:

1. shot is **short**: it stagnates before x gets to txmin
2. shot is **long**: it stagnates before y gets past tymax, but x has stagnated after txmax
3. shot **undershoots**: y smaller than tymin but x has not yet stagnated
4. shot **overshoots**: x past txmax, no stagnation
5. shot **shoots through**: trajectory intersects the target, but doesn't overlap

1. -> increase initial vx
2. -> derease initial vx
3. -> increase initial vy?
4. -> decrease vx
5. is the tricky one!
