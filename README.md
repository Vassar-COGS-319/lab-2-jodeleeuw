# Lab 2: Monte Carlo Methods in R, Schelling's Model

## Monte Carlo Methods

> Any method which solves a problem by generating suitable random numbers and observing that fraction of the numbers obeying some property or properties. The method is useful for obtaining numerical solutions to problems which are too complicated to solve analytically. It was named by S. Ulam, who in 1946 became the first mathematician to dignify this approach with a name, in honor of a relative having a propensity to gamble (Hoffman 1998, p. 239). Nicolas Metropolis also made important contributions to the development of such methods.
>
> - http://mathworld.wolfram.com/MonteCarloMethod.html

To complete this lab, start with the `tutorial.R` file. This will introduce you to a few new R techniques and methods that will be useful for Monte Carlo simulations. After you complete the tutorial, there are two challenges to solve, `1-birthday-problem.R` and `2-hot-potato-problem.R`. I recommend doing them in order, but you are under no obligation to do so.

## Schelling's Model

We will implement Schelling's model of segregation. Most of the implementation has already been done for you, in the `schelling-model.R` file. We'll go over this implementation in class, and collectively write the final piece of the model.

After we implement Schelling's version, it'll be up to you to come up with a variation on the model and make the changes necessary to explore that variation. This variation should explore how segregation does or does not change when compared to the base Schelling model. You'll want to run a range of parameter values and see if there are differences in the equilibrium points or the speed with which the model reaches equilibrium. You'll do this work in the `my-variation.R` file. You should use comments in this file to narrate what you are doing and what you are learning.

Some ideas for variations:

- Expand the concept of a neighborhood, so that agents are looking not just at the immediate neighbors, but at some larger section of the grid.
- Add some additional characteristic of agents, that may or may not vary across groups, to capture some other aspect of the real world that is not captured in the original model.
- Allow agents to vary in their preferences.
- Explore what happens if agents seek out diversity instead of merely avoiding being the minority.
