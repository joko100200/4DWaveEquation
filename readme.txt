Here we will attempt to combine the manifold code and the 4dspecial code into one.

This code will be one step away from a true GR solver. Since this code will be fed a function for how we want the 4d flat feild to become a 4d curved feild and to see how that looks like with geodesics and all.

Once we get this code working we can replace the part of the code that transforms the space into the actual metric solver then we can just get the metric at every point bingo bango GR solver

Unlike our other codes the metric will not need to be solved using finite diffrence of the space
It will be solved directly from the GR Differential equations there will be no need to output all
the points in space like we did with the last 2







We are done with the main block of code now you must do the animation part which you need to figure out how to get the animation working on gnuplot we know that this works I don't know how you are going to do it tho good luck soldier :)))

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

We were able to get the animation to work animation.gnuplot however I would like to see it work with pm3d but this would require us to find out how to tell gnuplot to pick t2 number of blocks and we would need to add spacing after every y change to the graph. You got this tho king

The simulation is unstable and only works correctly for the first few time steps. then it fucks off to all hell. So we need to find a way to show it's deviation from accuracy as well as to write down how it should work.

see if there is a volume integral condition that this graph must meet
