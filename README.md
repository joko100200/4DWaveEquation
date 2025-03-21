## Description
This code uses a uniform 4D mesh to integrate the klein-gordon equation. Using finite diffrence to solve: <br>

![equation](https://latex.codecogs.com/gif.image?%5Csmall%20%5Cdpi%7B120%7D%5Cbg%7Bwhite%7D%5CBox%5Cpsi-m%5E2%5Cpsi%5E2=4%5Cpi%5Crho%20) <br>

Then this equation is then split into two for propagation: <br>

![equation](https://latex.codecogs.com/gif.image?%5Csmall%20%5Cdpi%7B120%7D%5Cbg%7Bwhite%7D%5Cbegin%7Balign%7D%5Cpartial_t%5Cpsi=-%5Ckappa%5C%5C%5Cpartial_t%5Ckappa=-%5Cnabla%5E2%5Cpsi&plus;m%5E2%5Cpsi%5E2&plus;4%5Cpi%5Crho%5Cend%7Balign%7D) <br>

This is the two equations we propagate in the code.

## Code parameters
For an explination of all variables needed to input into the code look in the top of the wavefunction code.
