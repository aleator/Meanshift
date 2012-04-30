# Intro

This package details a basic version of the meanshift algorithm for
feature-space analysis. Mean shifting is an iterative process with
fixed points that correspond to 
modes of kernel density estimate performed
with the same bandwidth (first parameter). This 
can be used to, for example, to partition the data by
determining which fixed point each of the samples belongs to.

# Examples
The most basic usage example is:

 > fixedPointE 0.001 (meanShift 0.1 points) (V.fromList [1,1,1])

This code is somewhat slow, but can be used on reasonably large datasets,
such as images if waiting is an option:

![Using meanshift for noise reduction](Examples/meanshift-filter.png)

