# Intro

This package details a basic version of the meanshift algorithm for
feature-space analysis. Mean shifting is an iterative process with
fixed points that correspond to 
modes of kernel density estimate performed
with the same bandwidth (first parameter). This 
can be used to, for example, to partition the data by
determining which fixed point each of the samples belongs to.

# Of Code Quality and Examples

There is an example in the examples directory.

This code is somewhat slow, but can be used on reasonably large datasets,
such as images if waiting is an option:

![Using meanshift for noise reduction](http://github.com/aleator/Meanshift/raw/master/Examples/meanshift-filter.png)

