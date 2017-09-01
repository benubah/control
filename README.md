# Control Systems Toolbox for R Language

The Control Systems Toolbox is an R library that implements basic operations for the design and analysis of control systems.

This library started as a [Google Summer of Code project (2017)](https://summerofcode.withgoogle.com/projects/#5201658249216000) and was mentored by  [Dr. M. Kostrun](https://github.com/mkostrun) and [Hans W Borchers](https://github.com/hwborchers)


## Installation

The package may be installed from this GitHub repository using `devtools` in the following manner:

`install.packages("devtools")  # if not already installed on your PC`

`devtools::install_github("benubah/control")`


## Features

- Model Creation: Transfer function(tf), State-space(ss) and Zero-pole-gain (zpk) models; Transfer function Expressions
- Model Conversion: tf2ss, tf2zp, ss2tf, ss2zp, zp2tf, zp2ss, tfdata, ssdata, zpkdata
- System Interconnection: append, serial, parallel, feedback, connect
- Analysis: ctrb, obsv, pole, damp, dcgain
- Design: acker, care, pid, place, lqr
- Time Response: gensig, ltitr, lsim, initial, impulse, ramp, step
- Frequency Response: ltifr, freqresp, bode and nyquist
- Plotting: lsimplot, initialplot, impulseplot, rampplot, stepplot, bodeplot, nyquistplot
- Others: c2d, ode2ss

Most fundamental features of this toolbox were developed during the Google Summer of Code 2017 and a list of functions and activities produced during this summer could be found [here](https://github.com/benubah/controldev/blob/master/README.md).

[Google Summer of Code project (2017)](https://summerofcode.withgoogle.com/projects/#5201658249216000) related articles demonstrating the usage of this toolbox are underlisted:

- [Control Systems Toolbox in R - a GSoC 2017 Project](https://rviews.rstudio.com/2017/07/06/control-systems-toolbox-in-r---a-gsoc-2017-project/)
- [Control Systems Toolbox – System Interconnection](https://rviews.rstudio.com/2017/08/24/control-systems-toolbox/)

## Documentation

All callable functions (including utilities) are documented and could be accessed within the R environment after installing the toolbox.

A web-browser readable format of the documentation could be found here: https://rdrr.io/github/benubah/control/api/


