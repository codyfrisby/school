#Code for Chapter 8: odes

# Path of batted ball using Euler

# Establish base units and unit conversions
kg. = 1; s. = 1; m. = 1
lb. = 0.4536*kg.; oz. = 1/16*lb.; hr. = 3600*s.
ft. = 0.3048*m.; mile. = 5280*ft.; in. = 1/12*ft.; degree = pi/180

# Convert various quantities to SI units
v0.ball = 130*mile./hr. ; v0 = v0.ball/(m./s.)
diam.ball = 2.9*in. ; diam = diam.ball/m.
area = pi*(diam/2)^2
air.density = 0.077*lb./ft.^3 ; dens = air.density/(kg./m.^3) 
mass.ball = 5.1*oz.; mass = mass.ball/kg.
g = 9.806*m./s.^2
Cdrag = 0.4
C = 0.5*dens*area*Cdrag/mass

# Set initial conditions
x0=0 #homeplate
y0 = 4*ft. # a high fastball
theta0 = 30*degree # angle off the bat
vx0 = v0*cos(theta0) # initial velocity in x-direction
vy0 = v0*sin(theta0) # initial velocity in y-direction

# Initialize vectors with initial conditions
x = x0; y = y0; vx = vx0; vy= vy0; v = v0

t0 = 0 # Start the stopwatch running
sec = t0
dt = 0.1 # seconds
i=1

# Solve equations of motion using Euler method
# Stop when the ball hits the ground (y < 0)
while (y[i] >= 0) {
  dx = vx[i]*dt # increment in x
  dy = vy[i]*dt # increment in y
  v[i] = sqrt(vx[i]^2 + vy[i]^2) # total speed
  dvx = -C*v[i]*vx[i]*dt # air resistance
  dvy= (-g - C*v[i]*vy[i])*dt # gravity + air resistance
  x[i+1] = x[i] + dx
  y[i+1] = y[i] + dy
  vx[i+1] = vx[i] + dvx
  vy[i+1] = vy[i] + dvy
  v[i+1]  = sqrt(vx[i+1]^2 + vy[i+1]^2)
  sec[i+1] = sec[i] + dt
  i=i+1
}

# Tabulate results for x and y coordinates and velocity 
# as functions of time
x = round(x/ft.,0)
y = round(y/ft.,0)
v = round(v/(ft./s.),1)
mat = cbind(sec, x,y,v)
colnames(mat) = c("t/s", "x/ft", "y/ft", "v/(ft/s)")
n = length(sec)
show = seq(1,n,2) # every other time step

mat[show,]

#############################

# Earth in circular orbit around Sun
# Euler method
G = 3600^2*6.673e-20
Msun = 1.989e30
GM = G*Msun

x0 = 149.6e6; vx0 = 0
y0 = 0; vy0 = 29.786*3600
tmin = 0; tmax = 8800; dt = 1
hrs = seq(tmin, tmax, dt)
n = (tmax - tmin)/dt + 1
x = x0; vx = vx0; y = y0; vy = vy0
for(i in 2:n) {
  dx = vx[i-1]*dt
  dvx = -GM*x[i-1]/(x[i-1]^2 + y[i-1]^2)^1.5*dt
  dy = vy[i-1]*dt
  dvy = -GM*y[i-1]/(x[i-1]^2 + y[i-1]^2)^1.5*dt
  x[i] = x[i-1] + dx
  vx[i] = vx[i-1] + dvx
  y[i] = y[i-1] + dy
  vy[i] = vy[i-1] + dvy
}
r = round(sqrt(x^2 + y^2)*1e-8,3)
v = round(sqrt(vx^2 + vy^2)/3600,3)
x = round(x*1e-8,3)
y = round(y*1e-8,3)

show = seq(1,n,400)

hrsdisp = hrs[show]
xdisp = x[show]
ydisp = y[show]
rdisp = r[show]
vdisp = v[show]

options(digits=5)
mat = cbind(hrsdisp,xdisp,ydisp,rdisp,vdisp)
colnames(mat) = c("hrs", "x/1e8 km", "y/1e8 km", 
                  "r/1e8 km", "v km/ s")
mat

##################################

# Improved Euler for population decay (Fig 8.1)

p0 = 100 # initial population value
k = 1 # rate parameter
tmin=0;tmax=5;dt=0.1 # Beginning and end times and increment
time = seq(tmin,tmax,dt) # vector of times  
n = (tmax-tmin)/dt + 1 # number of evaluations of p
p = p0 # initialize p vector
for (i in 2:n) {
  dpa = -k*p[i-1]*dt
  pa = p[i-1] + dpa
  dpb = -k*pa*dt
  dp = (dpa + dpb)/2
  p[i] = p[i-1] + dp
}
par(mar=c(4,4,1.5,1.5),mex=.8,mgp=c(2,.5,0),tcl=0.3) 
plot(time, p, type = "l")

##################################

# Improved Euler for circular orbit

G = 3600^2*6.673e-20
Msun = 1.989e30
GM = G*Msun
x0 = 149.6e6; vx0 = 0
y0 = 0; vy0 = 29.786*3600
tmin = 0; tmax = 8800; dt = 10
hrs = seq(tmin, tmax, dt)
n = (tmax - tmin)/dt + 1
x = x0; vx = vx0; y = y0; vy = vy0
for(i in 2:n) {
  dxa = vx[i-1]*dt
  dvxa = -GM*x[i-1]/(x[i-1]^2 + y[i-1]^2)^1.5*dt
  dya = vy[i-1]*dt
  dvya = -GM*y[i-1]/(x[i-1]^2 + y[i-1]^2)^1.5*dt
  
  xa = x[i-1] + dxa
  vxa = vx[i-1] + dvxa
  ya = y[i-1] + dya
  vya = vy[i-1] + dvya
  
  dxb = vxa*dt
  dvxb = -GM*xa/(xa^2 + ya^2)^1.5*dt
  dyb = vya*dt
  dvyb = -GM*ya/(xa^2 + ya^2)^1.5*dt
  
  dx = (dxa + dxb)/2
  dvx = (dvxa + dvxb)/2
  dy = (dya + dyb)/2
  dvy = (dvya + dvyb)/2
  
  x[i] = x[i-1] + dx
  vx[i] = vx[i-1] + dvx
  y[i] = y[i-1] + dy
  vy[i] = vy[i-1] + dvy
}
r = round(sqrt(x^2 + y^2)*1e-8,3)
v = round(sqrt(vx^2 + vy^2)/3600,3)
x = round(x*1e-8,3)
y = round(y*1e-8,3)
show = seq(1,n,40)
hrsdisp = hrs[show]
xdisp = x[show]
ydisp = y[show]
rdisp = r[show]
vdisp = v[show]
options(digits=5)
mat = cbind(hrsdisp,xdisp,ydisp,rdisp,vdisp)
colnames(mat) = c("hrs", "x/1e8 km", "y/1e8 km", 
                  "r/1e8 km", "v km/ s")
mat

##########################

# Circular orbit with ode

require(deSolve)

# Compute parameter
G = 3600^2*6.673e-20
Msun = 1.989e30
GM = G*Msun
parms = GM

# Initialize variables
x0 = 149.6e6; vx0 = 0
y0 = 0; vy0 = 29.786*3600

# Set time sequence
tmin = 0; tmax = 8800; dt = 400
hrs = seq(tmin, tmax, dt)

# Define function that computes derivatives
orbit = function(t,y,GM) {
  dy1 = y[2]
  dy2 = -GM*y[1]/(y[1]^2+y[3]^2)^1.5
  dy3 = y[4]
  dy4 = -GM*y[3]/(y[1]^2+y[3]^2)^1.5
  return(list(c(dy1,dy2,dy3,dy4)))
}

# Call ode function to solve system of equations
# Arguments to the function are specified by position
out = ode(c(x0, vx0, y0, vy0), hrs, orbit, parms)

# Display results
options(digits=5)
hrs = out[,1]; x = out[,2]; vx = out[,3]
y = out[,4]; vy = out[,5] 
r = round(sqrt(x^2 + y^2)*1e-8,3)
v = round(sqrt(vx^2 + vy^2)/3600,3)
mat = cbind(hrs,x,y,r,v)
colnames(mat) = c("hrs", "x km", "y km", 
                  "r/1e8 km", "v km/s")
mat


###################################

# Bessel differential eq (Fig 8.2)

require(deSolve)

diffeqs = function(x,y,nu) {
  J=y[1]
  dJdx = y[2]
  with(as.list(parms), {
    dJ = dJdx
    ddJdx = -1/x^2*(x*dJdx + (x^2-nu^2)*J)
    res = c(dJ, ddJdx)
    list(res)
  })
}

# Time steps
xmin = 1e-15 # Don't start exactly at zero, to avoid infinity
xmax = 15
dx = 0.1
xx = seq(xmin, xmax, dx)

# Parameters
parms = c(nu = 1) # Bessel equation of order 1

# Initial values
y0 = c(J = 0, dJdx = 1)

# Solve with lsoda
out = lsoda(y0, xx, diffeqs, parms)

# Plot results
par(mar=c(4,4,1.5,1.5),mex=.8,mgp=c(2,.5,0),tcl=0.3) 
xx = out[,1]; J = out[,2]; dJdx = out[,3]
plot(xx, J, type="l"); curve(besselJ(x,1),0,15,add=T); abline(0,0)


###################################

# Oscillating Chemical Reaction (for Fig. 8.3)

require(deSolve)

# Reaction mechanism
diffeqs = function(t,x,parms) {
  X=x[1]
  Y=x[2] 
  with(as.list(parms), {
    dX = k1*A - k2*X - k3*X*Y^2
    dY = k2*X + k3*X*Y^2 - k4*Y
    list(c(dX, dY))
  })}

# Time steps
tmin = 0; tmax = 200; dt = 0.01
times = seq(tmin, tmax, dt)

# Parameters
parms = c(k1 = 0.01, k2 = 0.01, k3 = 1e6, k4 = 1, A = 0.05)

# Initial values
x0 = c(X = 0, Y = 0)

# Solve with adams method
out = ode(x0, times, diffeqs, parms, method = "adams")

# Plot results
par(mar=c(4,4,1.5,1.5),mex=.8,mgp=c(2,.5,0),tcl=0.3) 
time = out[,1]; X = out[,2]; Y = out[,3]

par(mfrow = c(1,3))
plot(time, X, type="l") # Time variation of X
plot(time, Y, type="l") # Time variation of Y
plot(X,Y, type="l")  # Phase plot of X vs Y
par(mfrow = c(1,1))

###################################

# Stiff systems

require(deSolve)

diffeqs = function(x,y,p) { # model equations
  y1=y[1];y2=y[2];y3=y[3]
  dy1 = -0.1*y1 - 49.9*y2
  dy2 = -50*y2
  dy3 = 70*y2 - 120*y3
  list(c(dy1, dy2, dy3))
}

# Time steps
xmin=0;xmax=2;dx=0.1
x = seq(xmin, xmax, dx)

# Initial values
y0 = c(2,1,2)

# Solve with bdf
out = ode(y0, x, diffeqs, parms = NULL, method = "bdf") 
mat = cbind(out[,1], out[,2], out[,3], out[,4])
mat[nrow(mat),]


###################################

# Matrix exponential solution

m = matrix(c(-0.1,-49.9,0,0,-50,0,0,70,-120), nrow=3, byrow=TRUE)
lam = c(); expt = c(); A = 0
time = 2
Ev = eigen(m)$vectors
c0 = c(2,1,2) # initial concentrations
w = solve(Ev, c0)
for (i in 1:nrow(m)) {
  lam[i] = eigen(m)$values[i]
  A = A + w[i]*Ev[1,i]*exp(lam[i]*time) 
}
A

###################################

# Code for event-driven RC circuit, to generate Fig. 8_4

require(deSolve)
tmin=0; tmax=4; dt=.01 #millisec 
times = seq(tmin, tmax, dt)
# Forcing function
pulse = c(rep(1,1/dt), rep(0,1/dt), rep(1,1/dt), rep(0,1/dt+1))
sqw = cbind(times, pulse)

# the forcing functions; rule = 2 avoids NaNs in interpolation
SqWave = approxfun(x = sqw[,1], y = sqw[,2], 
                   method = "linear", rule = 2)

# RC circuit equation for dV/dt
voltage = function( t, V, RC) list (c(SqWave(t) - V/RC))

parms = c(RC = 0.6) # millisec
V0 = 0 # Initial condition

Out = lsoda(y = V0, times = times, func = voltage,  parms = parms)
par(mar=c(4,4,1.5,1.5),mex=.8,mgp=c(2,.5,0),tcl=0.3) 
plot(Out[,1], Out[,2], type = "l", ylim = c(0,1), xlab = "msec", 
     ylab = "V")
lines(times, pulse)

###################################
# Accompanies Fig 8.5

# Drug metabolism kinetic equations
diffeqs = function(t,y,parms) {
  #A = y[1]
  #B = y[2]
  with(as.list(parms), {
    dy1 = - kf1*y[1] + kr1*y[2]
    dy2 = kf1*y[1] - kr1*y[2] - kf2*y[2]
    return(list(c(dy1,dy2)))
  })
}

## event triggered if B = 1
rootfun = function (t, y, parms) y[2] -1

## sets A = 2
eventfun = function(t, y, parms) {
  y[1] = 2
  return(y)
}

# Time steps
tmin = 0; tmax = 10; dt = 0.1
times = seq(tmin, tmax, dt)

# Parameters: values of rate constants
parms = c(kf1 = 1, kr1 = 0.1, kf2 = 1)

# Initial values of A and B
y0 = c(3,2)

# Solve with lsoda
out = ode(times = times, y = y0, func = diffeqs, parms = parms,
          events = list(func = eventfun, root = TRUE),
          rootfun = rootfun)

# Plot results
par(mar=c(4,4,1.5,1.5),mex=.8,mgp=c(2,.5,0),tcl=0.3) 
# par(mfrow = c(1,2))
time = out[,1]; A = out[,2]; B = out[,3]
plot(time, A, type="l", ylim = c(0,3), ylab = "conc")
lines(time, B, lty = 2)
legend("topright", legend = c("A","B"), lty = 1:2, bty="n")

###################################

# Code for Lotka-Volterra simulation

require(deSolve)

# Population dynamics
diffeqs = function(t,x,parms) {
  prey = x[1]
  pred = x[2]
  with(as.list(parms), {
    dprey = -k1*pred*prey + k2*prey
    dpred = k3*pred*prey - k4*pred
    res = c(dprey, dpred)
    list(res)
  })
}

# Time steps
tmin = 0; tmax = 200; dt = 1
times = seq(tmin, tmax, dt)

# Parameters
parms = c(k1 = 0.01, k2 = 0.1, k3 = 0.001, k4 = 0.05)

# Initial values
x0 = c(prey = 50, pred = 15)

# Solve with lsoda
out = lsoda(x0, times, diffeqs, parms, rtol = 1e-6, atol = 1e-6)

# Plot results
par(mar=c(4,4,1.5,1.5),mex=.8,mgp=c(2,.5,0),tcl=0.3) 
par(mfrow = c(1,2))
time = out[,1]; prey = out[,2]; pred = out[,3]
plot(time, prey, type="l", ylim = c(0,100))
lines(time, pred, lty = 2)
legend("topleft", legend = c("prey","pred"), lty = 1:2, bty="n")
plot(prey, pred, type = "l")
par(mfrow = c(1,1))


###################################

# Perturbed Lotka-Volterra, generating Fig 8.7

require(deSolve)

# Population dynamics
diffeqs = function(t,x,parms) {
  prey = x[1]
  pred = x[2]
  with(as.list(parms), {
    dprey = -k1*pred*prey + k2*prey
    dpred = k3*pred*prey - k4*pred
    res = c(dprey, dpred)
    list(res)
  })
}

eventdat = data.frame(var = "pred", time = 50, value = 2, method = "add")

# Time steps
tmin = 0; tmax = 200; dt = 1
times = seq(tmin, tmax, dt)

# Parameters
parms = c(k1 = 0.01, k2 = 0.1, k3 = 0.001, k4 = 0.05)



# Initial values
x0 = c(prey = 50, pred = 15)

# Solve with lsoda
out = lsoda(x0, times, diffeqs, parms, events = list(data =
                                                       eventdat))

# Plot results
par(mar=c(4,4,1.5,1.5),mex=.8,mgp=c(2,.5,0),tcl=0.3) 
par(mfrow = c(1,2))
time = out[,1]; prey = out[,2]; pred = out[,3]
plot(time, prey, type="l", ylim = c(0,100))
lines(time, pred, lty = 2)
legend("topleft", legend = c("prey","pred"), lty = 1:2, bty="n")
plot(prey, pred, type = "l")
par(mfrow = c(1,1))


########################

# Difference equation example
require(deSolve)
Population = function(t,y,param) {
  y1 = y[1] # 0-12 group population
  y2 = y[2] # 13-40 group population
  y3 = y[3] # 41 and older population
  y1.new = b*y2 + 11/12*y1*(1 - d1)
  y2.new = 1/12*y1*(1 - d1) + 26/27*y2*(1 - d2)
  y3.new = 1/27*y2*(1 - d2) + y3*(1 - d3)
  return(list(c(y1.new, y2.new, y3.new))) 
}
b = 0.5 # Birth rate in 13-40 group
d1 = 0.1 # Death rate of 0-12 group
d2 = 0.1 # Death rate of 13-40 group
d3 = 0.25 # Death rate of 41 and older

y = c(200, 400, 400) # Initial populations in each group
times = 0:50 # Time span

out = ode(func = Population, y = y, times = times, parms = 
            c(b,d1,d2,d3), method = "iteration")
par(mar=c(4,4,1.5,1.5),mex=.8,mgp=c(2,.5,0),tcl=0.3) 
plot(out) # Three separate graphs
# All three age groups on a single graph
matplot(times,out[,2:4], type = "l", lty=1:3, col = rep(1,3), 
        ylab = "Population")
legend("topleft", legend = c("0-12","13-40","40"), 
       bty="n",lty=1:3)
par(mfrow=c(1,1)) # Return to 1 row, 1 column


#############################

# Hutchinson equation

require(deSolve)

# Derivative function for Hutchinson model
func = function(t, y, parms) {
  tlag=t-tau
  if (tlag < 0) dy = 1 else dy = r*y*(1 - lagvalue(tlag)/K)
  return(list(c(dy)))
}
# Initial values and times
yinit = 0
times = 0:100

# Parameters
r = 1; K = 1; tau = 1

# Solutions
yout1 = dede(y = yinit, times = times, func = func, 
             parms = c(r,K, tau))

tau = 3
yout2 = dede(y = yinit, times = times, func = func, 
             parms = c(r,K, tau))

# Plot result
par(mar=c(4,4,1.5,1.5),mex=.8,mgp=c(2,.5,0),tcl=0.3) 
plot(yout1,yout2, type = "l", main="Comparison of lag times", 
     col = rep(1,2), ylim = c(0,10), ylab = "Population")
legend("topleft", legend = c("1","3"), lty = c(1,3), bty="n")

############################

# System of DDEs with two dependent variables

require(deSolve)

# Create a function to return derivatives
derivs = function(t,y,parms) {
  if (t < tau)
    lag = yinit
  else
    lag = lagvalue(t - tau)
  dy1 = a * y[1] - (y[1]^3/3) + m*(lag[1] - y[1])
  dy2 = y[1] - y[2]
  return(list(c(dy1,dy2)))
}

# Define initial values, parameters, and time sequence
yinit = c(1,1)
tau = 3; a = 2; m = -5
times = seq(0,30,0.1)

# Solve the dede system
yout = dede(y=yinit,times=times,func=derivs,parms=c(tau,a,m))

# Plot the results
par(mar=c(4,4,1.5,1.5),mex=.8,mgp=c(2,.5,0),tcl=0.3) 
plot(yout[,1], yout[,2], type="l", xlab="t", ylab="y",
     ylim=c(min(yout[,2], yout[,3]), 1.2*max(yout[,2], yout[,3])))
lines(yout[,1], yout[,3], lty=3)
legend("topleft", legend = c("y1", "y2"),lty = c(1,3), bty="n")

##############################

# DAE(1)

require(deSolve)

# Function defining the system

Res_DAE = function (t, y, dy, pars){
  y1=y[1]; y2=y[2]; dy1=dy[1]; dy2=dy[2]
  eq1 = dy1 - y2 - sin(t)
  eq2 = y1 + y2 - 1
  return(list(c(eq1, eq2), c(y1,y2))) 
}

# Time sequence and initial values
times = seq(pi,8,.1)
y = c(y1 = 0.5, y2 = 0.5)
dy = c(dy1 = 0.5, dy2 = -0.5)

# Solution with daspk
DAE = daspk(y = y, dy = dy, times = times,
            res = Res_DAE, parms = NULL, atol = 1e-10, rtol = 1e-10)

# Output and plotting
par(mar=c(4,4,1.5,1.5),mex=.8,mgp=c(2,.5,0),tcl=0.3) 
time = DAE[,1]; y1 = DAE[,2]; y2 = DAE[,3]
matplot(time, cbind(y1, y2), xlab = "time", ylab = "y1, y2", 
        type = "l", lty=c(1,3), col = 1)

legend("topleft", legend = c("y1", "y2"), lty = c(1,3), 
       col = 1, bty = "n")


###############################

# DAE(2)

require(deSolve)

Res_DAE = function (t, y, dy, pars){
  y1 = y[1]; y2 = y[2]; y3 = y[3]
  dy1 = dy[1]; dy2 = dy[2]; dy3 = dy[3]
  eq1 = dy1 - 1/4*(cos(t) - dy2)
  eq2 = dy2 - y3
  eq3 = dy3-y1
  return(list(c(eq1, eq2, eq3), c(y1,y2,y3)))
}

# times and initial values
times = seq(pi,7,.1)
y = c(y1 = -0.25, y2 = 1, y3 = 0)
dy = c(dy1 = -0.25, dy2 = 0, dy3 = -0.25)

DAE = daspk(y = y, dy = dy, times = times,
            res = Res_DAE, parms = NULL, atol = 1e-10, rtol = 1e-10)

time = DAE[,1]; y = DAE[,2]; x = DAE[,3]

par(mar=c(4,4,1.5,1.5),mex=.8,mgp=c(2,.5,0),tcl=0.3) 
matplot(time, cbind(y, x), xlab = "time", ylab = "y, x", 
        type = "l", lty=c(1,3), col = 1)
legend("bottomleft", legend = c("y", "x"), lty = c(1,3), 
       col = 1, bty = "n")

###############################

# Michaelis-Menten
# Approach to steady state
require(deSolve)
enzyme = function(t, state, pars) {
  with (as.list(c(state,pars)), {
    dS = Fin - (Vf/Kf*S - Vr/Kr*P)/(1 + S/Kf + P/Kr)
    dP = (Vf/Kf*S - Vr/Kr*P)/(1 + S/Kf + P/Kr) - Fout
    return(list(c(dS,dP)))}) 
}
pars = list(Fin = 0.1, Fout = 0.1, Vf = 1, Vr = 0.5, Kf = 1, Kr = 2)
out = ode (y = c(S = 1, P = 1), times = 0:50, 
           func = enzyme, parms = pars)
par(mar=c(4,4,1.5,1.5),mex=.8,mgp=c(2,.5,0),tcl=0.3) 
plot(out)

# Calculation of steady state with "runsteady"
require (rootSolve)
ysteady(y = c(S = 1, P = 1), time = c(0,100), func = enzyme, parms =
          pars, method = "runsteady")
ysteady$y
str(ysteady)

# Calculation of steady state with default ("stode")
steady(y = c(S = 1, P = 1), time = 100, func = enzyme, parms = pars)

##############################

# bvpshoot for liquid drop problem
require(bvpSolve)
fun = function(t, y, parms)
{ dy1 = y[2]
dy2 = -(1-y[1])*(1 + dy1^2)^(3/2)
return(list(c(dy1,
              dy2))) }

# initial and final condition; second conditions unknown
init = c(y = 0, dy = NA)
end =c(y=0,dy=NA)

# Solve bvp by shooting method: requires guess for init unknown 
sol = bvpshoot(yini = init, x = seq(-1,1,0.01),
               func = fun, yend = end, parms = NULL, guess = 1)

x = sol[,1]
y = sol[,2]
par(mar=c(4,4,1.5,1.5),mex=.8,mgp=c(2,.5,0),tcl=0.3) 
plot(x,y, type = "l")

###############################

# bvptwp example
require(bvpSolve)
fun = function(t, y, p)
{ dy1 = y[2]
dy2 = p*y[1]^2
return(list(c(dy1,
              dy2))) }

p = 100

# initial and final condition; second conditions unknown 
init = c(y = 1, dy = NA)
end =c(y=NA,dy=0)

# Solve bvp
sol  = bvptwp(yini = init, x = seq(0,1,0.1),
              func = fun, yend = end, parms = p)

x = sol[,1]
y = sol[,2]
par(mar=c(4,4,1.5,1.5),mex=.8,mgp=c(2,.5,0),tcl=0.3) 
plot(x,y, type = "l")

##############################

# bvpcol example

require(bvpSolve)
fun = function(t, y, p)
{ dy1 = y[2]
dy2 = -y[1]
return(list(c(dy1,
              dy2))) }

# initial and final condition; second conditions unknown 
init = c(y = 1, dy = NA)
end =c(y=2,dy=NA)

# Solve bvp
sol = bvpcol(yini = init, x = seq(0,1,0.01),
             func = fun, yend = end, parms = NULL)

x = sol[,1]
y = sol[,2]
par(mar=c(4,4,1.5,1.5),mex=.8,mgp=c(2,.5,0),tcl=0.3)
plot(x,y, type = "l")
# Verify boundary conditions
y[1]

##############################

# S + P = SP continuous

require(deSolve)
binding = function(Time, State, Pars) {
  with(as.list(c(State, Pars)), {
    rate = kf*S*P - kr*SP
    dS = -rate 
    dP = -rate 
    dSP = rate
    return(list(c(dS, dP, dSP)))
  })
}
pars = c(kf = 0.005, kr = 0.15)
yini = c(S = 10, P = 120, SP = 0)
times = seq(0, 10, by = 0.1)
out = ode(yini, times, binding, pars)
time = out[,1]
fractOcc = out[,4]/(out[,2] + out[,4])
par(mar=c(4,4,1.5,1.5),mex=.8,mgp=c(2,.5,0),tcl=0.3) 
plot(time, fractOcc, type = "l")

##############################

# S + P = SP Gillespie direct

require(GillespieSSA)
Loading required package: GillespieSSA
# Reversible binding reaction
# Three species, two reaction channels
# Forward and reverse reactions are separate channels
# S + P --kf--> SP
# SP --kr--> S + P
# Parameters
pars = c(kf = 0.005, kr = 0.15)
# Initial state vector
yini = c(S = 10, P = 120, SP = 0)
# State-change matrix: reactions in columns, species in rows
nu = matrix(c(-1, +1,
              -1, +1,
              1, -1),
            ncol = 2, byrow = TRUE)
# Propensity vector: rate for each channel
a = c("kf*S*P", "kr*SP")
# Final time
tf = 10
simName = "Reversible Binding Reaction" 
# Direct method
set.seed(1)
out = ssa(yini,a,nu,pars,tf,method="D",
          simName,verbose=TRUE,consoleInterval=1)
par(mar=c(4,4,4,1.5),mex=.8,mgp=c(2,.5,0),tcl=0.3) 
ssa.plot(out)

##############################

# S + P = SP Comparisons 

par(mfrow = c(2,2)) # Prepare for four plots
par(mar=c(4,4,1.5,1.5),mex=.8,mgp=c(2,.5,0),tcl=0.3) 

# Direct method
set.seed(1)
out = ssa(yini,a,nu,pars,tf,method="D",simName,
          verbose=FALSE,consoleInterval=1)
et = as.character(round(out$stats$elapsedWallTime,4)) #elapsed time
time = out$data[,1]
fractOcc = out$data[,4]/(out$data[,2] + out$data[,4])
plot(time, fractOcc, pch = 16, cex = 0.5, 
     main = paste("D ",et, " s"))

# Explict tau-leap method
set.seed(1)
out = ssa(yini,a,nu,pars,tf,method="ETL",simName,
          tau=0.003,verbose=FALSE,consoleInterval=1)
et = as.character(round(out$stats$elapsedWallTime,4)) #elapsed time
time = out$data[,1]
fractOcc = out$data[,4]/(out$data[,2] + out$data[,4])
plot(time, fractOcc, pch = 16, cex = 0.5, 
     main = paste("ETL ",et, "s"))

# Binomial tau-leap method
set.seed(1)
out = ssa(yini,a,nu,pars,tf,method="BTL",simName,
          verbose=FALSE,consoleInterval=1)
et = as.character(round(out$stats$elapsedWallTime,4)) #elapsed time
time = out$data[,1]
fractOcc = out$data[,4]/(out$data[,2] + out$data[,4])
plot(time, fractOcc, pch = 16, cex = 0.5, 
     main = paste("BTL ",et, "s"))

# Optimized tau-leap method
set.seed(1)
out = ssa(yini,a,nu,pars,tf,method="OTL",simName,
          verbose=FALSE,consoleInterval=1)
et = as.character(round(out$stats$elapsedWallTime,4)) #elapsed time
time = out$data[,1]
fractOcc = out$data[,4]/(out$data[,2] + out$data[,4])
plot(time, fractOcc, pch = 16, cex = 0.5, 
     main = paste("OTL ",et, "s"))

###############################

## DNA potential - cell model

require(bvpSolve)
fun = function(z,phi,parms) {
  dphi1 = phi[2]
  dphi2 = -1/2*exp(2*z)*(exp(-phi[1])-exp(phi[1]))
  return(list(c(dphi1,dphi2)))
}
init = c(phi=NA, dphi = 0.84)
end = c(phi=NA, dphi = 0)

sol = bvptwp(yini = init, x = seq(1,3,len=100), func=fun, yend = end)
## bvpcol gives the same result, but bvpshoot fails

z = sol[,1]
phi = sol[,2]
par(mar=c(4,4,1.5,1.5),mex=.8,mgp=c(2,.5,0),tcl=0.3) 
plot(z,phi,type="l", ylab=expression(phi))

###############################

# Shuttle launch trajectory

require(deSolve)

# Parameters
m0 = 2.04e6
burn.rate = 9800
R = 6371
thrust = 28.6e6
dens0 = 1.2
A = 100
Cdrag = 0.3
eps = 0.007
g = 9.8

# Equations of motion
launch = function(t, y,parms) {	
  xpos = y[1]
  xvel = y[2]
  ypos = y[3]
  yvel = y[4]
  airdens = dens0*exp(-ypos/8000)
  drag = 0.5*airdens*A*Cdrag*(xvel^2 + yvel^2)
  m = m0-burn.rate*t
  angle = eps*t
  grav = g*(R/(R+ypos/1000))^2
  xaccel = (thrust - drag)/m*sin(angle)
  yaccel = (thrust - drag)/m*cos(angle) - grav
  list(c(xvel, xaccel, yvel, yaccel))
}

# Initial values
init = c(0,0,0,0)

# Times
times = 0:120

# Solve with Adams method
out = ode(init, times, launch, parms=NULL, method="adams")

# Plot results
par(mar=c(4,4,1.5,1.5),mex=.8,mgp=c(2,.5,0),tcl=0.3) 
time = out[,1]; x = out[,2]; y = out[,4]
plot(x/1000,y/1000, cex=0.5, xlab="x/km", ylab="y/km")

###################################

## Bifurcation analysis of Lotka-Volterra system

## Thomas Petzoldt, R as a Simulation Platform in Ecological Modelling
## http://www.r-project.org/doc/Rnews/Rnews_2003-3.pdf, pp. 8--16

library(deSolve)
library(scatterplot3d)
f = function(x, y, k){x*y / (1+k*x)} #Holling II
model = function(t, xx, parms) {
  u = xx[1]
  v = xx[2]
  w = xx[3]
  with(as.list(parms),{
    du = a * u - alpha1 * f(u, v, k1)
    dv = -b * v + alpha1 * f(u, v, k1) +
      - alpha2 * f(v, w, k2)
    dw = -c * (w - wstar) + alpha2 * f(v, w, k2)
    list(c(du, dv, dw))
  })
}

times = seq(0, 200, 0.1)
parms = c(a=1, b=1, c=10,
          alpha1=0.2, alpha2=1,
          k1=0.05, k2=0, wstar=0.006)
xstart = c(u=10, v=5, w=0.1)

out = as.data.frame(lsoda(xstart, times,
                          model, parms))

par(mfrow=c(2,2))
par(mar=c(4,4,1.5,1.5),mex=.8,mgp=c(2,.5,0),tcl=0.3) 
plot(times, out$u, type="l", lty=1)
lines(times, out$v, type="l", lty=2)
plot(times, out$w, type="l", lty=1)
plot(out$w[-1], out$w[-length(out$w)], type="l")
scatterplot3d(out$u, out$v, out$w, type="l")

peaks = function(x) {
  l = length(x)
  xm1 = c(x[-1], x[l])
  xp1 = c(x[1], x[-l])
  x[x > xm1 & x > xp1 | x < xm1 & x < xp1]
}

par(mfrow = c(1,1))
par(mar=c(4,4,1.5,1.5),mex=.8,mgp=c(2,.5,0),tcl=0.3) 
plot(0,0, xlim=c(0,2), ylim=c(0,1.5),
     type="n", xlab="b", ylab="w")
system.time(for (b in seq(0.02,1.8,0.01)) {
  parms["b"] = b
  out = as.data.frame(lsoda(xstart, times,
                            model, parms, hmax=0.1))
  l = length(out$w) %/% 3
  out = out[(2*l):(3*l),]
  p = peaks(out$w)
  l = length(out$w)
  xstart = c(u=out$u[l], v=out$v[l], w=out$w[l])
  points(rep(b, length(p)), p, pch=".")
})

