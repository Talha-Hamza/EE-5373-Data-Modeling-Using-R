
# Based on https://en.wikipedia.org/wiki/R_%28programming_language%29
##########################################################
# Define the fractal function
fractal <- function(iters, dim, xlo, xhi, ylo, yhi) {
C <- complex( real=rep(seq(xlo,xhi, length.out=dim), each=dim ),
imag=rep(seq(ylo,yhi, length.out=dim), dim ) )
C <- array(C,c(dim,dim)) # reshape as square matrix of complex numbers
Z <- 0
for (k in 1:iters) {
Z <- Z^7+Z^6+Z^5+Z^4+Z^3+Z^2+Z+C
}
X <- array(0, c(dim,dim))
X <- exp(-abs(Z))
image(X,col=heat.colors(100))
}
##########################################################
##########################################################
# The main program starts here.
#
fractal(iters=20, dim=500, xlo=-1.8, xhi=0.6, ylo=-1.2, yhi=1.2)
