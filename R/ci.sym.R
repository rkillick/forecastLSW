ci.sym <-
function (x,y, xdel=0.2, ydel=0.2, col=1) 
{

xc <- c(x - xdel, x, x+xdel)
yc <- c(y-ydel, y, y-ydel)

lines(xc, yc, col=col)


}
