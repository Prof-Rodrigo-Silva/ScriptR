y^4 - x^4 - 4*y^2 + 9*x^2 = 0

x = seq(-10,10,length=1000)
y = seq(-10,10,length=1000)
z = outer(x,y,function(x,y) y^4 - x^4 - 4*y^2 + 9*x^2)
contour(x,y,z,levels = 0)
